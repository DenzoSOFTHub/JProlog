package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0745..0759 - 4.6 wave Q4: threads and tabling residue.
/**
 * Wave Q4 of the 4.6 completeness program: the per-engine working directory, the waits the load
 * cycle check sees, thread-safe library autoloading, {@code thread_signal/2} and the rest of the
 * thread API, the mutex-at-exit warning, tabling across threads, mode-directed tabling and the
 * minimal well-founded semantics ({@code tnot/1}, {@code undefined/0}).
 */
public class EngineV46ThreadsTablingTest {

    private Prolog prolog;
    private File dir;

    @Before
    public void setUp() throws Exception {
        prolog = new Prolog();
        dir = Files.createTempDirectory("q4").toFile();
    }

    @After
    public void tearDown() {
        deleteTree(dir);
    }

    private static void deleteTree(File f) {
        File[] kids = f.listFiles();
        if (kids != null) for (File k : kids) deleteTree(k);
        f.delete();
    }

    private static String path(File f) {
        return f.getAbsolutePath().replace("\\", "/");
    }

    private void ok(String goal) {
        assertFalse("expected success: " + goal, prolog.solve(goal + ".").isEmpty());
    }

    private void no(String goal) {
        assertTrue("expected failure: " + goal, prolog.solve(goal + ".").isEmpty());
    }

    private Term one(String goal, String var) {
        List<Map<String, Term>> s = prolog.solve(goal + ".");
        assertFalse("expected a solution: " + goal, s.isEmpty());
        return s.get(0).get(var);
    }

    private String str(String goal, String var) {
        return String.valueOf(one("once((" + goal + ")), with_output_to(string(S__), write(" + var + "))", "S__"))
            .replace("\"", "");
    }

    // ================================================================ ISS-2025-0745

    /** working_directory/2 is per engine: relative paths follow it, `user.dir` is never touched. */
    @Test(timeout = 60000)
    public void testISS0745_WorkingDirectoryIsPerEngine() throws Exception {
        String before = System.getProperty("user.dir");
        Prolog other = new Prolog();
        File sub = new File(dir, "sub");
        assertTrue(sub.mkdirs());
        Files.write(new File(sub, "q4wd.pl").toPath(), "q4wd(here).\n".getBytes(StandardCharsets.UTF_8));
        ok("working_directory(_, '" + path(sub) + "')");
        assertEquals("user.dir must not change", before, System.getProperty("user.dir"));
        ok("working_directory(D, D), atom_concat(_, 'sub/', D)");
        ok("exists_file('q4wd.pl')");
        ok("open('out.txt', write, S), write(S, hello), close(S)");
        assertTrue("open/3 resolves against the engine directory", new File(sub, "out.txt").isFile());
        ok("consult('q4wd.pl'), q4wd(here)");
        ok("absolute_file_name('q4wd.pl', A), atom_concat(_, 'sub/q4wd.pl', A)");
        // the other engine is unaffected
        assertTrue(other.solve("exists_file('q4wd.pl').").isEmpty());
        assertTrue(other.solve("working_directory(D, D), atom_concat(_, 'sub/', D).").isEmpty());
        // relative change, and an error for a missing directory
        ok("working_directory(_, '..'), exists_directory(sub)");
        ok("catch(working_directory(_, nonexistent_q4), error(existence_error(directory, nonexistent_q4), _), true)");
    }

    // ================================================================ ISS-2025-0746

    /**
     * A load that waits for a message only the blocked loader could send is refused with
     * permission_error(load, ...) instead of hanging: worker A loads F, whose directive starts
     * worker B and waits for B's message; B loads F too. Without the message-wait edge B waits for
     * A's file lock forever and A for B's message forever.
     */
    @Test(timeout = 60000)
    public void testISS0746_MessageWaitSeenByLoadCycleCheck() throws Exception {
        File f = new File(dir, "q4cyc.pl");
        String fp = path(f);
        Files.write(f.toPath(), (":- dynamic q4cyc_err/1.\n"
            + ":- thread_self(Me), thread_create((catch(consult('" + fp + "'), E, true), "
            + "thread_send_message(Me, got(E))), _, [detached(true)]), "
            + "thread_get_message(got(E)), assertz(q4cyc_err(E)).\n"
            + "q4cyc_loaded.\n").getBytes(StandardCharsets.UTF_8));
        ok("thread_create(consult('" + fp + "'), A), thread_join(A, S), S == true");
        ok("q4cyc_err(error(permission_error(load, source_sink, _), _))");
        ok("q4cyc_loaded");
    }

    // ================================================================ ISS-2025-0749

    /** thread_signal/2 interrupts a busy worker; the signal's exception unwinds it (SWI). */
    @Test(timeout = 60000)
    public void testISS0749_SignalInterruptsBusyWorker() {
        // the worker reports that it is inside catch/3 before it is signalled
        ok("thread_self(M), thread_create(catch((thread_send_message(M, q4ready), repeat, fail), stopped, true), T), "
            + "thread_get_message(q4ready), thread_signal(T, throw(stopped)), thread_join(T, S), S == true");
        // uncaught: the join status reports it
        ok("thread_create((repeat, fail), T), thread_signal(T, throw(bye)), thread_join(T, S), S == exception(bye)");
    }

    /** A worker blocked in thread_get_message/1 runs the signal; it runs IN the target thread. */
    @Test(timeout = 60000)
    public void testISS0749_SignalWakesBlockedWorker() {
        prolog.consult(":- dynamic q4ran/1.\n");
        ok("thread_create(thread_get_message(never_sent), T), "
            + "thread_signal(T, (thread_self(Me), assertz(q4ran(Me)))), "
            + "thread_signal(T, throw(woke)), thread_join(T, S), S == exception(woke), q4ran(T)");
        ok("thread_create(thread_sleep(30), T), thread_signal(T, throw(woke)), thread_join(T, exception(woke))");
        ok("mutex_create(M), mutex_lock(M), thread_create(with_mutex(M, true), T), "
            + "thread_signal(T, throw(woke)), thread_join(T, S), mutex_unlock(M), S == exception(woke)");
    }

    /** Signalling `main` (the embedder / JUnit thread) from a worker works, blocked or busy. */
    @Test(timeout = 60000)
    public void testISS0749_SignalMainFromWorker() {
        prolog.consult(":- dynamic q4main_sig/0.\n");
        ok("thread_self(Main), thread_create(thread_signal(Main, assertz(q4main_sig)), T), "
            + "thread_join(T, true), q4main_sig");
        ok("thread_self(Main), thread_create(thread_signal(Main, throw(from_worker)), _, [detached(true)]), "
            + "catch((repeat, fail), from_worker, true)");
        // bindings made by a signal goal are undone; failure is ignored
        ok("thread_self(Me), thread_signal(Me, fail), thread_signal(Me, X = 1), var(X)");
        ok("catch(thread_signal(no_such_thread_q4, true), error(existence_error(thread, no_such_thread_q4), _), true)");
    }

    // ================================================================ ISS-2025-0750

    @Test(timeout = 60000)
    public void testISS0750_ThreadStatisticsQueueAndMutexProperties() {
        ok("thread_statistics(main, inferences, N), integer(N), N > 0");
        ok("thread_self(M), thread_create((between(1, 200000, _), fail ; thread_send_message(M, q4busy), "
            + "thread_get_message(go)), T), thread_get_message(q4busy), "
            + "thread_statistics(T, inferences, N), thread_statistics(T, cputime, C), "
            + "thread_send_message(T, go), thread_join(T, _), integer(N), N > 1000, float(C)");
        ok("thread_statistics(main, epoch, E), number(E)");
        // message_queue_property/2 and a bounded queue
        ok("message_queue_create(Q, [alias(q4q), max_size(2)]), thread_send_message(q4q, a), "
            + "thread_send_message(q4q, b), message_queue_property(Q, size(2)), "
            + "message_queue_property(Q, max_size(2)), message_queue_property(Q, alias(q4q)), "
            + "\\+ thread_send_message(q4q, c, [timeout(0.05)]), thread_get_message(q4q, a), "
            + "thread_send_message(q4q, c, [timeout(0.05)]), message_queue_property(q4q, waiting(0)), "
            + "message_queue_destroy(q4q)");
        // mutex_property/2
        ok("mutex_create(_, [alias(q4m)]), mutex_property(q4m, status(unlocked)), "
            + "mutex_lock(q4m), mutex_lock(q4m), mutex_property(q4m, status(locked(main, 2))), "
            + "mutex_unlock(q4m), mutex_property(q4m, status(locked(main, 1))), mutex_unlock(q4m), "
            + "mutex_property(q4m, status(unlocked)), mutex_property(M, alias(q4m)), M == q4m, mutex_destroy(q4m)");
        ok("catch(message_queue_property(no_q4, size(_)), error(existence_error(message_queue, no_q4), _), true)");
    }

    @Test(timeout = 60000)
    public void testISS0750_ThreadPools() {
        prolog.consult("q4until(G) :- between(1, 200, _), (call(G) -> ! ; thread_sleep(0.05), fail).\n");
        ok("thread_pool_create(q4pool, 2, [])");
        ok("current_thread_pool(q4pool), thread_pool_property(q4pool, size(2))");
        ok("thread_create_in_pool(q4pool, thread_get_message(go), A, []), "
            + "thread_create_in_pool(q4pool, thread_get_message(go), B, []), "
            + "thread_pool_property(q4pool, free(0)), "
            + "catch(thread_create_in_pool(q4pool, true, _, [wait(false)]), "
            + "      error(resource_error(threads_in_pool(q4pool)), _), Full = yes), Full == yes, "
            + "thread_create(thread_create_in_pool(q4pool, true, _, []), W), "      // waits for a slot
            + "q4until(thread_pool_property(q4pool, backlog(1))), "
            + "thread_send_message(A, go), thread_join(A, true), thread_join(W, true), "
            + "thread_send_message(B, go), thread_join(B, true)");
        ok("catch(thread_pool_create(q4pool, 1, []), error(permission_error(create, thread_pool, q4pool), _), true)");
        ok("thread_pool_destroy(q4pool), \\+ current_thread_pool(q4pool)");
        ok("catch(thread_create_in_pool(q4pool, true, _, []), error(existence_error(thread_pool, q4pool), _), true)");
    }

    // ================================================================ ISS-2025-0751

    /** A mutex still held when a thread ends is released AND reported with a warning. */
    @Test(timeout = 60000)
    public void testISS0751_MutexHeldAtExitWarns() {
        prolog.consult(":- dynamic q4warn/1.\n"
            + "message_hook(format(F, A), warning, _) :- sub_atom(F, _, _, _, mutex), assertz(q4warn(A)).\n");
        ok("mutex_create(M), thread_create(mutex_lock(M), T), thread_join(T, true), "
            + "mutex_trylock(M), mutex_unlock(M), q4warn([T, M])");
    }

    // ================================================================ ISS-2025-0747

    /**
     * Eight worker threads autoload the same library modules at the same instant (released by one
     * broadcast), 20 times on fresh engines: every worker must see complete modules. Before the
     * fix a module was marked loaded BEFORE its clauses were installed, so a concurrent caller got
     * existence_error, and the module maps were plain HashMaps written while being read.
     */
    @Test(timeout = 120000)
    public void testISS0747_ConcurrentLibraryAutoload() {
        for (int rep = 0; rep < 20; rep++) {
            Prolog p = new Prolog();
            p.consult("q4auto :- thread_get_message(go), "
                + "pairs_keys_values(P, [a,b], [1,2]), P == [a-1,b-2], "
                + "foldl([X,A0,A]>>(A is A0+X), [1,2,3], 0, 6), "
                + "include([X]>>(X > 1), [1,2,3], [2,3]), last([1,2,3], 3), "
                + "sum_list([1,2], 3), freeze(V, true), V = 1.\n"
                + "q4spawn(0, []) :- !.\n"
                + "q4spawn(N, [T|Ts]) :- thread_create(q4auto, T), N1 is N - 1, q4spawn(N1, Ts).\n"
                + "q4go([]).\n"
                + "q4go([T|Ts]) :- thread_send_message(T, go), q4go(Ts).\n"
                + "q4join([], []).\n"
                + "q4join([T|Ts], [S|Ss]) :- thread_join(T, S), q4join(Ts, Ss).\n");
            List<Map<String, Term>> r = p.solve("q4spawn(8, Ts), q4go(Ts), q4join(Ts, Ss).");
            assertEquals(1, r.size());
            assertEquals("repetition " + rep, "[true,true,true,true,true,true,true,true]",
                String.valueOf(r.get(0).get("Ss")).replace(" ", ""));
        }
    }

    // ================================================================ ISS-2025-0748

    /**
     * The op/3 module context is per thread: while a worker is paused inside the load of a module
     * file, an op/3 run by `main` stays global (it used to be attributed to the module being
     * loaded, and so was invisible from user).
     */
    @Test(timeout = 60000)
    public void testISS0748_OpModuleContextIsPerThread() throws Exception {
        File f = new File(dir, "q4opm.pl");
        Files.write(f.toPath(), (":- module(q4opm, []).\n"
            + ":- op(700, xfx, q4mop).\n"
            + ":- thread_send_message(main, q4loading), thread_get_message(q4cont).\n").getBytes(StandardCharsets.UTF_8));
        ok("thread_create(consult('" + path(f) + "'), T), thread_get_message(q4loading), "
            + "op(700, xfx, q4glob), thread_send_message(T, q4cont), thread_join(T, true)");
        ok("current_op(700, xfx, q4glob)");
        no("current_op(700, xfx, q4mop)");
    }

    // ================================================================ ISS-2025-0752

    private static final String GRAPH =
          "q4e(X, Y) :- between(1, 40, X), Y is X mod 40 + 1.\n"          // a 40-node ring
        + "q4e(X, Y) :- between(1, 40, X), 0 =:= X mod 7, Y is (X * 3) mod 40 + 1.\n"
        + "q4path(X, Y) :- q4path(X, Z), q4e(Z, Y).\n"
        + "q4path(X, Y) :- q4e(X, Y).\n"
        + "q4count(S, N) :- findall(Y, q4path(S, Y), L), sort(L, U), length(U, N).\n"
        + "q4worker(S) :- thread_get_message(go), q4count(S, N), thread_send_message(main, q4n(S, N)).\n"
        + "q4spawn(0, []) :- !.\n"
        + "q4spawn(K, [T|Ts]) :- S is K mod 3 + 1, thread_create(q4worker(S), T), K1 is K - 1, q4spawn(K1, Ts).\n"
        + "q4go([]).\n"
        + "q4go([T|Ts]) :- thread_send_message(T, go), q4go(Ts).\n"
        + "q4collect(0, []) :- !.\n"
        + "q4collect(K, [N|Ns]) :- thread_get_message(q4n(_, N)), K1 is K - 1, q4collect(K1, Ns).\n"
        + "q4joinall([]).\n"
        + "q4joinall([T|Ts]) :- thread_join(T, true), q4joinall(Ts).\n";

    /**
     * Eight threads evaluate the same left-recursive tabled predicate at the same instant (three
     * variants), 20 times on fresh engines, private (the default) and shared: every thread gets the
     * exact answer set, no deadlock, and a shared table is published for main.
     */
    @Test(timeout = 180000)
    public void testISS0752_ConcurrentTablingStress() {
        for (int rep = 0; rep < 20; rep++) {
            boolean shared = (rep % 2) == 1;
            Prolog p = new Prolog();
            p.consult((shared ? ":- table q4path/2 as shared.\n" : ":- table q4path/2.\n") + GRAPH);
            List<Map<String, Term>> r = p.solve(
                "q4spawn(8, Ts), q4go(Ts), q4collect(8, Ns), q4joinall(Ts), msort(Ns, S).");
            assertEquals(1, r.size());
            assertEquals("repetition " + rep, "[40,40,40,40,40,40,40,40]",
                String.valueOf(r.get(0).get("S")).replace(" ", ""));
            // a shared table is visible to main without re-evaluation; a private one is not
            boolean seen = !p.solve("current_table(q4path(1, _), complete).").isEmpty()
                || !p.solve("current_table(q4path(2, _), complete).").isEmpty()
                || !p.solve("current_table(q4path(3, _), complete).").isEmpty();
            assertEquals("repetition " + rep + " shared=" + shared, shared, seen);
        }
    }

    /**
     * A tabled evaluation that waits for a thread which itself calls a tabled predicate. Under the
     * old engine-wide evaluation claim the worker waited for main's claim while main waited for
     * the worker (resource_error(tabling_busy) after 60 s); the worker now has its own space.
     */
    @Test(timeout = 30000)
    public void testISS0752_NoCrossThreadTablingWait() {
        prolog.consult(":- table q4ta/1, q4tb/1.\n"
            + "q4tb(X) :- member(X, [1,2,3]).\n"
            + "q4ta(N) :- thread_create((findall(X, q4tb(X), L), length(L, K), thread_exit(K)), T), "
            + "thread_join(T, exited(N)).\n");
        ok("q4ta(N), N == 3");
    }

    // ================================================================ ISS-2025-0753

    @Test(timeout = 60000)
    public void testISS0753_TableOptions() {
        java.io.PrintStream oldErr = System.err;
        java.io.ByteArrayOutputStream buf = new java.io.ByteArrayOutputStream();
        try {
            System.setErr(new java.io.PrintStream(buf, true));
            Prolog p = new Prolog();
            p.consult(":- table q4s/1 as subsumptive.\n"
                + ":- table (q4sh/1, q4pr/1) as shared.\n"
                + ":- table q4pr/1 as private.\n"
                + "q4s(1). q4sh(1). q4pr(1).\n");
            assertFalse(p.solve("q4s(1), q4sh(1), q4pr(1), current_table(q4sh(_), complete).").isEmpty());
            assertTrue("a shared table is published", !p.solve("current_table(q4sh(_), complete).").isEmpty());
            assertFalse(p.solve("catch(table(q4x/1 as bogus), error(domain_error(table_option, bogus), _), true).").isEmpty());
            assertFalse(p.solve("table(q4y/1 as (incremental, dynamic)).").isEmpty());
        } finally {
            System.setErr(oldErr);
        }
        String err = new String(buf.toByteArray(), StandardCharsets.UTF_8);
        assertTrue("subsumptive is reported: " + err, err.contains("subsumptive"));
        assertTrue("incremental is reported: " + err, err.contains("incremental"));
    }

    // ================================================================ ISS-2025-0754

    /** Mode-directed tabling: min/max by standard order, sum, lattice/3, po/2, free moded calls. */
    @Test(timeout = 60000)
    public void testISS0754_ModeDirectedTabling() {
        prolog.consult(
            // shortest path with min, over a cyclic weighted graph
            ":- table q4sp(_, _, min).\n"
            + "q4sp(X, Y, D) :- q4w(X, Y, D).\n"
            + "q4sp(X, Y, D) :- q4sp(X, Z, D1), q4w(Z, Y, D2), D is D1 + D2.\n"
            + "q4w(a, b, 5). q4w(a, c, 1). q4w(c, b, 1). q4w(b, d, 1). q4w(d, a, 1).\n"
            // longest path over a DAG with a lattice join
            + ":- table q4lp(_, _, lattice(q4longer/3)).\n"
            + "q4longer(P1, P2, P) :- length(P1, L1), length(P2, L2), (L1 >= L2 -> P = P1 ; P = P2).\n"
            + "q4lp(X, Y, [X, Y]) :- q4g(X, Y).\n"
            + "q4lp(X, Y, P) :- q4lp(X, Z, P0), q4g(Z, Y), append(P0, [Y], P).\n"
            + "q4g(1, 2). q4g(2, 3). q4g(1, 3). q4g(3, 4). q4g(2, 4).\n"
            // po: keep Old while call(PI, Old, New) succeeds (SWI: po('<'/2) keeps the smallest)
            + ":- table q4best(_, po('<'/2)).\n"
            + "q4best(k, 3). q4best(k, 1). q4best(k, 2).\n"
            + ":- table q4pareto(_, po(q4dom/2)).\n"
            + "q4dom(A-B, C-D) :- A =< C, B =< D.\n"          // Old dominates-or-equals New: keep Old
            + "q4pareto(k, 1-1). q4pareto(k, 2-2). q4pareto(k, 0-5).\n"
            // sum and standard-order min/max
            + ":- table q4tot(_, sum).\nq4tot(k, 1). q4tot(k, 2). q4tot(k, 3).\n"
            + ":- table q4mn(_, min), q4mx(_, max).\n"
            + "q4mn(k, b). q4mn(k, 1). q4mn(k, a).\nq4mx(k, b). q4mx(k, 1). q4mx(k, f(a)).\n"
            // first/last with a BOUND moded argument: evaluated free, then unified (SWI)
            + ":- table q4fl(_, first).\nq4fl(k, 1). q4fl(k, 2).\n"
            + ":- table q4two(_, min, max).\nq4two(k, 3, 3). q4two(k, 1, 2). q4two(k, 2, 7).\n");
        ok("findall(Y-D, q4sp(a, Y, D), L), msort(L, [a-4, b-2, c-1, d-3])");
        ok("q4sp(a, d, 3)");
        no("q4sp(a, d, 6)");
        ok("q4lp(1, 4, P), P == [1, 2, 3, 4]");
        ok("q4best(k, B), B == 1");
        ok("q4pareto(k, P), P == 0-5");
        ok("q4tot(k, S), S == 6");
        ok("q4mn(k, X), X == 1");
        ok("q4mx(k, X), X == f(a)");
        ok("q4fl(k, 1)");
        no("q4fl(k, 2)");
        ok("q4two(k, Mn, Mx), Mn == 1, Mx == 7");
        ok("catch(table(q4bad(_, lattice(q4longer/2))), error(domain_error(lattice_arity, 2), _), true)");
        ok("catch(table(q4bad(_, nope)), error(domain_error(tabled_mode, nope), _), true)");
    }

    // ================================================================ ISS-2025-0755

    private static final String WFS =
          ":- table p/0, q/0, r/0, s/0, win/1, a/0, b/0, c/0, u/0, p1/0, q1/0, p2/0, q2/0, r2/0.\n"
        + "p :- tnot(p).\n"
        + "q :- tnot(r).\nr :- tnot(q).\n"
        + "s :- tnot(p), fail.\n"
        + "move(a1, b1). move(b1, a1). move(b1, c1). move(c1, d1).\n"
        + "win(X) :- move(X, Y), tnot(win(Y)).\n"
        + "a :- tnot(b).\nb :- c.\nc.\n"
        + "u :- undefined.\n"
        // loops through negation that the SCC's simplification resolves
        + "p1 :- tnot(q1).\nq1 :- p1, fail.\n"
        + "p2 :- tnot(q2).\nq2 :- tnot(p2), r2.\nr2 :- fail.\n";

    /** The truth value of a ground query: true, false or undefined (a conditional answer). */
    private String truth(Prolog p, String goal) {
        final String[] r = {"false"};
        p.solveStream(goal, new Prolog.AnswerSink() {
            @Override public boolean onAnswer(Map<String, Term> answer, boolean more) {
                r[0] = (p.currentAnswerDelays() == null) ? "true" : "undefined";
                return false;
            }
        });
        return r[0];
    }

    /**
     * Minimal well-founded semantics: tnot/1 over a loop through negation delays instead of
     * raising permission_error (4.5); the SCC's completion simplifies the conditional answers;
     * what stays conditional is undefined. The classic win/1 game, p :- tnot(p), q/r mutual
     * negation, undefined/0 and call_delays/2.
     */
    @Test(timeout = 60000)
    public void testISS0755_TnotWellFounded() {
        Prolog p = prolog;
        p.consult(WFS);
        assertEquals("undefined", truth(p, "p"));
        assertEquals("undefined", truth(p, "q"));
        assertEquals("undefined", truth(p, "r"));
        assertEquals("false", truth(p, "s"));
        assertEquals("true", truth(p, "win(c1)"));
        assertEquals("false", truth(p, "win(d1)"));
        assertEquals("undefined", truth(p, "win(a1)"));
        assertEquals("undefined", truth(p, "win(b1)"));
        assertEquals("false", truth(p, "a"));
        assertEquals("undefined", truth(p, "u"));
        assertEquals("true", truth(p, "p1"));
        assertEquals("false", truth(p, "q1"));
        assertEquals("true", truth(p, "p2"));
        assertEquals("false", truth(p, "q2"));
        ok("findall(X-D, call_delays(win(X), D), L), msort(L, [a1-win(a1), b1-win(b1), c1-true])");
        ok("call_delays(u, D), D == u");                  // u's answer is conditional
        ok("call_delays(undefined, D), D == undefined");
        ok("call_delays(c, D), D == true");
        ok("catch(tnot(move(a1, b1)), error(permission_error(tnot, non_tabled_procedure, move/2), _), true)");
        ok("catch(tnot(_), error(instantiation_error, _), true)");
    }
}
// END_CHANGE: ISS-2025-0745..0759
