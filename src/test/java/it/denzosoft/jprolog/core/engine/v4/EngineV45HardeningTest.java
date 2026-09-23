package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.InferenceLimitException;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SafeModeOptions;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * 4.5 wave P6 — production hardening: threads, the concurrent family, the shared inference budget,
 * safe mode, the CLI (ISS-2025-0620..0639). One method per ISS; each fails on the 4.4.0 build and
 * on this tree before the wave. Every thread test has a JUnit timeout and uses at most 8 threads.
 */
public class EngineV45HardeningTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
        prolog.consult(
            ":- dynamic(cnt/1).\n"
          + "cnt(0).\n"
          + "spin(N) :- N > 0, N1 is N - 1, spin(N1).\n"
          + "spin(0).\n"
          + "loop :- loop.\n"
          + "bump :- retract(cnt(N)), N1 is N + 1, assertz(cnt(N1)).\n"
          + "bumps(0) :- !.\n"
          + "bumps(K) :- with_mutex(p6cnt, bump), K1 is K - 1, bumps(K1).\n"
          // wait (bounded, 20 s) until thread T has finished / has been reclaimed
          + "thread_join_wait(T) :- between(1, 4000, _), thread_property(T, status(S)), "
          + "(S == running -> thread_sleep(0.005), fail ; !).\n"
          + "gone(T) :- between(1, 4000, _), "
          + "(catch(thread_property(T, status(_)), error(existence_error(thread, _), _), fail) "
          + "-> thread_sleep(0.005), fail ; !).\n");
    }

    private void ok(String q) {
        assertFalse("must succeed: " + q, prolog.solve(q).isEmpty());
    }

    private void no(String q) {
        assertTrue("must fail: " + q, prolog.solve(q).isEmpty());
    }

    // ================================================================ ISS-2025-0620 thread_detach

    /**
     * thread_detach/1 (SWI): a live thread — detached or not — can be detached; a thread that
     * finished and was never joined is reclaimed; an unknown or reclaimed one is
     * existence_error(thread, Id). It raised a message atom (and raced the thread's end: the flaky
     * testISS0479_ThreadCreate3Options).
     */
    @Test(timeout = 30000)
    public void testISS0620_ThreadDetachSemantics() {
        ok("thread_create(thread_get_message(go), D, [detached(true)]), thread_detach(D), "
         + "thread_detach(D), thread_send_message(D, go)");
        // a finished detached thread is gone: wait (bounded) until it is reclaimed, then detach
        ok("thread_create(true, D, [detached(true)]), gone(D), "
         + "catch(thread_detach(D), error(existence_error(thread, D2), _), true), D2 == D");
        // finished, never joined: detach reclaims it, after which it cannot be joined
        ok("thread_create(true, T), thread_join_wait(T), thread_detach(T), "
         + "catch(thread_join(T, _), error(existence_error(thread, T), _), true)");
        ok("catch(thread_detach(p6nosuch), error(existence_error(thread, p6nosuch), _), true)");
    }

    // ================================================================ ISS-2025-0621 ISO thread errors

    @Test(timeout = 30000)
    public void testISS0621_ThreadErrorsAreIsoTerms() {
        ok("catch(thread_join(p6nosuch, _), error(existence_error(thread, p6nosuch), _), true)");
        ok("thread_create(thread_get_message(_), T, [alias(p6dup)]), "
         + "catch(thread_create(true, _, [alias(p6dup)]), error(permission_error(create, thread, p6dup), _), true), "
         + "thread_send_message(p6dup, x), thread_join(T, true)");
        ok("thread_create(thread_get_message(_), D, [detached(true)]), "
         + "catch(thread_join(D, _), error(permission_error(join, thread, D), _), true), "
         + "thread_send_message(D, x)");
        ok("catch(thread_join(main, _), error(permission_error(join, thread, main), _), true)");
        ok("catch(thread_create(_, _), error(instantiation_error, _), true)");
        ok("catch(thread_create(1, _), error(type_error(callable, 1), _), true)");
        ok("catch(thread_sleep(a), error(type_error(number, a), _), true)");
        ok("catch(thread_send_message(p6noq, x), error(existence_error(message_queue, p6noq), _), true)");
        ok("catch(thread_get_message(_, _), error(instantiation_error, _), true)");
    }

    // ================================================================ ISS-2025-0622 concurrent family

    /** A worker's ball reaches catch/3 unchanged; the budget is never turned into a ball. */
    @Test(timeout = 60000)
    public void testISS0622_ConcurrentFamilyPropagatesBalls() {
        ok("catch(concurrent_and([throw(foo)], []), foo, true)");
        ok("catch(concurrent_maplist(throw, [bar]), bar, true)");
        ok("catch(concurrent(2, [true, throw(baz(1))], []), baz(X), true), X == 1");
        ok("catch(first_solution(_, [p6undefined], []), "
         + "error(existence_error(procedure, p6undefined/0), _), true)");
        ok("catch(concurrent_or([throw(qux)], _), qux, true)");
        // bindings made by the goals come back (SWI), aliasing included
        ok("concurrent(2, [X = 1, Y = 2], []), X == 1, Y == 2");
        ok("concurrent_maplist(=(Z), [a, A]), Z == a, A == a");

        Prolog p = new Prolog();
        p.consult("loop :- loop.\n");
        p.setInferenceBudget(50000);
        for (String q : new String[] {
                "catch(first_solution(_, [loop], []), _, true).",
                "catch(concurrent_and([loop], []), _, true).",
                "catch(concurrent_maplist(call, [loop, true]), _, true).",
                "catch(concurrent(2, [loop, true], []), _, true).",
                "catch(concurrent_or([loop], _), _, true)." }) {
            try {
                p.solve(q);
                fail("the budget must reach the embedder through " + q);
            } catch (InferenceLimitException expected) {
                // the only correct outcome: untrusted catch/3 cannot swallow the budget
            }
        }
    }

    @Test(timeout = 30000)
    public void testISS0623_ConcurrentArgumentErrorsAreIso() {
        ok("catch(concurrent_maplist(foo, bar), error(type_error(list, bar), _), true)");
        ok("catch(concurrent_maplist(foo, [a|_]), error(instantiation_error, _), true)");
        ok("catch(concurrent(0, [true], []), error(domain_error(positive_integer, 0), _), true)");
        ok("catch(concurrent(a, [true], []), error(type_error(integer, a), _), true)");
        ok("catch(concurrent_and([1], []), error(type_error(callable, 1), _), true)");
        no("concurrent_and([true, fail], [])");
        no("first_solution(_, [fail, fail], [])");
    }

    // ================================================================ ISS-2025-0624 shared budget

    /**
     * ONE budget per query: the workers draw from the parent's pool. Each spin(6000) fits the
     * budget on its own; ten of them in parallel do not (each worker used to get a full budget).
     */
    @Test(timeout = 60000)
    public void testISS0624_BudgetIsSharedByTheWorkers() {
        Prolog p = new Prolog();
        p.consult("spin(N) :- N > 0, N1 is N - 1, spin(N1).\nspin(0).\n");
        p.setInferenceBudget(60000);
        assertFalse(p.solve("spin(6000).").isEmpty());
        try {
            p.solve("concurrent_maplist(spin, [6000, 6000, 6000, 6000, 6000, 6000, 6000, 6000, 6000, 6000]).");
            fail("ten spin(6000) must exhaust one shared budget");
        } catch (InferenceLimitException expected) {
            // good
        }
        List<Map<String, Term>> s = p.solve(
            "findall(S, (between(1, 8, _), thread_create(spin(6000), T), thread_join(T, S)), L).");
        assertEquals(1, s.size());
        assertTrue("some thread must have met the shared budget: " + s.get(0).get("L"),
                   s.get(0).get("L").toString().contains("inference_limit_exceeded"));
    }

    /** Natives that do O(N) work in one step charge the budget for it. */
    @Test(timeout = 60000)
    public void testISS0624_BudgetChargesTheWorkOfNatives() {
        Prolog p = new Prolog();
        p.setInferenceBudget(100000);
        String[] qs = {
            "length(L, N), fail.",
            "append(X, Y, Z), fail.",
            "member(a, L), fail.",
            "nth0(I, L, a), fail.",
            "nth1(I, L, a), fail.",
            "numlist(1, 3000, L), repeat, msort(L, _), fail.",
            "numlist(1, 3000, L), repeat, copy_term(L, _), fail.",
            "numlist(1, 3000, L), repeat, length(L, _), fail.",
            "numlist(1, 3000, L), atomic_list_concat(L, A), repeat, atom_codes(A, _), fail.",
        };
        for (String q : qs) {
            long t0 = System.nanoTime();
            try {
                p.solve(q);
                fail("must exhaust the budget: " + q);
            } catch (InferenceLimitException expected) {
                long ms = (System.nanoTime() - t0) / 1000000L;
                // 4.4.0: minutes (quadratic work per step). The bound is generous for a loaded box.
                assertTrue(q + " took " + ms + " ms", ms < 8000);
            }
        }
    }

    // ================================================================ ISS-2025-0629 message queues

    @Test(timeout = 30000)
    public void testISS0629_SelectiveReceiveAndTimeouts() {
        ok("message_queue_create(Q), thread_send_message(Q, a(1)), thread_send_message(Q, b(2)), "
         + "thread_get_message(Q, b(X)), X == 2, thread_get_message(Q, Y), Y == a(1)");
        // blocks until a unifiable message arrives, leaving the others queued in order
        ok("message_queue_create(Q), thread_send_message(Q, a(1)), "
         + "thread_create((thread_sleep(0.05), thread_send_message(Q, b(7))), T), "
         + "thread_get_message(Q, b(X)), X == 7, thread_join(T, true), thread_peek_message(Q, P), P == a(1)");
        ok("message_queue_create(Q), thread_send_message(Q, a(1)), thread_peek_message(Q, a(Z)), Z == 1, "
         + "\\+ thread_peek_message(Q, b(_))");
        // thread_get_message/3: timeout(T) fails when it expires
        no("message_queue_create(Q), thread_get_message(Q, _, [timeout(0.05)])");
        ok("message_queue_create(Q), thread_send_message(Q, c), thread_get_message(Q, c, [timeout(0)])");
        // queue ids and thread ids never collide
        ok("message_queue_create(Q), thread_create(true, T), thread_join(T, _), Q \\== T");
        // aliases and destroy
        ok("message_queue_create(Q, [alias(p6q)]), Q == p6q, thread_send_message(p6q, hi), "
         + "thread_get_message(p6q, hi), message_queue_destroy(p6q), "
         + "catch(thread_send_message(p6q, x), error(existence_error(message_queue, p6q), _), true)");
        ok("catch(message_queue_destroy(p6nothere), error(existence_error(message_queue, p6nothere), _), true)");
        // the main thread's queue, selective
        ok("thread_create(thread_send_message(main, p6hello(worker)), T), thread_join(T, true), "
         + "thread_get_message(p6hello(W)), W == worker");
    }

    // ================================================================ ISS-2025-0630 thread_property

    @Test(timeout = 30000)
    public void testISS0630_ThreadProperty() {
        ok("thread_property(main, status(running))");
        ok("thread_self(S), S == main, thread_property(S, id(1))");
        ok("thread_create(thread_get_message(_), T, [alias(p6prop)]), "
         + "thread_property(p6prop, status(running)), thread_property(p6prop, detached(false)), "
         + "thread_property(p6prop, alias(p6prop)), thread_send_message(p6prop, go), "
         + "thread_join(p6prop, true)");
        ok("thread_create(fail, T), thread_join_wait(T), thread_property(T, status(false)), thread_join(T, _)");
        ok("findall(I, thread_property(I, status(_)), L), memberchk(main, L)");
        ok("catch(thread_property(p6none, _), error(existence_error(thread, p6none), _), true)");
    }

    // ================================================================ ISS-2025-0631 mutexes, join/1

    @Test(timeout = 60000)
    public void testISS0631_MutexesAndWithMutex() {
        ok("mutex_create(M), mutex_lock(M), mutex_lock(M), mutex_unlock(M), mutex_unlock(M), "
         + "catch(mutex_unlock(M), error(permission_error(unlock, mutex, M), _), true), mutex_destroy(M)");
        ok("mutex_create(M, [alias(p6m)]), M == p6m, "
         + "catch(mutex_create(p6m), error(permission_error(create, mutex, p6m), _), true)");
        ok("catch(mutex_lock(_), error(instantiation_error, _), true)");
        ok("catch(mutex_unlock('$mutex'(999999)), error(existence_error(mutex, _), _), true)");
        // with_mutex/2 is once/1 holding the mutex, released on failure and on an exception
        ok("with_mutex(p6w, (X = 1 ; X = 2)), X == 1");
        no("with_mutex(p6w, fail)");
        ok("catch(with_mutex(p6w, throw(oops)), oops, true), mutex_trylock(p6w), mutex_unlock(p6w)");
        // mutual exclusion: 8 threads x 25 increments of a read-modify-write counter
        ok("findall(T, (between(1, 8, _), thread_create(bumps(25), T)), Ts), "
         + "forall(member(T, Ts), thread_join(T, true)), cnt(N), N == 200");
        // a thread that dies holding a mutex releases it
        ok("thread_create(mutex_lock(p6dead), T), thread_join(T, true), mutex_trylock(p6dead), mutex_unlock(p6dead)");
        // thread_join/1
        ok("thread_create(true, T), thread_join(T)");
        ok("thread_create(fail, T), catch(thread_join(T), error(thread_error(T, false), _), true)");
    }

    // ================================================================ ISS-2025-0632 thread_exit

    @Test(timeout = 30000)
    public void testISS0632_ThreadExit() {
        ok("thread_create(thread_exit(bye(1)), T), thread_join(T, S), S == exited(bye(1))");
        // catch/3 inside the thread cannot intercept it; cleanups still run
        ok("thread_create(catch(thread_exit(x), _, true), T), thread_join(T, S), S == exited(x)");
        ok("thread_create(setup_call_cleanup(true, thread_exit(y), thread_send_message(main, p6cleaned)), T), "
         + "thread_join(T, S), S == exited(y), thread_get_message(p6cleaned)");
        ok("catch(thread_exit(x), error(permission_error(exit, thread, main), _), true)");
        // at_exit/1 option
        ok("thread_create(true, T, [at_exit(thread_send_message(main, p6atexit))]), thread_join(T, true), "
         + "thread_get_message(p6atexit)");
    }

    // ================================================================ ISS-2025-0633 per-thread globals

    @Test(timeout = 30000)
    public void testISS0633_GlobalVariablesArePerThread() {
        ok("nb_setval(p6k, 1), thread_create(nb_setval(p6k, 2), T), thread_join(T, true), "
         + "nb_getval(p6k, V), V == 1");
        ok("nb_setval(p6k, 1), thread_create(catch(nb_getval(p6k, _), error(existence_error(variable, p6k), _), "
         + "thread_send_message(main, p6unset)), T), thread_join(T, _), thread_get_message(p6unset)");
        // ...while the main thread keeps its globals from one query to the next
        ok("nb_setval(p6keep, kept)");
        ok("nb_getval(p6keep, V), V == kept");
    }

    // ================================================================ ISS-2025-0634 concurrent_forall

    @Test(timeout = 30000)
    public void testISS0634_ConcurrentForall() {
        ok("concurrent_forall(member(X, [1, 2, 3]), X > 0)");
        no("concurrent_forall(member(X, [1, 2, 3]), X > 1, [threads(2)])");
        ok("concurrent_forall(fail, fail)");
        ok("concurrent_forall(between(1, 20, X), (Y is X * X, Y > 0), [threads(4)])");
        ok("catch(concurrent_forall(member(X, [1]), throw(cf(X))), cf(V), true), V == 1");
    }

    // ================================================================ ISS-2025-0625..0627 safe mode

    /** Every predicate that survives safe mode (registry names, native keys, prelude exports). */
    private static TreeSet<String> survivors(Prolog p) {
        TreeSet<String> out = new TreeSet<String>();
        for (String n : p.getBuiltInRegistry().getBuiltInNames()) {
            if (p.getBuiltInRegistry().getBuiltIn(n) != null) out.add("registry " + n);
        }
        for (String k : p.getV4Engine().natives().keys()) out.add("native " + k);
        for (Prelude.Lib lib : Prelude.libraries()) {
            for (String e : lib.exports) out.add("library " + e);
        }
        return out;
    }

    /**
     * Safe mode strips every host-touching predicate, in the registry AND the native table, and the
     * surviving set is pinned: a new built-in fails this test until someone decides whether it may
     * run in a sandbox (src/test/resources/safe-mode-allowlist.txt).
     */
    @Test(timeout = 60000)
    public void testISS0625_SafeModeSurvivorsArePinned() throws Exception {
        Prolog p = new Prolog();
        p.enableSafeMode();
        TreeSet<String> live = survivors(p);
        String[] hostTouching = {
            "open", "see", "tell", "csv_read_file", "csv_write_file", "log_to_file", "consult",
            "ensure_loaded", "load_files", "make", "absolute_file_name", "exists_file",
            "exists_directory", "delete_file", "directory_files", "shell", "getenv", "setenv",
            "thread_create", "concurrent_maplist", "mutex_create", "with_mutex", "message_queue_create",
            "http_get", "http_post", "tcp_connect", "jdbc_connect", "java_new", "java_call",
            "persistent_save", "process_create", "compile_file", "."
        };
        for (String name : hostTouching) {
            assertFalse("registry " + name + " must not survive safe mode", live.contains("registry " + name));
            for (String k : live) {
                assertFalse(k + " must not survive safe mode", k.startsWith("native " + name + "/"));
            }
        }
        TreeSet<String> pinned = new TreeSet<String>();
        BufferedReader r = new BufferedReader(new InputStreamReader(
            getClass().getResourceAsStream("/safe-mode-allowlist.txt"), StandardCharsets.UTF_8));
        try {
            for (String line; (line = r.readLine()) != null; ) {
                if (!line.isEmpty() && !line.startsWith("#")) pinned.add(line);
            }
        } finally {
            r.close();
        }
        TreeSet<String> added = new TreeSet<String>(live);
        added.removeAll(pinned);
        TreeSet<String> gone = new TreeSet<String>(pinned);
        gone.removeAll(live);
        assertTrue("new predicates survive safe mode — decide (allowlist or deny): " + added, added.isEmpty());
        assertTrue("predicates no longer survive safe mode — update the allowlist: " + gone, gone.isEmpty());
        // ...and plain logic still works
        assertFalse(p.solve("append([1], [2], L), csv_parse('a,b', _)").isEmpty());
    }

    /** The concrete leaks of 4.4.0: CSV files, the log file, the loaders — all refused. */
    @Test(timeout = 60000)
    public void testISS0625_SafeModeDeniesTheFileLeaks() throws Exception {
        File dir = Files.createTempDirectory("p6safe").toFile();
        File secret = new File(dir, "secret.pl");
        Files.write(secret.toPath(), "secret(42).\n".getBytes(StandardCharsets.UTF_8));
        String sp = secret.getAbsolutePath().replace("\\", "/");
        try {
            Prolog p = new Prolog();
            p.enableSafeMode();
            for (String q : new String[] {
                    "csv_write_file('" + sp + ".csv', [row(a)])",
                    "csv_read_file('" + sp + "', _)",
                    "log_to_file('" + sp + ".log')",
                    "consult('" + sp + "')",
                    "open('" + sp + "', read, _)" }) {
                assertTrue(q + " must be an unknown procedure in safe mode", p.solve(
                    "catch(" + q + ", error(existence_error(procedure, _), _), true)").size() == 1);
            }
            assertFalse(new File(sp + ".csv").exists());
            assertFalse(new File(sp + ".log").exists());
            // a consulted (untrusted) text cannot pull a file in through a directive either
            try {
                p.consult(":- include('" + sp + "').\n");
                fail("the include must be refused");
            } catch (it.denzosoft.jprolog.core.exceptions.PrologException expected) {
                assertTrue(String.valueOf(expected.getMessage()), String.valueOf(expected.getMessage()).contains("permission_error"));
            }
            assertTrue(p.solve("catch(secret(X), _, fail)").isEmpty());

            // allowFileRead(dir): read-only access, restricted to the directory
            Prolog q = new Prolog();
            q.enableSafeMode(new SafeModeOptions().allowFileRead(dir.getAbsolutePath()));
            assertFalse(q.solve("consult('" + sp + "'), secret(42)").isEmpty());
            assertFalse(q.solve("open('" + sp + "', read, S), read(S, T), close(S), T == secret(42)").isEmpty());
            assertFalse(q.solve("catch(open('" + sp + "', write, _), "
                + "error(permission_error(open, source_sink, _), _), true)").isEmpty());
            assertFalse(q.solve("catch(consult('/etc/hostname'), "
                + "error(permission_error(open, source_sink, _), _), true)").isEmpty());
            assertFalse(q.solve("catch(open('" + dir.getAbsolutePath().replace("\\", "/")
                + "/../outside.txt', read, _), error(permission_error(open, source_sink, _), _), true)").isEmpty());
            assertTrue(q.solve("catch(csv_write_file(x, []), error(existence_error(procedure, _), _), true)").size() == 1);
        } finally {
            for (File f : dir.listFiles()) f.delete();
            dir.delete();
        }
    }

    /** log_to_file/1 in one engine no longer redirects another engine's log; errors are ISO. */
    @Test(timeout = 30000)
    public void testISS0626_LoggingIsPerEngine() throws Exception {
        File dir = Files.createTempDirectory("p6log").toFile();
        File logA = new File(dir, "a.log");
        try {
            Prolog a = new Prolog();
            Prolog b = new Prolog();
            assertFalse(a.solve("log_to_file('" + logA.getAbsolutePath().replace("\\", "/") + "'), "
                + "log_info(from_a)").isEmpty());
            java.io.ByteArrayOutputStream err = new java.io.ByteArrayOutputStream();
            java.io.PrintStream old = System.err;
            System.setErr(new java.io.PrintStream(err, true, "UTF-8"));
            try {
                b.solve("log_level(warning), log_info(hidden_b), log_warning(from_b)");
            } finally {
                System.setErr(old);
            }
            String fileText = new String(Files.readAllBytes(logA.toPath()), StandardCharsets.UTF_8);
            assertTrue(fileText, fileText.contains("INFO: from_a"));
            assertFalse("engine B's log must not reach engine A's file: " + fileText, fileText.contains("from_b"));
            assertFalse(a.solve("catch(log_level(loud), error(domain_error(log_level, loud), _), true)").isEmpty());
            assertFalse(a.solve("catch(log_info(_), error(instantiation_error, _), true)").isEmpty());
            // engine A's level is untouched by engine B's log_level(warning)
            assertFalse(a.solve("log_info(still_a)").isEmpty());
            fileText = new String(Files.readAllBytes(logA.toPath()), StandardCharsets.UTF_8);
            assertTrue(fileText, fileText.contains("INFO: still_a"));
        } finally {
            logA.delete();
            dir.delete();
        }
    }

    // ================================================================ ISS-2025-0638 lazy HTTP client

    /** {@code new Prolog()} no longer builds an HttpClient + SSLContext. */
    @Test(timeout = 30000)
    public void testISS0638_HttpClientIsLazy() throws Exception {
        new Prolog();
        Class<?> http = Class.forName("it.denzosoft.jprolog.builtin.http.HttpServerPredicates");
        java.lang.reflect.Field built = http.getDeclaredField("clientBuilt");
        built.setAccessible(true);
        assertFalse("constructing an engine must not build the HTTP client", built.getBoolean(null));
        boolean eager = false;
        for (java.lang.reflect.Field f : http.getDeclaredFields()) {
            if (java.lang.reflect.Modifier.isStatic(f.getModifiers())
                    && f.getType().getName().equals("java.net.http.HttpClient")) eager = true;
        }
        assertFalse("no static HttpClient field may be initialised with the class", eager);
    }

    // ================================================================ ISS-2025-0639 load lock

    /**
     * A directive that starts a thread which consults another file and JOINS it used to deadlock
     * on the per-engine load lock (the directive's thread holds it). The joined thread now loads
     * in its place.
     */
    @Test(timeout = 60000)
    public void testISS0639_DirectiveThreadThatConsultsDoesNotDeadlock() throws Exception {
        File dir = Files.createTempDirectory("p6lock").toFile();
        try {
            File b = new File(dir, "lockb.pl");
            Files.write(b.toPath(), "p6b(1).\n".getBytes(StandardCharsets.UTF_8));
            File a = new File(dir, "locka.pl");
            String bp = b.getAbsolutePath().replace("\\", "/");
            Files.write(a.toPath(), (":- thread_create(consult('" + bp + "'), T), thread_join(T, S), "
                + "assertz(p6joined(S)).\np6a(1).\n").getBytes(StandardCharsets.UTF_8));
            Prolog p = new Prolog();
            final Prolog.LoadResult[] res = new Prolog.LoadResult[1];
            final Throwable[] err = new Throwable[1];
            Thread t = new Thread(() -> {
                try { res[0] = p.loadFile(a.getAbsolutePath()); } catch (Throwable e) { err[0] = e; }
            });
            t.setDaemon(true);
            t.start();
            t.join(30000);
            assertFalse("the load deadlocked", t.isAlive());
            assertTrue(String.valueOf(err[0]), err[0] == null);
            assertFalse(p.solve("p6joined(true), p6a(1), p6b(1)").isEmpty());
            // a plain concurrent load from another thread still waits its turn and then succeeds
            assertFalse(p.solve("thread_create(consult('" + bp + "'), T), thread_join(T, true)").isEmpty());
        } finally {
            for (File f : dir.listFiles()) f.delete();
            dir.delete();
        }
    }
}
