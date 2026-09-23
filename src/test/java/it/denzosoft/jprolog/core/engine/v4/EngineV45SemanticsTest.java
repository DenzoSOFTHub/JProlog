package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.DebugEvent;
import it.denzosoft.jprolog.core.engine.InferenceLimitException;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.QueryCancelledException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0514..0529 - wave P1 of the 4.5.0 production-readiness program.
/**
 * Wave P1 (engine semantics) of {@code docs/reports/report-production-readiness-2026-09-23.md}:
 * one method per item P1.1 .. P1.16, each named after its ISS id and each failing on the 4.4.0
 * classes. Reference semantics: ISO 13211-1 first, SWI-Prolog 9 where ISO is silent.
 */
public class EngineV45SemanticsTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private boolean succeeds(String query) {
        return !prolog.solve(query + ".").isEmpty();
    }

    private int count(String query) {
        return prolog.solve(query + ".").size();
    }

    private static String text(Term t) {
        return it.denzosoft.jprolog.core.util.TermFormatter.format(t, true, false, false, 1200);
    }

    /** Output a query prints, through the thread-local capture every built-in honours. */
    private String output(Runnable r) {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(bos, true);
        PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            r.run();
        } finally {
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        return bos.toString().replace("\r\n", "\n");
    }

    // ------------------------------------------------------------------ P1.1

    /** P1.1: an answer is a COPY; a binding made after it was delivered must not show in it. */
    @Test
    public void testISS0514_AnswersDoNotLeakLaterBindings() {
        List<Map<String, Term>> sols = prolog.solve("X = f(Y) ; Y = 1.");
        assertEquals(2, sols.size());
        Term x0 = Unify.deref(sols.get(0).get("X"));
        assertTrue("answer 1: X = f(_)", x0 instanceof CompoundTerm);
        assertTrue("answer 1 must keep Y unbound, got " + text(x0),
            Unify.deref(((CompoundTerm) x0).getArguments().get(0)) instanceof Variable);
        assertTrue("answer 1: Y unbound", Unify.deref(sols.get(0).get("Y")) instanceof Variable);
        assertEquals("1", text(sols.get(1).get("Y")));

        sols = prolog.solve("(true ; X = 1).");
        assertEquals(2, sols.size());
        assertTrue("answer 1 of (true ; X = 1) is X unbound",
            Unify.deref(sols.get(0).get("X")) instanceof Variable);
        assertEquals("1", text(sols.get(1).get("X")));

        // sharing inside ONE answer is preserved: X and Y are the same fresh variable
        sols = prolog.solve("X = f(Z, Z), Y = Z.");
        CompoundTerm f = (CompoundTerm) Unify.deref(sols.get(0).get("X"));
        assertTrue(Unify.deref(f.getArguments().get(0)) == Unify.deref(f.getArguments().get(1)));
        assertTrue(Unify.deref(f.getArguments().get(0)) == Unify.deref(sols.get(0).get("Y")));
        // an unbound query variable keeps its NAME in the answer
        assertEquals("Z", text(sols.get(0).get("Z")));

        // the streaming entry point copies too
        final List<Map<String, Term>> streamed = new ArrayList<Map<String, Term>>();
        prolog.solveStream("X = f(Y) ; Y = 1", s -> { streamed.add(s); return true; });
        assertEquals(2, streamed.size());
        Term sx = Unify.deref(streamed.get(0).get("X"));
        assertTrue(Unify.deref(((CompoundTerm) sx).getArguments().get(0)) instanceof Variable);
    }

    // ------------------------------------------------------------------ P1.2

    /** P1.2: `_` is not an answer variable; `_Foo` still is (SWI hides it only when printing). */
    @Test
    public void testISS0515_AnonymousVariablesNotInAnswers() {
        prolog.consult("data(a, 1).\n");
        List<Map<String, Term>> sols = prolog.solve("data(a, _).");
        assertEquals(1, sols.size());
        assertTrue("no anonymous variable in " + sols.get(0), sols.get(0).isEmpty());

        sols = prolog.solve("data(_, _Named).");
        assertEquals(Collections.singleton("_Named"), sols.get(0).keySet());
        assertEquals("1", text(sols.get(0).get("_Named")));

        sols = prolog.solve("X = f(_, _).");
        assertEquals(Collections.singleton("X"), sols.get(0).keySet());
    }

    // ------------------------------------------------------------------ P1.3

    /** P1.3: the Recovery of catch/3 is call(R) — a cut inside it is local (ISO 7.8.9). */
    @Test
    public void testISS0516_CutInCatchRecoveryIsLocal() {
        assertEquals(2, count("(X = 1 ; X = 2), catch(throw(x), x, !)"));
        prolog.consult("r(X) :- catch(throw(x), x, (X = 1, !)).\nr(2).\n");
        List<Map<String, Term>> sols = prolog.solve("r(X).");
        assertEquals(2, sols.size());
        assertEquals("1", text(sols.get(0).get("X")));
        assertEquals("2", text(sols.get(1).get("X")));
    }

    // ------------------------------------------------------------------ P1.4

    /** P1.4: a variable goal G is call(G) — a `!` it is bound to cuts nothing outside it. */
    @Test
    public void testISS0517_VariableGoalBoundToCutIsOpaque() {
        assertEquals(2, count("G = !, (X = 1 ; X = 2), G"));
        prolog.consult("t6(X) :- G = (!, fail), (G ; X = alt).\n"
            + "t7(X) :- G = !, member(X, [a, b]), G.\n");
        List<Map<String, Term>> sols = prolog.solve("t6(X).");
        assertEquals(1, sols.size());
        assertEquals("alt", text(sols.get(0).get("X")));
        assertEquals(2, count("t7(X)"));
        // a literal ! in the same position still cuts the clause
        prolog.consult("t8(X) :- member(X, [a, b]), !.\n");
        assertEquals(1, count("t8(X)"));
    }

    // ------------------------------------------------------------------ P1.5

    /** P1.5: the control spine of a goal is checked BEFORE any of it runs (ISO 7.6.2). */
    @Test
    public void testISS0518_CallBodyCheckedBeforeRunning() {
        final AtomicReference<Throwable> thrown = new AtomicReference<Throwable>();
        String out = output(() -> {
            try { prolog.solve("call((write(a), nl, 1))."); }
            catch (Throwable t) { thrown.set(t); }
        });
        assertEquals("nothing may run before the body is rejected", "", out);
        assertTrue(thrown.get() instanceof PrologException);
        Term err = Unify.deref(((PrologException) thrown.get()).getErrorTerm());
        assertEquals("error", ((CompoundTerm) err).getName());
        assertEquals("type_error(callable,(write(a),nl,1))",
            text(((CompoundTerm) err).getArguments().get(0)).replace(" ", ""));

        // call((fail, 1)) raises like SWI (ISO 7.8.3.3 via the body conversion of 7.6.2 — the
        // "8.15.1.3" citation was \+/1's clause, ISS-2025-0669), with the WHOLE goal as the culprit
        assertTrue(succeeds("catch(call((fail, 1)), error(type_error(callable, G), _), true), G == (fail, 1)"));
        assertTrue(succeeds("catch(findall(X, (fail, 1), _), error(type_error(callable, G), _), true), G == (fail, 1)"));
        assertTrue(succeeds("catch(\\+ (fail ; 1), error(type_error(callable, G), _), true), G == (fail ; 1)"));
        assertTrue(succeeds("catch(once((fail -> 1 ; true)), error(type_error(callable, _), _), true)"));
        assertTrue(succeeds("catch((fail, 1), error(type_error(callable, G), _), true), G == (fail, 1)"));
        assertTrue(succeeds("G = (fail, 1), catch(G, error(type_error(callable, _), _), true)"));
        // a variable in the spine is fine: it is call(V) and only runs when reached
        assertFalse(succeeds("call((fail, _))"));
        assertTrue(succeeds("once(call((true ; _)))"));
    }

    // ------------------------------------------------------------------ P1.6

    /** P1.6: setof/3 sorts a group AFTER its witnesses are unified (ISO 8.10.3.4). */
    @Test
    public void testISS0519_SetofDedupsAfterWitnessUnification() {
        List<Map<String, Term>> sols = prolog.solve("setof(X, member(X, [Y, Y]), L).");
        assertEquals(1, sols.size());
        // the one element IS the query variable Y (SWI: L = [Y])
        assertEquals("[Y]", text(sols.get(0).get("L")));
        assertTrue(succeeds("setof(X, member(X, [Y, Y]), L), L = [E], E == Y"));
        assertTrue(succeeds("setof(X-Z, member(X-Z, [b-W, a-W, b-W]), L), L == [a-W, b-W]"));
    }

    // ------------------------------------------------------------------ P1.7

    /** P1.7: bagof/3 grouping is n log n in the number of distinct witnesses (was quadratic). */
    @Test(timeout = 120000)
    public void testISS0520_BagofGroupingIsNotQuadratic() {
        long t0 = System.nanoTime();
        assertTrue(succeeds("numlist(1, 40000, Ns), findall(N-W, (member(N, Ns), W is N mod 40000), Ps), "
            + "findall(W, bagof(N, member(N-W, Ps), _), Ws), length(Ws, 40000)"));
        long ms = (System.nanoTime() - t0) / 1000000;
        assertTrue("40 000 witnesses took " + ms + " ms", ms < 30000);
        // group ORDER: bagof = first appearance, setof = standard order of the witness
        assertTrue(succeeds("findall(W-L, bagof(X, member(X-W, [1-b, 2-a, 3-b]), L), R), R == [b-[1,3], a-[2]]"));
        assertTrue(succeeds("findall(W-L, setof(X, member(X-W, [3-b, 2-a, 1-b]), L), R), R == [a-[2], b-[1,3]]"));
        // variant witnesses (not identical) share one group; non-variants do not (ISO 8.10.2.4)
        prolog.consult("pw(1, f(_)).\npw(2, f(_)).\npv(1, f(P, P)).\npv(2, f(_, _)).\npv(3, f(Q, Q)).\n");
        assertTrue(succeeds("findall(W-L, bagof(X, pw(X, W), L), R), R = [f(_)-[1, 2]]"));
        assertTrue(succeeds("findall(L, bagof(X, pv(X, W), L), R), msort(R, [[1, 3], [2]])"));
        // the witness is BOUND by the answer, and to one variable shared by the whole group
        assertTrue(succeeds("bagof(X, pv(X, W), [1, 3]), W = f(A, B), A == B"));
    }

    // ------------------------------------------------------------------ P1.8

    /** P1.8: between/3 stops at 2^63-1, continues into big integers for inf, and is lazy on bigints. */
    @Test(timeout = 60000)
    public void testISS0521_BetweenStopsAtLongMaxAndHandlesBigints() {
        assertTrue(succeeds("findall(X, between(9223372036854775806, 9223372036854775807, X), L), "
            + "L == [9223372036854775806, 9223372036854775807]"));
        assertTrue(succeeds("once((between(9223372036854775807, inf, X), X > 9223372036854775808)), "
            + "X =:= 9223372036854775809"));
        assertTrue(succeeds("findall(X, between(100000000000000000000, 100000000000000000002, X), L), length(L, 3)"));
        assertTrue(succeeds("once(between(100000000000000000000, inf, X)), X =:= 100000000000000000000"));
        assertTrue(succeeds("once(between(1, 100000000000000000000, X)), X == 1"));
        assertTrue(succeeds("between(1, 100000000000000000000, 5)"));
        assertTrue(succeeds("between(1, inf, 100000000000000000000)"));
        assertFalse(succeeds("between(1, 100000000000000000000, 100000000000000000001)"));
        assertFalse(succeeds("between(3, 2, _)"));
        assertTrue(succeeds("findall(X, between(-9223372036854775809, -9223372036854775807, X), L), length(L, 3)"));
    }

    // ------------------------------------------------------------------ P1.9

    /** P1.9: aggregate_all/3 follows SWI: evaluated sum/max/min, max(X,W)/min(X,W), checked spec. */
    @Test
    public void testISS0522_AggregateAllSwiForms() {
        assertTrue(succeeds("aggregate_all(max(X, W), member(X-W, [1-a, 3-b, 2-c]), M), M == max(3, b)"));
        assertTrue(succeeds("aggregate_all(min(X, W), member(X-W, [2-a, 1-b, 3-c]), M), M == min(1, b)"));
        assertFalse(succeeds("aggregate_all(max(X, W), member(X-W, []), _)"));
        assertTrue(succeeds("aggregate_all(sum(X*2), member(X, [1, 2]), S), S == 6"));
        assertTrue(succeeds("aggregate_all(max(X+1), member(X, [1, 5, 3]), M), M == 6"));
        assertTrue(succeeds("aggregate_all(min(X-1), member(X, [4, 2, 3]), M), M == 1"));
        assertTrue(succeeds("aggregate_all(count(X), member(X, [a, b]), C), C == 2"));
        assertTrue(succeeds("aggregate_all(count, member(_, [a, b, c]), C), C == 3"));
        assertTrue(succeeds("aggregate_all(sum(X), fail, S), S == 0"));
        assertTrue(succeeds("aggregate_all(bag(X), member(X, [b, a, b]), L), L == [b, a, b]"));
        assertTrue(succeeds("aggregate_all(set(X), member(X, [b, a, b]), L), L == [a, b]"));
        assertTrue(succeeds("catch(aggregate_all(foo, true, _), error(domain_error(aggregate_spec, foo), _), true)"));
        assertTrue(succeeds("catch(aggregate_all(foo(_, _, _), true, _), error(domain_error(aggregate_spec, _), _), true)"));
        assertTrue(succeeds("catch(aggregate_all(_, true, _), error(instantiation_error, _), true)"));
        assertTrue(succeeds("catch(aggregate_all(sum(X), member(X, [1, a]), _), error(type_error(evaluable, a/0), _), true)"));
        // O(1) memory: counting two million solutions builds no list
        assertTrue(succeeds("aggregate_all(count, between(1, 2000000, _), C), C == 2000000"));
        assertTrue(succeeds("aggregate_all(sum(X), between(1, 1000000, X), S), S == 500000500000"));
    }

    // ------------------------------------------------------------------ P1.10

    /** P1.10: an abandoned query (early stop, budget, cancel) still runs its pending cleanups. */
    @Test(timeout = 60000)
    public void testISS0523_CleanupRunsWhenQueryIsAbandoned() throws Exception {
        // (a) the solveStream sink stops after the first answer
        String out = output(() -> prolog.solveStream(
            "setup_call_cleanup(true, member(X, [1, 2, 3]), writeln(c))", s -> false));
        assertEquals("c\n", out);

        // (b) the inference budget aborts the goal
        prolog.consult("spin :- spin.\n");
        prolog.setInferenceBudget(20000);
        try {
            prolog.solve("setup_call_cleanup(true, spin, nb_setval(p1_cleaned, budget)).");
            fail("the budget must abort the query");
        } catch (InferenceLimitException expected) {
            // the primary control exception still propagates
        }
        prolog.setInferenceBudget(0);
        assertTrue(succeeds("nb_getval(p1_cleaned, V), V == budget"));

        // (b') the same inside a nested drive (findall/3)
        prolog.setInferenceBudget(20000);
        try {
            prolog.solve("findall(x, setup_call_cleanup(true, spin, nb_setval(p1_nested, yes)), _).");
            fail("the budget must abort the query");
        } catch (InferenceLimitException expected) {
        }
        prolog.setInferenceBudget(0);
        assertTrue(succeeds("nb_getval(p1_nested, V), V == yes"));

        // (c) cancellation (thread interrupt) of a running query
        final CountDownLatch started = new CountDownLatch(1);
        final AtomicReference<Throwable> err = new AtomicReference<Throwable>();
        prolog.consult("spin2 :- nb_setval(p1_started, yes), spin3.\nspin3 :- spin3.\n");
        Thread t = new Thread(() -> {
            try {
                prolog.solve("setup_call_cleanup(true, (spin_mark, spin2), nb_setval(p1_cancel, done)).");
            } catch (Throwable e) {
                err.set(e);
            }
        });
        prolog.consult("spin_mark.\n");
        // the latch is released by polling the global the goal sets once it is running
        t.start();
        long deadline = System.currentTimeMillis() + 20000;
        while (System.currentTimeMillis() < deadline) {
            if (succeedsQuietly("nb_getval(p1_started, yes)")) { started.countDown(); break; }
            Thread.sleep(5);
        }
        assertTrue("the goal never started", started.await(1, TimeUnit.SECONDS));
        t.interrupt();
        t.join(20000);
        assertFalse(t.isAlive());
        assertTrue("cancellation surfaces as QueryCancelledException, got " + err.get(),
            err.get() instanceof QueryCancelledException);
        assertTrue(succeeds("nb_getval(p1_cancel, V), V == done"));
    }

    private boolean succeedsQuietly(String q) {
        try { return succeeds(q); } catch (RuntimeException e) { return false; }
    }

    // ------------------------------------------------------------------ P1.11

    /** P1.11: copy_term/findall/assert copy arbitrarily deep terms completely (no depth cut-off). */
    @Test(timeout = 300000)
    public void testISS0524_DeepTermsCopiedCompletely() {
        prolog.consult(
            "mkv(0, V, V) :- !.\n"
          + "mkv(N, V, f(T, x)) :- N1 is N - 1, mkv(N1, V, T).\n"
          + "mk(0, T, T) :- !.\n"
          + "mk(N, A, T) :- N1 is N - 1, mk(N1, A + 1, T).\n"
          + "innermost(T, I) :- nonvar(T), T = f(A, _), !, innermost(A, I).\n"
          + "innermost(I, I).\n"
          + ":- dynamic(deep/1).\n"
          // the deep checks live in clauses so the huge terms are not answer variables
          + "deep_copy(N) :- mk(N, V, T), copy_term(T, C), C \\== T, term_variables(C, [W]), W \\== V.\n"
          + "deep_findall(N) :- mk(N, V, T), findall(T, true, [C]), term_variables(C, [W]), W \\== V, "
          + "V = 0, C \\== T.\n"
          + ":- dynamic(deepa/1).\n"
          + "deep_assert(N) :- mk(N, V, T), V = 0, assertz(deepa(T)), deepa(C), C == T, retract(deepa(_)).\n");
        // the reported repro: a copy past depth 2000 shared the original cells
        assertTrue(succeeds("mkv(3000, V, T), copy_term(T, C), C \\== T"));
        assertTrue(succeeds("mkv(3000, V, T), findall(T, true, [C]), innermost(C, I), I \\== V, var(I)"));
        // assert: the binding at depth 3000 must be stored, not the (later undone) cell
        assertTrue(succeeds("mkv(3000, V, T), (V = z ; true), assertz(deep(T)), fail ; true"));
        assertTrue(succeeds("deep(D), innermost(D, I), I == z"));
        // very deep left-nested terms: 1e5 and 1e6 levels
        for (String n : new String[] {"100000", "1000000"}) {
            assertTrue("copy_term at depth " + n, succeeds("deep_copy(" + n + ")"));
            assertTrue("findall at depth " + n, succeeds("deep_findall(" + n + ")"));
        }
        // the assert path (clause compilation, Rule construction, head instantiation)
        assertTrue("assertz at depth 100000", succeeds("deep_assert(100000)"));
    }

    // ------------------------------------------------------------------ P1.12

    /** P1.12: concurrent assertz/retract on one predicate never duplicate or lose a clause. */
    @Test(timeout = 300000)
    public void testISS0525_ConcurrentAssertsAreNotDuplicated() throws Exception {
        prolog.consult(":- dynamic(item/1).\n");
        final int threads = 8, per = 2000;
        for (int rep = 0; rep < 20; rep++) {
            runConcurrently(threads, k -> "forall(between(1, " + per + ", I), (K is " + k + " * 100000 + I, assertz(item(K))))");
            List<Map<String, Term>> r = prolog.solve("aggregate_all(count, item(_), N), findall(K, item(K), Ks), sort(Ks, S), length(S, U).");
            assertEquals("rep " + rep + ": clause count", String.valueOf(threads * per), text(r.get(0).get("N")));
            assertEquals("rep " + rep + ": distinct clauses", String.valueOf(threads * per), text(r.get(0).get("U")));
            runConcurrently(threads, k -> "forall(between(1, " + per + ", I), (K is " + k + " * 100000 + I, retract(item(K))))");
            r = prolog.solve("aggregate_all(count, item(_), N).");
            assertEquals("rep " + rep + ": after retract", "0", text(r.get(0).get("N")));
        }
        // the same through thread_create/3 workers
        assertTrue(succeeds("findall(Id, (between(1, 4, T), thread_create(forall(between(1, 200, I), "
            + "(K is T * 1000 + I, assertz(item(K)))), Id, [])), Ids), forall(member(Id, Ids), thread_join(Id, _)), "
            + "aggregate_all(count, item(_), 800), findall(K, item(K), Ks), sort(Ks, S), length(S, 800)"));
    }

    private interface Job { String query(int k); }

    private void runConcurrently(int n, final Job job) throws Exception {
        final CountDownLatch go = new CountDownLatch(1);
        final AtomicReference<Throwable> err = new AtomicReference<Throwable>();
        Thread[] ts = new Thread[n];
        for (int i = 0; i < n; i++) {
            final int k = i + 1;
            ts[i] = new Thread(() -> {
                try {
                    go.await();
                    if (prolog.solve(job.query(k) + ".").isEmpty()) err.compareAndSet(null, new AssertionError("failed: " + k));
                } catch (Throwable e) {
                    err.compareAndSet(null, e);
                }
            });
            ts[i].start();
        }
        go.countDown();
        for (Thread t : ts) t.join(120000);
        if (err.get() != null) throw new AssertionError("worker failed", err.get());
    }

    // ------------------------------------------------------------------ P1.13

    /** P1.13: nb_setval/2 stores a copy; later bindings of the caller's variables do not reach it. */
    @Test
    public void testISS0526_NbSetvalStoresACopy() {
        assertEquals(2, count("nb_setval(p1_k, f(X)), (X = 1 ; X = 2)"));
        assertTrue(succeeds("nb_getval(p1_k, V), V = f(A), var(A)"));
        // b_setval/2 keeps its backtrackable semantics
        assertTrue(succeeds("b_setval(p1_b, 1), b_getval(p1_b, 1)"));
    }

    // ------------------------------------------------------------------ P1.14

    /** P1.14: a cyclic term cannot be asserted or stored in a global variable. (On 4.4.0 the
     *  cyclic clause WAS stored and calling it looped, hence the timeout.) */
    @Test(timeout = 60000)
    public void testISS0527_AssertingCyclicTermRaises() {
        prolog.consult(":- dynamic(cyc/1).\n");
        assertTrue(succeeds("X = f(X), catch(assertz(cyc(X)), error(representation_error(cyclic_term), _), true)"));
        assertTrue(succeeds("X = g(X, a), catch(asserta(cyc(X)), error(representation_error(cyclic_term), _), true)"));
        assertTrue(succeeds("X = [1|X], catch(assertz((cyc(a) :- member(_, X))), error(representation_error(cyclic_term), _), true)"));
        assertFalse("nothing was stored", succeeds("cyc(_)"));
        assertTrue(succeeds("X = f(X), catch(nb_setval(p1_cyc, X), error(representation_error(cyclic_term), _), true)"));
        // acyclic terms with shared sub-terms are fine
        assertTrue(succeeds("Y = g(a), X = f(Y, Y), assertz(cyc(X)), cyc(f(g(a), g(a)))"));
    }

    // ------------------------------------------------------------------ P1.15

    private String trace(String program, String query) {
        Prolog p = new Prolog();
        if (!program.isEmpty()) p.consult(program);
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(bos, true);
        PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            p.setTracing(true);
            p.solve(query);
        } finally {
            p.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        return bos.toString().replaceAll("_G[0-9]+", "_G").replace("\r\n", "\n").trim();
    }

    /** P1.15: after a catch, the recovery runs at the catch goal's depth, and the unwound goal
     *  shows an Exception port. */
    @Test
    public void testISS0528_TraceDepthAfterCatch() {
        String t = trace("w(X) :- catch(thr, _, X = caught).\nthr :- throw(oops).\n", "w(X)");
        assertEquals(String.join("\n",
            "Call: (0) w(X)",
            "  Call: (1) thr",
            "  Exception: (1) thr",
            "  Call: (1) X=caught",
            "  Exit: (1) caught=caught",
            "Exit: (0) w(caught)"), t);
    }

    // ------------------------------------------------------------------ P1.16

    /** P1.16: an unrelated breakpoint must not make every port snapshot (resolve) its goal. */
    @Test(timeout = 120000)
    public void testISS0529_DebuggerBreakpointDoesNotSnapshotEveryPort() {
        prolog.consult("len([], 0).\nlen([_|T], N) :- len(T, M), N is M + 1.\nunrelated.\n");
        DebugController dc = new DebugController();
        dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        dc.addBreakpoint("unrelated/0");
        prolog.getEngineContext().setDebugController(dc);
        try {
            assertTrue(succeeds("numlist(1, 2000, L), len(L, N), N == 2000"));
            assertEquals("no port of len/2 needs a goal snapshot", 0L, dc.getGoalSnapshotCount());
            // a breakpoint on the predicate itself still gets its snapshot
            dc.addBreakpoint("len/2", null, null, Integer.MAX_VALUE);   // never pauses
            assertTrue(succeeds("len([a, b], N), N == 2"));
            assertTrue(dc.getGoalSnapshotCount() > 0);
        } finally {
            prolog.getEngineContext().setDebugController(null);
        }
    }
}
// END_CHANGE: ISS-2025-0514..0529
