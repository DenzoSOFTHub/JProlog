package it.denzosoft.jprolog.test.performance;

import it.denzosoft.jprolog.core.engine.Prolog;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * START_CHANGE: ISS-2025-0667 - performance regressions of the 4.5 wave P2 items, as GROWTH
 * tests.
 *
 * <p>Every check here compares the same workload at size N and at size 4N, in the same JVM,
 * after a warm-up, taking the minimum of three interleaved rounds of each: a linear (or
 * n log n) implementation grows about 4-5x, the quadratic ones P2 removed grew 16x. The bound is
 * a generous 10x plus a small absolute slack, so a loaded machine (GC, JIT, other processes)
 * does not make the test flaky, while a return to quadratic behaviour fails it. There is no
 * tight absolute time bound anywhere; the one absolute bound (bagof over 40 000 witnesses) is
 * ~10x what the 4.5 build needs on a loaded machine and ~100x below what 4.4.0 took.
 * The call-site/view counters that pin the mechanisms themselves live in
 * {@code core.engine.v4.EngineV45PerformanceTest}. END_CHANGE: ISS-2025-0667
 */
public class PerformanceRegressionTest {

    // START_CHANGE: ISS-2025-0667 - verifier: 50 ms slack was below one GC pause on the shared box
    private static final long SLACK_NS = 100_000_000L;     // 100 ms of noise allowance
    // END_CHANGE: ISS-2025-0667
    private static final double MAX_GROWTH = 10.0;         // linear ~4, quadratic ~16

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
        prolog.consult(
            ":- dynamic(rp/1).\n:- dynamic(ra/1).\n:- dynamic(vh/2).\n:- dynamic(ks/2).\n:- dynamic(w/2).\n"
          + "fill(N) :- forall(between(1, N, I), assertz(rp(I))).\n"
          + "rloop :- retract(rp(_)), !, rloop.\n"
          + "rloop.\n"
          + "keyloop(N) :- forall(between(1, N, I), (assertz(ks(I, x)), assertz(ks(I, y)))),\n"
          + "    forall(between(1, N, I), retract(ks(I, y))), retractall(ks(_, _)).\n"
          + "vhloop(N) :- retractall(vh(_, _)), assertz(vh(_, any)),\n"
          + "    forall(between(1, N, I), (assertz(vh(I, v)), vh(I, v), !)), retractall(vh(_, _)).\n"
          + "interleave(N) :- forall(between(1, N, I), (assertz(ra(I)), retract(ra(I)))).\n"
          + "cmp_num(O, A, B) :- compare(O, A, B).\n"
          + "rev_list(N, L) :- numlist(1, N, L0), reverse(L0, L).\n"
          + "wfill(N) :- retractall(w(_, _)), forall(between(1, N, I), assertz(w(I, I))).\n");
    }

    private long run(String goal) {
        long t0 = System.nanoTime();
        assertEquals(goal, 1, prolog.solve("once((" + goal + ")).").size());
        return System.nanoTime() - t0;
    }

    /** {@code template} has one {@code %d} for the size. */
    private void assertLinear(String what, String template, int n) {
        run(String.format(template, n));                    // warm-up
        run(String.format(template, 4 * n));
        long small = Long.MAX_VALUE, big = Long.MAX_VALUE;
        for (int round = 0; round < 3; round++) {
            small = Math.min(small, run(String.format(template, n)));
            big = Math.min(big, run(String.format(template, 4 * n)));
        }
        assertTrue(what + ": N=" + n + " took " + small / 1000000 + " ms, 4N took " + big / 1000000
            + " ms (growth " + String.format("%.1f", (double) big / small) + "x, bound "
            + MAX_GROWTH + "x)", big < MAX_GROWTH * small + SLACK_NS);
    }

    /** P2.5 (ISS-2025-0544/0545): retractall/1 over N clauses was quadratic in the KB. */
    @Test(timeout = 180000)
    public void testRetractallIsLinear() {
        assertLinear("retractall", "fill(%d), retractall(rp(_))", 25000);
    }

    /** P2.6 (ISS-2025-0546): the retract-first-clause loop copied the predicate per retract. */
    @Test(timeout = 180000)
    public void testRetractFirstLoopIsLinear() {
        assertLinear("retract loop", "fill(%d), rloop", 20000);
    }

    /** P2.7: asserta/1 in a loop was quadratic. */
    @Test(timeout = 180000)
    public void testAssertaLoopIsLinear() {
        assertLinear("asserta loop", "forall(between(1, %d, I), asserta(ra(I))), retractall(ra(_))", 20000);
    }

    /** P2.8: retract with a bound first argument in a big predicate. */
    @Test(timeout = 180000)
    public void testBoundKeyRetractIsLinear() {
        assertLinear("bound-key retract", "keyloop(%d)", 10000);
    }

    /** P2.9 (ISS-2025-0547): assert + indexed call with a variable-headed clause present. */
    @Test(timeout = 180000)
    public void testAssertThenIndexedCallWithVariableHeadIsLinear() {
        assertLinear("assert + indexed call", "vhloop(%d)", 5000);
    }

    /** Interleaved assert/retract of one clause keeps the predicate small: linear overall. */
    @Test(timeout = 180000)
    public void testAssertRetractInterleaveIsLinear() {
        // START_CHANGE: ISS-2025-0667 - verifier: at N=25000 the base run is ~20 ms, all noise
        assertLinear("assert/retract interleave", "interleave(%d)", 100000);
        // END_CHANGE: ISS-2025-0667
    }

    /** P2.10 (ISS-2025-0548): append(_, [Last], L) allocated quadratically. */
    @Test(timeout = 180000)
    public void testAppendSplitModeIsLinear() {
        assertLinear("append(_, [Last], L)", "numlist(1, %d, L), append(_, [X], L), X == %<d", 10000);
    }

    /** P2.11 (ISS-2025-0549): predsort is n log n (a reversed list was a worst case). */
    @Test(timeout = 180000)
    public void testPredsortIsNLogN() {
        assertLinear("predsort", "rev_list(%d, L), predsort(cmp_num, L, S), S = [1|_]", 20000);
    }

    /** P1.7 (ISS-2025-0520): bagof/setof grouping was quadratic in the number of witnesses. */
    @Test(timeout = 180000)
    public void testBagofGroupingIsNotQuadratic() {
        assertLinear("bagof groups", "wfill(%d), aggregate_all(count, bagof(X, w(X, _Y), _), C), C == %<d", 10000);
        prolog.solve("wfill(40000).");
        long t = run("aggregate_all(count, bagof(X, w(X, _Y), _), C), C == 40000");
        assertTrue("bagof with 40 000 witnesses took " + t / 1000000 + " ms (4.4.0: minutes)",
            t < 20_000_000_000L);
    }

    /** The maplist/foldl family goes through module call sites (ISS-2025-0549). */
    @Test(timeout = 180000)
    public void testMaplistFoldlAreLinear() {
        assertLinear("maplist/foldl",
            "numlist(1, %d, L), maplist(succ, L, L1), foldl(plus, L1, 0, S), integer(S)", 20000);
    }
}
