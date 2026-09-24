package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.InferenceLimitException;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.QueryCancelledException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;
import java.util.TreeSet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0463..0465 - engine v4 wave W5 (linear tabling with completion).
/**
 * Acceptance tests for wave W5 of the v4 engine: tabling.
 *
 * <p>Every test in this class fails on the pre-W5 engine, where a tabled call was delegated to the
 * recursive {@code QuerySolver.solveWithTabling} — a bounded 100-iteration re-evaluation over
 * name-keyed answer maps that returns <b>wrong answers</b> for a left-recursive predicate with a
 * bound argument (design limit L-03: {@code path(1, 51)} fails on a 3 000-edge chain while
 * {@code findall(Y, path(1,Y), L)} finds all 3 000 answers).
 *
 * <p>Like the other {@code EngineV4*Test} classes, every test selects v4 in {@link #setUp} and
 * restores the previous selection in {@link #tearDown}, so the class behaves identically under the
 * default profile and under {@code -Pengine-v4}. Sizes are chosen to pass under the surefire fork's
 * default JVM settings (no {@code -Xmx}, no {@code -Xss}); the full acceptance numbers — the
 * 3 000-edge chain in all three recursion shapes and the 100 000-edge chain — are in
 * {@code docs/reports/report-engine-v4-progress.md} section 11.
 */
public class EngineV4TablingTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    @After
    public void tearDown() {
    }

    /** {@code edge/2} as a generator plus one of the three recursion shapes of the acceptance set. */
    private static String chain(int n, String recursion) {
        return "edge(I, J) :- between(1, " + n + ", I), J is I + 1.\n"
             + ":- table path/2.\n"
             + "path(X, Y) :- edge(X, Y).\n"
             + recursion;
    }

    private static final String LEFT   = "path(X, Y) :- path(X, Z), edge(Z, Y).\n";
    private static final String RIGHT  = "path(X, Y) :- edge(X, Z), path(Z, Y).\n";
    private static final String DOUBLE = "path(X, Y) :- path(X, Z), path(Z, Y).\n";

    private void assertSucceeds(String query) {
        assertFalse("expected a solution for: " + query, prolog.solve(query).isEmpty());
    }

    private void assertFails(String query) {
        assertTrue("expected no solution for: " + query, prolog.solve(query).isEmpty());
    }

    // ================================================================ ISS-2025-0463 (the algorithm)

    /**
     * The headline L-03 repro. On the bounded re-evaluation loop {@code path(1, 3001)} and
     * {@code path(1, 51)} FAIL while the open variant finds every answer; with linear tabling all
     * three agree, at the default JVM stack.
     */
    @Test(timeout = 120000)
    public void testISS0463_LeftRecursiveChainIsCorrect() {
        prolog.consult(chain(3000, LEFT));
        assertSucceeds("path(1, 3001).");
        assertSucceeds("path(1, 51).");
        assertSucceeds("findall(Y, path(1, Y), L), length(L, 3000).");
        assertFails("path(1, 1).");
        assertFails("path(1, 3002).");
    }

    /** The same predicate written with right recursion. */
    @Test(timeout = 120000)
    public void testISS0463_RightRecursiveChainIsCorrect() {
        prolog.consult(chain(3000, RIGHT));
        assertSucceeds("path(1, 3001).");
        assertSucceeds("path(1, 51).");
        assertFails("path(1, 1).");
        // The full closure of a right-recursive chain materialises one table per node
        // (n*(n+1)/2 answers in total), so the findall form is checked at a size that fits the
        // surefire fork's default heap; section 11 of the progress report has the 3 000 run.
        Prolog small = new Prolog();
        small.consult(chain(300, RIGHT));
        assertFalse(small.solve("findall(Y, path(1, Y), L), length(L, 300).").isEmpty());
    }

    /**
     * {@code path(X,Y) :- path(X,Z), path(Z,Y)} — both body goals tabled and recursive. The join is
     * inherently cubic in the chain length for <i>any</i> tabling system (n subgoals, each joining
     * O(n) answers with O(n) answers), so the size here is modest; what matters is that the answers
     * are exactly right and that the evaluation terminates.
     */
    @Test(timeout = 120000)
    public void testISS0463_DoublyRecursiveDefinitionIsCorrect() {
        prolog.consult(chain(120, DOUBLE));
        assertSucceeds("path(1, 121).");
        assertSucceeds("path(1, 51).");
        assertFails("path(1, 1).");
        assertSucceeds("findall(Y, path(1, Y), L), length(L, 120).");
    }

    /** A bound variant and an open variant of the same predicate must agree, answer for answer. */
    @Test(timeout = 120000)
    public void testISS0463_BoundAndOpenVariantsAgree() {
        prolog.consult(chain(400, LEFT));
        List<Map<String, Term>> sols = prolog.solve("findall(Y, path(1, Y), L), sort(L, S).");
        assertEquals(1, sols.size());
        // every answer of the open variant succeeds as a bound variant ...
        assertSucceeds("findall(Y, path(1, Y), L), forall(member(Y, L), path(1, Y)).");
        // ... and every non-answer in range fails as a bound variant
        assertSucceeds("forall((between(1, 402, K), \\+ (K > 1, K < 402)), \\+ path(1, K)).");
        assertFails("path(1, 402).");
        assertSucceeds("path(1, 402) ; true.");
    }

    /** Classic memoisation: exponential naive Fibonacci becomes linear, and 1 000 is exact. */
    @Test(timeout = 60000)
    public void testISS0463_TabledFibonacci() {
        prolog.consult(":- table fib/2.\n"
                     + "fib(0, 0).\n"
                     + "fib(1, 1).\n"
                     + "fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.\n");
        assertSucceeds("fib(30, 832040).");
        assertSucceeds("fib(1000, F), F > 0, number_codes(F, C), length(C, 209).");
        // the exact value of fib(1000)
        List<Map<String, Term>> s = prolog.solve("fib(1000, F).");
        assertEquals(1, s.size());
        assertEquals("4346655768693745643568852767504062580256466051737178040248172908953655541794905"
                   + "1890403879840079255169295922593080322634775209689623239873322471161642996440906"
                   + "533187938298969649928516003704476137795166849228875",
            String.valueOf(s.get(0).get("F")));
    }

    /** Two tabled predicates in one SCC: neither can complete on its own. */
    @Test(timeout = 60000)
    public void testISS0463_MutualRecursionAcrossTwoTabledPredicates() {
        prolog.consult(":- table p/1.\n:- table q/1.\n"
                     + "p(X) :- q(X).\np(1).\n"
                     + "q(X) :- p(X).\nq(2).\n");
        assertSucceeds("findall(X, p(X), L), sort(L, [1, 2]).");
        assertSucceeds("findall(X, q(X), L), sort(L, [1, 2]).");
        assertSucceeds("p(1).");
        assertSucceeds("p(2).");
        assertFails("p(3).");

        Prolog eo = new Prolog();
        eo.consult(":- table even/1.\n:- table odd/1.\n"
                 + "even(0).\n"
                 + "even(N) :- N > 0, N1 is N - 1, odd(N1).\n"
                 + "odd(N) :- N > 0, N1 is N - 1, even(N1).\n");
        assertFalse(eo.solve("even(400).").isEmpty());
        assertTrue(eo.solve("odd(400).").isEmpty());
        assertFalse(eo.solve("odd(401).").isEmpty());
    }

    /** A cyclic graph: without tabling the left-recursive closure does not terminate at all. */
    @Test(timeout = 60000)
    public void testISS0463_CyclicGraphTerminates() {
        prolog.consult("edge(a,b). edge(b,c). edge(c,a). edge(b,d).\n"
                     + ":- table path/2.\n"
                     + "path(X, Y) :- edge(X, Y).\n"
                     + "path(X, Y) :- path(X, Z), edge(Z, Y).\n");
        assertSucceeds("findall(Y, path(a, Y), L), sort(L, [a, b, c, d]).");
        assertSucceeds("path(a, a).");
        assertFails("path(d, _).");
    }

    /** A tabled call inside the opaque meta-constructs. */
    @Test(timeout = 60000)
    public void testISS0463_TabledCallInsideFindallNegationAndCatch() {
        prolog.consult(chain(200, LEFT));
        assertSucceeds("findall(Y, path(1, Y), L), length(L, 200).");
        assertSucceeds("\\+ path(1, 1).");
        assertFails("\\+ path(1, 5).");
        assertSucceeds("catch(path(1, 201), _, fail).");
        assertSucceeds("once(path(1, Y)), Y == 2.");
        assertSucceeds("forall(member(K, [2, 3, 4, 201]), path(1, K)).");
        assertSucceeds("aggregate_all(count, path(1, _), 200).");
        assertSucceeds("bagof(Y, path(1, Y), L), length(L, 200).");
    }

    /** A {@code !} in a tabled clause body is local to that body: it prunes the body's own choice
     *  points and never aborts the production of the table. */
    @Test(timeout = 60000)
    public void testISS0463_CutInsideATabledClauseBodyIsLocal() {
        prolog.consult(":- table t/1.\n"
                     + "t(X) :- member(X, [1, 2, 3]), !.\n"
                     + "t(9).\n");
        assertSucceeds("findall(X, t(X), L), L == [1, 9].");
        assertSucceeds("t(9).");
        // t(2) is its OWN variant: its production runs member(2,[1,2,3]) and the cut, exactly as
        // untabled Prolog does, so it succeeds. The cut never truncated the t(X) table above.
        assertSucceeds("t(2).");
        assertFails("t(4).");
        assertSucceeds("findall(X, t(X), L), L == [1, 9].");
    }

    /**
     * An exception thrown out of a tabled evaluation must not leave a half-built table behind: the
     * table is discarded, so the next call recomputes it and raises again. The bug this guards is
     * "status stays EVALUATING", which would make the second call read a partial answer list.
     */
    @Test(timeout = 60000)
    public void testISS0463_ExceptionLeavesNoPartialTable() {
        prolog.consult(":- table bb/1.\n"
                     + "bb(1).\n"
                     + "bb(X) :- X = 2, throw(oops).\n"
                     + "bb(3).\n");
        assertSucceeds("catch(bb(_), oops, true).");
        // second call: the table must NOT be readable as a complete one-answer table
        assertSucceeds("catch(bb(_), oops, true).");
        assertFails("catch(bb(_), oops, fail).");
        // and the budget/interrupt case leaves the same clean state
        Prolog b = new Prolog();
        b.consult(chain(3000, LEFT));
        b.setInferenceBudget(5000);
        try {
            b.solve("path(1, 3001).");
            fail("the inference budget must abort a tabled evaluation");
        } catch (InferenceLimitException expected) {
            // the trust model: a plain RuntimeException, never a PrologException
        }
        b.setInferenceBudget(0);
        assertFalse("the aborted table must have been discarded", b.solve("path(1, 3001).").isEmpty());
    }

    /** The budget and the Stop interrupt reach INSIDE the fixpoint (they never did on the legacy
     *  tabling driver, which ran its whole re-evaluation loop outside the guard). */
    @Test(timeout = 120000)
    public void testISS0463_BudgetAndInterruptAbortATabledEvaluation() {
        Prolog b = new Prolog();
        b.consult(chain(100000, LEFT));
        b.setInferenceBudget(50000);
        try {
            b.solve("path(1, 100001).");
            fail("expected InferenceLimitException");
        } catch (InferenceLimitException expected) {
            assertTrue(expected.getMessage().contains("50000"));
        }

        final Prolog c = new Prolog();
        c.consult(chain(100000, LEFT));
        final Throwable[] caught = new Throwable[1];
        // ISS-2025-0664: latch-synchronised — the interrupt is sent once the query runs
        final it.denzosoft.jprolog.test.support.QueryStartLatch latch =
            new it.denzosoft.jprolog.test.support.QueryStartLatch();
        Thread t = new Thread(new Runnable() {
            @Override public void run() {
                try {
                    latch.solve(c, it.denzosoft.jprolog.test.support.QueryStartLatch.ANNOUNCE + "path(1, 100001).");
                } catch (Throwable e) { caught[0] = e; }
            }
        });
        t.start();
        try {
            latch.await(20);
            Thread.sleep(150);        // into the fixpoint
            t.interrupt();
            t.join(60000);
        } catch (InterruptedException ie) {
            Thread.currentThread().interrupt();
            fail("test thread interrupted");
        }
        assertFalse("the solver thread must have stopped", t.isAlive());
        assertTrue("expected QueryCancelledException, got " + caught[0],
            caught[0] instanceof QueryCancelledException);
    }

    /** Invariant 14: the generator/consumer choice point carries traceGoal/traceDepth, so a tabled
     *  call emits the same four ports as any other predicate. */
    @Test(timeout = 60000)
    public void testISS0463_TabledCallEmitsFourPorts() {
        prolog.consult("edge(a,b). edge(b,c).\n"
                     + ":- table path/2.\n"
                     + "path(X, Y) :- edge(X, Y).\n"
                     + "path(X, Y) :- path(X, Z), edge(Z, Y).\n");
        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.io.PrintStream ps = new java.io.PrintStream(bos);
        java.io.PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            prolog.setTracing(true);
            prolog.solve("path(a, W).");
        } finally {
            prolog.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        String out = bos.toString();
        assertTrue("Call port missing:\n" + out, out.contains("Call: (0) path(a,W)"));
        assertTrue("Exit port missing:\n" + out, out.contains("Exit: (0) path(a,b)"));
        assertTrue("Redo port missing:\n" + out, out.contains("Redo: (0) path(a,c)"));
        assertTrue("Fail port missing:\n" + out, out.contains("Fail: (0) path(a,W)"));
    }

    // ================================================================ ISS-2025-0464 (the built-ins)

    /** {@code abolish_all_tables/0} really drops the ANSWERS, observed through a side effect. */
    @Test(timeout = 60000)
    public void testISS0464_AbolishAllTablesForcesRecomputation() {
        prolog.consult(":- dynamic hits/1.\nhits(0).\n"
                     + ":- table se/1.\n"
                     + "se(a) :- retract(hits(N)), N1 is N + 1, assertz(hits(N1)).\n");
        assertSucceeds("se(a).");
        assertSucceeds("se(a).");
        assertSucceeds("se(a).");
        assertSucceeds("hits(1).");
        assertSucceeds("abolish_all_tables.");
        assertSucceeds("se(a).");
        assertSucceeds("hits(2).");
    }

    /** {@code abolish_table/1} clears one predicate (and, as on v2, un-declares it), and
     *  {@code current_table/2} reports what is in the store. */
    @Test(timeout = 60000)
    public void testISS0464_AbolishTableAndCurrentTable() {
        prolog.consult("edge(a,b). edge(b,c).\n"
                     + ":- table path/2.\n"
                     + "path(X, Y) :- edge(X, Y).\n"
                     + "path(X, Y) :- path(X, Z), edge(Z, Y).\n");
        assertFails("current_table(_, _).");
        assertSucceeds("path(a, c), current_table(path(a, c), complete).");
        assertSucceeds("abolish_table(path/2).");
        assertFails("current_table(_, _).");
        // ISO errors instead of the legacy silent failure
        assertSucceeds("catch(abolish_table(_), error(instantiation_error, _), true).");
        assertSucceeds("catch(abolish_table(foo), error(type_error(predicate_indicator, foo), _), true).");
    }

    /** The documented invalidation policy: asserting to or retracting from a tabled predicate drops
     *  that predicate's tables. */
    @Test(timeout = 60000)
    public void testISS0464_AssertAndRetractInvalidateTheTable() {
        prolog.consult(":- dynamic tp/1.\n:- table tp/1.\ntp(1).\n");
        assertSucceeds("findall(X, tp(X), [1]).");
        assertSucceeds("assertz(tp(2)).");
        assertSucceeds("findall(X, tp(X), [1, 2]).");
        assertSucceeds("retract(tp(1)).");
        assertSucceeds("findall(X, tp(X), [2]).");
    }

    // START_CHANGE: ISS-2025-0755 - tnot/1 is implemented since 4.6 (wave Q4.5): repinned
    /** {@code tnot/1} over a complete table: fails on an answer, succeeds on none. */
    @Test(timeout = 30000)
    public void testISS0464_TnotRaisesAnExistenceError() {
        prolog.consult(":- table p/1.\np(1).\n");
        assertSucceeds("\\+ tnot(p(1)), tnot(p(2)).");
    }
    // END_CHANGE: ISS-2025-0755

    // ================================================================ ISS-2025-0465

    /**
     * The W5 half of the W3 oracle: after deleting {@code Machine.tabledDelegate}, a tabled query
     * must not enter {@code QuerySolver.solveInternal} either. Before W5 the same queries entered
     * it thousands of times.
     */
    @Test(timeout = 60000)
    public void testISS0450_NoBuiltinReachesTheRecursiveSolverForTabledQueries() {
        prolog.consult(chain(200, LEFT));
        prolog.consult(":- table fibt/2.\nfibt(0,0).\nfibt(1,1).\n"
                     + "fibt(N,F) :- N > 1, N1 is N-1, N2 is N-2, fibt(N1,F1), fibt(N2,F2), F is F1+F2.\n");
        // ISS-2025-0484 - wave W9: the recursive solver is deleted; assert that structurally.
        // ISS-2025-0665: the "QuerySolver is deleted" check lives once, in EngineV4RetirementTest
        assertSucceeds("path(1, 201).");
        assertSucceeds("path(1, 51).");
        assertSucceeds("findall(Y, path(1, Y), L), length(L, 200).");
        assertSucceeds("fibt(60, 1548008755920).");
        assertSucceeds("\\+ path(1, 1).");
        assertSucceeds("abolish_all_tables.");
        assertSucceeds("path(1, 201).");
    }

    // ================================================================ engine-independent oracle

    /**
     * The oracle: random directed graphs (acyclic and cyclic), reachability computed by tabled
     * left-recursive transitive closure and compared against (a) a breadth-first search in Java and
     * (b) a findall-based, non-tabled transitive closure evaluated in Prolog. Both the open variant
     * {@code path(I, Y)} and every bound variant {@code path(I, K)} are checked.
     *
     * <p>ISS-2025-0491 (4.1 wave A): it used to run the whole comparison twice, once per engine.
     * There is one engine now, so it runs once — the oracle itself (findall-based transitive
     * closure vs the tabled predicate) is what makes it valuable, not the engine loop.
     */
    @Test(timeout = 240000)
    public void testOracle_RandomGraphReachabilityMatchesTransitiveClosure() {
        for (long seed : new long[]{1L, 7L, 99L}) {
            for (boolean cyclic : new boolean[]{false, true}) {
                checkGraph("v4", seed, cyclic, 15);
            }
        }
    }

    private void checkGraph(String engine, long seed, boolean cyclic, int n) {
        Random rnd = new Random(seed);
        boolean[][] e = new boolean[n + 1][n + 1];
        StringBuilder facts = new StringBuilder();
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= n; j++) {
                if (i == j) continue;
                if (!cyclic && j < i) continue;              // upper triangle == a DAG
                if (rnd.nextInt(6) != 0) continue;
                e[i][j] = true;
                facts.append("edge(").append(i).append(",").append(j).append(").\n");
            }
        }
        Map<Integer, TreeSet<Integer>> truth = new TreeMap<Integer, TreeSet<Integer>>();
        for (int i = 1; i <= n; i++) {
            TreeSet<Integer> r = new TreeSet<Integer>();
            Deque<Integer> st = new ArrayDeque<Integer>();
            st.push(Integer.valueOf(i));
            while (!st.isEmpty()) {
                int x = st.pop().intValue();
                for (int j = 1; j <= n; j++) if (e[x][j] && r.add(Integer.valueOf(j))) st.push(Integer.valueOf(j));
            }
            truth.put(Integer.valueOf(i), r);
        }

        Prolog p = new Prolog();
        p.consult(facts + ":- table path/2.\n"
                + "path(X, Y) :- edge(X, Y).\n"
                + "path(X, Y) :- path(X, Z), edge(Z, Y).\n"
                // the reference closure: no tabling, iterated to a fixpoint with findall/3
                + "closure(Start, R) :- closure_(Start, [], R).\n"
                + "closure_(Start, Acc, R) :-\n"
                + "    findall(Y, (member(X, [Start|Acc]), edge(X, Y)), Ys0),\n"
                + "    sort(Ys0, Ys),\n"
                + "    ( Ys == Acc -> R = Acc ; closure_(Start, Ys, R) ).\n");

        String where = engine + " seed=" + seed + (cyclic ? " cyclic" : " dag");
        for (int i = 1; i <= n; i++) {
            String want = listOf(truth.get(Integer.valueOf(i)));
            List<Map<String, Term>> tabled = p.solve("findall(Y, path(" + i + ", Y), L0), sort(L0, L).");
            assertEquals(where + " i=" + i + ": tabled closure", 1, tabled.size());
            assertEquals(where + " i=" + i + ": tabled closure", want,
                String.valueOf(tabled.get(0).get("L")).replace(" ", ""));
            List<Map<String, Term>> ref = p.solve("closure(" + i + ", L).");
            assertEquals(where + " i=" + i + ": reference closure", 1, ref.size());
            assertEquals(where + " i=" + i + ": reference closure", want,
                String.valueOf(ref.get(0).get("L")).replace(" ", ""));
            for (int k = 1; k <= n; k++) {
                boolean expected = truth.get(Integer.valueOf(i)).contains(Integer.valueOf(k));
                boolean got = !p.solve("path(" + i + ", " + k + ").").isEmpty();
                assertEquals(where + " bound variant path(" + i + "," + k + ")", expected, got);
            }
        }
    }

    private static String listOf(TreeSet<Integer> s) {
        List<String> parts = new ArrayList<String>();
        for (Integer x : s) parts.add(String.valueOf(x));
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < parts.size(); i++) {
            if (i > 0) sb.append(',');
            sb.append(parts.get(i));
        }
        return sb.append(']').toString();
    }
}
// END_CHANGE: ISS-2025-0463..0465
