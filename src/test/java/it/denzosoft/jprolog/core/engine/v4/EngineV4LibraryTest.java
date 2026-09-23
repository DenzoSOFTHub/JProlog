package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.InferenceLimitException;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0450..0456 - engine v4 wave W3 (native library and meta-calls).
/**
 * Acceptance tests for wave W3 of the v4 engine: the control, collection and library built-ins that
 * now run on the machine instead of the recursive {@code QuerySolver}, the Prolog prelude and the
 * yall lambdas.
 *
 * <p>Like {@link EngineV4Test}, every test selects v4 in {@link #setUp} and restores the previous
 * selection in {@link #tearDown}, so the class behaves identically under the default profile and
 * under {@code -Pengine-v4}. Sizes are chosen to pass under the surefire fork's default JVM
 * settings (no {@code -Xmx}, no {@code -Xss}); the full acceptance numbers are in
 * {@code docs/reports/report-engine-v4-progress.md}.
 */
public class EngineV4LibraryTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    @After
    public void tearDown() {
    }

    private static String value(List<Map<String, Term>> sols, int i, String var) {
        return String.valueOf(sols.get(i).get(var));
    }

    // ================================================================ ISS-2025-0450

    /**
     * The point of W3: no built-in reachable from a v4 query may fall back to the recursive
     * algorithm. Before the {@code SolverFacade.solve(Term, Map, List, CutStatus)} override,
     * {@code phrase/2,3}, {@code format ~@}/{@code ~p} and the persistence/DCG helpers all did.
     */
    @Test
    public void testISS0450_NoBuiltinReachesTheRecursiveSolver() {
        prolog.consult(
            "greeting --> [hello], [world].\n"
          + "p(1). p(2). p(3).\n"
          + "q(a, 1). q(a, 2). q(b, 3).\n"
          // START_CHANGE: ISS-2025-0465 - wave W5: a tabled call was the LAST routine path from a
          // v4 query into QuerySolver.solveInternal (Machine.tabledDelegate). It is gone.
          + "edge(1, 2). edge(2, 3). edge(3, 4).\n"
          + ":- table tpath/2.\n"
          + "tpath(X, Y) :- edge(X, Y).\n"
          + "tpath(X, Y) :- tpath(X, Z), edge(Z, Y).\n");
        // ISS-2025-0484 - wave W9: the recursive QuerySolver the probe counted entries into no
        // longer exists. The structural fact replaces the counter; the queries stay.
        // ISS-2025-0665: the "QuerySolver is deleted" check lives once, in EngineV4RetirementTest
        assertEquals(1, prolog.solve("phrase(greeting, [hello, world]).").size());
        assertEquals(1, prolog.solve("bagof(X, Y^q(Y, X), L).").size());
        assertEquals(1, prolog.solve("setof(X, p(X), [1,2,3]).").size());
        assertEquals(1, prolog.solve("aggregate_all(count, p(_), 3).").size());
        assertEquals(1, prolog.solve("forall(p(X), integer(X)).").size());
        assertEquals(1, prolog.solve("with_output_to(atom(A), write(hi)), A == hi.").size());
        assertEquals(1, prolog.solve("with_output_to(atom(A), format(\"~@\", [write(x)])), A == x.").size());
        assertEquals(1, prolog.solve("findall(X, p(X), [1,2,3]).").size());
        assertEquals(1, prolog.solve("maplist(integer, [1,2,3]).").size());
        assertEquals(1, prolog.solve("catch(throw(e), E, true).").size());
        assertEquals(1, prolog.solve("tpath(1, 4).").size());
        assertEquals(1, prolog.solve("findall(Y, tpath(1, Y), [2, 3, 4]).").size());
        assertEquals(1, prolog.solve("abolish_all_tables.").size());
        assertEquals(1, prolog.solve("tpath(1, 4).").size());
        // END_CHANGE: ISS-2025-0465
        // START_CHANGE: ISS-2025-0480 - wave W8: the concurrency predicates were the LAST routine
        // path from a v4 query into the recursive solver (deviation 7 of section 9.4, LIM-024).
        // Each worker now runs on its own Machine over the same Engine.
        assertEquals(1, prolog.solve("thread_create(p(1), T), thread_join(T, true).").size());
        assertEquals(1, prolog.solve("concurrent_maplist(integer, [1,2,3]).").size());
        assertEquals(1, prolog.solve("concurrent_maplist(succ, [1,2], L), L == [2,3].").size());
        assertEquals(1, prolog.solve("first_solution(X, [(X = only)], []), X == only.").size());
        assertEquals(1, prolog.solve("concurrent(2, [p(1), p(2)], []).").size());
        // END_CHANGE: ISS-2025-0480
    }

    // ================================================================ ISS-2025-0451 (phrase)

    /**
     * DCG over a long token list. On the recursive solver this died at the 2 000-deep recursion cap
     * ({@code resource_error(max_recursion_depth)}); native {@code phrase/2} pushes the translated
     * goal onto the machine's goal stack, so the only limit is the heap.
     */
    @Test(timeout = 60000)
    public void testISS0451_PhraseParsesALongTokenList() {
        prolog.consult("digits([]) --> [].\n"
                     + "digits([D|T]) --> [D], digits(T).\n");
        assertEquals(1, prolog.solve("numlist(1, 200000, L), phrase(digits(D), L), length(D, 200000).").size());
    }

    @Test
    public void testISS0451_PhraseKeepsItsIsoErrorsAndModes() {
        prolog.consult("ab --> [a], [b].\n");
        assertEquals(1, prolog.solve("phrase(ab, [a,b]).").size());
        assertEquals(1, prolog.solve("phrase(ab, [a,b,c], R), R == [c].").size());
        assertTrue(prolog.solve("phrase(ab, [a,c]).").isEmpty());
        // a control construct as the body must be translated, not blindly extended
        assertEquals(1, prolog.solve("phrase(([a] ; [b]), [b]).").size());
        assertEquals(1, prolog.solve("phrase(([a], {true}), [a]).").size());
        try {
            prolog.solve("phrase(ab, foo).");
            fail("phrase/2 on a non-list must raise type_error(list, foo)");
        } catch (PrologException e) {
            assertTrue(String.valueOf(e.getErrorTerm()).contains("type_error"));
        }
        try {
            prolog.solve("phrase(_, [a]).");
            fail("phrase/2 with an unbound body must raise instantiation_error");
        } catch (PrologException e) {
            assertTrue(String.valueOf(e.getErrorTerm()).contains("instantiation_error"));
        }
    }

    /** The budget must fire INSIDE phrase, not only around it. */
    @Test(timeout = 30000)
    public void testISS0451_PhraseHonoursTheInferenceBudget() {
        prolog.consult("loopy --> [_], loopy.\n"
                     + "loopy --> [].\n");
        prolog.setInferenceBudget(20000);
        try {
            prolog.solve("numlist(1, 100000, L), phrase(loopy, L, _), fail.");
            fail("the inference budget must abort inside phrase/3");
        } catch (InferenceLimitException expected) {
            assertTrue(true);
        } finally {
            prolog.setInferenceBudget(0);
        }
    }

    // ================================================================ ISS-2025-0452

    @Test
    public void testISS0452_BagofAndSetofGrouping() {
        prolog.consult("age(peter, 7). age(ann, 11). age(pat, 8). age(tom, 5). cls(a, 1). cls(a, 2). cls(b, 3).\n");
        // free variable grouping, one solution per witness, setof in standard order
        List<Map<String, Term>> s = prolog.solve("bagof(N, cls(K, N), L).");
        assertEquals(2, s.size());
        s = prolog.solve("setof(K-L, setof(N, cls(K, N), L), All).");
        assertEquals(1, s.size());
        // ^ removes the witness
        assertEquals(1, prolog.solve("bagof(N, K^cls(K, N), L), L == [1,2,3].").size());
        // no solutions -> failure (unlike findall/3)
        assertTrue(prolog.solve("bagof(X, cls(zzz, X), _L).").isEmpty());
        assertEquals(1, prolog.solve("findall(X, cls(zzz, X), []).").size());
        // setof sorts and removes duplicates
        assertEquals(1, prolog.solve("setof(X, member(X, [c,a,b,a]), [a,b,c]).").size());
        // variant witnesses merge into ONE group (ISS-2025-0412)
        prolog.consult("v(1, f(_)). v(2, f(_)).\n");
        assertEquals(1, prolog.solve("bagof(X, v(X, Y), L).").size());
    }

    @Test
    public void testISS0452_AggregateAllForms() {
        prolog.consult("n(1). n(2). n(3).\n");
        assertEquals(1, prolog.solve("aggregate_all(count, n(_), 3).").size());
        assertEquals(1, prolog.solve("aggregate_all(sum(X), n(X), 6).").size());
        assertEquals(1, prolog.solve("aggregate_all(max(X), n(X), 3).").size());
        assertEquals(1, prolog.solve("aggregate_all(min(X), n(X), 1).").size());
        assertEquals(1, prolog.solve("aggregate_all(bag(X), n(X), [1,2,3]).").size());
        assertEquals(1, prolog.solve("aggregate_all(set(X), member(X, [b,a,b]), [a,b]).").size());
        assertEquals(1, prolog.solve("aggregate_all(sum(X), fail, 0).").size());
        // ISS-2025-0413: max/min FAIL on no solution
        assertTrue(prolog.solve("aggregate_all(max(X), fail, _M).").isEmpty());
        // ISS-2025-0414: exact big-integer sums, float contagion
        assertEquals(1, prolog.solve("aggregate_all(sum(X), member(X, [1.5, 2.5]), S), float(S), S =:= 4.0.").size());
        // the Value-Witness form — ISS-2025-0522 (P1.9): SWI's max(Expr, Witness) answering
        // max(Value, Witness); the old non-standard max(V-W) pair form is gone (V-W is evaluated)
        assertEquals(1, prolog.solve("aggregate_all(max(V, W), member(V-W, [1-a, 3-b, 2-c]), max(3, b)).").size());
        try {
            prolog.solve("aggregate_all(sum(X), member(X, [1, a]), _S).");
            fail("a non-numeric element must raise type_error(evaluable, a/0)");   // ISS-2025-0522
        } catch (PrologException e) {
            assertTrue(String.valueOf(e.getErrorTerm()).contains("type_error"));
        }
    }

    @Test
    public void testISS0452_WithOutputToSinks() {
        assertEquals(1, prolog.solve("with_output_to(atom(A), (write(a), write(b))), A == ab.").size());
        assertEquals(1, prolog.solve("with_output_to(codes(C), write(ab)), C == [97, 98].").size());
        assertEquals(1, prolog.solve("with_output_to(chars(C), write(ab)), C == [a, b].").size());
        assertTrue(prolog.solve("with_output_to(atom(_A), fail).").isEmpty());
        // once/1 semantics: only the first solution's output is captured
        prolog.consult("two(a). two(b).\n");
        assertEquals(1, prolog.solve("with_output_to(atom(A), two(X)), A == '', X == a.").size());
    }

    @Test
    public void testISS0452_FormatCallDirective() {
        assertEquals(1,
            prolog.solve("with_output_to(atom(A), format(\"[~@]\", [write(inner)])), A == '[inner]'.").size());
    }

    // ================================================================ ISS-2025-0453 (generators)

    @Test(timeout = 60000)
    public void testISS0453_MemberIsALazyGenerator() {
        // once/1 over a long list must not build one solution map per element
        assertEquals(1, prolog.solve("numlist(1, 300000, L), once(member(X, L)), X == 1.").size());
        assertEquals(3, prolog.solve("member(X, [a,b,c]).").size());
        assertEquals(1, prolog.solve("member(b, [a,b,c]).").size());
        assertTrue(prolog.solve("member(z, [a,b,c]).").isEmpty());
        // a failing head unification must not leak its partial bindings into the next element
        assertEquals(1, prolog.solve("member(f(A, b), [f(1, c), f(2, b)]), A == 2.").size());
        assertEquals(1, prolog.solve("memberchk(b, [a,b,c]).").size());
    }

    @Test
    public void testISS0453_ListPredicatesAllModes() {
        assertEquals(1, prolog.solve("append([1,2], [3], [1,2,3]).").size());
        assertEquals(3, prolog.solve("append(X, Y, [1,2]).").size());
        assertEquals(1, prolog.solve("append([1,2], Y, Z), Y = [], Z == [1,2].").size());
        // ISS-2025-0468: fully open append/3 now ENUMERATES (the W3 deviation is paid off), so the
        // first solution is asserted with a cut instead of collecting an infinite relation.
        assertFalse(prolog.solve("append(X, Y, Z), X == [], !.").isEmpty());
        assertEquals(1, prolog.solve("append(X, _Y, _Z), length(X, 2), !.").size());
        assertEquals(3, prolog.solve("select(X, [a,b,c], _R).").size());
        assertEquals(1, prolog.solve("select(b, [a,b,c], R), R == [a,c].").size());
        assertEquals(3, prolog.solve("select(x, L, [1,2]).").size());
        assertEquals(1, prolog.solve("nth0(1, [a,b,c], b).").size());
        assertEquals(1, prolog.solve("nth1(1, [a,b,c], a).").size());
        assertEquals(3, prolog.solve("nth0(_I, [a,b,c], _E).").size());
        assertEquals(1, prolog.solve("last([1,2,3], 3).").size());
        // ISS-2025-0603 (P4.10): last/2 on a partial list ENUMERATES (SWI) — T = [], [X], [_,X], ...
        // — so the first answer is taken with a cut.
        assertEquals(1, prolog.solve("last([a|T], X), T == [], X == a, !.").size());
        assertTrue(prolog.solve("last([a|b], _X).").isEmpty());
        assertEquals(1, prolog.solve("reverse([1,2,3], [3,2,1]).").size());
        assertEquals(1, prolog.solve("reverse(L, [1,2]), L == [2,1].").size());
        assertEquals(1, prolog.solve("length([a,b,c], 3).").size());
        assertEquals(1, prolog.solve("length(L, 2), L = [_,_].").size());
        assertEquals(1, prolog.solve("msort([c,a,b,a], [a,a,b,c]).").size());
        assertEquals(1, prolog.solve("sort([c,a,b,a], [a,b,c]).").size());
        assertEquals(1, prolog.solve("sum_list([1,2,3], 6).").size());
        assertEquals(1, prolog.solve("numlist(1, 4, [1,2,3,4]).").size());
        assertEquals(1, prolog.solve("copy_term(f(X, Y, X), C), C = f(P, Q, R), P == R, P \\== Q.").size());
        // the ISO error clauses of sort/2 and msort/2 survive the rewrite
        try {
            prolog.solve("sort([a|_T], _S).");
            fail("sort/2 on a partial list must raise instantiation_error");
        } catch (PrologException e) {
            assertTrue(String.valueOf(e.getErrorTerm()).contains("instantiation_error"));
        }
        try {
            prolog.solve("msort(foo, _S).");
            fail("msort/2 on a non-list must raise type_error(list, foo)");
        } catch (PrologException e) {
            assertTrue(String.valueOf(e.getErrorTerm()).contains("type_error"));
        }
    }

    @Test
    public void testISS0453_ClauseRunsOnTheClauseStore() {
        prolog.consult("c(1). c(2) :- true. d(X) :- c(X), X > 1.\n");
        assertEquals(2, prolog.solve("clause(c(_X), _B).").size());
        assertEquals(1, prolog.solve("clause(c(1), true).").size());
        assertEquals(1, prolog.solve("clause(d(X), (c(X), X > 1)).").size());
        assertTrue(prolog.solve("clause(nosuch(_X), _B).").isEmpty());
        try {
            prolog.solve("clause(atom(_X), _B).");
            fail("clause/2 on a built-in must raise permission_error");
        } catch (PrologException e) {
            assertTrue(String.valueOf(e.getErrorTerm()).contains("permission_error"));
        }
        try {
            prolog.solve("clause(_H, _B).");
            fail("clause/2 with an unbound head must raise instantiation_error");
        } catch (PrologException e) {
            assertTrue(String.valueOf(e.getErrorTerm()).contains("instantiation_error"));
        }
    }

    /** The empty substring used to spin forever in {@code indexOf("", idx)} (heap death on v2). */
    @Test(timeout = 30000)
    public void testISS0453_SubAtomAllModes() {
        assertEquals(10, prolog.solve("sub_atom(abc, _B, _L, _A, _S).").size());
        assertEquals(4, prolog.solve("sub_atom(abc, _B, _L, _A, '').").size());
        assertEquals(1, prolog.solve("sub_atom(hello, 1, 3, A, X), A == 1, X == ell.").size());
        assertEquals(1, prolog.solve("sub_atom(hello, B, 2, A, el), B == 1, A == 2.").size());
        assertEquals(2, prolog.solve("sub_atom(abab, _B, 2, _A, ab).").size());
        assertEquals(1, prolog.solve("sub_string(\"hello\", 1, 3, _A, S), S == \"ell\".").size());
    }

    // ================================================================ ISS-2025-0454 (prelude)

    @Test(timeout = 60000)
    public void testISS0454_PreludeApplyIsLoadedAndLinear() {
        prolog.consult("dbl(X, Y) :- Y is X * 2.\n");
        assertEquals(1, prolog.solve("maplist(dbl, [1,2,3], [2,4,6]).").size());
        assertEquals(1, prolog.solve("maplist(integer, [1,2,3]).").size());
        assertEquals(1, prolog.solve("foldl([X,A0,A]>>(A is A0+X), [1,2,3,4], 0, 10).").size());
        assertEquals(1, prolog.solve("include([X]>>(X > 1), [1,2,3], [2,3]).").size());
        assertEquals(1, prolog.solve("exclude([X]>>(X > 1), [1,2,3], [1]).").size());
        assertEquals(1, prolog.solve("partition([X]>>(X > 1), [1,2,3], [2,3], [1]).").size());
        // maplist/2 on a partial list closes it (ISS-2025-0380)
        assertEquals(1, prolog.solve("maplist(atom, [a,b|T]), T == [].").size());
        // maplist is re-satisfiable through the mapped goal (ISS-2025-0381)
        assertEquals(4, prolog.solve("maplist(member, [_X,_Y], [[1,2],[3,4]]).").size());
        // linear: 200 000 elements, well inside the timeout
        long t0 = System.currentTimeMillis();
        assertEquals(1, prolog.solve("numlist(1, 200000, L), maplist(dbl, L, L2), last(L2, 400000).").size());
        long dt = System.currentTimeMillis() - t0;
        assertTrue("maplist/3 over 200 000 elements took " + dt + " ms", dt < 30000);
    }

    @Test
    public void testISS0454_UserDefinitionOverridesThePrelude() {
        // the quicksort partition/4 of examples/test_16_sorting.pl: different argument order,
        // and it must win over library(apply)'s partition/4
        prolog.consult(
            "partition([], _, [], []).\n"
          + "partition([H|T], P, [H|S], L) :- H =< P, !, partition(T, P, S, L).\n"
          + "partition([H|T], P, S, [H|L]) :- partition(T, P, S, L).\n");
        List<Map<String, Term>> s = prolog.solve("partition([3,1,4,1,5], 3, Small, Large).");
        assertEquals(1, s.size());
        assertEquals("[3, 1, 1]", value(s, 0, "Small"));
        assertEquals("[4, 5]", value(s, 0, "Large"));
    }

    // ================================================================ ISS-2025-0455 (yall)

    @Test
    public void testISS0455_YallLambdas() {
        assertEquals(1, prolog.solve("maplist([X,Y]>>(Y is X*2), [1,2,3], [2,4,6]).").size());
        assertEquals(1, prolog.solve("foldl([X,A0,A]>>(A is A0+X), [1,2,3], 0, 6).").size());
        assertEquals(1, prolog.solve("call([X,Y]>>(Y is X+1), 5, 6).").size());
        // the lambda is copied before EVERY call, so one lambda serves every element
        assertEquals(1, prolog.solve("maplist([X,Y]>>(Y = f(X)), [1,2], [f(1), f(2)]).").size());
        // Free/Params>>Body shares the free variable
        assertEquals(1, prolog.solve("N = 10, maplist(N/[X,Y]>>(Y is X*N), [1,2], [10,20]).").size());
        // ... and without the '/' the same variable is renamed apart, so it stays unbound
        assertEquals(1, prolog.solve("maplist([X,Y]>>(Y = g(X, _Fresh)), [1], [g(1, _)]).").size());
        // the library(lambda) spelling
        assertEquals(1, prolog.solve("maplist(\\X^Y^(Y is X*3), [1,2], [3,6]).").size());
    }

    // ================================================================ ISS-2025-0456

    /**
     * {@code bindVar} binds the younger cell to the older one, so the old "no variable of Specific
     * has a ref" test made {@code subsumes_term(f(X), f(Y))} answer false on v4 where every other
     * Prolog (and the v2 engine) answers true.
     */
    @Test
    public void testISS0456_SubsumesTermVariableToVariable() {
        assertEquals(1, prolog.solve("subsumes_term(f(_X), f(_Y)).").size());
        assertEquals(1, prolog.solve("subsumes_term(f(_X), f(a)).").size());
        assertEquals(1, prolog.solve("subsumes_term(f(_X, _Y), f(_A, _B)).").size());
        assertTrue(prolog.solve("subsumes_term(f(a), f(_X)).").isEmpty());
        assertTrue(prolog.solve("subsumes_term(f(_A, _A), f(_B, _C)).").isEmpty());
        assertEquals(1, prolog.solve("subsumes_term(f(_A, _B), f(_C, _C)).").size());
        // and it leaves nothing bound behind
        assertEquals(1, prolog.solve("subsumes_term(f(X), f(Y)), var(X), var(Y), X \\== Y.").size());
    }

    // ================================================================ ports / budget / sandbox

    /** A native generator must still emit the four ports (invariant 10 of the progress report). */
    @Test
    public void testISS0453_NativeGeneratorsEmitFourPorts() {
        java.io.ByteArrayOutputStream buf = new java.io.ByteArrayOutputStream();
        java.io.PrintStream ps = new java.io.PrintStream(buf, true);
        java.io.PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        try {
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
            prolog.setTracing(true);
            prolog.solve("member(X, [a,b]), X == b.");
        } finally {
            prolog.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        String out = buf.toString();
        assertTrue("Call port missing:\n" + out, out.contains("Call: ") && out.contains("member"));
        assertTrue("Exit port missing:\n" + out, out.contains("Exit: "));
        assertTrue("Redo port missing:\n" + out, out.contains("Redo: "));
    }

    /** The budget and the Stop interrupt must reach inside a native generator and the prelude. */
    @Test(timeout = 30000)
    public void testISS0453_BudgetReachesGeneratorsAndPrelude() {
        prolog.setInferenceBudget(50000);
        try {
            prolog.solve("numlist(1, 1000000, L), maplist(integer, L).");
            fail("the budget must abort a prelude maplist over a million elements");
        } catch (InferenceLimitException expected) {
            assertTrue(true);
        } finally {
            prolog.setInferenceBudget(0);
        }
        prolog.setInferenceBudget(50000);
        try {
            prolog.solve("numlist(1, 1000000, L), member(x, L).");
            fail("the budget must abort inside member/2's generator");
        } catch (InferenceLimitException expected) {
            assertTrue(true);
        } finally {
            prolog.setInferenceBudget(0);
        }
    }

    /** Safe mode must not be weakened by the new natives (none of them touches the host). */
    @Test
    public void testISS0453_SafeModeStillStripsTheUnsafeBuiltins() {
        Prolog sandboxed = new Prolog();
        sandboxed.enableSafeMode();
        assertEquals(1, sandboxed.solve("msort([b,a], [a,b]).").size());
        assertEquals(1, sandboxed.solve("maplist(atom, [a,b]).").size());
        try {
            sandboxed.solve("shell('echo hi').");
            fail("safe mode must still remove shell/1");
        } catch (PrologException expected) {
            assertTrue(String.valueOf(expected.getErrorTerm()).contains("existence_error"));
        }
    }
}
// END_CHANGE: ISS-2025-0450..0456
