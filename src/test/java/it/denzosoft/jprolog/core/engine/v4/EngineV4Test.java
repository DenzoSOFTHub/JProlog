package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.InferenceLimitException;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0438..0447 - engine v4, waves W1 (foundations) and W2 (clause store).
/**
 * Acceptance tests for the opt-in v4 resolution engine ({@code core.engine.v4}).
 *
 * <p>Every test here selects v4 explicitly in {@link #setUp} and restores the previous selection in
 * {@link #tearDown}, so the class behaves the same whether the suite is run on the default engine
 * or with {@code -Djprolog.engine=v4} / {@code mvn test -Pengine-v4}.
 *
 * <p>Sizes are chosen to pass under the surefire fork's DEFAULT JVM settings: no {@code -Xmx} and
 * no {@code -Xss} are assumed. The heavier acceptance numbers of design B.15 (loop(10 000 000) in a
 * 64 MB heap, nrev KLIPS, the 20 000-clause lookup) are measured with the scratchpad harness and
 * recorded in {@code docs/reports/report-engine-v4-progress.md}.
 */
public class EngineV4Test {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    @After
    public void tearDown() {
    }

    private List<Map<String, Term>> solve(String q) { return prolog.solve(q); }

    private void consult(String program) { prolog.consult(program); }

    // ================================================================ W1: variable cells (B.2)

    @Test
    public void testISS0438_VariableIsAnIdentityCell() {
        Variable a = new Variable("X");
        Variable b = new Variable("X");
        assertFalse("two cells with the same name are different variables", a.equals(b));
        assertTrue(a.equals(a));
        assertTrue("serials are strictly increasing", b.serial > a.serial);
        assertNull("a fresh cell is unbound", a.getRef());
        a.setRef(new Atom(("bound")));
        assertEquals("bound", a.getRef().getName());
    }

    @Test
    public void testISS0438_FreshCellGetsAUniqueLazyName() {
        Variable a = new Variable();
        Variable b = new Variable();
        assertNotNull(a.getName());
        assertFalse(a.getName().equals(b.getName()));
    }

    @Test
    public void testISS0438_NamedVariableCopiesToItself() {
        // B.14 audit: ~10 built-ins call x.copy() and then unify with the result, relying on the
        // copy ALIASING the original. With identity variables that only works if copy() is identity.
        Variable v = new Variable("X");
        assertSame(v, v.copy());
        assertEquals("arg/3 must alias, not clone", 1,
            solve("arg(1, f(X), A), X = hello, A == hello.").size());
        assertEquals(1, solve("member(X, [A,b]), X == A, !.").size());
    }

    // ================================================================ W1: cycle-safe walkers (B.4)

    @Test
    public void testISS0441_RationalTreesUnify() {
        // L-04: on v3.8.0 both of these hung forever and polled nothing, so neither the inference
        // budget nor a thread interrupt could stop them.
        assertEquals(1, solve("X = f(X), Y = f(Y), X = Y.").size());
        assertEquals(1, solve("X = [1|X], Y = [1|Y], X = Y.").size());
    }

    @Test
    public void testISS0441_CyclicTermPredicates() {
        assertEquals(1, solve("X = f(X), cyclic_term(X).").size());
        assertEquals(0, solve("X = f(X), acyclic_term(X).").size());
        assertEquals(1, solve("acyclic_term(f(a,g(b))).").size());
        assertEquals(0, solve("cyclic_term(f(a,g(b))).").size());
    }

    @Test
    public void testISS0441_OccursCheckErrorModeStillRaises() {
        // Decision 2: rational trees are the default, and occurs_check = error restores the ISO
        // behaviour on top of the same machinery.
        assertEquals(1, solve("set_prolog_flag(occurs_check, error).").size());
        try {
            solve("X = f(X).");
            fail("occurs_check = error must raise representation_error(cyclic_term)");
        } catch (PrologException e) {
            assertTrue(String.valueOf(e.getMessage()), String.valueOf(e.getMessage()).contains("cyclic_term"));
        } finally {
            solve("set_prolog_flag(occurs_check, false).");
        }
    }

    @Test
    public void testISS0441_DeepStructuresAtTheDefaultStack() {
        // The v4 walkers iterate on the LAST argument, so a long list costs no Java stack.
        assertEquals(1, solve("numlist(1, 200000, L), length(L, N), N =:= 200000.").size());
        assertEquals(1, solve("numlist(1, 200000, L), L == L.").size());
        assertEquals(1, solve("numlist(1, 200000, L), copy_term(L, L2), length(L2, M), M =:= 200000.").size());
        assertEquals(1, solve("numlist(1, 200000, L), ground(L).").size());
        assertEquals(1, solve("numlist(1, 200000, L), msort(L, S), S = [1|_].").size());
    }

    @Test
    public void testISS0441_ComparisonWalkersFollowBindings() {
        // v3.8.0's structuralEqual compared RESOLVED copies and blew the stack on this query.
        assertEquals(1, solve("X = f(Y), Y = 1, X == f(1).").size());
        assertEquals(1, solve("X = f(Y), Y = 1, X @< f(2).").size());
        assertEquals(1, solve("compare(O, f(1), f(2)), O == (<).").size());
        assertEquals(1, solve("term_variables(f(A, g(B), A), Vs), Vs = [A, B].").size());
        assertEquals(1, solve("subsumes_term(f(_), f(a)).").size());
        assertEquals(0, solve("subsumes_term(f(a), f(_)).").size());
    }

    // ================================================================ W1: machine core (B.6)

    @Test
    public void testISS0442_ControlConstructs() {
        consult("p(1). p(2). p(3).\nq(X) :- p(X), X > 1.\nr(X) :- (p(X) -> true ; X = none).\n");
        assertEquals(3, solve("p(X).").size());
        assertEquals(2, solve("q(X).").size());
        assertEquals(1, solve("r(X).").size());
        assertEquals(1, solve("\\+ p(9).").size());
        assertEquals(1, solve("once(p(X)).").size());
        assertEquals(1, solve("ignore(p(99)).").size());
        assertEquals(1, solve("forall(p(X), X > 0).").size());
        assertEquals(0, solve("forall(p(X), X > 1).").size());
        assertEquals(3, solve("(p(X) *-> true ; X = none).").size());
        assertEquals(1, solve("(p(9) *-> true ; X = none).").size());
        assertEquals(1, solve("findall(X, p(X), [1,2,3]).").size());
        assertEquals(1, solve("p(X), !.").size());
    }

    @Test
    public void testISS0442_CatchAndThrow() {
        assertEquals(1, solve("catch(throw(boom), boom, true).").size());
        assertEquals(1, solve("catch(X is 1/0, error(evaluation_error(zero_divisor), _), true).").size());
        assertEquals(1, solve("catch(findall(_, throw(inner), _), inner, true).").size());
        // an unmatched catcher keeps unwinding
        assertEquals(1, solve("catch(catch(throw(a), b, true), a, true).").size());
        // ISO 7.8.9: the catcher applies only during Goal's extent
        assertEquals(1, solve("catch(true, _, fail), true.").size());
    }

    @Test
    public void testISS0442_SetupCallCleanupFrames() {
        consult(":- dynamic(mark/1).\n");
        // deterministic exit
        assertEquals(1, solve("setup_call_cleanup(true, true, assertz(mark(det))), mark(det).").size());
        // failure
        assertEquals(1, solve("(setup_call_cleanup(true, fail, assertz(mark(f))) ; true), mark(f).").size());
        // exception, cleanup BEFORE the ball propagates
        assertEquals(1, solve(
            "catch(setup_call_cleanup(true, throw(b), assertz(mark(e))), b, true), mark(e).").size());
        // nondeterministic goal: every solution, then cleanup exactly once
        assertEquals(1, solve(
            "findall(X, setup_call_cleanup(true, member(X,[1,2]), assertz(mark(nd))), L), "
            + "L == [1,2], findall(_, mark(nd), M), length(M, 1).").size());
        // call_cleanup/2 is setup_call_cleanup(true, G, C)
        assertEquals(1, solve("call_cleanup(true, assertz(mark(cc))), mark(cc).").size());
    }

    @Test
    public void testISS0442_LazyGeneratorsAreNotBounded() {
        assertEquals(1, solve("between(1, 1000000, X), X >= 1000000, !.").size());
        assertEquals(1, solve("between(1, inf, X), X >= 100000, !.").size());
        assertEquals(1, solve("length(L, N), N >= 3, !, length(L, 3).").size());
        assertEquals(1, solve(
            "nb_setval(c, 0), repeat, nb_getval(c, V), V1 is V+1, nb_setval(c, V1), V1 >= 1500, !.").size());
    }

    @Test
    public void testISS0442_DeterministicRecursionLeavesNothingBehind() {
        // The cell model + conditional trailing + trust-me pop: a deterministic recursion must not
        // grow the choice-point stack or the trail (this is the mechanism behind loop(10M) in 64 MB).
        consult("loop(0) :- !.\nloop(N) :- N1 is N-1, loop(N1).\n");
        Engine e = prolog.getV4Engine();
        Machine m = new Machine(e, new it.denzosoft.jprolog.core.engine.ResourceGuard(0));
        final int[] count = {0};
        Term goal = it.denzosoft.jprolog.core.parser.v2.TermReader
            .parseTerm("loop(20000)", prolog.getOperatorTable());
        m.solve(goal, sol -> { count[0]++; return true; });
        assertEquals(1, count[0]);
        assertEquals("no choice point survives a deterministic recursion", 0, m.choicePointCount());
        assertEquals("no trail entry survives it either", 0, m.trailSize());
    }

    @Test
    public void testISS0442_OutOfMemoryBecomesACatchableResourceError() {
        // Not by exhausting the heap (that would be a hostile test) but by checking the mapping is
        // wired: a resource_error(memory) ball must be catchable like any other ISO error.
        assertEquals(1, solve("catch(throw(error(resource_error(memory), foo)), "
            + "error(resource_error(memory), _), true).").size());
    }

    // ================================================================ W1: SPI + adapter (B.5)

    @Test
    public void testISS0443_LegacyBuiltinsRunUnchanged() {
        assertEquals(1, solve("atom_length(abcde, 5).").size());
        assertEquals(3, solve("member(X, [a,b,c]).").size());
        assertEquals(1, solve("append([1,2], [3], [1,2,3]).").size());
        assertEquals(1, solve("msort([c,a,b], [a,b,c]).").size());
        assertEquals(1, solve("atom_codes(abc, [97,98,99]).").size());
        assertEquals(1, solve("sub_atom(abcde, 1, 3, _, bcd).").size());
        assertEquals(1, solve("setof(X, member(X, [b,a,b]), [a,b]).").size());
        assertEquals(1, solve("aggregate_all(count, member(_, [a,b,c]), 3).").size());
    }

    @Test
    public void testISS0443_NativeSetargKeepsObjectIdentity() {
        assertEquals(1, solve("T = f(a,b), setarg(1, T, z), T == f(z,b).").size());
        // backtrackable: the change is undone on failure
        assertEquals(1, solve("T = f(a), (setarg(1, T, z), fail ; true), T == f(a).").size());
        // nb_setarg is not
        assertEquals(1, solve("T = f(a), (nb_setarg(1, T, z), fail ; true), T == f(z).").size());
    }

    @Test
    public void testISS0443_MetaCallSubGoalsRunOnTheV4Machine() {
        // A BuiltInWithContext built-in gets a SolverFacade whose sub-goals run on this machine,
        // so they honour the budget and reach depths the recursive solver could not.
        consult("count([], 0).\ncount([_|T], N) :- count(T, M), N is M+1.\n");
        assertEquals(1, solve("numlist(1, 5000, L), aggregate_all(count, member(_, L), 5000).").size());
        assertEquals(1, solve("numlist(1, 5000, L), forall(member(X, L), X > 0).").size());
    }

    // ================================================================ W1: hardening on v4

    @Test
    public void testISS0444_InferenceBudgetAbortsOnV4() {
        consult("loop(0) :- !.\nloop(N) :- N1 is N-1, loop(N1).\n");
        prolog.setInferenceBudget(20000);
        try {
            solve("loop(1000000).");
            fail("the inference budget must abort the query");
        } catch (InferenceLimitException expected) {
            // plain RuntimeException, NOT a PrologException: catch/3 must not be able to trap it
            assertFalse("the trust model requires a non-PrologException",
                PrologException.class.isInstance(expected));
        } finally {
            prolog.setInferenceBudget(0);
        }
    }

    @Test
    public void testISS0444_BudgetSurvivesCatchAndMetaCalls() {
        consult("loop(0) :- !.\nloop(N) :- N1 is N-1, loop(N1).\n");
        prolog.setInferenceBudget(20000);
        try {
            for (String q : new String[]{"catch(loop(1000000), _, true).", "once(loop(1000000)).",
                                          "ignore(loop(1000000)).", "\\+ \\+ loop(1000000).",
                                          "findall(_, loop(1000000), _)."}) {
                try {
                    solve(q);
                    fail("untrusted code must not escape the budget through " + q);
                } catch (InferenceLimitException expected) {
                    // ok
                }
            }
        } finally {
            prolog.setInferenceBudget(0);
        }
    }

    @Test
    public void testISS0444_SafeModeStillRemovesHostBuiltinsOnV4() {
        prolog.enableSafeMode();
        try {
            solve("shell('echo hi', _).");
            fail("safe mode must remove the host-touching built-ins on v4 too");
        } catch (PrologException expected) {
            assertTrue(String.valueOf(expected.getMessage()).contains("existence_error"));
        }
    }

    // ISS-2025-0491 (4.1 wave A): `testISS0444_EngineSelectionFlag` is DELETED — it asserted
    // that Prolog.setUseV4Engine(false) selects another engine, and there is no other engine and
    // no flag any more. EngineV41RetirementTest pins what replaced it (the property warns, the
    // API is gone).

    // ================================================================ W2: clause store (B.7)

    @Test
    public void testISS0445_LogicalUpdateView() {
        consult(":- dynamic(c/1).\nc(1).\nc(2).\nc(3).\n");
        // ISS-2025-0396 family: retract/1 is re-executable and drains the predicate
        assertEquals(1, solve("findall(X, retract(c(X)), [1,2,3]).").size());
        assertEquals(0, solve("c(_).").size());

        consult("d(1).\nd(2).\nd(3).\n");
        assertEquals(1, solve("(retract(d(_)), fail ; true), findall(X, d(X), []).").size());

        // a clause asserted DURING a call is invisible to that call
        consult("e(1).\n");
        assertEquals("assertz during the scan must not extend it", 1,
            solve("findall(X, (e(X), assertz(e(99))), [1]).").size());
        assertEquals(1, solve("findall(X, e(X), [1,99]).").size());
    }

    @Test
    public void testISS0445_AssertaAssertzOrderAndIndex() {
        consult(":- dynamic(f/1).\n");
        assertEquals(1, solve("assertz(f(b)), asserta(f(a)), assertz(f(c)), findall(X, f(X), [a,b,c]).").size());
        // first-argument indexing must never drop a clause
        consult("g(a, 1).\ng(X, 2) :- atom(X).\ng(b, 3).\n");
        assertEquals("bucket + variable-headed clauses, in source order", 1,
            solve("findall(N, g(a, N), [1,2]).").size());
        assertEquals(1, solve("findall(N, g(b, N), [2,3]).").size());
        assertEquals(1, solve("findall(N, g(z, N), [2]).").size());
        // with K unbound the guarded clause fails (atom(X) on an unbound X), so only the two
        // fact clauses contribute
        assertEquals(1, solve("findall(K-N, g(K, N), [a-1,b-3]).").size());
    }

    @Test
    public void testISS0445_GenerationsAreVisibleInTheStore() {
        consult(":- dynamic(h/1).\nh(1).\n");
        ClauseStore store = prolog.getV4Engine().store();
        ClauseStore.Predicate p = store.lookup("h", 1);
        assertEquals(1, p.size());
        solve("assertz(h(2)).");
        p = store.lookup("h", 1);
        assertEquals("assertz appends without rebuilding", 2, p.size());
        long g = store.generation();
        solve("retract(h(1)).");
        assertTrue("retract advances the generation", store.generation() > g);
        assertEquals(1, solve("findall(X, h(X), [2]).").size());
    }

    @Test
    public void testISS0445_StoreResyncsAfterAnExternalWrite() {
        // A write that does NOT go through v4 (here: the Java API) must still be seen.
        consult("k(1).\n");
        assertEquals(1, solve("findall(X, k(X), [1]).").size());
        List<Term> body = new ArrayList<Term>();
        prolog.getV4Engine().kb().addRule(new Rule(
            new CompoundTerm(new Atom("k"), Arrays.asList((Term) new it.denzosoft.jprolog.core.terms.Number(2L))),
            body));
        assertEquals("the clause store re-syncs on the KnowledgeBase version", 1,
            solve("findall(X, k(X), [1,2]).").size());
    }

    @Test
    public void testISS0445_ManyClausesAndInterleavedWrites() {
        StringBuilder sb = new StringBuilder(":- dynamic(big/2).\n");
        for (int i = 0; i < 5000; i++) sb.append("big(").append(i).append(",").append(i).append(").\n");
        consult(sb.toString());
        assertEquals(1, solve("big(4999, 4999).").size());
        assertEquals(1, solve("big(0, 0).").size());
        assertEquals(0, solve("big(5000, _).").size());
        // interleaved asserts and calls stay linear (no per-write snapshot rebuild)
        assertEquals(1, solve(
            "(between(1, 5000, I), assertz(iv(I)), iv(I), fail ; true).").size());
        assertEquals(1, solve("findall(X, iv(X), L), length(L, 5000).").size());
    }

    @Test
    public void testISS0446_DatabaseBuiltinsSeeTheSameClauses() {
        consult(":- dynamic(m/1).\nm(1).\nm(2).\n");
        assertEquals(1, solve("assertz(m(3)), findall(X, clause(m(X), true), [1,2,3]).").size());
        assertEquals(1, solve("retractall(m(_)), findall(X, m(X), []).").size());
        assertEquals(1, solve("assertz(n(1)), abolish(n/1), \\+ catch(n(_), _, fail).").size());
        assertEquals(1, solve("assertz(o(1)), predicate_property(o(_), dynamic).").size());
    }

    @Test
    public void testISS0447_SourceLineSurvivesForBreakpoints() {
        prolog.consultWithDiagnostics("a1 :- true.\nb1 :- true.\nc1 :- true.\n", "t.pl");
        assertEquals("b1/0", prolog.getPredicateIndicatorAtLine(2));
        assertEquals("c1/0", prolog.getPredicateIndicatorAtLine(3));
    }

    // ================================================================ W1/W2 regression fixes

    @Test
    public void testISS0448_FindallIsOpaque() {
        // findall/3 ran its nested drive under a forced-trail extent, but the finally block closed
        // the extent BEFORE undoing to the mark — and cutTo() ends in Bindings.clearIfUnreachable(),
        // which wipes the trail once forceTrail is 0 and no choice point is left. The template
        // variable therefore kept its LAST binding.
        assertEquals("the template variable must be unbound again", 1,
            solve("findall(X, member(X,[1,2]), L), var(X).").size());
        assertEquals("it must NOT have kept the last solution", 0,
            solve("findall(X, member(X,[1,2]), L), X == 2.").size());
        assertEquals("nor may it leak into a companion variable's answer", 1,
            solve("findall(X-Y, member(X,[1,2]), L), var(X), var(Y).").size());
        assertEquals("the collected list itself is still right", 1,
            solve("findall(X, member(X,[1,2]), [1,2]).").size());
        // bindings made by a SIDE EFFECT inside the goal are equally opaque
        assertEquals(1, solve(
            "findall(_, (member(K,[1,2]), atom_length(abc,_)), _), var(K).").size());
        // nesting, and a goal that leaves a choice point behind
        assertEquals(1, solve("findall(A-B, (member(A,[1,2]), member(B,[x])), [1-x,2-x]).").size());
        assertEquals(1, solve(
            "findall(P, findall(Q, member(Q,[1,2]), P), [[1,2]]), var(P).").size());
    }

    @Test
    public void testISS0448_EveryMarkUndoExtentIsOpaque() {
        // the same ordering bug affected every construct that undoes to a mark while the
        // choice-point stack may be empty
        assertEquals("\\+/1 must not leak", 1, solve("\\+ member(Z,[1,2]), true ; var(Z).").size());
        assertEquals("\\=/2 must not leak", 1, solve("f(V) \\= f(1), var(V) ; var(V).").size());
        assertEquals("a NON-matching catcher must not leak", 1,
            solve("catch(catch(throw(a), f(E), true), a, true), var(E).").size());
        assertEquals("a matching catcher still binds", 1,
            solve("catch(throw(f(1)), f(E), true), E == 1.").size());
        assertEquals("subsumes_term/2 must not bind either side", 1,
            solve("subsumes_term(f(G), f(a)), var(G).").size());
        // the meta-call facade uses the same nested drive
        assertEquals(1, solve("aggregate_all(count, member(V,[1,2]), 2), var(V).").size());
        assertEquals(1, solve("forall(member(W,[1,2]), true), var(W).").size());
        assertEquals(1, solve("bagof(B, member(B,[1,2]), [1,2]), var(B).").size());
    }

    // 30 s is ~15x the fixed cost of this loop and well under the ~40 s it took while the leak was
    // present, so the timeout is a real regression guard rather than decoration.
    @Test(timeout = 30000)
    public void testISS0449_RetractAssertLoopStaysLinear() {
        // Dead clauses were only compacted at the query boundary, so a retract/assert loop grew the
        // predicate's array by one dead clause per iteration and both the retract candidate scan
        // and the clause-iterator scan became O(#clauses): cnt(100000) took ~60 s (v2: ~5 s).
        consult(":- dynamic(counter/1).\ncounter(0).\n"
            + "cnt(0) :- !.\n"
            + "cnt(N) :- retract(counter(C)), C1 is C+1, assertz(counter(C1)), N1 is N-1, cnt(N1).\n");
        assertEquals(1, solve("cnt(100000).").size());
        assertEquals("the counter really advanced 100000 times", 1,
            solve("counter(V), V =:= 100000.").size());
        assertEquals("and the predicate is back to a single clause", 1,
            prolog.getV4Engine().store().lookup("counter", 1).size());
    }

    @Test
    public void testISS0449_PhysicalClauseCountStaysBoundedDuringTheLoop() {
        // Store-level, so there is no query boundary anywhere in this loop to hide the leak.
        consult(":- dynamic(cc/1).\ncc(0).\n");
        ClauseStore store = prolog.getV4Engine().store();
        int max = 0;
        for (int i = 0; i < 5000; i++) {
            ClauseStore.Predicate p = store.lookup("cc", 1);
            long gen = store.generation();
            Clause live = null;
            for (Clause c : p.all()) {
                if (c.isAlive(gen)) { live = c; break; }
            }
            assertNotNull("the predicate must always have exactly one live clause", live);
            assertTrue(store.retractClause(p, live));
            List<Term> args = new ArrayList<Term>(1);
            args.add(it.denzosoft.jprolog.core.terms.Number.valueOf(i + 1));
            store.assertRule(new Rule(new CompoundTerm(new Atom("cc"), args), new ArrayList<Term>()), false);
            max = Math.max(max, store.lookup("cc", 1).size());
        }
        assertTrue("physical clause count must stay bounded during the loop, peaked at " + max,
            max <= 128);
        assertEquals(1, solve("cc(5000).").size());
    }

    // ================================================================ result shape

    @Test
    public void testISS0444_ResultsAreKeyedByQueryVariableName() {
        List<Map<String, Term>> sols = solve("X = 1, Y = f(X).");
        assertEquals(1, sols.size());
        assertEquals("1", sols.get(0).get("X").toString());
        assertEquals("f(1)", sols.get(0).get("Y").toString());
    }

    @Test
    public void testISS0442_RepeatedQueryVariableIsOneCell() {
        // The query term is normalised so all occurrences of a NAME share one cell; without that,
        // identity variables would make X = 1, X = 2 succeed.
        assertEquals(0, solve("X = 1, X = 2.").size());
        assertEquals(1, solve("f(X, X) = f(1, 1).").size());
        assertEquals(0, solve("f(X, X) = f(1, 2).").size());
    }

    @Test
    public void testISS0444_SolveStreamIsLazyOnV4() {
        consult("nat(0).\nnat(N) :- nat(M), N is M+1.\n");
        final int[] seen = {0};
        prolog.solveStream("nat(X).", sol -> { seen[0]++; return seen[0] < 5; });
        assertEquals("the sink stops the search", 5, seen[0]);
    }
}
// END_CHANGE: ISS-2025-0438..0447
