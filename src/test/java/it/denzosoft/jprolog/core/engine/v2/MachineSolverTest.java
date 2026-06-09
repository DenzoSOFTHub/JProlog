package it.denzosoft.jprolog.core.engine.v2;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.parser.v2.TermReader;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

/** Validates the prototype {@link MachineSolver}: backtracking, cut, lazy enumeration, deep recursion. */
public class MachineSolverTest {

    private final OperatorTable ops = OperatorTable.getDefault();
    private Term parse(String s) { return TermReader.parseTerm(s, ops); }

    /** Build a Rule from a clause string ("h :- a, b" or a fact "h"). */
    private Rule rule(String s) {
        Term t = parse(s);
        if (t instanceof CompoundTerm && ":-".equals(((CompoundTerm) t).getName())
                && ((CompoundTerm) t).getArguments().size() == 2) {
            CompoundTerm c = (CompoundTerm) t;
            return new Rule(c.getArguments().get(0), flatten(c.getArguments().get(1)));
        }
        return new Rule(t, new ArrayList<>());
    }

    private List<Term> flatten(Term body) {
        List<Term> gs = new ArrayList<>();
        Term cur = body;
        while (cur instanceof CompoundTerm && ",".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            gs.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        gs.add(cur);
        return gs;
    }

    private List<String> allOf(MachineSolver m, String query, String var) {
        List<String> out = new ArrayList<>();
        m.solve(parse(query), sol -> { out.add(String.valueOf(sol.get(var))); return true; });
        return out;
    }

    @Test public void memberEnumerates() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("member(X, [X|_])"),
            rule("member(X, [_|T]) :- member(X, T)")));
        assertEquals(Arrays.asList("a", "b", "c"), allOf(m, "member(X, [a, b, c])", "X"));
    }

    @Test public void appendBacktracksAllSplits() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("append([], L, L)"),
            rule("append([H|T], L, [H|R]) :- append(T, L, R)")));
        List<String> xs = new ArrayList<>();
        m.solve(parse("append(X, Y, [a, b])"),
            sol -> { xs.add(sol.get("X") + "/" + sol.get("Y")); return true; });
        // [] / [a,b] ; [a] / [b] ; [a,b] / []
        assertEquals(3, xs.size());
        assertEquals("[]/[a,b]", xs.get(0).replace(" ", ""));
    }

    @Test public void cutPrunesAlternatives() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("p(1)"), rule("p(2)"), rule("p(3)"),
            rule("first(X) :- p(X), !")));
        assertEquals(Collections.singletonList("1"), allOf(m, "first(X)", "X"));
    }

    @Test public void disjunctionAndCutInBranch() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("q(a)"), rule("q(b)"),
            rule("r(X) :- (q(X) ; X = z)")));
        assertEquals(Arrays.asList("a", "b", "z"), allOf(m, "r(X)", "X"));
    }

    @Test public void lazyStopsAfterFirst() {
        // nat/1 has infinitely many solutions; lazy enumeration must return the first without looping.
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("nat(z)"),
            rule("nat(s(X)) :- nat(X)")));
        final List<String> got = new ArrayList<>();
        m.solve(parse("nat(N)"), sol -> { got.add(String.valueOf(sol.get("N"))); return got.size() < 3; });
        assertEquals(3, got.size());
        assertEquals("z", got.get(0));
    }

    @Test public void arithmeticFactorial() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("fact(0, 1)"),
            rule("fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1")));
        assertEquals(Collections.singletonList("120"), allOf(m, "fact(5, F)", "F"));
    }

    @Test public void ifThenElseMax() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("max(A, B, M) :- (A >= B -> M = A ; M = B)")));
        assertEquals(Collections.singletonList("7"), allOf(m, "max(3, 7, M)", "M"));
        assertEquals(Collections.singletonList("9"), allOf(m, "max(9, 2, M)", "M"));
    }

    @Test public void negationAsFailure() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("p(a)"), rule("p(b)"),
            rule("only_b(X) :- p(X), \\+ X = a")));
        assertEquals(Collections.singletonList("b"), allOf(m, "only_b(X)", "X"));
    }

    @Test public void typeChecksAndComparison() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("kind(X, var) :- var(X), !"),
            rule("kind(X, int) :- integer(X), !"),
            rule("kind(X, atom) :- atom(X), !"),
            rule("kind(_, other)")));
        assertEquals(Collections.singletonList("int"), allOf(m, "kind(42, K)", "K"));
        assertEquals(Collections.singletonList("atom"), allOf(m, "kind(hello, K)", "K"));
        assertEquals(Collections.singletonList("var"), allOf(m, "kind(_, K)", "K"));
    }

    // ---------------- builtin bridge (reuse the existing registry) ----------------
    private it.denzosoft.jprolog.core.engine.BuiltInRegistry registry() {
        return new it.denzosoft.jprolog.core.engine.Prolog().getBuiltInRegistry();
    }

    @Test public void bridgeDeterministicBuiltin() {
        MachineSolver m = new MachineSolver(new ArrayList<>(), registry());
        assertEquals(Collections.singletonList("5"), allOf(m, "atom_length(hello, L)", "L"));
    }

    @Test public void bridgeBuiltinInsideRule() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("twice(A, N) :- atom_length(A, L), N is L * 2")), registry());
        assertEquals(Collections.singletonList("6"), allOf(m, "twice(abc, N)", "N"));
    }

    @Test public void bridgeNondeterministicChoicePoint() {
        // between/3 yields several solutions -> the bridge must create a choice point
        MachineSolver m = new MachineSolver(new ArrayList<>(), registry());
        assertEquals(Arrays.asList("1", "2", "3", "4"), allOf(m, "between(1, 4, X)", "X"));
    }

    // ---------------- native meta-predicates: findall / catch / throw ----------------
    @Test public void findallCollectsAll() {
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("m(X, [X|_])"),
            rule("m(X, [_|T]) :- m(X, T)")));
        final String[] got = {null};
        m.solve(parse("findall(X, m(X, [a, b, c]), L)"),
            sol -> { got[0] = it.denzosoft.jprolog.core.write.v2.TermWriter.write(sol.get("L")); return false; });
        assertEquals("[a,b,c]", got[0]);
    }

    @Test public void catchCatchesThrow() {
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Collections.singletonList("boom"), allOf(m, "catch(throw(boom), E, true)", "E"));
    }

    @Test public void catchRecoveryRebinds() {
        // X=1 then throw; the recovery runs in a state where X's binding was unwound, then X=99.
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Collections.singletonList("99"), allOf(m, "catch((X = 1, throw(err)), err, X = 99)", "X"));
    }

    @Test public void catchPassesThroughWhenNoThrow() {
        MachineSolver m = new MachineSolver(Arrays.asList(rule("p(a)"), rule("p(b)")));
        assertEquals(Arrays.asList("a", "b"), allOf(m, "catch(p(X), _, fail)", "X"));
    }

    @Test public void catchNonMatchingRethrows() {
        // inner throw(other) is not caught by catcher 'expected' -> outer catch handles it
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Collections.singletonList("caught"),
            allOf(m, "catch(catch(throw(other), expected, fail), other, R = caught)", "R"));
    }

    // ---------------- database: assert / retract on the mutable KB ----------------
    @Test public void assertThenQuery() {
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Arrays.asList("1", "2"),
            allOf(m, "(assertz(fact(1)), assertz(fact(2)), fact(X))", "X"));
    }

    @Test public void assertaInsertsAtFront() {
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Arrays.asList("2", "1"),
            allOf(m, "(assertz(fact(1)), asserta(fact(2)), fact(X))", "X"));
    }

    @Test public void retractRemovesClause() {
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Collections.singletonList("2"),
            allOf(m, "(assertz(p(1)), assertz(p(2)), retract(p(1)), p(X))", "X"));
    }

    @Test public void assertRuleAndQuery() {
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Collections.singletonList("6"),
            allOf(m, "(assertz((dbl(X, Y) :- Y is X * 2)), dbl(3, R))", "R"));
    }

    // ---------------- review fixes ----------------
    @Test public void throwInsideFindallReachesOuterCatch() {   // ISS-0308
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("p(1)"), rule("p(2) :- throw(boom)")));
        assertEquals(Collections.singletonList("caught"),
            allOf(m, "catch(findall(X, p(X), L), boom, R = caught)", "R"));
    }

    @Test public void nativeErrorReachesCatch() {              // ISS-0309 (same path: error -> catch)
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Collections.singletonList("caught"),
            allOf(m, "catch((_ is foo + 1), error(type_error(evaluable, _), _), R = caught)", "R"));
    }

    @Test public void retractFactViaClauseForm() {             // ISS-0310
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Collections.singletonList("gone"),
            allOf(m, "(assertz(f(1)), retract((f(1) :- true)), (f(1) -> R = present ; R = gone))", "R"));
    }

    // ---------------- engine parity fixes (gap-closing) ----------------
    @Test public void softCutEnumeratesAll() {              // ISS-0201
        MachineSolver m = new MachineSolver(Arrays.asList(rule("c(a)"), rule("c(b)"), rule("c(c)")));
        assertEquals(Arrays.asList("a", "b", "c"), allOf(m, "(c(X) *-> true ; X = none)", "X"));
    }
    @Test public void softCutElseWhenCondFails() {
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertEquals(Collections.singletonList("none"), allOf(m, "(fail *-> true ; X = none)", "X"));
    }
    @Test public void ieeeArithComparison() {              // ISS-0274
        MachineSolver m = new MachineSolver(new ArrayList<>());
        assertFalse(allOf(m, "(-0.0 =:= 0.0, X = yes)", "X").isEmpty());   // -0.0 =:= 0.0 succeeds
        assertTrue(allOf(m, "(X is nan, X =:= X)", "X").isEmpty());        // nan =:= nan fails
    }
    @Test public void occursCheckRespectsFlag() {          // ISS-0246
        it.denzosoft.jprolog.core.terms.Variable.setOccursCheckEnabled(true);
        try {
            MachineSolver m = new MachineSolver(new ArrayList<>());
            assertTrue("X = f(X) must fail with occurs_check", allOf(m, "X = f(X)", "X").isEmpty());
            assertFalse("acyclic unify still works", allOf(m, "Y = f(a)", "Y").isEmpty());
        } finally {
            it.denzosoft.jprolog.core.terms.Variable.setOccursCheckEnabled(false);
        }
    }
    @Test public void moduleQualifiedStripsAndCalls() {    // ISS handled: M:Goal
        MachineSolver m = new MachineSolver(Arrays.asList(rule("p(1)"), rule("p(2)")));
        assertEquals(2, allOf(m, "mymod:p(X)", "X").size());
    }

    @Test public void deepRecursionNoStackOverflow() {
        // down/1 over a 200,000-deep s(...) term. The legacy recursive solver overflows the Java
        // stack around a few thousand; the iterative machine must handle it.
        MachineSolver m = new MachineSolver(Arrays.asList(
            rule("down(z)"),
            rule("down(s(X)) :- down(X)")));
        Term deep = new Atom("z");
        for (int i = 0; i < 200_000; i++) deep = new CompoundTerm(new Atom("s"), Arrays.asList(deep));
        Term query = new CompoundTerm(new Atom("down"), Arrays.asList(deep));
        final int[] count = {0};
        m.solve(query, sol -> { count[0]++; return false; });
        assertEquals("one solution, no StackOverflowError", 1, count[0]);
    }
}
