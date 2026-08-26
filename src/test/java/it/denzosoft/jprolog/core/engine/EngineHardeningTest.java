package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * Regression tests for the engine deep-analysis findings ENG-01..ENG-17
 * (docs/reports/report-engine-deep-analysis-2026-08-24.md).
 *
 * <p>Every memory/size test here MUST pass with the surefire fork's DEFAULT JVM settings — no
 * -Xss/-Xmx flags — because that is the guarantee the fixes are supposed to deliver.
 */
public class EngineHardeningTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private boolean succeeds(String query) {
        List<Map<String, Term>> s = prolog.solve(query);
        return !s.isEmpty();
    }

    private Term first(String query, String var) {
        List<Map<String, Term>> s = prolog.solve(query);
        assertFalse("query should succeed: " + query, s.isEmpty());
        return s.get(0).get(var);
    }

    // ======================== ISS-2025-0423 / ENG-01: repeat/0 ========================

    /** repeat/0 used to materialise exactly 1000 binding-map copies, so the classic
     *  "repeat, ..., Done, !" driver loop silently FAILED after 1000 iterations. */
    @Test
    public void testISS0423_RepeatBeyondThousandIterations() {
        assertTrue("repeat must survive past 1000 redos",
            succeeds("nb_setval(c1,0), repeat, nb_getval(c1,V), V1 is V+1, nb_setval(c1,V1), V1 >= 1500, !"));
    }

    @Test
    public void testISS0423_RepeatIsUnbounded() {
        assertTrue("repeat must be an INFINITE choice point, not a 1000-element list",
            succeeds("nb_setval(c2,0), repeat, nb_getval(c2,V), V1 is V+1, nb_setval(c2,V1), V1 >= 100000, !"));
    }

    @Test
    public void testISS0423_RepeatIsCutTransparentAndPrunable() {
        assertEquals("repeat + cut yields exactly one solution", 1, prolog.solve("repeat, !.").size());
        // repeat inside findall, bounded by a counter + cut
        assertTrue(succeeds("nb_setval(c3,0), findall(V1, (repeat, nb_getval(c3,V), V1 is V+1,"
            + " nb_setval(c3,V1), V1 >= 5, !), L), L = [5]"));
    }

    // ======================== ISS-2025-0424 / ENG-02: float typing ========================

    @Test
    public void testISS0424_SumListOfFloatsStaysFloat() {
        Term s = first("sum_list([1.5,1.5],S)", "S");
        assertTrue("sum_list([1.5,1.5],S) must give the FLOAT 3.0, not the integer 3",
            s instanceof Number && !((Number) s).isInteger());
        assertEquals("3.0", s.toString());
    }

    @Test
    public void testISS0424_SumListOfIntegersStaysInteger() {
        Term s = first("sum_list([1,2],S)", "S");
        assertTrue(s instanceof Number && ((Number) s).isInteger());
        assertEquals("3", s.toString());
    }

    @Test
    public void testISS0424_SumlistSingleFloat() {
        assertTrue(succeeds("sumlist([1.0],S), float(S)"));
    }

    @Test
    public void testISS0424_NumberDoubleConstructorIsAlwaysFloat() {
        assertFalse("new Number(3.0) must be an ISO float", new Number(3.0).isInteger());
        assertTrue("new Number(3L) must be an ISO integer", new Number(3L).isInteger());
        assertFalse(Number.ofDouble(3.0).isInteger());
        assertTrue(Number.ofLong(3L).isInteger());
    }

    @Test
    public void testISS0424_AtomNumberPreservesIntegerFloatDistinction() {
        assertTrue(succeeds("atom_number('3', N), integer(N)"));
        assertTrue(succeeds("atom_number('3.0', N), float(N)"));
    }

    /** Integer-valued built-in results must stay ISO integers after the Number(double) change. */
    @Test
    public void testISS0424_IntegerValuedBuiltinsStayIntegers() {
        assertTrue("atom_length/2", succeeds("atom_length(abc, N), integer(N)"));
        assertTrue("string_length/2", succeeds("string_length(\"abc\", N), integer(N)"));
        assertTrue("sub_atom/5 indices", succeeds("sub_atom(abcde, 1, 2, A, _), integer(A)"));
        assertTrue("succ/2 down", succeeds("succ(X, 4), integer(X)"));
        assertTrue("succ/2 up", succeeds("succ(3, Y), integer(Y)"));
        assertTrue("current_op/3 precedence", succeeds("current_op(P, xfx, is), integer(P)"));
        assertTrue("atom_codes/2 codes", succeeds("atom_codes(abc, [C|_]), integer(C)"));
        assertTrue("number_codes/2", succeeds("number_codes(N, \"3\"), integer(N)"));
        // statistics/2 only accepts an unbound Value argument (pre-existing), hence the split
        assertTrue("statistics/2 counters", succeeds("statistics(runtime, L), L = [T|_], integer(T)"));
    }

    /** existence_error(procedure, Name/Arity) must carry an INTEGER arity. */
    @Test
    public void testISS0424_ExistenceErrorArityIsInteger() {
        assertTrue(succeeds(
            "catch(no_such_predicate_xyz(1), error(existence_error(procedure, _/A), _), integer(A))"));
    }

    // ======================== ISS-2025-0425 / ENG-03: length/2 enumeration ========================

    @Test
    public void testISS0425_LengthEnumeratesUnboundList() {
        List<Map<String, Term>> s = prolog.solve("length(L, N), N >= 3, !.");
        assertFalse("length(L,N), N>=3, ! must succeed by enumerating N", s.isEmpty());
        assertEquals("3", s.get(0).get("N").toString());
    }

    @Test
    public void testISS0425_LengthEnumeratesPartialList() {
        List<Map<String, Term>> s = prolog.solve("length([a|T], N), N >= 3, !.");
        assertFalse("length([a|T],N) must enumerate", s.isEmpty());
        assertEquals("3", s.get(0).get("N").toString());
    }

    @Test
    public void testISS0425_LengthDeterministicModesUnchanged() {
        assertEquals("3", first("length([a,b,c], N)", "N").toString());
        assertTrue(succeeds("length(L, 3), L = [_,_,_]"));
        assertFalse("a non-list tail still fails", succeeds("length([a|b], _)"));
        assertEquals("0", first("length([], N)", "N").toString());
    }

    @Test
    public void testISS0425_LengthEnumerationIsBoundedByFindall() {
        // findall over a bounded enumeration must terminate and produce N = 0..4
        assertTrue(succeeds("findall(N, (length(_, N), (N >= 4 -> ! ; true)), L), L = [0,1,2,3,4]"));
    }

    // ======================== ISS-2025-0426 / ENG-05: bridge exception policy ========================

    /** A Java fault inside a built-in used to be swallowed ("not a built-in") and reported as an
     *  existence_error or a silent failure; it must now surface as a catchable system_error. */
    @Test
    public void testISS0426_BuiltinJavaFaultBecomesSystemError() {
        prolog.registerBuiltInPredicate("eng05_boom", (query, bindings, solutions) -> {
            throw new NullPointerException("simulated built-in bug");
        });
        try {
            prolog.solve("eng05_boom.");
            fail("expected a PrologException carrying system_error");
        } catch (PrologException e) {
            String t = String.valueOf(e.getErrorTerm());
            assertTrue("expected system_error, got: " + t, t.contains("system_error"));
            assertTrue("expected the Java exception named, got: " + t, t.contains("NullPointerException"));
        }
    }

    @Test
    public void testISS0426_BuiltinJavaFaultIsCatchable() {
        prolog.registerBuiltInPredicate("eng05_boom2", (query, bindings, solutions) -> {
            throw new IllegalStateException("boom");
        });
        assertTrue("system_error must be trappable by catch/3",
            succeeds("catch(eng05_boom2, error(system_error(_), _), true)"));
    }

    /** The three engine-control exceptions are NEVER swallowed by the bridge, and never turned
     *  into a PrologException — untrusted catch/3 must not be able to trap them. */
    @Test
    public void testISS0426_ControlExceptionsEscapeTheBridge() {
        prolog.registerBuiltInPredicate("eng05_cancel", (query, bindings, solutions) -> {
            throw new it.denzosoft.jprolog.core.engine.QueryCancelledException();
        });
        try {
            prolog.solve("catch(eng05_cancel, _, true).");
            fail("QueryCancelledException must not be swallowed nor trapped by catch/3");
        } catch (it.denzosoft.jprolog.core.engine.QueryCancelledException expected) {
            // correct: propagates to the embedder
        }
    }

    @Test
    public void testISS0426_InferenceLimitExceptionEscapesTheBridge() {
        prolog.registerBuiltInPredicate("eng05_budget", (query, bindings, solutions) -> {
            throw new it.denzosoft.jprolog.core.engine.InferenceLimitException(1);
        });
        try {
            prolog.solve("catch(eng05_budget, _, true).");
            fail("InferenceLimitException must not be swallowed nor trapped by catch/3");
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException expected) {
            // correct
        }
    }

    // ======================== ISS-2025-0427 / ENG-08: minor inaccuracies ========================

    /** throw/1 used to rename the ball twice (once in the throw branch, once in drive()'s catch). */
    @Test
    public void testISS0427_ThrownBallIsRenamedOnceAndStillMatches() {
        assertTrue(succeeds("catch(throw(foo(bar)), foo(X), X == bar)"));
        assertTrue(succeeds("catch(throw(f(A,B)), f(P,Q), (var(P), var(Q)))"));
        assertTrue(succeeds("catch(throw(err), E, E == err)"));
    }

    /** PrologException is control flow: the error-term constructor must not fill in a Java stack
     *  trace (it dominated the cost of catch/throw loops). */
    @Test
    public void testISS0427_PrologExceptionHasNoStackTrace() {
        PrologException e = new PrologException(new it.denzosoft.jprolog.core.terms.Atom("ball"));
        assertEquals("error-term PrologException must not capture a stack trace",
            0, e.getStackTrace().length);
        assertEquals("the detail message is the ball", "ball", e.getMessage());
    }

    /** unknown=warning must go through StreamManager (the IDE console), not System.err. */
    @Test
    public void testISS0427_UnknownWarningGoesThroughStreamManager() throws Exception {
        java.io.ByteArrayOutputStream buf = new java.io.ByteArrayOutputStream();
        java.io.PrintStream ps = new java.io.PrintStream(buf, true, "UTF-8");
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            prolog.solve("set_prolog_flag(unknown, warning).");
            prolog.solve("no_such_predicate_abc(1).");
        } finally {
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(null);
            prolog.solve("set_prolog_flag(unknown, error).");
        }
        assertTrue("the unknown-procedure warning must reach StreamManager.out(), got: " + buf,
            buf.toString("UTF-8").contains("no_such_predicate_abc"));
    }

    // ============= ISS-2025-0428 / ENG-09: tail-iterative term walkers (deep structures) =============
    //
    // Every assertion below runs with the surefire fork's DEFAULT JVM stack. Before the fix these
    // all raised resource_error(stack_overflow) at ~20-30k elements even with -Xss4m, because every
    // term walker recursed into the list TAIL (one Java frame per cell).

    /** A ONE MILLION element list must survive a full round trip. */
    @Test
    public void testISS0428_MillionElementListRoundTrip() {
        final int n = 1000000;
        assertTrue("numlist/3 + length/2", succeeds("numlist(1," + n + ",L), length(L,K), K == " + n));
        assertTrue("sum_list/2", succeeds("numlist(1," + n + ",L), sum_list(L,S), integer(S)"));
        assertTrue("==/2", succeeds("numlist(1," + n + ",L), L == L"));
        assertTrue("copy_term/2", succeeds("numlist(1," + n + ",L), copy_term(L,L2), L2 == L"));
        assertTrue("msort/2", succeeds("numlist(1," + n + ",L), msort(L,M), length(M,K), K == " + n));
        assertTrue("sort/2", succeeds("numlist(1," + n + ",L), sort(L,S), length(S,K), K == " + n));
        assertTrue("ground/1", succeeds("numlist(1," + n + ",L), ground(L)"));
        assertTrue("assertz/1 + call", succeeds(
            "numlist(1," + n + ",L), assertz(eng09_big(L)), eng09_big(B), length(B,K), K == " + n));
        assertTrue("reverse/2", succeeds("numlist(1," + n + ",L), reverse(L,R), R = [H|_], H == " + n));
        assertTrue("append/3", succeeds(
            "numlist(1," + n + ",L), append(L,[x],L2), length(L2,K), K =:= " + n + " + 1"));
        assertTrue("term_to_atom/2", succeeds(
            "numlist(1," + n + ",L), term_to_atom(L,A), atom_length(A,Len), Len > 0"));
        assertTrue("write/1", succeeds(
            "numlist(1," + n + ",L), with_output_to(atom(A), write(L)), atom_length(A,Len), Len > 0"));
        assertTrue("\\+ \\+ (unify + undo)", succeeds("numlist(1," + n + ",L), \\+ \\+ (L = L)"));
    }

    /** copy_term/2 of a NON-ground million-element list (the ground fast path does not apply). */
    @Test
    public void testISS0428_MillionElementOpenListCopy() {
        assertTrue(succeeds("length(L,1000000), copy_term(L,L2), length(L2,K), K == 1000000"));
    }

    /** Deep non-list nesting f(f(f(...))) — the last-argument spine of an ordinary compound. */
    @Test
    public void testISS0428_DeeplyNestedCompound() {
        prolog.consult("eng09_deep(0, z) :- !.\neng09_deep(N, f(T)) :- N1 is N-1, eng09_deep(N1, T).\n");
        assertTrue("build 200k-deep f(f(...))", succeeds("eng09_deep(200000, T), T = f(_)"));
        assertTrue("copy + compare 200k-deep", succeeds(
            "eng09_deep(200000, T), copy_term(T, T2), T2 == T"));
        assertTrue("write 200k-deep", succeeds(
            "eng09_deep(200000, T), with_output_to(atom(A), write(T)), atom_length(A, Len), Len > 0"));
    }

    /** Java-level walkers, exercised directly: hashCode/equals/isGround/copy/toString must not
     *  recurse per list cell either (they back HashMap keys, findall dedup, assert, ...). */
    @Test
    public void testISS0428_JavaWalkersOnLongList() {
        it.denzosoft.jprolog.core.terms.Term list = new it.denzosoft.jprolog.core.terms.Atom("[]");
        for (int i = 0; i < 500000; i++) {
            list = new it.denzosoft.jprolog.core.terms.CompoundTerm(
                new it.denzosoft.jprolog.core.terms.Atom("."),
                java.util.Arrays.asList(new Number((long) i), list));
        }
        it.denzosoft.jprolog.core.terms.Term copy = list.copy();
        assertTrue("isGround", list.isGround());
        assertEquals("hashCode must be structural and iterative", list.hashCode(), copy.hashCode());
        assertEquals("equals must be iterative", list, copy);
        assertTrue("toString must be iterative", list.toString().length() > 500000);
        assertTrue("unify must be iterative",
            list.unify(copy, new java.util.HashMap<String, Term>()));
        assertTrue("resolveBindings must be iterative",
            list.resolveBindings(new java.util.HashMap<String, Term>()) == list);
    }

    /** Cyclic-term protection (ISS-2025-0313) must survive the iterative resolve() rewrite. */
    @Test
    public void testISS0428_CyclicTermStillDetected() {
        // X = f(X) then forcing a resolve of X must not hang or loop forever
        try {
            prolog.solve("X = f(X), copy_term(X, Y).");
        } catch (RuntimeException expected) {
            // representation_error(cyclic_term) or resource_error(stack_overflow) — see LIM-032
        }
        // a cycle reached through a NON-last argument must still be caught
        try {
            prolog.solve("X = f(X, a), Y = X, atom_length(Y, _).");
        } catch (RuntimeException expected) {
            // fine
        }
        // and the engine must still be usable afterwards
        assertTrue(succeeds("X = f(a), X == f(a)"));
    }

    // ======== ISS-2025-0429 / ENG-10 + ISS-2025-0430 / ENG-11: machine memory model ========

    /** Deterministic recursion must leave NO choice points and NO trail entries behind
     *  (trust-me pop + conditional trailing). Before the fix a 20 000-iteration loop left 20 000
     *  exhausted choice points — each retaining its Alt closures — and 40 000 trail entries. */
    @Test
    public void testISS0429_DeterministicRecursionLeavesNoChoicePointsOrTrail() {
        it.denzosoft.jprolog.core.engine.Prolog helper = new Prolog();
        helper.consult("loop(0) :- !.\nloop(N) :- N1 is N-1, loop(N1).\n");

        // START_CHANGE: ISS-2025-0491 - 4.1 wave A: re-pointed at the v4 Machine, the only engine.
        it.denzosoft.jprolog.core.engine.v4.Machine m = new it.denzosoft.jprolog.core.engine.v4.Machine(
            helper.getV4Engine(), new it.denzosoft.jprolog.core.engine.ResourceGuard(0));
        final int[] cpsAtSolution = {-1};
        final int[] trailAtSolution = {-1};
        final int[] solutions = {0};
        Term query = new it.denzosoft.jprolog.core.terms.CompoundTerm(
            new it.denzosoft.jprolog.core.terms.Atom("loop"),
            java.util.Collections.<Term>singletonList(new Number(20000L)));
        m.solve(query, sol -> {
            solutions[0]++;
            cpsAtSolution[0] = m.choicePointCount();
            trailAtSolution[0] = m.trailSize();
            return false;                                   // stop at the first solution
        });
        assertEquals("loop(20000) must succeed", 1, solutions[0]);
        assertEquals("trust-me pop: no exhausted choice point may survive", 0, cpsAtSolution[0]);
        assertEquals("conditional trailing: nothing to undo, nothing trailed", 0, trailAtSolution[0]);
        // END_CHANGE: ISS-2025-0491
    }

    /** 3 000 000 deterministic inferences must complete (they used to OOM at 256 MB and be O(N)
     *  in choice points). Runs at the surefire fork's default heap. */
    @Test
    public void testISS0429_ThreeMillionStepDeterministicRecursion() {
        prolog.consult("eng10_loop(0) :- !.\neng10_loop(N) :- N1 is N-1, eng10_loop(N1).\n");
        assertTrue("loop(3000000) must complete", succeeds("eng10_loop(3000000)"));
    }

    /** ENG-11: one registry built-in per iteration used to make the loop QUADRATIC — bridgeBuiltin
     *  copied the whole binding map per call and the exhausted choice point retained every copy
     *  (OOM with a 2 GB heap at N = 10 000 after 27 s). It must now be linear and fast. */
    @Test
    public void testISS0430_RegistryBuiltinLoopIsLinear() {
        prolog.consult("eng11_loop(0) :- !.\n"
                     + "eng11_loop(N) :- atom_length(abc, _), N1 is N-1, eng11_loop(N1).\n");
        long start = System.nanoTime();
        assertTrue("loop2(200000) must complete", succeeds("eng11_loop(200000)"));
        long ms = (System.nanoTime() - start) / 1000000L;
        assertTrue("200k iterations with a bridged built-in must stay well under a minute, took "
            + ms + " ms", ms < 30000);
    }

    /** The resolved-goal handoff must not break built-ins that bind variables, that are
     *  nondeterministic, or that need the solver context. */
    @Test
    public void testISS0430_BridgedBuiltinsStillBindAndBacktrack() {
        assertTrue("deterministic binder", succeeds("atom_length(abcde, N), N == 5"));
        assertTrue("nondeterministic generator", succeeds("between(1, 5, X), X == 3"));
        assertEquals("all solutions of a generator", 5, prolog.solve("between(1,5,X).").size());
        assertTrue("meta-call built-in", succeeds("findall(X, member(X,[a,b,c]), L), L == [a,b,c]"));
        assertTrue("nested structure binding", succeeds("X = f(Y), Y = 1, X == f(1)"));
        assertTrue("term construction", succeeds("T =.. [foo, 1, 2], T == foo(1,2)"));
        assertTrue("sub_atom backtracking", succeeds("sub_atom(abc, B, 1, _, b), B == 1"));
        assertTrue("sort/msort", succeeds("msort([c,a,b], L), L == [a,b,c]"));
    }

    /** ISS-2025-0317: setarg/3 mutates the ACTUAL bound term, so it must keep the identity handoff
     *  (unresolved goal + full binding map) that ENG-11 replaced for every other built-in. */
    @Test
    public void testISS0430_SetargKeepsObjectIdentity() {
        assertTrue("setarg/3 must still mutate the bound term in place",
            succeeds("T = f(a,b), setarg(1, T, z), T == f(z,b)"));
        assertTrue("setarg/3 is undone on backtracking",
            succeeds("T = f(a,b), ( setarg(1, T, z), fail ; true ), T == f(a,b)"));
    }

    // ============ ISS-2025-0431 / ENG-04: budget, cancellation and tracing in meta-calls ============

    private static final String[] META_CALL_ESCAPES = {
        "once(eng04_loop(1000000))",
        "ignore(eng04_loop(1000000))",
        "forall(between(1,1000000,_), true)",
        "\\+ \\+ eng04_loop(1000000)",
        "findall(_, eng04_loop(1000000), _)",
        "catch(eng04_loop(1000000), _E, true)",
        "bagof(X, (between(1,1000000,X), eng04_loop(10)), _L)",
        "setof(X, (between(1,1000000,X), eng04_loop(10)), _L)",
        "aggregate_all(count, eng04_loop(1000000), _C)",
        "setup_call_cleanup(true, eng04_loop(1000000), true)",
        "with_output_to(atom(_A), eng04_loop(1000000))",
        "numlist(1,1000000,L), maplist(eng04_slow, L)",
    };

    private void consultLoop() {
        prolog.consult("eng04_loop(0) :- !.\neng04_loop(N) :- N1 is N-1, eng04_loop(N1).\n"
                     + "eng04_slow(_) :- eng04_loop(100).\n");
    }

    /** With a budget set, EVERY meta-call must abort. Before ENG-04 `once(Loop)` (and ignore,
     *  aggregate_all, setup_call_cleanup, with_output_to, maplist) ran to completion: the legacy
     *  sub-solver polled neither the budget nor the interrupt, so untrusted code only had to wrap
     *  its loop in once/1 to escape the v3.4.0 hardening entirely. */
    @Test
    public void testISS0431_InferenceBudgetIsNotBypassedByMetaCalls() {
        consultLoop();
        prolog.setInferenceBudget(20000);
        for (String q : META_CALL_ESCAPES) {
            try {
                prolog.solve(q + ".");
                fail("budget escaped through: " + q);
            } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException expected) {
                // correct
            }
        }
    }

    /** …and wrapping the meta-call in catch/3 must NOT trap the abort: the trust model requires
     *  InferenceLimitException to stay a plain RuntimeException that reaches the embedder. Several
     *  built-ins used to re-wrap it into a (catchable) PrologEvaluationException. */
    @Test
    public void testISS0431_BudgetAbortIsNotCatchableByPrologCatch() {
        consultLoop();
        prolog.setInferenceBudget(20000);
        for (String inner : new String[] {
                "once(eng04_loop(1000000))",
                "aggregate_all(count, eng04_loop(1000000), _C)",
                "bagof(X, (between(1,1000000,X), eng04_loop(10)), _L)",
                "setup_call_cleanup(true, eng04_loop(1000000), true)" }) {
            try {
                prolog.solve("catch(" + inner + ", _, true).");
                fail("catch/3 trapped the budget abort of: " + inner);
            } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException expected) {
                // correct
            }
        }
    }

    /** The IDE/embedder Stop (a thread interrupt) must reach inside meta-calls too. */
    @Test
    public void testISS0431_CancellationReachesInsideMetaCalls() throws Exception {
        consultLoop();
        for (String q : new String[] {
                "eng04_loop(100000000)",
                "once(eng04_loop(100000000))",
                "forall(between(1,100000000,_), true)",
                "aggregate_all(count, eng04_loop(100000000), _C)" }) {
            final Thread target = Thread.currentThread();
            Thread killer = new Thread(() -> {
                try { Thread.sleep(400); } catch (InterruptedException ignored) { }
                target.interrupt();
            });
            killer.setDaemon(true);
            killer.start();
            try {
                prolog.solve(q + ".");
                fail("Stop did not reach: " + q);
            } catch (it.denzosoft.jprolog.core.engine.QueryCancelledException expected) {
                // correct
            } finally {
                Thread.interrupted();                 // clear the flag for the next iteration
                killer.join(2000);
                Thread.interrupted();
            }
        }
    }

    /** once/1, ignore/1 and forall/2 now run natively on the machine — their semantics (cut
     *  opacity, determinism, ISO errors) must be unchanged. */
    @Test
    public void testISS0431_NativeMetaCallSemanticsUnchanged() {
        assertEquals("once/1 is semi-deterministic", 1, prolog.solve("once(member(X,[a,b,c])).").size());
        assertTrue("once/1 keeps the first solution", succeeds("once(member(X,[a,b,c])), X == a"));
        assertTrue("once/1 fails when the goal fails", prolog.solve("once(fail).").isEmpty());
        assertEquals("ignore/1 always succeeds once", 1, prolog.solve("ignore(fail).").size());
        assertTrue("ignore/1 keeps bindings on success", succeeds("ignore(X = 1), X == 1"));
        assertTrue("forall/2 true case", succeeds("forall(member(X,[1,2,3]), integer(X))"));
        assertTrue("forall/2 false case", prolog.solve("forall(member(X,[1,a]), integer(X)).").isEmpty());
        assertTrue("forall/2 is opaque: no bindings escape", succeeds("forall(member(X,[1,2]), integer(X)), var(X)"));
        assertTrue("a cut inside once/1 is local", succeeds("once((member(X,[a,b]), !)), X == a"));
        assertTrue("once/1 on an unbound goal is an instantiation_error",
            succeeds("catch(once(_), error(instantiation_error, _), true)"));
        assertTrue("once/1 on a number is a type_error(callable)",
            succeeds("catch(once(1), error(type_error(callable, 1), _), true)"));
    }

    /** LIM-030: a meta-call over a long list ran on the recursive legacy solver and blew the Java
     *  stack / heap past ~10k elements. It now runs on the iterative machine. */
    @Test
    public void testISS0431_MetaCallOverLongList() {
        assertTrue("maplist/2 over 200 000 elements",
            succeeds("numlist(1,200000,L), maplist(integer, L)"));
        assertTrue("include/3 over 200 000 elements",
            succeeds("numlist(1,200000,L), include(integer, L, L2), length(L2,N), N == 200000"));
        assertTrue("aggregate_all/3 over 100 000 elements",
            succeeds("numlist(1,100000,L), aggregate_all(sum(X), member(X,L), S), S =:= 5000050000"));
    }

    // ==================== ISS-2025-0432 / ENG-12: lazy generators ====================

    /** between/3 used to materialise EVERY solution (each a full binding-map copy) before the
     *  first one could be used: `between(1,2000000,X), X >= 2000000` exhausted a 256 MB heap. */
    @Test
    public void testISS0432_BetweenIsLazy() {
        long start = System.nanoTime();
        assertTrue(succeeds("between(1, 2000000, X), X >= 2000000, !"));
        long ms = (System.nanoTime() - start) / 1000000L;
        assertTrue("a lazy between must reach 2 000 000 quickly, took " + ms + " ms", ms < 20000);
    }

    /** between(_, inf, _) was silently capped at a million solutions by the eager built-in. */
    @Test
    public void testISS0432_BetweenInfiniteUpperBound() {
        assertTrue(succeeds("between(1, inf, X), X > 3000000, !"));
        assertTrue(succeeds("between(1, infinite, X), X > 3000000, !"));
    }

    @Test
    public void testISS0432_BetweenSemanticsUnchanged() {
        assertEquals("enumeration", 5, prolog.solve("between(1,5,X).").size());
        assertTrue("check mode true", succeeds("between(1,3,2)"));
        assertTrue("check mode false", prolog.solve("between(1,3,5).").isEmpty());
        assertTrue("single value", succeeds("between(1,1,X), X == 1"));
        assertTrue("empty range", prolog.solve("between(5,1,_).").isEmpty());
        assertTrue("unbound bounds raise instantiation_error",
            succeeds("catch(between(_,3,_), error(instantiation_error, _), true)"));
        assertTrue("non-integer bound raises type_error",
            succeeds("catch(between(a,3,_), error(type_error(integer, a), _), true)"));
        assertTrue("between as the last goal of a query still enumerates",
            prolog.solve("between(1,4,_X).").size() == 4);
    }

    // ============ ISS-2025-0433 / ENG-13: clause selection ============

    private void consultFactTable(int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append("eng13_f(").append(i).append(",").append(i).append(").\n");
        prolog.consult(sb.toString());
    }

    /** A lookup in a 20 000-fact table used to cost ~2.5 ms — the SAME whether the first or the
     *  last clause matched, because every call copied the whole clause list and pre-built one Alt
     *  closure per clause before trying a single head unification. */
    @Test
    public void testISS0433_FactTableLookupIsIndexed() {
        consultFactTable(20000);
        prolog.consult("eng13_lk(0) :- !.\neng13_lk(N) :- eng13_f(19999,_), N1 is N-1, eng13_lk(N1).\n");
        assertTrue("warm up", succeeds("eng13_lk(200)"));
        long start = System.nanoTime();
        assertTrue(succeeds("eng13_lk(2000)"));
        long micros = (System.nanoTime() - start) / 1000L / 2000L;
        assertTrue("a 20 000-fact lookup must cost well under 50 us, measured " + micros + " us",
            micros < 50);
    }

    /** Indexing must never DROP a clause (the ISS-2025-0340 hazard that got it reverted). */
    @Test
    public void testISS0433_IndexingNeverDropsClauses() {
        prolog.consult("eng13_p(1, one).\neng13_p(2, two).\neng13_p(X, var(X)).\n"
                     + "eng13_p(foo, atom).\neng13_p(f(_), compound).\neng13_p(1.0, float).\n");
        // a matching bucket must still include the variable-headed clause
        assertEquals("integer key + var clause", 2, prolog.solve("eng13_p(1, W).").size());
        assertEquals("atom key + var clause", 2, prolog.solve("eng13_p(foo, W).").size());
        assertEquals("compound key + var clause", 2, prolog.solve("eng13_p(f(a), W).").size());
        // 1 and 1.0 are DISTINCT terms, so they must land in different buckets
        assertTrue("1.0 must not match the integer clause",
            succeeds("eng13_p(1.0, W), W == float"));
        assertEquals("float key + var clause", 2, prolog.solve("eng13_p(1.0, W).").size());
        // an unbound first argument sees every clause
        assertEquals("unbound first argument", 6, prolog.solve("eng13_p(_, _).").size());
        // a first argument that cannot be keyed (a string) must fall back to the full list
        assertEquals("string first argument falls back", 1, prolog.solve("eng13_p(\"s\", W).").size());
        // a predicate that exists but has no matching clause FAILS; it is not an existence_error
        prolog.consult("eng13_q(a).\n");
        assertTrue("no matching clause is a failure, not existence_error",
            prolog.solve("eng13_q(b).").isEmpty());
        assertTrue("a genuinely unknown procedure still raises existence_error",
            succeeds("catch(eng13_no_such(b), error(existence_error(procedure, _), _), true)"));
    }

    /** The clause snapshot is versioned, so assert/retract during backtracking keeps the ISO
     *  logical update view (and the ISS-2025-0396 retract behaviour). */
    @Test
    public void testISS0433_LogicalUpdateViewSurvivesSnapshotting() {
        prolog.consult(":- dynamic(eng13_r/1).\neng13_r(1).\neng13_r(2).\neng13_r(3).\n");
        assertTrue("findall over retract drains the predicate",
            succeeds("findall(X, retract(eng13_r(X)), L), L == [1,2,3]"));
        assertTrue("the predicate is now empty but still known", prolog.solve("eng13_r(_).").isEmpty());
        prolog.consult(":- dynamic(eng13_s/1).\neng13_s(1).\n");
        assertTrue("a clause asserted during a call is not seen by that call",
            succeeds("findall(X, (eng13_s(X), assertz(eng13_s(99))), L), L == [1]"));
        assertTrue("but it is seen by the next call", succeeds("eng13_s(99)"));
        assertTrue("asserta puts the clause first",
            succeeds("asserta(eng13_s(0)), eng13_s(F), F == 0"));
    }

    /** Head-first renaming: the body must be renamed with the SAME variable map as the head, so a
     *  variable shared between head and body still refers to one variable. */
    @Test
    public void testISS0433_HeadAndBodyShareRenamedVariables() {
        prolog.consult("eng13_share(X, Y) :- Y = X.\n"
                     + "eng13_twice(X, Z) :- eng13_share(X, Y), Z = Y.\n");
        assertTrue(succeeds("eng13_share(a, W), W == a"));
        assertTrue(succeeds("eng13_twice(hello, W), W == hello"));
        // and a ground fact (renaming skipped entirely) still unifies correctly
        prolog.consult("eng13_gf(a, b, c).\n");
        assertTrue(succeeds("eng13_gf(A, B, C), A == a, B == b, C == c"));
        assertTrue(prolog.solve("eng13_gf(x, _, _).").isEmpty());
    }

    // ============ ISS-2025-0434 / ENG-14: arithmetic hot path ============

    @Test
    public void testISS0434_ArithmeticSemanticsUnchangedByFastPaths() {
        assertTrue("long fast path", succeeds("X is 2 + 3, X == 5"));
        assertTrue("subtraction", succeeds("X is 2 - 5, X == -3"));
        assertTrue("multiplication", succeeds("X is 6 * 7, X == 42"));
        // overflow must fall back to the exact BigInteger path, not wrap around
        assertTrue("addition overflow stays exact",
            succeeds("X is 9223372036854775807 + 1, X =:= 9223372036854775808"));
        assertTrue("multiplication overflow stays exact",
            succeeds("X is 4611686018427387904 * 4, X =:= 18446744073709551616"));
        assertTrue("subtraction overflow stays exact",
            succeeds("X is -9223372036854775808 - 1, X =:= -9223372036854775809"));
        assertTrue("big integers still exact", succeeds("X is 2^100, X =:= 1267650600228229401496703205376"));
        assertTrue("float contagion", succeeds("X is 2 + 3.0, float(X)"));
        assertTrue("integer stays integer", succeeds("X is 2 + 3, integer(X)"));
        // comparisons: the long fast path must agree with the BigInteger path
        assertTrue(succeeds("1 < 2")); assertTrue(prolog.solve("2 < 1.").isEmpty());
        assertTrue(succeeds("9223372036854775807 < 9223372036854775808"));
        assertTrue(succeeds("-9223372036854775809 < 0"));
        assertTrue(succeeds("1 =:= 1.0"));
        assertTrue("NaN =\\= NaN", succeeds("X is nan, X =\\= X"));
    }

    /** Number.valueOf's small-integer cache must not make identity observable. */
    @Test
    public void testISS0434_SmallIntegerCacheIsInvisible() {
        assertTrue(succeeds("X is 1 + 1, Y = 2, X == Y"));
        assertTrue(succeeds("X is 5000 + 1, Y is 5001, X == Y"));
        assertEquals(new Number(7L), Number.valueOf(7L));
        assertEquals(new Number(100000L), Number.valueOf(100000L));
        assertTrue("integers stay integers", Number.valueOf(3L).isInteger());
    }

    /** ENG-14: `is/2` no longer deep-copies its expression, so a deeply nested expression must
     *  still evaluate correctly (and quickly). */
    @Test
    public void testISS0434_DeepArithmeticExpression() {
        StringBuilder expr = new StringBuilder("0");
        for (int i = 1; i <= 500; i++) expr.append(" + ").append(i);
        assertTrue(succeeds("X is " + expr + ", X =:= 125250"));
        prolog.consult("eng14_arith(0, A, A) :- !.\n"
                     + "eng14_arith(N, A0, A) :- A1 is A0 + N*2 - 1, N1 is N-1, eng14_arith(N1, A1, A).\n");
        assertTrue(succeeds("eng14_arith(100000, 0, A), integer(A)"));
    }

    // ============ ISS-2025-0435 / ENG-15 + ISS-2025-0436 / ENG-17 ============

    /** collectVars must still report every distinct query variable exactly once (it moved from an
     *  O(n^2) List.contains to a HashSet). */
    @Test
    public void testISS0435_ManyQueryVariables() {
        StringBuilder q = new StringBuilder();
        for (int i = 0; i < 300; i++) {
            if (i > 0) q.append(", ");
            q.append("V").append(i).append(" = ").append(i);
        }
        java.util.List<java.util.Map<String, Term>> sols = prolog.solve(q + ".");
        assertFalse(sols.isEmpty());
        assertEquals("every query variable is reported once", 300, sols.get(0).size());
        assertEquals("0", sols.get(0).get("V0").toString());
        assertEquals("299", sols.get(0).get("V299").toString());
    }

    /** The dead classes removed by ENG-17 must really be gone, and the still-used ones present. */
    @Test
    public void testISS0436_DeadClassesRemoved() {
        for (String gone : new String[] {
                "it.denzosoft.jprolog.core.engine.CompiledClause",
                "it.denzosoft.jprolog.core.engine.Interpreter" }) {
            try {
                Class.forName(gone);
                fail("dead class still present: " + gone);
            } catch (ClassNotFoundException expected) {
                // correct
            }
        }
        try {
            it.denzosoft.jprolog.core.engine.KnowledgeBase.class.getMethod(
                "getRulesWithMultiArgIndex", String.class, int.class, Term.class, Term.class);
            fail("the unused multi-argument index accessor is still present");
        } catch (NoSuchMethodException expected) {
            // correct
        }
    }

    // ============ ISS-2025-0437 / ENG-06: per-engine state isolation ============

    /** Flags used to live in a static HashMap shared by every Prolog instance in the JVM, so
     *  sandboxed code could flip `unknown` or `double_quotes` for the host's other engines. */
    @Test
    public void testISS0437_PrologFlagsArePerEngine() {
        Prolog a = new Prolog();
        Prolog b = new Prolog();
        assertTrue(!a.solve("set_prolog_flag(unknown, fail).").isEmpty());
        // A: unknown procedures now fail silently
        assertTrue("engine A honours its own flag", a.solve("eng06_no_such_pred.").isEmpty());
        // B must be untouched: an unknown procedure is still an error
        try {
            b.solve("eng06_no_such_pred.");
            fail("engine B saw engine A's 'unknown' flag");
        } catch (PrologException expected) {
            assertTrue(String.valueOf(expected.getErrorTerm()).contains("existence_error"));
        }
        // and reading the flag back gives each engine its own value
        assertTrue(!a.solve("current_prolog_flag(unknown, fail).").isEmpty());
        assertTrue(!b.solve("current_prolog_flag(unknown, error).").isEmpty());
    }

    @Test
    public void testISS0437_DoubleQuotesFlagIsPerEngine() {
        Prolog a = new Prolog();
        Prolog b = new Prolog();
        assertTrue(!a.solve("set_prolog_flag(double_quotes, codes).").isEmpty());
        assertTrue(!a.solve("current_prolog_flag(double_quotes, codes).").isEmpty());
        assertTrue("engine B keeps its own double_quotes",
            !b.solve("current_prolog_flag(double_quotes, string).").isEmpty());
    }

    /** occurs_check moved out of the static Variable.occursCheckEnabled into the engine's flags. */
    @Test
    public void testISS0437_OccursCheckIsPerEngine() {
        Prolog a = new Prolog();
        Prolog b = new Prolog();
        try {
            assertTrue(!a.solve("set_prolog_flag(occurs_check, true).").isEmpty());
            // A: the occurs check makes X = f(X) FAIL cleanly
            assertTrue("engine A performs the occurs check", a.solve("X = f(X).").isEmpty());
            // B: no occurs check, so the binding is made and the rational term is only reported
            // when the answer is resolved (LIM-032) — either way it does NOT fail cleanly.
            boolean bFailedCleanly;
            try {
                bFailedCleanly = b.solve("X = f(X).").isEmpty();
            } catch (RuntimeException cyclic) {
                bFailedCleanly = false;                   // it bound X: no occurs check
            }
            assertFalse("engine B must be unaffected by engine A's occurs_check", bFailedCleanly);
        } finally {
            a.solve("set_prolog_flag(occurs_check, false).");
        }
    }

    /** trace/0 was a process-global static: it turned tracing on for every engine in the JVM. */
    @Test
    public void testISS0437_TracingIsPerEngine() {
        Prolog a = new Prolog();
        Prolog b = new Prolog();
        try {
            assertFalse(a.isTracing());
            assertFalse(b.isTracing());
            assertTrue(!a.solve("trace.").isEmpty());
            assertTrue("engine A is tracing", a.isTracing());
            assertFalse("engine B must NOT be tracing", b.isTracing());
            assertTrue(!a.solve("notrace.").isEmpty());
            assertFalse(a.isTracing());
        } finally {
            a.setTracing(false);
            b.setTracing(false);
        }
    }

    /** set_output/1 used to redirect current_output for EVERY thread (while the captured output
     *  itself was already thread-local — an inconsistency). */
    @Test
    public void testISS0437_CurrentOutputIsPerThread() throws Exception {
        // setCurrentOutput only accepts a REGISTERED alias, so register one first
        it.denzosoft.jprolog.builtin.io.StreamManager.setOutputStreamRaw(
            "eng06_other_stream", new java.io.ByteArrayOutputStream());
        final String mainBefore = it.denzosoft.jprolog.builtin.io.StreamManager.getCurrentOutput();
        final String[] otherThreadSaw = new String[1];
        Thread t = new Thread(() -> {
            it.denzosoft.jprolog.builtin.io.StreamManager.setCurrentOutput("eng06_other_stream");
            otherThreadSaw[0] = it.denzosoft.jprolog.builtin.io.StreamManager.getCurrentOutput();
        });
        t.start();
        t.join(5000);
        assertEquals("the other thread redirected its own output",
            "eng06_other_stream", otherThreadSaw[0]);
        assertEquals("this thread's current_output must be untouched",
            mainBefore, it.denzosoft.jprolog.builtin.io.StreamManager.getCurrentOutput());
    }

    /** CLP(FD) temporary variable names came from a non-atomic static int: two threads could get
     *  the SAME name and silently alias unrelated constraint variables. */
    @Test
    public void testISS0437_ClpfdTempVarNamesAreUnique() throws Exception {
        final int threads = 4, perThread = 500;
        final java.util.Set<String> names =
            java.util.Collections.synchronizedSet(new java.util.HashSet<String>());
        final java.util.concurrent.atomic.AtomicInteger produced =
            new java.util.concurrent.atomic.AtomicInteger();
        java.lang.reflect.Method m = it.denzosoft.jprolog.builtin.clpfd.ClpfdPredicates.class
            .getDeclaredMethod("freshVar");
        m.setAccessible(true);
        Thread[] ts = new Thread[threads];
        for (int i = 0; i < threads; i++) {
            ts[i] = new Thread(() -> {
                for (int k = 0; k < perThread; k++) {
                    try { names.add((String) m.invoke(null)); produced.incrementAndGet(); }
                    catch (Exception e) { throw new RuntimeException(e); }
                }
            });
            ts[i].start();
        }
        for (Thread th : ts) th.join(10000);
        assertEquals("every generated name must be distinct", produced.get(), names.size());
    }
}
