package it.denzosoft.jprolog.test.audit;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Production-readiness audit findings captured as executable tests
 * (report: docs/reports/report-production-readiness-audit-2026-06-09.md).
 * Each test encodes the CORRECT behaviour; ones that fail mark an open defect being fixed in priority
 * order. Issue ids: ISS-2025-0335 (sort/vars), 0336 (freeze bindings), 0337 (call/=.. ISO errors),
 * 0338 (sandbox / safe mode), 0339 (inference budget / resource limit).
 */
public class ProductionAuditTest {

    private Prolog p() { return new Prolog(); }

    private List<Map<String, Term>> solve(Prolog pl, String q) { return pl.solve(q); }

    // ---- ISS-2025-0335: sort/msort/sort-4 must not fail on lists containing unbound variables ----
    @Test public void testISS0335_msortWithVariableSucceeds() {
        // standard order: a variable is below all other terms, so the sort must succeed
        assertFalse("msort([1, X], L) must not fail", solve(p(), "msort([1, X], L).").isEmpty());
    }

    @Test public void testISS0335_sortWithVariablesSucceeds() {
        assertFalse("sort([X, Y], L) must not fail", solve(p(), "sort([X, Y], L).").isEmpty());
    }

    @Test public void testISS0335_msortMixedVarAndGroundSucceeds() {
        assertFalse("msort([X, 1, a], L) must not fail", solve(p(), "msort([X, 1, a], L).").isEmpty());
    }

    // ---- ISS-2025-0336: freeze/2 must propagate the bindings the woken goal makes ----
    @Test public void testISS0336_freezePropagatesBindings() {
        // Y is bound inside the frozen goal once X is instantiated; that binding must survive.
        assertFalse("freeze must propagate Y=hello", solve(p(), "freeze(X, Y = hello), X = 1, Y == hello.").isEmpty());
    }

    // ---- ISS-2025-0337: call/1 and =../2 must raise ISO errors, not silently fail ----
    @Test public void testISS0337_callNonCallableRaisesTypeError() {
        Prolog pl = p();
        List<Map<String, Term>> r = solve(pl, "catch(call(123), error(type_error(callable, 123), _), true).");
        assertFalse("call/1 on a number must raise type_error(callable, 123)", r.isEmpty());
    }

    @Test public void testISS0337_univUnderInstantiatedRaisesInstantiationError() {
        Prolog pl = p();
        List<Map<String, Term>> r = solve(pl, "catch((X =.. Y), error(instantiation_error, _), true).");
        assertFalse("X =.. Y with both unbound must raise instantiation_error", r.isEmpty());
    }

    // ---- ISS-2025-0338: safe mode removes host-touching built-ins (sandbox) ----
    @Test public void testISS0338_safeModeRemovesDangerousBuiltins() {
        Prolog pl = p();
        int removed = pl.enableSafeMode();
        assertTrue("safe mode must remove many dangerous built-ins (got " + removed + ")", removed > 40);
        assertTrue(pl.isSafeMode());
        assertNull("shell must be gone", pl.getBuiltInRegistry().getBuiltIn("shell"));
        assertNull("java_call must be gone", pl.getBuiltInRegistry().getBuiltIn("java_call"));
        assertNull("write_atom_to_file must be gone", pl.getBuiltInRegistry().getBuiltIn("write_atom_to_file"));
        assertNull("tcp_connect must be gone", pl.getBuiltInRegistry().getBuiltIn("tcp_connect"));
    }

    @Test public void testISS0338_safeModeShellDoesNotExecute() {
        Prolog pl = p();
        pl.enableSafeMode();
        // shell/1 is no longer a built-in -> the goal cannot run a process (it errors/fails, caught here)
        List<Map<String, Term>> r = solve(pl, "catch(shell('echo pwned'), _, fail).");
        assertTrue("shell must not execute under safe mode", r.isEmpty());
    }

    @Test public void testISS0338_safeModeKeepsCoreBuiltins() {
        Prolog pl = p();
        pl.enableSafeMode();
        // pure logic/arithmetic must still work
        assertFalse("append/3 must still work in safe mode", solve(pl, "append([1], [2], L).").isEmpty());
        assertFalse("is/2 must still work in safe mode", solve(pl, "X is 2 + 3.").isEmpty());
    }

    // ---- ISS-2025-0339: per-query inference budget bounds CPU on runaway queries ----
    @Test(timeout = 10000) public void testISS0339_inferenceBudgetAbortsRunaway() {
        Prolog pl = p();
        pl.consult("loop :- loop.");
        pl.setInferenceBudget(100000);   // generous, but finite
        try {
            pl.solve("loop.");
            fail("a non-terminating query must hit the inference budget");
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException ok) { /* expected */ }
    }

    @Test(timeout = 10000) public void testISS0339_budgetIsNotCatchableByProgram() {
        Prolog pl = p();
        pl.consult("loop :- loop.");
        pl.setInferenceBudget(100000);
        try {
            pl.solve("catch(loop, _, true).");   // untrusted catch/3 must NOT swallow the budget
            fail("the inference budget must not be catchable by the running program");
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException ok) { /* expected */ }
    }

    @Test public void testISS0339_budgetDoesNotBreakNormalQueries() {
        Prolog pl = p();
        pl.setInferenceBudget(1000000);
        assertFalse("a small query must complete well under budget", solve(pl, "append([1,2,3], [4,5], L).").isEmpty());
    }

    // ---- ISS-2025-0340: first-argument indexing must keep clause selection correct ----
    @Test public void testISS0340_firstArgIndexingSelectsCorrectClause() {
        Prolog pl = p();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 500; i++) sb.append("color(c").append(i).append(", v").append(i).append(").\n");
        pl.consult(sb.toString());
        List<Map<String, Term>> r = solve(pl, "color(c300, V).");
        assertEquals("indexed lookup must return exactly the matching clause", 1, r.size());
        assertEquals("v300", r.get(0).get("V").toString());
        // an unbound first arg must still enumerate every clause
        assertEquals("unbound first arg must scan all clauses", 500, solve(pl, "color(_, V).").size());
    }

    // ---- ISS-2025-0341: a deep TERM must not crash the embedder with a raw StackOverflowError ----
    @Test(timeout = 20000) public void testISS0341_deepTermDoesNotCrash() {
        Prolog pl = p();
        pl.consult("wrap(0, X, X).\nwrap(N, X, W) :- N > 0, N1 is N - 1, wrap(N1, f(X), W).");
        try {
            pl.solve("wrap(200000, a, T).");   // builds a 200k-deep term -> SO while resolving it
            // succeeding (no SO at this depth/JVM) is also acceptable
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException converted) {
            // converted to a catchable ISO error -> good
        } catch (StackOverflowError raw) {
            fail("a deep term must not escape as a raw StackOverflowError");
        }
    }

    @Test(timeout = 15000) public void testISS0341_deeplyNestedInputDoesNotCrashParser() {
        Prolog pl = p();
        int depth = 100000;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < depth; i++) sb.append("f(");
        sb.append("a");
        for (int i = 0; i < depth; i++) sb.append(")");
        try {
            pl.solve(sb.append(".").toString());
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException ok) {
            // controlled ISO error -> good
        } catch (StackOverflowError raw) {
            fail("deeply nested untrusted input must not crash the parser with a raw StackOverflowError");
        }
    }
}
