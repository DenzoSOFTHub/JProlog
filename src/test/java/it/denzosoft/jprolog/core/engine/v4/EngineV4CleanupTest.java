package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0513 - 4.3 wave D follow-up: a cleanup's own exception must reach catch/3.
/**
 * The {@code setup_call_cleanup/3} / {@code call_cleanup/2} exception contract.
 *
 * <p>A cleanup goal collected while a ball was unwinding used to run AFTER
 * {@code Machine.handleBall} had found, popped and consumed the matching CATCH frame, so its own
 * exception had nothing left to unwind into; and {@code handleBall} is called from inside
 * {@code drive}'s {@code catch} clause, i.e. outside the loop that routes exceptions, so it
 * escaped the machine altogether. {@code catch(call_cleanup(throw(a), throw(b)), E, true)} reached
 * the Java embedder as an uncaught {@code PrologException}. It reproduces identically on 4.3.0, so
 * it is pre-existing, and it is <b>not</b> what ISS-2025-0509 fixed (that was the
 * argument-validation shape only).
 *
 * <p><b>The semantics chosen</b>, matching SWI-Prolog and JProlog's own already-correct
 * {@code setup_call_cleanup(true, true, throw(b))}: <b>the cleanup's ball wins</b>. It replaces the
 * one being unwound and is therefore matched against the catchers that ENCLOSE the
 * {@code setup_call_cleanup/3}, not against the one that matched the goal's ball. Nested cleanups
 * all run and the outermost ball survives. A cleanup reached by an unwinding ball also runs before
 * the trail is undone to the catch frame's mark, so it sees the bindings the goal made.
 *
 * <p>Four of the methods below fail on the v4.3.0 classes; the rest pin the neighbouring shapes
 * (deterministic exit, failure, cut, an outer exception, {@code findall/3}, nesting) that were
 * already correct, so that a future change to the unwinding protocol cannot break them silently.
 */
public class EngineV4CleanupTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private boolean succeeds(String query) {
        return !prolog.solve(query + ".").isEmpty();
    }

    private Term first(String query, String var) {
        List<Map<String, Term>> sols = prolog.solve(query + ".");
        assertTrue("query failed: " + query, !sols.isEmpty());
        Term t = sols.get(0).get(var);
        assertTrue("no binding for " + var + " in " + query, t != null);
        return t;
    }

    /** The reported defect: Goal AND Cleanup both throw; the CLEANUP's ball reaches the catcher. */
    @Test
    public void testISS0513_CleanupExceptionIsCaughtWhenGoalAlsoThrew() {
        assertEquals("b", first("catch(call_cleanup(throw(a), throw(b)), E, true)", "E").toString());
        assertEquals("b", first("catch(setup_call_cleanup(true, throw(a), throw(b)), E, true)", "E").toString());
    }

    /**
     * SWI's choice, and ours: the cleanup's ball REPLACES the goal's. It is therefore tested
     * against the catchers that enclose the {@code setup_call_cleanup/3}, not against the one that
     * matched the goal's ball — an inner catcher for the goal's ball must NOT swallow it.
     */
    @Test
    public void testISS0513_TheCleanupsBallReplacesTheGoalsAndIsRematched() {
        // the catcher that matches the CLEANUP's ball wins
        assertTrue(succeeds("catch(call_cleanup(throw(a), throw(b)), b, true)"));
        // a catcher that only matches the GOAL's ball does not, and the outer one sees b
        assertEquals("b",
            first("catch(catch(call_cleanup(throw(a), throw(b)), a, r1), E2, true)", "E2").toString());
    }

    /** Nested setup_call_cleanup/3: every cleanup runs, and the OUTERMOST ball survives. */
    @Test
    public void testISS0513_NestedCleanupsAllRunAndTheOutermostBallSurvives() {
        assertEquals("c", first(
            "catch(setup_call_cleanup(true, setup_call_cleanup(true, throw(a), throw(b)), throw(c)), E, true)",
            "E").toString());
    }

    /** With no catch/3 at all the embedder must see the CLEANUP's ball, not the goal's. */
    @Test
    public void testISS0513_UncaughtReportsTheCleanupsBall() {
        try {
            prolog.solve("setup_call_cleanup(true, throw(a), throw(b)).");
            fail("expected the cleanup's exception to reach the embedder");
        } catch (PrologException e) {
            assertEquals("b", e.getErrorTerm().toString());
        }
    }

    /** The neighbouring shapes: deterministic exit, failure, cut, an outer exception, findall/3. */
    @Test
    public void testISS0513_EveryOtherCleanupPathIsCatchableToo() {
        assertEquals("b", first("catch(setup_call_cleanup(true, true, throw(b)), E, true)", "E").toString());
        assertEquals("b", first("catch(setup_call_cleanup(true, fail, throw(b)), E, true)", "E").toString());
        assertEquals("b", first(
            "catch(setup_call_cleanup(true, (member(_,[1,2]), !), throw(b)), E, true)", "E").toString());
        assertEquals("b", first(
            "catch((setup_call_cleanup(true, member(_,[1,2]), throw(b)), fail), E, true)", "E").toString());
        assertEquals("b", first(
            "catch(findall(X, setup_call_cleanup(true, member(X,[1,2]), throw(b)), _), E, true)",
            "E").toString());
    }

    /** The cleanup runs exactly ONCE, and it sees the bindings the goal made before it threw. */
    @Test
    public void testISS0513_CleanupRunsOnceAndSeesTheGoalsBindings() {
        prolog.consult(":- dynamic ran0513/0.\n");
        assertEquals("b", first(
            "catch(setup_call_cleanup(true, throw(a), (assertz(ran0513), throw(b))), E, true)",
            "E").toString());
        assertEquals("[x]", first("findall(x, ran0513, L)", "L").toString());
        assertEquals("saw(bound)", first(
            "catch(setup_call_cleanup(true, (X = bound, throw(a)), throw(saw(X))), E, true)",
            "E").toString());
    }

    /**
     * The trust model survives (invariant 9): a budget abort inside a cleanup is an
     * {@code InferenceLimitException}, not a {@code PrologException}, so {@code catch/3} cannot
     * swallow it and the Java embedder still sees it. Same for {@code halt/1}.
     */
    @Test
    public void testISS0513_ControlExceptionsInACleanupStayUncatchable() {
        Prolog p = new Prolog();
        p.consult("loop0513(0) :- !.\nloop0513(N) :- N1 is N-1, loop0513(N1).\n");
        p.setInferenceBudget(200000);
        try {
            p.solve("catch(setup_call_cleanup(true, throw(a), loop0513(10000000)), _, true).");
            fail("a budget abort inside a cleanup must not be catchable by catch/3");
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException expected) {
            // the trust model: not a PrologException, so no untrusted catch/3 can see it
        }
        try {
            new Prolog().solve("catch(setup_call_cleanup(true, throw(a), halt(3)), _, true).");
            fail("halt/1 inside a cleanup must still halt");
        } catch (PrologException e) {
            assertTrue("halt/1 carries no ball", e.isHalt());
        }
    }
}
// END_CHANGE: ISS-2025-0513
