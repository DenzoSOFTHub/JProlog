package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.DebugEvent;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0491 - 4.1 wave A acceptance.
/**
 * 4.1 wave A acceptance: the v2 engine is gone, the second undo trail is gone, the hot goal path
 * no longer pays the module test for a plain user predicate, an idle debug controller costs
 * nothing, and {@code thread_self/1} answers SWI-style.
 *
 * <p>Covers ISS-2025-0491 (delete the v2 {@code MachineSolver} and everything only it kept alive),
 * ISS-2025-0492 (the engine-owned undo trail replaces {@code core.engine.Trail}), ISS-2025-0493
 * (the hot-path dispatch memo), ISS-2025-0494 ({@code DebugController.needsPorts()}) and
 * ISS-2025-0495 ({@code thread_self/1} reports {@code main} / the alias).
 */
public class EngineV41RetirementTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private void ok(String query) {
        assertFalse("must succeed: " + query, prolog.solve(query + ".").isEmpty());
    }

    private void no(String query) {
        assertTrue("must fail: " + query, prolog.solve(query + ".").isEmpty());
    }

    // ================================================================ ISS-2025-0491

    /** The v2 engine's classes are DELETED, not merely unreachable. */
    @Test
    public void testISS0491_TheV2EngineClassesAreGone() {
        for (String cn : new String[]{
                "it.denzosoft.jprolog.core.engine.v2.MachineSolver",
                "it.denzosoft.jprolog.core.engine.Trail",
                "it.denzosoft.jprolog.builtin.control.Freeze",
                "it.denzosoft.jprolog.builtin.control.When",
                "it.denzosoft.jprolog.builtin.control.Dif",
                "it.denzosoft.jprolog.builtin.term.AttributedVariables"}) {
            try {
                Class.forName(cn);
                fail(cn + " must be deleted in 4.1");
            } catch (ClassNotFoundException expected) {
                // the point
            }
        }
    }

    /** And so is the engine-selection API: there is nothing left to select. */
    @Test
    public void testISS0491_TheEngineSelectionApiIsGone() {
        for (String name : new String[]{"setUseV2Engine", "isUsingV2Engine",
                                        "setUseV4Engine", "isUsingV4Engine"}) {
            for (Method m : Prolog.class.getMethods()) {
                if (m.getName().equals(name)) fail("Prolog." + name + " must be deleted in 4.1");
            }
        }
        // the one engine is reachable and running
        assertNotNull(prolog.getV4Engine());
        ok("X = 1, X == 1");
    }

    /** The legacy attribute-unify hook went with the engine that fired it. */
    @Test
    public void testISS0491_TheLegacyAttributeHookIsGone() {
        for (Class<?> c : it.denzosoft.jprolog.core.terms.Variable.class.getDeclaredClasses()) {
            if ("AttributeUnifyHook".equals(c.getSimpleName())) {
                fail("Variable.AttributeUnifyHook must be deleted in 4.1");
            }
        }
        // ... and coroutining still works, through the prelude and the v4 wake queue
        ok("freeze(X, Y = woken), X = 1, Y == woken");
        ok("dif(A, b), A = c");
        no("dif(A, b), A = b");
        ok("when(ground(f(P, Q)), R = fired), P = 1, Q = 2, R == fired");
        ok("put_attr(V, mymod, 7), get_attr(V, mymod, W), W == 7");
    }

    /** {@code :- table} declarations survive; the v2 driver's answer cache does not. */
    @Test(timeout = 30000)
    public void testISS0491_TableDeclarationsStillDriveTheV4Tabling() {
        prolog.consult(":- table t41path/2.\n"
            + "t41edge(a, b). t41edge(b, c).\n"
            + "t41path(X, Y) :- t41edge(X, Y).\n"
            + "t41path(X, Y) :- t41path(X, Z), t41edge(Z, Y).\n");
        assertEquals(3, prolog.solve("t41path(a, Y).").size() + prolog.solve("t41path(b, Y).").size());
        ok("t41path(a, c)");
    }

    // ================================================================ ISS-2025-0492

    /** {@code b_setval/2} is undone when the branch that set it fails. */
    @Test
    public void testISS0492_BSetvalIsUndoneOnBacktracking() {
        prolog.solve("nb_setval(u41probe, none).");
        prolog.solve("b_setval(u41, 1), (b_setval(u41, 2), fail ; true), "
                   + "b_getval(u41, V), nb_setval(u41probe, V).");
        assertEquals("1", prolog.solve("nb_getval(u41probe, V).").get(0).get("V").toString());
    }

    /** {@code op/3} in a branch that fails leaves no operator behind. */
    @Test
    public void testISS0492_OpIsUndoneOnBacktracking() {
        String uniq = "u41op_" + System.nanoTime();
        // ISS-2025-0612 (P4.18, decision §8): op/3 is permanent now (ISO/SWI); the definition
        // made in the failed branch stays. (Method name kept from 4.1.0.)
        prolog.solve("(op(700, xfx, " + uniq + "), fail ; true).");
        assertEquals(1, prolog.solve("current_op(_, _, " + uniq + ").").size());
        // and an op/3 that is NOT backtracked over stays defined
        prolog.solve("op(700, xfx, " + uniq + "2).");
        assertEquals(1, prolog.solve("current_op(P, xfx, " + uniq + "2), P == 700.").size());
    }

    /** {@code setarg/3} is undone; {@code nb_setarg/3} is not. */
    @Test
    public void testISS0492_SetargIsUndoneOnBacktracking() {
        ok("T = f(1), (setarg(1, T, 2), fail ; true), T == f(1)");
        ok("T = f(1), (nb_setarg(1, T, 2), fail ; true), T == f(2)");
    }

    /** An attribute put on a variable is undone with the branch that put it. */
    @Test
    public void testISS0492_PutAttrIsUndoneOnBacktracking() {
        ok("(put_attr(X, m41, 1), fail ; true), \\+ get_attr(X, m41, _)");
        ok("put_attr(X, m41, 1), (put_attr(X, m41, 2), fail ; true), get_attr(X, m41, V), V == 1");
    }

    /** A CLP(FD) domain narrowing is undone when the posting branch fails. */
    @Test
    public void testISS0492_ClpfdNarrowingIsUndoneOnBacktracking() {
        // X in 1..10, then a failed branch narrows it to 1..2: the narrowing must not survive.
        ok("X in 1..10, (X #=< 2, fail ; true), fd_dom(X, D), D == 1..10");
        // and a narrowing that is NOT backtracked over does survive
        ok("X in 1..10, X #=< 2, fd_dom(X, D), D == 1..2");
    }

    /** The choice point carries ONE mark now: no {@code legacyMark}, no second trail. */
    @Test
    public void testISS0492_TheChoicePointHasOnlyTheBindingsMark() {
        for (java.lang.reflect.Field f : Machine.CP.class.getDeclaredFields()) {
            assertFalse("CP.legacyMark must be gone with core.engine.Trail",
                "legacyMark".equals(f.getName()));
        }
    }

    /** Undo actions recorded with no machine running are simply not recorded (documented). */
    @Test
    public void testISS0492_RecordingWithNoMachineIsANoOp() {
        final boolean[] ran = {false};
        Undo.record(new Runnable() { @Override public void run() { ran[0] = true; } });
        assertFalse("nothing to backtrack over, nothing recorded", ran[0]);
    }

    // ================================================================ ISS-2025-0493

    /** The dispatch memo must not answer a stale question when a module is (re)defined. */
    @Test
    public void testISS0493_DispatchMemoFollowsTheModuleStamp() {
        // the library definition first, so the decision for append/3 is memoised
        ok("append([1], [2], L), L == [1,2]");
        // now a module with its OWN append/3 — the module stamp changes and the memo must miss
        prolog.consult(":- module(mm41, [go/1]).\nappend(mine).\ngo(X) :- append(X).\n");
        ok("mm41:go(X), X == mine");
        // ... and the user context still sees the library one
        ok("append([1], [2], L2), L2 == [1,2]");
    }

    /** A prelude predicate still wins over the registry entry of the same name (invariant). */
    @Test
    public void testISS0493_PreludeStillWinsOverTheRegistry() {
        List<Map<String, Term>> s = prolog.solve("findall(X, member(X, [a,b,c]), L).");
        assertEquals(1, s.size());
        assertEquals("[a, b, c]", s.get(0).get("L").toString());
        ok("maplist([X,Y]>>(Y is X * 2), [1,2,3], D), D == [2,4,6]");
    }

    /** A user predicate that shares a name with no built-in still reaches its clauses. */
    @Test
    public void testISS0493_PlainUserPredicateStillDispatches() {
        prolog.consult("u41f(1).\nu41f(2).\n");
        assertEquals(2, prolog.solve("u41f(_).").size());
    }

    // ================================================================ ISS-2025-0494

    /** A controller that can observe nothing reports {@code needsPorts() == false}. */
    @Test
    public void testISS0494_AnIdleControllerNeedsNoPorts() {
        DebugController dc = new DebugController();
        dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        assertFalse("no listener, no breakpoint, CONTINUE, no trace: nothing to report",
            dc.needsPorts());

        dc.setTraceEnabled(true);            // the IDE trace toggle alone observes nothing:
        assertFalse("traceEnabled without a listener reports to nobody", dc.needsPorts());
        dc.setTraceEnabled(false);
        assertFalse(dc.needsPorts());

        dc.addBreakpoint("foo/1");
        assertTrue("a breakpoint is an observer", dc.needsPorts());
        dc.clearBreakpoints();
        assertFalse(dc.needsPorts());

        dc.setCurrentMode(DebugEvent.Action.STEP_INTO);
        assertTrue("stepping is an observer", dc.needsPorts());
        dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        assertFalse(dc.needsPorts());

        dc.setListener(new DebugController.DebugListener() {
            @Override public void onDebugPaused(DebugEvent event) { }
            @Override public void onTraceEvent(DebugEvent event) { }
            @Override public void onDebugFinished() { }
        });
        assertTrue("a listener is an observer", dc.needsPorts());
    }

    /** A fresh controller pauses by default, so it always needs ports. */
    @Test
    public void testISS0494_AFreshControllerStillNeedsPorts() {
        assertTrue("a fresh controller is in STEP_INTO mode", new DebugController().needsPorts());
    }

    /** An idle controller attached to a running engine changes neither answers nor its own state. */
    @Test(timeout = 30000)
    public void testISS0494_AnIdleControllerDoesNotSeeThePorts() {
        DebugController dc = new DebugController();
        dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        prolog.getEngineContext().setDebugController(dc);
        try {
            prolog.consult("i41(0).\ni41(N) :- N > 0, N1 is N - 1, i41(N1).\n");
            ok("i41(200)");
            assertTrue("an idle controller must not accumulate a call stack",
                dc.getCallStack().isEmpty());
        } finally {
            prolog.getEngineContext().setDebugController(null);
        }
    }

    /** A LISTENING controller still gets every port (the optimisation must not silence it). */
    @Test(timeout = 30000)
    public void testISS0494_AListeningControllerStillGetsThePorts() {
        final int[] ports = {0};
        DebugController dc = new DebugController();
        dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        dc.setTraceEnabled(true);
        dc.setListener(new DebugController.DebugListener() {
            @Override public void onDebugPaused(DebugEvent event) { }
            @Override public void onTraceEvent(DebugEvent event) { ports[0]++; }
            @Override public void onDebugFinished() { }
        });
        prolog.getEngineContext().setDebugController(dc);
        try {
            prolog.consult("l41(a).\nl41(b).\n");
            ok("l41(b)");
            assertTrue("the four ports must still be reported: " + ports[0], ports[0] > 0);
        } finally {
            prolog.getEngineContext().setDebugController(null);
        }
    }

    // ================================================================ ISS-2025-0495

    /** The top-level thread answers {@code main}, SWI-style. */
    @Test(timeout = 30000)
    public void testISS0495_ThreadSelfReportsMainOnTheMainThread() {
        ok("thread_self(S), S == main");
        ok("thread_self(S), atom(S)");
        // and the answer is still a usable target for the thread predicates
        // ISS-2025-0633: every non-worker thread is `main` and shares ONE queue across the JVM, so
        // the receive is selective — a message another test left there cannot be taken instead.
        ok("thread_self(S), thread_send_message(S, m41), thread_get_message(m41)");
    }

    /** An aliased worker answers its alias; an anonymous one answers its integer id. */
    @Test(timeout = 30000)
    public void testISS0495_ThreadSelfReportsTheAliasOfAWorker() {
        prolog.consult(":- dynamic(seen41/1).\n");
        ok("thread_create((thread_self(T), assertz(seen41(T))), Id, [alias(w41)]), "
         + "thread_join(w41, true)");
        ok("seen41(w41)");
        ok("thread_create((thread_self(T2), assertz(seen41(T2))), Id2), thread_join(Id2, true)");
        ok("seen41(N), integer(N)");
    }

    /** {@code thread_join/2} statuses are unchanged by the identity change. */
    @Test(timeout = 30000)
    public void testISS0495_ThreadJoinStatusesAreUnchanged() {
        ok("thread_create(true, A), thread_join(A, S), S == true");
        ok("thread_create(fail, B), thread_join(B, S), S == false");
        ok("thread_create(throw(boom41), C), thread_join(C, S), S == exception(boom41)");
    }
}
// END_CHANGE: ISS-2025-0491
