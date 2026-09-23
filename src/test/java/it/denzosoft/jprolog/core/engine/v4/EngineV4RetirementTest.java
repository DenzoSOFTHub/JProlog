package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.EngineContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0484 - wave W9 acceptance.
/**
 * Wave W9 (retirement) acceptance: the recursive engine is gone, the built-in context is an
 * interface, the last name-keyed hop in the v4 engine is gone, and the built-ins wave W9 migrated
 * behave exactly as their registry versions did.
 *
 * <p>(ISS-2025-0491, 4.1 wave A: these assertions were written to hold on the v4 engine AND on
 * the v2 fallback, which was still selectable in 4.0.0. The fallback is deleted; every test here
 * now runs on the one engine.)
 *
 * <p>Covers ISS-2025-0484 (delete the recursive engine), ISS-2025-0485 (reduce
 * {@code BuiltInWithContext} to the adapter), ISS-2025-0486 (the migrated built-ins and the
 * cell-based CLP(FD) hook), ISS-2025-0487 (the main thread's message queue) and ISS-2025-0488
 * (LIM-039: serialised table production).
 */
public class EngineV4RetirementTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    @After
    public void tearDown() {
        prolog = null;
    }

    private void gone(String className) {
        try {
            Class.forName(className);
            fail(className + " must be deleted by wave W9 (ISS-2025-0484)");
        } catch (ClassNotFoundException expected) {
            // the only correct outcome
        }
    }

    // ================================================================ ISS-2025-0484

    /** The recursive engine and everything that existed only to serve it. */
    @Test
    public void testISS0484_TheRecursiveEngineIsDeleted() {
        gone("it.denzosoft.jprolog.core.engine.QuerySolver");
        gone("it.denzosoft.jprolog.core.engine.CutStatus");
        gone("it.denzosoft.jprolog.core.engine.MutableCutStatus");
        gone("it.denzosoft.jprolog.core.engine.LayeredMap");
        gone("it.denzosoft.jprolog.core.engine.CollectionBuiltInAdapter");
    }

    /** The control constructs the recursive engine was the only dispatcher of. */
    @Test
    public void testISS0485_TheControlConstructBuiltInsAreDeleted() {
        gone("it.denzosoft.jprolog.builtin.control.Conjunction");
        gone("it.denzosoft.jprolog.builtin.control.IfThen");
        gone("it.denzosoft.jprolog.builtin.control.IfThenElse");
        gone("it.denzosoft.jprolog.builtin.control.NegationAsFailure");
        gone("it.denzosoft.jprolog.builtin.exception.Catch");
        gone("it.denzosoft.jprolog.builtin.meta.Call");
        gone("it.denzosoft.jprolog.builtin.meta.Caret");
    }

    /** No {@code QuerySolver}-shaped type may appear in the built-in SPI any more. */
    @Test
    public void testISS0484_BuiltInWithContextIsTypedAgainstTheInterface() throws Exception {
        Method m = BuiltInWithContext.class.getMethod(
            "executeWithContext", SolverContext.class, Term.class, Map.class, List.class);
        assertNotNull(m);
        assertEquals("the context must be the SolverContext interface",
                     SolverContext.class, m.getParameterTypes()[0]);
        assertTrue("EngineContext is a SolverContext",
                   SolverContext.class.isAssignableFrom(EngineContext.class));
        assertTrue("SolverFacade is a SolverContext",
                   SolverContext.class.isAssignableFrom(SolverFacade.class));
    }

    /** {@code solveLegacy} and the old solver accessor are gone from the embedding API. */
    @Test
    public void testISS0484_ProlgHasNoLegacyEntryPoints() {
        for (String name : new String[]{"solveLegacy", "getQuerySolver"}) {
            for (Method m : Prolog.class.getMethods()) {
                if (m.getName().equals(name)) fail("Prolog." + name + " must be deleted (W9)");
            }
        }
    }

    // ISS-2025-0491 (4.1 wave A): `testISS0484_OnlyV2SelectsAFallbackEngine` is DELETED with the
    // fallback it described. See EngineV41RetirementTest for the one-engine assertions.

    /** The durable context is where the IDE installs its debug controller. */
    @Test
    public void testISS0484_EngineContextIsTheDebugControllerHome() {
        EngineContext ctx = prolog.getEngineContext();
        assertNotNull("every Prolog owns an EngineContext", ctx);
        assertNull(ctx.getDebugController());
        DebugController dc = new DebugController();
        ctx.setDebugController(dc);
        assertEquals(dc, ctx.getDebugController());
        ctx.setDebugController(null);
        assertNull(ctx.getDebugController());
        // and the engine still solves with the context installed
        assertFalse(prolog.solve("X = 1, X == 1.").isEmpty());
    }

    /**
     * Deleting the Java implementations must NOT deregister the control constructs: ISO protection
     * (permission_error on assert/retract/clause) is what the registry entry buys, and both
     * machines implement the constructs natively.
     */
    @Test
    public void testISS0485_ControlConstructsStayIsoProtected() {
        assertEquals("assertz on call/1 must raise permission_error", 1, prolog.solve(
            "catch(assertz(call(x)), error(permission_error(modify, static_procedure, call/1), _), true).").size());
        assertEquals("assertz on ;/2 must raise permission_error", 1, prolog.solve(
            "catch(assertz(';'(a, b)), error(permission_error(modify, static_procedure, (;)/2), _), true).").size());
        assertEquals("clause/2 on catch/3 must raise permission_error", 1, prolog.solve(
            "catch(clause(catch(_,_,_), _B), error(permission_error(access, private_procedure, catch/3), _), true).").size());
        // ... and they still WORK
        assertFalse(prolog.solve("( true -> X = a ; X = b ), X == a.").isEmpty());
        assertFalse(prolog.solve("\\+ fail.").isEmpty());
        assertFalse(prolog.solve("call(atom, foo).").isEmpty());
        assertFalse(prolog.solve("catch(throw(e), e, true).").isEmpty());
        assertFalse(prolog.solve("Y^member(X, [1]).").isEmpty());
    }

    // ================================================================ ISS-2025-0486

    /**
     * The tabling regression the 4.0.0 port of the v2 driver exposed: with the production running
     * on a nested machine, a GROUND tabled variant whose body recurses through an OPEN one used to
     * bind the goal's own variable, because a fresh machine restarts its clause-renaming counter
     * at {@code _R1_}. Producing against the normalised pattern fixes it (invariant 51). Kept as a
     * regression test of v4's own linear tabling.
     */
    @Test(timeout = 30000)
    public void testISS0484_GroundTabledVariantThroughAnOpenOne() {
        prolog.consult(":- table tw9path/2.\n"
            + "tw9edge(a, b). tw9edge(b, c). tw9edge(c, d).\n"
            + "tw9path(X, Y) :- tw9edge(X, Y).\n"
            + "tw9path(X, Y) :- tw9path(X, Z), tw9edge(Z, Y).\n");
        assertEquals("the ground variant must succeed", 1, prolog.solve("tw9path(a, d).").size());
        assertEquals(1, prolog.solve("findall(Y, tw9path(a, Y), L), msort(L, S), S == [b, c, d].").size());
    }

    /** CLP(FD) posts through attributed cells: the name-to-cell hop is gone from the bridge. */
    @Test
    public void testISS0486_ClpfdHasNoNameKeyedHop() throws Exception {
        Class<?> bridge = Class.forName("it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge");
        for (Method m : bridge.getMethods()) {
            if ("cellFor".equals(m.getName()) || "onBindByName".equals(m.getName())) {
                fail("ClpfdV2Bridge." + m.getName() + " must be deleted by wave W9 (ISS-2025-0486)");
            }
        }
        assertNotNull("the cell-based hook replaces it",
                      bridge.getMethod("onBindCell",
                          it.denzosoft.jprolog.core.terms.Variable.class, Term.class));
    }

    /** Propagation still binds a functionally determined variable the goal never mentions. */
    @Test
    public void testISS0486_PropagationStillBindsDeterminedVariables() {
        List<Map<String, Term>> s = prolog.solve("C in 1..3, D #= C*2+1, C #= 1.");
        assertEquals(1, s.size());
        assertEquals("1", String.valueOf(s.get(0).get("C")));
        assertEquals("propagation must bind D even though the goal never mentions it",
                     "3", String.valueOf(s.get(0).get("D")));
        // labeling, all_different and the domain predicates keep working
        assertFalse(prolog.solve("X in 1..3, Y in 1..3, all_different([X, Y]), X #< Y, label([X, Y]).").isEmpty());
        assertTrue("an inconsistent posting fails", prolog.solve("X in 1..2, X #> 5.").isEmpty());
    }

    /** {@code current_op/3} enumerates the whole visible table, not only its first entry. */
    @Test
    public void testISS0486_CurrentOpEnumeratesEveryOperator() {
        assertTrue("many standard operators are visible",
                   prolog.solve("current_op(_, _, _).").size() > 20);
        assertFalse(prolog.solve("current_op(500, yfx, +).").isEmpty());
        assertFalse(prolog.solve("current_op(P, xfx, is), P == 700.").isEmpty());
        prolog.solve("op(699, xfx, w9op).");
        assertFalse("an op/3 declaration is visible immediately",
                    prolog.solve("current_op(699, xfx, w9op).").isEmpty());
    }

    /** {@code sort/4} keeps its key/order semantics and its ISO error terms (ISS-2025-0418). */
    @Test
    public void testISS0486_Sort4KeepsItsSemantics() {
        assertFalse(prolog.solve("sort(0, @<, [c, a, b, a], L), L == [a, b, c].").isEmpty());
        assertFalse(prolog.solve("sort(0, @=<, [c, a, b, a], L), L == [a, a, b, c].").isEmpty());
        assertFalse(prolog.solve("sort(0, @>, [c, a, b, a], L), L == [c, b, a].").isEmpty());
        assertFalse(prolog.solve("sort(1, @<, [f(2), f(1)], L), L == [f(1), f(2)].").isEmpty());
        assertEquals(1, prolog.solve(
            "catch(sort(a, @<, [1], _), error(type_error(integer, a), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(sort(0, foo, [1], _), error(domain_error(order, foo), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(sort(2, @<, [f(a)], _), error(domain_error(argument_index, 2), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(sort(1, @<, [a], _), error(type_error(compound, a), _), true).").size());
    }

    /** {@code predsort/3} keeps the SWI failure modes of ISS-2025-0419. */
    @Test
    public void testISS0486_PredsortKeepsItsSemantics() {
        assertFalse(prolog.solve("predsort(compare, [b, a, c, a], L), L == [a, b, c].").isEmpty());
        assertEquals("non-ground lists are legal", 1, prolog.solve("predsort(compare, [X, Y], _L).").size());
        prolog.consult("pw9fail(_, _, _) :- fail.\npw9bad(foo, _, _).\n");
        assertTrue(prolog.solve("predsort(pw9fail, [b, a], _L).").isEmpty());
        assertTrue("an Order outside <,=,> makes predsort FAIL, not error",
                   prolog.solve("predsort(pw9bad, [b, a], _L).").isEmpty());
        assertEquals(1, prolog.solve(
            "catch(predsort(_P, [a, b], _L), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve(
            "catch((predsort(7, [a, b], _L), fail), error(type_error(callable, 7), _), true).").size());
    }

    /** {@code max_list/2}, {@code min_list/2}, {@code nb_getval/2}, {@code b_getval/2}. */
    @Test
    public void testISS0486_MigratedDeterministicBuiltins() {
        assertFalse(prolog.solve("max_list([3, 1, 4, 1, 5], M), M == 5.").isEmpty());
        assertFalse(prolog.solve("min_list([3, 1, 4, 1, 5], M), M == 1.").isEmpty());
        // ISS-2025-0603 (P4.10): SWI evaluates the elements, so a non-numeric one RAISES
        assertEquals(1, prolog.solve("catch(max_list([1, a], _), error(type_error(evaluable, a/0), _), true).").size());
        assertTrue("an empty list fails", prolog.solve("max_list([], _).").isEmpty());
        assertFalse(prolog.solve("nb_setval(w9k, f(1)), nb_getval(w9k, V), V == f(1).").isEmpty());
        assertFalse(prolog.solve("b_setval(w9b, 42), b_getval(w9b, V), V == 42.").isEmpty());
        assertEquals("an unknown global raises existence_error", 1, prolog.solve(
            "catch(nb_getval(w9missing, _), error(existence_error(variable, w9missing), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(nb_getval(_, _), error(instantiation_error, _), true).").size());
    }

    // ================================================================ ISS-2025-0487

    /** The main thread owns a message queue, reachable as {@code main} (SWI). */
    @Test(timeout = 30000)
    public void testISS0487_MainThreadOwnsAMessageQueue() {
        // ISS-2025-0633: selective receives — `main`'s queue is shared by every non-worker thread
        assertFalse("thread_send_message(main, T) then thread_get_message/1 on the main thread",
            prolog.solve("thread_send_message(main, w9msg(1)), thread_get_message(w9msg(1)).").isEmpty());
        assertFalse("a worker can post to the main thread's queue",
            prolog.solve("thread_create(thread_send_message(main, w9from(worker)), T), "
                       + "thread_join(T, true), thread_get_message(w9from(worker)).").isEmpty());
        // ISS-2025-0495 (4.1 wave A): thread_self/1 now reports the ALIAS of an aliased thread,
        // so the top-level thread answers `main` instead of its integer id. It is still a usable
        // thread_send_message/2 target, which is what this assertion is really about.
        assertFalse("thread_self/1 reports a usable id",
            prolog.solve("thread_self(S), S == main, thread_send_message(S, w9self), "
                       + "thread_get_message(w9self).").isEmpty());
    }

    // ================================================================ ISS-2025-0490

    /**
     * An answer is rendered by the CLI and the IDE <b>after</b> {@code Prolog.solve} returns, i.e.
     * after the engine's {@code EngineState} has been uninstalled from the thread. {@code Writer}'s
     * fallback to the thread-current operator table therefore picked up a DEFAULT table, so a
     * user-declared operator printed canonically in the answer (`Y = is_bigger(a,b)`) even though
     * `write/1` inside the same query printed it correctly. The renderer takes the engine's table
     * explicitly now.
     */
    @Test
    public void testISS0490_AnswersRenderWithTheEnginesOperatorTable() {
        prolog.solve("op(200, xfy, w9likes).");
        List<Map<String, Term>> s = prolog.solve("X = (john w9likes (mary w9likes wine)).");
        assertEquals(1, s.size());
        List<String> lines = Answer.lines(s.get(0), prolog.residualGoals(s.get(0)),
                                          prolog.getOps().table());
        assertEquals(1, lines.size());
        assertEquals("X = john w9likes mary w9likes wine", lines.get(0));
        // ... and without the table it falls back to the thread-current one, i.e. canonical here
        assertEquals("X = w9likes(john,w9likes(mary,wine))",
                     Answer.lines(s.get(0), null, null).get(0));
        // a 700-priority operator must be parenthesised inside the `=` of the answer
        prolog.solve("op(700, xfx, w9isb).");
        List<Map<String, Term>> t = prolog.solve("Y =.. [w9isb, a, b].");
        assertEquals("Y = (a w9isb b)",
                     Answer.lines(t.get(0), null, prolog.getOps().table()).get(0));
    }

    // ================================================================ ISS-2025-0489

    /**
     * `stream_property/2` with an unbound first argument walks EVERY open stream, including
     * `user_input`. Computing `end_of_stream` for it used to peek — a blocking read on an
     * interactive stdin — so the documented `stream_property(S, alias(user_error))` hung forever,
     * and neither the inference budget nor a Stop interrupt could break it (the wait is inside a
     * bridged built-in). A non-repositionable input stream is reported as `not` instead.
     */
    @Test(timeout = 30000)
    public void testISS0489_StreamPropertyNeverBlocksOnStandardInput() {
        assertFalse("stream_property/2 must enumerate without blocking on stdin",
                    prolog.solve("stream_property(S, alias(user_error)).").isEmpty());
        assertFalse(prolog.solve("stream_property(S, alias(user_input)).").isEmpty());
        // member/2, not memberchk/2: memberchk/2 exists only on v4 and this class runs on both legs
        assertFalse("the full property set is still reported", prolog.solve(
            "findall(P, stream_property(user_input, P), L), member(end_of_stream(not), L), "
          + "member(mode(read), L), member(type(text), L).").isEmpty());
        assertFalse("a bound stream argument still works", prolog.solve(
            "stream_property(user_error, alias(A)), A == user_error.").isEmpty());
    }

    // ================================================================ ISS-2025-0488

    /**
     * LIM-039: several WORKER threads of one query producing tables on one engine must not corrupt
     * the shared store. Production is serialised (ISS-2025-0488), so whichever worker gets there
     * first produces the table and the others read it complete — every worker sees all 4 answers.
     *
     * <p>This is the supported concurrency model: workers of one top-level query
     * ({@code concurrent_maplist/3}, {@code thread_create/2,3}), not several top-level
     * {@code solve} calls on one {@code Prolog} at once — a top-level query owns the engine-wide
     * query boundary (it abandons every EVALUATING table when it ends), so two of them racing is
     * outside the contract on both engines.
     */
    @Test(timeout = 120000)
    public void testISS0488_ConcurrentTableProductionIsSafe() {
        prolog.consult(":- table w9t/2.\n"
            + "w9e(1, 2). w9e(2, 3). w9e(3, 4). w9e(4, 5).\n"
            + "w9t(X, Y) :- w9e(X, Y).\n"
            + "w9t(X, Y) :- w9t(X, Z), w9e(Z, Y).\n"
            + "w9count(_, N) :- findall(Y, w9t(1, Y), L), length(L, N).\n");
        List<Map<String, Term>> s = prolog.solve(
            "concurrent_maplist(w9count, [a, b, c, d], Ns), Ns == [4, 4, 4, 4].");
        assertEquals("every worker must see the complete table", 1, s.size());
    }
}
// END_CHANGE: ISS-2025-0484
