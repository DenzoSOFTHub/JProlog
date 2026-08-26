package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Modifier;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0500 - 4.2 wave C: op/3 and char_conversion/2 native over the engine's Ops.
/**
 * {@code op/3}, {@code char_conversion/2} and {@code current_char_conversion/2} as v4 natives over
 * the calling engine's {@link Ops} store, and the retirement of the public {@code Undo} API.
 *
 * <p>What this pins:
 * <ol>
 *   <li>the three indicators are in the {@link BuiltinTable}, so the registry versions are never
 *       dispatched again;</li>
 *   <li>an {@code op/3} in a branch that fails is undone — the definition is on the machine's own
 *       trail;</li>
 *   <li>two {@code Prolog} instances share neither operators nor character conversions (the
 *       conversion table was a {@code static} map until this wave);</li>
 *   <li>an operator declared by a consulted {@code :- op/3} directive is used by the parser for the
 *       rest of the file AND reported by {@code current_op/3}, and a module-local declaration stays
 *       local exactly as W7 left it;</li>
 *   <li>a CLP(FD) domain narrowing is still undone on backtracking, now that the bridge records its
 *       undo actions through a sink the engine installs instead of the public {@code Undo.record};</li>
 *   <li>{@code core.engine.v4.Undo} is package-private.</li>
 * </ol>
 */
public class EngineV4OpsTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private static String one(List<Map<String, Term>> sols, String var) {
        assertFalse("expected a solution", sols.isEmpty());
        Term t = sols.get(0).get(var);
        assertNotNull("no binding for " + var, t);
        return t.toString();
    }

    // ------------------------------------------------------------------ 1. native

    @Test
    public void testOpAndCharConversionAreNative() {
        BuiltinTable t = prolog.getV4Engine().natives();
        assertTrue("op/3 must be native", t.isNative("op", 3));
        assertTrue("char_conversion/2 must be native", t.isNative("char_conversion", 2));
        assertTrue("current_char_conversion/2 must be native", t.isNative("current_char_conversion", 2));
        assertTrue("current_op/3 must stay native", t.isNative("current_op", 3));
    }

    /** The undo doorway is an implementation detail of the machine again. */
    @Test
    public void testUndoIsNotPublicApi() {
        assertFalse("core.engine.v4.Undo must not be public",
            Modifier.isPublic(Undo.class.getModifiers()));
        try {
            java.lang.reflect.Method m = Undo.class.getDeclaredMethod("record", Runnable.class);
            assertFalse("Undo.record must not be public", Modifier.isPublic(m.getModifiers()));
        } catch (NoSuchMethodException e) {
            fail("Undo.record(Runnable) should still exist, package-private");
        }
    }

    // ------------------------------------------------------------------ 2. backtracking

    @Test
    public void testOpInAFailedBranchIsUndone() {
        assertFalse(prolog.solve("(op(700, xfx, zzop), fail ; true).").isEmpty());
        assertTrue("op/3 under a choice point must be undone on backtracking",
            prolog.solve("current_op(_, xfx, zzop).").isEmpty());
        // and a successful op/3 stays
        prolog.solve("op(700, xfx, zzop).");
        assertEquals("700", one(prolog.solve("current_op(P, xfx, zzop)."), "P"));
    }

    @Test
    public void testOpRemovalIsUndoneToo() {
        prolog.solve("op(700, xfx, keepme).");
        assertFalse(prolog.solve("(op(0, xfx, keepme), fail ; true).").isEmpty());
        assertEquals("700", one(prolog.solve("current_op(P, xfx, keepme)."), "P"));
    }

    @Test
    public void testCharConversionInAFailedBranchIsUndone() {
        assertFalse(prolog.solve("(char_conversion(a, b), fail ; true).").isEmpty());
        assertEquals("a", one(prolog.solve("current_char_conversion(a, X)."), "X"));
        // a committed one survives
        prolog.solve("char_conversion(a, b).");
        assertEquals("b", one(prolog.solve("current_char_conversion(a, X)."), "X"));
        // and From == To removes it
        prolog.solve("char_conversion(a, a).");
        assertEquals("a", one(prolog.solve("current_char_conversion(a, X)."), "X"));
    }

    // ------------------------------------------------------------------ 3. two engines

    @Test
    public void testTwoEnginesDoNotShareOperators() {
        Prolog other = new Prolog();
        prolog.solve("op(777, xfx, mine).");
        assertEquals("777", one(prolog.solve("current_op(P, xfx, mine)."), "P"));
        assertTrue("an operator must not leak into another engine",
            other.solve("current_op(_, xfx, mine).").isEmpty());
        // and the other engine's parser must not know it either: `a mine b` does not parse there
        try {
            other.solve("X = (a mine b).");
            fail("the other engine's parser must not know the operator");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException expected) {
            assertTrue(String.valueOf(expected.getMessage()).contains("mine"));
        }
        // while this engine's parser does
        assertEquals("mine(a, b)", one(prolog.solve("X = (a mine b)."), "X"));
    }

    @Test
    public void testTwoEnginesDoNotShareCharConversions() {
        Prolog other = new Prolog();
        prolog.solve("char_conversion(z, y).");
        assertEquals("y", one(prolog.solve("current_char_conversion(z, X)."), "X"));
        assertEquals("a character conversion must not leak into another engine",
            "z", one(other.solve("current_char_conversion(z, X)."), "X"));
    }

    // ------------------------------------------------------------------ 4. consulted operators

    @Test
    public void testConsultedOperatorIsUsedByTheParserAndReported() {
        prolog.consult(":- op(700, xfx, ===>).\nrule(a ===> b).\nrule(c ===> d).\n");
        List<Map<String, Term>> sols = prolog.solve("rule(X), X =.. L.");
        assertEquals(2, sols.size());
        assertEquals("[===>, a, b]", sols.get(0).get("L").toString());
        assertEquals("700", one(prolog.solve("current_op(P, T, ===>)."), "P"));
        assertEquals("xfx", one(prolog.solve("current_op(P, T, ===>)."), "T"));
    }

    /** W7's module scoping is unchanged: a module-local declaration is invisible elsewhere. */
    @Test
    public void testModuleLocalOperatorStaysLocalToItsModule() {
        prolog.consult(":- module(m1, []).\n:- op(699, xfx, localop).\n");
        Ops ops = prolog.getOps();
        boolean seenInM1 = false;
        for (Ops.Def d : ops.visibleIn("m1")) if ("localop".equals(d.name)) seenInM1 = true;
        assertTrue("m1 must see its own operator", seenInM1);
        for (Ops.Def d : ops.visibleIn("user")) {
            assertFalse("user must not see m1's operator", "localop".equals(d.name));
        }
    }

    /** The list form and precedence 0 (removal), both on the engine's own store. */
    @Test
    public void testOpAcceptsAListOfNamesAndPrecedenceZeroRemoves() {
        assertFalse(prolog.solve("op(700, xfx, [aa1, bb1]).").isEmpty());
        assertEquals("700", one(prolog.solve("current_op(P, xfx, aa1)."), "P"));
        assertEquals("700", one(prolog.solve("current_op(P, xfx, bb1)."), "P"));
        prolog.solve("op(0, xfx, aa1).");
        assertTrue(prolog.solve("current_op(_, xfx, aa1).").isEmpty());
        assertEquals("700", one(prolog.solve("current_op(P, xfx, bb1)."), "P"));
    }

    /** The registry version's validation and ISO error terms, unchanged (ISS-2025-0278). */
    @Test
    public void testOpErrorTermsArePreserved() {
        List<Map<String, Term>> s = prolog.solve("catch(op(700.5, xfx, f), E, true).");
        assertEquals("error(type_error(integer, 700.5), op/3)", one(s, "E"));
        assertThrowsEvaluation("op(1300, xfx, f).");
        assertThrowsEvaluation("op(-1, xfx, f).");
        assertThrowsEvaluation("op(700, blah, f).");
        assertThrowsEvaluation("op(700, xfx, 3).");
        assertThrowsEvaluation("op(P, xfx, f).");
    }

    private void assertThrowsEvaluation(String goal) {
        try {
            prolog.solve(goal);
            fail("expected an error from " + goal);
        } catch (RuntimeException e) {
            assertTrue(e instanceof it.denzosoft.jprolog.core.exceptions.PrologException);
        }
    }

    /** op/3 is a protected procedure: the native registration is what says so (invariant 59). */
    @Test
    public void testOpIsAProtectedProcedure() {
        try {
            prolog.solve("assertz(op(1, 2, 3)).");
            fail("assertz(op/3) must be a permission_error");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            assertTrue(String.valueOf(e.getMessage()).contains("permission_error"));
        }
    }

    // ------------------------------------------------------------------ 5. CLP(FD) on the trail

    /**
     * The CLP(FD) bridge records its store rollbacks through the sink the engine installs
     * ({@code ClpfdNative}'s static initialiser) instead of the public {@code Undo.record}. A
     * narrowing posted in a branch that fails must still be rolled back.
     */
    @Test
    public void testClpfdNarrowingIsUndoneOnBacktracking() {
        List<Map<String, Term>> sols = prolog.solve(
            "X in 1..10, ( X #> 5, X #< 7, fail ; true ), fd_dom(X, D).");
        assertFalse(sols.isEmpty());
        // fd_dom/2 answers the term '..'(1, 10); Term.toString is not the ISO writer
        assertEquals("the narrowing of the failed branch must be undone", "..(1, 10)",
            sols.get(0).get("D").toString());
    }

    @Test
    public void testClpfdLabelingStillEnumeratesAfterABacktrackedPost() {
        List<Map<String, Term>> sols = prolog.solve(
            "X in 1..3, ( X #= 2, fail ; true ), label([X]).");
        assertEquals(3, sols.size());
        assertEquals("1", sols.get(0).get("X").toString());
        assertEquals("3", sols.get(2).get("X").toString());
    }

    @Test
    public void testClpfdConstraintSurvivesWhenTheBranchSucceeds() {
        List<Map<String, Term>> sols = prolog.solve("X in 1..10, X #> 7, label([X]).");
        assertEquals(3, sols.size());
        assertEquals("8", sols.get(0).get("X").toString());
    }

    // ------------------------------------------------------------------ 6. .jpc round trip

    /**
     * A dynamic operator must still reach the ONE {@link it.denzosoft.jprolog.core.operator.OperatorTable}
     * the parser, {@code write_term/2,3} and the {@code .jpc} writer share — that shared object is
     * what makes dynamic operators round-trip through the compiled format — and a file declaring
     * one must still consult (and re-consult through {@code consultSmart}) to the same answers.
     *
     * <p>Note: {@code Prolog.compileFile} itself parses with the LEGACY parser, which does not read
     * the store, so pre-compiling a file whose own {@code :- op/3} directive it needs fails on
     * 4.2.0 exactly as it does here. That is a pre-existing limitation of the legacy parser path,
     * not of the operator store, and this wave does not change it.
     */
    @Test
    public void testDynamicOperatorReachesTheSharedTableAndReConsults() throws Exception {
        prolog.solve("op(701, xfx, ===>).");
        it.denzosoft.jprolog.core.operator.OperatorTable t = prolog.getOperatorTable();
        assertNotNull("op/3 must write into the table the parser and the .jpc writer share",
            t.getInfixOperator("===>"));
        assertEquals(701, t.getInfixOperator("===>").getPrecedence());
        assertEquals("the store and the table must agree", 701, prolog.getOps().infix("===>").precedence);

        java.io.File dir = java.nio.file.Files.createTempDirectory("jpcops").toFile();
        java.io.File pl = new java.io.File(dir, "ops.pl");
        java.io.PrintWriter w = new java.io.PrintWriter(pl, "UTF-8");
        w.println(":- op(700, xfx, ===>).");
        w.println("rule(a ===> b).");
        w.close();
        try {
            for (int i = 0; i < 2; i++) {
                Prolog loaded = new Prolog();
                loaded.consultSmart(pl.getAbsolutePath());
                List<Map<String, Term>> sols = loaded.solve("rule(X), X =.. L.");
                assertEquals(1, sols.size());
                assertEquals("[===>, a, b]", sols.get(0).get("L").toString());
                assertEquals("700", one(loaded.solve("current_op(P, xfx, ===>)."), "P"));
            }
        } finally {
            java.io.File[] fs = dir.listFiles();
            if (fs != null) for (java.io.File f : fs) f.delete();
            dir.delete();
        }
    }
}
// END_CHANGE: ISS-2025-0500
