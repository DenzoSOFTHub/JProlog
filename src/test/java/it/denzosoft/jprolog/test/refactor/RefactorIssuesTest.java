package it.denzosoft.jprolog.test.refactor;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Refactor verification tests — each test asserts the CORRECT behavior that
 * should hold after the corresponding refactor (R1..R10) is completed.
 *
 * All tests are currently marked {@code @Ignore} because the refactor has not
 * yet been performed. Once a refactor is done, remove the {@code @Ignore} on
 * the related tests; they should pass without modification.
 *
 * Refactor IDs reference the plan in track-issues.md / CHANGELOG.
 */
public class RefactorIssuesTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    // ===================================================================
    // R1 - TRAIL ENGINE
    // ===================================================================
    // Goal: introduce a trail stack so backtrackable mutation is possible.
    // Affects: b_setval/2, op/3 undo, setarg/3, attribute changes.

    // @Ignore enabled
    @Test
    public void testR1_bSetvalBacktrackable() {
        // After binding x=1, the alternative branch sets x=2, then fail.
        // Trail should restore x to 1.
        prolog.solve("nb_setval(probe, []).");
        prolog.solve(
            "b_setval(x, 1), (b_setval(x, 2), fail ; true), " +
            "b_getval(x, V), nb_setval(probe, V).");
        List<Map<String, Term>> r = prolog.solve("nb_getval(probe, V).");
        assertEquals("1", r.get(0).get("V").toString());
    }

    @Test
    public void testR1_opRedefinitionBacktrackable() {
        // Define operator temporarily under a choicepoint, then fail.
        // After fail, operator should be gone. Use a unique name to avoid test-suite cross-pollution.
        String uniq = "myop_r1_" + System.nanoTime();
        prolog.solve("(op(700, xfx, " + uniq + "), fail ; true).");
        List<Map<String, Term>> r = prolog.solve("current_op(_, _, " + uniq + ").");
        assertEquals(0, r.size());
    }

    // @Ignore enabled
    @Test
    public void testR1_setargDestructive() {
        prolog.solve(
            "T = f(a, b, c), setarg(2, T, x), arg(2, T, X).");
        List<Map<String, Term>> r = prolog.solve(
            "T = f(a, b, c), setarg(2, T, x), arg(2, T, X).");
        assertEquals("x", r.get(0).get("X").toString());
    }

    // ===================================================================
    // R2 - MODULE-LOCAL OPERATORS
    // ===================================================================
    // Goal: operators defined inside a module are visible only within
    // that module (and modules that import it explicitly).
    // Affects: parser, op/3 dispatch, TermFormatter.

    // @Ignore enabled
    @Test
    public void testR2_operatorLocalToModule() {
        prolog.consult(":- module(m1, []).");
        prolog.consult(":- op(800, xfx, mylocalop).");
        prolog.consult(":- module(m2, []).");
        // Inside m2, mylocalop should be undefined → parsing X = a mylocalop b
        // should fail with syntax error or unify as plain atom-sequence.
        List<Map<String, Term>> r = prolog.solve("current_op(_, _, mylocalop).");
        assertEquals("operator must not leak to m2", 0, r.size());
    }

    // re-enabled v2.9.3
    @Test
    public void testR2_emptyExportListHidesAll() {
        prolog.consult(":- module(secret, []).");
        prolog.consult("hidden(42).");
        try {
            List<Map<String, Term>> r = prolog.solve("secret:hidden(X).");
            assertEquals("hidden predicate must not be visible", 0, r.size());
        } catch (RuntimeException e) {
            // existence_error is also acceptable
        }
    }

    // ===================================================================
    // R3 - STREAM ENCODING + EOF ACTION
    // ===================================================================
    // Goal: open/4 honors options (type, encoding, eof_action, reposition).

    // @Ignore enabled
    @Test
    public void testR3_openWithEncoding() throws Exception {
        java.io.File f = java.io.File.createTempFile("utf8test", ".txt");
        f.deleteOnExit();
        java.nio.file.Files.write(f.toPath(), "café".getBytes("UTF-8"));
        String path = f.getAbsolutePath().replace("\\", "\\\\");
        prolog.solve("open('" + path + "', read, S, [encoding(utf8)]), " +
                     "get_char(S, C), close(S), assertz(probe(C)).");
        List<Map<String, Term>> r = prolog.solve("probe(C).");
        assertEquals("c", r.get(0).get("C").toString());
    }

    // @Ignore enabled
    @Test
    public void testR3_eofActionError() throws Exception {
        java.io.File f = java.io.File.createTempFile("eoftest", ".txt");
        f.deleteOnExit();
        java.nio.file.Files.write(f.toPath(), "a".getBytes());
        String path = f.getAbsolutePath().replace("\\", "\\\\");
        try {
            prolog.solve(
                "open('" + path + "', read, S, [eof_action(error)]), " +
                "get_char(S, _), get_char(S, _), get_char(S, _).");
            fail("Expected permission_error / past end-of-stream");
        } catch (RuntimeException e) {
            // expected
        }
    }

    // @Ignore enabled
    @Test
    public void testR3_binaryStream() throws Exception {
        java.io.File f = java.io.File.createTempFile("bintest", ".bin");
        f.deleteOnExit();
        java.nio.file.Files.write(f.toPath(), new byte[]{(byte) 0xFF, 0x00, 0x42});
        String path = f.getAbsolutePath().replace("\\", "\\\\");
        prolog.solve("open('" + path + "', read, S, [type(binary)]), " +
                     "get_byte(S, B), close(S), assertz(probe(B)).");
        List<Map<String, Term>> r = prolog.solve("probe(B).");
        assertEquals("255", r.get(0).get("B").toString());
    }

    // ===================================================================
    // R4 - FORMAT COLUMN TABBING + PORTRAY
    // ===================================================================
    // Goal: implement ~|, ~t, ~+ column control and ~p portray hook.

    // @Ignore enabled
    @Test
    public void testR4_columnTab() {
        java.io.PrintStream orig = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            // Output 'hi' right-aligned at column 10 with spaces as fill
            prolog.solve("format('~t~w~10|', [hi]).");
        } finally {
            System.setOut(orig);
        }
        // Expected: 8 spaces + "hi"
        assertEquals("        hi", baos.toString());
    }

    // @Ignore enabled
    @Test
    public void testR4_relativeTab() {
        java.io.PrintStream orig = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            // Two columns of width 5
            prolog.solve("format('~w~t~5|~w', [a, b]).");
        } finally {
            System.setOut(orig);
        }
        // Expected: "a    b" (a, 4 spaces, b)
        assertEquals("a    b", baos.toString());
    }

    // re-enabled v2.9.3
    @Test
    public void testR4_portrayHook() {
        java.io.PrintStream orig = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            prolog.consult("portray(point(X, Y)) :- format('<~w,~w>', [X, Y]).");
            prolog.solve("format('~p', [point(3, 4)]).");
        } finally {
            System.setOut(orig);
        }
        assertEquals("<3,4>", baos.toString());
    }

    // ===================================================================
    // R5 - TABLING WELL-FOUNDED SEMANTICS
    // ===================================================================
    // Goal: tabled predicates handle left-recursion + negation correctly.

    // @Ignore enabled
    @Test
    public void testR5_tabledLeftRecursion() {
        prolog.consult(":- table path/2.");
        prolog.consult("edge(a, b). edge(b, c). edge(c, d).");
        prolog.consult("path(X, Y) :- edge(X, Y).");
        prolog.consult("path(X, Y) :- path(X, Z), edge(Z, Y).");
        // Left-recursive: must terminate via tabling
        List<Map<String, Term>> r = prolog.solve("path(a, d).");
        assertEquals(1, r.size());
    }

    // @Ignore enabled
    @Test
    public void testR5_tabledNegation() {
        prolog.consult(":- table p/1.");
        prolog.consult("p(X) :- \\+ p(X).");
        // Under WFS, p(a) is undefined; should NOT loop infinitely.
        // Implementation may choose: undefined → fail, or "unknown" atom.
        try {
            long start = System.currentTimeMillis();
            prolog.solve("p(a).");
            long elapsed = System.currentTimeMillis() - start;
            assertTrue("must terminate within 1s under WFS", elapsed < 1000);
        } catch (RuntimeException e) {
            // explicit undefined/loop_error is acceptable
        }
    }

    // ===================================================================
    // Coroutining (refinement, complements R1)
    // ===================================================================

    // @Ignore enabled
    @Test
    public void testCoroutining_freezeFiresOnUnify() {
        prolog.consult("probe(_).");
        prolog.solve("freeze(X, assertz(probe(triggered))), X = 1.");
        List<Map<String, Term>> r = prolog.solve("probe(triggered).");
        assertEquals(1, r.size());
    }

    @Ignore("API design limitation: variables across separate solve() calls are different parsed objects; SWI REPL maintains identity via var-name map. JProlog programmatic API would need session-level identity tracking — deferred as scope expansion.")
    @Test
    public void testCoroutining_whenReSuspends() {
        prolog.solve("when(ground(f(X, Y)), assertz(probe(fired))), X = 1.");
        List<Map<String, Term>> r1 = prolog.solve("probe(fired).");
        assertEquals("when must not fire while Y unbound", 0, r1.size());
        prolog.solve("Y = 2.");
        List<Map<String, Term>> r2 = prolog.solve("probe(fired).");
        assertEquals("when must fire after Y bound", 1, r2.size());
    }

    // ===================================================================
    // R8 - LISTTERM CONSOLIDATION (behavior check, not feature)
    // ===================================================================

    // @Ignore enabled
    @Test
    public void testR8_listTermVsConsCellIdentity() {
        // After consolidation, parser should emit one canonical form
        List<Map<String, Term>> r = prolog.solve(
            "X = [a, b, c], X == '.'(a, '.'(b, '.'(c, []))).");
        assertEquals(1, r.size());
    }
}
