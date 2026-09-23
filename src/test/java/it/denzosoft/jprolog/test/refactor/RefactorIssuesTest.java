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
        // ISS-2025-0612 (P4.18, decision §8): op/3 is permanent (ISO/SWI) — the operator defined
        // in the failed branch stays. (Method name kept.)
        prolog.solve("(op(700, xfx, " + uniq + "), fail ; true).");
        List<Map<String, Term>> r = prolog.solve("current_op(_, _, " + uniq + ").");
        assertEquals(1, r.size());
    }

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

    @Test
    public void testR2_operatorLocalToModule() {
        // ISS-2025-0573 (P3.2): a module's scope ends with the load that declared it, so the
        // three directives must be ONE load (as separate consults the op was declared in user)
        prolog.consult(":- module(m1, []).\n:- op(800, xfx, mylocalop).\n:- module(m2, []).\n");
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
        // ISS-2025-0611 (P4.17, decision §8): M:G runs G in M, exported or not (SWI)
        // ISS-2025-0662: no catch-anything — the answer is asserted exactly
        List<Map<String, Term>> r = prolog.solve("secret:hidden(X), X == 42.");
        assertEquals("a qualified call reaches the module's own predicate", 1, r.size());
    }

    // ===================================================================
    // R3 - STREAM ENCODING + EOF ACTION
    // ===================================================================
    // Goal: open/4 honors options (type, encoding, eof_action, reposition).

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

    @Test
    public void testR3_eofActionError() throws Exception {
        java.io.File f = java.io.File.createTempFile("eoftest", ".txt");
        f.deleteOnExit();
        java.nio.file.Files.write(f.toPath(), "a".getBytes());
        String path = f.getAbsolutePath().replace("\\", "\\\\");
        // START_CHANGE: ISS-2025-0662 - the exact ISO error, and the stream is closed (it leaked)
        List<Map<String, Term>> r = prolog.solve(
            "open('" + path + "', read, S, [eof_action(error)]), " +
            "catch((get_char(S, _), get_char(S, _), get_char(S, _)), E, true), close(S), " +
            "E = error(permission_error(input, past_end_of_stream, S1), _), S1 == S.");
        assertEquals(1, r.size());
        // END_CHANGE: ISS-2025-0662
    }

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

    @Test
    public void testR5_tabledNegation() {
        prolog.consult(":- table p/1.");
        prolog.consult("p(X) :- \\+ p(X).");
        // START_CHANGE: ISS-2025-0661 - under WFS p(a) is undefined; WFS is not implemented
        // (LIM-046), so negation over the incomplete table raises (decision §8) instead of
        // answering `true` as it did.
        List<Map<String, Term>> r = prolog.solve("catch(p(a), E, true), "
            + "E = error(permission_error(negate, incomplete_table, G), _), G == p(a).");
        assertEquals(1, r.size());
        // a stratified negation under tabling is unaffected
        prolog.consult(":- table w/1.\nw(X) :- m(X, Y), \\+ w(Y).\nm(a, b). m(b, c).");
        assertEquals(1, prolog.solve("findall(X, w(X), L), L == [b].").size());
        // END_CHANGE: ISS-2025-0661
    }

    // ===================================================================
    // Coroutining (refinement, complements R1)
    // ===================================================================

    @Test
    public void testCoroutining_freezeFiresOnUnify() {
        // Use a fresh predicate (no anonymous-matching fact) to assert.
        prolog.solve("freeze(X, assertz(triggered_marker)), X = 1.");
        List<Map<String, Term>> r = prolog.solve("triggered_marker.");
        assertEquals(1, r.size());
    }

    // v2.9.4: session-scoped attributed variables enable cross-solve identity
    @Test
    public void testCoroutining_whenReSuspends() {
        // START_CHANGE: ISS-2025-0347 - declare probe/1 dynamic: querying an UNDECLARED unknown
        // procedure now raises existence_error per ISO 7.7.7 (unknown=error), as in SWI.
        prolog.consult(":- dynamic(probe/1).");
        // END_CHANGE: ISS-2025-0347
        // START_CHANGE: ISS-2025-0461 - engine v4 wave W4, design decision 3 (B.17, approved):
        // cross-query coroutining is DROPPED. A query's variables die with the query, so the `Y`
        // of the second query is a NEW variable and the suspension of the first can never fire in
        // it. ISS-2025-0491 (4.1 wave A): the v2 branch that asserted the old session-scoped
        // behaviour is gone with the v2 engine.
        prolog.solve("when(ground(f(X, Y)), assertz(probe(fired))), X = 1.");
        assertEquals("when must not fire while Y unbound", 0, prolog.solve("probe(fired).").size());
        prolog.solve("Y = 2.");
        assertEquals("a finished query's suspension never fires in a later one",
            0, prolog.solve("probe(fired).").size());
        // ... and within ONE query it fires, with the woken goal's bindings propagating
        // (ISS-2025-0336, which is what the wake queue fixes).
        assertEquals("when fires inside one query", 1,
            prolog.solve("when(ground(f(A, B)), C = fired), A = 1, B = 2, C == fired.").size());
        // END_CHANGE: ISS-2025-0461
    }

    // ===================================================================
    // R8 - LISTTERM CONSOLIDATION (behavior check, not feature)
    // ===================================================================

    @Test
    public void testR8_listTermVsConsCellIdentity() {
        // After consolidation, parser should emit one canonical form
        List<Map<String, Term>> r = prolog.solve(
            "X = [a, b, c], X == '.'(a, '.'(b, '.'(c, []))).");
        assertEquals(1, r.size());
    }
}
