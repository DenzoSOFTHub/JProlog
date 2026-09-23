package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SafeModeOptions;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * START_CHANGE: ISS-2025-0661 - 4.5 wave P7: the engine-side fixes of the release wave, one
 * method per ISS id (the test-suite items of the wave change the existing classes instead).
 * END_CHANGE: ISS-2025-0661
 */
public class EngineV45ReleaseTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private boolean succeeds(String q) {
        return !prolog.solve(q + ".").isEmpty();
    }

    /** §8 decision: negation over an incomplete table of the same SCC raises (WFS is LIM-046). */
    @Test(timeout = 30000)
    public void testISS0661_TabledNegationOverAnIncompleteTableRaises() {
        prolog.consult(":- table p/1.\np(X) :- \\+ p(X).\n"
            + ":- table win/1.\nwin(X) :- move(X, Y), \\+ win(Y).\nmove(a, b). move(b, a). move(b, c).\n"
            + ":- table w/1.\nw(X) :- m(X, Y), \\+ w(Y).\nm(a, b). m(b, c).\n");
        assertTrue("p :- \\+ p answered instead of raising", succeeds(
            "catch(p(a), error(permission_error(negate, incomplete_table, G), _), true), G == p(a)"));
        // the classic non-stratified game (a <-> b cycle through negation) raises too
        assertTrue(succeeds("catch(win(a), error(E, _), true), E = permission_error(negate, incomplete_table, _)"));
        // a stratified negation under tabling still answers, and the tables stay usable
        assertTrue(succeeds("findall(X, w(X), L), L == [b]"));
        assertTrue(succeeds("\\+ w(a), w(b), \\+ w(c)"));
        // outside a tabled evaluation \+ is untouched
        assertTrue(succeeds("\\+ fail, \\+ \\+ true"));
        // the permission error abandoned p's evaluation: a later call raises again, not `true`
        assertTrue(succeeds("catch(p(a), error(permission_error(_, _, _), _), true)"));
    }

    /** A query that does not parse raises error(syntax_error(M), query), not a message atom. */
    @Test
    public void testISS0671_QuerySyntaxErrorIsIso() {
        for (String bad : new String[] {"X = f(.", "foo(", "f(a b)."}) {
            try {
                prolog.solve(bad);
                fail("no syntax error for " + bad);
            } catch (PrologException e) {
                Term t = e.getErrorTerm();
                assertTrue(bad + ": " + t, t instanceof CompoundTerm);
                CompoundTerm c = (CompoundTerm) t;
                assertEquals("error", c.getName());
                assertEquals(2, c.arity());
                Term formal = c.arg(0);
                assertTrue(bad + ": " + formal, formal instanceof CompoundTerm
                    && "syntax_error".equals(((CompoundTerm) formal).getName()));
                assertEquals("query", c.arg(1).toString());
            }
        }
        // the streaming entry point reports the same term
        try {
            prolog.solveStream("foo(", s -> true);
            fail("no syntax error from solveStream");
        } catch (PrologException e) {
            assertEquals("error", ((CompoundTerm) e.getErrorTerm()).getName());
        }
    }

    /** Safe mode denies halt/0,1 by default; SafeModeOptions.allowHalt() keeps them. */
    @Test
    public void testISS0672_HaltIsSandboxedInSafeMode() {
        Prolog sandboxed = new Prolog();
        sandboxed.enableSafeMode();
        assertEquals(1, sandboxed.solve("catch(halt, error(E, _), true), "
            + "E == permission_error(call, sandboxed, halt).").size());
        assertEquals(1, sandboxed.solve("catch(halt(3), error(E, _), true), "
            + "E == permission_error(call, sandboxed, halt(3)).").size());
        // untrusted code cannot end the embedder's query with it
        assertEquals(1, sandboxed.solve("catch(halt, _, true), X = after, X == after.").size());

        Prolog allowed = new Prolog();
        allowed.enableSafeMode(new SafeModeOptions().allowHalt());
        try {
            allowed.solve("halt(4).");
            fail("halt/1 must still signal when allowed");
        } catch (PrologException e) {
            assertTrue(e.isHalt());
            assertEquals(4, e.getExitCode());
        }
        // without safe mode halt is the embedder's signal, as before
        try {
            prolog.solve("halt.");
            fail("halt/0 must signal");
        } catch (PrologException e) {
            assertTrue(e.isHalt());
            assertEquals(0, e.getExitCode());
        }
    }

    /** writeq of ESC is '\e' (SWI), not the octal '\33\'; the reader reads both. */
    @Test
    public void testISS0673_WriteqEscapeCharacter() {
        assertTrue(succeeds("with_output_to(atom(A), writeq('\\e')), A == '\\'\\\\e\\''"));
        assertTrue(succeeds("with_output_to(atom(A), writeq('a\\eb')), atom_length(A, 6)"));
        assertTrue(succeeds("with_output_to(atom(A), writeq(\"x\\ey\")), sub_atom(A, _, _, _, '\\\\e')"));
        assertTrue(succeeds("X = '\\33\\', Y = '\\e', X == Y, char_code(X, 27)"));
        // round trip
        assertTrue(succeeds("with_output_to(string(S), writeq(f('\\e', \"\\e\"))), "
            + "term_string(T, S), T == f('\\e', \"\\e\")"));
    }

    /** The unreachable legacy classes are deleted; the names stay protected and working. */
    @Test
    public void testISS0670_LegacyClassesDeletedNamesStillWork() {
        for (String gone : new String[] {
                "it.denzosoft.jprolog.builtin.io.Read",
                "it.denzosoft.jprolog.builtin.io.ReadTerm",
                "it.denzosoft.jprolog.builtin.term.AtomToTerm",
                "it.denzosoft.jprolog.builtin.term.TermToAtom",
                "it.denzosoft.jprolog.builtin.arithmetic.Between",
                "it.denzosoft.jprolog.builtin.dcg.DCGUtils$DCGTranslateRule"}) {
            try {
                Class.forName(gone);
                fail(gone + " must be deleted (ISS-2025-0670)");
            } catch (ClassNotFoundException expected) {
                // the point
            }
        }
        for (String pi : new String[] {"between(1,2,3)", "read_term(a,b)", "read(a)", "term_to_atom(a,b)",
                "atom_to_term(a,b,c)", "dcg_translate_rule(a,b)"}) {
            assertTrue(pi, succeeds("catch(assertz(" + pi + "), error(permission_error(modify, "
                + "static_procedure, _), _), true)"));
        }
        assertTrue(succeeds("findall(X, between(1, 3, X), L), L == [1,2,3]"));
        assertTrue(succeeds("term_to_atom(f(a, 'a b'), A), atom(A), atom_to_term(A, T, _), T == f(a, 'a b')"));
        assertTrue(succeeds("atom_to_term('g(X, Y, X)', T, B), T = g(P, Q, R), P == R, P \\== Q, length(B, 2)"));
        assertTrue(succeeds("dcg_translate_rule((a --> [x]), R), R = (H :- _), functor(H, a, 2)"));
        // dcg_translate_rule/4 was a message-atom stub of the deleted class: now an unknown procedure
        assertTrue(succeeds("catch(dcg_translate_rule(a, b, c, d), error(existence_error(procedure, PI), _), true), "
            + "PI == dcg_translate_rule/4"));
        // call_dcg/3 translates through the v2 translator (the legacy one made \+ a nonterminal)
        assertTrue(succeeds("call_dcg((\\+ [y], [x]), [x, y], R), R == [y]"));
        assertTrue(succeeds("\\+ call_dcg((\\+ [x], [x]), [x, y], _)"));
    }

    /** The version flags follow the release (they said 2.0.15), in SWI's shapes. */
    @Test
    public void testISS0675_VersionFlags() {
        assertTrue(succeeds("current_prolog_flag(version, V), V == 40500"));
        assertTrue(succeeds("current_prolog_flag(version_data, D), D == jprolog(4, 5, 0, [])"));
        assertTrue(succeeds("current_prolog_flag(prolog_version, P), P == 'jprolog-4.5.0'"));
        assertTrue(succeeds("catch(set_prolog_flag(version, 1), error(E, _), true), "
            + "E = permission_error(modify, flag, version)"));
    }
}
