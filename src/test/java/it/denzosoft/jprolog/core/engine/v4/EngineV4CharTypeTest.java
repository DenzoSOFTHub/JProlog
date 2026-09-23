package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0503 - 4.2 wave C: char_type/2 and code_type/2 as native generators.
/**
 * {@code char_type/2} and {@code code_type/2}: the native generators, the parametric forms the
 * manual used to document as unsupported, and the modes and answers the registry versions gave.
 */
public class EngineV4CharTypeTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private List<String> all(String goal, String var) {
        List<String> out = new ArrayList<String>();
        for (Map<String, Term> s : prolog.solve(goal)) {
            Term t = s.get(var);
            out.add(t == null ? "<unbound>" : t.toString());
        }
        return out;
    }

    private String first(String goal, String var) {
        List<Map<String, Term>> s = prolog.solve(goal);
        assertFalse("expected a solution for " + goal, s.isEmpty());
        Term t = s.get(0).get(var);
        assertNotNull("no binding for " + var + " in " + goal, t);
        return t.toString();
    }

    private void succeeds(String goal) {
        assertFalse(goal + " should succeed", prolog.solve(goal).isEmpty());
    }

    private void fails(String goal) {
        assertTrue(goal + " should fail", prolog.solve(goal).isEmpty());
    }

    // ------------------------------------------------------------------ native

    @Test
    public void testCharTypeAndCodeTypeAreNative() {
        BuiltinTable t = prolog.getV4Engine().natives();
        assertTrue("char_type/2 must be native", t.isNative("char_type", 2));
        assertTrue("code_type/2 must be native", t.isNative("code_type", 2));
    }

    // ------------------------------------------------------------------ the classic modes

    @Test
    public void testClassicTestModeIsUnchanged() {
        succeeds("char_type(a, alpha).");
        succeeds("char_type('5', digit).");
        succeeds("char_type(' ', space).");
        succeeds("char_type('A', upper).");
        succeeds("char_type(a, lower).");
        succeeds("char_type(a, alnum).");
        succeeds("char_type('f', xdigit).");
        succeeds("char_type('\\n', newline).");
        succeeds("char_type('\\n', end_of_line).");
        succeeds("code_type(0'7, digit).");
        succeeds("code_type(0'a, alpha).");
        succeeds("code_type(0'a, csym).");
        fails("char_type(a, digit).");
        // ISS-2025-0601 (P4.8): an unknown class is domain_error(char_type, T) (SWI), not a failure
        assertTrue(prolog.solve("catch(char_type(a, zzz), error(domain_error(char_type, zzz), _), true).").size() == 1);
        fails("char_type(ab, alpha).");
        fails("char_type(1, alpha).");
        fails("code_type(a, alpha).");            // code_type wants a code, not an atom
    }

    @Test
    public void testTypeEnumerationKeepsItsHistoricalPrefix() {
        // char_type/2 has always answered alnum, alpha, ascii, ... in this order
        List<String> ts = all("char_type('A', T).", "T");
        assertEquals("alnum", ts.get(0));
        assertEquals("alpha", ts.get(1));
        assertEquals("ascii", ts.get(2));
        assertTrue(ts.contains("upper"));
        assertTrue(ts.contains("xdigit"));
        // code_type/2 has always started with alpha, alnum, space, ...
        List<String> cs = all("code_type(0'a, T).", "T");
        assertEquals("alpha", cs.get(0));
        assertEquals("alnum", cs.get(1));
        assertTrue(cs.contains("csym"));
    }

    @Test
    public void testCharacterEnumerationIsBoundedToAscii() {
        assertEquals(10, all("char_type(C, digit).", "C").size());
        assertEquals("0", all("char_type(C, digit).", "C").get(0));
        assertEquals(26, all("char_type(C, upper).", "C").size());
        assertEquals(10, all("code_type(C, digit).", "C").size());
        assertEquals("48", all("code_type(C, digit).", "C").get(0));
    }

    /** The generator hands out one answer per redo: `once` must not build the whole enumeration. */
    @Test
    public void testEnumerationIsLazy() {
        assertEquals("0", first("once(char_type(C, digit)).", "C"));
        assertEquals(1, prolog.solve("once(char_type(_, _)).").size());
    }

    // ------------------------------------------------------------------ the new classes

    @Test
    public void testSwiClassNamesAreAccepted() {
        succeeds("char_type('_', csym).");
        succeeds("char_type(a, csymf).");
        fails("char_type('5', csymf).");
        succeeds("char_type(' ', white).");
        succeeds("char_type('.', period).");
        succeeds("char_type('!', period).");
        succeeds("char_type('?', period).");
        succeeds("char_type('\"', quote).");
        succeeds("char_type('''', quote).");
        succeeds("char_type('(', paren).");
        succeeds("char_type(')', paren).");
        fails("char_type('[', paren).");
        succeeds("code_type(0'., period).");
        succeeds("code_type(0'\", quote).");
        succeeds("code_type(0'(, paren).");
    }

    /** The ten names only {@code char_type/2} used to know now work for {@code code_type/2} too. */
    @Test
    public void testCodeTypeAcceptsTheCharTypeOnlyClasses() {
        succeeds("code_type(0'f, xdigit).");
        succeeds("code_type(0'\\n, newline).");
        succeeds("code_type(0'\\\\, meta).");
        succeeds("code_type(0';, solo).");
        succeeds("code_type(0'+, symbol).");
        succeeds("code_type(0' , layout).");
    }

    // ------------------------------------------------------------------ the parametric forms

    @Test
    public void testDigitWeight() {
        assertEquals("7", first("char_type('7', digit(W)).", "W"));
        assertEquals("7", first("code_type(0'7, digit(W)).", "W"));
        succeeds("char_type('7', digit(7)).");
        fails("char_type('7', digit(8)).");
        fails("char_type(a, digit(_)).");
        assertEquals("3", first("char_type(X, digit(3)).", "X"));
        assertEquals("51", first("code_type(X, digit(3)).", "X"));
    }

    @Test
    public void testUpperAndLowerParametricForms() {
        assertEquals("a", first("char_type('A', upper(L)).", "L"));
        assertEquals("A", first("char_type(a, lower(U)).", "U"));
        assertEquals("97", first("code_type(0'A, upper(L)).", "L"));
        assertEquals("65", first("code_type(0'a, lower(U)).", "U"));
        fails("char_type(a, upper(_)).");
        fails("char_type('A', lower(_)).");
        fails("code_type(0'a, upper(_)).");
        // and in the generate direction
        assertEquals("A", first("char_type(X, upper(a)).", "X"));
        assertEquals("a", first("char_type(X, lower('A')).", "X"));
    }

    @Test
    public void testToLowerAndToUpper() {
        assertEquals("A", first("char_type(a, to_upper(U)).", "U"));
        assertEquals("a", first("char_type('A', to_lower(L)).", "L"));
        assertEquals("65", first("code_type(0'a, to_upper(U)).", "U"));
        assertEquals("97", first("code_type(0'A, to_lower(L)).", "L"));
        // a non-letter converts to itself
        assertEquals("5", first("char_type('5', to_upper(U)).", "U"));
        assertEquals(".", first("char_type('.', to_lower(L)).", "L"));
        // and the generate direction: both 'A' and 'a' uppercase to 'A'
        List<String> xs = all("char_type(X, to_upper('A')).", "X");
        assertEquals(2, xs.size());
        assertTrue(xs.contains("A"));
        assertTrue(xs.contains("a"));
    }

    /** With the type unbound, the parametric forms are enumerated as well as the atoms. */
    @Test
    public void testParametricFormsAppearInTheTypeEnumeration() {
        List<String> ts = all("char_type('A', T).", "T");
        assertTrue(ts.toString(), ts.contains("upper(a)"));
        assertTrue(ts.toString(), ts.contains("to_lower(a)"));
        assertTrue(ts.toString(), ts.contains("to_upper(A)"));
        List<String> cs = all("code_type(0'5, T).", "T");
        assertTrue(cs.toString(), cs.contains("digit(5)"));
        assertTrue(cs.toString(), cs.contains("to_upper(53)"));
    }

    // ------------------------------------------------------------------ shapes and errors

    @Test
    public void testMalformedTypeTermsSimplyFail() {
        // ISS-2025-0601 (P4.8): a malformed class is domain_error(char_type, T) now
        assertTrue(prolog.solve("catch(char_type(a, f(x, y)), error(domain_error(char_type, f(x, y)), _), true).").size() == 1);
        assertTrue(prolog.solve("catch(char_type(a, 3), error(domain_error(char_type, 3), _), true).").size() == 1);
        assertTrue(prolog.solve("catch(char_type(a, \"alpha\"), error(domain_error(char_type, _), _), true).").size() == 1);
        assertTrue(prolog.solve("catch(code_type(0'a, f(x, y)), error(domain_error(char_type, f(x, y)), _), true).").size() == 1);
        fails("code_type(1.5, alpha).");
        fails("code_type(-1, alpha).");            // -1 is end_of_file only (ISS-2025-0601)
    }

    /** Both are protected procedures (invariant 59: the native registration is what says so). */
    @Test
    public void testCharTypeIsAProtectedProcedure() {
        try {
            prolog.solve("assertz(char_type(a, b)).");
            assertFalse("assertz(char_type/2) must raise", true);
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            assertTrue(String.valueOf(e.getMessage()).contains("permission_error"));
        }
    }

    /** Used inside a DCG, which is where code_type/2 actually earns its keep. */
    @Test
    public void testUsedFromADcgBody() {
        prolog.consult(
            "digits([D|T]) --> [D], { code_type(D, digit) }, digits(T).\n"
          + "digits([D]) --> [D], { code_type(D, digit) }.\n");
        List<Map<String, Term>> s = prolog.solve("atom_codes('123', Cs), phrase(digits(D), Cs).");
        assertFalse(s.isEmpty());
        assertEquals("[49, 50, 51]", s.get(0).get("D").toString());
    }
}
// END_CHANGE: ISS-2025-0503
