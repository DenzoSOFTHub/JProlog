package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0497 - 4.1 wave B acceptance: the text families on the v4 SPI.
/**
 * 4.1 wave B, step 2: the {@code atom_*}, {@code string_*}, {@code number_*}, {@code char_code/2},
 * case-conversion, {@code atomic_list_concat/2,3}, {@code split_string/4}, {@code term_to_atom/2},
 * {@code term_string/2}, {@code keysort/2}, {@code delete/3} and {@code flatten/2} predicates are
 * v4 natives.
 *
 * <p>{@link #testISS0497_TheTextFamiliesAreNative} fails if any of them is not registered in the
 * {@link BuiltinTable}; the rest pin the modes and the ISO error terms the registry versions
 * produced, so the migration cannot quietly change one.
 */
public class EngineV4TextTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private String text(Term t) {
        return it.denzosoft.jprolog.core.util.TermFormatter.format(t, true, false, true, 1200);
    }

    /** All solutions of a one-variable query, rendered as "V1;V2;..." ("false" when there are none). */
    private String all(String goal, String var) {
        List<Map<String, Term>> sols = prolog.solve(goal + ".");
        if (sols.isEmpty()) return "false";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < sols.size(); i++) {
            if (i > 0) sb.append(';');
            sb.append(text(sols.get(i).get(var)));
        }
        return sb.toString();
    }

    /** All solutions, rendered as "A=..,B=..;A=..,B=.." over the named variables. */
    private String all(String goal, String... vars) {
        List<Map<String, Term>> sols = prolog.solve(goal + ".");
        if (sols.isEmpty()) return "false";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < sols.size(); i++) {
            if (i > 0) sb.append(';');
            for (int v = 0; v < vars.length; v++) {
                if (v > 0) sb.append(',');
                sb.append(vars[v]).append('=').append(text(sols.get(i).get(vars[v])));
            }
        }
        return sb.toString();
    }

    private String err(String goal) {
        try {
            prolog.solve(goal + ".");
        } catch (PrologEvaluationException e) {
            return "eval:" + e.getMessage();
        } catch (PrologException e) {
            return text(e.getErrorTerm());
        }
        fail("expected an error from: " + goal);
        return null;
    }

    // ================================================================ registration

    @Test
    public void testISS0497_TheTextFamiliesAreNative() {
        BuiltinTable t = prolog.getV4Engine().natives();
        String[][] expected = {
            {"atom_length", "2"}, {"atom_concat", "3"}, {"atom_chars", "2"}, {"atom_codes", "2"},
            {"char_code", "2"}, {"upcase_atom", "2"}, {"downcase_atom", "2"},
            {"number_chars", "2"}, {"number_codes", "2"}, {"atom_number", "2"},
            {"atom_string", "2"}, {"number_string", "2"}, {"string_to_atom", "2"},
            {"string_chars", "2"}, {"string_codes", "2"}, {"string_length", "2"},
            {"string_concat", "3"}, {"string_code", "3"}, {"split_string", "4"},
            {"atomic_list_concat", "2"}, {"atomic_list_concat", "3"},
            {"term_to_atom", "2"}, {"term_string", "2"},
            {"keysort", "2"}, {"delete", "3"}, {"flatten", "2"},
        };
        for (String[] e : expected) {
            assertTrue(e[0] + "/" + e[1] + " must be a v4 native",
                t.isNative(e[0], Integer.parseInt(e[1])));
        }
    }

    // ================================================================ atom_length/2

    @Test
    public void testISS0497_AtomLengthModesAndErrors() {
        assertEquals("5", all("atom_length(hello, L)", "L"));
        assertEquals("0", all("atom_length('', L)", "L"));
        assertEquals("3", all("atom_length(\"str\", L)", "L"));
        assertEquals("false", all("atom_length(hello, 3)", "L"));
        assertEquals("error(instantiation_error,'atom_length/2')", err("atom_length(_, _)"));
        assertEquals("error(type_error(atom,123),'atom_length/2')", err("atom_length(123, _)"));
        assertEquals("error(type_error(atom,f(x)),'atom_length/2')", err("atom_length(f(x), _)"));
        assertEquals("error(type_error(integer,a),'atom_length/2')", err("atom_length(ab, a)"));
        assertEquals("error(domain_error(not_less_than_zero,-1),'atom_length/2')",
            err("atom_length(hello, -1)"));
    }

    // ================================================================ atom_concat/3

    @Test
    public void testISS0497_AtomConcatAllModes() {
        assertEquals("abcd", all("atom_concat(ab, cd, X)", "X"));
        assertEquals("ab", all("atom_concat(X, cd, abcd)", "X"));
        assertEquals("cd", all("atom_concat(ab, X, abcd)", "X"));
        assertEquals("X='',Y=abc;X=a,Y=bc;X=ab,Y=c;X=abc,Y=''",
            all("atom_concat(X, Y, abc)", "X", "Y"));
        assertEquals("X='',Y=''", all("atom_concat(X, Y, '')", "X", "Y"));
        assertEquals("error(type_error(atom,1),'atom_concat/3')", err("atom_concat(1, 2, _)"));
        assertEquals("error(instantiation_error,'atom_concat/3')", err("atom_concat(_, _, _)"));
    }

    /** The split mode is a lazy generator now: once/1 stops it after the first alternative. */
    @Test
    public void testISS0497_AtomConcatSplitIsLazy() {
        assertEquals("X='',Y=abcdef", all("once(atom_concat(X, Y, abcdef))", "X", "Y"));
        assertEquals("X=\"\",Y=\"abcdef\"", all("once(string_concat(X, Y, \"abcdef\"))", "X", "Y"));
    }

    // ================================================================ atom_chars/2, atom_codes/2

    @Test
    public void testISS0497_AtomCharsAndCodes() {
        assertEquals("[a,b,c]", all("atom_chars(abc, X)", "X"));
        assertEquals("abc", all("atom_chars(X, [a,b,c])", "X"));
        assertEquals("['1','2','3']", all("atom_chars(123, X)", "X"));
        assertEquals("[]", all("atom_chars('', X)", "X"));
        assertEquals("[97,98,99]", all("atom_codes(abc, X)", "X"));
        assertEquals("ab", all("atom_codes(X, [97,98])", "X"));
        assertEquals("[49,46,53]", all("atom_codes(1.5, X)", "X"));
        assertEquals("error(instantiation_error,'atom_chars/2')", err("atom_chars(_, _)"));
        assertEquals("error(instantiation_error,'atom_chars/2')", err("atom_chars(_, [a|_])"));
        assertEquals("error(type_error(character,1),'atom_chars/2')", err("atom_chars(_, [a,1])"));
        assertEquals("error(instantiation_error,'atom_codes/2')", err("atom_codes(_, _)"));
    }

    // ================================================================ char_code/2

    @Test
    public void testISS0497_CharCode() {
        assertEquals("97", all("char_code(a, X)", "X"));
        assertEquals("a", all("char_code(X, 97)", "X"));
        assertEquals("error(instantiation_error,'char_code/2')", err("char_code(_, _)"));
        assertEquals("error(type_error(character,ab),'char_code/2')", err("char_code(ab, _)"));
        assertEquals("error(type_error(integer,0.5),'char_code/2')", err("char_code(_, 0.5)"));
    }

    // ================================================================ case conversion

    @Test
    public void testISS0497_CaseConversion() {
        assertEquals("'HELLO'", all("upcase_atom(hello, X)", "X"));
        assertEquals("'HELLO WORLD'", all("upcase_atom('Hello World', X)", "X"));
        assertEquals("hello", all("downcase_atom('HeLLo', X)", "X"));
        assertTrue(err("upcase_atom(_, _)").startsWith("eval:"));
        assertTrue(err("upcase_atom(123, _)").startsWith("eval:"));
    }

    // ================================================================ number_chars/2, number_codes/2

    @Test
    public void testISS0497_NumberTextConversion() {
        assertEquals("[49,50,51]", all("number_codes(123, X)", "X"));
        assertEquals("123", all("number_codes(X, \"123\")", "X"));
        assertEquals("12", all("number_codes(X, [49,50])", "X"));
        assertEquals("['1','2']", all("number_chars(12, X)", "X"));
        assertEquals("12", all("number_chars(X, ['1','2'])", "X"));
        assertEquals("error(syntax_error(illegal_number),'number_chars/2')",
            err("number_chars(X, [a])"));
        assertEquals("error(syntax_error(illegal_number),'number_codes/2')",
            err("number_codes(X, [])"));
        assertEquals("error(type_error(number,a),'number_codes/2')", err("number_codes(a, _)"));
    }

    // ================================================================ atom_number/2 and the strings

    @Test
    public void testISS0497_AtomNumberAndStringConversions() {
        assertEquals("123", all("atom_number('123', X)", "X"));
        assertEquals("'123'", all("atom_number(X, 123)", "X"));
        assertEquals("false", all("atom_number(abc, X)", "X"));
        assertEquals("12.5", all("atom_number('12.5', X)", "X"));
        assertEquals("false", all("atom_number(X, Y)", "X"));
        assertEquals("42", all("number_string(N, \"42\")", "N"));
        assertEquals("\"42\"", all("number_string(42, S)", "S"));
        assertEquals("\"abc\"", all("atom_string(abc, S)", "S"));
        assertEquals("abc", all("atom_string(A, \"abc\")", "A"));
        assertEquals("\"ab\"", all("string_chars(S, [a,b])", "S"));
        assertEquals("[a,b]", all("string_chars(\"ab\", L)", "L"));
        assertEquals("[97,98]", all("string_codes(\"ab\", L)", "L"));
        assertEquals("\"a\"", all("string_codes(S, [97])", "S"));
        assertEquals("3", all("string_length(\"abc\", L)", "L"));
        assertEquals("3", all("string_length(abc, L)", "L"));
        assertEquals("\"abcd\"", all("string_concat(\"ab\", \"cd\", S)", "S"));
        assertEquals("97", all("string_code(1, \"abc\", C)", "C"));
        assertEquals("false", all("string_code(0, \"abc\", C)", "C"));
        assertEquals("abc", all("string_to_atom(\"abc\", A)", "A"));
        assertEquals("\"abc\"", all("string_to_atom(S, abc)", "S"));
    }

    // ================================================================ split_string/4

    @Test
    public void testISS0497_SplitString() {
        assertEquals("[\"a\",\"b\",\"\",\"c\"]", all("split_string(\"a,b,,c\", \",\", \"\", L)", "L"));
        assertEquals("[\"a\",\"b\"]", all("split_string(\" a b \", \" \", \" \", L)", "L"));
        assertEquals("[\"abc\"]", all("split_string(\"abc\", \"\", \"\", L)", "L"));
        assertEquals("[\"\",\"home\",\"\",\"x\"]", all("split_string(\"/home//x\", \"/\", \"\", L)", "L"));
        assertTrue(err("split_string(abc, \",\", \"\", _)").startsWith("eval:"));
    }

    // ================================================================ atomic_list_concat/2,3

    @Test
    public void testISS0497_AtomicListConcat() {
        assertEquals("abc", all("atomic_list_concat([a,b,c], X)", "X"));
        assertEquals("a1s", all("atomic_list_concat([a,1,\"s\"], X)", "X"));
        assertEquals("false", all("atomic_list_concat(L, ab)", "L"));
        assertEquals("'a-b'", all("atomic_list_concat([a,b], '-', X)", "X"));
        assertEquals("[a,b,c]", all("atomic_list_concat(L, '-', 'a-b-c')", "L"));
        assertEquals("[a,b,c]", all("atomic_list_concat(L, '', abc)", "L"));
        assertEquals("false", all("atomic_list_concat([a,_], '-', 'a-b')", "L"));
        assertTrue(err("atomic_list_concat(_, _, _)").startsWith("eval:"));
    }

    // ================================================================ term_to_atom/2, term_string/2

    @Test
    public void testISS0497_TermTextRoundTrip() {
        assertEquals("'f(X,y)'", all("term_to_atom(f(X,y), A)", "A"));
        assertEquals("f(a,b)", all("term_to_atom(T, 'f(a,b)')", "T"));
        assertEquals("\"f(x)\"", all("term_string(f(x), S)", "S"));
        assertEquals("foo(a,B)", all("term_string(T, \"foo(a,B)\")", "T").replaceAll("_G?\\d+", "B"));
    }

    /** term_string/2 is NEW in 4.2.0 — it did not exist as a registry built-in. */
    @Test
    public void testISS0497_TermStringIsNew() {
        assertFalse("term_string/2 must not have a legacy registration",
            prolog.getBuiltInRegistry().getBuiltInNames().contains("term_string"));
        assertTrue(prolog.getV4Engine().natives().isNative("term_string", 2));
    }

    // ================================================================ the three list natives

    @Test
    public void testISS0497_KeysortDeleteFlatten() {
        assertEquals("[a-2,b-1,b-0]", all("keysort([b-1,a-2,b-0], L)", "L"));
        assertEquals("error(instantiation_error,'keysort/2')", err("keysort([a-1|_], _)"));
        assertEquals("error(type_error(pair,a),'keysort/2')", err("keysort([a], _)"));
        assertEquals("[b,c]", all("delete([a,b,a,c], a, L)", "L"));
        assertEquals("[c]", all("delete([a,B,c], a, L)", "L"));
        assertEquals("[a,b,c,d,e]", all("flatten([a,[b,[c,d]],e], L)", "L"));
        assertEquals("[a]", all("flatten(a, L)", "L"));
    }

    /** keysort/2 is stable: equal keys keep their input order. */
    @Test
    public void testISS0497_KeysortIsStable() {
        assertEquals("[a-1,a-2,a-3]", all("keysort([a-1,a-2,a-3], L)", "L"));
    }

    /** A million-element atom_codes round trip must not touch the Java stack. */
    @Test
    public void testISS0497_LongTextRoundTripIsIterative() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 200000; i++) sb.append('x');
        prolog.consult("big('" + sb + "').\n");
        List<Map<String, Term>> sols = prolog.solve("big(A), atom_codes(A, C), atom_codes(B, C), atom_length(B, N).");
        assertEquals(1, sols.size());
        assertEquals("200000", text(sols.get(0).get("N")));
    }
}
// END_CHANGE: ISS-2025-0497
