package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * ISS-2025-0595 (4.5 wave P4.6) — format/2,3 as a table: {@code format(atom(A), Format, Args)}
 * and the expected text, or {@code ERR:<formal prefix>} for the error term's formal. The
 * reference is SWI-Prolog 9 (pl-fmt.c) and C printf for ~e/~f/~g.
 */
public class EngineV4FormatTest {

    private static final String[][] ROWS = {
        // ~w ~q ~a ~p ~k ~W
        { "'~w'", "[hello]", "hello" },
        { "'~w'", "['A b']", "A b" },
        { "'~q'", "['A b']", "'A b'" },
        { "'~p'", "['A b']", "'A b'" },
        { "'~k'", "[1+a]", "+(1,a)" },
        { "'~W'", "[f('A',b), [quoted(true)]]", "f('A',b)" },
        { "'~a'", "[abc]", "abc" },
        { "'~a'", "[42]", "42" },
        { "'~a'", "[f(x)]", "ERR:format(" },
        { "'~w'", "x", "x" },                                   // non-list = one argument
        // ~d ~D
        { "'~d'", "[42]", "42" },
        { "'~d'", "[-42]", "-42" },
        { "'~d'", "[9223372036854775807]", "9223372036854775807" },
        { "'~d'", "[-123456789012345678901234567890]", "-123456789012345678901234567890" },
        { "'~2d'", "[314]", "3.14" },
        { "'~2d'", "[5]", "0.05" },
        { "'~2d'", "[-5]", "-0.05" },
        { "'~0d'", "[7]", "7" },
        { "'~D'", "[1234567]", "1,234,567" },
        { "'~D'", "[-12345678901234567890]", "-12,345,678,901,234,567,890" },
        { "'~2D'", "[1234567]", "12,345.67" },
        { "'~D'", "[123]", "123" },
        { "'~d'", "[1.0]", "ERR:format(" },
        { "'~d'", "[a]", "ERR:format(" },
        // ~f ~e ~g
        { "'~f'", "[1.5]", "1.500000" },
        { "'~2f'", "[3.14159]", "3.14" },
        { "'~0f'", "[2.5]", "2" },
        { "'~2f'", "[0.125]", "0.12" },
        { "'~1f'", "[0.15]", "0.1" },
        { "'~2f'", "[10]", "10.00" },
        { "'~2f'", "[100000000000000000000]", "100000000000000000000.00" },
        { "'~e'", "[1.5]", "1.500000e+00" },
        { "'~3e'", "[123456]", "1.235e+05" },
        { "'~e'", "[0.0]", "0.000000e+00" },
        { "'~2e'", "[1.0e-10]", "1.00e-10" },
        { "'~g'", "[0.1]", "0.1" },
        { "'~g'", "[100000.0]", "100000" },
        { "'~g'", "[1000000.0]", "1e+06" },
        { "'~g'", "[1.0e-5]", "1e-05" },
        { "'~g'", "[0.0001]", "0.0001" },
        { "'~3g'", "[3.14159]", "3.14" },
        { "'~f'", "[a]", "ERR:format(" },
        { "'~e'", "[\"x\"]", "ERR:format(" },
        { "'~g'", "[f(1)]", "ERR:format(" },
        // ~s ~c ~r ~R
        { "'~s'", "[[104,105]]", "hi" },
        { "'~s'", "[[h,i]]", "hi" },
        { "'~s'", "[\"str\"]", "str" },
        { "'~s'", "[f(x)]", "ERR:format(" },
        { "'~c'", "[65]", "A" },
        { "'~3c'", "[120]", "xxx" },
        { "'~*c'", "[4, 0'y]", "yyyy" },
        { "'~c'", "[foo]", "ERR:format(" },
        { "'~c'", "[-1]", "ERR:format(" },
        { "'~8r'", "[255]", "377" },
        { "'~16r'", "[255]", "ff" },
        { "'~16R'", "[255]", "FF" },
        { "'~36r'", "[123456789012345678901234567890]", "byw97um9s91dlz68tsi" },
        { "'~r'", "[10]", "ERR:format(" },
        { "'~1r'", "[10]", "ERR:format(" },
        // ~n ~~ ~i ~*
        { "'a~nb'", "[]", "a\nb" },
        { "'~2n'", "[]", "\n\n" },
        { "'~3n'", "[]", "\n\n\n" },
        { "'~~'", "[]", "~" },
        { "'~i~w'", "[a,b]", "b" },
        { "'~*c~w'", "[2, 0'-, end]", "--end" },
        // column stops and fill characters
        { "'~w~t~10|~w'", "[ab, x]", "ab        x" },
        { "'~t~w~10|'", "[right]", "     right" },
        { "'~t~w~t~11|'", "[mid]", "    mid    " },
        { "'~`-t~30|'", "[]", "------------------------------" },
        { "'~`*t~w~`*t~9|'", "[ab]", "***ab****" },
        { "'~48t~d~6|'", "[42]", "000042" },
        { "'~w~t~8+~w'", "[ab, c]", "ab      c" },
        { "'~w~t~+~w~t~+~w'", "[a, b, c]", "a       b       c" },
        { "'~a~t~15|~w~n~a~t~15|~w'", "[abc,1,defgh,2]", "abc            1\ndefgh          2" },
        { "'~t~2f~10|'", "[3.14159]", "      3.14" },
        { "'~w~5|~w'", "[toolongtext, x]", "toolongtextx" },
        // argument count
        { "'~w'", "[a,b]", "ERR:format(too many arguments)" },
        { "'~w~w'", "[a]", "ERR:format(not enough arguments)" },
        { "'~z'", "[a]", "ERR:format(" },
        { "'~w'", "[]", "ERR:format(not enough arguments)" },
    };

    @Test
    public void testISS0595_FormatDirectiveTable() {
        Prolog prolog = new Prolog();
        List<String> failures = new ArrayList<String>();
        for (String[] row : ROWS) {
            String q = "format(atom(A__), " + row[0] + ", " + row[1] + ")";
            String expected = row[2];
            String got;
            try {
                List<Map<String, Term>> s = prolog.solve(q + ".");
                if (s.isEmpty()) {
                    got = "FAIL";
                } else {
                    Term a = s.get(0).get("A__");
                    got = (a instanceof Atom) ? ((Atom) a).getName() : String.valueOf(a);
                }
            } catch (PrologException e) {
                Term err = e.getErrorTerm();
                got = "ERR:" + (err == null ? e.getMessage()
                    : TermFormatterHolder.text(((it.denzosoft.jprolog.core.terms.CompoundTerm) err).arg(0)));
            }
            boolean match = expected.startsWith("ERR:") && expected.endsWith("(") ? got.startsWith(expected)
                          : expected.equals(got);
            if (!match) failures.add(q + "  expected [" + expected + "] got [" + got + "]");
        }
        assertTrue(ROWS.length + " rows, failures:\n" + String.join("\n", failures), failures.isEmpty());
        assertTrue(ROWS.length >= 60);
    }

    /** format/3 sinks: atom, string, codes, codes with a tail, chars, chars with a tail. */
    @Test
    public void testISS0595_FormatSinks() {
        Prolog prolog = new Prolog();
        assertEquals(1, prolog.solve("format(codes(C), '~w', [ab]), C == [0'a, 0'b].").size());
        assertEquals(1, prolog.solve("format(codes(C, T), '~w', [ab]), T = [0'c], C == [0'a, 0'b, 0'c].").size());
        assertEquals(1, prolog.solve("format(chars(C), '~w', [ab]), C == [a, b].").size());
        assertEquals(1, prolog.solve("format(chars(C, T), '~w', [ab]), T = [], C == [a, b].").size());
        assertEquals(1, prolog.solve("format(string(S), '~w', [ab]), S == \"ab\".").size());
        assertEquals(1, prolog.solve("format(atom(A), '~w', [ab]), A == ab.").size());
        assertEquals(1, prolog.solve("format(codes(C), '~c', [128512]), C == [128512].").size());
        assertEquals(1, prolog.solve("format(atom(A), '~@', [write(hi)]), A == hi.").size());
        assertEquals(1, prolog.solve("format(atom(A), '~@', [member(X, [a,b])]), A == ''.").size());
    }

    /** Holder for the writer call, kept out of the row loop for readability. */
    private static final class TermFormatterHolder {
        static String text(Term t) {
            return it.denzosoft.jprolog.core.util.TermFormatter.format(t, false, false, false, 1200);
        }
    }
}
