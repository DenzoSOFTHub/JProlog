package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0496 - 4.1 wave B acceptance: the io family on the v4 SPI.
/**
 * 4.1 wave B, step 1: {@code format/1,2,3}, the {@code write/1} family, {@code nl}, {@code tab},
 * the character I/O predicates and the current-stream accessors are v4 natives.
 *
 * <p>Every test here would also pass on the bridged registry implementation — that is the point,
 * the migration is behaviour-preserving — <b>except</b> {@link #testISS0496_TheIoFamilyIsNative},
 * which fails the moment one of them is not registered in the {@link BuiltinTable}, and the two
 * corrections this wave makes on purpose ({@code writeq/2} captured, {@code put_code/2}).
 */
public class EngineV4IoTest {

    private Prolog prolog;
    private ByteArrayOutputStream buffer;

    @Before
    public void setUp() {
        prolog = new Prolog();
        buffer = new ByteArrayOutputStream();
        StreamManager.setThreadLocalOutput(new PrintStream(buffer, true));
    }

    @After
    public void tearDown() {
        StreamManager.setThreadLocalOutput(null);
    }

    /** Run a goal and return everything it printed. */
    private String out(String goal) {
        buffer.reset();
        List<Map<String, Term>> sols = prolog.solve(goal + ".");
        assertFalse("goal failed: " + goal, sols.isEmpty());
        return buffer.toString();
    }

    private Term errorOf(String goal) {
        try {
            prolog.solve(goal + ".");
        } catch (PrologException e) {
            return e.getErrorTerm();
        }
        fail("expected an error from: " + goal);
        return null;
    }

    private String text(Term t) {
        return it.denzosoft.jprolog.core.util.TermFormatter.format(t, true, false, true, 1200);
    }

    // ================================================================ registration

    /** The structural guarantee: these indicators are NATIVE, not bridged. */
    @Test
    public void testISS0496_TheIoFamilyIsNative() {
        BuiltinTable t = prolog.getV4Engine().natives();
        String[][] expected = {
            {"format", "1"}, {"format", "2"}, {"format", "3"},
            {"write", "1"}, {"write", "2"}, {"writeln", "1"}, {"writeln", "2"},
            {"writeq", "1"}, {"writeq", "2"}, {"print", "1"}, {"print", "2"},
            {"write_canonical", "1"}, {"write_canonical", "2"},
            {"write_term", "2"}, {"write_term", "3"},
            {"nl", "0"}, {"nl", "1"}, {"tab", "1"}, {"tab", "2"},
            {"put_char", "1"}, {"put_char", "2"}, {"put_code", "1"}, {"put_code", "2"},
            {"get_char", "1"}, {"get_char", "2"}, {"get_code", "1"}, {"get_code", "2"},
            {"peek_char", "1"}, {"peek_char", "2"}, {"peek_code", "1"}, {"peek_code", "2"},
            {"flush_output", "0"}, {"flush_output", "1"},
            {"current_input", "1"}, {"current_output", "1"},
            {"set_input", "1"}, {"set_output", "1"},
            {"at_end_of_stream", "0"}, {"at_end_of_stream", "1"},
        };
        for (String[] e : expected) {
            assertTrue(e[0] + "/" + e[1] + " must be a v4 native",
                t.isNative(e[0], Integer.parseInt(e[1])));
        }
    }

    // ================================================================ the write family

    @Test
    public void testISS0496_WriteFamilyQuoting() {
        assertEquals("A b", out("write('A b')"));
        assertEquals("'A b'", out("writeq('A b')"));
        assertEquals("A b", out("print('A b')"));
        assertEquals("'A b'", out("write_canonical('A b')"));
        assertEquals("hello\n", out("writeln(hello)"));
        assertEquals("1+2*3", out("write(1+2*3)"));
        assertEquals("'.'(a,'.'(b,[]))", out("write_canonical([a,b])"));
    }

    @Test
    public void testISS0496_WriteTermOptions() {
        assertEquals("f(A,'a b')", out("write_term(f(_,'a b'), [quoted(true)])").replaceAll("_G?\\d+", "A"));
        assertEquals("B", out("write_term('$VAR'(1), [numbervars(true)])"));
        assertEquals("$VAR(1)", out("write_term('$VAR'(1), [numbervars(false)])"));
        assertEquals("[1|...]", out("write_term([1,2,3], [max_depth(2)])"));
        assertEquals("f(a)", out("write_term(user_output, f(a), [quoted(true)])"));
    }

    /** An unknown write option is a domain error, and it comes from the native now. */
    @Test
    public void testISS0496_WriteTermRejectsUnknownOption() {
        assertEquals("error(domain_error(write_option,bogus(true)),'write_term/2')",
            text(errorOf("write_term(f(a), [bogus(true)])")));
    }

    /** ISS-2025-0496 correction: writeq/2 used the static stream map and escaped the capture. */
    @Test
    public void testISS0496_WriteqToAStreamIsCaptured() {
        assertEquals("'A b'", out("writeq(user_output, 'A b')"));
    }

    /** ISS-2025-0496 correction: put_code/2 threw "put_code/1 requires exactly 1 argument". */
    @Test
    public void testISS0496_PutCodeAcceptsAStream() {
        assertEquals("a", out("put_code(user_output, 0'a)"));
    }

    @Test
    public void testISS0496_TabAndPutCharFailureModes() {
        assertEquals("   ", out("tab(3)"));
        assertEquals("", out("tab(0)"));
        assertTrue(prolog.solve("tab(-1).").isEmpty());
        assertTrue(prolog.solve("tab(a).").isEmpty());
        assertEquals("a", out("put_char(a)"));
        // START_CHANGE: ISS-2025-0505 - 4.3 wave D: put_char/1 and put_code/1 raise the ISO
        // errors of 8.12.3.3 instead of failing. tab/1,2 keeps its silent failure (non-ISO).
        assertEquals("error(type_error(character,ab),'put_char/1')", text(errorOf("put_char('ab')")));
        assertEquals("error(instantiation_error,'put_char/1')", text(errorOf("put_char(_)")));
        assertEquals("error(type_error(integer,a),'put_code/1')", text(errorOf("put_code(a)")));
        assertEquals("error(representation_error(character_code),'put_code/1')",
            text(errorOf("put_code(-1)")));
        // END_CHANGE: ISS-2025-0505
    }

    @Test
    public void testISS0496_CurrentStreams() {
        assertEquals("[{S=user_output}]", showOne("current_output(S)"));
        assertEquals("[{S=user_input}]", showOne("current_input(S)"));
    }

    private String showOne(String goal) {
        List<Map<String, Term>> sols = prolog.solve(goal + ".");
        assertEquals(1, sols.size());
        StringBuilder sb = new StringBuilder("[{");
        java.util.TreeMap<String, Term> t = new java.util.TreeMap<String, Term>(sols.get(0));
        boolean first = true;
        for (Map.Entry<String, Term> e : t.entrySet()) {
            if (e.getKey().startsWith("_")) continue;
            if (!first) sb.append(", ");
            first = false;
            sb.append(e.getKey()).append('=').append(text(e.getValue()));
        }
        return sb.append("}]").toString();
    }

    // ================================================================ format/1,2,3 directives

    @Test
    public void testISS0496_FormatSimpleDirectives() {
        assertEquals("hello\n", out("format(\"hello~n\", [])"));
        assertEquals("a-b\n", out("format(\"~a-~a~n\", [a,b])"));
        assertEquals("f(A,A b,[1,2])\n", out("format(\"~w~n\", [f(_,'A b',[1,2])])").replaceAll("_G?\\d+", "A"));
        assertEquals("f(A,'A b',[1,2])\n", out("format(\"~q~n\", [f(_,'A b',[1,2])])").replaceAll("_G?\\d+", "A"));
        assertEquals("42", out("format(\"~d\", [42])"));
        assertEquals("123.45", out("format(\"~2d\", [12345])"));
        assertEquals("1,234,567", out("format(\"~D\", [1234567])"));
        assertEquals("no args", out("format(\"no args\")"));
        assertEquals("hello", out("format('~w', hello)"));
        assertEquals("~", out("format(\"~~\", [])"));
        assertEquals("shown", out("format(\"~i~w\", [skipped,shown])"));
    }

    @Test
    public void testISS0496_FormatFloatsRadixAndChars() {
        assertEquals("3.141590", out("format(\"~f\", [3.14159])"));
        assertEquals("3.14", out("format(\"~2f\", [3.14159])"));
        assertEquals("3.141590e+04", out("format(\"~e\", [31415.9])"));
        assertEquals("3.142e+04", out("format(\"~3e\", [31415.9])"));
        assertEquals("31415.9", out("format(\"~g\", [31415.9])"));
        assertEquals("1010", out("format(\"~2r\", [10])"));
        assertEquals("FF", out("format(\"~16R\", [255])"));
        assertEquals("ff", out("format(\"~r\", [255])"));
        assertEquals("A", out("format(\"~c\", [65])"));
        assertEquals("AAA", out("format(\"~3c\", [65])"));
        assertEquals("xxx", out("format(\"~*c\", [3,0'x])"));
        assertEquals("hi", out("format(\"~s\", [[104,105]])"));
        assertEquals("hi", out("format(\"~s\", [\"hi\"])"));
    }

    /** The column stops: {@code ~t} marks the fill point, {@code ~N|} the absolute column. */
    @Test
    public void testISS0496_FormatColumnStops() {
        assertEquals("        x", out("format(\"~8|x\", [])"));
        assertEquals("         a          b", out("format(\"~ta~t~20|b\", [])"));
        assertEquals("ab        cd", out("format(\"~w~t~10|~w\", [ab,cd])"));
    }

    /** {@code ~@} runs a goal on the machine and splices its output in. */
    @Test
    public void testISS0496_FormatCallDirective() {
        prolog.consult("p1 :- write(one).\n");
        assertEquals("one!", out("format(\"~@!\", [p1])"));
    }

    /** {@code ~p} uses the user's portray/1, and falls back to ~w when there is none. */
    @Test
    public void testISS0496_FormatPortrayDirective() {
        prolog.consult("portray(pt(X)) :- write('<<'), write(X), write('>>').\n");
        assertEquals("<<9>>", out("format(\"~p\", [pt(9)])"));
        assertEquals("nopt(9)", out("format(\"~p\", [nopt(9)])"));
        assertEquals("<<1>>", out("print(pt(1))"));
    }

    @Test
    public void testISS0496_FormatCaptureSinks() {
        assertEquals("[{A='1+2'}]", showOne("format(atom(A), \"~w+~w\", [1,2])"));
        assertEquals("[{S=\"ab\"}]", showOne("format(string(S), \"~w\", [ab])"));
        assertEquals("[{C=[104,105]}]", showOne("format(codes(C), \"hi\", [])"));
        assertEquals("[{C=[h,i]}]", showOne("format(chars(C), \"hi\", [])"));
    }

    /** The three strict-mode errors of ISS-2025-0409, unchanged by the migration. */
    @Test
    public void testISS0496_FormatErrors() {
        assertEquals("error(format('not enough arguments'),'format/2')",
            text(errorOf("format(\"~w ~w\", [1])")));
        assertEquals("error(format('unknown directive: ~z'),'format/2')",
            text(errorOf("format(\"~z\", [1])")));
        assertEquals("error(type_error(integer,foo),'format/2')",
            text(errorOf("format(\"~d\", [foo])")));
    }

    @Test
    public void testISS0496_FormatToAStream() {
        assertEquals("to-stream\n", out("format(user_output, \"to-stream~n\", [])"));
    }

    // ================================================================ character I/O over a file

    /** get_char/get_code/peek_char/peek_code read through the engine's own decoder. */
    @Test
    public void testISS0496_CharacterIoOnAFileStream() throws Exception {
        java.io.File f = java.io.File.createTempFile("jprolog-io-", ".txt");
        try {
            java.io.PrintStream w = new java.io.PrintStream(f, "UTF-8");
            w.print("ab");
            w.close();
            String path = f.getAbsolutePath().replace("\\", "\\\\");
            List<Map<String, Term>> sols = prolog.solve(
                "open('" + path + "', read, S), peek_char(S, P), get_char(S, C1), "
              + "get_code(S, C2), get_char(S, C3), close(S).");
            assertEquals(1, sols.size());
            Map<String, Term> s = sols.get(0);
            assertEquals("a", text(s.get("P")));
            assertEquals("a", text(s.get("C1")));
            assertEquals("98", text(s.get("C2")));
            assertEquals("end_of_file", text(s.get("C3")));
        } finally {
            if (!f.delete()) f.deleteOnExit();
        }
    }

    /** A stream argument is a TERM: an unbound one is an instantiation error, not a crash. */
    @Test
    public void testISS0496_StreamArgumentErrors() {
        assertEquals("error(instantiation_error,'write/2')", text(errorOf("write(_, foo)")));
        assertEquals("error(existence_error(stream,nosuch),'write/2')",
            text(errorOf("write(nosuch, foo)")));
        assertEquals("error(domain_error(stream_or_alias,3),'write/2')",
            text(errorOf("write(3, foo)")));
    }

    /** with_output_to/2 captures the natives through the thread-local override (invariant 11). */
    @Test
    public void testISS0496_NativesGoThroughStreamManager() {
        assertEquals("[{A=ab}]", showOne("with_output_to(atom(A), (write(a), write(b)))"));
        assertEquals("[{A='x y'}]", showOne("with_output_to(atom(A), format(\"~w ~w\", [x,y]))"));
        assertEquals("[{A='  '}]", showOne("with_output_to(atom(A), tab(2))"));
    }
}
// END_CHANGE: ISS-2025-0496
