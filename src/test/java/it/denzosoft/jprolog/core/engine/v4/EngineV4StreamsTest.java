package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.PrintWriter;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0472/0473 - engine v4 wave W7 acceptance: per-engine streams (design B.11)
/**
 * Wave W7 acceptance for the stream layer: the per-engine stream table, the text decoder that makes
 * repositioning correct (limit <b>L-07</b>), the complete {@code stream_property/2} set, the new
 * introspection predicates, the per-thread current streams, engine isolation (limit <b>L-06</b>,
 * LIM-034) and the capture that no longer touches {@code System.out} (LIM-025).
 *
 * <p>Everything here is engine-neutral — the stream table is owned by the {@code Prolog} instance
 * and reached through the {@code StreamManager} facade by both engines — except the two tests that
 * select v4 explicitly, which restore the previous selection in {@code tearDown}.
 */
public class EngineV4StreamsTest {

    private Prolog prolog;
    private File tmp;

    @Before
    public void setUp() throws Exception {
        prolog = new Prolog();
        tmp = File.createTempFile("v4streams", ".txt");
        tmp.deleteOnExit();
        write(tmp, "hello\nworld\nthird line\n");
    }

    @After
    public void tearDown() {
        if (tmp != null) tmp.delete();
    }

    private static void write(File f, String text) throws Exception {
        PrintWriter w = new PrintWriter(f, "UTF-8");
        try { w.print(text); } finally { w.close(); }
    }

    private String path() {
        return tmp.getAbsolutePath().replace("\\", "/");
    }

    private Map<String, Term> one(String query) {
        List<Map<String, Term>> r = prolog.solve(query);
        assertFalse("query must succeed: " + query, r.isEmpty());
        return r.get(0);
    }

    // ==================================================================
    // L-07: repositioning a TEXT stream
    // ==================================================================

    /**
     * The headline limit: before wave W7 the reader was a {@code PushbackReader} over the raw file
     * with its own 8 KB buffer, so a {@code seek/4} moved the channel but not the reader and the
     * second {@code get_char/2} answered {@code e} (the second character) instead of {@code h}.
     */
    @Test
    public void testISS0472_SeekBofOnTextStreamRewindsWhatIsReadNext() {
        Map<String, Term> s = one("open('" + path() + "', read, S), get_char(S, C1), "
            + "seek(S, 0, bof, P), get_char(S, C2), close(S)");
        assertEquals("h", s.get("C1").toString());
        assertEquals("0", s.get("P").toString());
        assertEquals("seek(bof) must rewind what get_char/2 reads next", "h", s.get("C2").toString());
    }

    @Test
    public void testISS0472_SeekToAnArbitraryOffsetIsExact() {
        Map<String, Term> s = one("open('" + path() + "', read, S), seek(S, 6, bof, _), "
            + "get_char(S, C1), get_char(S, C2), close(S)");
        assertEquals("w", s.get("C1").toString());
        assertEquals("o", s.get("C2").toString());
    }

    @Test
    public void testISS0472_SetStreamPositionAlsoFlushesTheDecoder() {
        Map<String, Term> s = one("open('" + path() + "', read, S), get_char(S, _), get_char(S, _), "
            + "set_stream_position(S, 0), get_char(S, C), close(S)");
        assertEquals("h", s.get("C").toString());
    }

    @Test
    public void testISS0472_PeekIsAOneCharacterLookaheadOnTheDecoder() {
        Map<String, Term> s = one("open('" + path() + "', read, S), peek_char(S, P1), peek_char(S, P2), "
            + "get_char(S, G1), get_char(S, G2), close(S)");
        assertEquals("h", s.get("P1").toString());
        assertEquals("peek must not consume", "h", s.get("P2").toString());
        assertEquals("h", s.get("G1").toString());
        assertEquals("e", s.get("G2").toString());
    }

    @Test
    public void testISS0472_PeekedCharacterIsDiscardedByAReposition() {
        Map<String, Term> s = one("open('" + path() + "', read, S), get_char(S, _), peek_char(S, _), "
            + "seek(S, 0, bof, _), get_char(S, C), close(S)");
        assertEquals("h", s.get("C").toString());
    }

    // ==================================================================
    // positions and counters
    // ==================================================================

    @Test
    public void testISS0473_LineCountAndLinePositionAfterReadingTwoLines() {
        // "hello\nworld\n..." — read 7 characters: h e l l o \n w
        Map<String, Term> s = one("open('" + path() + "', read, S), "
            + "get_char(S,_), get_char(S,_), get_char(S,_), get_char(S,_), get_char(S,_), "
            + "get_char(S,_), get_char(S,C), "
            + "line_count(S, L), line_position(S, LP), character_count(S, CC), close(S)");
        assertEquals("w", s.get("C").toString());
        assertEquals("second line", "2", s.get("L").toString());
        assertEquals("one character into it", "1", s.get("LP").toString());
        assertEquals("7", s.get("CC").toString());
    }

    @Test
    public void testISS0473_StreamPositionDataFields() {
        Map<String, Term> s = one("open('" + path() + "', read, S), get_char(S,_), get_char(S,_), "
            + "stream_property(S, position(P)), "
            + "stream_position_data(char_count, P, C), "
            + "stream_position_data(line_count, P, L), "
            + "stream_position_data(line_position, P, LP), "
            + "stream_position_data(byte_count, P, B), close(S)");
        assertEquals("2", s.get("C").toString());
        assertEquals("1", s.get("L").toString());
        assertEquals("2", s.get("LP").toString());
        assertEquals("2", s.get("B").toString());
    }

    @Test
    public void testISS0473_LineCountersSurviveAReposition() {
        Map<String, Term> s = one("open('" + path() + "', read, S), seek(S, 6, bof, _), "
            + "line_count(S, L), line_position(S, LP), close(S)");
        assertEquals("2", s.get("L").toString());
        assertEquals("0", s.get("LP").toString());
    }

    // ==================================================================
    // stream_property/2 — the complete set
    // ==================================================================

    @Test
    public void testISS0473_StreamPropertyHasTheCompleteSet() {
        Map<String, Term> s = one("open('" + path() + "', read, S, [alias(v4in), eof_action(eof_code)]), "
            + "findall(P, stream_property(S, P), L), close(S)");
        String props = s.get("L").toString();
        for (String p : new String[] { "file_name(", "mode(read)", "input", "alias(v4in)", "position(",
                                       "end_of_stream(", "eof_action(eof_code)", "reposition(true)",
                                       "type(text)", "encoding(utf8)", "line_count(" }) {
            assertTrue("stream_property/2 must report " + p + " — got " + props, props.contains(p));
        }
    }

    @Test
    public void testISS0473_StreamPropertyAcceptsAnAliasAsTheStreamArgument() {
        Map<String, Term> s = one("open('" + path() + "', read, S, [alias(v4alias)]), "
            + "stream_property(v4alias, alias(A)), stream_property(v4alias, mode(M)), close(S)");
        assertEquals("v4alias", s.get("A").toString());
        assertEquals("read", s.get("M").toString());
    }

    @Test
    public void testISS0473_EndOfStreamProperty() {
        Map<String, Term> s = one("open('" + path() + "', read, S), "
            + "stream_property(S, end_of_stream(E0)), close(S)");
        assertEquals("not", s.get("E0").toString());
    }

    @Test
    public void testISS0473_SetStreamAddsAnAliasAndChangesProperties() {
        Map<String, Term> s = one("open('" + path() + "', read, S), set_stream(S, alias(v4set)), "
            + "set_stream(S, eof_action(error)), "
            + "stream_property(v4set, eof_action(EA)), close(S)");
        assertEquals("error", s.get("EA").toString());
    }

    @Test
    public void testISS0473_CurrentStreamEnumeratesOpenFileStreams() {
        Map<String, Term> s = one("open('" + path() + "', read, S), current_stream(F, M, S), close(S)");
        assertEquals("read", s.get("M").toString());
        assertTrue(s.get("F").toString().endsWith(".txt"));
    }

    @Test
    public void testISS0473_ClosedStreamIsGone() {
        // existence_error(stream, S) once the stream is closed
        List<Map<String, Term>> r = prolog.solve("open('" + path() + "', read, S), close(S), "
            + "catch((stream_property(S, mode(_)), Ok = yes), error(existence_error(stream, _), _), Ok = gone)");
        assertFalse(r.isEmpty());
        assertEquals("gone", r.get(0).get("Ok").toString());
    }

    // ==================================================================
    // read_term/2,3 options
    // ==================================================================

    @Test
    public void testISS0473_ReadTermOptions() throws Exception {
        File src = File.createTempFile("v4read", ".pl");
        src.deleteOnExit();
        write(src, "foo(X, Y, X).\nbar(1).\n");
        String p = src.getAbsolutePath().replace("\\", "/");
        Map<String, Term> s = one("open('" + p + "', read, S), "
            + "read_term(S, T, [variable_names(V), singletons(Sg), term_position(P0)]), "
            + "read_term(S, T2, [term_position(P1)]), close(S)");
        // ISS-2025-0566 (P3.4): a read term's variables are fresh cells; the names are in V
        Term t = s.get("T");
        assertEquals("foo", t.getName());
        assertTrue(t.getArguments().get(0) instanceof Variable);
        assertTrue(t.getArguments().get(0) == t.getArguments().get(2));
        assertTrue(t.getArguments().get(0) != t.getArguments().get(1));
        assertEquals("bar(1)", s.get("T2").toString());
        assertTrue("variable_names lists the named variables: " + s.get("V"),
            s.get("V").toString().contains("X") && s.get("V").toString().contains("Y"));
        assertTrue("Y is the singleton: " + s.get("Sg"), s.get("Sg").toString().contains("Y"));
        assertEquals("$stream_position(0, 1, 0, 0)", s.get("P0").toString());
        assertEquals("the second term starts on line 2, byte 14",
            "$stream_position(14, 2, 0, 14)", s.get("P1").toString());
        src.delete();
    }

    // ==================================================================
    // per-engine and per-thread isolation (L-06 / LIM-034)
    // ==================================================================

    @Test
    public void testISS0472_TwoEnginesDoNotShareStreams() {
        Prolog other = new Prolog();
        prolog.solve("open('" + path() + "', read, S, [alias(v4shared)])");
        List<Map<String, Term>> r = other.solve(
            "catch((stream_property(v4shared, mode(_)), Ok = visible), _, Ok = isolated)");
        assertFalse(r.isEmpty());
        assertEquals("a stream opened by engine A must be invisible to engine B",
            "isolated", r.get(0).get("Ok").toString());
    }

    @Test
    public void testISS0474_TwoEnginesDoNotShareOperators() {
        Prolog other = new Prolog();
        prolog.solve("op(333, xfx, v4opa)");
        assertFalse(prolog.solve("current_op(333, xfx, v4opa)").isEmpty());
        assertTrue("an operator defined in engine A must be invisible to engine B",
            other.solve("current_op(333, xfx, v4opa)").isEmpty());
    }

    @Test
    public void testISS0477_TwoEnginesDoNotShareSpyPointsOrProfilerCounters() {
        Prolog other = new Prolog();
        prolog.solve("spy(v4spied/1)");
        assertTrue(prolog.getEngineState().spies().has("v4spied", 1));
        assertFalse("a spy point set in engine A must not exist in engine B",
            other.getEngineState().spies().has("v4spied", 1));

        prolog.getEngineState().profile().enable();
        prolog.consult("prof_probe(1).\n");
        prolog.solve("prof_probe(_)");
        assertFalse("engine A counted something", prolog.getEngineState().profile().snapshot().isEmpty());
        assertFalse("the profiler must not be enabled in engine B",
            other.getEngineState().profile().isEnabled());
        assertTrue("engine B counted nothing", other.getEngineState().profile().snapshot().isEmpty());
    }

    @Test(timeout = 60000)
    public void testISS0472_TwoThreadsHaveTheirOwnCurrentOutput() throws Exception {
        final File a = File.createTempFile("v4outA", ".txt");
        final File b = File.createTempFile("v4outB", ".txt");
        a.deleteOnExit();
        b.deleteOnExit();
        final AtomicReference<Throwable> failure = new AtomicReference<Throwable>();
        Runnable r1 = new Runnable() {
            public void run() {
                try {
                    // ISS-2025-0635: rendezvous AFTER set_output, so both threads have redirected
                    // their current output at the same time before either writes
                    if (prolog.solve("open('" + a.getAbsolutePath().replace("\\", "/") + "', write, S), "
                        + "set_output(S), thread_send_message(p66two, ready), "
                        + "thread_get_message(p66one, ready, [timeout(20)]), "
                        + "write(from_thread_one), flush_output, close(S)").isEmpty()) {
                        throw new AssertionError("thread one: rendezvous timed out");
                    }
                } catch (Throwable t) { failure.compareAndSet(null, t); }
            }
        };
        Runnable r2 = new Runnable() {
            public void run() {
                try {
                    if (prolog.solve("open('" + b.getAbsolutePath().replace("\\", "/") + "', write, S), "
                        + "set_output(S), thread_send_message(p66one, ready), "
                        + "thread_get_message(p66two, ready, [timeout(20)]), "
                        + "write(from_thread_two), flush_output, close(S)").isEmpty()) {
                        throw new AssertionError("thread two: rendezvous timed out");
                    }
                } catch (Throwable t) { failure.compareAndSet(null, t); }
            }
        };
        // START_CHANGE: ISS-2025-0635 - wave P6.6: the two threads really OVERLAP now. The test used
        // to join t1 before starting t2, so it never had two current outputs alive at once.
        prolog.solve("message_queue_create(_, [alias(p66one)]), message_queue_create(_, [alias(p66two)])");
        Thread t1 = new Thread(r1);
        Thread t2 = new Thread(r2);
        t1.start();
        t2.start();
        t1.join(30000);
        t2.join(30000);
        prolog.solve("message_queue_destroy(p66one), message_queue_destroy(p66two)");
        // END_CHANGE: ISS-2025-0635
        if (failure.get() != null) throw new AssertionError("threads must not throw", failure.get());
        assertEquals("from_thread_one", read(a));
        assertEquals("from_thread_two", read(b));
        // and the main thread's current output was never touched
        assertEquals("user_output", StreamManager.getCurrentOutput());
    }

    private static String read(File f) throws Exception {
        byte[] data = java.nio.file.Files.readAllBytes(f.toPath());
        return new String(data, "UTF-8");
    }

    // ==================================================================
    // capture without System.out (LIM-025)
    // ==================================================================

    @Test
    public void testISS0472_WithOutputToDoesNotTouchSystemOut() {
        java.io.PrintStream before = System.out;
        Map<String, Term> s = one("with_output_to(atom(A), (write(captured), write(' too')))");
        assertEquals("captured too", s.get("A").toString());
        assertTrue("with_output_to/2 must not swap System.out", before == System.out);
    }

    @Test
    public void testISS0472_FormatToAtomAndCodesAndString() {
        assertEquals("x=1", one("format(atom(A), 'x=~w', [1])").get("A").toString());
        assertFalse(prolog.solve("format(codes(C), 'ab', []), C = [0'a, 0'b]").isEmpty());
        assertFalse(prolog.solve("with_output_to(codes(C), write(ab)), C = [0'a, 0'b]").isEmpty());
        assertFalse(prolog.solve("with_output_to(chars(C), write(ab)), C = [a, b]").isEmpty());
    }

    @Test
    public void testISS0472_NestedCapturesDoNotLeak() {
        Map<String, Term> s = one(
            "with_output_to(atom(A), (write(out1), with_output_to(atom(B), write(inner)), write(out2)))");
        assertEquals("out1out2", s.get("A").toString());
        assertEquals("inner", s.get("B").toString());
    }

    // ==================================================================
    // the parser's nesting limit is a resource error, never a silent failure
    // ==================================================================

    @Test
    public void testISS0473_DeeplyNestedInputRaisesParserNesting() {
        StringBuilder sb = new StringBuilder();
        // ISS-2025-0561 (P3.8): nesting is bounded by a real, generous limit (200 000) now, read
        // on a deep-stack helper; 5 000 levels read fine, so the test goes past the limit
        for (int i = 0; i < 250000; i++) sb.append('(');
        sb.append('a');
        for (int i = 0; i < 250000; i++) sb.append(')');
        List<Map<String, Term>> r = prolog.solve(
            "catch(term_to_atom(_T, '" + sb + "'), error(E, _), true)");
        assertFalse("term_to_atom/2 on 250 000 nested parentheses must not fail silently", r.isEmpty());
        assertNotNull("it must raise an error", r.get(0).get("E"));
        assertEquals("resource_error(parser_nesting)", r.get(0).get("E").toString());
    }

    // ==================================================================
    // the same stream layer on a second engine instance
    // ==================================================================

    @Test
    public void testISS0472_TheStreamLayerBehavesIdenticallyOnV4() {
        Prolog v4 = new Prolog();                        // ISS-2025-0491: one engine
        List<Map<String, Term>> r = v4.solve("open('" + path() + "', read, S), get_char(S, C1), "
            + "seek(S, 0, bof, _), get_char(S, C2), line_count(S, L), close(S)");
        assertFalse(r.isEmpty());
        assertEquals("h", r.get(0).get("C1").toString());
        assertEquals("h", r.get(0).get("C2").toString());
        assertEquals("1", r.get(0).get("L").toString());
    }

    @Test
    public void testISS0472_BinaryStreamBytesAndTextCharactersShareOnePosition() throws Exception {
        File bin = File.createTempFile("v4bin", ".bin");
        bin.deleteOnExit();
        java.io.FileOutputStream out = new java.io.FileOutputStream(bin);
        try { out.write(new byte[] { 1, 2, 3, 4 }); } finally { out.close(); }
        Map<String, Term> s = one("open('" + bin.getAbsolutePath().replace("\\", "/")
            + "', read, S, [type(binary)]), get_byte(S, B1), peek_byte(S, B2), get_byte(S, B3), "
            + "stream_property(S, position(P)), stream_position_data(byte_count, P, N), close(S)");
        assertEquals("1", s.get("B1").toString());
        assertEquals("2", s.get("B2").toString());
        assertEquals("2", s.get("B3").toString());
        assertEquals("2", s.get("N").toString());
        bin.delete();
    }
}
// END_CHANGE: ISS-2025-0472/0473
