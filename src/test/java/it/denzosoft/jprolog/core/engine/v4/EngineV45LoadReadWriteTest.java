package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.parser.v2.TermReader;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * 4.5 wave P3 — loading, reading and writing terms (ISS-2025-0560..0579). One method per ISS; each
 * fails on the 4.4.0 build (and on this tree before the wave).
 */
public class EngineV45LoadReadWriteTest {

    private Prolog prolog;
    private File dir;

    @Before
    public void setUp() throws Exception {
        prolog = new Prolog();
        dir = Files.createTempDirectory("p3load").toFile();
    }

    @After
    public void tearDown() {
        deleteTree(dir);
    }

    private static void deleteTree(File f) {
        File[] kids = f.listFiles();
        if (kids != null) for (File k : kids) deleteTree(k);
        f.delete();
    }

    private File file(String rel, String text) throws Exception {
        File f = new File(dir, rel);
        f.getParentFile().mkdirs();
        Files.write(f.toPath(), text.getBytes(StandardCharsets.UTF_8));
        return f;
    }

    private String path(File f) {
        return f.getAbsolutePath().replace("\\", "/");
    }

    private void ok(String query) {
        List<Map<String, Term>> s = prolog.solve(query + ".");
        assertTrue("expected success: " + query, !s.isEmpty());
    }

    private void no(String query) {
        List<Map<String, Term>> s = prolog.solve(query + ".");
        assertTrue("expected failure: " + query, s.isEmpty());
    }

    private void err(String goal, String formal) {
        ok("catch((" + goal + "), error(E__, _), true), nonvar(E__), E__ = " + formal);
    }

    private String output(String query) {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(bos, true);
        PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            prolog.solve(query + ".");
        } finally {
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        ps.flush();
        return bos.toString();
    }

    // ------------------------------------------------------------------ P3.10 escapes

    @Test
    public void testISS0560_LexerEscapesFollowSwi() {
        ok("atom_codes('\\e', [27])");
        ok("atom_codes('\\s', [32])");
        ok("atom_codes('\\u00e9', [233])");
        ok("atom_codes('\\U0001F600', [128512])");
        ok("atom_codes('\\x41\\\\101\\', [65, 65])");
        ok("X = \"a\\sb\", string_codes(X, [97, 32, 98])");
        // an undefined escape is a syntax error (it was read as the letter itself)
        err("term_to_atom(_, '''\\\\z''')", "syntax_error(_)");
        err("atom_to_term('f(''\\\\q'')', _, _)", "syntax_error(_)");
    }

    // ------------------------------------------------------------------ P3.8 / P3.12 nesting

    @Test
    public void testISS0561_OperatorChainsAndDeepTermsLoad() throws Exception {
        StringBuilder body = new StringBuilder("big :- ");
        for (int i = 0; i < 20000; i++) body.append(i == 0 ? "" : ", ").append("true");
        body.append(".\nsemi(X) :- ");
        for (int i = 0; i < 3000; i++) body.append(i == 0 ? "" : " ; ").append("X = ").append(i);
        body.append(".\n");
        StringBuilder deep = new StringBuilder("deep(");
        for (int i = 0; i < 8000; i++) deep.append("f(");
        deep.append('x');
        for (int i = 0; i < 8000; i++) deep.append(')');
        deep.append(").\nleft(");
        for (int i = 0; i < 8000; i++) deep.append(i == 0 ? "" : "+").append(i);
        deep.append(").\nparens(");
        for (int i = 0; i < 8000; i++) deep.append('(');
        deep.append('y');
        for (int i = 0; i < 8000; i++) deep.append(')');
        deep.append(").\n");
        prolog.consult(body.toString() + deep);
        ok("big");
        ok("semi(2999)");
        ok("deep(X), X = f(f(_))");
        ok("left(X), Y is X, Y =:= 7999 * 8000 / 2");
        ok("parens(y)");
        // P3.12: the same clauses through a .jpc (its writer and reader recursed) and listing
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        new Prolog().compile(deep.toString(), bos);
        Prolog q = new Prolog();
        q.consultCompiled(new java.io.ByteArrayInputStream(bos.toByteArray()));
        assertFalse(q.solve("deep(X), X = f(f(_)).").isEmpty());
        assertTrue(q.getListingOutput("deep/1").length() > 16000);
        // 8000 nested parentheses through term_to_atom/2 (5 000 raised parser_nesting before)
        StringBuilder p = new StringBuilder();
        for (int i = 0; i < 8000; i++) p.append('(');
        p.append('a');
        for (int i = 0; i < 8000; i++) p.append(')');
        ok("term_to_atom(T, '" + p + "'), T == a");
        // a real limit stays, and it is a resource error, never a StackOverflowError
        StringBuilder huge = new StringBuilder();
        for (int i = 0; i < 300000; i++) huge.append('[');
        for (int i = 0; i < 300000; i++) huge.append(']');
        err("term_to_atom(_, '" + huge + "')", "resource_error(parser_nesting)");
    }

    // ------------------------------------------------------------------ P3.9 writer round trip

    @Test
    public void testISS0562_WriterOutputReadsBack() {
        assertEquals("-(2^2)", output("writeq(-(2^2))"));
        assertEquals("-(2**2)", output("writeq(-(2**2))"));
        assertEquals("-(1.5^a)", output("writeq(-(1.5^a))"));
        assertEquals("-(1)", output("writeq(-(1))"));
        assertEquals("-(-1)", output("writeq(-(-1))"));
        assertEquals("- -a", output("writeq(-(-(a)))"));
        assertEquals("(-)-(-)", output("writeq(-(-,-))"));
        assertEquals("1-(-)", output("writeq(1-(-))"));
        assertEquals("f(-)", output("writeq(f(-))"));
        assertEquals("[-]", output("writeq([-])"));
        assertEquals("'[]'(a,b)", output("writeq('[]'(a,b))"));
        assertEquals("'{}'(a,b)", output("writeq('{}'(a,b))"));
        assertEquals("'\\177\\'", output("writeq('\\x7F\\')"));
        assertEquals("- (a,b)", output("writeq(-((a,b)))"));
        for (String t : new String[] {"-(2^2)", "-(1)", "-(-,-)", "1-(-)", "- (-)", "'[]'(a,b)",
                "'{}'(a,b)", "'\\x7F\\'", "-((a,b))", "\\+ (-)", "a=(\\+b)", "a = \\+b", "f(:- a)", "- - - a", "1 - -1",
                "2^ -1", "f(;, '|', '[]', [], {}, '{}')", "{-}", "[a|b]", "\"a\\\"b\"", "'a''b'",
                "(a:-b,c;d->e)", "(:-)", "f(:-, dynamic)", "- (1)", "-(-(1))", "1.0e10", "-0.0"}) {
            ok("X = (" + t + "), with_output_to(atom(A), writeq(X)), term_to_atom(Y, A), X == Y");
        }
    }

    /** P3.9 property test: random terms -> writeq -> v2 reader -> the same term. */
    @Test
    public void testISS0562_WriteqReadRoundTripProperty() {
        OperatorTable ops = prolog.getOps().table();
        Random rnd = new Random(20260923L);
        Writer.Options o = Writer.Options.writeq();
        o.ops = ops;
        for (int i = 0; i < 4000; i++) {
            Term t = randomTerm(rnd, 4, new ArrayList<Variable>(), ops);
            String text = Writer.format(t, o);
            Term back;
            try {
                back = new TermReader(it.denzosoft.jprolog.core.parser.v2.Lexer.tokenize(text), ops).readSingle();
            } catch (RuntimeException e) {
                fail("writeq output does not read back: " + text + " -- " + e.getMessage());
                return;
            }
            assertTrue("round trip changed the term: " + text + " -> " + Writer.format(back, o),
                variant(t, back, new IdentityHashMap<Variable, Variable>(), new IdentityHashMap<Variable, Variable>()));
        }
    }

    private static final String[] ATOMS = {
        "a", "foo", "[]", "{}", "A", "hello world", "-", "+", "*", ";", ",", "|", "!", "\n", "",
        "it's", "===>", "/*", ".", "\u007f", "mod", "is", "dynamic", "\\", "\\+", "->", ":-", "?-",
        "a.b", "_x", "[", "e", "%", "été", "{", "}", "'", "\"", "^", "@", "#"};
    private static final String[] INFIX = {"+", "-", "*", "/", "^", "**", "=", "==", "\\=", "is", "<",
        ":-", ",", ";", "->", ":", "mod", "rem", "xor", "//", "=..", "-->", "|"};
    private static final String[] PREFIX = {"-", "+", "\\", "\\+", ":-", "?-", "dynamic", "$"};

    private static Term randomTerm(Random r, int depth, List<Variable> vars, OperatorTable ops) {
        int k = r.nextInt(depth <= 0 ? 6 : 13);
        switch (k) {
            case 0: return new Atom(ATOMS[r.nextInt(ATOMS.length)]);
            case 1: return new Number((long) (r.nextInt(2001) - 1000));
            case 2: {
                double[] ds = {0.5, -2.25, 1.0e10, 1.5e-7, 123456789.125, -0.0, 1.0e15, 3.0e300,
                    Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY, 0.1};
                return new Number(ds[r.nextInt(ds.length)], false);
            }
            case 3: return new Number(new BigInteger("-123456789012345678901234567890").multiply(BigInteger.valueOf(r.nextInt(5) - 2)));
            case 4: {
                if (vars.isEmpty() || r.nextBoolean()) { Variable v = new Variable("V" + vars.size()); vars.add(v); return v; }
                return vars.get(r.nextInt(vars.size()));
            }
            case 5: return new PrologString(new String[] {"", "abc", "a\"b", "x\\y", "l\nm"}[r.nextInt(5)]);
            case 6: case 7: {
                String op = INFIX[r.nextInt(INFIX.length)];
                if (ops.getInfixOperator(op) == null) op = "+";
                return new CompoundTerm(new Atom(op), Arrays.asList(randomTerm(r, depth - 1, vars, ops), randomTerm(r, depth - 1, vars, ops)));
            }
            case 8: {
                String op = PREFIX[r.nextInt(PREFIX.length)];
                return new CompoundTerm(new Atom(op), Arrays.asList(randomTerm(r, depth - 1, vars, ops)));
            }
            case 9: {
                int n = 1 + r.nextInt(3);
                List<Term> as = new ArrayList<Term>();
                for (int i = 0; i < n; i++) as.add(randomTerm(r, depth - 1, vars, ops));
                return new CompoundTerm(new Atom(ATOMS[r.nextInt(ATOMS.length)]), as);
            }
            case 10: {
                Term tail = r.nextInt(4) == 0 ? randomTerm(r, depth - 1, vars, ops) : new Atom("[]");
                int n = 1 + r.nextInt(3);
                for (int i = 0; i < n; i++) {
                    tail = new CompoundTerm(new Atom("."), Arrays.asList(randomTerm(r, depth - 1, vars, ops), tail));
                }
                return tail;
            }
            case 11: return new CompoundTerm(new Atom("{}"), Arrays.asList(randomTerm(r, depth - 1, vars, ops)));
            default: return new Number((long) -r.nextInt(50));
        }
    }

    private static boolean variant(Term a, Term b, IdentityHashMap<Variable, Variable> ab, IdentityHashMap<Variable, Variable> ba) {
        a = Unify.deref(a);
        b = Unify.deref(b);
        if (a instanceof Variable || b instanceof Variable) {
            if (!(a instanceof Variable) || !(b instanceof Variable)) return false;
            Variable x = ab.get(a), y = ba.get(b);
            if (x == null && y == null) { ab.put((Variable) a, (Variable) b); ba.put((Variable) b, (Variable) a); return true; }
            return x == b && y == a;
        }
        if (a instanceof Number && b instanceof Number) {
            Number x = (Number) a, y = (Number) b;
            if (x.isInteger() != y.isInteger()) return false;
            if (x.isInteger()) return x.bigIntegerValue().equals(y.bigIntegerValue());
            return Double.compare(x.doubleValue(), y.doubleValue()) == 0;
        }
        if (a instanceof PrologString && b instanceof PrologString) {
            return ((PrologString) a).getStringValue().equals(((PrologString) b).getStringValue());
        }
        if (a instanceof Atom && b instanceof Atom) return ((Atom) a).getName().equals(((Atom) b).getName());
        if (a instanceof CompoundTerm && b instanceof CompoundTerm) {
            CompoundTerm x = (CompoundTerm) a, y = (CompoundTerm) b;
            if (!x.getName().equals(y.getName()) || x.getArguments().size() != y.getArguments().size()) return false;
            for (int i = 0; i < x.getArguments().size(); i++) {
                if (!variant(x.getArguments().get(i), y.getArguments().get(i), ab, ba)) return false;
            }
            return true;
        }
        return false;
    }

    // ------------------------------------------------------------------ float text, inf/nan

    @Test
    public void testISS0563_FloatsPrintInSwiShortestForm() {
        assertEquals("10000000.0", output("X is 1.0e7, write(X)"));
        assertEquals("1790168689.01", output("X is 1790168689.01, write(X)"));
        assertEquals("100000000000000.0", output("X is 10.0**14, write(X)"));
        assertEquals("1.0e15", output("X is 10.0**15, write(X)"));
        assertEquals("0.0001", output("X is 0.0001, write(X)"));
        assertEquals("1.0e-5", output("X is 0.00001, write(X)"));
        assertEquals("0.1", output("X is 0.1, write(X)"));
        assertEquals("-2.5", output("X is -2.5, write(X)"));
        assertEquals("1.5e20", output("X is 1.5e20, write(X)"));
        // every printed float reads back to the same double
        ok("forall(member(F, [1.0e7, 1790168689.0123, 0.1, 1.0e15, 123456789012345.6, 5.0e-324, 1.7976931348623157e308]),"
            + " (number_codes(F, C), number_codes(G, C), F =:= G, float(G)))");
        ok("get_time(T), number_codes(T, C), \\+ memberchk(0'e, C)");
    }

    @Test
    public void testISS0564_InfAndNanReadBack() {
        assertEquals("1.0Inf", output("X is inf, writeq(X)"));
        assertEquals("-1.0Inf", output("X is -inf, writeq(X)"));
        assertEquals("1.5NaN", output("X is nan, writeq(X)"));
        assertEquals("inf", output("X is inf, write(X)"));
        ok("X = 1.0Inf, float(X), X =:= inf");
        ok("X = -1.0Inf, X < 0, float(X)");
        ok("X = 1.5NaN, float(X)");
        ok("X is inf, with_output_to(atom(A), writeq(X)), term_to_atom(Y, A), Y =:= inf");
    }

    // ------------------------------------------------------------------ CLI error line

    @Test
    public void testISS0565_CliErrorLineUsesTheWriter() throws Exception {
        String in = "foo_undefined_p3.\nX is foo+1.\n";
        PrintStream oldOut = System.out, oldErr = System.err;
        java.io.InputStream oldIn = System.in;
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        try {
            System.setIn(new java.io.ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8)));
            System.setOut(new PrintStream(new ByteArrayOutputStream(), true));
            System.setErr(new PrintStream(err, true));
            it.denzosoft.jprolog.PrologCLI.main(new String[] {"--batch"});
        } finally {
            System.setIn(oldIn);
            System.setOut(oldOut);
            System.setErr(oldErr);
        }
        String e = err.toString();
        assertTrue(e, e.contains("existence_error(procedure, foo_undefined_p3/0)"));
        assertTrue(e, e.contains("type_error(evaluable, foo/0)"));
        assertFalse(e, e.contains("/(foo"));
    }

    // ------------------------------------------------------------------ P3.4 readers

    @Test
    public void testISS0566_ReadersUseTheV2Parser() {
        ok("open_string(\"'a''b'. f(-). [-]. -(-). \\\"a\\\"\\\"b\\\". 0'''. {}. f(===>). 0'. . end.\", S),"
            + " read(S, A), read(S, B), read(S, C), read(S, D), read(S, E), read(S, F), read(S, G),"
            + " catch(read(S, H), _, true), read(S, I), read(S, J), read(S, K), close(S),"
            + " A == 'a''b', B == f(-), C == [-], D == -(-), string(E), string_codes(E, [97,34,98]),"
            + " F == 39, G == {}, I == 46, J == end, K == end_of_file");
        // user operators are visible, and syntax errors are ISO error terms
        ok("op(700, xfx, ===>)");
        ok("term_to_atom(T, 'a ===> b'), T == (a ===> b)");
        ok("open_string(\"x ===> y.\", S), read(S, T), close(S), T = ===>(x, y)");
        err("term_to_atom(_, 'foo(')", "syntax_error(_)");
        err("open_string(\"a b. c.\", S), read(S, _)", "syntax_error(_)");
        ok("open_string(\"a b. c.\", S), catch(read(S, _), _, true), read(S, Y), close(S), Y == c");
        no("open_string(\"a b.\", S), read_term(S, _, [syntax_errors(fail)])");
        // read_term options
        ok("open_string(\"a(X,Y,_Z,X,_).\", S), read_term(S, T, [variable_names(V), singletons(Sg), variables(Vs)]),"
            + " close(S), T = a(P, Q, R, P, _), V == ['X'=P, 'Y'=Q, '_Z'=R], Sg == ['Y'=Q], length(Vs, 4)");
        ok("open_string(\"f(\\\"ab\\\").\", S), read_term(S, T, [double_quotes(codes)]), close(S), T == f([97,98])");
        err("read_term(user_input, _, [bogus(1)])", "domain_error(read_option, bogus(1))");
        // a read term's variables are fresh cells: two reads never alias on the assert path
        ok("open_string(\"f(X). f(X).\", S), read(S, A), read(S, B), close(S), A = f(P), B = f(Q), P \\== Q");
        // backquotes read as a code list (SWI back_quotes=codes)
        ok("X = `ab`, X == [97, 98]");
        ok("read_term_from_atom('g(A, B, A)', T, []), T = g(X, Y, Z), X == Z, X \\== Y");
    }

    @Test
    public void testISS0567_ReadStreamArgumentErrorsAreIso() {
        err("read(foo_no_stream, _)", "existence_error(stream, foo_no_stream)");
        err("read(f(x), _)", "domain_error(stream_or_alias, f(x))");
        err("read(_, _)", "instantiation_error");
        err("read_term(no_such_stream_p3, _, [])", "existence_error(stream, no_such_stream_p3)");
        err("read_term(user_output, _, [])", "permission_error(input, stream, user_output)");
    }

    @Test
    public void testISS0568_TextToTermOnTheV2Parser() {
        ok("atom_to_term('f(X, _Y, _, X)', T, B), T = f(P, Q, _, P2), P == P2, B == ['X'=P, '_Y'=Q]");
        ok("term_to_atom(T, '{}'), T == {}");
        ok("term_string(T, \"'a''b'\"), T == 'a''b'");
        ok("term_to_atom(f(X, 'A b', \"s\", [1|T]), A), A == 'f(_123,\\'A b\\',\"s\",[1|_456])' ; true");
        ok("term_to_atom(T, '- (1)'), T == -(1)");
        ok("term_to_atom(T, '-(-(1))'), T = -(X), X == -(1)");
    }

    // ------------------------------------------------------------------ P3.5 string streams

    @Test
    public void testISS0569_InMemoryInput() {
        ok("open_string(\"line one\\nline two\\n\", S), read_line_to_string(S, A), read_line_to_codes(S, B),"
            + " read_line_to_string(S, C), close(S), A == \"line one\", atom_codes('line two', B), C == end_of_file");
        ok("open_string(\"x\\n\", S), read_line_to_codes(S, L, T), T = [], close(S), L == [120, 10]");
        ok("open_string(\"\", S), read_line_to_codes(S, L), close(S), L == -1");
        ok("open_string(\"hello world\", S), read_string(S, 5, A), read_string(S, N, B), close(S),"
            + " A == \"hello\", B == \" world\", N == 6");
        ok("open_string(\"  a , b\", S), read_string(S, \",\", \" \", Sep, Str), read_string(S, \",\", \" \", Sep2, Str2),"
            + " close(S), Sep == 0',, Str == \"a\", Sep2 == -1, Str2 == \"b\"");
        ok("with_input_from(atom('t(1). t(2).'), (read(X), read(Y))), X == t(1), Y == t(2)");
        ok("with_input_from(codes([0'a]), get_char(C)), C == a");
        ok("open_string(abc, S), get_char(S, C), close(S), C == a");
        ok("open_string([0'q], S), read_term(S, T, []), close(S), T == q");
        err("with_input_from(nonsense, true)", "domain_error(input_source, nonsense)");
        err("open_string(_, _)", "instantiation_error");
    }

    // ------------------------------------------------------------------ P3.7 listing

    @Test
    public void testISS0570_ListingIsReReadable() {
        prolog.consult(":- dynamic counter/1.\ncounter(0).\n"
            + "lst(X, Y) :- ( X > 0 -> Y = pos ; X < 0 -> Y = neg ; Y = zero ), \\+ Y == none, write('it''s').\n"
            + "lst('A b', 'it''s').\n"
            + "lst([a, \"str\", -(1), - a, 1-(-), 'X'], {x, y}).\n"
            + "single(X, _Y, Z) :- Z = X, (a ; b ; c).\n");
        String l = prolog.getListingOutput("lst/2");
        assertTrue(l, l.contains("lst('A b', 'it''s')."));
        assertTrue(l, l.contains("lst(A, B) :-\n    (   A>0\n    ->  B=pos\n    ;   A<0\n    ->  B=neg\n    ;   B=zero\n    ),"));
        assertFalse(l, l.contains("_G"));
        String c = prolog.getListingOutput("counter/1");
        assertTrue(c, c.startsWith(":- dynamic counter/1.\n\ncounter(0).\n"));
        String s = prolog.getListingOutput("single/3");
        assertTrue(s, s.contains("single(A, _, B) :-"));
        // the whole listing consults back to variant clauses
        String all = prolog.getListingOutput();
        Prolog other = new Prolog();
        other.consult(all);
        for (String q : new String[] {"lst(A, B)", "counter(A)", "single(A, B, C)"}) {
            List<Map<String, Term>> r = other.solve("findall(" + q + "-Body, clause(" + q + ", Body), L1).");
            List<Map<String, Term>> r0 = prolog.solve("findall(" + q + "-Body, clause(" + q + ", Body), L1).");
            String a = Writer.format(r.get(0).get("L1"), Writer.Options.canonical());
            String b = Writer.format(r0.get(0).get("L1"), Writer.Options.canonical());
            assertEquals(b.replaceAll("_G\\d+", "_"), a.replaceAll("_G\\d+", "_"));
        }
        assertEquals("p(A, B) :-\n    q(A),\n    r(B, _).\n", output("portray_clause((p(X,Y) :- q(X), r(Y, _)))"));
    }

    // ------------------------------------------------------------------ P3.11 DCG / expansion

    @Test
    public void testISS0571_DcgTranslateRuleAndExpandTerm() {
        ok("dcg_translate_rule((a --> \\+ b, !, [c]), (H :- B)), H = a(S0, S),"
            + " B = ((\\+ b(S0, _), S0 = S1), (!, S1 = S2), S2 = [c|S])");
        ok("dcg_translate_rule(((a, [p]) --> b), (H :- B)), H = a(S0, S), B = (b(S0, S1), S = [p|S1])");
        ok("expand_term((x --> y), C), C = (x(S0, S) :- y(S0, S))");
        ok("expand_term(foo, C), C == foo");
        prolog.consult("term_expansion(double(X), [X, X]).\nterm_expansion(gone(_), []).\n"
            + "double(fact(1)).\ngone(x).\n");
        ok("findall(X, fact(X), L), L == [1, 1]");
        ok("catch(gone(_), error(existence_error(procedure, _), _), true)");
        ok("expand_term(double(q), C), C == [q, q]");
    }

    // ------------------------------------------------------------------ P3.3 table directives

    @Test
    public void testISS0572_TableDirectiveForms() {
        prolog.consult(":- table ev/1, od/1.\n"
            + "ev(0).\nev(N) :- od(M), M < 20, N is M + 1.\nod(N) :- ev(M), M < 20, N is M + 1.\n"
            + ":- table([conn/2]).\n"
            + "conn(X, Y) :- conn(X, Z), e(Z, Y).\nconn(X, Y) :- e(X, Y).\n"
            + "e(a, b). e(b, c). e(c, a).\n"
            + ":- table sp(_, _, min).\n"
            + "sp(X, Y, D) :- w(X, Y, D).\n"
            + "sp(X, Y, D) :- sp(X, Z, D1), w(Z, Y, D2), D is D1 + D2.\n"
            + "w(a, b, 5). w(a, c, 1). w(c, b, 1). w(b, d, 1). w(d, a, 1).\n"
            + ":- table lp(_, max).\nlp(X, L) :- w(X, _, L).\n"
            + ":- table fl(_, first).\nfl(k, 1). fl(k, 2).\n"
            + ":- table fl2(_, last).\nfl2(k, 1). fl2(k, 2).\n"
            + ":- table(as(p3, subsumptive)).\np3.\n");
        ok("findall(X, ev(X), L), length(L, 11)");
        ok("findall(Y, conn(a, Y), L), msort(L, [a, b, c])");
        ok("findall(Y-D, sp(a, Y, D), L), msort(L, [a-4, b-2, c-1, d-3])");
        ok("lp(a, 5)");
        ok("findall(V, fl(k, V), [1])");
        ok("findall(V, fl2(k, V), [2])");
        ok("predicate_property(sp(_, _, _), tabled)");
        ok("p3");
        err("table(q(_, lattice(j/3)))", "domain_error(table_mode, lattice(j/3))");
        try {
            new Prolog().consult(":- table z(_, po(foo/2)).\nz(1, 2).\n");
            fail("an unsupported table mode must be reported");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage(), e.getMessage().contains("table_mode"));
        }
    }

    // ------------------------------------------------------------------ P3.2 module scope

    @Test
    public void testISS0573_ModuleScopeEndsWithTheLoad() {
        prolog.consult(":- module(m1p3, [p1/1]).\np1(X) :- q1(X).\nq1(one).\n");
        prolog.consult(":- module(m2p3, [p2/1]).\np2(two).\n");
        ok("p1(X), X == one");               // m1's export imported into user
        ok("p2(X), X == two");
        err("q1(_)", "existence_error(procedure, q1/1)");
        prolog.consult("plain_p3(1).\n");     // a plain file after a module file stays in user
        ok("plain_p3(1)");
        ok("predicate_property(plain_p3(_), number_of_clauses(1))");
        no("catch(m2p3:plain_p3(1), _, fail), predicate_property(m2p3:plain_p3(_), defined_in(m2p3))");
        ok("current_module(user)");
        assertEquals("user", prolog.getModuleManager().getCurrentModule().getName());
    }

    // ------------------------------------------------------------------ P3.1 loading files

    @Test
    public void testISS0574_ConsultEnsureLoadedLoadFiles() throws Exception {
        file("sub/helper.pl", "helper(ok).\n:- prolog_load_context(file, F), assertz(helper_file(F)).\n");
        file("m1.pl", ":- module(m1p3f, [pm/1]).\npm(X) :- qm(X).\nqm(from_m1).\n");
        file("plain.pl", "plain(1).\n");
        File main = file("main.pl", ":- consult(sub/helper).\n:- ensure_loaded('sub/helper').\n"
            + ":- use_module(m1).\nmain_fact(1). main_fact(2).\n"
            + ":- initialization(assertz(init_ran)).\n"
            + ":- prolog_load_context(directory, D), assertz(loaded_dir(D)).\n");
        ok("consult('" + path(main) + "')");
        ok("findall(X, main_fact(X), [1, 2])");
        ok("helper(ok), findall(F, helper_file(F), [_])");    // ensure_loaded did not reload it
        ok("pm(X), X == from_m1");
        ok("init_ran");
        ok("loaded_dir(D), atom(D)");
        ok("source_file(F), sub_atom(F, _, _, 0, 'main.pl')");
        ok("source_file(main_fact(_), F), sub_atom(F, _, _, 0, 'main.pl')");
        String d = path(dir);
        ok("['" + d + "/plain', '" + d + "/plain.pl'], findall(X, plain(X), [1])");   // reconsult, no duplicate
        ok("consult('" + d + "/plain'), findall(X, plain(X), [1])");
        ok("load_files(['" + d + "/plain'], [if(not_loaded)]), findall(X, plain(X), [1])");
        ok("load_files('" + d + "/plain', [if(changed)])");
        err("consult(no_such_file_p3)", "existence_error(source_sink, no_such_file_p3)");
        err("consult(_)", "instantiation_error");
        err("load_files('" + d + "/plain', [must_be_module(true)])", "domain_error(module_file, _)");
        ok("ensure_loaded(library(lists))");
        err("consult(library(no_such_lib_p3))", "existence_error(source_sink, library(no_such_lib_p3))");
        no("prolog_load_context(module, _)");                  // not loading
        // Java API: relative consult from inside a loaded file, and the load count
        Prolog.LoadResult r = prolog.loadFile(path(file("count.pl", "a(1). a(2). a(3).\n")));
        assertEquals(3, r.clauses);
        // safe mode strips the loader (host file access)
        Prolog safe = new Prolog();
        safe.enableSafeMode();
        List<Map<String, Term>> s = safe.solve("catch(consult('" + path(main) + "'), error(E, _), true).");
        assertEquals("existence_error(procedure,consult/1)", Writer.format(s.get(0).get("E"), Writer.Options.writeq()).replace(" ", ""));
    }

    @Test
    public void testISS0575_IncludeIsTextual() throws Exception {
        file("inc/part.pl", "included(yes).\nin_module_check :- true.\n");
        File main = file("inc_main.pl", ":- module(incp3, [included/1]).\n:- include('inc/part').\nafter(1).\n");
        ok("consult('" + path(main) + "')");
        ok("included(yes)");                               // included clauses belong to the module
        ok("incp3:after(1)");
        err("after(1)", "existence_error(procedure, after/1)");
        try {
            new Prolog().consult(":- include(no_such_include_p3).\nx.\n");
            fail("a missing include is an error");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage(), e.getMessage().contains("existence_error"));
        }
    }

    @Test
    public void testISS0576_SourceFileAndLoadContext() throws Exception {
        File f = file("ctx.pl", ":- prolog_load_context(module, M), assertz(ctx_mod(M)).\n"
            + ":- prolog_load_context(source, S), assertz(ctx_src(S)).\n"
            + ":- findall(K, prolog_load_context(K, _), Ks), assertz(ctx_keys(Ks)).\ndef(1).\n");
        prolog.loadFile(path(f));
        ok("ctx_mod(user)");
        ok("ctx_src(S), sub_atom(S, _, _, 0, 'ctx.pl')");
        ok("ctx_keys(Ks), memberchk(directory, Ks), memberchk(file, Ks), memberchk(source, Ks)");
        ok("source_file(def(_), F), sub_atom(F, _, _, 0, 'ctx.pl')");
        no("source_file(nothing_p3(_), _)");
    }

    // ------------------------------------------------------------------ P3.6 .jpc

    @Test
    public void testISS0577_JpcCompilesThroughTheV2Reader() throws Exception {
        String src = ":- op(700, xfx, ===>).\n:- dynamic dyn/1.\nr1(a ===> b).\nq('a''b').\nbq(`ab`).\n"
            + "e({}).\ne2('{}'(x)).\nsym(===>).\ng --> [x], g2.\ng2 --> [].\n"
            + ":- initialization(assertz(jpc_init)).\n";
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        new Prolog().compile(src, bos);
        Prolog b = new Prolog();
        b.consultCompiled(new java.io.ByteArrayInputStream(bos.toByteArray()));
        List<Map<String, Term>> s = b.solve("r1(X), X == (a ===> b), q('a''b'), bq([97, 98]), e(E), E == {},"
            + " e2(E2), E2 == {x}, sym(===>), phrase(g, [x]), jpc_init, predicate_property(dyn(_), dynamic).");
        assertFalse(s.isEmpty());
    }

    /** P3.6: consulting every examples/*.pl and loading its .jpc give the same clauses. */
    @Test
    public void testISS0577_ConsultAndJpcAreEquivalentOnEveryExample() throws Exception {
        File ex = new File("examples");
        File[] files = ex.listFiles((d, n) -> n.endsWith(".pl"));
        assertNotNull(files);
        Arrays.sort(files);
        int compared = 0;
        PrintStream oldOut = System.out, oldErr = System.err;
        try {
            PrintStream sink = new PrintStream(new ByteArrayOutputStream(), true);
            System.setOut(sink);
            System.setErr(sink);
            for (File f : files) {
                String text = new String(Files.readAllBytes(f.toPath()), StandardCharsets.UTF_8);
                if (text.contains("halt") || text.contains("read(") || text.contains("initialization(main")
                        || text.contains("http") || text.contains("thread") || text.contains("random")
                        || text.contains("get_time") || text.contains("tcp_")) continue;   // run-dependent
                String a = loadAndList(text, false);
                String b = loadAndList(text, true);
                if (a == null || b == null) { assertEquals(f.getName(), a, b); continue; }
                assertEquals(f.getName(), a, b);
                compared++;
            }
        } finally {
            System.setOut(oldOut);
            System.setErr(oldErr);
        }
        assertTrue("compared " + compared, compared >= 20);
    }

    private static String loadAndList(String text, boolean viaJpc) {
        Prolog p = new Prolog();
        ByteArrayOutputStream cap = new ByteArrayOutputStream();
        PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(new PrintStream(cap, true));
        try {
            if (viaJpc) {
                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                try {
                    new Prolog().compile(text, bos);
                } catch (java.io.IOException e) {
                    return "PARSE-ERROR";
                }
                try { p.consultCompiled(new java.io.ByteArrayInputStream(bos.toByteArray())); }
                catch (RuntimeException | java.io.IOException e) { /* clause errors: compare what loaded */ }
            } else {
                try {
                    it.denzosoft.jprolog.core.parser.v2.TermReader.parseProgram(text, p.getOps().table());
                } catch (RuntimeException e) {
                    return "PARSE-ERROR";
                }
                try { p.consult(text); } catch (RuntimeException e) { /* same */ }
            }
        } finally {
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        return p.getListingOutput();
    }

    // ------------------------------------------------------------------ CLI clause count

    @Test
    public void testISS0578_CliCountsClausesNotLines() throws Exception {
        File f = file("two.pl", "a(1). a(2).\nb :-\n    a(1).\n% a comment ending in a dot.\n");
        String in = ":consult " + path(f) + "\n";
        PrintStream oldOut = System.out, oldErr = System.err;
        java.io.InputStream oldIn = System.in;
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try {
            System.setIn(new java.io.ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8)));
            System.setOut(new PrintStream(out, true));
            System.setErr(new PrintStream(new ByteArrayOutputStream(), true));
            it.denzosoft.jprolog.PrologCLI.main(new String[] {"--batch"});
        } finally {
            System.setIn(oldIn);
            System.setOut(oldOut);
            System.setErr(oldErr);
        }
        assertTrue(out.toString(), out.toString().contains("File loaded: 3 clauses loaded, 0 errors"));
    }
}
