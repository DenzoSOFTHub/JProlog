package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0475/0476 - engine v4 wave W7 acceptance: the writer and answer printing
/**
 * Wave W7 acceptance for {@code core.engine.v4.Writer} (design B.12) and for the console's answer
 * format (design decision 5 of B.17, limit <b>L-11</b>).
 *
 * <p>The writer is engine-neutral — {@code write/1}, {@code writeq/1}, {@code print/1},
 * {@code write_canonical/1}, {@code write_term/2,3} and {@code format ~w/~q/~p} route through it on
 * both engines — so most tests run under whichever engine the suite selected. The residual-goal
 * tests select v4 explicitly (attributed variables live in the answer's cells there) and restore the
 * previous selection in {@code tearDown}.
 */
public class EngineV4WriterTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    @After
    public void tearDown() {
    }

    /** Render {@code goal}'s output by capturing it into an atom. */
    private String out(String goal) {
        List<Map<String, Term>> r = prolog.solve("with_output_to(atom(A), (" + goal + "))");
        assertFalse("goal must succeed: " + goal, r.isEmpty());
        return r.get(0).get("A").toString();
    }

    // ==================================================================
    // the write_term option matrix
    // ==================================================================

    @Test
    public void testISS0475_QuotedAtoms() {
        assertEquals("'hello world'", out("write_term('hello world', [quoted(true)])"));
        assertEquals("hello world", out("write_term('hello world', [quoted(false)])"));
        assertEquals("'\\n'", out("write_term('\\n', [quoted(true)])"));
        assertEquals("'42'", out("write_term('42', [quoted(true)])"));
        assertEquals("[]", out("write_term([], [quoted(true)])"));
        assertEquals("!", out("write_term(!, [quoted(true)])"));
        assertEquals(";", out("write_term(;, [quoted(true)])"));
        assertEquals("'don''t'", out("write_term('don\\'t', [quoted(true)])"));
    }

    @Test
    public void testISS0475_OperatorsSpacingAndParentheses() {
        assertEquals("1-2-3", out("write_term(1-2-3, [])"));
        assertEquals("1-(2-3)", out("write_term(1-(2-3), [])"));
        assertEquals("a- -1", out("write_term(a-(-1), [])"));
        assertEquals("-(1)", out("write_term(-(1), [])"));   // ISS-2025-0562: SWI's +/-(Number)
        assertEquals("-a", out("write_term(-(a), [])"));
        assertEquals("f(:-)", out("write_term(f(:-), [quoted(true)])"));
        assertEquals("a,b", out("write_term(','(a,b), [])"));
        assertEquals("1+2*3", out("write_term(1+2*3, [])"));
        assertEquals("(1+2)*3", out("write_term((1+2)*3, [])"));
        assertEquals("1 is 2", out("write_term(is(1,2), [])"));
    }

    @Test
    public void testISS0475_ListsAndCurlies() {
        assertEquals("[a|b]", out("write_term([a|b], [])"));
        assertEquals("[a,b,c]", out("write_term([a,b,c], [])"));
        assertEquals("{a,b}", out("write_term({a,b}, [])"));
        assertEquals("[]", out("write_term([], [])"));
    }

    @Test
    public void testISS0475_IgnoreOpsIsFunctionalNotation() {
        assertEquals("-(1,2)", out("write_term(1-2, [ignore_ops(true)])"));
        assertEquals("+(1,*(2,3))", out("write_term(1+2*3, [ignore_ops(true)])"));
        assertEquals("+(1,2)", out("write_canonical(1+2)"));
    }

    @Test
    public void testISS0475_NumbervarsAndVariableNames() {
        assertEquals("B", out("write_term('$VAR'(1), [numbervars(true)])"));
        assertEquals("$VAR(1)", out("write_term('$VAR'(1), [numbervars(false)])"));
        assertEquals("A1", out("write_term('$VAR'(26), [numbervars(true)])"));
        assertEquals("f(Foo,Bar)",
            out("write_term(f(X,Y), [variable_names(['Foo'=X,'Bar'=Y])])"));
    }

    @Test
    public void testISS0475_MaxDepth() {
        assertEquals("[1,2|...]", out("write_term([1,2,3,4,5], [max_depth(3)])"));
        assertEquals("f(f(...))", out("write_term(f(f(f(f(a)))), [max_depth(3)])"));
        assertEquals("[1,2,3,4,5]", out("write_term([1,2,3,4,5], [max_depth(0)])"));
    }

    @Test
    public void testISS0475_SpacingNextArgument() {
        assertEquals("f(a, b, c)", out("write_term(f(a,b,c), [spacing(next_argument)])"));
        assertEquals("f(a,b,c)", out("write_term(f(a,b,c), [spacing(standard)])"));
        assertEquals("[a, b]", out("write_term([a,b], [spacing(next_argument)])"));
    }

    @Test
    public void testISS0475_Strings() {
        assertEquals("\"abc\"", out("X = \"abc\", write_term(X, [quoted(true)])"));
    }

    @Test
    public void testISS0475_UnknownOptionIsADomainError() {
        List<Map<String, Term>> r = prolog.solve(
            "catch(write_term(a, [frobnicate(true)]), error(E, _), true)");
        assertFalse(r.isEmpty());
        assertEquals("domain_error(write_option, frobnicate(true))", r.get(0).get("E").toString());
    }

    @Test
    public void testISS0475_Portray() {
        prolog.consult("portray(secret(_)) :- write('<hidden>').\n");
        assertEquals("<hidden>", out("print(secret(42))"));
        assertEquals("f(<hidden>)", out("print(f(secret(1)))"));
        assertEquals("secret(42)", out("write(secret(42))"));
        assertEquals("<hidden>", out("write_term(secret(42), [portray(true)])"));
    }

    // ==================================================================
    // cycles and size
    // ==================================================================

    @Test
    public void testISS0475_CyclicTermPrintingTerminates() {
        Prolog v4 = new Prolog();                        // ISS-2025-0491: one engine
        List<Map<String, Term>> r = v4.solve("X = f(X), with_output_to(atom(A), write(X))");
        assertFalse(r.isEmpty());
        assertEquals("f(...)", r.get(0).get("A").toString());

        r = v4.solve("X = [1|X], with_output_to(atom(A), write(X))");
        assertFalse(r.isEmpty());
        assertEquals("[1|...]", r.get(0).get("A").toString());

        r = v4.solve("X = f(X), with_output_to(atom(A), write_term(X, [cycles(true), quoted(true)]))");
        assertFalse(r.isEmpty());
        assertEquals("@(_S1,[_S1=f(_S1)])", r.get(0).get("A").toString());
    }

    /** A million-element list must print at the DEFAULT JVM stack (no {@code -Xss}). */
    @Test(timeout = 120000)
    public void testISS0475_MillionElementListPrintsAtTheDefaultStack() {
        List<Map<String, Term>> r = prolog.solve(
            "numlist(1, 1000000, L), with_output_to(atom(A), write(L)), atom_length(A, N)");
        assertFalse(r.isEmpty());
        // "[1,2,...,1000000]" — 6 888 897 characters
        assertEquals("6888897", r.get(0).get("N").toString());
    }

    /** A deep last-argument spine must not recurse either. */
    @Test(timeout = 120000)
    public void testISS0475_DeepLastArgumentSpinePrintsAtTheDefaultStack() {
        Term t = new Atom("a");
        for (int i = 0; i < 200000; i++) {
            t = new CompoundTerm(new Atom("f"), Arrays.asList((Term) new Atom("x"), t));
        }
        String s = Writer.format(t, Writer.Options.write());
        assertEquals(200000 * 5 + 1, s.length());        // "f(x," + ")" per level, plus the final "a"
    }

    // ==================================================================
    // portray_clause / print_message
    // ==================================================================

    @Test
    public void testISS0475_PortrayClause() {
        assertEquals("a :-\n    b,\n    c.\n", out("portray_clause((a :- b, c))"));
        assertEquals("fact.\n", out("portray_clause(fact)"));
        // ISS-2025-0570 (P3.7): SWI argument spacing
        assertEquals("p(A, B) :-\n    q(A),\n    r(B).\n", out("portray_clause((p(X,Y) :- q(X), r(Y)))"));
        assertEquals("'my atom'.\n", out("portray_clause('my atom')"));
    }

    @Test
    public void testISS0475_PrintMessageRendersAnIsoErrorReadably() {
        String s = Writer.message("error",
            new CompoundTerm(new Atom("error"), Arrays.<Term>asList(
                new CompoundTerm(new Atom("type_error"), Arrays.<Term>asList(
                    new Atom("integer"), new Atom("abc"))),
                new CompoundTerm(new Atom("/"), Arrays.<Term>asList(
                    new Atom("foo"), new it.denzosoft.jprolog.core.terms.Number(1L))))));
        assertEquals("ERROR: Type error: `integer' expected, found `abc' (foo/1)", s);
        assertTrue(Writer.message("error",
            new CompoundTerm(new Atom("error"), Arrays.<Term>asList(
                new Atom("instantiation_error"),
                new Atom("x")))).contains("not sufficiently instantiated"));
        // print_message/2 succeeds and prints nothing for kind `silent`
        assertEquals("", out("print_message(silent, hello)"));
        // ISS-2025-0607 (P4.14): a term that is not a known message is "Unknown message: T" (SWI);
        // format(F, A) is formatted
        assertEquals("% Unknown message: hello\n", out("print_message(informational, hello)"));
        assertEquals("% hello\n", out("print_message(informational, format('hello', []))"));
    }

    // ==================================================================
    // current_op/3 sees an operator a consulted file declared (LIM-034)
    // ==================================================================

    @Test
    public void testISS0474_CurrentOpSeesAConsultedOpDirective() {
        assertTrue("not defined yet", prolog.solve("current_op(_, xfx, consulted_op)").isEmpty());
        prolog.consult(":- op(699, xfx, consulted_op).\n");
        List<Map<String, Term>> r = prolog.solve("current_op(P, T, consulted_op)");
        assertFalse("current_op/3 must see an operator declared by a consulted `:- op/3`", r.isEmpty());
        assertEquals("699", r.get(0).get("P").toString());
        assertEquals("xfx", r.get(0).get("T").toString());
        // ... and the writer uses it
        assertEquals("a consulted_op b", out("write_term(consulted_op(a,b), [])"));
    }

    @Test
    public void testISS0474_OpThreeAndCurrentOpShareOneStore() {
        prolog.solve("op(650, xfy, v4writer_op)");
        assertFalse(prolog.solve("current_op(650, xfy, v4writer_op)").isEmpty());
        assertEquals("a v4writer_op b", out("write_term(v4writer_op(a,b), [])"));
        prolog.solve("op(0, xfy, v4writer_op)");
        assertTrue(prolog.solve("current_op(_, xfy, v4writer_op)").isEmpty());
        assertEquals("v4writer_op(a,b)", out("write_term(v4writer_op(a,b), [])"));
    }

    // ==================================================================
    // the console answer format (design decision 5, limit L-11)
    // ==================================================================

    private String answer(String query) {
        List<Map<String, Term>> r = prolog.solve(query);
        if (r.isEmpty()) return "false.";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < r.size(); i++) {
            List<String> lines = Answer.lines(r.get(i), prolog.residualGoals(r.get(i)));
            if (lines.isEmpty()) sb.append("true");
            for (int k = 0; k < lines.size(); k++) {
                if (k > 0) sb.append(",\n");
                sb.append(lines.get(k));
            }
            sb.append(i < r.size() - 1 ? " ;\n" : ".");
        }
        return sb.toString();
    }

    @Test
    public void testISS0476_AnswersPrintInQuotedOperatorNotation() {
        assertEquals("X = 'a b'-1.", answer("X = 'a b'-1"));
        assertEquals("PI = foo/1.", answer("PI = foo/1"));
        assertEquals("Body = (p,q).", answer("Body = (p,q)"));
        assertEquals("A = '42'.", answer("A = '42'"));
        assertEquals("X = 'hello world'.", answer("X = 'hello world'"));
        assertEquals("X = [1,2,3].", answer("X = [1,2,3]"));
        assertEquals("true.", answer("true"));
        assertEquals("false.", answer("fail"));
    }

    @Test
    public void testISS0476_FreshVariablesPrintAsUnderscoreLetters() {
        assertEquals("X = f(Y).", answer("X = f(Y)"));
        String a = answer("X = f(_)");
        assertTrue("a fresh variable must print as _A, got: " + a, a.startsWith("X = f(_"));
        assertFalse("no internal name may leak", a.contains("_G"));
        assertFalse(a.contains("_R"));
    }

    @Test
    public void testISS0476_ResidualGoalsArePrintedAfterTheBindings() {
        Prolog v4 = new Prolog();                        // ISS-2025-0491: one engine
        List<Map<String, Term>> r = v4.solve("freeze(X, writeln(hi))");
        assertFalse(r.isEmpty());
        List<String> lines = Answer.lines(r.get(0), v4.residualGoals(r.get(0)));
        assertEquals(1, lines.size());
        assertEquals("freeze(X,writeln(hi))", lines.get(0));

        r = v4.solve("dif(X, a)");
        lines = Answer.lines(r.get(0), v4.residualGoals(r.get(0)));
        assertEquals("dif(X,a)", lines.get(0));

        r = v4.solve("X in 1..3");
        lines = Answer.lines(r.get(0), v4.residualGoals(r.get(0)));
        assertEquals("X in 1..3", lines.get(0));
    }

    /** The CLI itself, driven end to end. */
    @Test
    public void testISS0476_CliPrintsTheNewAnswerFormat() throws Exception {
        String out = runCli("X = 'a b'-1.\nPI = foo/1.\nBody = (p,q).\nA = '42'.\nnosuchthing.\ntrue.\n");
        assertTrue("quoted operator notation, got:\n" + out, out.contains("X = 'a b'-1."));
        assertTrue(out.contains("PI = foo/1."));
        assertTrue(out.contains("Body = (p,q)."));
        assertTrue(out.contains("A = '42'."));
        assertTrue(out.contains("true."));
        assertFalse("no canonical notation may survive", out.contains("-(a b, 1)"));
    }

    private static String runCli(String input) throws Exception {
        java.io.InputStream oldIn = System.in;
        java.io.PrintStream oldOut = System.out;
        java.io.ByteArrayOutputStream buf = new java.io.ByteArrayOutputStream();
        try {
            System.setIn(new java.io.ByteArrayInputStream(input.getBytes("UTF-8")));
            System.setOut(new java.io.PrintStream(buf, true, "UTF-8"));
            new it.denzosoft.jprolog.PrologCLI().start();
        } finally {
            System.setIn(oldIn);
            System.setOut(oldOut);
        }
        return buf.toString("UTF-8");
    }
}
// END_CHANGE: ISS-2025-0475/0476
