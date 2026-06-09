package it.denzosoft.jprolog.core.parser.v2;

import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

/**
 * Verifies the clean-room v2 parser solves the whole class of parsing bugs that the
 * legacy parser gets wrong (canonical functor, operator-as-atom, 0'c, doubled quotes,
 * postfix operators, quote-aware clause splitting, precedence, negative literals).
 */
public class NewParserTest {

    private final OperatorTable ops = OperatorTable.getDefault();

    private Term term(String s) { return TermReader.parseTerm(s, ops); }

    private static void assertCompound(Term t, String name, int arity) {
        assertTrue("expected compound, got " + t.getClass().getSimpleName() + ": " + t, t instanceof CompoundTerm);
        CompoundTerm c = (CompoundTerm) t;
        assertEquals("functor", name, c.getName());
        assertEquals("arity", arity, c.getArguments().size());
    }
    private static void assertAtom(Term t, String name) {
        assertTrue("expected atom, got " + t.getClass().getSimpleName() + ": " + t, t instanceof Atom);
        assertEquals(name, ((Atom) t).getName());
    }
    private static void assertInt(Term t, long v) {
        assertTrue("expected number: " + t, t instanceof Number);
        assertTrue(((Number) t).isInteger());
        assertEquals(v, ((Number) t).longValue());
    }

    // ---- canonical functor notation for operator symbols ----
    @Test public void canonicalFunctorMinus() {
        Term t = term("-(1,2)");                 // must be -/2, NOT -(','(1,2))
        assertCompound(t, "-", 2);
        assertInt(((CompoundTerm) t).getArguments().get(0), 1);
        assertInt(((CompoundTerm) t).getArguments().get(1), 2);
    }
    @Test public void canonicalFunctorWithSpaceIsPrefix() {
        Term t = term("- (1)");                  // space => prefix minus applied to (1)
        assertCompound(t, "-", 1);
    }

    // ---- operator-as-atom ----
    @Test public void operatorAsAtomEquals() {
        Term t = term("X = -");                  // =(X, '-')
        assertCompound(t, "=", 2);
        assertTrue(((CompoundTerm) t).getArguments().get(0) instanceof Variable);
        assertAtom(((CompoundTerm) t).getArguments().get(1), "-");
    }
    @Test public void operatorAtomsAsArgs() {
        Term t = term("foo(-, +)");
        assertCompound(t, "foo", 2);
        assertAtom(((CompoundTerm) t).getArguments().get(0), "-");
        assertAtom(((CompoundTerm) t).getArguments().get(1), "+");
    }
    @Test public void operatorAtomMidArgs() {
        Term t = term("p(a, -, b)");
        assertCompound(t, "p", 3);
        assertAtom(((CompoundTerm) t).getArguments().get(1), "-");
    }
    @Test public void parenthesisedOperatorAtom() {
        assertAtom(term("(-)"), "-");
    }

    // ---- character-code literals ----
    @Test public void charCodeLiteral() {
        assertInt(term("0'a"), 97);
        assertInt(term("0' "), 32);
        assertInt(term("0'\\n"), 10);
    }

    // ---- doubled-quote escapes ----
    @Test public void doubledQuoteAtom() {
        assertAtom(term("'don''t'"), "don't");
        assertAtom(term("'it''s ok'"), "it's ok");
    }
    @Test public void doubledQuoteString() {
        // The lexer must decode "" inside a double-quoted string to a single " (content a"b),
        // independent of the (global) double_quotes flag that decides codes/chars/atom/string.
        List<Lexer.Token> toks = Lexer.tokenize("\"a\"\"b\"");
        assertEquals(Lexer.Kind.STRING, toks.get(0).kind);
        assertEquals("a\"b", toks.get(0).text);
    }

    // ---- radix + negative literals ----
    @Test public void radixLiterals() {
        assertInt(term("0xFF"), 255);
        assertInt(term("0o17"), 15);
        assertInt(term("0b1010"), 10);
    }
    @Test public void negativeLiteral() {
        Term t = term("-42");                    // adjacent => the integer -42
        assertInt(t, -42);
        assertInt(term("-0xFF"), -255);
    }

    // ---- precedence ----
    @Test public void precedence() {
        Term t = term("1 + 2 * 3");              // +(1, *(2,3))
        assertCompound(t, "+", 2);
        assertCompound(((CompoundTerm) t).getArguments().get(1), "*", 2);
    }
    @Test public void rightAssocComma() {
        Term t = term("a, b, c");                // ','(a, ','(b, c))
        assertCompound(t, ",", 2);
        assertAtom(((CompoundTerm) t).getArguments().get(0), "a");
        assertCompound(((CompoundTerm) t).getArguments().get(1), ",", 2);
    }

    // ---- postfix operators ----
    @Test public void postfixOperator() {
        OperatorTable t = OperatorTable.createEmpty();
        t.defineOperator(200, Operator.Type.YF, "fact");   // e.g. factorial postfix
        Term r = TermReader.parseTerm("5 fact", t);
        assertCompound(r, "fact", 1);
        assertInt(((CompoundTerm) r).getArguments().get(0), 5);
    }

    // ---- lists / braces ----
    @Test public void listWithTail() {
        Term t = term("[a, b | T]");
        assertCompound(t, ".", 2);
        // [a | [b | T]]
        Term rest = ((CompoundTerm) t).getArguments().get(1);
        assertCompound(rest, ".", 2);
        assertTrue(((CompoundTerm) rest).getArguments().get(1) instanceof Variable);
    }
    @Test public void braceTerm() {
        Term t = term("{a, b}");
        assertCompound(t, "{}", 1);
        assertCompound(((CompoundTerm) t).getArguments().get(0), ",", 2);
    }

    // ---- quote-aware clause splitting ----
    @Test public void clauseSplittingIsQuoteAware() {
        // A '.' inside a quoted atom and a 0'. char-code must NOT split clauses.
        List<Term> clauses = TermReader.parseProgram("p('a. b'). q(0'.). r(1).", ops);
        assertEquals(3, clauses.size());
        assertCompound(clauses.get(0), "p", 1);
        assertAtom(((CompoundTerm) clauses.get(0)).getArguments().get(0), "a. b");
        assertInt(((CompoundTerm) clauses.get(1)).getArguments().get(0), 46); // 0'. == '.' code
    }
    @Test public void clauseSplittingRulesAndDirectives() {
        List<Term> clauses = TermReader.parseProgram(
            "foo(X) :- bar(X), baz(X).\n:- initialization(main).\nq.", ops);
        assertEquals(3, clauses.size());
        assertCompound(clauses.get(0), ":-", 2);   // head :- body
        assertCompound(clauses.get(1), ":-", 1);   // directive
        assertAtom(clauses.get(2), "q");
    }

    // ---- shared variables within a clause ----
    @Test public void sharedVariables() {
        Term t = term("f(X, X)");
        CompoundTerm c = (CompoundTerm) t;
        assertSame("same variable name shares one Variable object",
            c.getArguments().get(0), c.getArguments().get(1));
    }

    // ---- comments + layout ----
    @Test public void commentsAndLayout() {
        Term t = term("foo(  /* c */ 1, % line\n 2 )");
        assertCompound(t, "foo", 2);
    }

    // ---- incremental driver executes op/3 directives between clauses ----
    @Test public void incrementalOpDirective() {
        OperatorTable t = OperatorTable.createEmpty();
        t.defineOperator(1200, Operator.Type.XFX, ":-");
        t.defineOperator(1200, Operator.Type.FX, ":-");
        TermReader r = new TermReader(Lexer.tokenize(":- op(700, xfx, ===).\na === b."), t);
        Term d = r.nextClause();                 // the directive
        assertCompound(d, ":-", 1);
        // a consult driver would now EXECUTE op/3; simulate it:
        t.defineOperator(700, Operator.Type.XFX, "===");
        Term c = r.nextClause();                 // a === b  -> now parses as ===(a, b)
        assertCompound(c, "===", 2);
        assertNull(r.nextClause());              // EOF
    }
}
