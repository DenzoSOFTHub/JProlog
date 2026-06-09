package it.denzosoft.jprolog.core.write.v2;

import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.parser.v2.TermReader;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Validates the v2 operator-aware {@link TermWriter}: specific outputs plus a round-trip property
 * (parse → writeq → parse yields a structurally equal term) against the v2 parser.
 */
public class TermWriterTest {

    private final OperatorTable ops = OperatorTable.getDefault();
    private Term parse(String s) { return TermReader.parseTerm(s, ops); }
    private String w(String s) { return TermWriter.write(parse(s)); }
    private String wq(String s) { return TermWriter.writeq(parse(s)); }

    // ---- operator-aware output (the whole point) ----
    @Test public void infixPrecedence() {
        assertEquals("1+2*3", w("1 + 2 * 3"));
        assertEquals("(1+2)*3", w("(1 + 2) * 3"));
        assertEquals("1-2", w("-(1,2)"));                 // canonical functor round-trips as infix
        assertEquals("a:-b,c", w("a :- b, c"));
        assertEquals("X is 1+2", w("X is 1 + 2"));        // alphabetic op gets spaces
        assertEquals("1 mod 2", w("1 mod 2"));
    }

    @Test public void listsAndCurly() {
        assertEquals("[1,2,3]", w("[1, 2, 3]"));
        assertEquals("[a,b|T]", w("[a, b | T]"));
        assertEquals("[]", w("[]"));
        assertEquals("{a,b}", w("{a, b}"));
    }

    @Test public void operatorAtomsAndFunctors() {
        assertEquals("f(a,-,b)", w("f(a, -, b)"));        // '-' written as a plain atom argument
        assertEquals("foo(bar(X),Y)", w("foo(bar(X), Y)"));
        assertEquals("- 1", w("- 1"));                    // prefix minus keeps a space (not the literal -1)
        assertEquals("\\+a", w("\\+ a"));                 // symbolic prefix + alnum arg: no space needed
    }

    @Test public void quoting() {
        assertEquals("hello", wq("hello"));
        assertEquals("'hello world'", wq("'hello world'"));
        assertEquals("'don\\'t'", wq("'don''t'"));
        assertEquals("[]", wq("[]"));
        assertEquals("+", wq("+"));                       // symbolic atom needs no quotes
        assertEquals("'Abc'", wq("'Abc'"));               // uppercase start needs quotes
        // write (non-quoted) leaves atoms bare
        assertEquals("hello world", TermWriter.write(parse("'hello world'")));
    }

    @Test public void canonicalIgnoresOps() {
        String c = TermWriter.write(parse("1 + 2 * 3"), ops, TermWriter.Options.canonical());
        assertEquals("+(1,*(2,3))", c);
    }

    // ---- round-trip: parse -> writeq -> parse  is structure-preserving ----
    @Test public void roundTrip() {
        String[] cases = {
            "1+2*3-4", "f(a,b,c)", "[1,2,3]", "[a,b|c]", "{x,y}", "a:-b,c",
            "-(1,2)", "foo(-,+)", "1 mod 2 + 3", "g(h(i(j)))",
            "p(X,Y,X)", "a=b;c=d", "(a->b;c)", "[1,2|[3,4]]", "- 1", "\\+ member(X,L)",
        };
        for (String src : cases) {
            Term t1 = parse(src);
            String out = TermWriter.writeq(t1);
            Term t2 = parse(out);
            assertTrue("round-trip changed structure for '" + src + "' -> '" + out + "'",
                structurallyEqual(t1, t2));
        }
    }

    /** Structural equality ignoring variable names (round-trip renames variables). */
    private static boolean structurallyEqual(Term a, Term b) {
        if (a instanceof Variable) return b instanceof Variable;
        if (a instanceof Number && b instanceof Number) {
            Number na = (Number) a, nb = (Number) b;
            return na.isInteger() == nb.isInteger()
                && (na.isInteger() ? na.bigIntegerValue().equals(nb.bigIntegerValue())
                                   : na.doubleValue() == nb.doubleValue());
        }
        if (a instanceof Atom && b instanceof Atom) return ((Atom) a).getName().equals(((Atom) b).getName());
        if (a instanceof CompoundTerm && b instanceof CompoundTerm) {
            CompoundTerm ca = (CompoundTerm) a, cb = (CompoundTerm) b;
            if (!ca.getName().equals(cb.getName())) return false;
            if (ca.getArguments().size() != cb.getArguments().size()) return false;
            for (int i = 0; i < ca.getArguments().size(); i++) {
                if (!structurallyEqual(ca.getArguments().get(i), cb.getArguments().get(i))) return false;
            }
            return true;
        }
        return false;
    }
}
