package it.denzosoft.jprolog.core.write.v2;

import it.denzosoft.jprolog.core.operator.OperatorTable;
import org.junit.Test;

import static org.junit.Assert.*;

/** Validates the v2 {@link PrologFormatter}. */
public class PrologFormatterTest {

    private final OperatorTable ops = OperatorTable.getDefault();
    private String fmt(String s) { return PrologFormatter.format(s, ops); }

    @Test public void factOnOneLine() {
        assertEquals("parent(tom,bob).\n", fmt("parent(tom,bob)."));
    }

    @Test public void ruleBodyOnePerLineIndented() {
        String out = fmt("grandparent(X,Z):-parent(X,Y),parent(Y,Z).");
        assertEquals("grandparent(X,Z) :-\n    parent(X,Y),\n    parent(Y,Z).\n", out);
    }

    @Test public void directiveCompact() {
        assertEquals(":- module(foo,[bar/1]).\n", fmt(":-module(foo,[bar/1])."));
    }

    @Test public void blankLineBetweenClauses() {
        String out = fmt("a. b.");
        assertEquals("a.\n\nb.\n", out);
    }

    @Test public void leadingCommentsPreserved() {
        String out = fmt("% a fact\nfoo(1).");
        assertEquals("% a fact\nfoo(1).\n", out);
    }

    @Test public void operatorsRenderedReadably() {
        String out = fmt("calc(R):-R is 2+3*4.");
        assertEquals("calc(R) :-\n    R is 2+3*4.\n", out);
    }

    @Test public void unparseableLeftUnchanged() {
        // a malformed clause must not be corrupted/dropped (a trailing newline is normalised in)
        String bad = "this is ) not valid (.";
        assertEquals(bad + "\n", fmt(bad));
    }

    @Test public void emptyStaysEmpty() {
        assertEquals("", fmt(""));
        assertEquals("   ", fmt("   "));
    }
}
