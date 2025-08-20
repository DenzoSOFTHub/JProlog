package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.parser.Parser;
import it.denzosoft.jprolog.core.engine.Rule;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;

public class ComprehensiveParsingTest {

    @Test
    public void testAtomParsing() {
        Parser parser = new Parser();
        try {
            Term atom = parser.parseTerm("hello");
            assertTrue(atom instanceof Atom);
            assertEquals("hello", ((Atom) atom).getName());
        } catch (Exception e) {
            fail("Should parse simple atom: " + e.getMessage());
        }
    }

    @Test
    public void testQuotedAtomParsing() {
        Parser parser = new Parser();
        try {
            Term atom = parser.parseTerm("'hello world'");
            assertTrue(atom instanceof Atom);
            assertEquals("hello world", ((Atom) atom).getName());
        } catch (Exception e) {
            fail("Should parse quoted atom: " + e.getMessage());
        }
    }

    @Test
    public void testNumberParsing() {
        Parser parser = new Parser();
        try {
            Term number = parser.parseTerm("42");
            assertTrue(number instanceof it.denzosoft.jprolog.core.terms.Number);
            assertEquals(42.0, ((it.denzosoft.jprolog.core.terms.Number) number).getValue(), 0.001);
            
            Term decimal = parser.parseTerm("3.14");
            assertTrue(decimal instanceof it.denzosoft.jprolog.core.terms.Number);
            assertEquals(3.14, ((it.denzosoft.jprolog.core.terms.Number) decimal).getValue(), 0.001);
        } catch (Exception e) {
            fail("Should parse numbers: " + e.getMessage());
        }
    }

    @Test
    public void testVariableParsing() {
        Parser parser = new Parser();
        try {
            Term variable = parser.parseTerm("X");
            assertTrue(variable instanceof it.denzosoft.jprolog.core.terms.Variable);
            assertEquals("X", ((it.denzosoft.jprolog.core.terms.Variable) variable).getName());
            
            Term underscore = parser.parseTerm("_");
            assertTrue(underscore instanceof it.denzosoft.jprolog.core.terms.Variable);
            it.denzosoft.jprolog.core.terms.Variable underscoreVar = (it.denzosoft.jprolog.core.terms.Variable) underscore;
            assertTrue("Anonymous variable should be anonymous", underscoreVar.isAnonymous());
            assertEquals("_", underscoreVar.getDisplayName());
        } catch (Exception e) {
            fail("Should parse variables: " + e.getMessage());
        }
    }

    @Test
    public void testCompoundTermParsing() {
        Parser parser = new Parser();
        try {
            Term compound = parser.parseTerm("parent(tom, bob)");
            assertTrue(compound instanceof it.denzosoft.jprolog.core.terms.CompoundTerm);
            assertEquals("parent", compound.getName());
            assertEquals(2, compound.getArguments().size());
        } catch (Exception e) {
            fail("Should parse compound terms: " + e.getMessage());
        }
    }

    @Test
    public void testListParsing() {
        Parser parser = new Parser();
        try {
            Term emptyList = parser.parseTerm("[]");
            assertTrue(emptyList instanceof Atom);
            assertEquals("[]", ((Atom) emptyList).getName());
            
            Term list = parser.parseTerm("[a, b, c]");
            assertTrue(list instanceof it.denzosoft.jprolog.core.terms.CompoundTerm);
            assertEquals(".", list.getName());
        } catch (Exception e) {
            fail("Should parse lists: " + e.getMessage());
        }
    }

    @Test
    public void testRuleParsing() {
        Parser parser = new Parser();
        try {
            List<Rule> rules = parser.parse("parent(X, Y) :- father(X, Y).");
            assertEquals(1, rules.size());
            
            Rule rule = rules.get(0);
            assertEquals("parent", rule.getHead().getName());
            assertEquals(1, rule.getBody().size());
            assertEquals("father", rule.getBody().get(0).getName());
        } catch (Exception e) {
            fail("Should parse rules: " + e.getMessage());
        }
    }

    @Test
    public void testFactParsing() {
        Parser parser = new Parser();
        try {
            List<Rule> rules = parser.parse("likes(mary, food).");
            assertEquals(1, rules.size());
            
            Rule rule = rules.get(0);
            assertEquals("likes", rule.getHead().getName());
            assertTrue(rule.getBody().isEmpty());
        } catch (Exception e) {
            fail("Should parse facts: " + e.getMessage());
        }
    }

    @Test
    public void testArithmeticExpressionParsing() {
        Parser parser = new Parser();
        try {
            // Simple arithmetic should work
            Term expr = parser.parseTerm("2 + 3");
            assertTrue(expr instanceof it.denzosoft.jprolog.core.terms.CompoundTerm);
            assertEquals("+", expr.getName());
            
            Term complex = parser.parseTerm("2 + 3 * 4");
            assertTrue(complex instanceof it.denzosoft.jprolog.core.terms.CompoundTerm);
            // Should parse as +(2, *(3, 4)) due to precedence
        } catch (Exception e) {
            fail("Should parse arithmetic expressions: " + e.getMessage());
        }
    }

    @Test
    public void testOperatorPrecedence() {
        Parser parser = new Parser();
        try {
            // Test that * binds tighter than +
            Term expr = parser.parseTerm("2 + 3 * 4");
            assertEquals("+", expr.getName());
            assertEquals(2, expr.getArguments().size());
            
            // First arg should be 2
            Term firstArg = expr.getArguments().get(0);
            assertTrue(firstArg instanceof it.denzosoft.jprolog.core.terms.Number);
            
            // Second arg should be *(3, 4)
            Term secondArg = expr.getArguments().get(1);
            assertTrue(secondArg instanceof it.denzosoft.jprolog.core.terms.CompoundTerm);
            assertEquals("*", secondArg.getName());
        } catch (Exception e) {
            fail("Should handle operator precedence: " + e.getMessage());
        }
    }

    @Test
    public void testComparison() {
        Parser parser = new Parser();
        try {
            Term comparison = parser.parseTerm("X >= 5");
            assertTrue(comparison instanceof it.denzosoft.jprolog.core.terms.CompoundTerm);
            assertEquals(">=", comparison.getName());
            assertEquals(2, comparison.getArguments().size());
        } catch (Exception e) {
            fail("Should parse comparison operators: " + e.getMessage());
        }
    }
}