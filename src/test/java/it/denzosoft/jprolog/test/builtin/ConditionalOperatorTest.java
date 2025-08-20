package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;

/**
 * Test suite for conditional operators (-> and ;) implementation.
 */
public class ConditionalOperatorTest {

    @Test
    public void testIfThenElse() {
        Prolog prolog = new Prolog();
        
        // Test if-then-else: (5 > 0 -> X = positive ; X = negative)
        List<Map<String, Term>> solutions = prolog.solve("(5 > 0 -> X = positive ; X = negative).");
        assertEquals(1, solutions.size());
        assertEquals("positive", solutions.get(0).get("X").toString());
        
        // Test with false condition: (-1 > 0 -> X = positive ; X = negative)
        solutions = prolog.solve("(-1 > 0 -> X = positive ; X = negative).");
        assertEquals(1, solutions.size());
        assertEquals("negative", solutions.get(0).get("X").toString());
    }

    @Test
    public void testIfThenElseWithVariables() {
        Prolog prolog = new Prolog();
        
        // Define facts
        prolog.consult("positive(X) :- X > 0.");
        prolog.consult("classify(X, Result) :- (positive(X) -> Result = pos ; Result = neg).");
        
        // Test positive number
        List<Map<String, Term>> solutions = prolog.solve("classify(5, R).");
        assertEquals(1, solutions.size());
        assertEquals("pos", solutions.get(0).get("R").toString());
        
        // Test negative number
        solutions = prolog.solve("classify(-3, R).");
        assertEquals(1, solutions.size());
        assertEquals("neg", solutions.get(0).get("R").toString());
    }

    @Test
    public void testSimpleDisjunction() {
        Prolog prolog = new Prolog();
        
        // Define facts
        prolog.consult("color(red).");
        prolog.consult("color(blue).");
        prolog.consult("shape(circle).");
        prolog.consult("shape(square).");
        
        // Test disjunction: (color(X) ; shape(X))
        List<Map<String, Term>> solutions = prolog.solve("(color(X) ; shape(X)).");
        assertEquals(4, solutions.size()); // red, blue, circle, square
    }

    @Test
    public void testIfThenOnly() {
        Prolog prolog = new Prolog();
        
        // Test if-then without else: (5 > 0 -> X = success)
        List<Map<String, Term>> solutions = prolog.solve("(5 > 0 -> X = success).");
        assertEquals(1, solutions.size());
        assertEquals("success", solutions.get(0).get("X").toString());
        
        // Test with false condition: (-1 > 0 -> X = success)
        solutions = prolog.solve("(-1 > 0 -> X = success).");
        assertEquals(0, solutions.size()); // Should fail
    }

    @Test
    public void testNestedConditionals() {
        Prolog prolog = new Prolog();
        
        // Test nested conditionals: (X > 0 -> (X > 10 -> Y = big ; Y = small) ; Y = negative)
        List<Map<String, Term>> solutions = prolog.solve("X = 15, (X > 0 -> (X > 10 -> Y = big ; Y = small) ; Y = negative).");
        assertEquals(1, solutions.size());
        assertEquals("big", solutions.get(0).get("Y").toString());
        
        solutions = prolog.solve("X = 5, (X > 0 -> (X > 10 -> Y = big ; Y = small) ; Y = negative).");
        assertEquals(1, solutions.size());
        assertEquals("small", solutions.get(0).get("Y").toString());
        
        solutions = prolog.solve("X = -2, (X > 0 -> (X > 10 -> Y = big ; Y = small) ; Y = negative).");
        assertEquals(1, solutions.size());
        assertEquals("negative", solutions.get(0).get("Y").toString());
    }

    @Test
    public void testComplexConditionalLogic() {
        Prolog prolog = new Prolog();
        
        // Define rules using conditionals
        prolog.consult("grade(Score, Grade) :- (Score >= 90 -> Grade = a ; (Score >= 80 -> Grade = b ; (Score >= 70 -> Grade = c ; Grade = f))).");
        
        // Test different scores
        List<Map<String, Term>> solutions = prolog.solve("grade(95, G).");
        assertEquals(1, solutions.size());
        assertEquals("a", solutions.get(0).get("G").toString());
        
        solutions = prolog.solve("grade(85, G).");
        assertEquals(1, solutions.size());
        assertEquals("b", solutions.get(0).get("G").toString());
        
        solutions = prolog.solve("grade(75, G).");
        assertEquals(1, solutions.size());
        assertEquals("c", solutions.get(0).get("G").toString());
        
        solutions = prolog.solve("grade(65, G).");
        assertEquals(1, solutions.size());
        assertEquals("f", solutions.get(0).get("G").toString());
    }
}