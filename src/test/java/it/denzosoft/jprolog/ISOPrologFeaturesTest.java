package it.denzosoft.jprolog;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;

/**
 * Comprehensive test for all new ISO Prolog features implemented.
 */
public class ISOPrologFeaturesTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    @Test
    public void testTypePredicates() {
        // callable/1
        assertFalse(prolog.solve("callable(hello).").isEmpty());
        assertFalse(prolog.solve("callable(foo(bar)).").isEmpty());
        assertTrue(prolog.solve("callable(123).").isEmpty());
        
        // ground/1
        assertFalse(prolog.solve("ground(hello).").isEmpty());
        assertFalse(prolog.solve("ground(123).").isEmpty());
        assertTrue(prolog.solve("ground(_X).").isEmpty());
        
        // is_list/1
        assertFalse(prolog.solve("is_list([]).").isEmpty());
        assertFalse(prolog.solve("is_list([a,b,c]).").isEmpty());
        assertTrue(prolog.solve("is_list(atom).").isEmpty());
        
        // simple/1
        assertFalse(prolog.solve("simple(atom).").isEmpty());
        assertFalse(prolog.solve("simple(123).").isEmpty());
        assertTrue(prolog.solve("simple(foo(bar)).").isEmpty());
    }

    @Test
    public void testArithmeticFunctions() {
        // max/min
        List<Map<String, Term>> solutions = prolog.solve("X is max(5, 3).");
        assertFalse(solutions.isEmpty());
        assertEquals("5.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is min(5, 3).");
        assertFalse(solutions.isEmpty());
        assertEquals("3.0", solutions.get(0).get("X").toString());
        
        // div/rem
        solutions = prolog.solve("X is div(7, 3).");
        assertFalse(solutions.isEmpty());
        assertEquals("2.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is rem(7, 3).");
        assertFalse(solutions.isEmpty());
        assertEquals("1.0", solutions.get(0).get("X").toString());
        
        // sign
        solutions = prolog.solve("X is sign(5).");
        assertFalse(solutions.isEmpty());
        assertEquals("1.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is sign(-5).");
        assertFalse(solutions.isEmpty());
        assertEquals("-1.0", solutions.get(0).get("X").toString());
        
        // Constants
        solutions = prolog.solve("X is pi.");
        assertFalse(solutions.isEmpty());
        assertTrue(solutions.get(0).get("X").toString().startsWith("3.14"));
        
        solutions = prolog.solve("X is e.");
        assertFalse(solutions.isEmpty());
        assertTrue(solutions.get(0).get("X").toString().startsWith("2.71"));
    }

    @Test
    public void testStringBuiltins() {
        // string_length/2
        assertFalse(prolog.solve("string_length(\"hello\", 5).").isEmpty());
        assertTrue(prolog.solve("string_length(\"hello\", 4).").isEmpty());
        
        // string_concat/3
        assertFalse(prolog.solve("string_concat(\"hello\", \" world\", \"hello world\").").isEmpty());
        assertTrue(prolog.solve("string_concat(\"hello\", \" world\", \"wrong\").").isEmpty());
        
        // atom_string/2
        assertFalse(prolog.solve("atom_string(hello, \"hello\").").isEmpty());
        assertTrue(prolog.solve("atom_string(hello, \"world\").").isEmpty());
        
        // number_string/2
        assertFalse(prolog.solve("number_string(123, \"123\").").isEmpty());
        assertTrue(prolog.solve("number_string(123, \"456\").").isEmpty());
    }

    @Test
    public void testSystemFlags() {
        // current_prolog_flag/2
        assertFalse(prolog.solve("current_prolog_flag(bounded, true).").isEmpty());
        assertFalse(prolog.solve("current_prolog_flag(dialect, iso).").isEmpty());
        assertTrue(prolog.solve("current_prolog_flag(nonexistent, _).").isEmpty());
        
        // set_prolog_flag/2 (test modifiable flags)
        assertFalse(prolog.solve("set_prolog_flag(debug, on).").isEmpty());
        assertFalse(prolog.solve("current_prolog_flag(debug, on).").isEmpty());
        
        assertFalse(prolog.solve("set_prolog_flag(debug, off).").isEmpty());
        assertFalse(prolog.solve("current_prolog_flag(debug, off).").isEmpty());
    }

    @Test
    public void testBitwiseOperations() {
        // xor
        List<Map<String, Term>> solutions = prolog.solve("X is xor(5, 3).");
        assertFalse(solutions.isEmpty());
        assertEquals("6.0", solutions.get(0).get("X").toString());
        
        // Bitwise OR
        solutions = prolog.solve("X is \\/(5, 3).");
        assertFalse(solutions.isEmpty());
        assertEquals("7.0", solutions.get(0).get("X").toString());
        
        // Bitwise AND
        solutions = prolog.solve("X is /\\(5, 3).");
        assertFalse(solutions.isEmpty());
        assertEquals("1.0", solutions.get(0).get("X").toString());
        
        // Bit shifts
        solutions = prolog.solve("X is <<(5, 1).");
        assertFalse(solutions.isEmpty());
        assertEquals("10.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is >>(10, 1).");
        assertFalse(solutions.isEmpty());
        assertEquals("5.0", solutions.get(0).get("X").toString());
    }

    @Test
    public void testExistingFeaturesStillWork() {
        // Ensure existing features weren't broken
        assertFalse(prolog.solve("atom(hello).").isEmpty());
        assertFalse(prolog.solve("number(123).").isEmpty());
        assertFalse(prolog.solve("compound(foo(bar)).").isEmpty());
        
        // Arithmetic still works
        List<Map<String, Term>> solutions = prolog.solve("X is 3 + 2.");
        assertFalse(solutions.isEmpty());
        assertEquals("5.0", solutions.get(0).get("X").toString());
        
        // Unification still works
        assertFalse(prolog.solve("X = hello, X = hello.").isEmpty());
        
        // List operations still work
        assertFalse(prolog.solve("append([1,2], [3,4], [1,2,3,4]).").isEmpty());
    }
}