package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;



public class ConversionBuiltinsTest {

    @Test
    public void testAtomNumber() {
        Prolog prolog = new Prolog();
        
        // Test atom_number with atom to number conversion
        List<Map<String, Term>> solutions = prolog.solve("atom_number('123', N).");
        assertEquals(1, solutions.size());
        Term nValue = solutions.get(0).get("N");
        assertNotNull(nValue);
        assertEquals("123.0", nValue.toString());
        
        // Test atom_number with number to atom conversion
        solutions = prolog.solve("atom_number(A, 123.0).");
        assertEquals(1, solutions.size());
        Term aValue = solutions.get(0).get("A");
        assertNotNull(aValue);
        assertEquals("123", aValue.toString());
        
        // Test with float number
        solutions = prolog.solve("atom_number('3.14', N).");
        assertEquals(1, solutions.size());
        nValue = solutions.get(0).get("N");
        assertNotNull(nValue);
        assertEquals("3.14", nValue.toString());
        
        // Test conversion back to atom
        solutions = prolog.solve("atom_number(A, 3.14).");
        assertEquals(1, solutions.size());
        aValue = solutions.get(0).get("A");
        assertNotNull(aValue);
        assertEquals("3.14", aValue.toString());
    }

    @Test
    public void testAtomChars() {
        Prolog prolog = new Prolog();
        
        // Test atom_chars with atom to char list conversion
        List<Map<String, Term>> solutions = prolog.solve("atom_chars('hello', L).");
        assertEquals(1, solutions.size());
        Term lValue = solutions.get(0).get("L");
        assertNotNull(lValue);
        // Verify it can be unified with [h,e,l,l,o]
        List<Map<String, Term>> verify = prolog.solve("L = [h,e,l,l,o].");
        assertEquals(1, verify.size());
        
        // Test atom_chars with char list to atom conversion
        solutions = prolog.solve("atom_chars(A, [h,e,l,l,o]).");
        assertEquals(1, solutions.size());
        Term aValue = solutions.get(0).get("A");
        assertNotNull(aValue);
        assertEquals("hello", aValue.toString());
    }

    @Test
    public void testNumberChars() {
        Prolog prolog = new Prolog();
        
        // Test number_chars with number to char list conversion
        List<Map<String, Term>> solutions = prolog.solve("number_chars(123, L).");
        assertEquals(1, solutions.size());
        Term lValue = solutions.get(0).get("L");
        assertNotNull(lValue);
        // Verify it can be unified with ['1','2','3']
        List<Map<String, Term>> verify = prolog.solve("L = ['1','2','3'].");
        assertEquals(1, verify.size());
        
        // Test number_chars with char list to number conversion
        solutions = prolog.solve("number_chars(N, ['1','2','3']).");
        assertEquals(1, solutions.size());
        Term nValue = solutions.get(0).get("N");
        assertNotNull(nValue);
        assertEquals("123.0", nValue.toString());
        
        // Test with float number
        solutions = prolog.solve("number_chars(3.14, L).");
        assertEquals(1, solutions.size());
        lValue = solutions.get(0).get("L");
        assertNotNull(lValue);
        // Verify it can be unified with ['3','.','1','4']
        verify = prolog.solve("L = ['3','.','1','4'].");
        assertEquals(1, verify.size());
    }
}
