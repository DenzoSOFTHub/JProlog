package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for atom manipulation predicates (atom_codes/2, sub_atom/5).
 */
public class AtomManipulationTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    @Test
    public void testAtomCodesToCodes() {
        // Test atom to codes conversion
        List<Map<String, Term>> solutions = prolog.solve("atom_codes(hello, X)");
        assertFalse("atom_codes(hello, X) should succeed", solutions.isEmpty());
        
        // Check that we get the correct ASCII codes
        String result = solutions.get(0).get("X").toString();
        assertTrue("Should contain ASCII code for 'h' (104)", result.contains("104.0"));
        assertTrue("Should contain ASCII code for 'e' (101)", result.contains("101.0"));
        assertTrue("Should contain ASCII code for 'l' (108)", result.contains("108.0"));
        assertTrue("Should contain ASCII code for 'o' (111)", result.contains("111.0"));
    }
    
    @Test
    public void testAtomCodesToAtom() {
        // Test codes to atom conversion
        List<Map<String, Term>> solutions = prolog.solve("atom_codes(X, [104, 101, 108, 108, 111])");
        assertFalse("atom_codes(X, [104, 101, 108, 108, 111]) should succeed", solutions.isEmpty());
        assertEquals("hello", solutions.get(0).get("X").toString());
        
        // Test with empty list
        solutions = prolog.solve("atom_codes(X, [])");
        assertFalse("atom_codes(X, []) should succeed", solutions.isEmpty());
        assertEquals("", solutions.get(0).get("X").toString());
        
        // Test with single character
        solutions = prolog.solve("atom_codes(X, [65])");
        assertFalse("atom_codes(X, [65]) should succeed", solutions.isEmpty());
        assertEquals("A", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testAtomCodesSpecialCharacters() {
        // Test with space and special characters
        List<Map<String, Term>> solutions = prolog.solve("atom_codes('hello world!', X)");
        assertFalse("atom_codes with spaces should succeed", solutions.isEmpty());
        
        String result = solutions.get(0).get("X").toString();
        assertTrue("Should contain ASCII code for space (32)", result.contains("32.0"));
        assertTrue("Should contain ASCII code for '!' (33)", result.contains("33.0"));
    }
    
    @Test
    public void testSubAtomBasic() {
        // Test basic sub_atom extraction
        List<Map<String, Term>> solutions = prolog.solve("sub_atom(hello, 1, 3, 1, X)");
        assertFalse("sub_atom(hello, 1, 3, 1, X) should succeed", solutions.isEmpty());
        assertEquals("ell", solutions.get(0).get("X").toString());
        
        // Test with zero length
        solutions = prolog.solve("sub_atom(hello, 2, 0, 3, X)");
        assertFalse("sub_atom with zero length should succeed", solutions.isEmpty());
        assertEquals("", solutions.get(0).get("X").toString());
        
        // Test extracting entire atom
        solutions = prolog.solve("sub_atom(hello, 0, 5, 0, X)");
        assertFalse("sub_atom extracting entire atom should succeed", solutions.isEmpty());
        assertEquals("hello", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testSubAtomReverse() {
        // Test finding position of known substring
        List<Map<String, Term>> solutions = prolog.solve("sub_atom(hello, X, 2, Y, el)");
        assertFalse("Finding position of 'el' should succeed", solutions.isEmpty());
        assertEquals("1.0", solutions.get(0).get("X").toString());
        assertEquals("2.0", solutions.get(0).get("Y").toString());
        
        // Test finding all occurrences of 'l'
        solutions = prolog.solve("sub_atom(hello, X, 1, Y, l)");
        assertEquals("Should find 2 occurrences of 'l'", 2, solutions.size());
    }
    
    @Test
    public void testSubAtomAllSubstrings() {
        // Test generating all substrings of length 2
        List<Map<String, Term>> solutions = prolog.solve("sub_atom(abc, X, 2, Y, Z)");
        assertEquals("Should find 2 substrings of length 2", 2, solutions.size());
        
        // Check that we get 'ab' and 'bc'
        boolean foundAb = false, foundBc = false;
        for (Map<String, Term> solution : solutions) {
            String substring = solution.get("Z").toString();
            if ("ab".equals(substring)) foundAb = true;
            if ("bc".equals(substring)) foundBc = true;
        }
        assertTrue("Should find 'ab'", foundAb);
        assertTrue("Should find 'bc'", foundBc);
    }
    
    @Test
    public void testSubAtomEdgeCases() {
        // Test with empty atom
        List<Map<String, Term>> solutions = prolog.solve("sub_atom('', 0, 0, 0, X)");
        assertFalse("sub_atom with empty atom should succeed", solutions.isEmpty());
        assertEquals("", solutions.get(0).get("X").toString());
        
        // Test invalid constraints (should fail)
        solutions = prolog.solve("sub_atom(hello, 10, 1, 0, X)");
        assertTrue("Invalid constraints should fail", solutions.isEmpty());
        
        solutions = prolog.solve("sub_atom(hello, 0, 10, 0, X)");
        assertTrue("Length too long should fail", solutions.isEmpty());
    }
    
    @Test
    public void testSubAtomSingleCharacter() {
        // Test with single character atom
        List<Map<String, Term>> solutions = prolog.solve("sub_atom(a, X, Y, Z, W)");
        assertEquals("Single char atom should have 3 possible substrings", 3, solutions.size());
        
        // Should get empty string (twice) and the character itself
        int emptyCount = 0;
        boolean foundChar = false;
        for (Map<String, Term> solution : solutions) {
            String substring = solution.get("W").toString();
            if ("".equals(substring)) emptyCount++;
            if ("a".equals(substring)) foundChar = true;
        }
        assertEquals("Should find 2 empty substrings", 2, emptyCount);
        assertTrue("Should find 'a'", foundChar);
    }
    
    @Test
    public void testAtomCodesBidirectional() {
        // Test that atom_codes is truly bidirectional
        List<Map<String, Term>> solutions = prolog.solve("atom_codes(test, Codes), atom_codes(Result, Codes)");
        assertFalse("Bidirectional test should succeed", solutions.isEmpty());
        assertEquals("test", solutions.get(0).get("Result").toString());
    }
    
    @Test
    public void testIntegrationWithOtherPredicates() {
        // Test combining atom_codes with list operations
        prolog.consult("first_char_code(Atom, Code) :- atom_codes(Atom, [Code|_]).");
        
        List<Map<String, Term>> solutions = prolog.solve("first_char_code(hello, X)");
        assertFalse("first_char_code should succeed", solutions.isEmpty());
        assertEquals("104.0", solutions.get(0).get("X").toString()); // ASCII for 'h'
    }
}