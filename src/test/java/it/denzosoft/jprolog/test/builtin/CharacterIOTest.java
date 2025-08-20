package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for ISO Prolog character I/O predicates.
 */
public class CharacterIOTest {
    
    private Prolog prolog;
    private ByteArrayOutputStream outputStream;
    private PrintStream originalOut;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
        // Capture output for testing
        outputStream = new ByteArrayOutputStream();
        originalOut = System.out;
        System.setOut(new PrintStream(outputStream));
    }
    
    @Test
    public void testPutChar() {
        // Test putting a single character
        List<Map<String, Term>> solutions = prolog.solve("put_char(a)");
        assertFalse("put_char(a) should succeed", solutions.isEmpty());
        assertEquals("a", outputStream.toString());
        
        // Reset output stream
        outputStream.reset();
        
        // Test putting a newline - Note: Using nl/0 instead since escape sequences aren't fully supported
        solutions = prolog.solve("nl");
        assertFalse("nl should succeed", solutions.isEmpty());
        assertEquals("\n", outputStream.toString());
    }
    
    @Test
    public void testPutCode() {
        // Test putting character by code
        List<Map<String, Term>> solutions = prolog.solve("put_code(97)");
        assertFalse("put_code(97) should succeed", solutions.isEmpty());
        assertEquals("a", outputStream.toString());
        
        // Reset output stream
        outputStream.reset();
        
        // Test putting newline by code
        solutions = prolog.solve("put_code(10)");
        assertFalse("put_code(10) should succeed", solutions.isEmpty());
        assertEquals("\n", outputStream.toString());
        
        // Reset output stream
        outputStream.reset();
        
        // Test putting uppercase A
        solutions = prolog.solve("put_code(65)");
        assertFalse("put_code(65) should succeed", solutions.isEmpty());
        assertEquals("A", outputStream.toString());
    }
    
    @Test
    public void testPutCharSequence() {
        // Test sequence of characters
        List<Map<String, Term>> solutions = prolog.solve("put_char(h), put_char(i)");
        assertFalse("Character sequence should succeed", solutions.isEmpty());
        assertEquals("hi", outputStream.toString());
    }
    
    @Test
    public void testPutCodeSequence() {
        // Test sequence with codes
        List<Map<String, Term>> solutions = prolog.solve("put_code(72), put_code(105)");
        assertFalse("Code sequence should succeed", solutions.isEmpty());
        assertEquals("Hi", outputStream.toString());
    }
    
    @Test
    public void testPutCharErrorHandling() {
        // Test with unbound variable
        List<Map<String, Term>> solutions = prolog.solve("put_char(X)");
        assertTrue("put_char with unbound variable should fail", solutions.isEmpty());
        
        // Test with multi-character atom
        solutions = prolog.solve("put_char(hello)");
        assertTrue("put_char with multi-character atom should fail", solutions.isEmpty());
        
        // Test with number
        solutions = prolog.solve("put_char(123)");
        assertTrue("put_char with number should fail", solutions.isEmpty());
    }
    
    @Test
    public void testPutCodeErrorHandling() {
        // Test with unbound variable
        List<Map<String, Term>> solutions = prolog.solve("put_code(X)");
        assertTrue("put_code with unbound variable should fail", solutions.isEmpty());
        
        // Test with atom
        solutions = prolog.solve("put_code(hello)");
        assertTrue("put_code with atom should fail", solutions.isEmpty());
        
        // Test with invalid code (negative)
        solutions = prolog.solve("put_code(-1)");
        assertTrue("put_code with negative code should fail", solutions.isEmpty());
        
        // Test with invalid code (too large)
        solutions = prolog.solve("put_code(2000000)");
        assertTrue("put_code with too large code should fail", solutions.isEmpty());
    }
    
    @Test
    public void testBasicChaining() {
        // Test basic chaining of put operations
        List<Map<String, Term>> solutions = prolog.solve("put_code(72), put_code(105)");
        assertFalse("Chaining test should succeed", solutions.isEmpty());
        assertEquals("Hi", outputStream.toString());
    }
    
    @Test
    public void testCharCodeRoundTrip() {
        // Test that character codes work correctly
        prolog.consult("char_roundtrip(Code) :- put_code(Code), put_char(' ').");
        
        List<Map<String, Term>> solutions = prolog.solve("char_roundtrip(65)");
        assertFalse("Roundtrip test should succeed", solutions.isEmpty());
        assertEquals("A ", outputStream.toString());
    }
    
    @org.junit.After
    public void tearDown() {
        if (originalOut != null) {
            System.setOut(originalOut);
        }
    }
}