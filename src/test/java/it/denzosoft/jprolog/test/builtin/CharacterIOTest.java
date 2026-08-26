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
        // START_CHANGE: ISS-2025-0505 - 4.3 wave D: ISO 8.12.3.3 — instantiation_error and
        // type_error(character, C) where put_char/1 used to fail silently.
        assertFalse("put_char(X) must raise instantiation_error", prolog.solve(
            "catch(put_char(X), error(instantiation_error, _), true)").isEmpty());
        assertFalse("put_char(hello) must raise type_error(character, hello)", prolog.solve(
            "catch(put_char(hello), error(type_error(character, hello), _), true)").isEmpty());
        assertFalse("put_char(123) must raise type_error(character, 123)", prolog.solve(
            "catch(put_char(123), error(type_error(character, 123), _), true)").isEmpty());
        // END_CHANGE: ISS-2025-0505
    }
    
    @Test
    public void testPutCodeErrorHandling() {
        // START_CHANGE: ISS-2025-0505 - ISO 8.12.3.3 / 7.12.2: instantiation_error,
        // type_error(integer, C), representation_error(character_code).
        assertFalse("put_code(X) must raise instantiation_error", prolog.solve(
            "catch(put_code(X), error(instantiation_error, _), true)").isEmpty());
        assertFalse("put_code(hello) must raise type_error(integer, hello)", prolog.solve(
            "catch(put_code(hello), error(type_error(integer, hello), _), true)").isEmpty());
        assertFalse("put_code(-1) must raise representation_error(character_code)", prolog.solve(
            "catch(put_code(-1), error(representation_error(character_code), _), true)").isEmpty());
        assertFalse("put_code(2000000) must raise representation_error(character_code)", prolog.solve(
            "catch(put_code(2000000), error(representation_error(character_code), _), true)").isEmpty());
        // END_CHANGE: ISS-2025-0505
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