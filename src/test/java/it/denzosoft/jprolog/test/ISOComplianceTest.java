package it.denzosoft.jprolog.test;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;

/**
 * Comprehensive test suite for ISO Prolog compliance.
 * Tests all newly implemented features for full ISO compliance.
 */
public class ISOComplianceTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    // Module System Tests
    
    // START_CHANGE: ISS-2025-0660 - the tautologies of this class (assertTrue(true),
    // "empty || !empty") are replaced by assertions on the actual answers.
    @Test
    public void testModuleDeclaration() {
        // module/2 in a consulted text creates the module, exports its list and imports it
        prolog.consult(":- module(test_module, [foo/1, bar/2]).\nfoo(1).\nbar(a, b).\n");
        assertEquals(1, prolog.solve("current_module(test_module).").size());
        assertEquals(1, prolog.solve("foo(X), X == 1.").size());
        assertEquals(1, prolog.solve("test_module:bar(A, B), A == a, B == b.").size());
    }
    
    @Test
    public void testModuleImport() {
        // Test use_module/1
        prolog.asserta(":- module(mod1, [pred1/1]).");
        prolog.asserta("pred1(a).");
        prolog.asserta(":- module(mod2, []).");
        prolog.asserta(":- use_module(mod1).");
        
        // Should be able to call pred1/1 from mod2
        List<Map<String, Term>> solutions = prolog.solve("pred1(X).");
        assertFalse(solutions.isEmpty());
    }
    
    @Test
    public void testModuleQualifiedCall() {
        // Test module:goal syntax
        prolog.asserta(":- module(test_mod, [test_pred/0]).");
        prolog.asserta("test_pred.");
        
        // Qualified call
        List<Map<String, Term>> solutions = prolog.solve("test_mod:test_pred.");
        assertFalse(solutions.isEmpty());
    }
    
    // DCG Tests
    
    @Test
    public void testDCGBasicRule() {
        // Test basic DCG rule transformation
        prolog.asserta("noun --> [cat].");
        prolog.asserta("noun --> [dog].");
        
        // Test phrase/2
        List<Map<String, Term>> solutions = prolog.solve("phrase(noun, [cat]).");
        assertFalse(solutions.isEmpty());
        
        solutions = prolog.solve("phrase(noun, [dog]).");
        assertFalse(solutions.isEmpty());
        
        solutions = prolog.solve("phrase(noun, [bird]).");
        assertTrue(solutions.isEmpty());
    }
    
    @Test
    public void testDCGWithArguments() {
        // Test DCG rule with arguments
        prolog.asserta("noun(N) --> [N], {atom(N)}.");
        
        List<Map<String, Term>> solutions = prolog.solve("phrase(noun(cat), [cat]).");
        assertFalse(solutions.isEmpty());
    }
    
    @Test
    public void testDCGConjunction() {
        // Test DCG conjunction
        prolog.asserta("det --> [the].");
        prolog.asserta("noun --> [cat].");
        prolog.asserta("np --> det, noun.");
        
        List<Map<String, Term>> solutions = prolog.solve("phrase(np, [the, cat]).");
        assertFalse(solutions.isEmpty());
    }
    
    @Test
    public void testDCGDisjunction() {
        // Test DCG disjunction
        prolog.asserta("pet --> [cat] ; [dog].");
        
        List<Map<String, Term>> solutions = prolog.solve("phrase(pet, [cat]).");
        assertFalse(solutions.isEmpty());
        
        solutions = prolog.solve("phrase(pet, [dog]).");
        assertFalse(solutions.isEmpty());
    }
    
    @Test
    public void testPhraseWithRemainder() {
        // Test phrase/3 with remainder
        prolog.asserta("a --> [a].");
        
        List<Map<String, Term>> solutions = prolog.solve("phrase(a, [a, b, c], Rest).");
        assertFalse(solutions.isEmpty());
        assertEquals("[b, c]", solutions.get(0).get("Rest").toString());
    }
    
    // Custom Operator Tests
    
    @Test
    public void testOperatorDefinition() {
        // Test op/3 predicate
        List<Map<String, Term>> solutions = prolog.solve("op(800, xfx, likes).");
        assertTrue(solutions.isEmpty() == false); // Should succeed
        
        // Should be able to use the operator
        prolog.asserta("mary likes wine.");
        solutions = prolog.solve("mary likes X.");
        assertFalse(solutions.isEmpty());
        assertEquals("wine", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testOperatorRemoval() {
        // Define operator
        prolog.solve("op(800, xfx, dislikes).");
        
        assertEquals(1, prolog.solve("current_op(800, xfx, dislikes).").size());
        // Remove operator: op/3 with priority 0 succeeds and the definition is gone
        assertEquals(1, prolog.solve("op(0, xfx, dislikes).").size());
        assertTrue(prolog.solve("current_op(_, xfx, dislikes).").isEmpty());
    }
    
    @Test
    public void testCurrentOp() {
        // Test current_op/3
        prolog.solve("op(750, xfx, custom_op).");
        
        List<Map<String, Term>> solutions = prolog.solve("current_op(750, xfx, custom_op).");
        assertFalse(solutions.isEmpty());
    }
    
    // Advanced I/O Tests
    
    @Test
    public void testReadTerm() {
        // read_term/3 with variable_names/1 from a string stream
        List<Map<String, Term>> solutions = prolog.solve(
            "open_string(\"foo(X, Y, X). \", S), read_term(S, T, [variable_names(V)]), close(S), "
            + "T = foo(A, B, C), A == C, A \\== B, V == ['X' = A, 'Y' = B].");
        assertEquals(1, solutions.size());
    }
    
    @Test
    public void testWriteTerm() {
        // Test write_term/2 with options
        List<Map<String, Term>> solutions = prolog.solve(
            "with_output_to(atom(A), write_term('hello world', [quoted(true)])), A == '\\'hello world\\''.");
        assertEquals(1, solutions.size());
        solutions = prolog.solve(
            "with_output_to(atom(A), write_term('hello world', [quoted(false)])), A == 'hello world'.");
        assertEquals(1, solutions.size());
    }
    
    @Test
    public void testFormat() {
        // Test format/2
        List<Map<String, Term>> solutions = prolog.solve(
            "format(atom(A), 'Hello ~w!', [world]), A == 'Hello world!'.");
        assertEquals(1, solutions.size());
    }
    
    // Character Type Tests
    
    @Test
    public void testCharType() {
        // Test char_type/2
        List<Map<String, Term>> solutions = prolog.solve("char_type(a, alpha).");
        assertFalse(solutions.isEmpty());
        
        solutions = prolog.solve("char_type('5', digit).");
        assertFalse(solutions.isEmpty());
        
        solutions = prolog.solve("char_type(' ', space).");
        assertFalse(solutions.isEmpty());
    }
    
    @Test
    public void testCharTypeGeneration() {
        // Test char_type/2 for character generation
        List<Map<String, Term>> solutions = prolog.solve("char_type(C, digit).");
        assertFalse(solutions.isEmpty());
        
        String charResult = solutions.get(0).get("C").toString();
        assertTrue(charResult.matches("[0-9]"));
    }
    
    @Test
    public void testCharCode() {
        // Test char_code/2
        List<Map<String, Term>> solutions = prolog.solve("char_code(a, 97).");
        assertFalse(solutions.isEmpty());
        
        solutions = prolog.solve("char_code(a, Code).");
        assertFalse(solutions.isEmpty());
        assertEquals("97", solutions.get(0).get("Code").toString());
        
        solutions = prolog.solve("char_code(Char, 98).");
        assertFalse(solutions.isEmpty());
        assertEquals("b", solutions.get(0).get("Char").toString());
    }
    
    // System Statistics Tests
    
    @Test
    public void testStatistics() {
        // Test statistics/2
        List<Map<String, Term>> solutions = prolog.solve("statistics(runtime, Time).");
        assertFalse(solutions.isEmpty());
        
        solutions = prolog.solve("statistics(heapused, Memory).");
        assertFalse(solutions.isEmpty());
        
        // Should return numeric values
        Term memoryTerm = solutions.get(0).get("Memory");
        assertTrue(memoryTerm instanceof it.denzosoft.jprolog.core.terms.Number);
    }
    
    // Exception Handling Tests
    
    @Test
    public void testCatchThrow() {
        // Test catch/3 and throw/1
        prolog.asserta("test_throw :- throw(my_error).");
        
        List<Map<String, Term>> solutions = prolog.solve("catch(test_throw, Error, true).");
        assertFalse(solutions.isEmpty());
        assertEquals("my_error", solutions.get(0).get("Error").toString());
    }
    
    @Test
    public void testISOErrorTerms() {
        // Test ISO error term structure
        prolog.asserta("test_type_error :- throw(error(type_error(integer, abc), context)).");
        
        List<Map<String, Term>> solutions = prolog.solve("catch(test_type_error, error(type_error(Type, Culprit), Context), true).");
        assertFalse(solutions.isEmpty());
        assertEquals("integer", solutions.get(0).get("Type").toString());
        assertEquals("abc", solutions.get(0).get("Culprit").toString());
    }
    
    // Comprehensive Feature Integration Tests
    
    @Test
    public void testCompleteISOFeatureSet() {
        // Test that all major ISO features work together
        
        // Define module with DCG and custom operator
        prolog.asserta(":- module(test_integration, [sentence/2]).");
        prolog.asserta(":- op(800, xfx, means).");
        prolog.asserta("det --> [the].");
        prolog.asserta("noun --> [cat] ; [dog].");
        prolog.asserta("sentence --> det, noun.");
        prolog.asserta("meaning means interpretation.");
        
        // Test DCG parsing
        List<Map<String, Term>> solutions = prolog.solve("phrase(sentence, [the, cat]).");
        assertFalse(solutions.isEmpty());
        
        // Test custom operator
        solutions = prolog.solve("meaning means X.");
        assertFalse(solutions.isEmpty());
        assertEquals("interpretation", solutions.get(0).get("X").toString());
        
        // Test character operations
        assertEquals(1, prolog.solve("char_type(c, alpha), char_type(c, lower(U)), U == 'C'.").size());

        // Test format output
        assertEquals(1, prolog.solve("format(atom(A), 'Test: ~w~n', [success]), A == 'Test: success\\n'.").size());
    }
    
    @Test
    public void testISOComplianceLevel() {
        // Every listed ISO/core predicate indicator is defined (built-in, native or library).
        // The old probe called each NAME at arity 0 with unknown=fail and counted "no exception"
        // as implemented, which a missing predicate satisfies too.
        String[] isoPredicates = {
            "=/2", "\\=/2", "==/2", "\\==/2", "@</2", "@=</2", "@>/2", "@>=/2", "compare/3",
            "is/2", "=:=/2", "=\\=/2", "</2", "=</2", ">/2", ">=/2",
            "functor/3", "arg/3", "=../2", "copy_term/2", "term_variables/2",
            "var/1", "nonvar/1", "atom/1", "number/1", "integer/1", "float/1",
            "atomic/1", "compound/1", "callable/1", "ground/1", "is_list/1",
            "findall/3", "bagof/3", "setof/3", "call/1", "call/3", "once/1", "ignore/1", "forall/2",
            "asserta/1", "assertz/1", "retract/1", "retractall/1", "abolish/1", "current_predicate/1",
            "clause/2", "append/3", "length/2", "member/2", "reverse/2", "sort/2", "msort/2", "keysort/2",
            "atom_length/2", "atom_concat/3", "sub_atom/5", "atom_chars/2", "atom_codes/2",
            "char_code/2", "number_codes/2", "number_chars/2",
            "string_length/2", "string_concat/3", "sub_string/5", "string_chars/2", "atom_string/2",
            "write/1", "writeln/1", "writeq/1", "print/1", "write_canonical/1", "nl/0", "nl/1",
            "read/1", "read_term/2", "read_term/3", "open/3", "open/4", "close/1", "close/2",
            "current_input/1", "current_output/1", "set_input/1", "set_output/1", "flush_output/0",
            "get_char/1", "put_char/1", "get_code/1", "put_code/1", "peek_char/1", "get_byte/2",
            "put_byte/2", "stream_property/2", "set_stream_position/2", "at_end_of_stream/0",
            "catch/3", "throw/1", "halt/0", "halt/1",
            "current_prolog_flag/2", "set_prolog_flag/2", "op/3", "current_op/3", "char_conversion/2",
            "phrase/2", "phrase/3", "write_term/2", "write_term/3", "format/1", "format/2", "format/3",
            "char_type/2", "code_type/2", "statistics/2", "no_such_predicate_p7/0"
        };
        List<String> missing = new java.util.ArrayList<>();
        for (String pi : isoPredicates) {
            int slash = pi.lastIndexOf('/');
            String goal = "functor(H, '" + pi.substring(0, slash).replace("\\", "\\\\").replace("'", "\\'") + "', "
                + pi.substring(slash + 1) + "), predicate_property(H, defined).";
            if (prolog.solve(goal).isEmpty()) {
                missing.add(pi);
            }
        }
        assertEquals("exactly the sentinel is undefined", java.util.Collections.singletonList("no_such_predicate_p7/0"), missing);
    }
    // END_CHANGE: ISS-2025-0660
}
