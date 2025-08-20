package it.denzosoft.jprolog;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.exception.Throw;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Tests for ISO exception handling system.
 */
public class ExceptionHandlingTest {
    
    private Throw throwPredicate;
    private Map<String, Term> bindings;
    private List<Map<String, Term>> solutions;
    
    @Before
    public void setUp() {
        throwPredicate = new Throw();
        bindings = new HashMap<>();
        solutions = new ArrayList<>();
    }
    
    @Test
    public void testBasicThrow() {
        // Test: throw(my_error)
        Term throwQuery = createCompoundTerm("throw", new Atom("my_error"));
        
        try {
            throwPredicate.execute(throwQuery, bindings, solutions);
            fail("Expected PrologException to be thrown");
        } catch (PrologException e) {
            assertNotNull("Exception should have error term", e.getErrorTerm());
            assertEquals("Error term should match", new Atom("my_error"), e.getErrorTerm());
        }
    }
    
    @Test
    public void testThrowWithVariable() {
        // Test: throw(X) where X is unbound should fail with instantiation error
        Term throwQuery = createCompoundTerm("throw", new Variable("X"));
        
        try {
            throwPredicate.execute(throwQuery, bindings, solutions);
            fail("Expected PrologException for unbound variable");
        } catch (PrologException e) {
            assertNotNull("Exception should have error term", e.getErrorTerm());
            assertTrue("Should be instantiation error", 
                      e.getErrorTerm().toString().contains("instantiation_error"));
        }
    }
    
    @Test
    public void testISOErrorTermCreation() {
        // Test ISO error term factory methods
        
        // Instantiation error
        Term instError = ISOErrorTerms.instantiationError("test_context");
        assertTrue("Should be compound term", instError instanceof CompoundTerm);
        CompoundTerm instCompound = (CompoundTerm) instError;
        assertEquals("Should be error/2", "error", instCompound.getFunctor().getName());
        assertEquals("Should have 2 args", 2, instCompound.getArguments().size());
        
        // Type error  
        Term typeError = ISOErrorTerms.typeError("integer", new Atom("not_int"), "test");
        assertTrue("Should be compound term", typeError instanceof CompoundTerm);
        CompoundTerm typeCompound = (CompoundTerm) typeError;
        assertEquals("Should be error/2", "error", typeCompound.getFunctor().getName());
        
        // Zero divisor error
        Term zeroDiv = ISOErrorTerms.zeroDivisorError("(/)/2");
        assertTrue("Should be compound term", zeroDiv instanceof CompoundTerm);
        CompoundTerm zeroDivCompound = (CompoundTerm) zeroDiv;
        assertEquals("Should be error/2", "error", zeroDivCompound.getFunctor().getName());
        
        // Check structure of zero divisor error
        List<Term> args = zeroDivCompound.getArguments();
        assertEquals("Should have 2 arguments", 2, args.size());
        Term evalError = args.get(0);
        assertTrue("First arg should be compound", evalError instanceof CompoundTerm);
        CompoundTerm evalCompound = (CompoundTerm) evalError;
        assertEquals("Should be evaluation_error", "evaluation_error", 
                    evalCompound.getFunctor().getName());
    }
    
    @Test
    public void testPermissionError() {
        Term permError = ISOErrorTerms.permissionError("modify", "static_procedure", 
                                                      new CompoundTerm(new Atom("foo"), 
                                                      Arrays.asList()), "test");
        assertTrue("Should be compound term", permError instanceof CompoundTerm);
        CompoundTerm permCompound = (CompoundTerm) permError;
        assertEquals("Should be error/2", "error", permCompound.getFunctor().getName());
        
        // Check structure
        List<Term> args = permCompound.getArguments();
        assertEquals("Should have 2 arguments", 2, args.size());
        Term permissionErrorTerm = args.get(0);
        assertTrue("Should be compound", permissionErrorTerm instanceof CompoundTerm);
        CompoundTerm permissCompound = (CompoundTerm) permissionErrorTerm;
        assertEquals("Should be permission_error", "permission_error", 
                    permissCompound.getFunctor().getName());
        assertEquals("Should have 3 args", 3, permissCompound.getArguments().size());
    }
    
    @Test
    public void testSpecificEvaluationErrors() {
        // Test specific evaluation error factory methods
        Term zeroDiv = ISOErrorTerms.zeroDivisorError("test");
        Term undefined = ISOErrorTerms.undefinedFunctionError("test");
        Term overflow = ISOErrorTerms.floatOverflowError("test");
        
        // All should be error/2 compounds
        Term[] errors = {zeroDiv, undefined, overflow};
        for (Term error : errors) {
            assertTrue("Should be compound term", error instanceof CompoundTerm);
            CompoundTerm errorCompound = (CompoundTerm) error;
            assertEquals("Should be error/2", "error", errorCompound.getFunctor().getName());
            assertEquals("Should have 2 args", 2, errorCompound.getArguments().size());
        }
        
        // Check specific evaluation error types
        checkEvaluationErrorType(zeroDiv, "zero_divisor");
        checkEvaluationErrorType(undefined, "undefined");
        checkEvaluationErrorType(overflow, "float_overflow");
    }
    
    @Test
    public void testThrowInvalidArity() {
        // Test throw with wrong number of arguments
        Term throwQuery0 = createCompoundTerm("throw");
        Term throwQuery2 = createCompoundTerm("throw", new Atom("a"), new Atom("b"));
        
        try {
            throwPredicate.execute(throwQuery0, bindings, solutions);
            fail("Expected exception for wrong arity");
        } catch (PrologException e) {
            assertNotNull("Should have error term", e.getErrorTerm());
        }
        
        try {
            throwPredicate.execute(throwQuery2, bindings, solutions);
            fail("Expected exception for wrong arity");
        } catch (PrologException e) {
            assertNotNull("Should have error term", e.getErrorTerm());
        }
    }
    
    // Helper methods
    
    private void checkEvaluationErrorType(Term error, String expectedType) {
        assertTrue("Should be compound term", error instanceof CompoundTerm);
        CompoundTerm errorCompound = (CompoundTerm) error;
        Term evalError = errorCompound.getArguments().get(0);
        assertTrue("Should be compound term", evalError instanceof CompoundTerm);
        CompoundTerm evalCompound = (CompoundTerm) evalError;
        assertEquals("Should be evaluation_error", "evaluation_error", 
                    evalCompound.getFunctor().getName());
        assertEquals("Should have correct type", expectedType, 
                    evalCompound.getArguments().get(0).toString());
    }
    
    private Term createCompoundTerm(String functor, Term... args) {
        return new CompoundTerm(new Atom(functor), Arrays.asList(args));
    }
}