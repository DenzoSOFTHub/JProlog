package it.denzosoft.jprolog;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.Prolog;
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
    
    // START_CHANGE: ISS-2025-0665 - the throw/1 tests drove the legacy builtin.exception.Throw
    // class directly, which the engine never dispatches (throw/1 is a machine control
    // construct; the registry entry only gives it its permission_error). They now run through
    // the engine and assert the exact ball. The ISOErrorTerms factory tests are kept: that
    // class is live (core.engine.v4.Errors builds every error term with it).
    private Prolog prolog;
    // END_CHANGE: ISS-2025-0665
    private Map<String, Term> bindings;
    private List<Map<String, Term>> solutions;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
        bindings = new HashMap<>();
        solutions = new ArrayList<>();
    }
    
    @Test
    public void testBasicThrow() {
        try {
            prolog.solve("throw(my_error).");
            fail("Expected PrologException to be thrown");
        } catch (PrologException e) {
            assertEquals("Error term should match", new Atom("my_error"), e.getErrorTerm());
        }
        assertEquals(1, prolog.solve("catch(throw(my_error), B, true), B == my_error.").size());
    }
    
    @Test
    public void testThrowWithVariable() {
        assertEquals("throw(X) with X unbound raises instantiation_error", 1, prolog.solve(
            "catch(throw(_), error(E, _), true), E == instantiation_error.").size());
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
        // throw/0 and throw/2 are unknown procedures
        assertEquals(1, prolog.solve(
            "catch(call(throw), error(E, _), true), E == existence_error(procedure, throw/0).").size());
        assertEquals(1, prolog.solve(
            "catch(call(throw, a, b), error(E, _), true), E == existence_error(procedure, throw/2).").size());
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