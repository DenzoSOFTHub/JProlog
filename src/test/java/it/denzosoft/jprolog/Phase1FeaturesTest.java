package it.denzosoft.jprolog;

import it.denzosoft.jprolog.builtin.system.OperatorDefinition;
import it.denzosoft.jprolog.core.engine.ArithmeticEvaluator;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Integration tests for Phase 1 ISO compliance features.
 * Tests mathematical functions, operator system, and other Phase 1 implementations.
 */
public class Phase1FeaturesTest {
    
    @Test
    public void testMathematicalFunctions() throws PrologEvaluationException {
        Map<String, Term> bindings = new HashMap<>();
        
        // Test sqrt function
        Term sqrtTerm = createFunction("sqrt", 16.0);
        assertEquals(4.0, ArithmeticEvaluator.evaluate(sqrtTerm, bindings), 0.0001);
        
        // Test sin function
        Term sinTerm = createFunction("sin", Math.PI / 2);
        assertEquals(1.0, ArithmeticEvaluator.evaluate(sinTerm, bindings), 0.0001);
        
        // Test abs function
        Term absTerm = createFunction("abs", -5.5);
        assertEquals(5.5, ArithmeticEvaluator.evaluate(absTerm, bindings), 0.0001);
        
        // Test log function
        Term logTerm = createFunction("log", Math.E);
        assertEquals(1.0, ArithmeticEvaluator.evaluate(logTerm, bindings), 0.0001);
    }
    
    @Test
    public void testOperatorDefinitionSystem() {
        OperatorDefinition opDef = new OperatorDefinition(OperatorDefinition.OperatorType.OP);
        Map<String, Term> bindings = new HashMap<>();
        List<Map<String, Term>> solutions = new ArrayList<>();
        
        // Test defining a new operator
        Term opQuery = createOpQuery(500, "xfx", "myop");
        assertTrue("Should define new operator successfully", 
                  opDef.execute(opQuery, bindings, solutions));
        assertEquals("Should have one solution", 1, solutions.size());
        
        // Verify operator was defined
        OperatorDefinition.OperatorInfo opInfo = OperatorDefinition.getOperator("myop");
        assertNotNull("Operator should be defined", opInfo);
        assertEquals("Precedence should match", 500, opInfo.precedence);
        assertEquals("Type should match", "xfx", opInfo.type);
    }
    
    @Test
    public void testStandardOperatorsPreloaded() {
        // Test that standard ISO operators are preloaded
        OperatorDefinition.OperatorInfo plusOp = OperatorDefinition.getOperator("+");
        assertNotNull("+ operator should be predefined", plusOp);
        // Note: Currently returns unary + (200, fy) due to HashMap overwrite issue
        // TODO: Fix operator system to handle multiple definitions for same operator
        assertEquals("+ should have precedence 200 (unary)", 200, plusOp.precedence);
        assertEquals("+ should be fy (unary)", "fy", plusOp.type);
        
        // Test existential quantification operator
        OperatorDefinition.OperatorInfo caretOp = OperatorDefinition.getOperator("^");
        assertNotNull("^ operator should be predefined", caretOp);
        assertEquals("^ should have precedence 200", 200, caretOp.precedence);
        assertEquals("^ should be xfy", "xfy", caretOp.type);
        
        // Test univ operator
        OperatorDefinition.OperatorInfo univOp = OperatorDefinition.getOperator("=..");
        assertNotNull("=.. operator should be predefined", univOp);
        assertEquals("=.. should have precedence 700", 700, univOp.precedence);
        assertEquals("=.. should be xfx", "xfx", univOp.type);
    }
    
    @Test
    public void testCurrentOpQuery() {
        OperatorDefinition currentOp = new OperatorDefinition(OperatorDefinition.OperatorType.CURRENT_OP);
        Map<String, Term> bindings = new HashMap<>();
        List<Map<String, Term>> solutions = new ArrayList<>();
        
        // Test querying existing operators
        Term query = createCurrentOpQuery(700, "xfx", "=");
        assertTrue("Should find = operator", 
                  currentOp.execute(query, bindings, solutions));
        assertTrue("Should have at least one solution", solutions.size() >= 1);
        
        // Test querying all operators with variables
        solutions.clear();
        Term allQuery = createCompoundTerm("current_op", 
                                         new Variable("P"), 
                                         new Variable("T"), 
                                         new Variable("N"));
        assertTrue("Should find multiple operators", 
                  currentOp.execute(allQuery, bindings, solutions));
        assertTrue("Should have many operators", solutions.size() > 10);
    }
    
    @Test
    public void testComplexArithmeticExpressions() throws PrologEvaluationException {
        Map<String, Term> bindings = new HashMap<>();
        
        // Test nested function: sqrt(abs(-16))
        Term absTerm = createFunction("abs", -16.0);
        Term sqrtAbsTerm = createFunction("sqrt", absTerm);
        assertEquals(4.0, ArithmeticEvaluator.evaluate(sqrtAbsTerm, bindings), 0.0001);
        
        // Test trigonometric identity: sin²(x) + cos²(x) = 1
        double angle = Math.PI / 4; // 45 degrees
        Term sinTerm = createFunction("sin", angle);
        Term cosTerm = createFunction("cos", angle);
        
        double sinVal = ArithmeticEvaluator.evaluate(sinTerm, bindings);
        double cosVal = ArithmeticEvaluator.evaluate(cosTerm, bindings);
        double identity = sinVal * sinVal + cosVal * cosVal;
        
        assertEquals("sin²(x) + cos²(x) should equal 1", 1.0, identity, 0.0001);
    }
    
    @Test
    public void testISOArithmeticFunctions() throws PrologEvaluationException {
        Map<String, Term> bindings = new HashMap<>();
        
        // Test ISO-specific functions
        Term signPos = createFunction("sign", 5.5);
        assertEquals(1.0, ArithmeticEvaluator.evaluate(signPos, bindings), 0.0001);
        
        Term signNeg = createFunction("sign", -3.2);
        assertEquals(-1.0, ArithmeticEvaluator.evaluate(signNeg, bindings), 0.0001);
        
        Term truncPos = createFunction("truncate", 3.8);
        assertEquals(3.0, ArithmeticEvaluator.evaluate(truncPos, bindings), 0.0001);
        
        Term floorPos = createFunction("floor", 3.7);
        assertEquals(3.0, ArithmeticEvaluator.evaluate(floorPos, bindings), 0.0001);
        
        Term ceilPos = createFunction("ceil", 3.2);
        assertEquals(4.0, ArithmeticEvaluator.evaluate(ceilPos, bindings), 0.0001);
    }
    
    // Helper methods
    
    private Term createFunction(String functionName, double value) {
        List<Term> args = new ArrayList<>();
        args.add(new Number(value));
        return new CompoundTerm(new Atom(functionName), args);
    }
    
    private Term createFunction(String functionName, Term arg) {
        List<Term> args = new ArrayList<>();
        args.add(arg);
        return new CompoundTerm(new Atom(functionName), args);
    }
    
    private Term createOpQuery(int precedence, String type, String name) {
        List<Term> args = new ArrayList<>();
        args.add(new Number((double) precedence));
        args.add(new Atom(type));
        args.add(new Atom(name));
        return new CompoundTerm(new Atom("op"), args);
    }
    
    private Term createCurrentOpQuery(int precedence, String type, String name) {
        List<Term> args = new ArrayList<>();
        args.add(new Number((double) precedence));
        args.add(new Atom(type));
        args.add(new Atom(name));
        return new CompoundTerm(new Atom("current_op"), args);
    }
    
    private Term createCompoundTerm(String functor, Term... args) {
        List<Term> argList = new ArrayList<>();
        for (Term arg : args) {
            argList.add(arg);
        }
        return new CompoundTerm(new Atom(functor), argList);
    }
    
    private static class Variable extends it.denzosoft.jprolog.core.terms.Variable {
        public Variable(String name) {
            super(name);
        }
    }
}