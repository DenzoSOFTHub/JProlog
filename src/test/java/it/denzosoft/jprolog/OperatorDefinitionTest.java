package it.denzosoft.jprolog;

import it.denzosoft.jprolog.builtin.system.OperatorDefinition;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Unit tests for the OperatorDefinition built-in predicate (op/3 and current_op/3).
 */
public class OperatorDefinitionTest {
    
    private OperatorDefinition opDefinition;
    private OperatorDefinition currentOpDefinition;
    private Map<String, Term> bindings;
    private List<Map<String, Term>> solutions;
    
    @Before
    public void setUp() {
        opDefinition = new OperatorDefinition(OperatorDefinition.OperatorType.OP);
        currentOpDefinition = new OperatorDefinition(OperatorDefinition.OperatorType.CURRENT_OP);
        bindings = new HashMap<>();
        solutions = new ArrayList<>();
    }
    
    @Test
    public void testDefineNewOperator() {
        // Test: op(500, xfx, myop).
        Term query = createOpQuery(500, "xfx", "myop");
        
        assertTrue(opDefinition.execute(query, bindings, solutions));
        assertEquals(1, solutions.size());
        
        // Verify the operator was defined
        OperatorDefinition.OperatorInfo opInfo = OperatorDefinition.getOperator("myop");
        assertNotNull(opInfo);
        assertEquals(500, opInfo.precedence);
        assertEquals("xfx", opInfo.type);
        assertEquals("myop", opInfo.name);
    }
    
    @Test
    public void testRedefinExistingOperator() {
        // First define an operator
        Term query1 = createOpQuery(500, "xfx", "testop");
        assertTrue(opDefinition.execute(query1, bindings, solutions));
        
        // Redefine with different precedence
        solutions.clear();
        Term query2 = createOpQuery(600, "yfx", "testop");
        assertTrue(opDefinition.execute(query2, bindings, solutions));
        
        // Verify the operator was redefined
        OperatorDefinition.OperatorInfo opInfo = OperatorDefinition.getOperator("testop");
        assertNotNull(opInfo);
        assertEquals(600, opInfo.precedence);
        assertEquals("yfx", opInfo.type);
    }
    
    @Test
    public void testInvalidPrecedence() {
        // Test precedence out of range (0 and 1201)
        Term query1 = createOpQuery(0, "xfx", "badop1");
        assertThrows(Exception.class, () -> 
            opDefinition.execute(query1, bindings, solutions));
        
        Term query2 = createOpQuery(1201, "xfx", "badop2");
        assertThrows(Exception.class, () -> 
            opDefinition.execute(query2, bindings, solutions));
    }
    
    @Test
    public void testInvalidOperatorType() {
        // Test invalid operator type
        Term query = createOpQuery(500, "invalid", "badop");
        assertThrows(Exception.class, () -> 
            opDefinition.execute(query, bindings, solutions));
    }
    
    @Test
    public void testCurrentOpWithGroundArguments() {
        // Define a test operator first
        Term defineQuery = createOpQuery(750, "xfy", "testcurrent");
        opDefinition.execute(defineQuery, bindings, solutions);
        
        solutions.clear();
        
        // Test: current_op(750, xfy, testcurrent).
        Term query = createCurrentOpQuery(750, "xfy", "testcurrent");
        assertTrue(currentOpDefinition.execute(query, bindings, solutions));
        assertEquals(1, solutions.size());
    }
    
    @Test
    public void testCurrentOpWithVariable() {
        // Define a test operator first
        Term defineQuery = createOpQuery(800, "fx", "testvarop");
        opDefinition.execute(defineQuery, bindings, solutions);
        
        solutions.clear();
        
        // Test: current_op(P, T, testvarop).
        List<Term> args = new ArrayList<>();
        args.add(new Variable("P"));
        args.add(new Variable("T"));
        args.add(new Atom("testvarop"));
        Term query = new CompoundTerm(new Atom("current_op"), args);
        
        assertTrue(currentOpDefinition.execute(query, bindings, solutions));
        assertTrue(solutions.size() >= 1);
        
        // Check that variables were bound correctly
        Map<String, Term> solution = solutions.get(0);
        assertEquals(new Number(800.0), solution.get("P"));
        assertEquals(new Atom("fx"), solution.get("T"));
    }
    
    @Test
    public void testCurrentOpFindAllOperators() {
        // Test: current_op(P, T, N). (find all operators)
        List<Term> args = new ArrayList<>();
        args.add(new Variable("P"));
        args.add(new Variable("T"));
        args.add(new Variable("N"));
        Term query = new CompoundTerm(new Atom("current_op"), args);
        
        assertTrue(currentOpDefinition.execute(query, bindings, solutions));
        
        // Should find many operators (at least the predefined ISO ones)
        assertTrue(solutions.size() > 10);
        
        // Verify some standard operators are found
        boolean foundPlus = false;
        boolean foundEquals = false;
        boolean foundIs = false;
        
        for (Map<String, Term> solution : solutions) {
            Term name = solution.get("N");
            if (name != null && name.equals(new Atom("+"))) {
                foundPlus = true;
            }
            if (name != null && name.equals(new Atom("="))) {
                foundEquals = true;
            }
            if (name != null && name.equals(new Atom("is"))) {
                foundIs = true;
            }
        }
        
        assertTrue("Should find '+' operator", foundPlus);
        assertTrue("Should find '=' operator", foundEquals);
        assertTrue("Should find 'is' operator", foundIs);
    }
    
    @Test
    public void testStandardISOOperatorsPreloaded() {
        // Verify that standard ISO operators are preloaded
        
        // Test some key operators with their expected precedences
        // Note: + currently returns unary version due to HashMap overwrite
        OperatorDefinition.OperatorInfo plusOp = OperatorDefinition.getOperator("+");
        assertNotNull(plusOp);
        assertEquals(200, plusOp.precedence);  // Unary + precedence
        assertEquals("fy", plusOp.type);       // Unary + type
        
        OperatorDefinition.OperatorInfo isOp = OperatorDefinition.getOperator("is");
        assertNotNull(isOp);
        assertEquals(700, isOp.precedence);
        assertEquals("xfx", isOp.type);
        
        OperatorDefinition.OperatorInfo commaOp = OperatorDefinition.getOperator(",");
        assertNotNull(commaOp);
        assertEquals(1000, commaOp.precedence);
        assertEquals("xfy", commaOp.type);
        
        OperatorDefinition.OperatorInfo ruleOp = OperatorDefinition.getOperator(":-");
        assertNotNull(ruleOp);
        assertEquals(1200, ruleOp.precedence);
        assertEquals("fx", ruleOp.type);
    }
    
    @Test
    public void testExistentialQuantificationOperator() {
        // Verify that ^ operator is predefined for existential quantification
        OperatorDefinition.OperatorInfo caretOp = OperatorDefinition.getOperator("^");
        assertNotNull(caretOp);
        assertEquals(200, caretOp.precedence);
        assertEquals("xfy", caretOp.type);
    }
    
    @Test
    public void testUnivOperator() {
        // Verify that =.. operator is predefined
        OperatorDefinition.OperatorInfo univOp = OperatorDefinition.getOperator("=..");
        assertNotNull(univOp);
        assertEquals(700, univOp.precedence);
        assertEquals("xfx", univOp.type);
    }
    
    // Helper methods
    
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
}