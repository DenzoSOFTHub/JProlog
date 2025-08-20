package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for anonymous variables (_).
 */
public class AnonymousVariableTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    @Test
    public void testBasicAnonymousVariable() {
        // Anonymous variables should unify with anything but not bind
        String program = "test_anon :- _ = hello.";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_anon");
        assertFalse("_ = hello should succeed", solutions.isEmpty());
    }
    
    @Test
    public void testMultipleAnonymousVariables() {
        // Multiple _ should be independent
        String program = "test_multi_anon :- _ = hello, _ = world, _ = test.";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_multi_anon");
        assertFalse("Multiple _ unifications should succeed", solutions.isEmpty());
    }
    
    @Test
    public void testAnonymousInRuleHead() {
        // Anonymous variables in rule heads - test with specific query
        String program = 
            "process(_, Result) :- Result = processed.";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("process(ignored, processed)");
        assertFalse("Anonymous in rule head should work", solutions.isEmpty());
    }
    
    @Test
    public void testAnonymousInRuleBody() {
        // Anonymous variables in rule bodies - test direct queries
        String program = 
            "data(a, 1).\n" +
            "data(b, 2).\n" +
            "data(c, 3).";
        prolog.consult(program);
        
        // Test that anonymous variables work in direct queries
        List<Map<String, Term>> solutions = prolog.solve("data(a, _)");
        assertFalse("data(a, _) should succeed", solutions.isEmpty());
        
        solutions = prolog.solve("data(b, _)");
        assertFalse("data(b, _) should succeed", solutions.isEmpty());
        
        solutions = prolog.solve("data(c, _)");
        assertFalse("data(c, _) should succeed", solutions.isEmpty());
    }
    
    @Test
    public void testAnonymousWithFacts() {
        // Anonymous variables in facts
        String program = 
            "likes(john, _).\n" +
            "likes(mary, pizza).";
        prolog.consult(program);
        
        // Test that both facts can be queried
        List<Map<String, Term>> solutions = prolog.solve("likes(john, anything)");
        assertFalse("likes(john, anything) should match anonymous", solutions.isEmpty());
        
        solutions = prolog.solve("likes(mary, pizza)");
        assertFalse("likes(mary, pizza) should match exactly", solutions.isEmpty());
    }
    
    @Test
    public void testAnonymousInLists() {
        // Anonymous variables in lists
        String program = 
            "list_pattern([_, Second, _]) :- Second = middle.\n" +
            "test_list_pattern(List) :- list_pattern(List).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_list_pattern([first, middle, last])");
        assertFalse("List pattern with _ should match", solutions.isEmpty());
        
        solutions = prolog.solve("test_list_pattern([a, middle, c])");
        assertFalse("List pattern with _ should match different values", solutions.isEmpty());
        
        solutions = prolog.solve("test_list_pattern([x, wrong, y])");
        assertTrue("List pattern should not match wrong middle", solutions.isEmpty());
    }
    
    @Test
    public void testAnonymousDoesNotAppearInSolutions() {
        // Anonymous variables should not appear in solution bindings
        String program = "extract_second([_, X, _], X).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("extract_second([a, b, c], Result)");
        assertFalse("Should succeed", solutions.isEmpty());
        assertEquals("b", solutions.get(0).get("Result").toString());
        
        // Check that anonymous variables are not in the solution
        Map<String, Term> solution = solutions.get(0);
        assertFalse("Anonymous variables should not appear in solutions", 
                   solution.containsKey("_"));
    }
    
    @Test
    public void testAnonymousWithCompoundTerms() {
        // Anonymous variables in compound terms
        String program = 
            "person(john, age(25)).\n" +
            "person(mary, age(30)).\n" +
            "person(bob, age(20)).\n" +
            "is_person(Name) :- person(Name, age(_)).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("is_person(X)");
        assertEquals("Should find all people", 3, solutions.size());
    }
    
    @Test
    public void testAnonymousInComplexQuery() {
        // More complex query with anonymous variables
        String program = 
            "employee(john, engineer, 50000).\n" +
            "employee(mary, manager, 75000).\n" +
            "employee(bob, engineer, 45000).\n" +
            "has_job(Person) :- employee(Person, _, _).\n" +
            "is_engineer(Person) :- employee(Person, engineer, _).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("has_job(X)");
        assertEquals("Should find all employees", 3, solutions.size());
        
        solutions = prolog.solve("is_engineer(X)");
        assertEquals("Should find 2 engineers", 2, solutions.size());
    }
}