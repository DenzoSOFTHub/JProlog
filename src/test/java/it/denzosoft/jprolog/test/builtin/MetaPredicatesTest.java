package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for ISO Prolog meta-predicates.
 */
public class MetaPredicatesTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    @Test
    public void testCallWithAtom() {
        // Test call/1 with simple atom
        String program = "test_fact. test_call :- call(test_fact).";
        prolog.consult(program);
        
        // ISS-2025-0663: exact answer counts ("not empty" held for any number of answers)
        assertEquals("call/1 should execute the goal", 1, prolog.solve("test_call").size());
        assertEquals(0, prolog.solve("call(fail)").size());
    }
    
    @Test
    public void testCallWithCompound() {
        // Test call/1 with compound term
        String program = 
            "parent(john, mary).\n" +
            "test_call(X) :- call(parent(john, X)).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_call(Child)");
        assertFalse("Should find solution", solutions.isEmpty());
        assertEquals("mary", solutions.get(0).get("Child").toString());
    }
    
    @Test
    public void testCallWithVariable() {
        // Test call/1 with variable goal
        String program = 
            "fact_a.\n" +
            "fact_b.\n" +
            "test_call(Goal) :- Goal = fact_a, call(Goal).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_call(G)");
        assertFalse("Should execute variable goal", solutions.isEmpty());
        assertEquals(1, solutions.size());
        assertEquals("fact_a", solutions.get(0).get("G").toString());
    }
    
    @Test
    public void testCallWithExtraArgs() {
        // START_CHANGE: ISS-2025-0085 - Test call/N with user-defined predicate
        // Test call/2 and call/3 with extra arguments
        String program =
            "my_add(X, Y, Z) :- Z is X + Y.\n" +
            "test_call2(Result) :- call(my_add(1), 2, Result).\n" +
            "test_call1 :- call(my_add(1, 2, 3)).";
        prolog.consult(program);

        // Test call/3: call(my_add(1), 2, Result) -> my_add(1, 2, Result)
        List<Map<String, Term>> solutions = prolog.solve("test_call2(R)");
        assertFalse("call/3 should work", solutions.isEmpty());
        String result = solutions.get(0).get("R").toString();
        assertEquals("call/3 result is the integer 3", 1, prolog.solve("test_call2(R), R == 3").size());

        // Test call/1: call(my_add(1, 2, 3)) -> my_add(1, 2, 3)
        solutions = prolog.solve("test_call1");
        assertFalse("call/1 should work", solutions.isEmpty());
        // END_CHANGE: ISS-2025-0085
    }
    
    @Test
    public void testOnceSucceedsOnce() {
        // Test that once/1 succeeds at most once - simplified
        String program = 
            "multi(1).\n" +
            "multi(2).\n" +
            "multi(3).\n" +
            "test_once(X) :- once(multi(X)).";
        prolog.consult(program);
        
        // ISS-2025-0663: the goal must be NONDETERMINISTIC for the test to mean anything
        // (once(multi(1)) has one answer with or without once/1)
        assertEquals("multi/1 alone has three answers", 3, prolog.solve("multi(X)").size());
        List<Map<String, Term>> solutions = prolog.solve("test_once(X)");
        assertEquals("once/1 should return only one solution", 1, solutions.size());
        assertEquals("the first one", 1, prolog.solve("test_once(X), X == 1").size());
    }
    
    @Test
    public void testOnceWithFail() {
        // Test that once/1 fails if goal fails
        String program = "test_once_fail :- once(fail).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_once_fail");
        assertTrue("once(fail) should fail", solutions.isEmpty());
    }
    
    @Test
    public void testIgnoreSuccess() {
        // Test that ignore/1 succeeds even when goal succeeds
        String program = "test_ignore_success :- ignore(true).";
        prolog.consult(program);
        
        assertEquals("ignore(true) should succeed once", 1, prolog.solve("test_ignore_success").size());
        assertEquals("ignore/1 keeps the goal's bindings", 1, prolog.solve("ignore(X = 1), X == 1").size());
    }
    
    @Test
    public void testIgnoreFail() {
        // Test that ignore/1 succeeds even when goal fails
        String program = "test_ignore_fail :- ignore(fail).";
        prolog.consult(program);
        
        assertEquals("ignore(fail) should succeed once", 1, prolog.solve("test_ignore_fail").size());
    }
    
    @Test
    public void testIgnoreWithSideEffects() {
        // Test that ignore/1 executes goal for side effects - simplified
        String program = 
            ":- dynamic(seen/1).\n" +
            "test_ignore :- ignore((member(X, [a, b]), assertz(seen(X)), X == b)).";
        prolog.consult(program);
        
        // ISS-2025-0663: the side effects happen, and ignore/1 commits to the first success
        assertEquals("Should succeed once with ignore", 1, prolog.solve("test_ignore").size());
        assertEquals(1, prolog.solve("findall(X, seen(X), L), L == [a, b]").size());
    }
    
    @Test
    public void testForallSuccess() {
        // Test forall/2 succeeds when condition holds for all
        String program = 
            "number_fact(1).\n" +
            "number_fact(2).\n" +
            "number_fact(3).\n" +
            "positive(X) :- X > 0.\n" +
            "test_forall :- forall(number_fact(X), positive(X)).";
        prolog.consult(program);
        
        assertEquals("forall should succeed once when all satisfy condition", 1, prolog.solve("test_forall").size());
    }
    
    @Test
    public void testForallFail() {
        // Test forall/2 fails when condition doesn't hold for all
        String program = 
            "number_fact(1).\n" +
            "number_fact(-2).\n" +
            "number_fact(3).\n" +
            "positive(X) :- X > 0.\n" +
            "test_forall :- forall(number_fact(X), positive(X)).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_forall");
        assertTrue("forall should fail when not all satisfy", solutions.isEmpty());
    }
    
    @Test
    public void testForallEmptyCondition() {
        // Test forall/2 succeeds when condition generates no solutions
        String program = "test_forall_empty :- forall(fail, write(never)).";
        prolog.consult(program);
        
        assertEquals("forall with no solutions should succeed", 1, prolog.solve("test_forall_empty").size());
    }
    
    @Test
    public void testCallNested() {
        // Test nested call predicates
        String program = 
            "goal(test).\n" +
            "test.\n" +
            "test_nested :- call(call(goal(G))), call(G).";
        prolog.consult(program);
        
        assertEquals("Nested calls should work", 1, prolog.solve("test_nested").size());
        assertEquals(1, prolog.solve("call(call(goal(G))), G == test").size());
    }
}