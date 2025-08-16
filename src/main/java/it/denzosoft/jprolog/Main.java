package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;


public class Main {

    public static void main(String[] args) {
        System.out.println("JProlog Interpreter Demo");
        System.out.println("========================");
        
        // Create Prolog engine
        Prolog prolog = new Prolog();
        
        // Load some sample rules
        try {
            System.out.println("\nLoading facts:");
            prolog.consult("father(tom, bob).");
            prolog.consult("father(bob, liz).");
            prolog.consult("mother(ann, bob).");
            prolog.consult("mother(bob, liz).");
            
            System.out.println("\nLoading rules:");
            prolog.consult("parent(X, Y) :- father(X, Y).");
            prolog.consult("parent(X, Y) :- mother(X, Y).");
            prolog.consult("grandparent(X, Z) :- parent(X, Y), parent(Y, Z).");
        } catch (Exception e) {
            System.err.println("Error during loading: " + e.getMessage());
            e.printStackTrace();
            return;
        }
        
        System.out.println("\nKnowledge Base loaded:");
        for (Rule rule : prolog.getRules()) {
            System.out.println("  " + rule);
        }
        
        // Run some queries
        System.out.println("\nRunning queries:");
        
        // Simple fact query
        runQuery(prolog, "father(tom, bob).");
        
        // Variable query
        runQuery(prolog, "father(tom, X).");
        
        // Rule query
        runQuery(prolog, "parent(X, liz).");
        
        // Recursive rule query
        runQuery(prolog, "grandparent(X, liz).");
        
        // Arithmetic query
        runQuery(prolog, "X is 2 + 3 * 4.");
        
        // Collection query
        prolog.consult("ancestor(X, Y) :- parent(X, Y).");
        prolog.consult("ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).");
        runQuery(prolog, "findall(X, ancestor(tom, X), Ancestors).");
        
        // New predicates demonstration
        System.out.println("\n=== New Built-ins Demonstration ===");
        
        // Term comparison
        runQuery(prolog, "a @< b.");
        runQuery(prolog, "f(a,b) == f(a,b).");
        runQuery(prolog, "f(a,b) \\== f(b,a).");
        
        // Term construction
        runQuery(prolog, "functor(f(a,b), F, A).");
        runQuery(prolog, "arg(2, f(a,b,c), X).");
        runQuery(prolog, "f(a,b) =.. L.");
        runQuery(prolog, "copy_term(f(X,Y), Copy).");
        
        // Arithmetic comparison
        runQuery(prolog, "3 > 2.");
        runQuery(prolog, "1 + 2 =:= 3.");
        runQuery(prolog, "2 * 3 =\\= 7.");
        
        // Run comprehensive built-in tests
        runBuiltInTests(prolog);
    }
    
    private static void runQuery(Prolog prolog, String queryString) {
        System.out.println("\nQuery: " + queryString);
        try {
            // Remove the trailing period from the query string
            if (queryString.endsWith(".")) {
                queryString = queryString.substring(0, queryString.length() - 1);
            }

            List<Map<String, Term>> solutions = prolog.solve(queryString);
            if (solutions.isEmpty()) {
                System.out.println("  No solutions found.");
            } else {
                System.out.println("  Solutions:");
                for (int i = 0; i < solutions.size(); i++) {
                    Map<String, Term> solution = solutions.get(i);
                    System.out.println("    Solution " + (i+1) + ": " + solution);
                }
            }
        } catch (Exception e) {
            System.err.println("  Error: " + e.getMessage());
            e.printStackTrace(); // Print stack trace for debugging
        }
    }
    
    private static void runBuiltInTests(Prolog prolog) {
        System.out.println("\n=== Built-in Predicates Tests ===");
        
        // Test cases: {query, expectedSuccess}
        Object[][] testCases = {
            // Unification
            {"=(a, a)", true},
            {"=(a, b)", false},
            {"=(X, a)", true},
            
            // Unify with occurs check
            {"unify_with_occurs_check(a, a)", true},
            {"unify_with_occurs_check(a, b)", false},
            {"unify_with_occurs_check(X, a)", true},
            
            // Type checking
            {"var(X)", true},
            {"var(a)", false},
            {"nonvar(a)", true},
            {"nonvar(X)", false},
            {"atom(hello)", true},
            {"atom(42)", false},
            {"integer(42)", true},
            {"integer(3.14)", false},
            {"integer(hello)", false},
            {"float(3.14)", true},
            {"float(42)", false},
            {"float(hello)", false},
            {"atomic(hello)", true},
            {"atomic(42)", true},
            {"atomic(f(a))", false},
            {"compound(f(a))", true},
            {"compound(hello)", false},
            {"number(42)", true},
            {"number(3.14)", true},
            {"number(hello)", false},
            
            // Term comparison
            {"a @< b", true},
            {"b @< a", false},
            {"f(a,b) == f(a,b)", true},
            {"f(a,b) == f(b,a)", false},
            {"f(a,b) \\== f(b,a)", true},
            {"f(a,b) \\== f(a,b)", false},
            
            // Term construction
            {"functor(f(a,b), f, 2)", true},
            {"functor(a, a, 0)", true},
            {"arg(2, f(a,b,c), b)", true},
            {"arg(1, f(a,b,c), a)", true},
            {"f(a,b) =.. [f,a,b]", true},
            
            // Arithmetic comparison
            {"3 > 2", true},
            {"2 > 3", false},
            {"3 >= 3", true},
            {"1 + 2 =:= 3", true},
            {"1 + 2 =:= 4", false},
            {"2 * 3 =\\= 7", true},
            {"2 * 3 =\\= 6", false},
            
            // List operations
            {"append([a,b], [c,d], [a,b,c,d])", true},
            {"append([a,b], [c,d], [a,b,c,e])", false},
            {"length([a,b,c], 3)", true},
            {"length([a,b,c], 4)", false},
            {"member(b, [a,b,c])", true},
            {"member(d, [a,b,c])", false},
            {"nth0(1, [a,b,c], b)", true},
            {"nth0(0, [a,b,c], a)", true},
            {"nth1(2, [a,b,c], b)", true},
            {"nth1(1, [a,b,c], a)", true},
            {"msort([c,b,a,c,b], [a,b,b,c,c])", true},
            {"reverse([a,b,c], [c,b,a])", true},
            {"select(b, [a,b,c], [a,c])", true}
        };
        
        int passed = 0;
        int failed = 0;
        
        for (Object[] testCase : testCases) {
            String query = (String) testCase[0];
            boolean expected = (Boolean) testCase[1];
            
            try {
                List<Map<String, Term>> solutions = prolog.solve(query);
                boolean actual = !solutions.isEmpty();
                
                if (actual == expected) {
                    System.out.println("PASSED: " + query);
                    passed++;
                } else {
                    System.out.println("FAILED: " + query + " (expected " + expected + ", got " + actual + ")");
                    failed++;
                }
            } catch (Exception e) {
                System.out.println("ERROR : " + query + " (" + e.getMessage() + ")");
                failed++;
            }
        }
        
        System.out.println("\n=== Test Results ===");
        System.out.println("Passed: " + passed);
        System.out.println("Failed: " + failed);
        System.out.println("Total : " + (passed + failed));
    }
}
