package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.extension.example.MathExtensions;
import it.denzosoft.jprolog.core.parser.Parser;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;



public class Main {

    public static void main(String[] args) {
        System.out.println("JProlog Interpreter Demo");
        System.out.println("========================");
        
        // Registra le estensioni matematiche personalizzate
        MathExtensions.registerCustomFunctions();
        
        // Crea Prolog engine
        Prolog prolog = new Prolog();
        
        // Carica alcuni fatti di esempio
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
        
        // Esegui alcune query
        System.out.println("\nRunning queries:");
        
        // Query semplice su fatti
        runQuery(prolog, "father(tom, bob).");
        
        // Query con variabile
        runQuery(prolog, "father(tom, X).");
        
        // Query con regola
        runQuery(prolog, "parent(X, liz).");
        
        // Query con regola ricorsiva
        runQuery(prolog, "grandparent(X, liz).");
        
        // Query aritmetica di base
        runQuery(prolog, "X is 2 + 3 * 4.");
        
        // Query aritmetica con funzioni estese
        System.out.println("\n=== Funzioni Matematiche Estese ===");
        runQuery(prolog, "X is pow(2, 3).");
        runQuery(prolog, "X is max(10, 5).");
        runQuery(prolog, "X is fact(5).");
        
        // Query con collezioni
        prolog.consult("ancestor(X, Y) :- parent(X, Y).");
        prolog.consult("ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).");
        runQuery(prolog, "findall(X, ancestor(tom, X), Ancestors).");
        
        // Dimostrazione nuovi predicati integrati
        System.out.println("\n=== Nuovi Predicati Integrati ===");
        
        // Confronto termini
        runQuery(prolog, "a @< b.");
        runQuery(prolog, "f(a,b) == f(a,b).");
        runQuery(prolog, "f(a,b) \\== f(b,a).");
        
        // Costruzione termini
        runQuery(prolog, "functor(f(a,b), F, A).");
        runQuery(prolog, "arg(2, f(a,b,c), X).");
        runQuery(prolog, "f(a,b) =.. L.");
        runQuery(prolog, "copy_term(f(X,Y), Copy).");
        
        // Confronto aritmetico
        runQuery(prolog, "3 > 2.");
        runQuery(prolog, "1 + 2 =:= 3.");
        runQuery(prolog, "2 * 3 =\\= 7.");
        
        // Dimostrazione conversioni di tipo
        System.out.println("\n=== Dimostrazione Conversioni di Tipo ===");
        
        // Conversione atom_number
        runQuery(prolog, "atom_number('123', N).");
        runQuery(prolog, "atom_number(A, 456.0).");
        runQuery(prolog, "atom_number('3.14', N).");
        
        // Conversione atom_chars
        runQuery(prolog, "atom_chars('hello', L).");
        runQuery(prolog, "atom_chars(A, [w,o,r,l,d]).");
        
        // Conversione number_chars
        runQuery(prolog, "number_chars(789, L).");
        runQuery(prolog, "number_chars(N, ['1','2','.','5']).");
        
        // Esegui test completi dei predicati integrati
        runBuiltInTests(prolog);
        
        // Mostra informazioni sulle estensioni matematiche
        System.out.println("\n=== Estensioni Matematiche ===");
        it.denzosoft.jprolog.extension.example.MathExtensions.demonstrateUsage();
    }
    
    private static void runQuery(Prolog prolog, String queryString) {
        System.out.println("\nQuery: " + queryString);
        try {
            // Rimuovi il punto finale dalla stringa della query
            if (queryString.endsWith(".")) {
                queryString = queryString.substring(0, queryString.length() - 1);
            }

            // Use the string-based solve method to avoid ambiguity
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
            e.printStackTrace(); // Stampa stack trace per debug
        }
    }
    
    private static void runBuiltInTests(Prolog prolog) {
        System.out.println("\n=== Test Predicati Integrati ===");
        
        // Casi di test: {query, expectedSuccess}
        Object[][] testCases = {
            // Unificazione
            {"=(a, a)", true},
            {"=(a, b)", false},
            {"=(X, a)", true},
            
            // Unificazione con controllo occorrenze
            {"unify_with_occurs_check(a, a)", true},
            {"unify_with_occurs_check(a, b)", false},
            {"unify_with_occurs_check(X, a)", true},
            
            // Controllo tipi
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
            
            // Confronto termini
            {"a @< b", true},
            {"b @< a", false},
            {"f(a,b) == f(a,b)", true},
            {"f(a,b) == f(b,a)", false},
            {"f(a,b) \\== f(b,a)", true},
            {"f(a,b) \\== f(a,b)", false},
            
            // Costruzione termini
            {"functor(f(a,b), f, 2)", true},
            {"functor(a, a, 0)", true},
            {"arg(2, f(a,b,c), b)", true},
            {"arg(1, f(a,b,c), a)", true},
            {"f(a,b) =.. [f,a,b]", true},
            
            // Confronto aritmetico
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
            {"select(b, [a,b,c], [a,c])", true},
            
            // Conversioni di tipo
            {"atom_number('123', 123.0)", true},
            {"atom_number('3.14', 3.14)", true},
            {"atom_chars('hello', [h,e,l,l,o])", true},
            {"number_chars(123, ['1','2','3'])", true},
            {"number_chars(3.14, ['3','.','1','4'])", true}
        };
        
        int passed = 0;
        int failed = 0;
        
        Parser parser = new Parser();
        
        for (Object[] testCase : testCases) {
            String query = (String) testCase[0];
            boolean expected = (Boolean) testCase[1];
            
            try {
                // Use the string-based solve method to avoid ambiguity
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
        
        System.out.println("\n=== Risultati Test ===");
        System.out.println("Passed: " + passed);
        System.out.println("Failed: " + failed);
        System.out.println("Total : " + (passed + failed));
    }
}
