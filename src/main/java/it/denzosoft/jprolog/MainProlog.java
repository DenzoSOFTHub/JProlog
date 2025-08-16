package it.denzosoft.jprolog;

public class MainProlog {

    public static void main(String[] args) {
        Interpreter interpreter = new Interpreter();
        interpreter.enableTrace();
        
        // Load facts and rules properly using string-based approach
        interpreter.consult("father(john, george).");
        interpreter.consult("parent(X, Y) :- father(X, Y).");
        
        // Create a simple query term for parent(X, Y)
        interpreter.consult("test_query :- parent(X, Y), write('X='), write(X), write(', Y='), write(Y), nl.");
        
        // Execute the test query
        boolean result = interpreter.query(new it.denzosoft.jprolog.terms.Atom("test_query"));
        System.out.println("Query result: " + result);
    }
}
