package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;
import it.denzosoft.jprolog.terms.Atom;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;







public class Interpreter {

    private final Prolog prologEngine;
    private final Parser parser; // Dependency on the Parser
    private boolean traceEnabled = false;

    public Interpreter() {
        this(new Prolog(), new Parser()); // Use default Prolog and Parser
    }

    public Interpreter(Prolog prologEngine, Parser parser) {
        this.prologEngine = prologEngine;
        this.parser = parser;
        System.out.println("Interpreter initialized with Prolog and Parser.");
    }

    public void loadProgram(List<Clause> clauses) {
        // Convert clauses to string representation for consult
        StringBuilder programBuilder = new StringBuilder();
        for (Clause clause : clauses) {
            programBuilder.append(clause.toString()).append("\n");
        }
        String program = programBuilder.toString();
        prologEngine.consult(program);
        System.out.println("Program loaded with " + clauses.size() + " clauses.");
    }

    public void registerExtension(String name, Extension extension) {
        prologEngine.registerBuiltInPredicate(name, (query, bindings, solutions) -> {
            if (query instanceof CompoundTerm) {
                System.out.println("Executing extension: " + name + " with term: " + query);
                return extension.execute((CompoundTerm) query, bindings);
            }
            System.out.println("Extension " + name + " called with non-compound term: " + query);
            return false;
        });
        System.out.println("Extension " + name + " registered.");
    }

    public void enableTrace() {
        this.traceEnabled = true;
        prologEngine.setTraceEnabled(true);
        System.out.println("Trace enabled.");
    }

    public void disableTrace() {
        this.traceEnabled = false;
        prologEngine.setTraceEnabled(false);
        System.out.println("Trace disabled.");
    }

    public void consult(String programString) {
        try {
            prologEngine.consult(programString); // Use the Parser
            System.out.println("Program consulted successfully.");
        } catch (Exception e) {
            System.err.println("Error parsing program: " + e.getMessage());
        }
    }

    private Clause createClauseFromTerm(CompoundTerm term) {
        //TODO: Implement rule creation and parsing of body term.
        Term head = term.getFunctor();
        List<Term> arguments = term.getArguments();
        Term body = null; // You might need to parse the body from the arguments
        return new Clause(head, body);
    }

    public boolean query(Term query) {
        if (traceEnabled) {
            System.out.println("Query: " + query);
        }
        List<Map<String, Term>> solutions = prologEngine.solve(query);
        boolean result = !solutions.isEmpty(); //Simplified Query Result
        System.out.println("Query " + query + " executed with result: " + result);
        return result;
    }

   public static class SimpleExtension implements Extension {
        @Override
        public boolean execute(CompoundTerm term, Map<String, Term> substitution) {
            // Example: process a term like 'print(X)'
            if (term.getName().equals("print")) {
                List<Term> args = term.getArguments();
                if (args.size() == 1) {
                    Term arg = args.get(0);
                    if (arg instanceof Variable) {
                        Term value = substitution.get(((Variable) arg).getName());
                        System.out.println("Value of " + arg + " is " + (value != null ? value : "unknown"));
                    } else {
                        System.out.println(arg);
                    }
                    return true;
                }
            }
            return false;
        }
    }

    public static void main(String[] args) {

        Interpreter interpreter = new Interpreter();

        Atom father = new Atom("father");
        Variable X = new Variable("X");
        Variable Y = new Variable("Y");
        Atom john = new Atom("john");
        Atom peter = new Atom("peter");

        List<Term> args1 = new ArrayList<>();
        args1.add(X);
        args1.add(john);
        CompoundTerm fatherXJohn = new CompoundTerm(father, args1);

        List<Term> args2 = new ArrayList<>();
        args2.add(peter);
        args2.add(Y);
        CompoundTerm fatherPeterY = new CompoundTerm(father, args2);

        Clause clause = new Clause(fatherXJohn, fatherPeterY);
        interpreter.loadProgram(List.of(clause));

        Variable Z = new Variable("Z");
        List<Term> args3 = new ArrayList<>();
        args3.add(Z);
        args3.add(john);
        CompoundTerm query = new CompoundTerm(father, args3);
        boolean result = interpreter.query(query);
        System.out.println("Query result: " + result);
    }
}
