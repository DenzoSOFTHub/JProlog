package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologParserException;
import it.denzosoft.jprolog.core.parser.TermParser;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;




public class Read implements BuiltInWithContext {

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("read/1 requires exactly 1 argument.");
        }

        Term termVar = query.getArguments().get(0);
        if (!(termVar instanceof Variable)) {
             throw new PrologEvaluationException("read/1 argument must be variable.");
        }

        System.out.print("?- ");
        Scanner scanner = new Scanner(System.in);
        String inputLine = scanner.nextLine().trim();

        if (inputLine.isEmpty()) {
             throw new PrologEvaluationException("read/1: No input provided.");
        }
        if (inputLine.endsWith(".")) {
            inputLine = inputLine.substring(0, inputLine.length() - 1);
        }

        // Use termParser from context if available
        Prolog prolog = solver.getPrologContext();
        if (prolog != null) {
            try {
                // Get the term parser from the Prolog instance
                TermParser termParser = prolog.getTermParser();
                Term parsedTerm = termParser.parseTerm(inputLine);
                if (parsedTerm != null) {
                     Map<String, Term> newBindings = new HashMap<>(bindings);
                     if (termVar.unify(parsedTerm, newBindings)) {
                         solutions.add(newBindings);
                         return true;
                     }
                }
                return false;
            } catch (PrologParserException e) {
                throw new PrologEvaluationException("Error parsing term in read/1: " + e.getMessage(), e);
            }
        } else {
            throw new PrologEvaluationException("Prolog context not available for read/1");
        }
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'read' must be invoked with context");
    }
}
