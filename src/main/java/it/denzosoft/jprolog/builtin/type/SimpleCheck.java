package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.PrologString;

import java.util.List;
import java.util.Map;

/**
 * simple/1 - Succeeds if the argument is a simple term.
 * A simple term is atomic: an atom, number, or string (not compound or variable).
 */
public class SimpleCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("simple/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);
        
        // A term is simple if it's atomic (atom, number, or string)
        boolean isSimple = (termArg instanceof Atom) || 
                          (termArg instanceof Number) ||
                          (termArg instanceof PrologString);

        if (isSimple) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
}