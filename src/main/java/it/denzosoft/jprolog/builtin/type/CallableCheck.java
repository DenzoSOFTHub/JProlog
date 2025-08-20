package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * callable/1 - Succeeds if the argument is a callable term.
 * A term is callable if it is an atom or compound term.
 */
public class CallableCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("callable/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);
        
        // A term is callable if it's an atom or compound term
        boolean isCallable = (termArg instanceof Atom) || (termArg instanceof CompoundTerm);

        if (isCallable) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
}