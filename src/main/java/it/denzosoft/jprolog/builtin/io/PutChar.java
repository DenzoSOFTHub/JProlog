package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

/**
 * Implementation of put_char/1 predicate.
 * 
 * put_char(+Char)
 * 
 * Writes a single character to standard output.
 * Char must be an atom representing a single character.
 * 
 * Examples:
 * ?- put_char(a).
 * a
 * true.
 * 
 * ?- put_char('\\n').
 * 
 * true.
 */
public class PutChar implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0373 - put_char/2 (ISO 8.12.3): write the char to the given stream
        int arity = (query.getArguments() == null) ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("put_char/1 or put_char/2 expected");
        }

        java.io.PrintStream out;
        Term charTerm;
        if (arity == 1) {
            out = StreamManager.out();
            charTerm = query.getArguments().get(0).resolveBindings(bindings);
        } else {
            out = IOStreamUtils.resolveOutputStream(query.getArguments().get(0), bindings, "put_char/2");
            charTerm = query.getArguments().get(1).resolveBindings(bindings);
        }
        // END_CHANGE: ISS-2025-0373

        if (charTerm instanceof Variable) {
            return false; // Fail silently for unbound variables
        }

        if (!(charTerm instanceof Atom)) {
            return false; // Fail silently for non-atoms
        }

        String charString = ((Atom) charTerm).getName();

        if (charString.length() != 1) {
            return false; // Fail silently for multi-character atoms
        }

        // START_CHANGE: ISS-2025-0373 - write to the resolved stream
        out.print(charString);
        out.flush();
        // END_CHANGE: ISS-2025-0373

        solutions.add(bindings);
        return true;
    }
}