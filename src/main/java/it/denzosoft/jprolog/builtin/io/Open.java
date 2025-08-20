package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * open/3 - open(+File, +Mode, -Stream)
 * Opens a file and returns a stream handle.
 */
public class Open implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("open/3 requires exactly 3 arguments: open(+File, +Mode, -Stream).");
        }

        Term fileTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term modeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term streamTerm = query.getArguments().get(2);

        if (!(fileTerm instanceof Atom)) {
            throw new PrologEvaluationException("open/3: File must be an atom.");
        }

        if (!(modeTerm instanceof Atom)) {
            throw new PrologEvaluationException("open/3: Mode must be an atom.");
        }

        String filename = ((Atom) fileTerm).getName();
        String mode = ((Atom) modeTerm).getName();

        try {
            String streamAlias = StreamManager.openStream(filename, mode);
            Atom streamAtom = new Atom(streamAlias);
            
            // Try to unify the stream term with the stream atom
            Term resolvedStreamTerm = streamTerm.resolveBindings(bindings);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (resolvedStreamTerm.unify(streamAtom, newBindings)) {
                solutions.add(newBindings);
                return true;
            } else {
                // Failed to unify
                StreamManager.closeStream(streamAlias); // Clean up
                return false;
            }
        } catch (IOException e) {
            throw new PrologEvaluationException("open/3: Failed to open file '" + filename + "': " + e.getMessage());
        }
    }
}