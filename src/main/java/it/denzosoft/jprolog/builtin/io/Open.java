package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * open/3 - open(+File, +Mode, -Stream)
 * open/4 - open(+File, +Mode, -Stream, +Options)
 *
 * Options (v2.8.3): alias(Name), type(text|binary), encoding(...),
 *                   eof_action(error|eof_code|reset), reposition(true|false).
 * Currently alias/1 is registered with StreamManager; other options are
 * parsed and stored as stream properties but not all are enforced at runtime.
 */
public class Open implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 3 && arity != 4) {
            throw new PrologEvaluationException("open/3 or open/4 expected.");
        }

        Term fileTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term modeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term streamTerm = query.getArguments().get(2);

        if (!(fileTerm instanceof Atom)) {
            throw new PrologEvaluationException("open: File must be an atom.");
        }
        if (!(modeTerm instanceof Atom)) {
            throw new PrologEvaluationException("open: Mode must be an atom.");
        }

        String filename = ((Atom) fileTerm).getName();
        String mode = ((Atom) modeTerm).getName();

        // START_CHANGE: R3 - parse all open/4 options
        String aliasName = null;
        String typeOpt = null;
        String encodingOpt = null;
        String eofActionOpt = null;
        if (arity == 4) {
            Term optsTerm = query.getArguments().get(3).resolveBindings(bindings);
            List<Term> opts = ListUtils.extractElements(optsTerm);
            if (opts != null) {
                for (Term opt : opts) {
                    if (opt instanceof CompoundTerm) {
                        CompoundTerm c = (CompoundTerm) opt;
                        String n = c.getName();
                        if (c.getArguments() == null || c.getArguments().size() != 1) continue;
                        Term v = c.getArguments().get(0);
                        if (!(v instanceof Atom)) continue;
                        String vs = ((Atom) v).getName();
                        switch (n) {
                            case "alias": aliasName = vs; break;
                            case "type": typeOpt = vs; break;
                            case "encoding": encodingOpt = vs; break;
                            case "eof_action": eofActionOpt = vs; break;
                            default: break;
                        }
                    }
                }
            }
        }
        // END_CHANGE: R3

        try {
            String streamAlias = StreamManager.openStream(filename, mode);
            // START_CHANGE: R3 - register user alias + record stream properties
            String userAlias = (aliasName != null) ? aliasName : streamAlias;
            if (aliasName != null) {
                StreamManager.aliasStream(streamAlias, aliasName);
            }
            if (typeOpt != null) {
                StreamManager.setProperty(streamAlias, StreamManager.PROP_TYPE, typeOpt);
                StreamManager.setProperty(userAlias, StreamManager.PROP_TYPE, typeOpt);
            }
            if (encodingOpt != null) {
                StreamManager.setProperty(streamAlias, StreamManager.PROP_ENCODING, encodingOpt);
                StreamManager.setProperty(userAlias, StreamManager.PROP_ENCODING, encodingOpt);
            }
            if (eofActionOpt != null) {
                StreamManager.setProperty(streamAlias, StreamManager.PROP_EOF_ACTION, eofActionOpt);
                StreamManager.setProperty(userAlias, StreamManager.PROP_EOF_ACTION, eofActionOpt);
            }
            Atom streamAtom = new Atom(userAlias);
            // END_CHANGE: R3

            Term resolvedStreamTerm = streamTerm.resolveBindings(bindings);
            Map<String, Term> newBindings = new HashMap<>(bindings);

            if (resolvedStreamTerm.unify(streamAtom, newBindings)) {
                solutions.add(newBindings);
                return true;
            } else {
                StreamManager.closeStream(streamAlias);
                return false;
            }
        } catch (IOException e) {
            throw new PrologEvaluationException("open: Failed to open file '" + filename + "': " + e.getMessage());
        }
    }
}