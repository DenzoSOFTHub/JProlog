package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;

/**
 * Implementation of get_code/1 predicate.
 * 
 * get_code(?Code)
 * 
 * Reads a single character from standard input and unifies Code with its character code.
 * If end of stream is reached, unifies with -1.
 * 
 * Examples:
 * ?- get_code(X).
 * a
 * X = 97.
 * 
 * ?- get_code(-1).
 * % Succeeds if at end of input stream
 */
public class GetCode implements BuiltIn {
    
    // START_CHANGE: ISS-2025-0173 - Make stdin reader final to prevent reassignment and document non-closure
    /** Cached BufferedReader for System.in - must not be closed as that would close System.in */
    private static final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    // END_CHANGE: ISS-2025-0173
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0376 - get_code/2 (ISO 8.12.1): stream-argument form, mirroring GetChar
        int arity = query.getArguments() == null ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("get_code/1 or get_code/2 expected");
        }

        String streamAlias = null;
        Term codeTerm;
        if (arity == 1) {
            codeTerm = query.getArguments().get(0);
        } else {
            Term sTerm = query.getArguments().get(0).resolveBindings(bindings);
            if (!(sTerm instanceof Atom)) {
                throw new PrologEvaluationException("get_code/2: stream must be atom");
            }
            streamAlias = ((Atom) sTerm).getName();
            codeTerm = query.getArguments().get(1);
        }
        // END_CHANGE: ISS-2025-0376
        // START_CHANGE: ISS-2025-0375 - honour set_input/1: get_code/1 (and an explicit current_input)
        // must read from the CURRENT input stream, not always from System.in.
        if (streamAlias == null || "current_input".equals(streamAlias)) {
            String cur = StreamManager.getCurrentInput();
            streamAlias = (cur == null || "user_input".equals(cur)) ? null : cur;
        }
        // END_CHANGE: ISS-2025-0375

        try {
            int charCode;
            // START_CHANGE: ISS-2025-0376 - read via Reader when stream alias known (encoding-aware)
            if (streamAlias != null && !"user_input".equals(streamAlias)) {
                java.io.Reader r = StreamManager.getReader(streamAlias);
                if (r == null) {
                    throw new PrologEvaluationException("existence_error(stream, " + streamAlias + ")");
                }
                charCode = r.read();
                if (charCode == -1) {
                    String eofAction = StreamManager.getProperty(streamAlias, StreamManager.PROP_EOF_ACTION);
                    if ("error".equals(eofAction)) {
                        throw new PrologEvaluationException("permission_error(input, past_end_of_stream, " + streamAlias + ")");
                    }
                } else if (Character.isHighSurrogate((char) charCode)) {
                    // codepoint-aware: combine surrogate pairs into a single code
                    int low = r.read();
                    if (low != -1) {
                        charCode = Character.toCodePoint((char) charCode, (char) low);
                    }
                }
            } else {
                charCode = reader.read();
            }
            // END_CHANGE: ISS-2025-0376
            Term codeValue;

            if (charCode == -1) {
                // End of file
                codeValue = new it.denzosoft.jprolog.core.terms.Number(-1.0);
            } else {
                // Character code as number
                codeValue = new it.denzosoft.jprolog.core.terms.Number((double) charCode);
            }

            // Try to unify
            if (codeTerm.unify(codeValue, bindings)) {
                solutions.add(bindings);
                return true;
            } else {
                return false;
            }

        } catch (IOException e) {
            throw new PrologEvaluationException("get_code: I/O error - " + e.getMessage());
        }
    }
}