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
 * Implementation of get_char/1 predicate.
 * 
 * get_char(?Char)
 * 
 * Reads a single character from standard input and unifies it with Char.
 * If end of stream is reached, unifies with the atom 'end_of_file'.
 * 
 * Examples:
 * ?- get_char(X).
 * a
 * X = a.
 * 
 * ?- get_char(end_of_file).
 * % Succeeds if at end of input stream
 */
public class GetChar implements BuiltIn {
    
    // START_CHANGE: ISS-2025-0173 - Make stdin reader final to prevent reassignment and document non-closure
    /** Cached BufferedReader for System.in - must not be closed as that would close System.in */
    private static final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    // END_CHANGE: ISS-2025-0173
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments() == null ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("get_char/1 or get_char/2 expected");
        }

        // START_CHANGE: R3 - get_char/2 dispatches to named stream with eof_action + encoding support
        String streamAlias = null;
        Term charTerm;
        if (arity == 1) {
            charTerm = query.getArguments().get(0);
        } else {
            Term sTerm = query.getArguments().get(0).resolveBindings(bindings);
            if (!(sTerm instanceof Atom)) {
                throw new PrologEvaluationException("get_char/2: stream must be atom");
            }
            streamAlias = ((Atom) sTerm).getName();
            charTerm = query.getArguments().get(1);
        }
        // END_CHANGE: R3

        try {
            int charCode;
            // START_CHANGE: R3 - read via Reader when stream alias known (encoding-aware)
            if (streamAlias != null && !"user_input".equals(streamAlias) && !"current_input".equals(streamAlias)) {
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
                    // default eof_code: bind end_of_file atom
                }
            } else {
                charCode = reader.read();
            }
            // END_CHANGE: R3

            Term charValue;
            if (charCode == -1) {
                charValue = new Atom("end_of_file");
            } else {
                // codepoint-aware: high surrogate handling
                if (Character.isHighSurrogate((char) charCode)) {
                    java.io.Reader r2 = (streamAlias != null) ? StreamManager.getReader(streamAlias) : reader;
                    int low = r2 != null ? r2.read() : -1;
                    if (low != -1) {
                        int cp = Character.toCodePoint((char) charCode, (char) low);
                        charValue = new Atom(new String(Character.toChars(cp)));
                    } else {
                        charValue = new Atom(String.valueOf((char) charCode));
                    }
                } else {
                    charValue = new Atom(String.valueOf((char) charCode));
                }
            }

            if (charTerm.unify(charValue, bindings)) {
                solutions.add(bindings);
                return true;
            }
            return false;

        } catch (IOException e) {
            throw new PrologEvaluationException("get_char: I/O error - " + e.getMessage());
        }
    }
}