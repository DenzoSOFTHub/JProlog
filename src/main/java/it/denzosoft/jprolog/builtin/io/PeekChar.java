package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.io.PushbackReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * peek_char/1 - ISO Prolog I/O predicate
 * Reads the next character from the current input stream without consuming it.
 * peek_char/2 - peek_char(+Stream, ?Char): stream-argument form (ISO 8.12.2).
 */
public class PeekChar implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0376 - peek_char/2 (ISO 8.12.2): stream-argument form
        int arity = query.getArguments() == null ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("peek_char/1 or peek_char/2 expected");
        }

        String streamAlias = null;
        Term charTerm;
        if (arity == 1) {
            charTerm = query.getArguments().get(0);
        } else {
            Term sTerm = query.getArguments().get(0).resolveBindings(bindings);
            if (!(sTerm instanceof Atom)) {
                throw new PrologEvaluationException("peek_char/2: stream must be atom");
            }
            streamAlias = ((Atom) sTerm).getName();
            charTerm = query.getArguments().get(1);
        }
        if (streamAlias == null || "current_input".equals(streamAlias)) {
            streamAlias = StreamManager.getCurrentInput();
        }
        // END_CHANGE: ISS-2025-0376

        try {
            // START_CHANGE: ISS-2025-0376 - named streams peek through the SAME PushbackReader that
            // get_char/2 and get_code/2 read from (StreamManager.getReader), so a peek followed by a
            // get sees the identical character. The legacy PushbackInputStream path is kept only for
            // user_input (stdin).
            if (streamAlias != null && !"user_input".equals(streamAlias)) {
                java.io.Reader r = StreamManager.getReader(streamAlias);
                if (r == null) {
                    throw new PrologEvaluationException("existence_error(stream, " + streamAlias + ")");
                }
                int charCode = r.read();
                Term charValue;
                if (charCode == -1) {
                    charValue = new Atom("end_of_file");
                } else if (Character.isHighSurrogate((char) charCode) && r instanceof PushbackReader) {
                    int low = r.read();
                    if (low != -1) {
                        ((PushbackReader) r).unread(low);
                        ((PushbackReader) r).unread(charCode);
                        int cp = Character.toCodePoint((char) charCode, (char) low);
                        charValue = new Atom(new String(Character.toChars(cp)));
                    } else {
                        ((PushbackReader) r).unread(charCode);
                        charValue = new Atom(String.valueOf((char) charCode));
                    }
                } else {
                    if (r instanceof PushbackReader) {
                        ((PushbackReader) r).unread(charCode);
                    }
                    charValue = new Atom(String.valueOf((char) charCode));
                }
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (charTerm.unify(charValue, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
                return false;
            }
            // END_CHANGE: ISS-2025-0376

            String currentInputAlias = (streamAlias != null) ? streamAlias : StreamManager.getCurrentInput();
            InputStream inputStream = StreamManager.getInputStream(currentInputAlias);

            if (inputStream == null) {
                throw new PrologEvaluationException("Cannot peek from current input stream: " + currentInputAlias);
            }

            // START_CHANGE: ISS-2025-0193 - Wrap and register PushbackInputStream for reuse
            PushbackInputStream pushbackStream;
            if (inputStream instanceof PushbackInputStream) {
                pushbackStream = (PushbackInputStream) inputStream;
            } else {
                pushbackStream = new PushbackInputStream(inputStream);
                StreamManager.registerInputStream(currentInputAlias, pushbackStream);
            }
            // END_CHANGE: ISS-2025-0193

            int charCode = pushbackStream.read();

            if (charCode == -1) {
                // End of stream
                Atom endOfFileAtom = new Atom("end_of_file");
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (charTerm.unify(endOfFileAtom, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                } else {
                    return false;
                }
            } else {
                // Push the character back
                pushbackStream.unread(charCode);

                char ch = (char) charCode;
                Atom charAtom = new Atom(String.valueOf(ch));

                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (charTerm.unify(charAtom, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                } else {
                    return false;
                }
            }

        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in peek_char: " + e.getMessage());
        }
    }
}
