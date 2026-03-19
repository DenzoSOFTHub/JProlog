// START_CHANGE: ISS-2025-0047 - Implement write_canonical/1 and write_canonical/2
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;

/**
 * write_canonical/1 and write_canonical/2 - ISO Prolog I/O predicates.
 * Writes a term in canonical (functor) form that can be read back with read/1.
 * All operators are written in functional notation: +(1,2) instead of 1+2.
 */
public class WriteCanonical implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();

        if (arity == 1) {
            Term term = query.getArguments().get(0).resolveBindings(bindings);
            return writeCanonicalToCurrentOutput(term, bindings, solutions);
        } else if (arity == 2) {
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            Term term = query.getArguments().get(1).resolveBindings(bindings);
            return writeCanonicalToStream(streamTerm, term, bindings, solutions);
        } else {
            throw new PrologEvaluationException("write_canonical expects 1 or 2 arguments, got " + arity);
        }
    }

    private boolean writeCanonicalToCurrentOutput(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            String currentOutputAlias = StreamManager.getCurrentOutput();
            OutputStream outputStream = StreamManager.getOutputStream(currentOutputAlias);
            if (outputStream == null) {
                throw new PrologEvaluationException("Cannot write to current output stream");
            }
            PrintStream printStream = new PrintStream(outputStream);
            printStream.print(canonicalRepresentation(term));
            printStream.flush();
            solutions.add(bindings);
            return true;
        } catch (Exception e) {
            throw new PrologEvaluationException("I/O error in write_canonical/1: " + e.getMessage());
        }
    }

    private boolean writeCanonicalToStream(Term streamTerm, Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException("write_canonical/2: stream must be an atom");
        }
        try {
            String streamAlias = ((Atom) streamTerm).getName();
            OutputStream outputStream = StreamManager.getOutputStream(streamAlias);
            if (outputStream == null) {
                throw new PrologEvaluationException("Stream does not exist: " + streamAlias);
            }
            PrintStream printStream = new PrintStream(outputStream);
            printStream.print(canonicalRepresentation(term));
            printStream.flush();
            solutions.add(bindings);
            return true;
        } catch (Exception e) {
            throw new PrologEvaluationException("I/O error in write_canonical/2: " + e.getMessage());
        }
    }

    private String canonicalRepresentation(Term term) {
        if (term instanceof Atom) {
            return quoteAtom(((Atom) term).getName());
        } else if (term instanceof Number) {
            return term.toString();
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            StringBuilder sb = new StringBuilder();
            sb.append(quoteAtom(compound.getName()));
            if (compound.getArguments() != null && !compound.getArguments().isEmpty()) {
                sb.append("(");
                for (int i = 0; i < compound.getArguments().size(); i++) {
                    if (i > 0) sb.append(",");
                    sb.append(canonicalRepresentation(compound.getArguments().get(i)));
                }
                sb.append(")");
            }
            return sb.toString();
        } else {
            return term.toString();
        }
    }

    private String quoteAtom(String name) {
        if (name.isEmpty() || !Character.isLowerCase(name.charAt(0)) || !isSimpleAtom(name)) {
            StringBuilder sb = new StringBuilder("'");
            for (char c : name.toCharArray()) {
                if (c == '\'') sb.append("''");
                else sb.append(c);
            }
            sb.append("'");
            return sb.toString();
        }
        return name;
    }

    private boolean isSimpleAtom(String name) {
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (!Character.isLetterOrDigit(c) && c != '_') return false;
        }
        return true;
    }
}
// END_CHANGE: ISS-2025-0047
