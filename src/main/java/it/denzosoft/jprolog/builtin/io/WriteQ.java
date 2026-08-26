package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;

/**
 * writeq/1 and writeq/2 - ISO Prolog I/O predicates
 * Writes a term to output using quoted representation when necessary.
 */
public class WriteQ implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        
        if (arity == 1) {
            // writeq/1 - write to current output
            Term termToWrite = query.getArguments().get(0).resolveBindings(bindings);
            return writeQuotedToCurrentOutput(termToWrite, bindings, solutions);
        } else if (arity == 2) {
            // writeq/2 - write to specified stream
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            Term termToWrite = query.getArguments().get(1).resolveBindings(bindings);
            return writeQuotedToStream(streamTerm, termToWrite, bindings, solutions);
        } else {
            throw new PrologEvaluationException("writeq expects 1 or 2 arguments, got " + arity);
        }
    }

    private boolean writeQuotedToCurrentOutput(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            String currentOutputAlias = StreamManager.getCurrentOutput();
            // START_CHANGE: ISS-2025-0387 - write through the thread-local-aware StreamManager.out()
            // for the default output (the static user_output map entry captures System.out at
            // class-load, bypassing per-thread/test redirections; output discipline requires out()).
            if ("user_output".equals(currentOutputAlias)) {
                java.io.PrintStream ps = StreamManager.out();
                ps.print(getQuotedRepresentation(term));
                ps.flush();
                solutions.add(bindings);
                return true;
            }
            // END_CHANGE: ISS-2025-0387
            OutputStream outputStream = StreamManager.getOutputStream(currentOutputAlias);
            
            if (outputStream != null) {
                PrintStream printStream = new PrintStream(outputStream);
                String quotedRepresentation = getQuotedRepresentation(term);
                printStream.print(quotedRepresentation);
                printStream.flush();
                solutions.add(bindings);
                return true;
            } else {
                throw new PrologEvaluationException("Cannot write to current output stream: " + currentOutputAlias);
            }
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("I/O error in writeq/1: " + e.getMessage());
        }
    }

    private boolean writeQuotedToStream(Term streamTerm, Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException("writeq/2: stream must be an atom");
        }

        try {
            String streamAlias = ((Atom) streamTerm).getName();
            OutputStream outputStream = StreamManager.getOutputStream(streamAlias);
            
            if (outputStream != null) {
                PrintStream printStream = new PrintStream(outputStream);
                String quotedRepresentation = getQuotedRepresentation(term);
                printStream.print(quotedRepresentation);
                printStream.flush();
                solutions.add(bindings);
                return true;
            } else {
                throw new PrologEvaluationException("Stream does not exist: " + streamAlias);
            }
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("I/O error in writeq/2: " + e.getMessage());
        }
    }

    /**
     * Get quoted representation of a term according to ISO Prolog rules.
     * This includes quoting atoms that need quotes, escaping special characters, etc.
     */
    private String getQuotedRepresentation(Term term) {
        // START_CHANGE: ISS-2025-0242 - delegate to operator-aware formatter with quoted=true
        // START_CHANGE: ISS-2025-0389 - ISO 8.14.2: writeq/1 = write_term(T,[quoted(true),numbervars(true)])
        return it.denzosoft.jprolog.core.util.TermFormatter.format(term, true, false, true, 1200);
        // END_CHANGE: ISS-2025-0389
        // END_CHANGE: ISS-2025-0242
    }

    /**
     * Get quoted representation of an atom, adding quotes if necessary.
     */
    private String getQuotedAtomRepresentation(String atomName) {
        // Check if atom needs quotes
        if (needsQuotes(atomName)) {
            StringBuilder sb = new StringBuilder();
            sb.append("'");
            // Escape single quotes within the atom
            for (char c : atomName.toCharArray()) {
                if (c == '\'') {
                    sb.append("''");  // Escape single quote
                } else if (c == '\\') {
                    sb.append("\\\\");  // Escape backslash
                } else {
                    sb.append(c);
                }
            }
            sb.append("'");
            return sb.toString();
        } else {
            return atomName;
        }
    }

    /**
     * Determine if an atom needs quotes according to ISO Prolog rules.
     */
    private boolean needsQuotes(String atomName) {
        if (atomName.isEmpty()) {
            return true;  // Empty atom needs quotes
        }

        // Check if it starts with lowercase letter and contains only alphanumeric + underscore
        char first = atomName.charAt(0);
        if (Character.isLowerCase(first)) {
            for (int i = 1; i < atomName.length(); i++) {
                char c = atomName.charAt(i);
                if (!Character.isLetterOrDigit(c) && c != '_') {
                    return true;
                }
            }
            return false;  // Standard atom, no quotes needed
        }

        // Special atoms that don't need quotes
        if (isSpecialAtom(atomName)) {
            return false;
        }

        // Everything else needs quotes
        return true;
    }

    /**
     * Check if atom is a special operator or symbol that doesn't need quotes.
     */
    private boolean isSpecialAtom(String atomName) {
        String[] specialAtoms = {
            "!", ";", ",", "=", "\\=", "==", "\\==", "@<", "@=<", "@>", "@>=",
            "is", "=:=", "=\\=", "<", "=<", ">", ">=", "+", "-", "*", "/", 
            "mod", "rem", "**", "^", "<<", ">>", "/\\", "\\/", "\\",
            "true", "false", "fail"
        };
        
        for (String special : specialAtoms) {
            if (special.equals(atomName)) {
                return true;
            }
        }
        
        return false;
    }
}