package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;

/**
 * flush_output/0 and flush_output/1 - ISO Prolog I/O predicates
 * Flushes output to ensure all buffered output is written.
 */
public class FlushOutput implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        
        if (arity == 0) {
            // flush_output/0 - flush current output
            return flushCurrentOutput(bindings, solutions);
        } else if (arity == 1) {
            // flush_output/1 - flush specified stream
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            return flushStream(streamTerm, bindings, solutions);
        } else {
            throw new PrologEvaluationException("flush_output expects 0 or 1 arguments, got " + arity);
        }
    }

    private boolean flushCurrentOutput(Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            String currentOutputAlias = StreamManager.getCurrentOutput();
            OutputStream outputStream = StreamManager.getOutputStream(currentOutputAlias);
            
            if (outputStream != null) {
                outputStream.flush();
                solutions.add(bindings);
                return true;
            } else {
                throw new PrologEvaluationException("Cannot flush current output stream: " + currentOutputAlias);
            }
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error flushing output: " + e.getMessage());
        }
    }

    private boolean flushStream(Term streamTerm, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (streamTerm instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw new PrologEvaluationException("flush_output/1: stream argument must be instantiated");
        }

        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException("flush_output/1: stream must be an atom");
        }

        try {
            String streamAlias = ((Atom) streamTerm).getName();
            OutputStream outputStream = StreamManager.getOutputStream(streamAlias);
            
            if (outputStream != null) {
                outputStream.flush();
                solutions.add(bindings);
                return true;
            } else {
                throw new PrologEvaluationException("Stream does not exist: " + streamAlias);
            }
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error flushing stream: " + e.getMessage());
        }
    }
}