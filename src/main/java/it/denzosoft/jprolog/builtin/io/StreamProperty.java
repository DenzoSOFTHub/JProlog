package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * stream_property/2 - ISO Prolog I/O predicate
 * Relates a stream to its properties.
 * stream_property(?Stream, ?Property)
 */
public class StreamProperty implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("stream_property/2 requires exactly 2 arguments");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term propertyTerm = query.getArguments().get(1).resolveBindings(bindings);

        // For simplicity, we'll handle specific known streams
        // In a full implementation, this would enumerate all open streams
        
        if (streamTerm instanceof Variable) {
            // Generate solutions for all known streams
            return generateStreamProperties(streamTerm, propertyTerm, bindings, solutions);
        } else {
            // Check properties for specific stream
            return checkStreamProperties(streamTerm, propertyTerm, bindings, solutions);
        }
    }

    private boolean generateStreamProperties(Term streamTerm, Term propertyTerm, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // Check standard streams
        String[] standardStreams = {"user_input", "user_output", "user_error"};
        
        boolean foundSolution = false;
        
        for (String streamAlias : standardStreams) {
            if (StreamManager.hasStream(streamAlias)) {
                Atom streamAtom = new Atom(streamAlias);
                List<Term> properties = getStreamProperties(streamAlias);
                
                for (Term property : properties) {
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (streamTerm.unify(streamAtom, newBindings) && 
                        propertyTerm.unify(property, newBindings)) {
                        solutions.add(newBindings);
                        foundSolution = true;
                    }
                }
            }
        }
        
        return foundSolution;
    }

    private boolean checkStreamProperties(Term streamTerm, Term propertyTerm, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException("stream_property/2: stream must be an atom");
        }

        String streamAlias = ((Atom) streamTerm).getName();
        
        if (!StreamManager.hasStream(streamAlias)) {
            throw new PrologEvaluationException("Stream does not exist: " + streamAlias);
        }

        List<Term> properties = getStreamProperties(streamAlias);
        
        if (propertyTerm instanceof Variable) {
            // Generate all properties for this stream
            boolean foundSolution = false;
            for (Term property : properties) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (propertyTerm.unify(property, newBindings)) {
                    solutions.add(newBindings);
                    foundSolution = true;
                }
            }
            return foundSolution;
        } else {
            // Check if this stream has the specified property
            for (Term property : properties) {
                if (property.equals(propertyTerm)) {
                    solutions.add(bindings);
                    return true;
                }
            }
            return false;
        }
    }

    private List<Term> getStreamProperties(String streamAlias) {
        // Determine stream properties based on stream type and alias
        
        if ("user_input".equals(streamAlias)) {
            return Arrays.asList(
                new Atom("input"),
                new Atom("text"),
                new CompoundTerm(new Atom("mode"), Arrays.asList(new Atom("read"))),
                new CompoundTerm(new Atom("alias"), Arrays.asList(new Atom("user_input"))),
                new CompoundTerm(new Atom("type"), Arrays.asList(new Atom("text")))
            );
        } else if ("user_output".equals(streamAlias) || "user_error".equals(streamAlias)) {
            return Arrays.asList(
                new Atom("output"),
                new Atom("text"),
                new CompoundTerm(new Atom("mode"), Arrays.asList(new Atom("write"))),
                new CompoundTerm(new Atom("alias"), Arrays.asList(new Atom(streamAlias))),
                new CompoundTerm(new Atom("type"), Arrays.asList(new Atom("text")))
            );
        } else {
            // For file streams, determine properties based on whether it's input or output
            InputStream is = StreamManager.getInputStream(streamAlias);
            OutputStream os = StreamManager.getOutputStream(streamAlias);
            
            if (is != null) {
                return Arrays.asList(
                    new Atom("input"),
                    new Atom("text"),
                    new CompoundTerm(new Atom("mode"), Arrays.asList(new Atom("read"))),
                    new CompoundTerm(new Atom("alias"), Arrays.asList(new Atom(streamAlias))),
                    new CompoundTerm(new Atom("type"), Arrays.asList(new Atom("text")))
                );
            } else if (os != null) {
                return Arrays.asList(
                    new Atom("output"),
                    new Atom("text"),
                    new CompoundTerm(new Atom("mode"), Arrays.asList(new Atom("write"))),
                    new CompoundTerm(new Atom("alias"), Arrays.asList(new Atom(streamAlias))),
                    new CompoundTerm(new Atom("type"), Arrays.asList(new Atom("text")))
                );
            } else {
                return Arrays.asList();
            }
        }
    }
}