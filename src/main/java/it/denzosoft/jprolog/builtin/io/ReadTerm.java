package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.parser.TermParser;
import it.denzosoft.jprolog.core.terms.*;

import it.denzosoft.jprolog.util.TermUtils;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

/**
 * Implementation of read_term/2 predicate for advanced term reading.
 * 
 * read_term(+Stream, -Term)
 * read_term(-Term, +Options)
 */
public class ReadTerm extends AbstractBuiltInWithContext {
    
    /**
     * Create read_term predicate.
     * 
     * @param solver The query solver
     */
    public ReadTerm(QuerySolver solver) {
        super(solver);
    }
    
    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return solve(solver, bindings);
    }
    
    @Override
    public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        
        if (args.length == 2) {
            // Check if first argument is stream or term
            Term first = args[0];
            Term second = args[1];
            
            if (isStream(first)) {
                // read_term(+Stream, -Term)
                return readTermFromStream(first, second, Collections.emptyList(), bindings);
            } else {
                // read_term(-Term, +Options)
                return readTermWithOptions(first, second, bindings);
            }
        }
        
        return false;
    }
    
    /**
     * Read term from specified stream.
     * 
     * @param streamTerm The stream term
     * @param termVar The variable to unify with read term
     * @param options Read options
     * @param bindings Variable bindings
     * @return true if successful
     */
    private boolean readTermFromStream(Term streamTerm, Term termVar, List<ReadOption> options, Map<String, Term> bindings) {
        try {
            // Get stream (simplified - in full implementation, use StreamManager)
            BufferedReader reader = getCurrentInputStream();
            
            // Read and parse term
            String input = reader.readLine();
            if (input == null) {
                // End of file
                return unifyTerm(termVar, new Atom("end_of_file"), bindings);
            }
            
            // Parse the input
            TermParser parser = new TermParser();
            Term parsedTerm = parser.parseTerm(input.trim());
            
            // Apply options
            Term resultTerm = applyReadOptions(parsedTerm, options);
            
            // Unify with the term variable
            return unifyTerm(termVar, resultTerm, bindings);
            
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Read term with options from current input.
     * 
     * @param termVar The variable to unify with read term
     * @param optionsTerm The options list
     * @param bindings Variable bindings
     * @return true if successful
     */
    private boolean readTermWithOptions(Term termVar, Term optionsTerm, Map<String, Term> bindings) {
        try {
            // Parse options
            List<ReadOption> options = parseReadOptions(optionsTerm);
            
            // Read from current input stream
            return readTermFromStream(new Atom("current_input"), termVar, options, bindings);
            
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Parse read options from term.
     * 
     * @param optionsTerm The options term
     * @return List of parsed options
     */
    private List<ReadOption> parseReadOptions(Term optionsTerm) {
        List<ReadOption> options = new ArrayList<>();
        
        if (optionsTerm instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(optionsTerm))) {
            // Parse list of options
            List<Term> optionTerms = extractListElements(optionsTerm);
            
            for (Term optionTerm : optionTerms) {
                ReadOption option = parseReadOption(optionTerm);
                if (option != null) {
                    options.add(option);
                }
            }
        }
        
        return options;
    }
    
    /**
     * Parse a single read option.
     * 
     * @param optionTerm The option term
     * @return The parsed option, or null if invalid
     */
    private ReadOption parseReadOption(Term optionTerm) {
        if (optionTerm instanceof Atom) {
            String optionName = ((Atom) optionTerm).getName();
            switch (optionName) {
                case "variables":
                    return new ReadOption(ReadOption.Type.VARIABLES, null);
                case "variable_names":
                    return new ReadOption(ReadOption.Type.VARIABLE_NAMES, null);
                case "singletons":
                    return new ReadOption(ReadOption.Type.SINGLETONS, null);
                default:
                    return null;
            }
        } else if (optionTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) optionTerm;
            String functor = TermUtils.getFunctorName(compound);
            
            if (TermUtils.getArity(compound) == 1) {
                Term arg = TermUtils.getArgument(compound, 0);
                
                switch (functor) {
                    case "variables":
                        return new ReadOption(ReadOption.Type.VARIABLES, arg);
                    case "variable_names":
                        return new ReadOption(ReadOption.Type.VARIABLE_NAMES, arg);
                    case "singletons":
                        return new ReadOption(ReadOption.Type.SINGLETONS, arg);
                    case "module":
                        return new ReadOption(ReadOption.Type.MODULE, arg);
                    default:
                        return null;
                }
            }
        }
        
        return null;
    }
    
    /**
     * Apply read options to the parsed term.
     * 
     * @param term The parsed term
     * @param options The read options
     * @return The modified term
     */
    private Term applyReadOptions(Term term, List<ReadOption> options) {
        // In a full implementation, this would:
        // - Collect variable information
        // - Handle module contexts
        // - Process variable naming
        
        // For now, return the term as-is
        return term;
    }
    
    /**
     * Check if a term represents a stream.
     * 
     * @param term The term to check
     * @return true if it's a stream
     */
    private boolean isStream(Term term) {
        // Simplified stream detection
        if (term instanceof Atom) {
            String name = ((Atom) term).getName();
            return name.equals("current_input") || name.equals("current_output") || 
                   name.equals("user_input") || name.equals("user_output");
        }
        
        // Could also be a stream handle (compound term)
        return term instanceof CompoundTerm && "stream".equals(TermUtils.getFunctorName(term));
    }
    
    /**
     * Get current input stream.
     * 
     * @return BufferedReader for current input
     */
    private BufferedReader getCurrentInputStream() {
        // Simplified - in full implementation, use StreamManager
        return new BufferedReader(new InputStreamReader(System.in));
    }
    
    /**
     * Unify a term with a variable.
     * 
     * @param var The variable term
     * @param value The value to unify
     * @param bindings The variable bindings
     * @return true if successful
     */
    private boolean unifyTerm(Term var, Term value, Map<String, Term> bindings) {
        if (var instanceof Variable) {
            String varName = ((Variable) var).getName();
            bindings.put(varName, value);
            return true;
        } else {
            // Try to unify with existing term
            return var.equals(value);
        }
    }
    
    /**
     * Extract elements from a Prolog list.
     * 
     * @param list The list term
     * @return List of elements
     */
    private List<Term> extractListElements(Term list) {
        List<Term> elements = new ArrayList<>();
        Term current = list;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            elements.add(TermUtils.getArgument(cons, 0));
            current = TermUtils.getArgument(cons, 1);
        }
        
        return elements;
    }
    
    /**
     * Represents a read option for read_term/2.
     */
    private static class ReadOption {
        enum Type {
            VARIABLES,
            VARIABLE_NAMES,
            SINGLETONS,
            MODULE
        }
        
        private final Type type;
        private final Term value;
        
        ReadOption(Type type, Term value) {
            this.type = type;
            this.value = value;
        }
        
        Type getType() { return type; }
        Term getValue() { return value; }
    }
}