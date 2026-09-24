package it.denzosoft.jprolog.builtin.dcg;

import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;
import java.util.logging.Logger;

/**
 * Implementation of phrase/4 predicate with options support
 * following ISO/IEC DTS 13211-3 specifications.
 * 
 * phrase(DCGBody, List, Rest, Options) - Parse with options
 * 
 * Supported options:
 * - variable_names(VarNames) - Variable name bindings
 * - syntax_errors(Action) - How to handle syntax errors (error/fail/warning)
 * - max_depth(N) - Maximum recursion depth
 * - trace(Boolean) - Enable/disable DCG tracing
 * - debug(Boolean) - Enable/disable DCG debugging
 */
public class PhraseWithOptions implements BuiltIn {
    // START_CHANGE: ISS-2025-0178 - Replace System.out.println with Logger
    private static final Logger LOGGER = Logger.getLogger(PhraseWithOptions.class.getName());
    // END_CHANGE: ISS-2025-0178
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 4) {
            throw LibArgs.unknownArity(query); //  ISS-2025-0695: phrase(+DCGBody, ?List, ?Rest, +Options)");
        }
        
        Term dcgBody = query.getArguments().get(0).resolveBindings(bindings);
        Term inputList = query.getArguments().get(1).resolveBindings(bindings);
        Term restList = query.getArguments().get(2).resolveBindings(bindings);
        Term options = query.getArguments().get(3).resolveBindings(bindings);
        
        try {
            // Parse options
            DCGOptions dcgOptions = parseOptions(options);
            
            // Execute phrase with options
            return executeWithOptions(dcgBody, inputList, restList, dcgOptions, bindings, solutions);
            
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw Errors.host(e, "execute", "dcg", null, "phrase_with_options", 4);   // ISS-2025-0695
        }
    }
    
    /**
     * Parse DCG options from the options term
     */
    private DCGOptions parseOptions(Term optionsTerm) {
        DCGOptions options = new DCGOptions();
        
        if (optionsTerm instanceof Atom && "[]".equals(((Atom) optionsTerm).getName())) {
            // Empty options list - use defaults
            return options;
        }
        
        if (!(optionsTerm instanceof CompoundTerm)) {
            throw LibArgs.notA("list", optionsTerm, "phrase_with_options", 4, "the options");   // ISS-2025-0695
        }
        
        List<Term> optionList = extractListElements(optionsTerm);
        
        for (Term option : optionList) {
            parseOption(option, options);
        }
        
        return options;
    }
    
    /**
     * Parse individual option
     */
    private void parseOption(Term option, DCGOptions options) {
        if (!(option instanceof CompoundTerm)) {
            throw LibArgs.notA("compound", option, "phrase_with_options", 4, "an option");   // ISS-2025-0695
        }
        
        CompoundTerm compound = (CompoundTerm) option;
        String functor = compound.getName();
        
        switch (functor) {
            case "variable_names":
                if (compound.getArguments().size() == 1) {
                    options.variableNames = compound.getArguments().get(0);
                }
                break;
                
            case "syntax_errors":
                if (compound.getArguments().size() == 1) {
                    Term action = compound.getArguments().get(0);
                    if (action instanceof Atom) {
                        String actionName = ((Atom) action).getName();
                        switch (actionName) {
                            case "error":
                            case "fail":
                            case "warning":
                                options.syntaxErrors = actionName;
                                break;
                            default:
                                throw Errors.domain("syntax_errors", action, "phrase_with_options", 4,
                                                    "syntax_errors must be error, fail or warning");   // ISS-2025-0695
                        }
                    }
                }
                break;
                
            case "max_depth":
                if (compound.getArguments().size() == 1) {
                    Term depth = compound.getArguments().get(0);
                    if (depth instanceof it.denzosoft.jprolog.core.terms.Number) {
                        double value = ((it.denzosoft.jprolog.core.terms.Number) depth).getValue();
                        if (value > 0 && value == (int) value) {
                            options.maxDepth = (int) value;
                        } else {
                            throw Errors.domain("positive_integer", depth, "phrase_with_options", 4,
                                                "max_depth must be a positive integer");   // ISS-2025-0695
                        }
                    }
                }
                break;
                
            case "trace":
                if (compound.getArguments().size() == 1) {
                    Term trace = compound.getArguments().get(0);
                    if (trace instanceof Atom) {
                        String traceName = ((Atom) trace).getName();
                        options.trace = "true".equals(traceName) || "on".equals(traceName);
                    }
                }
                break;
                
            case "debug":
                if (compound.getArguments().size() == 1) {
                    Term debug = compound.getArguments().get(0);
                    if (debug instanceof Atom) {
                        String debugName = ((Atom) debug).getName();
                        options.debug = "true".equals(debugName) || "on".equals(debugName);
                    }
                }
                break;
                
            default:
                // Unknown option - could be implementation-specific
                options.unknownOptions.put(functor, compound);
                break;
        }
    }
    
    /**
     * Execute phrase with parsed options
     */
    private boolean executeWithOptions(Term dcgBody, Term inputList, Term restList, 
                                     DCGOptions options, Map<String, Term> bindings, 
                                     List<Map<String, Term>> solutions) {
        
        // Apply tracing if enabled
        if (options.trace) {
            // START_CHANGE: ISS-2025-0178 - Replace System.out.println with Logger
            if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("DCG TRACE: Executing " + dcgBody + " on " + inputList);   // ISS-2025-0550: lazy
            // END_CHANGE: ISS-2025-0178
        }
        
        // Apply debugging if enabled
        if (options.debug) {
            // START_CHANGE: ISS-2025-0178 - Replace System.out.println with Logger
            if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("DCG DEBUG: Body=" + dcgBody + ", Input=" + inputList + ", Rest=" + restList);   // ISS-2025-0550: lazy
            // END_CHANGE: ISS-2025-0178
        }
        
        try {
            // Use enhanced phrase implementation with options
            EnhancedPhrase enhancedPhrase = new EnhancedPhrase();
            
            // Create a phrase/3 query
            Term phraseQuery = new CompoundTerm(
                new Atom("phrase"), 
                Arrays.asList(dcgBody, inputList, restList)
            );
            
            // Apply max depth checking
            if (options.maxDepth > 0) {
                // This would require integration with the actual query solver
                // For now, we'll just pass through to the enhanced phrase
            }
            
            boolean result = enhancedPhrase.execute(phraseQuery, bindings, solutions);
            
            // Handle variable names option
            if (options.variableNames != null && result && !solutions.isEmpty()) {
                bindVariableNames(options.variableNames, solutions.get(0));
            }
            
            if (options.trace) {
                // START_CHANGE: ISS-2025-0178 - Replace System.out.println with Logger
                if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("DCG TRACE: Result=" + result);   // ISS-2025-0550: lazy
                // END_CHANGE: ISS-2025-0178
            }
            
            return result;
            
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            // Handle syntax errors according to options
            switch (options.syntaxErrors) {
                case "error":
                    if (e instanceof it.denzosoft.jprolog.core.exceptions.PrologException) {   // ISS-2025-0695
                        throw (it.denzosoft.jprolog.core.exceptions.PrologException) e;
                    }
                    throw Errors.syntax("dcg", "phrase_with_options", 4, String.valueOf(e.getMessage()));
                case "fail":
                    return false;
                case "warning":
                    // START_CHANGE: ISS-2025-0185 - Use LOGGER instead of System.err
                    LOGGER.warning("DCG warning: " + e.getMessage());
                    // END_CHANGE: ISS-2025-0185
                    return false;
                default:
                    throw Errors.host(e, "execute", "dcg", null, "phrase_with_options", 4);   // ISS-2025-0695
            }
        }
    }
    
    /**
     * Bind variable names from the variable_names option
     */
    private void bindVariableNames(Term variableNames, Map<String, Term> solution) {
        // This would bind variable names for debugging/inspection purposes
        // Implementation depends on the specific variable name format used
        if (variableNames instanceof CompoundTerm) {
            // Handle structured variable name bindings
            // This is a placeholder for full implementation
        }
    }
    
    /**
     * Extract elements from a Prolog list term
     */
    private List<Term> extractListElements(Term listTerm) {
        List<Term> elements = new ArrayList<>();
        Term current = listTerm;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (!".".equals(compound.getName()) || compound.getArguments().size() != 2) {
                break;
            }
            
            elements.add(compound.getArguments().get(0));
            current = compound.getArguments().get(1);
        }
        
        return elements;
    }
    
    /**
     * DCG Options holder class
     */
    private static class DCGOptions {
        Term variableNames = null;
        String syntaxErrors = "error";
        int maxDepth = 1000; // Default max depth
        boolean trace = false;
        boolean debug = false;
        Map<String, CompoundTerm> unknownOptions = new HashMap<>();
    }
}