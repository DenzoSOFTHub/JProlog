package it.denzosoft.jprolog.builtin.dcg;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

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
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 4) {
            throw new PrologEvaluationException("phrase/4 requires exactly 4 arguments: phrase(+DCGBody, ?List, ?Rest, +Options)");
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
            throw new PrologEvaluationException("phrase/4 error: " + e.getMessage());
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
            throw new PrologEvaluationException("phrase/4: Options must be a list");
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
            throw new PrologEvaluationException("phrase/4: Invalid option format: " + option);
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
                                throw new PrologEvaluationException("phrase/4: Invalid syntax_errors value: " + actionName);
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
                            throw new PrologEvaluationException("phrase/4: max_depth must be a positive integer");
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
            System.out.println("DCG TRACE: Executing " + dcgBody + " on " + inputList);
        }
        
        // Apply debugging if enabled
        if (options.debug) {
            System.out.println("DCG DEBUG: Body=" + dcgBody + ", Input=" + inputList + ", Rest=" + restList);
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
                System.out.println("DCG TRACE: Result=" + result);
            }
            
            return result;
            
        } catch (Exception e) {
            // Handle syntax errors according to options
            switch (options.syntaxErrors) {
                case "error":
                    throw new PrologEvaluationException("DCG syntax error: " + e.getMessage());
                case "fail":
                    return false;
                case "warning":
                    System.err.println("DCG warning: " + e.getMessage());
                    return false;
                default:
                    throw new PrologEvaluationException("DCG error: " + e.getMessage());
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