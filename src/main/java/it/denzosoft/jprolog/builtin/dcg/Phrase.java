package it.denzosoft.jprolog.builtin.dcg;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;
import it.denzosoft.jprolog.util.TermUtils;

/**
 * Implementation of phrase/2 and phrase/3 predicates for DCG support.
 * 
 * phrase(RuleSet, List) - Parse List using RuleSet
 * phrase(RuleSet, List, Rest) - Parse List using RuleSet, leaving Rest
 */
public class Phrase extends AbstractBuiltInWithContext {
    
    /**
     * Create a phrase predicate.
     * 
     * @param solver The query solver for context
     */
    public Phrase(QuerySolver solver) {
        super(solver);
    }
    
    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        boolean result = solve(solver, bindings);
        if (result) {
            // Add the successful binding to solutions
            solutions.add(new HashMap<>(bindings));
        }
        return result;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        this.solver = solver;
        
        // Extract arguments from query
        if (query instanceof it.denzosoft.jprolog.core.terms.CompoundTerm) {
            it.denzosoft.jprolog.core.terms.CompoundTerm compound = (it.denzosoft.jprolog.core.terms.CompoundTerm) query;
            this.arguments = new Term[compound.getArguments().size()];
            for (int i = 0; i < compound.getArguments().size(); i++) {
                this.arguments[i] = compound.getArguments().get(i);
            }
        } else {
            this.arguments = new Term[0];
        }
        
        boolean result = solve(solver, bindings);
        if (result) {
            // Add the successful binding to solutions
            solutions.add(new HashMap<>(bindings));
        }
        return result;
    }
    
    @Override
    public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        
        if (args.length == 2) {
            return phrase2(args[0], args[1], bindings);
        } else if (args.length == 3) {
            return phrase3(args[0], args[1], args[2], bindings);
        }
        
        return false;
    }
    
    /**
     * phrase/2 implementation: phrase(RuleSet, List)
     */
    private boolean phrase2(Term ruleSet, Term list, Map<String, Term> bindings) {
        // phrase(RuleSet, List) is equivalent to phrase(RuleSet, List, [])
        return phrase3(ruleSet, list, new Atom("[]"), bindings);
    }
    
    /**
     * phrase/3 implementation: phrase(RuleSet, List, Rest)
     */
    private boolean phrase3(Term ruleSet, Term list, Term rest, Map<String, Term> bindings) {
        try {
            // Create a goal: RuleSet(List, Rest)
            Term goal = createDCGGoal(ruleSet, list, rest);
            
            // Solve the DCG goal using the query solver
            List<Map<String, Term>> solutionList = new ArrayList<>();
            boolean success = this.solver.solve(goal, bindings, solutionList, CutStatus.notOccurred());
            
            // Return true if successful
            if (success && !solutionList.isEmpty()) {
                // Apply the first solution to bindings
                Map<String, Term> solution = solutionList.get(0);
                bindings.putAll(solution);
                return true;
            }
            
            return false;
            
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Create a DCG goal from the rule set and arguments.
     * 
     * @param ruleSet The DCG rule set (non-terminal)
     * @param list The input list
     * @param rest The rest list
     * @return The created goal term
     */
    private Term createDCGGoal(Term ruleSet, Term list, Term rest) {
        if (ruleSet instanceof Atom) {
            // Simple non-terminal: nt --> nt(List, Rest)
            String functor = ((Atom) ruleSet).getName();
            return TermUtils.createCompound(functor, list, rest);
        } else if (ruleSet instanceof CompoundTerm) {
            // Complex non-terminal: nt(Args) --> nt(Args, List, Rest)
            CompoundTerm compound = (CompoundTerm) ruleSet;
            List<Term> newArgs = new ArrayList<>();
            
            // Add original arguments
            for (int i = 0; i < TermUtils.getArity(compound); i++) {
                newArgs.add(TermUtils.getArgument(compound, i));
            }
            
            // Add difference list arguments
            newArgs.add(list);
            newArgs.add(rest);
            
            return TermUtils.createCompound(TermUtils.getFunctorName(compound), newArgs);
        } else if (ruleSet instanceof Variable) {
            // Variable rule set: call the variable with difference list
            return TermUtils.createCompound("call", ruleSet, list, rest);
        }
        
        throw new IllegalArgumentException("Invalid DCG rule set: " + ruleSet);
    }
    
    /**
     * Validate that a term is a proper list.
     * 
     * @param term The term to validate
     * @return true if it's a proper list
     */
    private boolean isProperList(Term term) {
        Term current = term;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            current = TermUtils.getArgument(cons, 1);
        }
        
        return current instanceof Atom && "[]".equals(((Atom) current).getName());
    }
    
    /**
     * Convert a Prolog list to a Java list.
     * 
     * @param term The Prolog list term
     * @return Java list of terms
     */
    private List<Term> termToList(Term term) {
        List<Term> result = new ArrayList<>();
        Term current = term;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            result.add(TermUtils.getArgument(cons, 0));
            current = TermUtils.getArgument(cons, 1);
        }
        
        return result;
    }
    
    /**
     * Convert a Java list to a Prolog list term.
     * 
     * @param elements The list elements
     * @param tail The list tail (usually [])
     * @return The Prolog list term
     */
    private Term listToTerm(List<Term> elements, Term tail) {
        Term result = tail;
        
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = TermUtils.createCompound(".", elements.get(i), result);
        }
        
        return result;
    }
}