package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * Abstract base class for built-in predicates that need QuerySolver context.
 */
public abstract class AbstractBuiltInWithContext implements BuiltInWithContext {
    
    protected QuerySolver solver;
    protected Term[] arguments;
    
    /**
     * Create a built-in with context.
     * 
     * @param solver The query solver
     */
    public AbstractBuiltInWithContext(QuerySolver solver) {
        this.solver = solver;
    }
    
    /**
     * Set the arguments for this built-in.
     * 
     * @param arguments The arguments
     */
    public void setArguments(Term[] arguments) {
        this.arguments = arguments;
    }
    
    /**
     * Get the arguments for this built-in.
     * 
     * @return The arguments
     */
    protected Term[] getArguments() {
        return arguments;
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
        
        return solve(solver, bindings);
    }
    
    /**
     * Solve the built-in predicate.
     * 
     * @param solver The query solver
     * @param bindings Variable bindings
     * @return true if successful
     */
    public abstract boolean solve(QuerySolver solver, Map<String, Term> bindings);
}