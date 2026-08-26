package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.Term;

// START_CHANGE: ISS-2025-0352 - report success through the solutions list
import java.util.HashMap;
// END_CHANGE: ISS-2025-0352
import java.util.List;
import java.util.Map;

/**
 * Abstract base class for built-in predicates that need SolverContext context.
 */
public abstract class AbstractBuiltInWithContext implements BuiltInWithContext {
    
    protected SolverContext solver;
    protected Term[] arguments;
    
    /**
     * Create a built-in with context.
     * 
     * @param solver The query solver
     */
    public AbstractBuiltInWithContext(SolverContext solver) {
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
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
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
        
        // START_CHANGE: ISS-2025-0352 - a successful deterministic builtin must add its bindings
        // to the solutions list: both engines treat an empty solutions list as failure.
        boolean result = solve(solver, bindings);
        if (result) {
            solutions.add(new HashMap<>(bindings));
        }
        return result;
        // END_CHANGE: ISS-2025-0352
    }
    
    /**
     * Solve the built-in predicate.
     * 
     * @param solver The query solver
     * @param bindings Variable bindings
     * @return true if successful
     */
    public abstract boolean solve(SolverContext solver, Map<String, Term> bindings);
}