package it.denzosoft.jprolog.builtin.meta;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of call/1, call/2, ..., call/8 predicates.
 * 
 * call(+Goal)
 * call(+Goal, +ExtraArg1)
 * call(+Goal, +ExtraArg1, +ExtraArg2)
 * ...
 * 
 * Meta-call: execute Goal with optional extra arguments added.
 */
public class Call implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public Call(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        List<Term> args = query.getArguments();
        if (args.isEmpty() || args.size() > 8) {
            throw new PrologException(createTypeError("callable", query, "call: arity must be 1-8"));
        }
        
        Term goal = args.get(0).resolveBindings(bindings);
        
        // Check if goal is instantiated
        if (goal instanceof Variable) {
            throw new PrologException(createInstantiationError("call: goal must be instantiated"));
        }
        
        // Construct the actual goal to call
        Term actualGoal = constructGoal(goal, args.subList(1, args.size()), bindings);
        
        // Execute the constructed goal
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean success = solver.solve(actualGoal, new HashMap<>(bindings), goalSolutions, CutStatus.notOccurred());
        
        if (success) {
            solutions.addAll(goalSolutions);
            return true;
        }
        
        return false;
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("call requires context");
    }
    
    private Term constructGoal(Term baseTerm, List<Term> extraArgs, Map<String, Term> bindings) {
        // Resolve extra arguments
        List<Term> resolvedExtraArgs = new ArrayList<>();
        for (Term arg : extraArgs) {
            resolvedExtraArgs.add(arg.resolveBindings(bindings));
        }
        
        if (baseTerm instanceof Atom) {
            // Simple atom - create compound term with extra args
            if (resolvedExtraArgs.isEmpty()) {
                return baseTerm; // call(atom) -> atom
            } else {
                return new CompoundTerm((Atom) baseTerm, resolvedExtraArgs);
            }
            
        } else if (baseTerm instanceof CompoundTerm) {
            // Compound term - add extra args
            CompoundTerm compound = (CompoundTerm) baseTerm;
            List<Term> allArgs = new ArrayList<>(compound.getArguments());
            allArgs.addAll(resolvedExtraArgs);
            return new CompoundTerm(compound.getFunctor(), allArgs);
            
        } else {
            throw new PrologException(createTypeError("callable", baseTerm, "call: goal must be callable"));
        }
    }
    
    private Term createInstantiationError(String context) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new Atom("instantiation_error"),
                    new Atom(context)
                )
            );
        } catch (Exception e) {
            return new Atom("instantiation_error");
        }
    }
    
    private Term createTypeError(String expectedType, Term culprit, String context) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new CompoundTerm(
                        new Atom("type_error"),
                        java.util.Arrays.asList(
                            new Atom(expectedType),
                            culprit
                        )
                    ),
                    new Atom(context)
                )
            );
        } catch (Exception e) {
            return new Atom("type_error");
        }
    }
}