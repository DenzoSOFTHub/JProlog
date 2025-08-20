package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Clause;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of asserta/1 predicate.
 * 
 * asserta(+Clause)
 * 
 * Assert a clause at the beginning of the database.
 * Clause can be a fact (term) or a rule (Head :- Body).
 */
public class Asserta implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public Asserta(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologException(createTypeError("callable", query, "asserta/1 requires exactly one argument"));
        }
        
        Term clauseTerm = query.getArguments().get(0).resolveBindings(bindings);
        
        // Check if clause is instantiated
        if (clauseTerm instanceof Variable) {
            throw new PrologException(createInstantiationError("asserta/1: clause must be instantiated"));
        }
        
        try {
            Clause clause = parseClause(clauseTerm);
            
            // Add clause to the beginning of the database
            // Note: This requires access to the Prolog engine's clause database
            // For now, we'll add it to the solver's database if available
            Prolog prolog = solver.getPrologContext();
            if (prolog != null) {
                prolog.addClauseFirst(clause);
            } else {
                // If we can't access the database, this is a system limitation
                throw new PrologException(createSystemError("asserta/1: cannot access clause database"));
            }
            
            solutions.add(new HashMap<>(bindings));
            return true;
            
        } catch (Exception e) {
            throw new PrologException(createSystemError("asserta/1: " + e.getMessage()));
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("asserta/1 requires context");
    }
    
    private Clause parseClause(Term clauseTerm) {
        if (clauseTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) clauseTerm;
            
            // Check if it's a rule (Head :- Body)
            if (compound.getFunctor().getName().equals(":-") && compound.getArguments().size() == 2) {
                Term head = compound.getArguments().get(0);
                Term body = compound.getArguments().get(1);
                return new Clause(head, body);
            }
        }
        
        // It's a fact
        return new Clause(clauseTerm, null);
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
    
    private Term createSystemError(String message) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new Atom("system_error"),
                    new Atom(message)
                )
            );
        } catch (Exception e) {
            return new Atom("system_error");
        }
    }
}