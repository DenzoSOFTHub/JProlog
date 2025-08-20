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
 * Implementation of assertz/1 predicate.
 * 
 * assertz(+Clause)
 * 
 * Assert a clause at the end of the database.
 * Clause can be a fact (term) or a rule (Head :- Body).
 */
public class Assertz implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public Assertz(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologException(createTypeError("callable", query, "assertz/1 requires exactly one argument"));
        }
        
        Term clauseTerm = query.getArguments().get(0).resolveBindings(bindings);
        
        // Check if clause is instantiated
        if (clauseTerm instanceof Variable) {
            throw new PrologException(createInstantiationError("assertz/1: clause must be instantiated"));
        }
        
        try {
            Clause clause = parseClause(clauseTerm);
            
            // Add clause to the end of the database
            Prolog prolog = solver.getPrologContext();
            if (prolog != null) {
                prolog.addClauseLast(clause);
            } else {
                throw new PrologException(createSystemError("assertz/1: cannot access clause database"));
            }
            
            solutions.add(new HashMap<>(bindings));
            return true;
            
        } catch (Exception e) {
            throw new PrologException(createSystemError("assertz/1: " + e.getMessage()));
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("assertz/1 requires context");
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