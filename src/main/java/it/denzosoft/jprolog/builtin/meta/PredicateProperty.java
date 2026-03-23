// START_CHANGE: LIM-005 - predicate_property/2 built-in predicate
package it.denzosoft.jprolog.builtin.meta;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Implementation of predicate_property/2.
 *
 * predicate_property(Head, Property)
 *
 * Queries properties of predicates. Head is a callable term (e.g., append(_,_,_)).
 * Property is one of: built_in, dynamic, static, defined, undefined.
 *
 * Non-deterministic: if Property is unbound, generates all applicable properties
 * as multiple solutions.
 */
public class PredicateProperty implements BuiltInWithContext {

    private final QuerySolver querySolver;

    public PredicateProperty(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologException(new CompoundTerm(new Atom("error"),
                java.util.Arrays.asList(
                    new Atom("type_error"),
                    new Atom("predicate_property/2 requires exactly 2 arguments"))));
        }

        Term headTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term propertyTerm = query.getArguments().get(1).resolveBindings(bindings);

        Prolog prolog = solver.getPrologContext();
        if (prolog == null) {
            return false;
        }

        // If Head is a variable, enumerate all predicates
        if (headTerm instanceof Variable) {
            return enumerateAllPredicates(solver, (Variable) headTerm, propertyTerm, bindings, solutions);
        }

        // Extract functor and arity from Head
        String functor;
        int arity;

        if (headTerm instanceof Atom) {
            functor = ((Atom) headTerm).getName();
            arity = 0;
        } else if (headTerm instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) headTerm;
            functor = ct.getFunctor().getName();
            arity = ct.getArguments().size();
        } else {
            // Head must be callable
            throw new PrologException(new CompoundTerm(new Atom("error"),
                java.util.Arrays.asList(
                    new CompoundTerm(new Atom("type_error"),
                        java.util.Arrays.asList(new Atom("callable"), headTerm)),
                    new Atom("predicate_property/2"))));
        }

        // Determine predicate properties
        boolean isBuiltIn = solver.getBuiltInRegistry().hasBuiltIn(functor);
        Set<String> currentPredicates = prolog.getCurrentPredicates();
        String predIndicator = functor + "/" + arity;
        boolean isUserDefined = currentPredicates.contains(predIndicator);
        boolean isDefined = isBuiltIn || isUserDefined;

        // Collect applicable properties
        List<Term> properties = new ArrayList<>();

        if (isBuiltIn) {
            properties.add(new Atom("built_in"));
        }
        if (isDefined) {
            properties.add(new Atom("defined"));
        } else {
            properties.add(new Atom("undefined"));
        }
        if (isUserDefined && !isBuiltIn) {
            // User-defined predicates added via assert are dynamic;
            // those loaded via consult are static. Since JProlog doesn't
            // track this distinction explicitly, we report all user-defined
            // predicates as dynamic (they can be modified at runtime).
            properties.add(new Atom("dynamic"));
        }
        if (isUserDefined && isBuiltIn) {
            // If both built-in and has user clauses, report both
            properties.add(new Atom("dynamic"));
        }
        if (!isBuiltIn && isUserDefined) {
            // static only for user-defined that are not built-in
            // Actually, in standard Prolog, dynamic and static are mutually exclusive.
            // We'll just report dynamic for user-defined predicates.
        }

        // If Property is bound, check if it matches any applicable property
        if (!(propertyTerm instanceof Variable)) {
            for (Term prop : properties) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (propertyTerm.unify(prop, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            }
            return false;
        }

        // Property is unbound: generate all applicable properties as solutions
        Variable propVar = (Variable) propertyTerm;
        for (Term prop : properties) {
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (propVar.unify(prop, newBindings)) {
                solutions.add(newBindings);
            }
        }

        return !solutions.isEmpty();
    }

    /**
     * Enumerate all predicates (both built-in and user-defined) and their properties.
     */
    private boolean enumerateAllPredicates(QuerySolver solver, Variable headVar,
                                           Term propertyTerm,
                                           Map<String, Term> bindings,
                                           List<Map<String, Term>> solutions) {
        Prolog prolog = solver.getPrologContext();
        if (prolog == null) return false;

        // Enumerate user-defined predicates
        Set<String> currentPredicates = prolog.getCurrentPredicates();
        for (String predStr : currentPredicates) {
            String[] parts = predStr.split("/");
            if (parts.length == 2) {
                String functor = parts[0];
                int arity = Integer.parseInt(parts[1]);
                Term headTemplate = createHeadTemplate(functor, arity);

                List<Term> props = new ArrayList<>();
                props.add(new Atom("defined"));
                props.add(new Atom("dynamic"));
                if (solver.getBuiltInRegistry().hasBuiltIn(functor)) {
                    props.add(new Atom("built_in"));
                }

                addSolutionsForHead(headVar, headTemplate, propertyTerm, props, bindings, solutions);
            }
        }

        return !solutions.isEmpty();
    }

    /**
     * Create a head template term for a given functor/arity using anonymous variables.
     */
    private Term createHeadTemplate(String functor, int arity) {
        if (arity == 0) {
            return new Atom(functor);
        }
        List<Term> args = new ArrayList<>(arity);
        for (int i = 0; i < arity; i++) {
            args.add(new Variable("_A" + i));
        }
        return new CompoundTerm(new Atom(functor), args);
    }

    /**
     * Add solutions for a specific head and its properties.
     */
    private void addSolutionsForHead(Variable headVar, Term headTemplate,
                                     Term propertyTerm, List<Term> properties,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (propertyTerm instanceof Variable) {
            Variable propVar = (Variable) propertyTerm;
            for (Term prop : properties) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (headVar.unify(headTemplate, newBindings) &&
                    propVar.unify(prop, newBindings)) {
                    solutions.add(newBindings);
                }
            }
        } else {
            // Property is bound - check if it matches
            for (Term prop : properties) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (headVar.unify(headTemplate, newBindings) &&
                    propertyTerm.unify(prop, newBindings)) {
                    solutions.add(newBindings);
                    break; // Only one match needed per head
                }
            }
        }
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings,
                          List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("predicate_property/2 requires context");
    }
}
// END_CHANGE: LIM-005
