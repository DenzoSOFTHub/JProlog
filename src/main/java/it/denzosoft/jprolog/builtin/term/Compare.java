package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
// START_CHANGE: ISS-2025-0417 - validate a pre-bound Order argument
import it.denzosoft.jprolog.core.terms.Variable;
// END_CHANGE: ISS-2025-0417

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * compare/3 - ISO Prolog predicate for three-way term comparison
 * compare(?Order, +Term1, +Term2)
 * 
 * Unifies Order with:
 * - '<' if Term1 @< Term2 (Term1 is before Term2 in standard ordering)
 * - '=' if Term1 == Term2 (Term1 is identical to Term2)
 * - '>' if Term1 @> Term2 (Term1 is after Term2 in standard ordering)
 */
public class Compare implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("compare/3 requires exactly 3 arguments");
        }

        Term orderTerm = query.getArguments().get(0);
        Term term1 = query.getArguments().get(1).resolveBindings(bindings);
        Term term2 = query.getArguments().get(2).resolveBindings(bindings);

        // START_CHANGE: ISS-2025-0417 - ISO 8.4.2.3: a pre-bound Order that is not an atom
        // raises type_error(atom, Order); an atom other than <, =, > raises
        // domain_error(order, Order) — never silent failure.
        Term resolvedOrder = orderTerm.resolveBindings(bindings);
        if (!(resolvedOrder instanceof Variable)) {
            if (!(resolvedOrder instanceof Atom)) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("atom", resolvedOrder, "compare/3"));
            }
            String orderName = ((Atom) resolvedOrder).getName();
            if (!"<".equals(orderName) && !"=".equals(orderName) && !">".equals(orderName)) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("order", resolvedOrder, "compare/3"));
            }
        }
        // END_CHANGE: ISS-2025-0417

        // Compare the two terms using standard term ordering
        int comparisonResult = StandardTermOrdering.compare(term1, term2);
        
        // Convert numeric comparison result to Prolog atom
        Atom orderAtom;
        if (comparisonResult < 0) {
            orderAtom = new Atom("<");
        } else if (comparisonResult > 0) {
            orderAtom = new Atom(">");
        } else {
            orderAtom = new Atom("=");
        }

        // Try to unify the order term with the result
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (orderTerm.unify(orderAtom, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        
        return false;
    }
}