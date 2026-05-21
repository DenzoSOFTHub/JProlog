// START_CHANGE: v2.9.7 - must_be/2 type-checking helper (SWI library)
package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * must_be(+Type, @Value) — SWI library predicate.
 *
 * Succeeds if Value satisfies Type, else throws an ISO-compliant error.
 * Throws {@code instantiation_error} if Value is var and Type requires bound;
 * throws {@code type_error(Type, Value)} otherwise.
 *
 * Supported types:
 *   atom, atomic, integer, float, number, callable, compound, var, nonvar,
 *   ground, list, boolean, positive_integer, nonneg
 */
public class MustBe implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException("must_be/2 requires exactly 2 arguments");
        }
        Term typeTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term value = query.getArguments().get(1).resolveBindings(bindings);

        if (!(typeTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", typeTerm, "must_be/2"));
        }
        String type = ((Atom) typeTerm).getName();

        // For most types, var(Value) → instantiation_error (SWI convention)
        if (value instanceof Variable
            && !"var".equals(type) && !"nonvar".equals(type)) {
            throw new PrologException(ISOErrorTerms.instantiationError("must_be/2"));
        }

        boolean ok;
        switch (type) {
            case "atom": ok = value instanceof Atom; break;
            case "atomic": ok = (value instanceof Atom) || (value instanceof Number); break;
            case "number": ok = value instanceof Number; break;
            case "integer": ok = (value instanceof Number) && ((Number) value).isInteger(); break;
            case "float": ok = (value instanceof Number) && !((Number) value).isInteger(); break;
            case "compound": ok = value instanceof CompoundTerm; break;
            case "callable": ok = (value instanceof Atom) || (value instanceof CompoundTerm); break;
            case "var": ok = value instanceof Variable; break;
            case "nonvar": ok = !(value instanceof Variable); break;
            case "ground": ok = isGround(value); break;
            case "list": ok = isProperList(value); break;
            case "boolean":
                ok = (value instanceof Atom)
                     && ("true".equals(((Atom) value).getName()) || "false".equals(((Atom) value).getName()));
                break;
            case "positive_integer":
                ok = (value instanceof Number) && ((Number) value).isInteger()
                     && ((Number) value).longValue() > 0;
                break;
            case "nonneg":
                ok = (value instanceof Number) && ((Number) value).isInteger()
                     && ((Number) value).longValue() >= 0;
                break;
            default:
                throw new PrologException(ISOErrorTerms.domainError("type", typeTerm, "must_be/2"));
        }

        if (!ok) {
            throw new PrologException(ISOErrorTerms.typeError(type, value, "must_be/2"));
        }
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private static boolean isGround(Term t) {
        if (t instanceof Variable) return false;
        if (t instanceof CompoundTerm) {
            for (Term a : ((CompoundTerm) t).getArguments()) {
                if (!isGround(a)) return false;
            }
        }
        return true;
    }

    private static boolean isProperList(Term t) {
        java.util.IdentityHashMap<Term, Boolean> visited = new java.util.IdentityHashMap<>();
        Term c = t;
        while (true) {
            if (c instanceof Atom) return "[]".equals(((Atom) c).getName());
            if (!(c instanceof CompoundTerm)) return false;
            CompoundTerm ct = (CompoundTerm) c;
            if (!".".equals(ct.getName()) || ct.getArguments().size() != 2) return false;
            if (visited.put(ct, Boolean.TRUE) != null) return false;
            c = ct.getArguments().get(1);
        }
    }
}
// END_CHANGE: v2.9.7
