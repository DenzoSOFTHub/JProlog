package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0369 - dynamic/1 callable as a runtime goal
/**
 * Implementation of dynamic/1 as a callable goal.
 *
 * dynamic(+PredicateIndicators)
 *
 * Declares the indicated procedures dynamic, so calling them while they have no clauses fails
 * silently instead of raising existence_error (ISO 7.4.2.6 directive semantics; SWI/GNU also
 * accept dynamic/1 as a goal). Accepts a single Name/Arity, a ','-sequence of indicators, a list
 * of indicators, or a bare atom (SWI extension: Name/0 — consistent with the ':- dynamic'
 * directive handling in Prolog.processDynamicDirective). Reuses the ISS-2025-0347 machinery:
 * {@link KnowledgeBase#markDynamic(String, int)}.
 */
public class Dynamic implements BuiltInWithContext {

    private final SolverContext querySolver;

    public Dynamic(SolverContext querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query,
                                    Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) {

        if (query.getArguments().size() != 1) {
            throw new PrologException(ISOErrorTerms.typeError("predicate_indicator", query,
                "dynamic/1 requires exactly one argument"));
        }

        Term spec = query.getArguments().get(0).resolveBindings(bindings);
        declare(solver.getKnowledgeBase(), spec);

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("dynamic/1 requires context");
    }

    /** Mark every indicated procedure dynamic; recurses through ','-sequences and lists. */
    private void declare(KnowledgeBase knowledgeBase, Term spec) {
        if (spec instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("dynamic/1"));
        }
        if (spec instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) spec;
            String f = c.getName();
            int arity = c.getArguments().size();
            if ((",".equals(f) || ".".equals(f)) && arity == 2) {    // ','-sequence or list cell
                declare(knowledgeBase, c.getArguments().get(0));
                declare(knowledgeBase, c.getArguments().get(1));
                return;
            }
            if ("/".equals(f) && arity == 2) {                       // Name/Arity
                Term name = c.getArguments().get(0);
                Term arityTerm = c.getArguments().get(1);
                if (name instanceof Variable || arityTerm instanceof Variable) {
                    throw new PrologException(ISOErrorTerms.instantiationError("dynamic/1"));
                }
                if (name instanceof Atom && arityTerm instanceof Number
                        && ((Number) arityTerm).isInteger() && ((Number) arityTerm).getValue() >= 0) {
                    knowledgeBase.markDynamic(((Atom) name).getName(),
                        (int) Math.round(((Number) arityTerm).getValue()));
                    return;
                }
                throw new PrologException(
                    ISOErrorTerms.typeError("predicate_indicator", spec, "dynamic/1"));
            }
            throw new PrologException(
                ISOErrorTerms.typeError("predicate_indicator", spec, "dynamic/1"));
        }
        if (spec instanceof Atom) {
            String name = ((Atom) spec).getName();
            if ("[]".equals(name)) {                                 // end of a PI list
                return;
            }
            // bare atom (SWI extension): mark the arity-0 procedure, like the directive does
            knowledgeBase.markDynamic(name, 0);
            return;
        }
        throw new PrologException(
            ISOErrorTerms.typeError("predicate_indicator", spec, "dynamic/1"));
    }
}
// END_CHANGE: ISS-2025-0369
