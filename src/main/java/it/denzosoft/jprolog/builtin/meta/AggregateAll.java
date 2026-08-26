package it.denzosoft.jprolog.builtin.meta;

// START_CHANGE: ISS-2025-0122 - aggregate_all/3 built-in predicate
// START_CHANGE: ISS-2025-0383/ISS-2025-0384 - ISO error propagation + callable checks
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
// END_CHANGE: ISS-2025-0383/ISS-2025-0384
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
// START_CHANGE: ISS-2025-0383 - re-throw Prolog error balls unchanged
import it.denzosoft.jprolog.core.exceptions.PrologException;
// END_CHANGE: ISS-2025-0383
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
// START_CHANGE: ISS-2025-0384 - detect unbound goals
import it.denzosoft.jprolog.core.terms.Variable;
// END_CHANGE: ISS-2025-0384
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.util.*;

/**
 * aggregate_all(+Template, :Goal, -Result)
 *
 * Collects results of Goal and aggregates them:
 *   aggregate_all(count, Goal, Count)              - count solutions
 *   aggregate_all(sum(X), Goal, Sum)               - sum of X across solutions
 *   aggregate_all(max(X), Goal, Max)               - maximum X
 *   aggregate_all(min(X), Goal, Min)               - minimum X
 *   aggregate_all(bag(Template), Goal, Bag)         - like findall
 *   aggregate_all(set(Template), Goal, Set)         - like findall + sort
 *
 * count/sum/bag/set always succeed (returning 0/[]/etc. for no solutions);
 * max/min FAIL when Goal has no solutions (SWI semantics, ISS-2025-0413).
 */
public class AggregateAll implements BuiltInWithContext {

    private final SolverContext querySolver;

    public AggregateAll(SolverContext querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'aggregate_all' must be invoked with context");
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        SolverContext qs = solver != null ? solver : this.querySolver;
        if (qs == null) throw new PrologEvaluationException("aggregate_all/3: no query solver available.");

        List<Term> args = query.getArguments();
        if (args.size() != 3) throw new PrologEvaluationException("aggregate_all/3 requires 3 arguments.");

        Term templateSpec = args.get(0).resolveBindings(bindings);
        Term goal = args.get(1).resolveBindings(bindings);
        Term resultVar = args.get(2);

        // START_CHANGE: ISS-2025-0384 - ISO callable check on Goal: instantiation_error when
        // unbound, type_error(callable) when neither atom nor compound (never N = 0 silently).
        if (goal instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("aggregate_all/3"));
        }
        if (!(goal instanceof Atom) && !(goal instanceof CompoundTerm)) {
            throw new PrologException(ISOErrorTerms.typeError("callable", goal, "aggregate_all/3"));
        }
        // END_CHANGE: ISS-2025-0384

        // Solve the goal
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        try {
            qs.solveMeta(goal, bindings, goalSolutions)   /* ISS-2025-0431 - ENG-04 */;
        // START_CHANGE: ISS-2025-0383 - let ISO error balls from the goal propagate unchanged
        // (mirrors CollectionUtils.genericListCollector) instead of flattening the error term
        // into an uncatchable message string.
        } catch (PrologException e) {
            throw e;
        // END_CHANGE: ISS-2025-0383
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("aggregate_all/3: error solving goal: " + e.getMessage());
        }

        Term result;

        if (templateSpec instanceof Atom && "count".equals(((Atom) templateSpec).getName())) {
            // aggregate_all(count, Goal, Count)
            result = new Number(goalSolutions.size());
        } else if (templateSpec instanceof CompoundTerm) {
            String functor = ((CompoundTerm) templateSpec).getName();
            List<Term> specArgs = templateSpec.getArguments();

            switch (functor) {
                case "sum":
                    if (specArgs.size() != 1) throw new PrologEvaluationException("aggregate_all: sum/1 expected.");
                    result = aggregateSum(specArgs.get(0), goalSolutions);
                    break;
                case "max":
                    if (specArgs.size() != 1) throw new PrologEvaluationException("aggregate_all: max/1 expected.");
                    result = aggregateMinMax(specArgs.get(0), goalSolutions, true);
                    // START_CHANGE: ISS-2025-0413 - SWI: max/min FAIL when Goal has no solutions
                    if (result == null) return false;
                    // END_CHANGE: ISS-2025-0413
                    break;
                case "min":
                    if (specArgs.size() != 1) throw new PrologEvaluationException("aggregate_all: min/1 expected.");
                    result = aggregateMinMax(specArgs.get(0), goalSolutions, false);
                    // START_CHANGE: ISS-2025-0413 - SWI: max/min FAIL when Goal has no solutions
                    if (result == null) return false;
                    // END_CHANGE: ISS-2025-0413
                    break;
                case "bag":
                    if (specArgs.size() != 1) throw new PrologEvaluationException("aggregate_all: bag/1 expected.");
                    result = collectBag(specArgs.get(0), goalSolutions);
                    break;
                case "set":
                    if (specArgs.size() != 1) throw new PrologEvaluationException("aggregate_all: set/1 expected.");
                    result = collectSet(specArgs.get(0), goalSolutions);
                    break;
                default:
                    // Treat as template like findall
                    result = collectBag(templateSpec, goalSolutions);
            }
        } else {
            // Unknown template — collect as bag
            result = collectBag(templateSpec, goalSolutions);
        }

        Map<String, Term> nb = new HashMap<>(bindings);
        if (resultVar.resolveBindings(bindings).unify(result, nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    // START_CHANGE: ISS-2025-0414 - exact big-integer sums (BigInteger accumulator), integer
    // sums stay integers, float contagion gives a float result, and a non-numeric solution
    // raises type_error(number, T) instead of being silently skipped.
    private Term aggregateSum(Term template, List<Map<String, Term>> goalSolutions) {
        java.math.BigInteger intSum = java.math.BigInteger.ZERO;
        double floatSum = 0.0;
        boolean sawFloat = false;
        for (Map<String, Term> sol : goalSolutions) {
            Term resolved = template.copy().resolveBindings(sol);
            if (!(resolved instanceof Number)) {
                throw new PrologException(ISOErrorTerms.typeError("number", resolved, "aggregate_all/3"));
            }
            Number n = (Number) resolved;
            if (n.isInteger() && !sawFloat) {
                intSum = intSum.add(n.bigIntegerValue());
            } else {
                if (!sawFloat) {
                    sawFloat = true;
                    floatSum = intSum.doubleValue();
                }
                floatSum += n.doubleValue();
            }
        }
        return sawFloat ? new Number(floatSum, false) : new Number(intSum);
    }
    // END_CHANGE: ISS-2025-0414

    // START_CHANGE: ISS-2025-0413 - SWI semantics: returns null (-> aggregate_all fails) when
    // Goal has no solutions instead of throwing a bare-text exception, raises
    // type_error(number, T) on a non-numeric solution instead of leaving the ±Infinity seed,
    // and compares exactly (BigInteger) so big-integer extrema survive.
    private Term aggregateMinMax(Term template, List<Map<String, Term>> goalSolutions, boolean isMax) {
        Term best = null;
        for (Map<String, Term> sol : goalSolutions) {
            Term resolved = template.copy().resolveBindings(sol);
            if (!(resolved instanceof Number)) {
                throw new PrologException(ISOErrorTerms.typeError("number", resolved, "aggregate_all/3"));
            }
            if (best == null) {
                best = resolved;
                continue;
            }
            int c = numCompare((Number) resolved, (Number) best);
            if (isMax ? c > 0 : c < 0) best = resolved;
        }
        return best;   // null when Goal had no solutions
    }

    private static int numCompare(Number x, Number y) {
        if (x.isInteger() && y.isInteger()) {
            return x.bigIntegerValue().compareTo(y.bigIntegerValue());
        }
        return Double.compare(x.doubleValue(), y.doubleValue());
    }
    // END_CHANGE: ISS-2025-0413

    private Term collectBag(Term template, List<Map<String, Term>> goalSolutions) {
        List<Term> collected = new ArrayList<>();
        for (Map<String, Term> sol : goalSolutions) {
            collected.add(template.copy().resolveBindings(sol));
        }
        return CollectionUtils.createListTerm(collected);
    }

    private Term collectSet(Term template, List<Map<String, Term>> goalSolutions) {
        List<Term> collected = new ArrayList<>();
        Set<String> seen = new LinkedHashSet<>();
        for (Map<String, Term> sol : goalSolutions) {
            Term resolved = template.copy().resolveBindings(sol);
            String key = resolved.toString();
            if (seen.add(key)) {
                collected.add(resolved);
            }
        }
        // START_CHANGE: ISS-2025-0193 - Use ISO term ordering instead of toString comparison
        Collections.sort(collected, (a, b) -> it.denzosoft.jprolog.builtin.list.Sort.compareTerms(a, b));
        // END_CHANGE: ISS-2025-0193
        return CollectionUtils.createListTerm(collected);
    }
}
// END_CHANGE: ISS-2025-0122
