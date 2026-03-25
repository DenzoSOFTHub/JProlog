package it.denzosoft.jprolog.builtin.meta;

// START_CHANGE: ISS-2025-0122 - aggregate_all/3 built-in predicate
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
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
 * Always succeeds (returns 0/[]/etc. for no solutions).
 */
public class AggregateAll implements BuiltInWithContext {

    private final QuerySolver querySolver;

    public AggregateAll(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'aggregate_all' must be invoked with context");
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        QuerySolver qs = solver != null ? solver : this.querySolver;
        if (qs == null) throw new PrologEvaluationException("aggregate_all/3: no query solver available.");

        List<Term> args = query.getArguments();
        if (args.size() != 3) throw new PrologEvaluationException("aggregate_all/3 requires 3 arguments.");

        Term templateSpec = args.get(0).resolveBindings(bindings);
        Term goal = args.get(1).resolveBindings(bindings);
        Term resultVar = args.get(2);

        // Solve the goal
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        try {
            qs.solve(goal, bindings, goalSolutions, CutStatus.notOccurred());
        } catch (Exception e) {
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
                    break;
                case "min":
                    if (specArgs.size() != 1) throw new PrologEvaluationException("aggregate_all: min/1 expected.");
                    result = aggregateMinMax(specArgs.get(0), goalSolutions, false);
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

    private Term aggregateSum(Term template, List<Map<String, Term>> goalSolutions) {
        double sum = 0;
        for (Map<String, Term> sol : goalSolutions) {
            Term resolved = template.copy().resolveBindings(sol);
            if (resolved instanceof Number) {
                sum += ((Number) resolved).getValue();
            }
        }
        return new Number(sum);
    }

    private Term aggregateMinMax(Term template, List<Map<String, Term>> goalSolutions, boolean isMax) {
        if (goalSolutions.isEmpty()) {
            throw new PrologEvaluationException("aggregate_all: " + (isMax ? "max" : "min") + " requires at least one solution.");
        }
        double result = isMax ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;
        for (Map<String, Term> sol : goalSolutions) {
            Term resolved = template.copy().resolveBindings(sol);
            if (resolved instanceof Number) {
                double v = ((Number) resolved).getValue();
                if (isMax ? v > result : v < result) result = v;
            }
        }
        return new Number(result);
    }

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
