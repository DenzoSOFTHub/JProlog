package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0479 - engine v4 wave W8, design B.13: one Machine per thread.
/**
 * Runs a goal on a <b>fresh {@link Machine} over the same {@link Engine}</b>, from any thread.
 *
 * <p>This is what takes the concurrency predicates off the recursive solver
 * (limit <b>LIM-024</b>, deviation 7 of section 9.4 of the progress report — the last routine path
 * into it). A {@code Machine} is single-threaded by
 * construction (one goal stack, one choice-point list, and bindings that live in shared
 * {@code Variable} cells), so a worker cannot borrow the caller's machine — it gets its own.
 *
 * <p>What is shared and what is not:
 * <ul>
 *   <li><b>Shared</b>: the {@link Engine} — the clause store (thread-safe by birth/death
 *       generations), the module owner, the tabling store, the native and legacy built-in tables,
 *       the {@code KnowledgeBase}. {@code assertz}/{@code retract} from two threads are therefore
 *       visible to both.</li>
 *   <li><b>Shared</b>: the engine's {@code PrologFlags} and {@link EngineState} — installed as the
 *       worker thread's current state for the duration, so the worker sees the same flags,
 *       operators and stream table. {@code current_input}/{@code current_output} are per thread
 *       <em>inside</em> {@code Streams}, so a worker starts on the standard streams and cannot
 *       disturb the parent's (design B.11).</li>
 *   <li><b>Not shared</b>: the goal's TERM. Bindings live in the cells, so a cell reachable from
 *       two machines would be bound without either machine's trail knowing. Every goal handed to a
 *       worker is therefore {@code copy_term}'d in, and every answer is copied back out.</li>
 *   <li><b>Not shared</b>: the {@link ResourceGuard}. Each worker gets its own with the
 *       <em>same limit</em> as the parent, so one worker's steps cannot exhaust another's budget
 *       and each worker still polls its own thread's interrupt flag.</li>
 * </ul>
 *
 * <p>Cancellation: interrupting a worker thread makes its guard raise
 * {@code QueryCancelledException} (a plain {@code RuntimeException} — the trust model of
 * invariant 9), which is how a parent that is itself interrupted stops its workers.
 *
 * <p><b>A worker owns no query boundary</b> ({@link Machine#asWorker()}): the sweeps at the end of
 * {@link Machine#solve} — restoring the shared {@code EngineContext}'s guard, compacting the clause
 * store and {@code Tabling.endQuery()} — are engine-wide and belong to the top-level query. A
 * worker finishing first would otherwise abandon the parent's in-progress tabled evaluation.
 *
 * <p><b>Known limit</b>: the engine's tabling store is not itself thread-safe. Two tabled
 * evaluations running concurrently on the same engine share one variant table, one producing stack
 * and one answer list; the answers of a table completed by one thread are fine to consume from
 * another, but two threads PRODUCING at once is undefined. Table a predicate before you fan out, or
 * keep tabled goals on one thread.
 */
public final class Workers {

    private Workers() {}

    /** How a worker reports one answer: the parent's variable NAMES to copied values. */
    public interface AnswerSink { boolean onAnswer(Map<String, Term> answer); }

    /**
     * Run {@code goal} to completion (or until {@code sink} says stop) on a new machine.
     *
     * @param engine  the shared engine
     * @param budget  the inference budget for this worker (0 = unlimited)
     * @param goal    the goal, in the CALLER's cells — it is copied before it reaches the machine
     * @param sink    receives one map per solution, keyed by the caller's variable names, holding
     *                terms that share no cell with either machine
     * @return true when at least one solution was found
     */
    public static boolean run(Engine engine, long budget, Term goal, AnswerSink sink) {
        final IdentityHashMap<Variable, Variable> in = new IdentityHashMap<Variable, Variable>();
        Term workerGoal = Unify.copy(Unify.resolve(goal, null), in, null);

        // The caller's unbound cells, paired with the copy the worker will bind.
        final List<Variable> origs = new ArrayList<Variable>();
        final List<Variable> copies = new ArrayList<Variable>();
        for (Map.Entry<Variable, Variable> e : in.entrySet()) {
            if (e.getKey().ref == null) { origs.add(e.getKey()); copies.add(e.getValue()); }
        }

        Prolog prolog = engine.prolog();
        it.denzosoft.jprolog.core.system.PrologFlags prevFlags = (prolog == null) ? null
            : it.denzosoft.jprolog.core.system.PrologFlags.setCurrent(prolog.getFlags());
        EngineState prevState = (prolog == null) ? null
            : EngineState.setCurrent(prolog.getEngineState());
        final boolean[] any = {false};
        try {
            final Machine m = new Machine(engine, new ResourceGuard(budget));
            m.asWorker();   // engine-wide query-boundary sweeps belong to the top-level query
            m.solve(workerGoal, new Machine.SolutionSink() {
                @Override public boolean onSolution(Map<String, Term> ignored) {
                    any[0] = true;
                    Map<String, Term> answer = new HashMap<String, Term>();
                    for (int i = 0; i < origs.size(); i++) {
                        Term v = copies.get(i);
                        // resolve inside the worker, then copy OUT so the answer shares no cell
                        Term resolved = Unify.resolve(v, null);
                        Term detached = Unify.copy(resolved, new IdentityHashMap<Variable, Variable>(), null);
                        answer.put(origs.get(i).getName(), detached);
                    }
                    return sink.onAnswer(answer);
                }
            });
        } finally {
            if (prolog != null) {
                it.denzosoft.jprolog.core.system.PrologFlags.setCurrent(prevFlags);
                EngineState.setCurrent(prevState);
            }
        }
        return any[0];
    }

    /**
     * The eager form the legacy {@code BuiltIn} protocol needs: collect at most
     * {@code maxSolutions} answers (0 = all) into {@code solutions}.
     */
    public static boolean solve(Engine engine, long budget, Term goal, Map<String, Term> bindings,
                                final List<Map<String, Term>> solutions, final int maxSolutions) {
        Term g = (bindings == null || bindings.isEmpty()) ? goal : goal.resolveBindings(bindings);
        final Map<String, Term> base = (bindings == null || bindings.isEmpty()) ? null : bindings;
        return run(engine, budget, g, new AnswerSink() {
            @Override public boolean onAnswer(Map<String, Term> answer) {
                Map<String, Term> merged;
                if (base == null) {
                    merged = answer;
                } else {
                    merged = new HashMap<String, Term>(base);
                    merged.putAll(answer);
                }
                solutions.add(merged);
                return maxSolutions <= 0 || solutions.size() < maxSolutions;
            }
        });
    }
}
// END_CHANGE: ISS-2025-0479
