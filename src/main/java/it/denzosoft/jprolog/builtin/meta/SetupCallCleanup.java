package it.denzosoft.jprolog.builtin.meta;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * setup_call_cleanup(:Setup, :Goal, :Cleanup) and call_cleanup(:Goal, :Cleanup).
 *
 * START_CHANGE: ISS-2025-0273 - ISO/de-facto resource-safety meta-predicate.
 * Setup is run once; Goal is then executed and Cleanup is run EXACTLY ONCE when Goal
 * finishes (all solutions exhausted, failure, or an exception). If Setup fails or raises,
 * Cleanup is not run. Simplified for this engine's eager-solution model: Goal's solutions
 * are collected, then Cleanup runs; on an exception from Goal, Cleanup runs before the
 * exception is re-raised.
 * END_CHANGE: ISS-2025-0273
 */
public class SetupCallCleanup implements BuiltInWithContext {

    private final SolverContext querySolver;

    public SetupCallCleanup(SolverContext querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query,
                                      Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        Term setup, goal, cleanup;
        if (args.size() == 3) {
            setup = args.get(0);
            goal = args.get(1);
            cleanup = args.get(2);
        } else if (args.size() == 2) {
            setup = new Atom("true"); // call_cleanup/2 == setup_call_cleanup(true, Goal, Cleanup)
            goal = args.get(0);
            cleanup = args.get(1);
        } else {
            throw new PrologException(instErr("setup_call_cleanup requires 2 or 3 arguments"));
        }

        Term setupResolved = setup.resolveBindings(bindings);
        if (setupResolved instanceof Variable
                || goal.resolveBindings(bindings) instanceof Variable
                || cleanup.resolveBindings(bindings) instanceof Variable) {
            throw new PrologException(instErr("setup_call_cleanup: arguments must be instantiated"));
        }

        // Run Setup once. If it fails or raises, propagate without running Cleanup.
        List<Map<String, Term>> setupSols = new ArrayList<>();
        boolean setupOk = solver.solveMeta(setupResolved, new HashMap<>(bindings), setupSols)   /* ISS-2025-0431 - ENG-04 */;
        if (!setupOk || setupSols.isEmpty()) {
            return false;
        }
        Map<String, Term> afterSetup = setupSols.get(0); // once(Setup)

        // Run Goal, guaranteeing Cleanup runs exactly once afterward.
        boolean cleaned = false;
        try {
            List<Map<String, Term>> goalSols = new ArrayList<>();
            solver.solveMeta(goal.resolveBindings(afterSetup), new HashMap<>(afterSetup),
                    goalSols)   /* ISS-2025-0431 - ENG-04 */;
            runCleanup(solver, cleanup.resolveBindings(afterSetup), afterSetup);
            cleaned = true;
            if (goalSols.isEmpty()) {
                return false;
            }
            solutions.addAll(goalSols);
            return true;
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            if (!cleaned) {
                runCleanup(solver, cleanup.resolveBindings(afterSetup), afterSetup);
            }
            throw e;
        }
    }

    private void runCleanup(SolverContext solver, Term cleanup, Map<String, Term> b) {
        try {
            List<Map<String, Term>> cs = new ArrayList<>();
            solver.solveMeta(cleanup, new HashMap<>(b), cs)   /* ISS-2025-0431 - ENG-04 */;
        } catch (RuntimeException ignore) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(ignore);   // ISS-2025-0431
            // Best-effort cleanup: suppress cleanup errors so they do not mask the goal result.
        }
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("setup_call_cleanup requires context");
    }

    private Term instErr(String ctx) {
        return new CompoundTerm(new Atom("error"),
                java.util.Arrays.asList(new Atom("instantiation_error"), new Atom(ctx)));
    }
}
