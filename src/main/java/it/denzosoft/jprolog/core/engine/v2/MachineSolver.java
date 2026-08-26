package it.denzosoft.jprolog.core.engine.v2;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInRegistry;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.DebugEvent;
import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.EngineContext;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.engine.TableStore;
import it.denzosoft.jprolog.core.module.Module;
import it.denzosoft.jprolog.core.module.ModuleManager;
import it.denzosoft.jprolog.core.module.PredicateSignature;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Clean-room prototype of a new resolution engine core, addressing the architectural debt of the
 * eager recursive solver (LIM-023/024). Three design changes, validated by {@code MachineSolverTest}:
 *
 * <ol>
 *   <li><b>Mutable bindings + trail</b> instead of copying a {@code Map<String,Term>} substitution
 *       per step: unification binds into one store and records each binding on a trail; backtracking
 *       undoes to a mark in O(changes), not O(depth × bindings).</li>
 *   <li><b>Lazy enumeration</b>: solutions are produced one at a time through a {@link SolutionSink};
 *       a caller can stop after the first, so cut prunes correctly and {@code repeat}-style generators
 *       do not blow up memory (the eager model collects every solution up-front).</li>
 *   <li><b>Iterative SLD machine</b> with an explicit goal stack + choice-point stack on the heap, so
 *       deep recursion does <b>not</b> overflow the Java call stack (the eager solver recurses in Java
 *       and dies with {@code StackOverflowError}).</li>
 * </ol>
 *
 * <p>Since v3.1.0 this is the DEFAULT resolution engine ({@code -Djprolog.engine=legacy} falls back
 * to the recursive solver): {@code Prolog.solve} builds a fresh MachineSolver per query
 * over the live KnowledgeBase/BuiltInRegistry, with native dispatch for the control constructs and
 * frequent built-ins, registry delegation for the rest, four-port debug events (ISS-2025-0331),
 * the inference budget (ISS-2025-0339), and CLP(FD)/attribute hooks.
 */
public final class MachineSolver {

    // ----------------------------------------------------------------- knowledge base
    private final Map<String, List<Rule>> kb = new HashMap<>();   // used when liveKb == null
    private final KnowledgeBase liveKb;        // when set, clause lookup + assert/retract delegate here
    private final BuiltInRegistry registry;    // nullable: when set, non-native goals delegate here
    private final EngineContext contextSolver;   // nullable: context handed to BuiltInWithContext builtins
    private final ModuleManager modules;       // nullable: when set, clause lookup is module-aware
    private final TableStore tableStore;       // nullable: when set, tabled predicates delegate to the legacy solver
    private DebugController debugController;    // ISS-2025-0331: nullable; when set, fire four-port debug events
    private long inferenceBudget = 0;          // ISS-2025-0339: max resolution steps (0 = unlimited)
    // START_CHANGE: ISS-2025-0431 - ENG-04: the step counter lives in a ResourceGuard shared with
    // the engine context for the duration of the query, so budget and cancellation also apply
    // to the sub-solves that BuiltInWithContext built-ins run there.
    private it.denzosoft.jprolog.core.engine.ResourceGuard guard =
        new it.denzosoft.jprolog.core.engine.ResourceGuard(0);
    // END_CHANGE: ISS-2025-0431
    private int renameCounter = 0;

    /**
     * Abort the query by throwing {@link it.denzosoft.jprolog.core.engine.InferenceLimitException}
     * (a plain RuntimeException, so {@code catch/3} cannot trap it) after this many machine steps
     * (0 = unlimited).
     *
     * <p><b>Unit</b> (ISS-2025-0427 / ENG-08): a "step" is one iteration of the drive loop, not one
     * logical inference. Conjunction splits, {@code true}, cut and the internal action goals the
     * machine pushes for if-then-else, tracing and cleanup each consume a step, so the count is an
     * upper bound on — and typically 2-4x larger than — the number of predicate calls. It is a
     * runaway-query guard, not a metering device: do not derive LIPS from it.
     */
    public void setInferenceBudget(long budget) { this.inferenceBudget = budget; }

    // START_CHANGE: ISS-2025-0431 - ENG-04: a nested machine (EngineContext.solveMeta) must charge the
    // OUTER query's counter, not start a fresh budget of its own.
    private it.denzosoft.jprolog.core.engine.ResourceGuard inheritedGuard;

    /** Run the next {@link #solve} against {@code g} instead of a fresh guard (null = fresh). */
    public void setResourceGuard(it.denzosoft.jprolog.core.engine.ResourceGuard g) { this.inheritedGuard = g; }
    // END_CHANGE: ISS-2025-0431

    public MachineSolver(List<Rule> rules) { this(rules, null); }

    public MachineSolver(List<Rule> rules, BuiltInRegistry registry) {
        for (Rule r : rules) kb.computeIfAbsent(key(r.getHead()), k -> new ArrayList<>()).add(r);
        this.registry = registry;
        this.liveKb = null;
        this.contextSolver = null;
        this.modules = null;
        this.tableStore = null;
    }

    /** Engine-integrated mode: read clauses from and assert/retract to the live {@link KnowledgeBase}
     *  (module-aware via {@code modules}); delegate {@link BuiltInWithContext} built-ins to {@code contextSolver}. */
    public MachineSolver(KnowledgeBase liveKb, BuiltInRegistry registry, EngineContext contextSolver,
                         ModuleManager modules, TableStore tableStore) {
        this.liveKb = liveKb;
        this.registry = registry;
        this.contextSolver = contextSolver;
        this.modules = modules;
        this.tableStore = tableStore;
    }

    private List<Rule> clausesFor(Term lookup) {
        // Use the module manager for a Module:Goal qualified call (always), and for unqualified goals
        // ONLY when user-defined modules exist (>1 module incl. "user") — so it enforces import/export
        // visibility for module programs. Plain (no-module) programs use the flat KB, because routing
        // every lookup through the module manager changes clause-set/assert semantics and destabilises
        // them (ISS-2025-0314).
        if (modules != null) {
            boolean qualified = lookup instanceof CompoundTerm
                && ":".equals(((CompoundTerm) lookup).getName()) && ((CompoundTerm) lookup).getArguments().size() == 2;
            if (qualified) {
                return qualifiedClauses((CompoundTerm) lookup);     // with export enforcement
            }
            if (modules.getAllModuleNames().size() > 1) {
                try {
                    return modules.getRulesForPredicate(lookup);    // unqualified: current module + imports
                } catch (RuntimeException e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    return null;
                }
            }
        }
        if (liveKb != null) {
            // START_CHANGE: ISS-2025-0433 - ENG-13: FIRST-ARGUMENT INDEXING re-landed on the v2 path,
            // over versioned immutable snapshots. Two things made this safe after the ISS-2025-0340
            // revert: ISS-2025-0344 fixed index maintenance and made an index miss degrade to the
            // FULL clause list, and KnowledgeBase.getClauseSnapshot now always merges the
            // variable-headed bucket and falls back to the full list for any first argument it
            // cannot key (unbound, string, ...). Together with the snapshot cache this removes both
            // the O(#clauses) list copy and the O(#clauses) head unifications per call.
            String f; int ar; Term firstArg = null;
            if (lookup instanceof Atom) { f = ((Atom) lookup).getName(); ar = 0; }
            else {
                CompoundTerm c = (CompoundTerm) lookup;
                f = c.getName(); ar = c.getArguments().size();
                if (ar > 0) firstArg = deref(c.getArguments().get(0));
            }
            return liveKb.getClauseSnapshot(f, ar, firstArg);
            // END_CHANGE: ISS-2025-0433
        }
        return kb.get(key(lookup));
    }

    /** Clauses for a {@code Module:Goal} call, enforcing export visibility when the caller is a
     *  different module (ISS-2025-0314): a non-exported predicate is invisible from outside. */
    private List<Rule> qualifiedClauses(CompoundTerm qc) {
        Term mt = deref(qc.getArguments().get(0));
        Term g = deref(qc.getArguments().get(1));
        if (!(mt instanceof Atom) || !(g instanceof Atom || g instanceof CompoundTerm)) return null;
        Module mod = modules.getModule(((Atom) mt).getName());
        if (mod == null) return null;                              // unknown module -> fail
        String f = (g instanceof Atom) ? ((Atom) g).getName() : ((CompoundTerm) g).getName();
        int ar = (g instanceof Atom) ? 0 : ((CompoundTerm) g).getArguments().size();
        PredicateSignature sig = new PredicateSignature(f, ar);
        // A qualified Module:Goal enforces export visibility: a non-exported predicate is invisible.
        if (mod.resolvePredicateForExternalAccess(sig) == null) {
            return new ArrayList<>();                              // not exported -> not visible
        }
        return mod.getRulesForPredicate(sig);
    }

    private static String key(Term head) {
        if (head instanceof Atom) return ((Atom) head).getName() + "/0";
        if (head instanceof CompoundTerm) return ((CompoundTerm) head).getName() + "/" + ((CompoundTerm) head).getArguments().size();
        return "?";
    }

    // ----------------------------------------------------------------- bindings + trail
    private final Map<String, Term> binding = new HashMap<>();
    // START_CHANGE: ISS-2025-0343 - the trail holds variable names (String -> remove the binding)
    // OR undo actions (Runnable -> run on backtracking), so non-binding effects like disarming a
    // catch frame are undone when execution backtracks into the frame's goal extent.
    private final ArrayList<Object> trail = new ArrayList<>();

    private int mark() { return trail.size(); }
    private void undo(int m) {
        for (int i = trail.size() - 1; i >= m; i--) {
            Object e = trail.get(i);
            if (e instanceof Runnable) ((Runnable) e).run();
            else binding.remove((String) e);
        }
        if (m < trail.size()) trail.subList(m, trail.size()).clear();
    }
    // END_CHANGE: ISS-2025-0343
    // START_CHANGE: ISS-2025-0429 - ENG-10: CONDITIONAL TRAILING. bind() used to append to the trail
    // unconditionally, even when there was no choice point to undo to — a deterministic recursion of
    // N steps left an N-entry trail behind for nothing. A binding only needs trailing when some
    // future undo() can reach it, i.e. when a choice point (or catch frame) exists, or while an
    // explicit mark/undo extent is open (findall/3, \=/2, the catcher unification): those bracket
    // themselves with forceTrail. Bindings made before the newest choice point was pushed are NOT
    // undone by it (its trailMark is the current trail size), so skipping them is sound.
    private int forceTrail = 0;

    private void bind(String var, Term val) {
        binding.put(var, val);
        if (forceTrail > 0 || !cps.isEmpty()) trail.add(var);
    }

    /**
     * Drop the whole trail once nothing can undo it. Every {@code undo(mark)} in the machine takes
     * its mark either from a choice point / catch frame on {@code cps} or from an explicit
     * mark-undo extent (which brackets itself with {@code forceTrail}); when both are gone no live
     * mark exists, so the accumulated entries are pure garbage.
     *
     * <p>This replaces the serial-based conditional trailing proposed by ENG-10 (trail only
     * variables older than the newest choice point). It needs no per-variable serial number, is
     * easier to prove correct, and reclaims MORE: it drops entries made before the last choice
     * point disappeared, not just the ones a serial test would have skipped.
     */
    private void reclaimTrailIfUnreachable() {
        if (forceTrail == 0 && cps.isEmpty() && !trail.isEmpty()) trail.clear();
    }
    // END_CHANGE: ISS-2025-0429

    private Term deref(Term t) {
        while (t instanceof Variable) {
            Term b = binding.get(((Variable) t).getName());
            if (b == null) return t;
            t = b;
        }
        return t;
    }

    /** Does the variable {@code name} occur in {@code term}? (iterative, derefs through bindings.) */
    private boolean occurs(String name, Term term) {
        java.util.ArrayDeque<Term> stack = new java.util.ArrayDeque<>();
        stack.push(term);
        while (!stack.isEmpty()) {
            Term t = deref(stack.pop());
            if (t instanceof Variable) {
                if (((Variable) t).getName().equals(name)) return true;
            } else if (t instanceof CompoundTerm) {
                for (Term arg : ((CompoundTerm) t).getArguments()) stack.push(arg);
            }
        }
        return false;
    }

    /** Coroutining wake-up under v2 (ISS-2025-0318): when an attributed variable {@code v} is bound to
     *  {@code value}, invoke the engine's attribute-unify hook (set by {@code Prolog.solveWithV2Engine})
     *  so freeze/when/dif goals fire (or re-suspend). Returns false if the hook fails the unification. */
    /** Goals woken by binding a frozen variable — drained onto the goal stack so they run in THIS
     *  machine's binding/trail context and their bindings propagate (ISS-2025-0336). */
    private final List<Term> woken = new ArrayList<>();

    private boolean wakeAttrs(Variable v, Term value) {
        if (!v.hasAttributes()) return true;
        // freeze/2: run the delayed goal on the v2 goal stack (NOT via the legacy hook) so the bindings
        // it makes land in this machine's `binding` map. Remove the attribute (trailed) to avoid re-firing.
        Term fg = v.getAttribute(it.denzosoft.jprolog.builtin.control.Freeze.FREEZE_MODULE);
        if (fg != null) {
            final Variable fv = v; final Term ffg = fg;
            v.removeAttribute(it.denzosoft.jprolog.builtin.control.Freeze.FREEZE_MODULE);
            it.denzosoft.jprolog.core.engine.Trail.record(() ->
                fv.putAttribute(it.denzosoft.jprolog.builtin.control.Freeze.FREEZE_MODULE, ffg));
            woken.add(fg);
        }
        // other attribute kinds (when/dif) still go through the legacy attribute-unify hook
        Variable.AttributeUnifyHook hook = Variable.getAttributeUnifyHook();
        if (hook != null && v.hasAttributes()) return hook.onAttributeUnify(v, value, binding);
        return true;
    }

    private boolean unify(Term a, Term b) {
        a = deref(a); b = deref(b);
        if (a instanceof Variable) {
            if (b instanceof Variable && ((Variable) a).getName().equals(((Variable) b).getName())) return true;
            if (Variable.isOccursCheckEnabled() && occurs(((Variable) a).getName(), b)) return false;
            bind(((Variable) a).getName(), b);
            // START_CHANGE: ISS-2025-0355 - var-var aliasing must respect CLP(FD) domains: when the
            // bound variable is FD-constrained, the alias target inherits its FdVar (or, when both
            // are FD-constrained, an equality constraint intersects the domains — disjoint -> fail).
            // Other attribute kinds (freeze/when/dif) keep the established skip-on-var-var behavior.
            if (b instanceof Variable) {
                return !((Variable) a).hasAttributes()
                    || it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.onAlias((Variable) a, (Variable) b);
            }
            // END_CHANGE: ISS-2025-0355
            if (((Variable) a).hasAttributes()) return wakeAttrs((Variable) a, b);
            return true;
        }
        if (b instanceof Variable) {
            if (Variable.isOccursCheckEnabled() && occurs(((Variable) b).getName(), a)) return false;
            bind(((Variable) b).getName(), a);
            if (((Variable) b).hasAttributes()) return wakeAttrs((Variable) b, a);
            return true;
        }
        if (a instanceof Atom && b instanceof Atom) return ((Atom) a).getName().equals(((Atom) b).getName());
        if (a instanceof Number && b instanceof Number) return a.equals(b);
        // START_CHANGE: ISS-2025-0428 - ENG-09: iterate down the LAST argument instead of
        // recursing into it. A list of N cells needed N Java frames here.
        while (a instanceof CompoundTerm && b instanceof CompoundTerm) {
            CompoundTerm ca = (CompoundTerm) a, cb = (CompoundTerm) b;
            List<Term> aa = ca.getArguments(), ba = cb.getArguments();
            if (!ca.getName().equals(cb.getName()) || aa.size() != ba.size()) return false;
            int n = aa.size();
            for (int i = 0; i < n - 1; i++) {
                if (!unify(aa.get(i), ba.get(i))) return false;
            }
            if (n == 0) return true;
            a = deref(aa.get(n - 1));
            b = deref(ba.get(n - 1));
            if (!(a instanceof CompoundTerm) || !(b instanceof CompoundTerm)) {
                return unify(a, b);                       // leaf / variable: one recursion
            }
        }
        return false;
        // END_CHANGE: ISS-2025-0428
    }

    // ----------------------------------------------------------------- machine state
    /** A pending goal plus the choice-point height a {@code !} in it should cut back to. An optional
     *  {@code action} (with no term) runs an internal side effect — used by soft-cut. */
    private static final class Goal {
        final Term term; final int cutBarrier; final Goal next; final Runnable action;
        Goal(Term term, int cutBarrier, Goal next) { this.term = term; this.cutBarrier = cutBarrier; this.next = next; this.action = null; }
        Goal(Runnable action, Goal next) { this.term = null; this.cutBarrier = 0; this.next = next; this.action = action; }
    }
    /** An alternative: install the next goal stack, or return FAILED if it doesn't apply. */
    private interface Alt { Goal apply(); }
    private static final Goal FAILED = new Goal(null, -1, null);

    // START_CHANGE: ISS-2025-0423 - ENG-01/ENG-03: a LAZY, possibly infinite alternative supply.
    // {@code next()} returns the goal stack for the next alternative, {@link #FAILED} to skip it,
    // or {@link #EXHAUSTED} when the generator is spent. (It must NOT use null for that: null is a
    // perfectly good goal stack — it is what an empty continuation looks like when the generator is
    // the query's last goal.) Choice points built on a Gen never materialise
    // their alternatives, so repeat/0 and length/2's enumeration cost O(1) memory per redo
    // instead of pre-building a (bounded!) list of solutions.
    private interface Gen { Goal next(CP cp); }
    /** Sentinel returned by a {@link Gen} that has no more alternatives. */
    private static final Goal EXHAUSTED = new Goal(null, -2, null);
    // END_CHANGE: ISS-2025-0423

    private static final class CP {
        final List<Alt> alts; int idx; final int trailMark;
        final int legacyMark;   // ISS-2025-0316: snapshot of the legacy backtrackable Trail (b_setval, op/3, setarg)
        // catch-frame payload (isCatch == true => no alternatives; used by throw/1 unwinding)
        final boolean isCatch; final Term catcher, recovery; final Goal cont; final int cutBarrier;
        // START_CHANGE: ISS-2025-0343 - a catch frame is armed only while its Goal's extent runs:
        // disarmed when the Goal exits (trailed, so backtracking into the Goal re-arms it).
        boolean active = true;
        // END_CHANGE: ISS-2025-0343
        Term traceGoal = null; int traceDepth = 0;   // ISS-2025-0329: 4-port trace (Redo/Fail) for this goal
        boolean traceDebug = false;                   // ISS-2025-0331: also notify the DebugController
        // START_CHANGE: ISS-2025-0423 - ENG-01/ENG-03: lazy alternative supply (null for list CPs)
        final Gen gen;
        // ISS-2025-0433 - ENG-13: set by a Gen that has just handed out its LAST alternative, so
        // advance() can trust-me pop the frame exactly as it does for a list choice point.
        boolean genExhausted = false;
        // END_CHANGE: ISS-2025-0423
        CP(List<Alt> alts, int trailMark) {
            this.alts = alts; this.trailMark = trailMark; this.gen = null;
            this.legacyMark = it.denzosoft.jprolog.core.engine.Trail.mark();
            this.isCatch = false; this.catcher = null; this.recovery = null; this.cont = null; this.cutBarrier = 0;
        }
        // START_CHANGE: ISS-2025-0423 - lazy-generator choice point
        CP(Gen gen, int trailMark) {
            this.alts = null; this.trailMark = trailMark; this.gen = gen;
            this.legacyMark = it.denzosoft.jprolog.core.engine.Trail.mark();
            this.isCatch = false; this.catcher = null; this.recovery = null; this.cont = null; this.cutBarrier = 0;
        }
        // END_CHANGE: ISS-2025-0423
        CP(int trailMark, Term catcher, Term recovery, Goal cont, int cutBarrier) {
            this.alts = null; this.trailMark = trailMark; this.gen = null;
            this.legacyMark = it.denzosoft.jprolog.core.engine.Trail.mark();
            this.isCatch = true; this.catcher = catcher; this.recovery = recovery; this.cont = cont; this.cutBarrier = cutBarrier;
        }
    }

    private Goal goalStack;
    private final ArrayList<CP> cps = new ArrayList<>();

    // START_CHANGE: ISS-2025-0429 - ENG-10: test hooks pinning the machine's memory invariants
    // (deterministic execution must leave neither choice points nor trail entries behind).
    /** Number of live choice points (including catch frames). Package-private: test hook. */
    int choicePointCount() { return cps.size(); }
    /** Number of live trail entries. Package-private: test hook. */
    int trailSize() { return trail.size(); }
    // END_CHANGE: ISS-2025-0429

    public interface SolutionSink { boolean onSolution(Map<String, Term> solution); }

    /** Solve {@code query}, streaming each solution; the sink returns false to stop. */
    public void solve(Term query, SolutionSink sink) {
        binding.clear(); trail.clear(); cps.clear(); woken.clear();
        forceTrail = 0;                                            // ISS-2025-0429 - ENG-10
        // ISS-2025-0331: pick up the IDE debugger (set on the shared EngineContext) so the v2 engine
        // fires four-port CALL/EXIT/FAIL/REDO events and honours breakpoints/stepping.
        debugController = (contextSolver != null) ? contextSolver.getDebugController() : null;
        List<String> queryVars = new ArrayList<>();
        collectVars(query, queryVars);
        goalStack = new Goal(query, 0, null);
        // START_CHANGE: ISS-2025-0431 - ENG-04: publish this query's budget/cancellation guard on the
        // shared EngineContext so every nested sub-solve charges the SAME counter and sees the SAME
        // interrupt. Restored afterwards (a BuiltInWithContext built-in may re-enter the machine).
        guard = (inheritedGuard != null)                           // ISS-2025-0431 - ENG-04
            ? inheritedGuard
            : new it.denzosoft.jprolog.core.engine.ResourceGuard(inferenceBudget);
        it.denzosoft.jprolog.core.engine.ResourceGuard prevGuard =
            (contextSolver != null) ? contextSolver.getResourceGuard() : null;
        if (contextSolver != null) contextSolver.setResourceGuard(guard);
        try {
            drive(() -> sink.onSolution(snapshot(queryVars)), 0);
        } finally {
            if (contextSolver != null) contextSolver.setResourceGuard(prevGuard);
        }
        // END_CHANGE: ISS-2025-0431
    }

    private interface Driver { boolean onSolution(); }

    /** Run the machine until exhausted. {@code floor} is the choice-point height below which this
     *  run must not backtrack, so a nested run (findall/catch) leaves the caller's choice points. */
    private void drive(Driver onSol, int floor) {
        while (true) {
          // Cancellation: the IDE Stop button interrupts the solver thread; abort the query promptly
          // (a non-PrologException so user catch/3 cannot trap it). (ISS-2025-0320)
          // Inference budget: hard per-query step cap. Thrown as a NON-PrologException so untrusted
          // catch/3 cannot trap it and loop forever — it propagates to the embedder. (ISS-2025-0339)
          // ISS-2025-0431 - ENG-04: both now go through the guard shared with the legacy solver.
          // ISS-2025-0435 - ENG-15: step() polls Thread.isInterrupted() once every 1024 steps
          // instead of on every single drive iteration; at engine speed that is still sub-millisecond
          // latency for the Stop button, and it takes a volatile read off the hottest path.
          guard.step();
          try {
            if (!woken.isEmpty()) {                                    // freeze-woken goals run next (ISS-0336)
                for (int i = woken.size() - 1; i >= 0; i--) goalStack = new Goal(woken.get(i), cps.size(), goalStack);
                woken.clear();
                continue;
            }
            if (goalStack == null) {                                   // all goals solved -> a solution
                if (!onSol.onSolution() || !backtrack(floor)) return;
                continue;
            }
            Goal g = goalStack;
            goalStack = g.next;
            if (g.action != null) { g.action.run(); continue; }       // soft-cut side effect
            Term t = deref(g.term);

            if (t instanceof Atom) {
                String n = ((Atom) t).getName();
                if ("true".equals(n)) continue;
                if ("fail".equals(n) || "false".equals(n)) { if (!backtrack(floor)) return; continue; }
                if ("!".equals(n)) { cut(g.cutBarrier); continue; }
                // START_CHANGE: ISS-2025-0423 - ENG-01: repeat/0 is an INFINITE choice point.
                // The registry built-in materialised exactly 1000 copies of the binding map, so
                // `repeat, ..., Done, !` silently FAILED after 1000 iterations (and cost 1000 full
                // map copies up front). Handled natively here — including while debugging, where
                // the bridge would otherwise reintroduce the bound — with the four ports emitted.
                if ("repeat".equals(n)) { repeat(t); continue; }
                // END_CHANGE: ISS-2025-0423
                int rb0 = bridgeBuiltin(t, n, 0);
                if (rb0 == 1) continue;
                if (rb0 == 0) { if (!backtrack(floor)) return; continue; }
                if (!callUser(t, t)) { if (!backtrack(floor)) return; }
                continue;
            }
            if (t instanceof CompoundTerm) {
                CompoundTerm c = (CompoundTerm) t;
                String f = c.getName(); List<Term> a = c.getArguments();
                if (",".equals(f) && a.size() == 2) {
                    goalStack = new Goal(a.get(0), g.cutBarrier, new Goal(a.get(1), g.cutBarrier, goalStack));
                    continue;
                }
                if (";".equals(f) && a.size() == 2) {
                    Term left = deref(a.get(0));
                    if (left instanceof CompoundTerm && ((CompoundTerm) left).getArguments().size() == 2
                            && "->".equals(((CompoundTerm) left).getName())) {       // (C -> T ; E)
                        CompoundTerm arrow = (CompoundTerm) left;
                        ite(arrow.getArguments().get(0), arrow.getArguments().get(1), a.get(1), g.cutBarrier);
                    } else if (left instanceof CompoundTerm && ((CompoundTerm) left).getArguments().size() == 2
                            && "*->".equals(((CompoundTerm) left).getName())) {      // (C *-> T ; E)
                        CompoundTerm sc = (CompoundTerm) left;
                        softCut(sc.getArguments().get(0), sc.getArguments().get(1), a.get(1), g.cutBarrier);
                    } else {
                        disjunction(a.get(0), a.get(1), g.cutBarrier);
                    }
                    continue;
                }
                if ("->".equals(f) && a.size() == 2) {                // (C -> T)  ==  (C -> T ; fail)
                    ite(a.get(0), a.get(1), new Atom("fail"), g.cutBarrier);
                    continue;
                }
                if ("=".equals(f) && a.size() == 2 && debugController == null) {
                    if (!unify(a.get(0), a.get(1))) { if (!backtrack(floor)) return; }
                    continue;
                }   // when debugging, =/2 is routed through the bridge so it is traced (ISS-2025-0332)
                if (("\\+".equals(f) || "not".equals(f)) && a.size() == 1) {     // negation as failure
                    ite(a.get(0), new Atom("fail"), new Atom("true"), g.cutBarrier);
                    continue;
                }
                if ("call".equals(f) && a.size() >= 1) {              // call/N is opaque to cut
                    Term goal = (a.size() == 1) ? a.get(0)
                        : addArgs(deref(a.get(0)), a.subList(1, a.size()));
                    goalStack = new Goal(goal, cps.size(), goalStack);
                    continue;
                }
                // START_CHANGE: ISS-2025-0398 - V^Goal as an ordinary goal behaves as call(Goal)
                // (SWI/SICStus/YAP consensus; the quantifier only matters inside bagof/setof,
                // which strip it themselves before solving). Opaque to cut, like call/1.
                if ("^".equals(f) && a.size() == 2) {
                    goalStack = new Goal(a.get(1), cps.size(), goalStack);
                    continue;
                }
                // END_CHANGE: ISS-2025-0398
                if (":".equals(f) && a.size() == 2) {                 // Module:Goal
                    Term inner = deref(a.get(1));
                    if (modules != null && (inner instanceof Atom || inner instanceof CompoundTerm)) {
                        // module-aware: find Goal's clauses in the named module, unify against Goal
                        if (!callUser(inner, t)) { if (!backtrack(floor)) return; }
                    } else {
                        goalStack = new Goal(inner, cps.size(), goalStack);   // no module system: just run Goal
                    }
                    continue;
                }
                if ("findall".equals(f) && a.size() == 3) {
                    // START_CHANGE: ISS-2025-0416 - ISO 8.10.1.3(c): Instances must be a list or
                    // partial list -> type_error(list, Instances) instead of silent failure.
                    it.denzosoft.jprolog.core.utils.CollectionUtils
                        .checkInstancesArgument(resolve(a.get(2)), "findall/3");
                    // END_CHANGE: ISS-2025-0416
                    Term list = makeList(findAll(a.get(0), a.get(1)));
                    if (!unify(a.get(2), list)) { if (!backtrack(floor)) return; }
                    continue;
                }
                if ("catch".equals(f) && a.size() == 3) {            // install a catch frame, then run Goal
                    // START_CHANGE: ISS-2025-0343 - disarm the frame when Goal's extent exits (ISO
                    // 7.8.9: the catcher applies only DURING the execution of Goal). The disarm runs
                    // as an action goal between Goal and the continuation; its undo is trailed, so
                    // backtracking into a choice point inside Goal re-arms the frame for re-execution.
                    final CP frame = new CP(mark(), a.get(1), a.get(2), goalStack, g.cutBarrier);
                    cps.add(frame);
                    goalStack = new Goal(a.get(0), cps.size(), new Goal(() -> {   // opaque to cut
                        frame.active = false;
                        trail.add((Runnable) () -> frame.active = true);
                    }, goalStack));
                    // END_CHANGE: ISS-2025-0343
                    continue;
                }
                if ("throw".equals(f) && a.size() == 1) {            // raised as a Java exception,
                    // START_CHANGE: ISS-2025-0363 - throw(Ball) with Ball unbound must raise
                    // instantiation_error (ISO 7.8.10.3), not throw the fresh variable as the ball
                    // (which would unify with ANY catcher). Matches the legacy Throw builtin.
                    Term ball = resolve(a.get(0));
                    if (ball instanceof Variable) {
                        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("throw/1"));
                    }
                    // START_CHANGE: ISS-2025-0427 - ENG-08: the ball was renamed HERE and again in
                    // drive()'s catch clause (which renames every ball it routes). One copy is
                    // enough; resolve() already detached it from the bindings that unwind.
                    throw new it.denzosoft.jprolog.core.exceptions.PrologException(ball);  // caught by drive()
                    // END_CHANGE: ISS-2025-0427
                    // END_CHANGE: ISS-2025-0363
                }
                if (("assertz".equals(f) || "assert".equals(f)) && a.size() == 1) { assertClause(a.get(0), false); continue; }
                if ("asserta".equals(f) && a.size() == 1) { assertClause(a.get(0), true); continue; }
                if ("retract".equals(f) && a.size() == 1) {
                    // START_CHANGE: ISS-2025-0396 - retract/1 is re-executable (ISO 8.9.3): it
                    // pushes a choice point over the matching clauses, retracting one per solution.
                    if (!retractClause(a.get(0))) { if (!backtrack(floor)) return; }
                    // END_CHANGE: ISS-2025-0396
                    continue;
                }
                // START_CHANGE: ISS-2025-0431 - ENG-04: the common meta-calls run NATIVELY on the
                // machine instead of being bridged to the recursive legacy solver. On the
                // legacy path they were eager, ~70x slower per inference, and (before the
                // ResourceGuard) invisible to the budget, the Stop interrupt and the v2 trace.
                // Expressed with the machine's own control constructs, which already give them the
                // right cut opacity: a `!` inside the goal is local to it.
                //   once(G)     == (G -> true)
                //   ignore(G)   == (G -> true ; true)
                //   forall(C,A) == \+ (C, \+ A)
                // While debugging/tracing the goals keep going through the registry bridge so the
                // four ports fire exactly as before (same rule as the =/2 and solveBuiltin fast
                // paths); the ResourceGuard bounds that path regardless.
                if (!debugTraceActive()) {
                    if ("once".equals(f) && a.size() == 1) {
                        ite(a.get(0), ATOM_TRUE, ATOM_FAIL, g.cutBarrier);
                        continue;
                    }
                    if ("ignore".equals(f) && a.size() == 1) {
                        ite(a.get(0), ATOM_TRUE, ATOM_TRUE, g.cutBarrier);
                        continue;
                    }
                    if ("forall".equals(f) && a.size() == 2) {
                        Term negAction = new CompoundTerm(new Atom("\\+"),
                            java.util.Collections.singletonList(a.get(1)));
                        Term conj = new CompoundTerm(new Atom(","), Arrays.asList(a.get(0), negAction));
                        ite(conj, ATOM_FAIL, ATOM_TRUE, g.cutBarrier);
                        continue;
                    }
                    // ENG-12: between/3 as a LAZY generator (see betweenNative)
                    if ("between".equals(f) && a.size() == 3) {
                        int rb2 = betweenNative(t, a);
                        if (rb2 == 1) continue;
                        if (rb2 == 0) { if (!backtrack(floor)) return; continue; }
                    }
                }
                // END_CHANGE: ISS-2025-0431
                // START_CHANGE: ISS-2025-0425 - ENG-03: length/2 in the (partial list, unbound
                // length) mode enumerates N = Prefix, Prefix+1, ... as a lazy infinite choice point
                // (ISO/SWI). The Java built-in only handles (proper list, _) and (_, integer) and
                // returned false for every other mode, so `length(L,N), N >= 3, !` and
                // `length([a|T], N)` failed. The deterministic modes still go to the built-in.
                if ("length".equals(f) && a.size() == 2 && lengthEnumerate(t, a)) continue;
                // END_CHANGE: ISS-2025-0425
                if (debugController == null) {                    // fast-path native builtins (skipped
                    int r = solveBuiltin(t, f, a);               // while debugging so they trace via the bridge)
                    if (r == 1) continue;
                    if (r == 0) { if (!backtrack(floor)) return; continue; }
                }
                int rb = bridgeBuiltin(t, f, a.size());
                if (rb == 1) continue;
                if (rb == 0) { if (!backtrack(floor)) return; continue; }
                if (!callUser(t, t)) { if (!backtrack(floor)) return; }
                continue;
            }
            // ISS-2025-0337: a non-callable in goal position is an ISO error, not a silent failure —
            // an unbound variable -> instantiation_error; any other non-callable (number, string) ->
            // type_error(callable, Term). (Reaches call/123, X (uncalled var), etc.)
            if (t instanceof Variable) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("call"));
            }
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("callable", resolve(t), "call"));
          } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            // route the ball to the nearest catch frame within this run's floor; if none, re-throw
            // so an enclosing drive (lower floor) — e.g. a catch/3 around findall/3 — can handle it.
            Term ball = e.getErrorTerm();
            if (ball == null) throw e;
            if (!handleBall(rename(ball, renameCounter++, new HashMap<>()), floor)) throw e;
          }
        }
    }

    // ----------------------------------------------------------------- control
    private void cut(int barrier) {
        while (cps.size() > barrier) cps.remove(cps.size() - 1);
        reclaimTrailIfUnreachable();          // ISS-2025-0429 - ENG-10
    }

    private void disjunction(Term left, Term right, int cutBarrier) {
        final Goal cont = goalStack;
        List<Alt> alts = Arrays.asList(
            () -> new Goal(left, cutBarrier, cont),
            () -> new Goal(right, cutBarrier, cont));
        CP cp = new CP(alts, mark());
        cps.add(cp);
        advance(cp);                                                   // first branch always installs
    }

    private static final Atom CUT = new Atom("!");

    /** (Cond -> Then ; Else): commit to Cond's first solution, then Then; else Else. */
    private void ite(Term cond, Term then, Term els, int cutBarrier) {
        final Goal cont = goalStack;
        final int barrier = cps.size();                               // cut target = this ITE choice point
        // START_CHANGE: ISS-2025-0342 - Cond runs as call(Cond): a user '!' inside it is local
        // (barrier ABOVE the ITE CP), so it cannot cut away the pending Else alternative; only the
        // internal commit CUT (fired when Cond succeeds) cuts back to the ITE CP itself.
        final Goal alt1 = new Goal(cond, barrier + 1, new Goal(CUT, barrier, new Goal(then, cutBarrier, cont)));
        // END_CHANGE: ISS-2025-0342
        final Goal alt2 = new Goal(els, cutBarrier, cont);
        List<Alt> alts = Arrays.asList(() -> alt1, () -> alt2);
        CP cp = new CP(alts, mark());
        cps.add(cp);
        advance(cp);
    }

    /** (Cond *-> Then ; Else): if Cond has any solution, run Then for EACH (no commit); else Else. */
    private void softCut(Term cond, Term then, Term els, int cutBarrier) {
        final Goal cont = goalStack;
        final boolean[] found = {false};
        // START_CHANGE: ISS-2025-0342 - a user '!' inside Cond is local (barrier above this CP)
        final Goal alt1 = new Goal(cond, cps.size() + 1, new Goal(() -> found[0] = true, new Goal(then, cutBarrier, cont)));
        // END_CHANGE: ISS-2025-0342
        List<Alt> alts = Arrays.asList(
            () -> alt1,
            () -> found[0] ? FAILED : new Goal(els, cutBarrier, cont));
        CP cp = new CP(alts, mark());
        cps.add(cp);
        advance(cp);
    }

    // START_CHANGE: ISS-2025-0423 - ENG-01: repeat/0 as an infinite choice point.
    /** {@code repeat} — succeed now and on every redo, forever, in O(1) memory. */
    private void repeat(Term goal) {
        final Goal cont = goalStack;
        final boolean traced = debugTraceActive();
        final int depth = cps.size();
        if (traced) portCall(goal, depth);
        final Goal body = traced ? new Goal(() -> portExit(goal, depth), cont) : cont;
        CP cp = new CP(unused -> body, mark());
        if (traced) { cp.traceGoal = goal; cp.traceDepth = depth; cp.traceDebug = (debugController != null); }
        cps.add(cp);
        advance(cp);                                              // an infinite generator never fails
    }
    // END_CHANGE: ISS-2025-0423

    // START_CHANGE: ISS-2025-0431 - ENG-04: shared atoms for the native meta-call expansions
    private static final Atom ATOM_TRUE = new Atom("true");
    private static final Atom ATOM_FAIL = new Atom("fail");
    // END_CHANGE: ISS-2025-0431

    // START_CHANGE: ISS-2025-0432 - ENG-12: between/3 as a lazy generator.
    /**
     * {@code between(+Low, +High, -Value)} in the ENUMERATION mode, as a lazy choice point: one
     * {@code Number} per redo instead of the eager built-in's up-front list of every solution
     * (each a full binding-map copy). {@code between(1,2000000,X), X >= 2000000} used to exhaust a
     * 256 MB heap before producing its first solution; {@code between(1, inf, X)} was silently
     * capped at a million solutions. Returns 1 = succeeded, 0 = failed, -1 = mode not handled here
     * (the registry built-in keeps every other mode and all the ISO error cases).
     */
    private int betweenNative(Term goal, List<Term> a) {
        Term lo = deref(a.get(0)), hi = deref(a.get(1)), v = deref(a.get(2));
        if (!(v instanceof Variable)) return -1;                    // check mode -> built-in
        if (!(lo instanceof Number) || !((Number) lo).isInteger() || !((Number) lo).fitsInLong()) return -1;
        long low = ((Number) lo).longValue();
        long high;
        if (hi instanceof Atom) {
            String hn = ((Atom) hi).getName();
            if (!"inf".equals(hn) && !"infinite".equals(hn)) return -1;   // -> built-in raises type_error
            high = Long.MAX_VALUE;
        } else if (hi instanceof Number && ((Number) hi).isInteger() && ((Number) hi).fitsInLong()) {
            high = ((Number) hi).longValue();
        } else {
            return -1;
        }
        if (low > high) return 0;
        final Term value = v;
        final Goal cont = goalStack;
        final long last = high;
        final long[] next = {low};
        CP cp = new CP(self -> {
            if (next[0] > last) return EXHAUSTED;                   // generator spent
            long i = next[0]++;
            if (next[0] > last) self.genExhausted = true;           // ISS-2025-0433: last value
            return unify(value, Number.valueOf(i)) ? cont : FAILED;   // ISS-2025-0434
        }, mark());
        cps.add(cp);
        if (advance(cp)) return 1;
        cps.remove(cps.size() - 1);
        return 0;
    }
    // END_CHANGE: ISS-2025-0432

    // START_CHANGE: ISS-2025-0425 - ENG-03: length/2 enumeration for partial lists.
    private static final Atom DOT = new Atom(".");
    private static final Atom NIL = new Atom("[]");
    private int lenVarCounter = 0;

    /**
     * {@code length(PartialList, Var)} — enumerate the list length. Returns false (leaving the goal
     * to the deterministic Java built-in) unless the length is unbound AND the list's spine ends in
     * an unbound tail; in that mode it installs a lazy, infinite choice point binding
     * {@code Tail = []}, {@code [_]}, {@code [_,_]}, … and the length accordingly.
     */
    private boolean lengthEnumerate(Term goal, List<Term> a) {
        Term lenT = deref(a.get(1));
        if (!(lenT instanceof Variable)) return false;            // length known -> deterministic mode
        Term cur = deref(a.get(0));
        int prefix = 0;
        java.util.IdentityHashMap<Term, Boolean> seen = new java.util.IdentityHashMap<>();
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            if (seen.put(cur, Boolean.TRUE) != null) return false;         // cyclic spine
            prefix++;
            cur = deref(((CompoundTerm) cur).getArguments().get(1));
        }
        if (!(cur instanceof Variable)) return false;             // proper list or non-list tail
        if (cur == lenT || ((Variable) cur).getName().equals(((Variable) lenT).getName())) return false;
        final Term tail = cur, lenVar = lenT;
        final int base = prefix;
        final Goal cont = goalStack;
        final boolean traced = debugTraceActive();
        final int depth = cps.size();
        if (traced) portCall(goal, depth);
        final Goal after = traced ? new Goal(() -> portExit(goal, depth), cont) : cont;
        final int[] extra = {0};
        CP cp = new CP(unused -> {
            int k = extra[0]++;
            Term list = NIL;
            for (int i = k - 1; i >= 0; i--) {
                list = new CompoundTerm(DOT, Arrays.asList(
                    (Term) new Variable("_Len" + (lenVarCounter++)), list));
            }
            if (!unify(tail, list)) return FAILED;
            if (!unify(lenVar, Number.valueOf(base + k))) return FAILED;   // ISS-2025-0434
            return after;
        }, mark());
        if (traced) { cp.traceGoal = goal; cp.traceDepth = depth; cp.traceDebug = (debugController != null); }
        cps.add(cp);
        advance(cp);                                              // an infinite generator never fails
        return true;
    }
    // END_CHANGE: ISS-2025-0425

    /** Deterministic builtins: 1 = succeeded, 0 = failed, -1 = not a builtin (try user clauses). */
    private int solveBuiltin(Term t, String f, List<Term> a) {
        int n = a.size();
        if (n == 2) {
            switch (f) {
                case "is":   return unify(a.get(0), evalNum(a.get(1))) ? 1 : 0;
                case "<": case ">": case "=<": case ">=": case "=:=": case "=\\=":
                    return numRel(f, evalNum(a.get(0)), evalNum(a.get(1))) ? 1 : 0;
                case "==":   return structuralEqual(resolve(a.get(0)), resolve(a.get(1))) ? 1 : 0;
                case "\\==": return structuralEqual(resolve(a.get(0)), resolve(a.get(1))) ? 0 : 1;
                // ISS-2025-0429 - ENG-10: an explicit mark/undo extent must trail unconditionally
                case "\\=": {
                    int m = mark(); forceTrail++;
                    boolean u;
                    try { u = unify(a.get(0), a.get(1)); } finally { forceTrail--; }
                    undo(m); return u ? 0 : 1;
                }
                default: return -1;
            }
        }
        if (n == 1) {
            Term x = deref(a.get(0));
            switch (f) {
                case "var":      return x instanceof Variable ? 1 : 0;
                case "nonvar":   return x instanceof Variable ? 0 : 1;
                case "atom":     return x instanceof Atom ? 1 : 0;
                // START_CHANGE: ISS-2025-0348 - strings are atomic
                case "atomic":   return (x instanceof Atom || x instanceof Number || x instanceof PrologString) ? 1 : 0;
                // END_CHANGE: ISS-2025-0348
                case "number":   return x instanceof Number ? 1 : 0;
                case "integer":  return (x instanceof Number && ((Number) x).isInteger()) ? 1 : 0;
                case "float":    return (x instanceof Number && !((Number) x).isInteger()) ? 1 : 0;
                case "compound": return x instanceof CompoundTerm ? 1 : 0;
                case "callable": return (x instanceof Atom || x instanceof CompoundTerm) ? 1 : 0;
                default: return -1;
            }
        }
        return -1;
    }

    // START_CHANGE: ISS-2025-0434 - ENG-14: evaluate against the binding store directly. evalNum
    // used to deep-copy the expression with resolve() (allocating a spine walk plus a CompoundTerm
    // per node) and then hand ArithEvaluator an empty HashMap — which STILL called
    // resolveBindings() at every node, walking each sub-term once per level. Now a one-level deref
    // hook is passed instead: no copy, no map, O(1) per node.
    private final java.util.function.UnaryOperator<Term> derefFn = this::deref;

    private Number evalNum(Term t) {
        return it.denzosoft.jprolog.core.arith.v2.ArithEvaluator.evalDeref(t, derefFn);
    }
    // END_CHANGE: ISS-2025-0434

    /** ISO arithmetic comparison with IEEE float semantics (-0.0 =:= 0.0, NaN =\= NaN). Integers
     *  compare exactly via BigInteger; otherwise primitive double comparison. */
    private boolean numRel(String op, Number a, Number b) {
        if (a.isInteger() && b.isInteger()) {
            // START_CHANGE: ISS-2025-0434 - ENG-14: compare small integers as primitives.
            // bigIntegerValue() allocates a BigInteger per operand on every comparison, and the
            // arithmetic comparisons are among the hottest goals in any Prolog program.
            if (a.fitsInLong() && b.fitsInLong()) {
                long x = a.longValue(), y = b.longValue();
                switch (op) {
                    case "<": return x < y;   case ">": return x > y;   case "=<": return x <= y;
                    case ">=": return x >= y; case "=:=": return x == y; case "=\\=": return x != y;
                }
            }
            // END_CHANGE: ISS-2025-0434
            int c = a.bigIntegerValue().compareTo(b.bigIntegerValue());
            switch (op) {
                case "<": return c < 0;  case ">": return c > 0;  case "=<": return c <= 0;
                case ">=": return c >= 0; case "=:=": return c == 0; case "=\\=": return c != 0;
            }
        }
        double x = a.doubleValue(), y = b.doubleValue();
        switch (op) {
            case "<": return x < y;  case ">": return x > y;  case "=<": return x <= y;
            case ">=": return x >= y; case "=:=": return x == y; case "=\\=": return x != y;
        }
        return false;
    }

    private boolean structuralEqual(Term a, Term b) {
        // ISS-2025-0434 - ENG-14: `L == L` on a big list is the common case and resolve() is now
        // structure-sharing, so both sides are literally the same object: answer in O(1).
        if (a == b) return true;
        if (a instanceof Variable && b instanceof Variable) return ((Variable) a).getName().equals(((Variable) b).getName());
        if (a instanceof Atom && b instanceof Atom) return ((Atom) a).getName().equals(((Atom) b).getName());
        if (a instanceof Number && b instanceof Number) return a.equals(b);
        // START_CHANGE: ISS-2025-0348 - strings are identical iff their content matches
        if (a instanceof PrologString && b instanceof PrologString) {
            return ((PrologString) a).getStringValue().equals(((PrologString) b).getStringValue());
        }
        // END_CHANGE: ISS-2025-0348
        // START_CHANGE: ISS-2025-0428 - ENG-09: iterative on the last argument
        while (a instanceof CompoundTerm && b instanceof CompoundTerm) {
            CompoundTerm ca = (CompoundTerm) a, cb = (CompoundTerm) b;
            List<Term> aa = ca.getArguments(), ba = cb.getArguments();
            if (!ca.getName().equals(cb.getName()) || aa.size() != ba.size()) return false;
            int n = aa.size();
            for (int i = 0; i < n - 1; i++) {
                if (!structuralEqual(aa.get(i), ba.get(i))) return false;
            }
            if (n == 0) return true;
            a = aa.get(n - 1);
            b = ba.get(n - 1);
        }
        return (a instanceof CompoundTerm || b instanceof CompoundTerm) ? false : structuralEqualLeaf(a, b);
        // END_CHANGE: ISS-2025-0428
    }

    // START_CHANGE: ISS-2025-0428 - ENG-09: leaf comparison, extracted so the spine loop can call it
    private boolean structuralEqualLeaf(Term a, Term b) {
        if (a instanceof Variable && b instanceof Variable) return ((Variable) a).getName().equals(((Variable) b).getName());
        if (a instanceof Atom && b instanceof Atom) return ((Atom) a).getName().equals(((Atom) b).getName());
        if (a instanceof Number && b instanceof Number) return a.equals(b);
        if (a instanceof PrologString && b instanceof PrologString) {
            return ((PrologString) a).getStringValue().equals(((PrologString) b).getStringValue());
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0428

    /**
     * Delegate a non-native goal to the existing {@link BuiltInRegistry} (reusing the 200+ builtin
     * implementations). Deterministic builtins yield one solution; nondeterministic ones (e.g.
     * {@code between/3}) yield several → a choice point. Returns 1 success / 0 fail / -1 not-a-builtin.
     * Context-dependent builtins (findall/catch/…) throw without a solver and fall through (-1) for
     * now — they will be handled natively by the machine in a later step.
     */
    // START_CHANGE: ISS-2025-0430 - ENG-11: built-ins that mutate a bound term in place and therefore
    // need the ORIGINAL term objects, not a dereferenced copy (ISS-2025-0317).
    private static final java.util.Set<String> IDENTITY_BUILTINS =
        new java.util.HashSet<>(Arrays.asList("setarg", "nb_setarg"));
    // END_CHANGE: ISS-2025-0430

    private int bridgeBuiltin(Term goal, String functor, int arity) {
        if (registry == null || !registry.isBuiltIn(functor, arity)) return -1;
        BuiltIn b = registry.getBuiltIn(functor);
        if (b == null) return -1;
        // ISS-2025-0332: trace/debug builtins too. CALL before executing; EXIT (via a continuation
        // marker, for correct Redo/Exit ordering) per solution; FAIL when it yields nothing; the choice
        // point carries the goal so backtrack emits Redo/Fail for nondeterministic builtins.
        final int dd = debugTraceActive() ? cps.size() : -1;
        if (dd >= 0) portCall(goal, dd);
        // START_CHANGE: ISS-2025-0430 - ENG-11: hand the built-in a RESOLVED goal and an EMPTY map.
        //
        // The old contract passed the goal unresolved together with `new HashMap<>(binding)` — a full
        // copy of every binding in the query, per built-in call. Each built-in then returned solution
        // maps that were copies of that copy (Member copies twice per element), applySolution walked
        // the whole returned map, and the exhausted choice point retained every copy: memory and time
        // were Sigma(bindings at call i) = O(N^2). One `atom_length(abc,_)` per iteration was enough to
        // exhaust a 2 GB heap at N = 10 000.
        //
        // A resolved goal carries all the information the built-in needs (resolve() is now
        // structure-sharing, so unchanged sub-terms are not copied) and every variable still in it is
        // unbound — so an empty map is a faithful view and the built-in returns only the bindings it
        // creates. Variable OBJECTS and names survive resolve(), so applySolution installs them into
        // the real store unchanged.
        //
        // Exception (ISS-2025-0317): the destructive built-ins need object identity to mutate the
        // actual bound term rather than a dereferenced copy, so they keep the old handoff.
        final boolean needsIdentity = IDENTITY_BUILTINS.contains(functor);
        final Term callGoal = needsIdentity ? goal : resolve(goal);
        Map<String, Term> inMap = needsIdentity ? new HashMap<>(binding) : new HashMap<>();
        // END_CHANGE: ISS-2025-0430
        List<Map<String, Term>> sols = new ArrayList<>();
        boolean ok;
        try {
            // BuiltInWithContext builtins (findall-adapter, setup_call_cleanup, predsort, format, ...)
            // need a solver to run their sub-goals; hand them the engine's solver (ISS-2025-0312).
            if (b instanceof BuiltInWithContext && contextSolver != null) {
                ok = ((BuiltInWithContext) b).executeWithContext(contextSolver, callGoal, inMap, sols);
            } else {
                ok = b.execute(callGoal, inMap, sols);
            }
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            throw pe;    // ISS-2025-0309: a real ISO error must reach catch/3, not be swallowed
        // START_CHANGE: ISS-2025-0426 - ENG-05: `catch (RuntimeException e) { return -1; }` turned
        // EVERY Java failure inside a built-in (NPE, ClassCastException, IndexOutOfBounds, ...) into
        // "not a built-in", which then fell through to callUser -> existence_error or silent failure.
        // Worse, it swallowed the three engine-control exceptions raised inside a nested sub-solve.
        // Policy now: (a) the control exceptions propagate untouched — the trust model requires that
        // untrusted catch/3 cannot trap them; (b) only an explicit NeedsSolverContextException means
        // "not bridgeable"; (c) anything else becomes a catchable system_error naming the culprit.
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException
               | it.denzosoft.jprolog.core.engine.QueryCancelledException
               | it.denzosoft.jprolog.core.engine.DebugController.DebugStopException control) {
            throw control;
        } catch (it.denzosoft.jprolog.core.engine.NeedsSolverContextException nsc) {
            return -1;   // genuinely not bridgeable here -> let the caller try user clauses
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            if (dd >= 0) portFail(goal, dd);
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.systemError(
                    e.getClass().getSimpleName() + (e.getMessage() == null ? "" : ": " + e.getMessage()),
                    functor + "/" + arity));
        // END_CHANGE: ISS-2025-0426
        }
        if (!ok || sols.isEmpty()) { if (dd >= 0) portFail(goal, dd); return 0; }
        // START_CHANGE: ISS-2025-0430 - ENG-11: a DETERMINISTIC built-in (exactly one solution) gets
        // no choice point at all — no CP object, no Alt closure, nothing for backtracking to walk
        // past. Its bindings are undone by the enclosing choice point's trail mark, exactly as
        // before. While tracing the choice-point path is kept so Redo/Fail ports still fire.
        if (sols.size() == 1 && dd < 0) {
            applySolution(sols.get(0));
            return 1;
        }
        // END_CHANGE: ISS-2025-0430
        final Goal cont = goalStack;
        final int fdd = dd;
        List<Alt> alts = new ArrayList<>(sols.size());
        for (Map<String, Term> sol : sols) {
            final Map<String, Term> fsol = sol;
            alts.add(() -> {
                applySolution(fsol);
                // EXIT fires when the continuation runs (after any backtrack Redo) -> correct ordering.
                return (fdd >= 0) ? new Goal(() -> portExit(goal, fdd), cont) : cont;
            });
        }
        CP cp = new CP(alts, mark());
        if (dd >= 0) { cp.traceGoal = goal; cp.traceDepth = dd; cp.traceDebug = (debugController != null); }
        cps.add(cp);
        if (advance(cp)) return 1;
        cps.remove(cps.size() - 1);
        if (dd >= 0) portFail(goal, dd);
        return 0;
    }

    /** findall/3: collect a (renamed-apart) copy of Template for every solution of Goal. */
    private List<Term> findAll(Term template, Term goal) {
        Goal savedGoals = goalStack;
        int floor = cps.size();
        int m = mark();
        List<Term> results = new ArrayList<>();
        goalStack = new Goal(goal, floor, null);
        forceTrail++;                                              // ISS-2025-0429 - ENG-10
        try {
            drive(() -> { results.add(rename(resolve(template), renameCounter++, new HashMap<>())); return true; }, floor);
        } finally {
            forceTrail--;
            // restore even if a ball unwinds through the nested drive (ISS-2025-0308)
            undo(m);                                               // findall is opaque: discard Goal's bindings
            goalStack = savedGoals;
        }
        return results;
    }

    /** Handle a thrown ball: unwind choice points down to {@code floor} looking for a catch frame
     *  whose catcher unifies with the ball; if found, install its recovery and return true. If none
     *  is found within this run's floor, return false — drive() re-throws so an OUTER drive (lower
     *  floor) gets a chance, and only an uncaught ball at floor 0 escapes as a real exception. This
     *  is what makes throw/1 transparent across findall/3's nested drive (ISS-2025-0308). */
    private boolean handleBall(Term ball, int floor) {
        while (cps.size() > floor) {
            CP top = cps.remove(cps.size() - 1);
            if (top.isCatch) {
                // START_CHANGE: ISS-2025-0343 - a disarmed frame (its Goal already exited) is no
                // catcher candidate: pop it like a plain choice point and keep unwinding.
                if (!top.active) continue;
                // END_CHANGE: ISS-2025-0343
                undo(top.trailMark);
                it.denzosoft.jprolog.core.engine.Trail.rollbackTo(top.legacyMark);   // ISS-0316
                int m = mark();
                forceTrail++;                                     // ISS-2025-0429 - ENG-10
                boolean matched;
                try { matched = unify(top.catcher, ball); } finally { forceTrail--; }
                if (matched) {
                    goalStack = new Goal(top.recovery, top.cutBarrier, top.cont);
                    return true;
                }
                undo(m);                                          // catcher didn't match; keep unwinding
            }
        }
        return false;
    }

    private static Term makeList(List<Term> elems) {
        Term list = new Atom("[]");
        for (int i = elems.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), Arrays.asList(elems.get(i), list));
        }
        return list;
    }

    // ----------------------------------------------------------------- database (assert/retract)

    // START_CHANGE: ISS-2025-0366 - shared ISO validation for assert/retract Clause arguments
    // (8.9.1.3/8.9.2.3/8.9.3.3): an unbound Clause or head -> instantiation_error; a head that is
    // not callable (number, string) -> type_error(callable, Head). Previously retract(X)/retract(1)
    // reached clausesFor()'s unchecked (CompoundTerm) cast and the raw ClassCastException escaped
    // catch/3 entirely. Returns the dereferenced head for further checks.
    private Term checkClauseArgument(Term clause, String context, boolean checkBody) {
        Term q = deref(clause);
        if (q instanceof Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(context));
        }
        Term head = q;
        if (q instanceof CompoundTerm && ":-".equals(((CompoundTerm) q).getName())
                && ((CompoundTerm) q).getArguments().size() == 2) {
            head = deref(((CompoundTerm) q).getArguments().get(0));
            if (head instanceof Variable) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(context));
            }
            if (checkBody) checkBodyGoals(((CompoundTerm) q).getArguments().get(1), context);
        }
        if (!(head instanceof Atom) && !(head instanceof CompoundTerm)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("callable", head, context));
        }
        return head;
    }
    // END_CHANGE: ISS-2025-0366

    // START_CHANGE: ISS-2025-0368 - walk a clause body through ','/2, ';'/2 and '->'/2: a number or
    // string in goal position raises type_error(callable, G) at assert time (ISO 7.6.2); an unbound
    // goal is legal (converted to call/1 at call time), as is any atom/compound.
    private void checkBodyGoals(Term body, String context) {
        Term b = deref(body);
        // START_CHANGE: ISS-2025-0428 - ENG-09: a long right-nested conjunction (maplist expansion,
        // generated clauses) is a last-argument spine; iterate on it, recurse only on the left.
        while (b instanceof CompoundTerm && ((CompoundTerm) b).getArguments().size() == 2) {
            String f = ((CompoundTerm) b).getName();
            if (!",".equals(f) && !";".equals(f) && !"->".equals(f)) break;
            checkBodyGoals(((CompoundTerm) b).getArguments().get(0), context);
            b = deref(((CompoundTerm) b).getArguments().get(1));
        }
        // END_CHANGE: ISS-2025-0428
        if (b instanceof Number || b instanceof PrologString) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("callable", b, context));
        }
    }
    // END_CHANGE: ISS-2025-0368

    // START_CHANGE: ISS-2025-0367 - ISO 8.9.1.3/8.9.2.3/8.9.3.3: assert/retract on a procedure the
    // BuiltInRegistry claims as a built-in (a static procedure) raises
    // permission_error(modify, static_procedure, Name/Arity) instead of silently corrupting it.
    private void checkModifiable(Term head, String context) {
        if (registry == null) return;
        String f; int ar;
        if (head instanceof Atom) { f = ((Atom) head).getName(); ar = 0; }
        else if (head instanceof CompoundTerm) {
            f = ((CompoundTerm) head).getName(); ar = ((CompoundTerm) head).getArguments().size();
        } else return;
        if (registry.isBuiltIn(f, ar)) {
            Term pi = new CompoundTerm(new Atom("/"), Arrays.asList(new Atom(f), new Number((long) ar)));
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.permissionError(
                    "modify", "static_procedure", pi, context));
        }
    }
    // END_CHANGE: ISS-2025-0367

    /** assert a (copied) clause at the front (asserta) or back (assertz) of its predicate. */
    private void assertClause(Term clause, boolean front) {
        // START_CHANGE: ISS-2025-0368 - ISO 8.9.1.3/8.9.2.3: validate Clause before storing it
        // (previously assertz(X), assertz(1), assertz((1:-true)), assertz((foo:-7)) all succeeded,
        // inserting garbage rules keyed "unknown/0" into the KB).
        Term checkedHead = checkClauseArgument(clause, front ? "asserta/1" : "assertz/1", true);
        // END_CHANGE: ISS-2025-0368
        // START_CHANGE: ISS-2025-0367 - built-in procedures are static: refuse to modify them
        checkModifiable(checkedHead, front ? "asserta/1" : "assertz/1");
        // END_CHANGE: ISS-2025-0367
        Rule r = toRule(rename(resolve(clause), renameCounter++, new HashMap<>()));   // copy_term
        if (liveKb != null) {
            // START_CHANGE: ISS-2025-0347 - assert implies the procedure is dynamic (ISO 8.9.1),
            // so it keeps failing (not existence_error) after being retracted to empty.
            Term h = r.getHead();
            if (h instanceof Atom) liveKb.markDynamic(((Atom) h).getName(), 0);
            else if (h instanceof CompoundTerm) liveKb.markDynamic(((CompoundTerm) h).getName(),
                ((CompoundTerm) h).getArguments().size());
            // END_CHANGE: ISS-2025-0347
            if (front) liveKb.asserta(r); else liveKb.addRule(r);
        } else {
            List<Rule> list = kb.computeIfAbsent(key(r.getHead()), k -> new ArrayList<>());
            if (front) list.add(0, r); else list.add(r);
        }
    }

    // START_CHANGE: ISS-2025-0396 - retract/1 is RE-EXECUTABLE (ISO 8.9.3, resolves LIM-026): a
    // choice point over a snapshot of the matching clauses (logical update view) replaces the old
    // "first-match (semi-det)" scan, so on redo the next matching clause is retracted — making
    // findall(X, retract(p(X)), L) drain the predicate and `(retract(c(X)), fail ; true)` purge
    // every clause. Retractions of earlier solutions persist across backtracking (the removal is a
    // side effect, deliberately NOT trailed); a snapshot clause already removed by an intervening
    // retract is skipped (identity check) rather than retracted twice.
    /** retract one clause that unifies with {@code clause} per solution; nondeterministic. Both the
     *  query and each stored clause are normalised to (Head :- Body) form, so a fact retracts via
     *  either {@code retract(Head)} or {@code retract((Head :- true))} (ISS-2025-0310). Returns
     *  false when no clause matches (the caller backtracks). */
    private boolean retractClause(Term clause) {
        Term q = deref(clause);
        // START_CHANGE: ISS-2025-0366 - ISO 8.9.3.3: retract(X) -> instantiation_error and
        // retract(1) -> type_error(callable, 1) as catchable Prolog errors (previously a raw
        // ClassCastException escaped catch/3).
        Term checkedHead = checkClauseArgument(q, "retract/1", false);
        // END_CHANGE: ISS-2025-0366
        // START_CHANGE: ISS-2025-0367 - built-in procedures are static: refuse to modify them
        checkModifiable(checkedHead, "retract/1");
        // END_CHANGE: ISS-2025-0367
        Term head;
        Term queryClause;
        if (q instanceof CompoundTerm && ":-".equals(((CompoundTerm) q).getName())
                && ((CompoundTerm) q).getArguments().size() == 2) {
            head = ((CompoundTerm) q).getArguments().get(0);
            queryClause = q;
        } else {
            head = q;
            queryClause = new CompoundTerm(new Atom(":-"), Arrays.asList(q, new Atom("true")));
        }
        final String protoKey = key(deref(head));                   // for prototype-mode removal
        List<Rule> list = (liveKb != null) ? clausesFor(deref(head)) : kb.get(protoKey);
        if (list == null || list.isEmpty()) return false;
        final List<Rule> candidates = new ArrayList<>(list);        // snapshot: logical update view
        final Goal cont = goalStack;
        final Term qc = queryClause;
        List<Alt> alts = new ArrayList<>(candidates.size());
        for (Rule rule : candidates) {
            final Rule fr = rule;
            alts.add(() -> {
                Term stored = makeClauseTerm(renameRule(fr));       // always (Head :- Body); facts -> (Head :- true)
                if (!unify(stored, qc)) return FAILED;
                if (!removeRetracted(fr, protoKey)) return FAILED;  // already gone (retracted meanwhile)
                return cont;
            });
        }
        CP cp = new CP(alts, mark());
        cps.add(cp);
        if (advance(cp)) return true;
        cps.remove(cps.size() - 1);
        return false;
    }

    /** Remove the retracted clause from the live database; false when it is no longer present. */
    private boolean removeRetracted(Rule rule, String protoKey) {
        if (liveKb != null) return liveKb.retract(rule);
        List<Rule> live = kb.get(protoKey);
        if (live == null) return false;
        for (int i = 0; i < live.size(); i++) {
            if (live.get(i) == rule) { live.remove(i); return true; }
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0396

    private Rule toRule(Term c) {
        if (c instanceof CompoundTerm && ":-".equals(((CompoundTerm) c).getName())
                && ((CompoundTerm) c).getArguments().size() == 2) {
            CompoundTerm cc = (CompoundTerm) c;
            return new Rule(cc.getArguments().get(0), flattenBody(cc.getArguments().get(1)));
        }
        return new Rule(c, new ArrayList<>());
    }

    private List<Term> flattenBody(Term body) {
        List<Term> gs = new ArrayList<>();
        Term cur = body;
        while (cur instanceof CompoundTerm && ",".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            gs.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        gs.add(cur);
        return gs;
    }

    private Term makeClauseTerm(Rule r) {
        List<Term> gs = r.getBody();
        Term body = new Atom("true");
        if (!gs.isEmpty()) {
            body = gs.get(gs.size() - 1);
            for (int i = gs.size() - 2; i >= 0; i--) body = new CompoundTerm(new Atom(","), Arrays.asList(gs.get(i), body));
        }
        return new CompoundTerm(new Atom(":-"), Arrays.asList(r.getHead(), body));
    }

    /** Install a builtin solution map: bind every variable the builtin introduced (on the trail). */
    private void applySolution(Map<String, Term> sol) {
        for (Map.Entry<String, Term> e : sol.entrySet()) {
            if (!binding.containsKey(e.getKey())) {
                // START_CHANGE: ISS-2025-0397 - skip identity var-var entries. Legacy-solver
                // solution maps can contain a self-binding (e.g. {R=R, T=R} from a var-var union
                // in phrase/3); blindly installing it creates a deref cycle R -> R that resolve()
                // then mis-reports as representation_error(cyclic_term) — or that deref() loops
                // on, depending on map order. Binding a variable to itself is a no-op, so the
                // entry is skipped whenever the value ultimately dereferences back to the key
                // variable (the exact condition under which this bind would create a var cycle).
                // Real cyclic-term protection for rational trees through compounds (ISS-2025-0313)
                // is untouched: a compound value is never skipped.
                Term v = deref(e.getValue());
                if (v instanceof Variable && ((Variable) v).getName().equals(e.getKey())) continue;
                // END_CHANGE: ISS-2025-0397
                bind(e.getKey(), e.getValue());
            }
        }
    }

    private Term addArgs(Term goal, List<Term> extra) {
        if (goal instanceof Atom) return new CompoundTerm((Atom) goal, new ArrayList<>(extra));
        if (goal instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) goal;
            List<Term> args = new ArrayList<>(c.getArguments());
            args.addAll(extra);
            return new CompoundTerm(c.getFunctor(), args);   // ISS-2025-0429 - ENG-10: reuse the Atom
        }
        return goal;
    }

    /** Resolve and run a user predicate. {@code lookup} is used to find clauses (module-aware: it may
     *  be a {@code Module:Goal} term); {@code unifyGoal} is the (unqualified) goal each clause head
     *  unifies with. For ordinary calls the two are identical. */
    private boolean callUser(Term unifyGoal, Term lookup) {
        // ISS-2025-0319: tabled predicates (:- table p/n) need variant tabling (memoization + loop
        // detection); the v2 iterative SLD has none, so the call goes to the driver below and its
        // answers are surfaced as a choice point.
        // ISS-2025-0484 - wave W9: `bypassTablingOnce` is how the driver's PRODUCE phase runs the
        // goal's own clauses without re-entering itself (see tabledAnswers/produceTabled).
        if (tableStore != null && isTabled(deref(unifyGoal))) {
            if (bypassTablingOnce) {
                bypassTablingOnce = false;
            } else {
                return tabledDelegate(deref(unifyGoal));
            }
        }
        // ISS-2025-0315: feed the profiler (zero overhead when disabled), like the legacy solver
        if (it.denzosoft.jprolog.core.engine.Profiler.isEnabled()) {
            Term gg = deref(unifyGoal);
            if (gg instanceof Atom) it.denzosoft.jprolog.core.engine.Profiler.recordCall(((Atom) gg).getName(), 0);
            else if (gg instanceof CompoundTerm) it.denzosoft.jprolog.core.engine.Profiler.recordCall(
                ((CompoundTerm) gg).getName(), ((CompoundTerm) gg).getArguments().size());
        }
        // ISS-2025-0329: four-port call tracing (trace/0 .. notrace/0). Emitted to the shared output so
        // it appears on the CLI stdout AND in the IDE Run console. User predicates only.
        final boolean tracing = it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled();
        final boolean debugging = debugController != null;
        final int tdepth = cps.size();
        if (tracing) tracePort("Call", unifyGoal, tdepth);
        if (debugging) debugPort(DebugEvent.Port.CALL, unifyGoal, tdepth);
        List<Rule> rules = clausesFor(lookup);
        if (rules == null || rules.isEmpty()) {
            // START_CHANGE: ISS-2025-0433 - ENG-13: with first-argument indexing an empty candidate
            // list usually means "this predicate has clauses, but none can match this first
            // argument" — a plain FAILURE. Only a predicate with NO clauses at all is unknown.
            if (predicateHasClauses(lookup)) {
                if (tracing) tracePort("Fail", unifyGoal, tdepth);
                if (debugging) debugPort(DebugEvent.Port.FAIL, unifyGoal, tdepth);
                return false;
            }
            // END_CHANGE: ISS-2025-0433
            // START_CHANGE: ISS-2025-0347 - unknown procedure: honour the 'unknown' flag (ISO 7.7.7
            // + 7.11.2.4): error -> existence_error(procedure, Name/Arity); warning -> warn + fail;
            // fail -> silent failure. Dynamic procedures (declared or implied by assert) just fail.
            raiseUnknownIfRequired(lookup);
            // END_CHANGE: ISS-2025-0347
            if (tracing) tracePort("Fail", unifyGoal, tdepth);
            if (debugging) debugPort(DebugEvent.Port.FAIL, unifyGoal, tdepth);
            return false;
        }
        final Goal cont = goalStack;
        final int barrier = cps.size();                               // this CP's index = cut target for the body
        final Term g = unifyGoal;
        final boolean ftrace = tracing, fdebug = debugging;
        // START_CHANGE: ISS-2025-0433 - ENG-13: a LAZY choice point over the clause snapshot.
        // The old code allocated one Alt closure per candidate clause BEFORE the first head
        // unification (20 000 lambdas for a 20 000-fact table) and renamed head AND body of every
        // candidate before even looking at the head. Now the frame holds (snapshot, index) and
        // pulls one clause per redo; renaming is head-first (the body is renamed only after the
        // head unifies, sharing the same variable map) and skipped entirely for a ground fact.
        final List<Rule> candidates = rules;
        final int candidateCount = candidates.size();
        final int[] nextClause = {0};
        CP cp = new CP(self -> {
            if (nextClause[0] >= candidateCount) return EXHAUSTED;
            Rule fr = candidates.get(nextClause[0]++);
            if (nextClause[0] >= candidateCount) self.genExhausted = true;
            int id;
            Map<String, Variable> vmap;
            Term head;
            if (fr.isGroundFact()) {          // no variables in the head: nothing to rename
                id = -1; vmap = null; head = fr.getHead();
            } else {
                id = renameCounter++;
                vmap = new HashMap<>();
                head = rename(fr.getHead(), id, vmap);
            }
            if (!unify(head, g)) return FAILED;
            Goal after = cont;
            if (ftrace || fdebug) {
                after = new Goal(() -> {
                    if (ftrace) tracePort("Exit", g, tdepth);
                    if (fdebug) debugPort(DebugEvent.Port.EXIT, g, tdepth);
                }, cont);
            }
            List<Term> body = fr.getBody();
            if (body.isEmpty()) return after;
            if (vmap == null) { id = renameCounter++; vmap = new HashMap<>(); }   // ground head, var body
            List<Term> renamedBody = new ArrayList<>(body.size());
            for (Term b : body) renamedBody.add(rename(b, id, vmap));
            return pushBody(renamedBody, barrier, after);
        }, mark());
        // END_CHANGE: ISS-2025-0433
        if (tracing || debugging) {                                   // for Redo/Fail on backtracking
            cp.traceGoal = g; cp.traceDepth = tdepth; cp.traceDebug = debugging;
        }
        cps.add(cp);
        if (advance(cp)) return true;
        cps.remove(cps.size() - 1);
        if (tracing) tracePort("Fail", g, tdepth);
        if (debugging) debugPort(DebugEvent.Port.FAIL, g, tdepth);
        return false;
    }

    // START_CHANGE: ISS-2025-0433 - ENG-13: does this predicate have ANY clause (ignoring the
    // first-argument filter)? Used only on the empty-candidate path, so the extra lookup is rare.
    private boolean predicateHasClauses(Term lookup) {
        if (liveKb == null) return false;
        Term g = deref(lookup);
        String f; int ar;
        if (g instanceof Atom) { f = ((Atom) g).getName(); ar = 0; }
        else if (g instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) g;
            if (":".equals(c.getName()) && c.getArguments().size() == 2) return false;   // Module:Goal
            f = c.getName(); ar = c.getArguments().size();
        } else {
            return false;
        }
        return !liveKb.getClauseSnapshot(f, ar).isEmpty();
    }
    // END_CHANGE: ISS-2025-0433

    // START_CHANGE: ISS-2025-0347 - existence_error(procedure, Name/Arity) for unknown procedures
    /** Apply the ISO {@code unknown} flag (7.7.7/7.11.2.4) to a call with no clauses: throw an
     *  existence_error (error), warn and fail (warning), or fail silently (fail). Procedures marked
     *  dynamic fail silently; module-qualified calls and module programs keep the established
     *  visibility-based failure semantics (ISS-2025-0314). */
    private void raiseUnknownIfRequired(Term lookup) {
        if (liveKb == null) return;                                   // prototype mode: flat local KB
        Term g = deref(lookup);
        String f; int ar;
        if (g instanceof Atom) { f = ((Atom) g).getName(); ar = 0; }
        else if (g instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) g;
            if (":".equals(c.getName()) && c.getArguments().size() == 2) return;   // Module:Goal
            f = c.getName(); ar = c.getArguments().size();
        } else {
            return;
        }
        if (modules != null && modules.getAllModuleNames().size() > 1) return;     // module program
        if (liveKb.isDynamic(f, ar)) return;
        Term mode = it.denzosoft.jprolog.core.system.PrologFlags.getFlag("unknown");
        String m = (mode instanceof Atom) ? ((Atom) mode).getName() : "error";
        if ("fail".equals(m)) return;
        Term pi = new CompoundTerm(new Atom("/"), Arrays.asList(new Atom(f), new Number((long) ar)   /* ISS-2025-0424 */));
        if ("warning".equals(m)) {
            // START_CHANGE: ISS-2025-0427 - ENG-08: write through the thread-local StreamManager
            // (output discipline) so the IDE Run console and captured output see the warning;
            // System.err bypassed both.
            it.denzosoft.jprolog.builtin.io.StreamManager.out()
                .println("Warning: unknown procedure " + f + "/" + ar);
            // END_CHANGE: ISS-2025-0427
            return;
        }
        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.existenceError("procedure", pi, f + "/" + ar));
    }
    // END_CHANGE: ISS-2025-0347

    /** Fire a four-port event to the IDE debugger (ISS-2025-0331). {@code notifyPort} handles call-stack
     *  bookkeeping, breakpoint/step decisions and the two-thread pause; a {@code DebugStopException}
     *  (Stop pressed) propagates out of the drive loop to abort. Variables are snapshotted at this point. */
    private void debugPort(DebugEvent.Port port, Term goal, int depth) {
        if (debugController == null) return;
        Term g;
        try { g = resolve(goal); } catch (RuntimeException e) { g = goal; }
        debugController.notifyPort(port, g, new HashMap<>(binding), depth);
    }

    /** True when either the trace flag or the IDE debugger wants four-port notifications. */
    private boolean debugTraceActive() {
        return debugController != null || it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled();
    }
    // Combined trace+debug port emitters (each underlying call self-guards). (ISS-2025-0332)
    private void portCall(Term g, int d) { tracePort("Call", g, d); debugPort(DebugEvent.Port.CALL, g, d); }
    private void portExit(Term g, int d) { tracePort("Exit", g, d); debugPort(DebugEvent.Port.EXIT, g, d); }
    private void portFail(Term g, int d) { tracePort("Fail", g, d); debugPort(DebugEvent.Port.FAIL, g, d); }

    /** Emit one four-port trace line to the shared output stream (when trace/0 is active). */
    private void tracePort(String port, Term goal, int depth) {
        if (!it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled()) return;
        try {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < depth; i++) sb.append("  ");
            String g = it.denzosoft.jprolog.core.util.TermFormatter.format(resolve(goal), false, false, false, 1200);
            it.denzosoft.jprolog.builtin.io.StreamManager.out().println(sb + port + ": (" + depth + ") " + g);
        } catch (RuntimeException ignored) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(ignored);   // ISS-2025-0431
            /* tracing must never break resolution */ }
    }

    private boolean isTabled(Term goal) {
        if (goal instanceof Atom) return tableStore.isTabled(((Atom) goal).getName(), 0);
        if (goal instanceof CompoundTerm) return tableStore.isTabled(((CompoundTerm) goal).getName(), ((CompoundTerm) goal).getArguments().size());
        return false;
    }

    // START_CHANGE: ISS-2025-0484 - wave W9: the recursive solver is gone, and with it
    // `solveWithTabling`, the variant-tabling driver this method used to delegate to
    // (`contextSolver.solve(...)` reached it through the top-level recursive solve). The driver is
    // ported here, unchanged in behaviour: variant normalisation over the resolved goal, a memo
    // cache, the in-progress partial cache that makes LEFT RECURSION terminate, and the bounded
    // fixpoint iteration. The PRODUCE phase runs the goal on a nested MachineSolver with the
    // tabling interception suppressed for its own first tabled call - the exact analogue of the
    // recursive driver calling `solveAgainstKnowledgeBase` for this call while nested calls to the
    // same variant still went through `solveInternal` and hit the partial cache.
    //
    // The v4 engine does NOT use any of this: it has real linear tabling with completion
    // (core.engine.v4.Tabling, wave W5) and no iteration cap.
    // START_CHANGE: ISS-2025-0488 - LIM-039: production is serialised on the TableStore, so two
    // worker threads producing on one engine can no longer corrupt the shared in-progress map.

    /** True while THIS machine must skip the tabling interception for its first tabled call. */
    private boolean bypassTablingOnce = false;

    /** Run a tabled call: memo cache, loop detection, fixpoint production; expose the answers as a
     *  choice point. */
    private boolean tabledDelegate(Term goal) {
        List<Map<String, Term>> sols;
        try {
            // ISS-2025-0488 (LIM-039): one thread at a time evaluates on this store; a second
            // waits and then reads the completed table.
            tableStore.enterCall();
            try {
                sols = tabledAnswers(resolve(goal));   // ground the known args; sets up tabling
            } finally {
                tableStore.exitCall();
            }
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return false;
        }
        if (sols.isEmpty()) return false;
        final Goal cont = goalStack;
        List<Alt> alts = new ArrayList<>(sols.size());
        for (Map<String, Term> sol : sols) {
            final Map<String, Term> fsol = sol;
            alts.add(() -> { applySolution(fsol); return cont; });
        }
        CP cp = new CP(alts, mark());
        cps.add(cp);
        if (advance(cp)) return true;
        cps.remove(cps.size() - 1);
        return false;
    }

    /** The answers of one tabled variant, computing them (with a fixpoint) if they are not cached. */
    private List<Map<String, Term>> tabledAnswers(Term resolvedGoal) {
        it.denzosoft.jprolog.core.engine.TableStore.NormalizedGoal norm = tableStore.normalize(resolvedGoal);
        String key = norm.cacheKey;
        synchronized (tableStore) {
            List<Map<String, Term>> cached = tableStore.getCachedSolutions(key);
            if (cached != null) return replayTabled(cached, norm);
            if (tableStore.isInProgress(key)) {
                // A variant already being produced (left recursion): hand back what it has so far.
                List<Map<String, Term>> partial = tableStore.getPartialCache(key);
                return (partial == null) ? new ArrayList<Map<String, Term>>() : replayTabled(partial, norm);
            }
            tableStore.markInProgress(key);
            tableStore.setPartialCache(key, new ArrayList<Map<String, Term>>());
        }
        try {
            List<Map<String, Term>> aggregated = new ArrayList<>();
            java.util.Set<String> seen = new java.util.HashSet<>();
            final int maxIters = 100;
            for (int iter = 0; iter < maxIters; iter++) {
                // Produce against the NORMALISED pattern, whose variables are _TV0, _TV1, ...
                // Two reasons: the answers come back already canonical (no name mapping needed to
                // record them), and — the bug this fixes — the caller's variable names may collide
                // with the fresh machine's clause-renaming scheme (`_R1_Z`), because each machine
                // restarts that counter. Producing `path(a, _R1_Z)` on a machine that renames the
                // recursive body call's Z to `_R1_Z` too would bind the goal's own variable.
                List<Map<String, Term>> computed = produceTabled(norm.pattern);
                boolean changed = false;
                for (Map<String, Term> sol : computed) {
                    if (seen.add(canonicalKey(sol))) { aggregated.add(sol); changed = true; }
                }
                synchronized (tableStore) {
                    tableStore.setPartialCache(key, new ArrayList<>(aggregated));
                }
                if (!changed) break;
            }
            synchronized (tableStore) { tableStore.cacheSolutions(key, aggregated); }
            return replayTabled(aggregated, norm);
        } finally {
            synchronized (tableStore) {
                tableStore.clearPartialCache(key);
                tableStore.unmarkInProgress(key);
            }
        }
    }

    /** Order-independent identity of one canonical answer (a HashMap's toString is not stable). */
    private static String canonicalKey(Map<String, Term> sol) {
        return new java.util.TreeMap<>(sol).toString();
    }

    /** One PRODUCE pass: the pattern's own clauses, with this variant's tabling interception off. */
    private List<Map<String, Term>> produceTabled(Term pattern) {
        MachineSolver m = new MachineSolver(liveKb, registry, contextSolver, modules, tableStore);
        m.setResourceGuard(guard);
        m.bypassTablingOnce = true;
        final List<Map<String, Term>> out = new ArrayList<>();
        m.solve(pattern, sol -> { out.add(sol); return true; });
        return out;
    }

    /** Map canonically-named answers (_TV0, ...) onto the calling goal's variable names. */
    private List<Map<String, Term>> replayTabled(
            List<Map<String, Term>> canonical,
            it.denzosoft.jprolog.core.engine.TableStore.NormalizedGoal norm) {
        List<Map<String, Term>> out = new ArrayList<>(canonical.size());
        for (Map<String, Term> sol : canonical) {
            Map<String, Term> replayed = new HashMap<>();
            for (Map.Entry<String, Term> e : sol.entrySet()) {
                String orig = norm.canonicalToOrig.get(e.getKey());
                if (orig != null) replayed.put(orig, e.getValue());
            }
            out.add(replayed);
        }
        return out;
    }
    // END_CHANGE: ISS-2025-0488
    // END_CHANGE: ISS-2025-0484

    /** Try the next alternative of {@code cp}, undoing the trail first; sets {@link #goalStack}. */
    private boolean advance(CP cp) {
        // START_CHANGE: ISS-2025-0423 - ENG-01/ENG-03: lazy generator choice points pull one
        // alternative at a time and may never be exhausted (repeat/0, length/2 enumeration).
        if (cp.gen != null) {
            while (true) {
                undo(cp.trailMark);
                it.denzosoft.jprolog.core.engine.Trail.rollbackTo(cp.legacyMark);
                Goal gs = cp.gen.next(cp);
                if (gs == EXHAUSTED) return false;            // generator spent
                if (gs != FAILED) {
                    goalStack = gs;
                    // ISS-2025-0433 - ENG-13: trust-me pop for lazy choice points too
                    if (cp.genExhausted && cp.traceGoal == null
                            && !cps.isEmpty() && cps.get(cps.size() - 1) == cp) {
                        cps.remove(cps.size() - 1);
                        reclaimTrailIfUnreachable();
                    }
                    return true;
                }
            }
        }
        // END_CHANGE: ISS-2025-0423
        while (cp.idx < cp.alts.size()) {
            undo(cp.trailMark);
            it.denzosoft.jprolog.core.engine.Trail.rollbackTo(cp.legacyMark);   // ISS-0316: undo b_setval etc.
            Goal gs = cp.alts.get(cp.idx++).apply();
            if (gs != FAILED) {
                goalStack = gs;
                // START_CHANGE: ISS-2025-0429 - ENG-10: TRUST-ME POP. An exhausted choice point used
                // to stay on `cps` forever (idx == alts.size()), keeping alive everything its Alt
                // closures captured — the continuation, and for bridged built-ins a full copy of the
                // binding map. A deterministic N-step recursion therefore left N dead frames behind
                // and cut/backtrack had to walk through them. Once the LAST alternative has been
                // taken the frame is dead: drop it when it is on top (the only position from which
                // removal cannot shift another frame's absolute cut barrier). Cut barriers stay
                // valid — they are captured BEFORE the push and cut() only removes frames ABOVE the
                // barrier, so a barrier that now equals cps.size() is simply a no-op cut.
                // Traced/debugged frames are kept: they still owe a Redo/Fail port.
                if (cp.idx >= cp.alts.size() && cp.traceGoal == null
                        && !cps.isEmpty() && cps.get(cps.size() - 1) == cp) {
                    cps.remove(cps.size() - 1);
                    reclaimTrailIfUnreachable();
                }
                // END_CHANGE: ISS-2025-0429
                return true;
            }
        }
        return false;
    }

    private boolean backtrack(int floor) {
        while (cps.size() > floor) {
            CP cp = cps.get(cps.size() - 1);
            if (cp.isCatch) { cps.remove(cps.size() - 1); continue; }  // a catch frame has no alternatives
            if (advance(cp)) {
                // re-entering a traced goal to try another clause -> Redo (ISS-2025-0329/0331)
                if (cp.traceGoal != null) {
                    tracePort("Redo", cp.traceGoal, cp.traceDepth);
                    if (cp.traceDebug) debugPort(DebugEvent.Port.REDO, cp.traceGoal, cp.traceDepth);
                }
                return true;
            }
            if (cp.traceGoal != null) {                                // alternatives exhausted -> Fail
                tracePort("Fail", cp.traceGoal, cp.traceDepth);
                if (cp.traceDebug) debugPort(DebugEvent.Port.FAIL, cp.traceGoal, cp.traceDepth);
            }
            cps.remove(cps.size() - 1);
        }
        return false;
    }

    private static Goal pushBody(List<Term> body, int barrier, Goal cont) {
        Goal gs = cont;
        for (int i = body.size() - 1; i >= 0; i--) gs = new Goal(body.get(i), barrier, gs);
        return gs;
    }

    // ----------------------------------------------------------------- variable renaming
    private Rule renameRule(Rule rule) {
        int id = renameCounter++;
        Map<String, Variable> map = new HashMap<>();
        Term head = rename(rule.getHead(), id, map);
        List<Term> body = new ArrayList<>();
        for (Term b : rule.getBody()) body.add(rename(b, id, map));
        return new Rule(head, body);
    }

    // START_CHANGE: ISS-2025-0428 - ENG-09: iterative on the last argument. Renaming a clause whose
    // head or body holds a long list used one Java frame per cell.
    private Term rename(Term t, int id, Map<String, Variable> map) {
        if (t instanceof Variable) {
            String name = ((Variable) t).getName();
            return map.computeIfAbsent(name, nm -> new Variable("_R" + id + "_" + nm));
        }
        if (!(t instanceof CompoundTerm)) return t;
        ArrayList<CompoundTerm> spine = new ArrayList<>();
        CompoundTerm cur = (CompoundTerm) t;
        while (true) {
            spine.add(cur);
            List<Term> as = cur.getArguments();
            if (as.isEmpty()) break;
            Term last = as.get(as.size() - 1);
            if (!(last instanceof CompoundTerm)) break;
            cur = (CompoundTerm) last;
        }
        Term below = null;
        for (int k = spine.size() - 1; k >= 0; k--) {
            CompoundTerm node = spine.get(k);
            List<Term> as = node.getArguments();
            int n = as.size();
            List<Term> args = new ArrayList<>(n);
            for (int i = 0; i < n - 1; i++) args.add(rename(as.get(i), id, map));
            if (n > 0) {
                args.add(k < spine.size() - 1 ? below : rename(as.get(n - 1), id, map));
            }
            below = new CompoundTerm(node.getFunctor(), args);
        }
        return below;
    }
    // END_CHANGE: ISS-2025-0428

    // ----------------------------------------------------------------- result extraction
    private void collectVars(Term root, List<String> out) {
        // ISS-2025-0435 - ENG-15: a HashSet for the membership test; List.contains made this
        // O(n^2) in the number of distinct query variables.
        java.util.Set<String> seen = new java.util.HashSet<>();
        java.util.ArrayDeque<Term> stack = new java.util.ArrayDeque<>();
        stack.push(root);
        while (!stack.isEmpty()) {
            Term t = stack.pop();
            if (t instanceof Variable) {
                String n = ((Variable) t).getName();
                if (seen.add(n)) out.add(n);
            } else if (t instanceof CompoundTerm) {
                for (Term a : ((CompoundTerm) t).getArguments()) stack.push(a);
            }
        }
    }

    private Map<String, Term> snapshot(List<String> vars) {
        Map<String, Term> m = new HashMap<>();
        for (String v : vars) m.put(v, resolve(new Variable(v)));
        return m;
    }

    private Term resolve(Term t) { return resolve(t, new java.util.HashSet<>()); }

    /** Fully dereference {@code t}, detecting cyclic terms (e.g. X = f(X) with occurs_check off) so a
     *  rational tree raises a controlled representation_error instead of a {@link StackOverflowError}
     *  (ISS-2025-0313). {@code active} holds the variable names on the current resolution path. */
    private Term resolve(Term t, java.util.Set<String> active) {
        if (t instanceof Variable) {
            String n = ((Variable) t).getName();
            Term b = binding.get(n);
            if (b == null) return t;                                  // unbound
            if (!active.add(n)) {                                     // already on the path -> cycle
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.representationError("cyclic_term", "resolve"));
            }
            Term r = resolve(b, active);
            active.remove(n);
            return r;
        }
        if (t instanceof CompoundTerm) {
            // START_CHANGE: ISS-2025-0428 - ENG-09: iterative on the LAST argument. resolve() is the
            // hottest walker in the engine (every builtin call, every trace line, every snapshot) and
            // used one Java frame per list cell, capping terms at ~20-30k elements even with -Xss4m.
            //
            // Phase 1 walks the last-argument spine, dereferencing THROUGH bound variables (a list
            // tail is normally a bound variable) and pushing every variable it passes into {@code
            // active} — exactly what the recursive version did — so rational-tree detection is
            // unchanged. Phase 2 rebuilds bottom-up, popping each link's variables again as it
            // leaves that level, so a sibling subtree sees precisely the path above it in
            // {@code active} (a cycle reached through a NON-last argument is still caught).
            ArrayList<CompoundTerm> spine = new ArrayList<>();
            ArrayList<List<String>> linkVars = new ArrayList<>();   // vars crossed below spine[k]
            CompoundTerm cur = (CompoundTerm) t;
            while (true) {
                spine.add(cur);
                List<Term> as = cur.getArguments();
                if (as.isEmpty()) { linkVars.add(null); break; }
                Term last = as.get(as.size() - 1);
                List<String> crossed = null;
                while (last instanceof Variable) {
                    String vn = ((Variable) last).getName();
                    Term b = binding.get(vn);
                    if (b == null) break;                            // unbound: end of the chain
                    if (!active.add(vn)) {                           // already on the path -> cycle
                        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms
                                .representationError("cyclic_term", "resolve"));
                    }
                    if (crossed == null) crossed = new ArrayList<>(1);
                    crossed.add(vn);
                    last = b;
                }
                linkVars.add(crossed);
                if (!(last instanceof CompoundTerm)) break;
                cur = (CompoundTerm) last;
            }
            // The deepest node's last argument is resolved normally below, and resolve() re-enters
            // the variables leading to it — release them first so that is not seen as a cycle.
            List<String> tailCrossed = linkVars.get(spine.size() - 1);
            if (tailCrossed != null) for (String v : tailCrossed) active.remove(v);

            // START_CHANGE: ISS-2025-0430 - ENG-11: STRUCTURE SHARING. resolve() used to allocate a
            // fresh CompoundTerm for every node, so dereferencing a goal that mentions a big ground
            // term copied the whole term. Nodes whose arguments all resolve to themselves are now
            // returned unchanged, which is what makes the resolved-goal handoff to built-ins cheap.
            Term below = null;
            boolean belowChanged = false;
            for (int k = spine.size() - 1; k >= 0; k--) {
                CompoundTerm node = spine.get(k);
                List<Term> as = node.getArguments();
                int n = as.size();
                boolean hasSpineChild = (k < spine.size() - 1);
                List<Term> args = null;                              // lazy: only when something changed
                for (int i = 0; i < n; i++) {
                    Term arg = as.get(i);
                    Term res;
                    if (hasSpineChild && i == n - 1) {
                        // the link may have crossed bound variables, in which case the argument
                        // itself changes (variable -> its value) even when the child node did not
                        res = belowChanged ? below
                            : (linkVars.get(k) != null ? spine.get(k + 1) : arg);
                    } else {
                        res = resolve(arg, active);
                    }
                    if (res != arg && args == null) {
                        args = new ArrayList<>(n);
                        for (int j = 0; j < i; j++) args.add(as.get(j));
                    }
                    if (args != null) args.add(res);
                }
                if (args == null) { below = node; belowChanged = false; }
                else { below = new CompoundTerm(node.getFunctor(), args); belowChanged = true; }
                if (k > 0) {
                    List<String> vs = linkVars.get(k - 1);           // leaving this level
                    if (vs != null) for (String v : vs) active.remove(v);
                }
            }
            return below;
            // END_CHANGE: ISS-2025-0430
            // END_CHANGE: ISS-2025-0428
        }
        return t;
    }
}
