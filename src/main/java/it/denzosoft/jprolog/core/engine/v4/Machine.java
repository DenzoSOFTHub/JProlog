package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.ControlFlow;
import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.DebugEvent;
import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.module.Module;
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
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0442 - engine v4, design B.6 (execution core).
/**
 * The v4 resolution machine: an iterative SLD engine over <b>variable cells</b> and <b>compiled
 * clause skeletons</b>.
 *
 * <p>Its drive loop has the shape of the v2 engine's — that part of the old engine was
 * right — with the four structural changes of the v4 design:
 * <ol>
 *   <li><b>No binding store.</b> Bindings live in {@link Variable#ref}; {@link Bindings} only holds
 *       the trail, with conditional trailing. A deterministic recursion therefore leaves nothing
 *       behind: {@code loop(10000000)} runs in a 64 MB heap, where v3.8.0 needed gigabytes because
 *       its name-keyed {@code HashMap} never reclaimed a dead variable (LIM-033 / L-01).</li>
 *   <li><b>Clause activation without copying.</b> {@link Clause#unifyHead} unifies the goal
 *       directly against the skeleton into a {@code Term[nvars]} frame; body goals are instantiated
 *       one at a time when they are pushed (L-12).</li>
 *   <li><b>Cycle-safe, cancellable walkers</b> ({@link Unify}): rational trees unify instead of
 *       hanging, and every long walk polls the {@link ResourceGuard} (L-04).</li>
 *   <li><b>Cleanup frames</b> for {@code setup_call_cleanup/3} and {@code call_cleanup/2}, and
 *       {@code OutOfMemoryError} converted to a catchable {@code resource_error(memory)} before the
 *       query unwinds (L-14).</li>
 * </ol>
 *
 * <p>Everything the machine does not implement natively runs through {@link LegacyBuiltinAdapter},
 * which drives the ~400 existing {@code BuiltIn} classes unchanged, so v4 is a drop-in alternative
 * from the first wave.
 *
 * <p><b>Trust model</b> (unchanged, and load-bearing): {@code InferenceLimitException},
 * {@code QueryCancelledException} and {@code DebugStopException} are plain {@code RuntimeException}s
 * and are never converted into a {@code PrologException}, so untrusted {@code catch/3} cannot trap
 * them; every broad catch in this file starts with {@link ControlFlow#rethrowIfControl}.
 */
public final class Machine {

    // ------------------------------------------------------------------ context

    private final Engine engine;
    private final Bindings B;
    private final ResourceGuard guard;
    private DebugController debugController;
    private String currentContext = "call";

    // START_CHANGE: ISS-2025-0466 - wave W6, design B.10: the module the goal currently being
    // stepped executes in. null == `user`. It is derived, never guessed: the drive loop sets it
    // from the goal it pops, and every goal a construct pushes inherits it (or, for a clause body,
    // carries the DEFINING module). A nested drive (findall, catch, a meta-call, a native
    // sub-query) saves and restores it, exactly as it saves the goal stack.
    private String ctxModule;

    /** The module the goal being executed right now runs in — never null. */
    public String contextModule() { return (ctxModule == null) ? Modules.USER : ctxModule; }

    /** A goal that continues in the CURRENT context module. */
    private Goal mg(Term t, int barrier, Goal next) {
        Goal g = new Goal(t, barrier, next);
        g.module = ctxModule;
        return g;
    }

    /** A goal that runs in an explicit module ({@code null} == {@code user}). */
    private static Goal mg(Term t, int barrier, Goal next, String mod) {
        Goal g = new Goal(t, barrier, next);
        g.module = mod;
        return g;
    }

    /** {@code null} for {@code user}, so the hot path never stores a redundant string. */
    private static String modKey(String name) {
        return (name == null || Modules.USER.equals(name)) ? null : name;
    }
    // END_CHANGE: ISS-2025-0466

    /** Lazily built: the SolverContext legacy context built-ins receive (design B.5). */
    private SolverFacade facade;

    // START_CHANGE: ISS-2025-0460 - the name -> cell COMPATIBILITY SHIM of waves W1-W3 is GONE.
    // It existed for the handful of legacy built-ins that report a binding by NAME for a cell their
    // goal never mentions; the live case was always the CLP(FD) v2 bridge's exportSingletons/1
    // (`C in 1..3, D #= C*2+1, label([C])` reports D). With wave W4 the bridge is an ordinary
    // attributed-cell client and keeps the engine cell of every FD variable it created in its own
    // per-query context, so LegacyBuiltinAdapter asks IT (ClpfdV2Bridge.cellFor) instead of a
    // general, engine-wide name index. Nothing else in the engine maps a name to a cell any more.
    // END_CHANGE: ISS-2025-0460

    // START_CHANGE: ISS-2025-0450 - a Machine is single-threaded by construction (one goal stack,
    // one choice-point list, bindings in shared Variable cells). The concurrency built-ins submit
    // sub-solves to worker threads; SolverFacade checks this before routing one onto the machine.
    private final Thread owner = Thread.currentThread();

    /** True when the caller is the thread this machine was created on. */
    boolean onOwnerThread() { return Thread.currentThread() == owner; }

    // START_CHANGE: ISS-2025-0480 - wave W8: with every concurrency predicate on its own machine
    // (core.engine.v4.Workers) there is no legitimate cross-thread entry left, so the former
    // "fall back to the recursive solver" check becomes an ASSERTION. It is deliberately an
    // IllegalStateException and not a PrologException: a Prolog program cannot cause it, only a
    // built-in that hands this machine to another thread can, and untrusted `catch/3` must not be
    // able to swallow the report (invariant 9, the trust model).
    /** @throws IllegalStateException when called from a thread other than this machine's owner */
    void assertOwnerThread(String what) {
        if (Thread.currentThread() != owner) {
            throw new IllegalStateException(
                "engine v4: " + what + " was entered from thread '" + Thread.currentThread().getName()
                + "' but this Machine belongs to '" + owner.getName()
                + "'. A worker must run on its own Machine (core.engine.v4.Workers).");
        }
    }
    // END_CHANGE: ISS-2025-0480
    // END_CHANGE: ISS-2025-0450

    // START_CHANGE: ISS-2025-0479 - false on a WORKER machine (core.engine.v4.Workers): the
    // query-boundary sweeps in solve()'s finally are engine-wide and belong to the top-level query.
    private boolean queryBoundary = true;

    /** Mark this machine as a worker: it runs inside another query and owns no query boundary. */
    void asWorker() { queryBoundary = false; }
    // END_CHANGE: ISS-2025-0479

    public Machine(Engine engine, ResourceGuard guard) {
        this.engine = engine;
        this.guard = guard;
        this.B = new Bindings(guard);
        // ISS-2025-0457 - wave W4: the real wake queue (design B.9) replaces the W1 AttrBridge
        this.B.attrHandler = new WakeHandler(this);
    }

    /** The solver handed to {@code BuiltInWithContext} built-ins. */
    SolverFacade facade() {
        if (facade == null) facade = new SolverFacade(this);
        return facade;
    }

    public Engine engine() { return engine; }
    public ResourceGuard guard() { return guard; }
    Bindings bindings() { return B; }

    /** The predicate indicator errors raised right now are stamped with. */
    public String currentContext() { return currentContext; }

    // ------------------------------------------------------------------ goal stack / choice points

    /** A pending goal plus the choice-point height a {@code !} in it cuts back to. Either a term
     *  (possibly a skeleton to instantiate against {@code frame}) or an internal action. */
    static final class Goal {
        final Term term; final int cutBarrier; final Goal next; final Runnable action; final Term[] frame;
        // START_CHANGE: ISS-2025-0466 - wave W6: the CONTEXT MODULE this goal runs in (design
        // B.10). null means `user`, which is both the default and the common case, so a
        // single-module program allocates and compares nothing extra.
        String module;
        // END_CHANGE: ISS-2025-0466
        Goal(Term term, int cutBarrier, Goal next) {
            this.term = term; this.cutBarrier = cutBarrier; this.next = next; this.action = null; this.frame = null;
        }
        Goal(Term skeleton, Term[] frame, int cutBarrier, Goal next) {
            this.term = skeleton; this.cutBarrier = cutBarrier; this.next = next; this.action = null; this.frame = frame;
        }
        Goal(Runnable action, Goal next) {
            this.term = null; this.cutBarrier = 0; this.next = next; this.action = action; this.frame = null;
        }
    }

    /** A lazy alternative supply. Returns the next goal stack, {@link #FAILED} to skip, or
     *  {@link #EXHAUSTED} when spent. Never null: null is a perfectly good (empty) goal stack. */
    interface Gen { Goal next(CP cp); }

    static final Goal FAILED = new Goal((Term) null, -1, null);
    static final Goal EXHAUSTED = new Goal((Term) null, -2, null);

    static final class CP {
        static final int CLAUSES = 0, GEN = 1, CATCH = 2, CLEANUP = 3;
        final int kind;
        final int trailMark;
        final long serialMark;
        Goal cont;
        int cutBarrier;
        // CLAUSES
        Term goal; Clause[] clauses; int idx; int limit; long generation; int barrier;
        // GEN
        Gen gen; boolean genExhausted;
        // START_CHANGE: ISS-2025-0482 - wave W8: how many alternatives this frame has handed out.
        // A frame that produced exactly ONE and is now exhausted is DETERMINISTIC: it can never
        // Redo, so it owes no port and may be trust-me popped even while tracing. Keeping it (the
        // pre-W8 rule for any traced frame) is what made `loop(1000000)` under trace retain one
        // choice point per iteration.
        int altsTaken;
        // END_CHANGE: ISS-2025-0482
        // CATCH
        Term catcher, recovery; boolean active = true;
        /** ISS-2025-0466: the context module the recovery goal runs in. */
        String module;
        // CLEANUP
        Term cleanup; boolean cleanupDone;
        // START_CHANGE: ISS-2025-0463 - TABLING: the generator/consumer frame of a tabled call.
        // A frame destroyed in mid-production (cut, exception, budget) must discard its half-built
        // table, so cut() and handleBall() tell it.
        Tabling.TableFrame tframe;
        // END_CHANGE: ISS-2025-0463
        // four-port tracing (ISS-2025-0329/0331): Redo/Fail are emitted from here
        Term traceGoal; int traceDepth; boolean traceDebug;

        CP(int kind, int trailMark) {
            this.kind = kind;
            this.trailMark = trailMark;
            this.serialMark = Variable.currentSerial();
            // ISS-2025-0492: no second mark. The bridged built-ins' undo actions live on the same
            // Bindings trail as the cell resets, so `trailMark` covers both.
        }
    }

    private static final Clause[] NO_CLAUSES = new Clause[0];

    private Goal goalStack;
    private final ArrayList<CP> cps = new ArrayList<CP>();
    /** Goals woken by binding a frozen/attributed variable; run before the next goal. */
    private final ArrayList<Term> woken = new ArrayList<Term>();

    private void pushCP(CP cp) {
        cps.add(cp);
        B.barrierSerial = cp.serialMark;
    }

    private CP popCP() {
        CP cp = cps.remove(cps.size() - 1);
        B.barrierSerial = cps.isEmpty() ? 0 : cps.get(cps.size() - 1).serialMark;
        return cp;
    }

    /** Test hook: live choice points. */
    public int choicePointCount() { return cps.size(); }
    /** Test hook: live trail entries. */
    public int trailSize() { return B.size(); }

    // ------------------------------------------------------------------ built-in facing API (B.5)

    /** Dereference a term through the binding cells. */
    public Term deref(Term t) { return Unify.deref(t); }

    /** Unify, trailing as needed. */
    public boolean unify(Term a, Term b) { return Unify.unify(a, b, B); }

    // START_CHANGE: ISS-2025-0453 - a FAILED unification can leave partial bindings behind (it
    // fails at the first mismatched argument, having already bound the ones before it). The machine
    // undoes them at the next redo, which is fine for a clause head; a Generator that tries several
    // alternatives INSIDE one next() has to undo them itself, or the second attempt sees the first
    // one's bindings. The forceTrail bracket is mandatory: without it a binding made when no choice
    // point exists is not trailed at all. Note the ISS-2025-0448 ordering — undo, THEN close.
    /** Unify; on failure undo every binding it made. */
    public boolean unifyOrUndo(Term a, Term b) {
        int mark = B.mark();
        B.forceTrail++;
        boolean ok;
        try {
            ok = Unify.unify(a, b, B);
            if (!ok) B.undo(mark);
        } finally {
            B.forceTrail--;
        }
        return ok;
    }
    // END_CHANGE: ISS-2025-0453

    /** Fully dereference (structure-sharing) — the handoff shape for legacy built-ins. */
    public Term resolve(Term t) { return Unify.resolve(t, guard); }

    /** {@code copy_term} semantics. */
    public Term copy(Term t) { return Unify.copy(t, new IdentityHashMap<Variable, Variable>(), guard); }

    /** Push a goal in front of the current continuation (opaque to cut). */
    public void pushGoal(Term goal) { goalStack = mg(goal, cps.size(), goalStack); }

    // START_CHANGE: ISS-2025-0453 - wave W3: the native library built-ins are nondeterministic, so
    // pushGenerator has to own the Exit/Redo/Fail ports of the goal that installed it. callNative
    // stashes the traced goal here before the call; the generator's choice point carries it, exactly
    // as a user predicate's clause frame does, and callNative then knows not to emit Exit twice.
    private Term nativeTraceGoal;
    private int nativeTraceDepth = -1;
    private boolean nativePortsHandled;

    /** Install a nondeterministic built-in's generator as a choice point and take its first
     *  solution. Returns false when the generator produced nothing. */
    public boolean pushGenerator(final Generator generator) {
        final Term tg = nativeTraceGoal;
        final int td = nativeTraceDepth;
        Goal after = goalStack;
        if (tg != null) {
            after = new Goal(new Runnable() { @Override public void run() { portExit(tg, td); } }, after);
            nativePortsHandled = true;
        }
        CP cp = new CP(CP.GEN, B.mark());
        cp.gen = new GeneratorGen(this, generator, after);
        if (tg != null) { cp.traceGoal = tg; cp.traceDepth = td; cp.traceDebug = debugPortsActive(); }
        pushCP(cp);
        if (advance(cp)) return true;
        popCP();
        if (tg != null) portFail(tg, td);
        return false;
    }
    // END_CHANGE: ISS-2025-0453

    // ------------------------------------------------------------------ entry point

    public interface SolutionSink { boolean onSolution(Map<String, Term> solution); }

    /**
     * Solve {@code query}, streaming each solution; the sink returns false to stop.
     *
     * <p>The query term is normalised first: one {@link Variable} cell per distinct name. That
     * matters because v4 identifies variables by object, while several producers of terms (the
     * legacy parser, {@code JpcReader}, {@code atom_to_term}) allocate one object per occurrence.
     */
    public void solve(Term query, SolutionSink sink) {
        cps.clear();
        woken.clear();
        portDepth = 0;                                             // ISS-2025-0482
        // START_CHANGE: ISS-2025-0479 - a WORKER machine does not attach the debug controller.
        // DebugController keeps ONE call stack and ONE pause lock for the session; a worker firing
        // ports into them from another thread would corrupt the stack the IDE renders and could
        // block a worker inside waitForUserAction with no way for the user to know. Tracing
        // (trace/0) still works in a worker — it only prints. Debugging INTO a thread would need a
        // controller per thread; recorded in section 14.6 of the v4 progress report.
        debugController = (queryBoundary && engine.context() != null)
            ? engine.context().getDebugController() : null;
        // END_CHANGE: ISS-2025-0479

        LinkedHashMap<String, Variable> queryVars = new LinkedHashMap<String, Variable>();
        Term q = normalise(query, queryVars);
        final List<String> names = new ArrayList<String>(queryVars.keySet());
        final List<Variable> cells = new ArrayList<Variable>(queryVars.values());
        ctxModule = modKey(engine.modules4().currentModule());      // ISS-2025-0466
        goalStack = mg(q, 0, null);
        // START_CHANGE: ISS-2025-0479 - a WORKER machine must not touch engine-wide, query-boundary
        // state: the shared EngineContext's guard field, the clause-store compaction and the tabling
        // sweep all belong to the top-level query, and a worker finishing first would install its
        // own guard on the parent's solver and ABANDON the parent's in-progress tabled evaluation.
        final boolean top = queryBoundary;
        it.denzosoft.jprolog.core.engine.ResourceGuard prevGuard =
            (top && engine.context() != null) ? engine.context().getResourceGuard() : null;
        if (top && engine.context() != null) engine.context().setResourceGuard(guard);
        // END_CHANGE: ISS-2025-0479
        // START_CHANGE: ISS-2025-0492 - this machine takes the bridged built-ins' undo actions
        // while it runs (b_setval/2, op/3, setarg/3, the CLP(FD) store). Saved and restored, so a
        // nested machine hands the role back.
        Machine prevUndoTarget = Undo.enter(this);
        try {
            drive(new Driver() {
                @Override public boolean onSolution() { return sink.onSolution(snapshot(names, cells)); }
            }, 0);
        } finally {
            if (top) {                                             // ISS-2025-0479
                if (engine.context() != null) engine.context().setResourceGuard(prevGuard);
                engine.store().compact();      // query boundary: no call can hold an old generation
                // ISS-2025-0463: no table may stay EVALUATING across queries. A query that unwound
                // through an exception, the inference budget or a Stop interrupt leaves half-built
                // tables behind; they are discarded here so a later call recomputes them.
                engine.tabling().endQuery();
            } else {
                // ISS-2025-0488 (LIM-039): a WORKER owns no query boundary, but it must not walk
                // away holding the store's evaluation claim either — whatever it left EVALUATING
                // is abandoned and the store is handed back to the threads waiting for it.
                engine.tabling().endWorker();
            }
            Undo.exit(prevUndoTarget);                             // ISS-2025-0492
            // END_CHANGE: ISS-2025-0492
        }
    }

    /**
     * Push a backtrackable undo action for a BRIDGED built-in ({@code b_setval/2}, {@code op/3},
     * {@code setarg/3}, the CLP(FD) store). It lands on the same trail as the cell resets, so
     * {@code B.undo(cp.trailMark)} runs it at exactly the point the bindings are undone.
     * See {@link Undo}. (ISS-2025-0492)
     */
    void pushUndo(Runnable undo) { B.pushUndo(undo); }

    /** Rebuild {@code t} so that all occurrences of a variable NAME share one cell. */
    public static Term normalise(Term t, Map<String, Variable> vars) {
        if (t instanceof Variable) {
            Variable v = (Variable) t;
            if (v.ref != null) return t;                    // already a live cell
            Variable shared = vars.get(v.getName());
            if (shared == null) { vars.put(v.getName(), v); return v; }
            return shared;
        }
        if (!(t instanceof CompoundTerm)) return t;
        ArrayList<CompoundTerm> spine = new ArrayList<CompoundTerm>();
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
        boolean belowChanged = false;
        for (int k = spine.size() - 1; k >= 0; k--) {
            CompoundTerm node = spine.get(k);
            List<Term> as = node.getArguments();
            int ar = as.size();
            boolean hasSpineChild = (k < spine.size() - 1);
            List<Term> out = null;
            for (int i = 0; i < ar; i++) {
                Term arg = as.get(i);
                Term res = (i == ar - 1 && hasSpineChild) ? (belowChanged ? below : arg)
                                                          : normalise(arg, vars);
                if (res != arg && out == null) {
                    out = new ArrayList<Term>(ar);
                    for (int j = 0; j < i; j++) out.add(as.get(j));
                }
                if (out != null) out.add(res);
            }
            if (out == null) { below = node; belowChanged = false; }
            else { below = new CompoundTerm(node.getFunctor(), out); belowChanged = true; }
        }
        return below;
    }

    private Map<String, Term> snapshot(List<String> names, List<Variable> cells) {
        Map<String, Term> m = new HashMap<String, Term>();
        for (int i = 0; i < names.size(); i++) m.put(names.get(i), Unify.resolve(cells.get(i), guard));
        return m;
    }

    private interface Driver { boolean onSolution(); }

    // ------------------------------------------------------------------ drive loop

    private static final Atom ATOM_TRUE = new Atom("true");
    private static final Atom ATOM_FAIL = new Atom("fail");
    private static final Atom CUT = new Atom("!");
    private static final Atom DOT = new Atom(".");
    private static final Atom NIL = new Atom("[]");
    private static final Atom COMMA = new Atom(",");

    /**
     * Run until exhausted. {@code floor} is the choice-point height this run must not backtrack
     * below, so a nested run (findall, catch, a meta-call) leaves the caller's choice points alone.
     */
    private void drive(Driver onSol, int floor) {
        while (true) {
            guard.step();
            try {
                if (!woken.isEmpty()) {                       // freeze/attribute-woken goals run next
                    for (int i = woken.size() - 1; i >= 0; i--) {
                        // ISS-2025-0466: a wake goal is engine-internal ('$attr_unify'/4) and
                        // resolves from `user`, so the coroutining library autoloads for it.
                        goalStack = mg(woken.get(i), cps.size(), goalStack, null);
                    }
                    woken.clear();
                    continue;
                }
                if (goalStack == null) {
                    if (!onSol.onSolution() || !backtrack(floor)) return;
                    continue;
                }
                Goal g = goalStack;
                goalStack = g.next;
                if (g.action != null) { g.action.run(); continue; }
                ctxModule = g.module;                         // ISS-2025-0466
                Term t = (g.frame != null) ? Clause.instantiate(g.term, g.frame) : g.term;
                t = Unify.deref(t);

                if (t instanceof Atom) {
                    if (!step0((Atom) t, g, floor)) return;
                    continue;
                }
                if (t instanceof CompoundTerm) {
                    if (!stepN((CompoundTerm) t, g, floor)) return;
                    continue;
                }
                // A non-callable in goal position is an ISO error, not a silent failure.
                if (t instanceof Variable) throw Errors.instantiation("call");
                throw Errors.type("callable", Unify.resolve(t, guard), "call");
            } catch (PrologException e) {
                Term ball = e.getErrorTerm();
                if (ball == null) throw e;
                if (!handleBall(Unify.copy(ball, new IdentityHashMap<Variable, Variable>(), guard), floor)) throw e;
            } catch (StackOverflowError so) {
                // The v4 core is iterative, but a legacy built-in can still overflow. Convert it
                // to a catchable ISO error INSIDE the loop so the running program's catch/3 sees
                // it (design B.6, limit L-14) — the v2 engine only converted after unwinding.
                if (!handleBall(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms
                        .resourceError("stack_overflow", currentContext), floor)) {
                    throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms
                        .resourceError("stack_overflow", currentContext));
                }
            } catch (OutOfMemoryError oom) {
                cps.clear();                                   // free the frames before doing anything
                B.clearIfUnreachable(true);
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms
                    .resourceError("memory", currentContext));
            }
        }
    }

    /** One drive step for an atom goal. Returns false to stop the run. */
    private boolean step0(Atom a, Goal g, int floor) {
        String n = a.getName();
        if ("true".equals(n)) return true;
        if ("fail".equals(n) || "false".equals(n)) return backtrack(floor);
        if ("!".equals(n)) { cut(g.cutBarrier); return true; }
        if ("repeat".equals(n)) { repeat(a); return true; }
        Builtin nat = engine.natives().lookup(n, 0);
        if (nat != null) {
            Integer r = callNative(nat, a, n, 0, LegacyBuiltinAdapter.NO_ARGS);
            if (r.intValue() == 1) return true;
            return backtrack(floor);
        }
        int rb = LegacyBuiltinAdapter.run(this, a, n, 0);
        if (rb == 1) return true;
        if (rb == 0) return backtrack(floor);
        if (!callUser(a, a)) return backtrack(floor);
        return true;
    }

    /** One drive step for a compound goal. Returns false to stop the run. */
    private boolean stepN(CompoundTerm c, Goal g, int floor) {
        String f = c.getName();
        List<Term> a = c.getArguments();
        int n = a.size();

        if (n == 2) {
            if (",".equals(f)) {
                goalStack = mg(a.get(0), g.cutBarrier, mg(a.get(1), g.cutBarrier, goalStack));
                return true;
            }
            if (";".equals(f)) {
                Term left = Unify.deref(a.get(0));
                if (left instanceof CompoundTerm && ((CompoundTerm) left).getArguments().size() == 2
                        && "->".equals(((CompoundTerm) left).getName())) {
                    CompoundTerm arrow = (CompoundTerm) left;
                    ite(arrow.getArguments().get(0), arrow.getArguments().get(1), a.get(1), g.cutBarrier);
                } else if (left instanceof CompoundTerm && ((CompoundTerm) left).getArguments().size() == 2
                        && "*->".equals(((CompoundTerm) left).getName())) {
                    CompoundTerm sc = (CompoundTerm) left;
                    softCut(sc.getArguments().get(0), sc.getArguments().get(1), a.get(1), g.cutBarrier);
                } else {
                    disjunction(a.get(0), a.get(1), g.cutBarrier);
                }
                return true;
            }
            if ("->".equals(f)) { ite(a.get(0), a.get(1), ATOM_FAIL, g.cutBarrier); return true; }
            if ("*->".equals(f)) { softCut(a.get(0), a.get(1), ATOM_FAIL, g.cutBarrier); return true; }
            // START_CHANGE: ISS-2025-0481 - wave W8 (design B.6, limit L-13): the inline path is no
            // longer disabled while debugging. The machine emits the two ports itself, so a debugged
            // run executes exactly the same code as an undebugged one.
            if ("=".equals(f)) {
                if (!debugTraceActive()) {
                    if (!Unify.unify(a.get(0), a.get(1), B)) return backtrack(floor);
                    return true;
                }
                final int d = enterPort();
                portCall(c, d);
                if (!Unify.unify(a.get(0), a.get(1), B)) { portFail(c, d); return backtrack(floor); }
                portExit(c, d);
                return true;
            }
            // END_CHANGE: ISS-2025-0481
            if ("^".equals(f)) {                                   // V^Goal as a plain goal == call(Goal)
                goalStack = mg(a.get(1), cps.size(), goalStack);
                return true;
            }
            if (":".equals(f)) {                                   // Module:Goal — design B.10
                if (!qualifiedCall(c)) return backtrack(floor);
                return true;
            }
            // START_CHANGE: ISS-2025-0469 - '$mctx'(M, G): run G with M as the CONTEXT module.
            // This is how a meta-argument carries its caller's module (see Modules.MCTX); unlike
            // M:G it is internal access, so a library predicate can call back into a private
            // helper of the module that called it.
            if (Modules.MCTX.equals(f)) {
                Term mt = Unify.deref(a.get(0));
                Term inner = a.get(1);
                goalStack = mg(inner, cps.size(), goalStack,
                    (mt instanceof Atom) ? modKey(((Atom) mt).getName()) : ctxModule);
                return true;
            }
            // END_CHANGE: ISS-2025-0469
        }
        if (n == 1 && ("\\+".equals(f) || "not".equals(f))) {
            ite(a.get(0), ATOM_FAIL, ATOM_TRUE, g.cutBarrier);
            return true;
        }
        if ("call".equals(f) && n >= 1) {
            // START_CHANGE: ISS-2025-0455 - library(yall) lambdas are expanded here, before the
            // arguments are appended: `call([X,Y]>>Body, 1, Y)` must copy the lambda, bind X=1 and
            // run Body — appending would have built the nonexistent `>>/4`.
            Term callee = Unify.deref(a.get(0));
            List<Term> extra = (n == 1) ? java.util.Collections.<Term>emptyList() : a.subList(1, n);
            Term goal;
            if (Lambdas.isLambda(callee)) {
                goal = Lambdas.expand(this, callee, extra);
                if (goal == null) goal = (n == 1) ? a.get(0) : addArgs(callee, extra);
            } else {
                goal = (n == 1) ? a.get(0) : addArgs(callee, extra);
            }
            // END_CHANGE: ISS-2025-0455
            goalStack = mg(goal, cps.size(), goalStack);
            return true;
        }
        // START_CHANGE: ISS-2025-0455 - a lambda reached directly in goal position, i.e. after
        // maplist/N appended its arguments to `[X,Y]>>Body` (`>>/4`) or after the user wrote one.
        if ((">>".equals(f) && n >= 2) || ("\\".equals(f) && n >= 1)
                || ("/".equals(f) && n >= 2 && Lambdas.isLambda(new CompoundTerm(c.getFunctor(), a.subList(0, 2))))) {
            int fixed = "\\".equals(f) ? 1 : 2;
            if (n >= fixed) {
                Term lam = (n == fixed) ? c : new CompoundTerm(c.getFunctor(), a.subList(0, fixed));
                if (Lambdas.isLambda(lam)) {
                    Term goal = Lambdas.expand(this, lam, a.subList(fixed, n));
                    if (goal != null) {
                        goalStack = mg(goal, cps.size(), goalStack);
                        return true;
                    }
                }
            }
        }
        // END_CHANGE: ISS-2025-0455
        if (n == 3 && "findall".equals(f)) {
            it.denzosoft.jprolog.core.utils.CollectionUtils
                .checkInstancesArgument(Unify.resolve(a.get(2), guard), "findall/3");
            Term list = makeList(findAll(a.get(0), a.get(1)));
            if (!Unify.unify(a.get(2), list, B)) return backtrack(floor);
            return true;
        }
        if (n == 3 && "catch".equals(f)) {
            final CP frame = new CP(CP.CATCH, B.mark());
            frame.catcher = a.get(1);
            frame.recovery = a.get(2);
            frame.cont = goalStack;
            frame.cutBarrier = g.cutBarrier;
            frame.module = ctxModule;                          // ISS-2025-0466
            pushCP(frame);
            goalStack = mg(a.get(0), cps.size(), new Goal(new Runnable() {
                @Override public void run() {
                    frame.active = false;                       // ISO 7.8.9: only during Goal's extent
                    B.pushUndo(new Runnable() { @Override public void run() { frame.active = true; } });
                }
            }, goalStack));
            return true;
        }
        if (n == 1 && "throw".equals(f)) {
            Term ball = Unify.resolve(a.get(0), guard);
            if (ball instanceof Variable) throw Errors.instantiation("throw/1");
            throw new PrologException(ball);
        }
        if (n == 1 && ("assertz".equals(f) || "assert".equals(f))) { assertClause(a.get(0), false); return true; }
        if (n == 1 && "asserta".equals(f)) { assertClause(a.get(0), true); return true; }
        if (n == 1 && "retract".equals(f)) {
            if (!retractClause(a.get(0))) return backtrack(floor);
            return true;
        }
        if (n == 3 && "setup_call_cleanup".equals(f)) {
            if (!setupCallCleanup(a.get(0), a.get(1), a.get(2), g.cutBarrier)) return backtrack(floor);
            return true;
        }
        if (n == 2 && "call_cleanup".equals(f)) {
            if (!setupCallCleanup(ATOM_TRUE, a.get(0), a.get(1), g.cutBarrier)) return backtrack(floor);
            return true;
        }
        // START_CHANGE: ISS-2025-0481 - wave W8 (design B.6, limit L-13): these four keep their
        // native inline path while tracing/debugging; the wrapper's own four ports are emitted by
        // iteTraced / betweenNative instead of by the legacy bridge, so the tracer no longer
        // changes how the program runs.
        if (n == 1 && "once".equals(f)) {
            if (debugTraceActive()) iteTraced(c, a.get(0), ATOM_TRUE, ATOM_FAIL, g.cutBarrier);
            else ite(a.get(0), ATOM_TRUE, ATOM_FAIL, g.cutBarrier);
            return true;
        }
        if (n == 1 && "ignore".equals(f)) {
            if (debugTraceActive()) iteTraced(c, a.get(0), ATOM_TRUE, ATOM_TRUE, g.cutBarrier);
            else ite(a.get(0), ATOM_TRUE, ATOM_TRUE, g.cutBarrier);
            return true;
        }
        if (n == 2 && "forall".equals(f)) {
            Term negAction = new CompoundTerm(new Atom("\\+"), java.util.Collections.singletonList(a.get(1)));
            Term conj = new CompoundTerm(COMMA, Arrays.asList(a.get(0), negAction));
            if (debugTraceActive()) iteTraced(c, conj, ATOM_FAIL, ATOM_TRUE, g.cutBarrier);
            else ite(conj, ATOM_FAIL, ATOM_TRUE, g.cutBarrier);
            return true;
        }
        if (n == 3 && "between".equals(f)) {
            int r = betweenNative(c, a);
            if (r == 1) return true;
            if (r == 0) return backtrack(floor);
        }
        // END_CHANGE: ISS-2025-0481
        if (n == 2 && "length".equals(f) && lengthEnumerate(c, a)) return true;

        // START_CHANGE: ISS-2025-0481 - wave W8 (design B.6, limit L-13): before this wave the
        // whole inline table was skipped whenever a DebugController was attached, so `X = 1`,
        // `Y is X+2` and `integer(Y)` ran through the legacy bridge (a different code path, a
        // materialised solution map and a forced choice point) only while debugging. They now run
        // the same way always and the machine emits their Call/Exit/Fail ports itself. They are
        // deterministic, so they own no Redo and push no choice point.
        if (!debugTraceActive()) {
            int r = solveBuiltin(f, a, n);
            if (r == 1) return true;
            if (r == 0) return backtrack(floor);
        } else if (isInlineBuiltin(f, n)) {
            final int d = enterPort();
            portCall(c, d);
            int r = solveBuiltin(f, a, n);                    // a PrologException leaves no port,
            if (r == 1) { portExit(c, d); return true; }      // exactly as the bridge did
            if (r == 0) { portFail(c, d); return backtrack(floor); }
        }
        // END_CHANGE: ISS-2025-0481
        Builtin nat = engine.natives().lookup(f, n);
        if (nat != null) {                                    // v4 natives run even while debugging:
            Integer r = callNative(nat, c, f, n, a.toArray(new Term[n]));   // they emit their own ports
            if (r.intValue() == 1) return true;
            return backtrack(floor);
        }
        // START_CHANGE: ISS-2025-0454 - a prelude predicate must not be shadowed by the legacy
        // registry entry of the same name (maplist/N, include/3, member/2, append/3, nth0/3 ...
        // are all registered Java built-ins). Going straight to callUser also keeps the
        // context-module-first override rule of selectClauses intact.
        // START_CHANGE: ISS-2025-0466 - and neither may a definition the CALLING MODULE can see:
        // a module that defines its own partition/4 must not get the registry's.
        // START_CHANGE: ISS-2025-0493 - 4.1 wave A: the module test is asked ONLY when there is a
        // registry entry to override. A plain user predicate (app/3 in nrev, loop/1, a fact table)
        // is not registered, so "does something override the built-in?" has no meaning for it and
        // the goal goes straight to its clauses — one HashMap probe instead of the two string
        // concatenations and up to four probes overridesBuiltin costs. The registry probe is the
        // same one LegacyBuiltinAdapter.run would do as its first statement.
        if (engine.registry() != null && engine.registry().isBuiltIn(f, n)) {
            if (engine.modules4().overridesBuiltin(ctxModule, f, n)) {
                if (!callUser(c, c)) return backtrack(floor);
                return true;
            }
            int rb = LegacyBuiltinAdapter.run(this, c, f, n);
            if (rb == 1) return true;
            if (rb == 0) return backtrack(floor);
        }
        // END_CHANGE: ISS-2025-0493
        // END_CHANGE: ISS-2025-0466
        // END_CHANGE: ISS-2025-0454
        if (!callUser(c, c)) return backtrack(floor);
        return true;
    }

    /** Run a native v4 built-in with the four ports. Returns 1 = succeeded, 0 = failed. */
    private Integer callNative(Builtin nat, Term goal, String f, int n, Term[] args) {
        final int dd = debugTraceActive() ? enterPort() : -1;
        if (dd >= 0) portCall(goal, dd);
        String prev = currentContext;
        currentContext = f + "/" + n;
        // ISS-2025-0453: save/restore, because a native may run a sub-query that calls other natives
        Term prevTG = nativeTraceGoal;
        int prevTD = nativeTraceDepth;
        boolean prevHandled = nativePortsHandled;
        nativeTraceGoal = (dd >= 0) ? goal : null;
        nativeTraceDepth = dd;
        nativePortsHandled = false;
        boolean handled;
        Builtin.Outcome o;
        try {
            o = nat.call(this, args);
        } catch (PrologException pe) {
            if (dd >= 0 && !nativePortsHandled) portFail(goal, dd);
            throw pe;
        } finally {
            handled = nativePortsHandled;
            currentContext = prev;
            nativeTraceGoal = prevTG;
            nativeTraceDepth = prevTD;
            nativePortsHandled = prevHandled;
        }
        if (o == Builtin.Outcome.FAILURE) {
            if (dd >= 0 && !handled) portFail(goal, dd);
            return Integer.valueOf(0);
        }
        if (dd >= 0 && !handled) portExit(goal, dd);
        return Integer.valueOf(1);
    }

    /**
     * A choice point over the solution maps a legacy built-in returned (design B.5): one map per
     * redo, installed onto the goal's cells. Deterministic built-ins never reach here.
     */
    boolean pushSolutionChoice(final Map<String, Variable> cells, final List<Map<String, Term>> sols,
                               final Term traceGoal, final int traceDepth) {
        final Goal cont = goalStack;
        final int[] i = {0};
        CP cp = new CP(CP.GEN, B.mark());
        final Machine self = this;
        cp.gen = new Gen() {
            @Override public Goal next(CP frame) {
                while (i[0] < sols.size()) {
                    Map<String, Term> sol = sols.get(i[0]++);
                    if (i[0] >= sols.size()) frame.genExhausted = true;
                    if (!LegacyBuiltinAdapter.apply(self, cells, sol)) return FAILED;
                    if (traceGoal != null) {
                        return new Goal(new Runnable() {
                            @Override public void run() { portExit(traceGoal, traceDepth); }
                        }, cont);
                    }
                    return cont;
                }
                return EXHAUSTED;
            }
        };
        if (traceGoal != null) { cp.traceGoal = traceGoal; cp.traceDepth = traceDepth; cp.traceDebug = debugPortsActive(); }
        pushCP(cp);
        if (advance(cp)) return true;
        popCP();
        if (traceGoal != null) portFail(traceGoal, traceDepth);
        return false;
    }

    // ------------------------------------------------------------------ control constructs

    private void cut(int barrier) {
        ArrayList<Term> cleanups = null;
        while (cps.size() > barrier) {
            CP top = popCP();
            if (top.tframe != null) top.tframe.discard();      // ISS-2025-0463
            if (top.kind == CP.CLEANUP && !top.cleanupDone) {
                top.cleanupDone = true;
                if (cleanups == null) cleanups = new ArrayList<Term>();
                cleanups.add(top.cleanup);
            } else if (top.kind == CP.GEN && top.gen instanceof GeneratorGen) {
                ((GeneratorGen) top.gen).generator.cut();
            }
        }
        B.clearIfUnreachable(cps.isEmpty());
        if (cleanups != null) for (int i = 0; i < cleanups.size(); i++) runCleanup(cleanups.get(i));
    }

    /** Cut back to {@code height} without running through {@link #cut}'s cleanup collection twice. */
    private void cutTo(int height) { cut(height); }

    private void disjunction(final Term left, final Term right, final int cutBarrier) {
        final Goal cont = goalStack;
        final String mod = ctxModule;                          // ISS-2025-0466
        CP cp = new CP(CP.GEN, B.mark());
        final int[] which = {0};
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                if (which[0] == 0) { which[0] = 1; return mg(left, cutBarrier, cont, mod); }
                if (which[0] == 1) { which[0] = 2; self.genExhausted = true; return mg(right, cutBarrier, cont, mod); }
                return EXHAUSTED;
            }
        };
        pushCP(cp);
        advance(cp);
    }

    /** {@code (Cond -> Then ; Else)}: commit to Cond's first solution. A user {@code !} inside Cond
     *  is local (its barrier is ABOVE this choice point). */
    private void ite(final Term cond, final Term then, final Term els, final int cutBarrier) {
        final Goal cont = goalStack;
        final int barrier = cps.size();
        final Goal alt1 = mg(cond, barrier + 1, mg(CUT, barrier, mg(then, cutBarrier, cont)));
        final Goal alt2 = mg(els, cutBarrier, cont);
        CP cp = new CP(CP.GEN, B.mark());
        final int[] which = {0};
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                if (which[0] == 0) { which[0] = 1; return alt1; }
                if (which[0] == 1) { which[0] = 2; self.genExhausted = true; return alt2; }
                return EXHAUSTED;
            }
        };
        pushCP(cp);
        advance(cp);
    }

    // START_CHANGE: ISS-2025-0481 - wave W8 (design B.6, limit L-13): {@code once/1},
    // {@code ignore/1} and {@code forall/2} keep the inline if-then-else while tracing, and the
    // WRAPPER's own four ports are emitted here instead of by the legacy bridge.
    //
    // The Fail port needs a frame that outlives the construct's own choice point (which a commit
    // cuts away), so a port-only frame is pushed underneath: it never yields an alternative, so
    // backtracking through it emits exactly one Fail and pops it. It is deliberately the same shape
    // the bridged built-in had, which is what keeps the traces comparable.
    private void iteTraced(final Term goal, final Term cond, final Term then, final Term els,
                           final int cutBarrier) {
        final int d = enterPort();
        portCall(goal, d);
        CP portFrame = new CP(CP.GEN, B.mark());
        portFrame.gen = EXHAUSTED_GEN;
        portFrame.traceGoal = goal;
        portFrame.traceDepth = d;
        portFrame.traceDebug = debugPortsActive();
        pushCP(portFrame);

        final Goal cont = goalStack;
        final Goal exitMark = new Goal(new Runnable() {
            @Override public void run() { portExit(goal, d); }
        }, cont);
        final int barrier = cps.size();                        // above portFrame: a commit keeps it
        final Goal alt1 = mg(cond, barrier + 1, mg(CUT, barrier, mg(then, cutBarrier, exitMark)));
        final Goal alt2 = mg(els, cutBarrier, exitMark);
        CP cp = new CP(CP.GEN, B.mark());
        final int[] which = {0};
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                if (which[0] == 0) { which[0] = 1; return alt1; }
                if (which[0] == 1) { which[0] = 2; self.genExhausted = true; return alt2; }
                return EXHAUSTED;
            }
        };
        pushCP(cp);
        advance(cp);
    }

    /** A generator with no alternatives at all — a frame that exists only to own a port. */
    private static final Gen EXHAUSTED_GEN = new Gen() {
        @Override public Goal next(CP cp) { return EXHAUSTED; }
    };
    // END_CHANGE: ISS-2025-0481

    /** {@code (Cond *-> Then ; Else)}: Then for EVERY solution of Cond, Else only if there is none. */
    private void softCut(final Term cond, final Term then, final Term els, final int cutBarrier) {
        final Goal cont = goalStack;
        final boolean[] found = {false};
        final String mod = ctxModule;                          // ISS-2025-0466
        final Goal alt1 = mg(cond, cps.size() + 1, new Goal(new Runnable() {
            @Override public void run() { found[0] = true; }
        }, mg(then, cutBarrier, cont)));
        CP cp = new CP(CP.GEN, B.mark());
        final int[] which = {0};
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                if (which[0] == 0) { which[0] = 1; return alt1; }
                if (which[0] == 1) { which[0] = 2; self.genExhausted = true; return found[0] ? FAILED : mg(els, cutBarrier, cont, mod); }
                return EXHAUSTED;
            }
        };
        pushCP(cp);
        advance(cp);
    }

    /** {@code repeat}: succeed now and on every redo, forever, in O(1) memory. */
    private void repeat(final Term goal) {
        final Goal cont = goalStack;
        final boolean traced = debugTraceActive();
        final int depth = cps.size();
        if (traced) portCall(goal, depth);
        final Goal body = traced ? new Goal(new Runnable() {
            @Override public void run() { portExit(goal, depth); }
        }, cont) : cont;
        CP cp = new CP(CP.GEN, B.mark());
        cp.gen = new Gen() { @Override public Goal next(CP self) { return body; } };
        if (traced) { cp.traceGoal = goal; cp.traceDepth = depth; cp.traceDebug = debugPortsActive(); }
        pushCP(cp);
        advance(cp);
    }

    // START_CHANGE: ISS-2025-0453 - a generator that knows it is handing out its LAST solution says
    // so, and the choice point is dropped on the spot (the trust-me pop of invariant 2). Without it
    // `member(X, [a])` or `append([1],[2],L)` would leave a dead frame behind and defeat both the
    // deterministic-exit detection of setup_call_cleanup/3 and Bindings.clearIfUnreachable.
    private boolean genLastSolution;

    /** Called by a {@link Generator} from inside {@link Generator#next} when the solution it is
     *  about to return is its last one. */
    public void lastSolution() { genLastSolution = true; }

    /** Wrapper so {@link Generator#cut()} can be called when the frame is cut away. */
    private static final class GeneratorGen implements Gen {
        final Generator generator; final Goal cont; final Machine m;
        GeneratorGen(Machine m, Generator generator, Goal cont) { this.m = m; this.generator = generator; this.cont = cont; }
        @Override public Goal next(CP cp) {
            m.genLastSolution = false;
            if (!generator.next(m)) return EXHAUSTED;
            if (m.genLastSolution) cp.genExhausted = true;
            return cont;
        }
    }
    // END_CHANGE: ISS-2025-0453

    // ------------------------------------------------------------------ setup_call_cleanup (B.6)

    /**
     * {@code setup_call_cleanup(Setup, Goal, Cleanup)}: run {@code once(Setup)}, then {@code Goal},
     * and run {@code Cleanup} exactly once — on deterministic exit, on failure, when the frame is
     * cut away, or when an exception unwinds past it.
     */
    private boolean setupCallCleanup(Term setup, Term goal, Term cleanup, int cutBarrier) {
        // START_CHANGE: ISS-2025-0509 - 4.3 wave D: the three arguments are checked BEFORE Setup
        // runs. Without this, `catch(call_cleanup(A, B), E, true)` with B unbound escaped catch/3
        // entirely: Goal's instantiation_error unwound past the (now consumed) CATCH frame and the
        // cleanup's own instantiation_error was then raised with no frame left to catch it, so the
        // Java embedder saw a PrologException where the Prolog program had asked for a catch.
        // Validating up front puts the error inside the catch scope, which is also what SWI does.
        checkCallable(setup, "setup_call_cleanup/3");
        checkCallable(goal, "setup_call_cleanup/3");
        checkCallable(cleanup, "setup_call_cleanup/3");
        // END_CHANGE: ISS-2025-0509
        if (!runOnce(setup)) return false;
        final CP frame = new CP(CP.CLEANUP, B.mark());
        frame.cleanup = cleanup;
        frame.cont = goalStack;
        pushCP(frame);
        final Goal cont = goalStack;
        goalStack = mg(goal, cps.size(), new Goal(new Runnable() {
            @Override public void run() {
                // Goal exited: if the frame is on top, the call was deterministic -> cleanup now.
                if (!cps.isEmpty() && cps.get(cps.size() - 1) == frame && !frame.cleanupDone) {
                    frame.cleanupDone = true;
                    popCP();
                    runCleanup(frame.cleanup);
                }
            }
        }, cont));
        return true;
    }

    // START_CHANGE: ISS-2025-0509
    /** ISO 7.8.x: a goal argument must be bound and callable. */
    private void checkCallable(Term t, String ctx) {
        Term d = Unify.deref(t);
        if (d instanceof Variable) throw Errors.instantiation(ctx);
        if (!(d instanceof Atom) && !(d instanceof CompoundTerm)) {
            throw Errors.type("callable", resolve(d), ctx);
        }
    }
    // END_CHANGE: ISS-2025-0509

    /** Run a cleanup goal as {@code once(Cleanup)}; a failure is ignored, an exception propagates. */
    private void runCleanup(Term cleanup) {
        try {
            runOnce(cleanup);
        } catch (RuntimeException e) {
            ControlFlow.rethrowIfControl(e);
            throw e;
        }
    }

    /** Run {@code goal} to its first solution on this machine, KEEPING its bindings. */
    boolean runOnce(Term goal) {
        Goal saved = goalStack;
        String savedMod = ctxModule;                           // ISS-2025-0466
        int floor = cps.size();
        goalStack = mg(goal, floor, null);
        final boolean[] found = {false};
        try {
            drive(new Driver() {
                @Override public boolean onSolution() { found[0] = true; return false; }
            }, floor);
        } finally {
            cutTo(floor);
            goalStack = saved;
            ctxModule = savedMod;
        }
        return found[0];
    }

    /**
     * Run {@code goal} as an independent sub-query on THIS machine (shared trail, choice-point
     * floor and resource guard), reporting each solution to {@code sink}; all bindings are undone
     * afterwards. This is what {@link SolverFacade} gives the legacy {@code BuiltInWithContext}
     * built-ins in place of a recursive sub-solve (design B.5).
     */
    boolean runSubQuery(Term goal, final SolutionSink sink) {
        LinkedHashMap<String, Variable> vars = new LinkedHashMap<String, Variable>();
        Term g = normalise(goal, vars);
        final List<String> names = new ArrayList<String>(vars.keySet());
        final List<Variable> cells = new ArrayList<Variable>(vars.values());
        Goal saved = goalStack;
        String savedMod = ctxModule;                           // ISS-2025-0466
        int floor = cps.size();
        int m = B.mark();
        final boolean[] any = {false};
        goalStack = mg(g, floor, null);
        B.forceTrail++;
        try {
            drive(new Driver() {
                @Override public boolean onSolution() {
                    any[0] = true;
                    return sink.onSolution(snapshot(names, cells));
                }
            }, floor);
        } finally {
            // START_CHANGE: ISS-2025-0448 - the undo MUST happen while the forced-trail extent is
            // still open. cutTo() ends in Bindings.clearIfUnreachable(), which drops the whole
            // trail when forceTrail == 0 and no choice point is left — so decrementing first threw
            // away exactly the entries this undo needs, and the sub-query's bindings survived it.
            cutTo(floor);
            B.undo(m);
            B.forceTrail--;
            // END_CHANGE: ISS-2025-0448
            goalStack = saved;
            ctxModule = savedMod;
        }
        return any[0];
    }

    // ------------------------------------------------------------------ generators

    /** {@code between(+Low, +High, -Value)} as a lazy choice point. 1 ok / 0 fail / -1 not handled. */
    private int betweenNative(Term goal, List<Term> a) {
        Term lo = Unify.deref(a.get(0)), hi = Unify.deref(a.get(1)), v = Unify.deref(a.get(2));
        // START_CHANGE: ISS-2025-0509 - 4.3 wave D: between/3's argument contract, checked in the
        // ONE place every mode passes through. An unbound bound is instantiation_error and a
        // non-integer bound (or a non-integer third argument) is type_error(integer, N); all of
        // them used to fall through to the registry version, which failed silently.
        if (lo instanceof Variable || hi instanceof Variable) throw Errors.instantiation("between/3");
        if (!(lo instanceof Number) || !((Number) lo).isInteger()) {
            throw Errors.type("integer", resolve(lo), "between/3");
        }
        if (!(hi instanceof Number) || !((Number) hi).isInteger()) {
            boolean inf = (hi instanceof Atom)
                && ("inf".equals(((Atom) hi).getName()) || "infinite".equals(((Atom) hi).getName()));
            if (!inf) throw Errors.type("integer", resolve(hi), "between/3");
        }
        if (!(v instanceof Variable) && (!(v instanceof Number) || !((Number) v).isInteger())) {
            throw Errors.type("integer", resolve(v), "between/3");
        }
        // END_CHANGE: ISS-2025-0509
        if (!(v instanceof Variable)) return -1;
        if (!(lo instanceof Number) || !((Number) lo).isInteger() || !((Number) lo).fitsInLong()) return -1;
        long low = ((Number) lo).longValue();
        long high;
        if (hi instanceof Atom) {
            String hn = ((Atom) hi).getName();
            if (!"inf".equals(hn) && !"infinite".equals(hn)) return -1;
            high = Long.MAX_VALUE;
        } else if (hi instanceof Number && ((Number) hi).isInteger() && ((Number) hi).fitsInLong()) {
            high = ((Number) hi).longValue();
        } else {
            return -1;
        }
        // START_CHANGE: ISS-2025-0481 - committed to the native generator: from here the four
        // ports are the machine's, so between/3 no longer has to be routed through the bridge while
        // tracing (limit L-13). Same shape as lengthEnumerate below.
        final boolean traced = debugTraceActive();
        final int depth = traced ? enterPort() : -1;
        if (traced) portCall(goal, depth);
        if (low > high) { if (traced) portFail(goal, depth); return 0; }
        final Term value = v;
        final Goal cont = goalStack;
        final Goal after = traced ? new Goal(new Runnable() {
            @Override public void run() { portExit(goal, depth); }
        }, cont) : cont;
        final long last = high;
        final long[] next = {low};
        CP cp = new CP(CP.GEN, B.mark());
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                if (next[0] > last) return EXHAUSTED;
                long i = next[0]++;
                if (next[0] > last) self.genExhausted = true;
                return Unify.unify(value, Number.valueOf(i), B) ? after : FAILED;
            }
        };
        if (traced) { cp.traceGoal = goal; cp.traceDepth = depth; cp.traceDebug = debugPortsActive(); }
        pushCP(cp);
        if (advance(cp)) return 1;
        popCP();
        if (traced) portFail(goal, depth);
        return 0;
        // END_CHANGE: ISS-2025-0481
    }

    /** {@code length(PartialList, Var)}: enumerate lengths as a lazy infinite choice point. */
    private boolean lengthEnumerate(final Term goal, List<Term> a) {
        Term lenT = Unify.deref(a.get(1));
        if (!(lenT instanceof Variable)) return false;
        Term cur = Unify.deref(a.get(0));
        int prefix = 0;
        IdentityHashMap<Term, Boolean> seen = new IdentityHashMap<Term, Boolean>();
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            if (seen.put(cur, Boolean.TRUE) != null) return false;
            prefix++;
            cur = Unify.deref(((CompoundTerm) cur).getArguments().get(1));
        }
        if (!(cur instanceof Variable)) return false;
        if (cur == lenT) return false;
        final Term tail = cur, lenVar = lenT;
        final int base = prefix;
        final Goal cont = goalStack;
        final boolean traced = debugTraceActive();
        final int depth = traced ? enterPort() : -1;
        if (traced) portCall(goal, depth);
        final Goal after = traced ? new Goal(new Runnable() {
            @Override public void run() { portExit(goal, depth); }
        }, cont) : cont;
        final int[] extra = {0};
        CP cp = new CP(CP.GEN, B.mark());
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                int k = extra[0]++;
                Term list = NIL;
                for (int i = k - 1; i >= 0; i--) {
                    list = new CompoundTerm(DOT, Arrays.asList((Term) new Variable(), list));
                }
                if (!Unify.unify(tail, list, B)) return FAILED;
                if (!Unify.unify(lenVar, Number.valueOf(base + k), B)) return FAILED;
                return after;
            }
        };
        if (traced) { cp.traceGoal = goal; cp.traceDepth = depth; cp.traceDebug = debugPortsActive(); }
        pushCP(cp);
        advance(cp);
        return true;
    }

    // ------------------------------------------------------------------ inline built-ins

    // START_CHANGE: ISS-2025-0481 - which indicators {@link #solveBuiltin} handles. It has to be
    // answerable BEFORE the built-in runs, because the Call port must precede the work; keep it in
    // step with solveBuiltin below.
    /** True when {@link #solveBuiltin} will handle {@code f/n} (so its ports are the machine's). */
    private static boolean isInlineBuiltin(String f, int n) {
        if (n == 2) {
            return "is".equals(f) || "<".equals(f) || ">".equals(f) || "=<".equals(f) || ">=".equals(f)
                || "=:=".equals(f) || "=\\=".equals(f) || "==".equals(f) || "\\==".equals(f)
                || "@<".equals(f) || "@>".equals(f) || "@=<".equals(f) || "@>=".equals(f)
                || "\\=".equals(f);
        }
        if (n == 1) {
            return "var".equals(f) || "nonvar".equals(f) || "atom".equals(f) || "atomic".equals(f)
                || "number".equals(f) || "integer".equals(f) || "float".equals(f)
                || "compound".equals(f) || "callable".equals(f);
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0481

    /** Deterministic inline built-ins: 1 = succeeded, 0 = failed, -1 = not handled here. */
    private int solveBuiltin(String f, List<Term> a, int n) {
        if (n == 2) {
            if ("is".equals(f)) return Unify.unify(a.get(0), evalNum(a.get(1)), B) ? 1 : 0;
            if ("<".equals(f) || ">".equals(f) || "=<".equals(f) || ">=".equals(f)
                    || "=:=".equals(f) || "=\\=".equals(f)) {
                return numRel(f, evalNum(a.get(0)), evalNum(a.get(1))) ? 1 : 0;
            }
            if ("==".equals(f)) return Unify.equalTerms(a.get(0), a.get(1), guard) ? 1 : 0;
            if ("\\==".equals(f)) return Unify.equalTerms(a.get(0), a.get(1), guard) ? 0 : 1;
            if ("@<".equals(f)) return Unify.compareTerms(a.get(0), a.get(1), guard) < 0 ? 1 : 0;
            if ("@>".equals(f)) return Unify.compareTerms(a.get(0), a.get(1), guard) > 0 ? 1 : 0;
            if ("@=<".equals(f)) return Unify.compareTerms(a.get(0), a.get(1), guard) <= 0 ? 1 : 0;
            if ("@>=".equals(f)) return Unify.compareTerms(a.get(0), a.get(1), guard) >= 0 ? 1 : 0;
            if ("\\=".equals(f)) {
                int m = B.mark();
                B.forceTrail++;
                boolean u;
                // ISS-2025-0448: undo inside the extent, then close it (see findAll)
                try { u = Unify.unify(a.get(0), a.get(1), B); } finally { B.undo(m); B.forceTrail--; }
                return u ? 0 : 1;
            }
            return -1;
        }
        if (n == 1) {
            Term x = Unify.deref(a.get(0));
            if ("var".equals(f)) return x instanceof Variable ? 1 : 0;
            if ("nonvar".equals(f)) return x instanceof Variable ? 0 : 1;
            if ("atom".equals(f)) return x instanceof Atom ? 1 : 0;
            if ("atomic".equals(f)) return (x instanceof Atom || x instanceof Number || x instanceof PrologString) ? 1 : 0;
            if ("number".equals(f)) return x instanceof Number ? 1 : 0;
            if ("integer".equals(f)) return (x instanceof Number && ((Number) x).isInteger()) ? 1 : 0;
            if ("float".equals(f)) return (x instanceof Number && !((Number) x).isInteger()) ? 1 : 0;
            if ("compound".equals(f)) return x instanceof CompoundTerm ? 1 : 0;
            if ("callable".equals(f)) return (x instanceof Atom || x instanceof CompoundTerm) ? 1 : 0;
            return -1;
        }
        return -1;
    }

    private final java.util.function.UnaryOperator<Term> derefFn = new java.util.function.UnaryOperator<Term>() {
        @Override public Term apply(Term t) { return Unify.deref(t); }
    };

    Number evalNum(Term t) {
        return it.denzosoft.jprolog.core.arith.v2.ArithEvaluator.evalDeref(t, derefFn);
    }

    private boolean numRel(String op, Number a, Number b) {
        if (a.isInteger() && b.isInteger()) {
            if (a.fitsInLong() && b.fitsInLong()) {
                long x = a.longValue(), y = b.longValue();
                if ("<".equals(op)) return x < y;
                if (">".equals(op)) return x > y;
                if ("=<".equals(op)) return x <= y;
                if (">=".equals(op)) return x >= y;
                if ("=:=".equals(op)) return x == y;
                return x != y;
            }
            int c = a.bigIntegerValue().compareTo(b.bigIntegerValue());
            if ("<".equals(op)) return c < 0;
            if (">".equals(op)) return c > 0;
            if ("=<".equals(op)) return c <= 0;
            if (">=".equals(op)) return c >= 0;
            if ("=:=".equals(op)) return c == 0;
            return c != 0;
        }
        double x = a.doubleValue(), y = b.doubleValue();
        if ("<".equals(op)) return x < y;
        if (">".equals(op)) return x > y;
        if ("=<".equals(op)) return x <= y;
        if (">=".equals(op)) return x >= y;
        if ("=:=".equals(op)) return x == y;
        return x != y;
    }

    // ------------------------------------------------------------------ findall / catch

    List<Term> findAll(final Term template, Term goal) {
        Goal savedGoals = goalStack;
        String savedMod = ctxModule;                           // ISS-2025-0466
        int floor = cps.size();
        int m = B.mark();
        final List<Term> results = new ArrayList<Term>();
        goalStack = mg(goal, floor, null);
        B.forceTrail++;
        try {
            drive(new Driver() {
                @Override public boolean onSolution() {
                    results.add(Unify.copy(template, new IdentityHashMap<Variable, Variable>(), guard));
                    return true;
                }
            }, floor);
        } finally {
            // START_CHANGE: ISS-2025-0448 - findall/3 is OPAQUE: none of Goal's bindings may
            // survive it. Undo BEFORE closing the forced-trail extent — cutTo() ends in
            // Bindings.clearIfUnreachable(), which wipes the trail once forceTrail is back to 0 and
            // no choice point is left, so the old order left the template variable bound to the
            // LAST solution (`findall(X, member(X,[1,2]), L), X == 2` succeeded).
            cutTo(floor);
            B.undo(m);
            B.forceTrail--;
            // END_CHANGE: ISS-2025-0448
            goalStack = savedGoals;
            ctxModule = savedMod;
        }
        return results;
    }

    /**
     * Route a thrown ball to the nearest armed catch frame at or above {@code floor}.
     *
     * <p>START_CHANGE: ISS-2025-0513 - a cleanup collected while a ball unwinds is run AT THE
     * POINT ITS FRAME IS POPPED, not after the search has finished. It used to be collected into a
     * list and run once a matching CATCH frame had been found, popped and its recovery installed —
     * so the cleanup executed with the frame that should have caught it already consumed, and its
     * own exception propagated out of {@code handleBall}, which {@link #drive} calls from inside
     * its {@code catch} clause and therefore cannot route. {@code catch(call_cleanup(throw(a),
     * throw(b)), E, true)} reached the Java embedder as an uncaught `PrologException: b`.
     *
     * <p>Running it here fixes that for free: the enclosing frames are still on the stack, so a
     * ball the cleanup throws simply <b>replaces</b> the one being unwound and the search
     * continues from the same position. That is SWI's answer (the cleanup's exception wins, and it
     * is tested against the catchers that enclose the {@code setup_call_cleanup/3}, not against
     * the one that matched the original ball), and it makes this path agree with
     * {@link #backtrack}, which has always run a cleanup at pop time.
     *
     * <p>Two consequences worth naming. A later cleanup still runs when an earlier one throws
     * (nested {@code setup_call_cleanup/3}: the outermost cleanup's ball is the one that survives).
     * And when no catcher matches, a REPLACED ball has to be thrown from here — {@code drive}
     * rethrows the original exception object when this method answers false, which would report
     * the goal's ball rather than the cleanup's. END_CHANGE: ISS-2025-0513
     */
    private boolean handleBall(Term ball, int floor) {
        boolean replaced = false;                              // ISS-2025-0513
        while (cps.size() > floor) {
            CP top = popCP();
            if (top.tframe != null) top.tframe.discard();      // ISS-2025-0463
            if (top.kind == CP.CLEANUP && !top.cleanupDone) {
                top.cleanupDone = true;
                // START_CHANGE: ISS-2025-0513
                Term thrown = runCleanupWhileUnwinding(top.cleanup);
                if (thrown != null) { ball = thrown; replaced = true; }
                // END_CHANGE: ISS-2025-0513
                continue;
            }
            if (top.kind != CP.CATCH || !top.active) continue;
            B.undo(top.trailMark);                             // ISS-2025-0492: one trail
            int m = B.mark();
            B.forceTrail++;
            boolean matched;
            // ISS-2025-0448: a non-matching catcher must leave NO binding behind; undo inside the
            // extent (see findAll) and only then close it.
            try {
                matched = Unify.unify(top.catcher, ball, B);
                if (!matched) B.undo(m);
            } finally {
                B.forceTrail--;
            }
            if (matched) {
                goalStack = mg(top.recovery, top.cutBarrier, top.cont, top.module);
                return true;
            }
        }
        // START_CHANGE: ISS-2025-0513 - nothing caught it. If a cleanup replaced the ball the
        // caller must see the NEW one, and drive() would otherwise rethrow the original object.
        if (replaced) throw new PrologException(ball);
        // END_CHANGE: ISS-2025-0513
        return false;
    }

    // START_CHANGE: ISS-2025-0513
    /**
     * Run a cleanup goal that was reached by an unwinding ball. Answers the cleanup's OWN ball when
     * it throws one — the caller then unwinds that instead — and null when it does not.
     *
     * <p>The trust model is preserved: {@code InferenceLimitException}, {@code QueryCancelledException}
     * and {@code DebugStopException} are not {@code PrologException}s and are not caught here, so a
     * budget abort or a Stop during a cleanup still tears the query down and stays invisible to
     * {@code catch/3}. Neither is {@code halt/0,1}, whose {@code PrologException} carries no ball.
     */
    private Term runCleanupWhileUnwinding(Term cleanup) {
        try {
            runCleanup(cleanup);
            return null;
        } catch (PrologException pe) {
            ControlFlow.rethrowIfControl(pe);
            Term b = pe.getErrorTerm();
            if (b == null || pe.isHalt()) throw pe;            // halt/1 is not a ball
            return Unify.copy(b, new IdentityHashMap<Variable, Variable>(), guard);
        }
    }
    // END_CHANGE: ISS-2025-0513

    static Term makeList(List<Term> elems) {
        Term list = NIL;
        for (int i = elems.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(DOT, Arrays.asList(elems.get(i), list));
        }
        return list;
    }

    Term addArgs(Term goal, List<Term> extra) {
        if (goal instanceof Atom) return new CompoundTerm((Atom) goal, new ArrayList<Term>(extra));
        if (goal instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) goal;
            // START_CHANGE: ISS-2025-0469 - call(M:G, X) is M:call(G, X), never ':'(M, G, X).
            // This is what carries a meta-argument's context module into the callee.
            if ((":".equals(c.getName()) || Modules.MCTX.equals(c.getName()))
                    && c.getArguments().size() == 2) {
                return new CompoundTerm(c.getFunctor(), Arrays.asList(
                    c.getArguments().get(0), addArgs(Unify.deref(c.getArguments().get(1)), extra)));
            }
            // END_CHANGE: ISS-2025-0469
            List<Term> args = new ArrayList<Term>(c.getArguments());
            args.addAll(extra);
            return new CompoundTerm(c.getFunctor(), args);
        }
        return goal;
    }


    // START_CHANGE: ISS-2025-0466 - wave W6, design B.10. Clause selection is module-aware:
    // M -> M's imports -> user (THE FLAT STORE) -> autoload libraries. The `modules.size() > 1`
    // special case, which used to divert every unqualified call through ModuleManager the moment a
    // second module existed, is GONE — `user` simply is the flat store.
    private boolean callUser(Term unifyGoal, Term lookup) { return callUser(unifyGoal, lookup, false); }

    // `flat` skips the module resolution so a Module:Goal term can be called as the plain :/2
    // predicate JProlog actually stores it under (the attr_unify_hook dispatch of design B.9).
    private boolean callUser(Term unifyGoal, Term lookup, boolean flat) {
        Term g0 = Unify.deref(unifyGoal);
        if (engine.tables() != null && isTabled(g0)) {
            return callTabled(unifyGoal, lookup, g0);      // ISS-2025-0463
        }
        Clause[] clauses = selectClauses(lookup, g0, flat);
        return activate(withMetaContext(g0, unifyGoal, selModule), unifyGoal,
                        clauses, selLimit, selModule, lookup);
    }

    // START_CHANGE: ISS-2025-0469 - meta_predicate/1 (design B.10). When a predicate declared
    // `meta_predicate p(0, +, ...)` in module M is called from module C, its module-sensitive
    // arguments are qualified with C before the head is unified, so the callee's `call/N` runs
    // them in the CALLER's context. Only a non-`user` defining module can carry a declaration that
    // changes anything, so a plain single-module program never even does the lookup.
    /** The goal the clause heads must unify with: {@code goal} with its meta-arguments qualified. */
    private Term withMetaContext(Term goal, Term fallback, String defMod) {
        if (defMod == null) return fallback;
        String f;
        int n;
        if (goal instanceof CompoundTerm) {
            f = ((CompoundTerm) goal).getName();
            n = ((CompoundTerm) goal).getArguments().size();
        } else {
            return fallback;
        }
        int[] spec = engine.modules4().metaSpec(defMod, f, n);
        if (spec == null) return fallback;
        return Modules.qualifyMetaArgs(goal, spec, contextModule());
    }
    // END_CHANGE: ISS-2025-0469

    /** Call a goal against a clause list that has already been resolved (the {@code Module:Goal}
     *  path): {@code unifyGoal} is the UNQUALIFIED goal the heads unify with, {@code traceGoal} the
     *  qualified one the four ports report. */
    private boolean callInModule(Term unifyGoal, Term traceGoal, Clause[] cs, String defMod) {
        Term g0 = Unify.deref(unifyGoal);
        if (engine.tables() != null && isTabled(g0)) return callTabled(unifyGoal, unifyGoal, g0);
        String dm = modKey(defMod);
        return activate(withMetaContext(g0, unifyGoal, dm), traceGoal, cs, cs.length, dm, null);
    }

    /**
     * Install the clause frame: the four ports, the choice point and the clause bodies, whose
     * goals run in {@code defMod} — that is what makes a library predicate's {@code call/N} and a
     * module predicate's helpers resolve in the right place.
     *
     * @param lookupForUnknown non-null to apply the {@code unknown} flag when there is no procedure
     */
    private boolean activate(Term unifyGoal, Term traceGoal, Clause[] clauses, final int limit,
                             final String defMod, Term lookupForUnknown) {
        Term g0 = Unify.deref(unifyGoal);
        if (it.denzosoft.jprolog.core.engine.Profiler.isEnabled()) {
            if (g0 instanceof Atom) it.denzosoft.jprolog.core.engine.Profiler.recordCall(((Atom) g0).getName(), 0);
            else if (g0 instanceof CompoundTerm) it.denzosoft.jprolog.core.engine.Profiler.recordCall(
                ((CompoundTerm) g0).getName(), ((CompoundTerm) g0).getArguments().size());
        }
        final boolean tracing = it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled();
        final boolean debugging = debugPortsActive();
        final int tdepth = (tracing || debugging) ? enterPort() : 0;   // ISS-2025-0482
        final Term g = traceGoal;
        if (tracing) tracePort("Call", g, tdepth);
        if (debugging) debugPort(DebugEvent.Port.CALL, g, tdepth);

        if (clauses == null || limit == 0) {
            if (clauses == null && lookupForUnknown != null) raiseUnknownIfRequired(lookupForUnknown);
            if (tracing || debugging) portDepth = tdepth;          // ISS-2025-0482
            if (tracing) tracePort("Fail", g, tdepth);
            if (debugging) debugPort(DebugEvent.Port.FAIL, g, tdepth);
            return false;
        }
        final Goal cont = goalStack;
        final int barrier = cps.size();
        final boolean ftrace = tracing, fdebug = debugging;
        CP cp = new CP(CP.CLAUSES, B.mark());
        cp.goal = unifyGoal;
        cp.clauses = clauses;
        cp.limit = limit;
        cp.idx = 0;
        cp.generation = engine.store().generation();
        cp.cont = cont;
        cp.barrier = barrier;
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                while (self.idx < self.limit) {
                    Clause cl = self.clauses[self.idx++];
                    if (!cl.isAlive(self.generation)) continue;          // logical update view
                    if (self.idx >= self.limit) self.genExhausted = true;
                    Term[] frame = (cl.nvars == 0) ? LegacyBuiltinAdapter.NO_FRAME : new Term[cl.nvars];
                    if (!cl.unifyHead(self.goal, frame, B)) return FAILED;
                    Goal after = self.cont;
                    if (ftrace || fdebug) {
                        after = new Goal(new Runnable() {
                            @Override public void run() {
                                portDepth = tdepth;             // ISS-2025-0482
                                if (ftrace) tracePort("Exit", g, tdepth);
                                if (fdebug) debugPort(DebugEvent.Port.EXIT, g, tdepth);
                            }
                        }, self.cont);
                    }
                    Term[] body = cl.body;
                    for (int i = body.length - 1; i >= 0; i--) {
                        Goal bg = new Goal(body[i], frame, self.barrier, after);
                        bg.module = defMod;                              // ISS-2025-0466
                        after = bg;
                    }
                    return after;
                }
                return EXHAUSTED;
            }
        };
        if (tracing || debugging) { cp.traceGoal = g; cp.traceDepth = tdepth; cp.traceDebug = debugging; }
        pushCP(cp);
        if (advance(cp)) return true;
        popCP();
        if (tracing || debugging) portDepth = tdepth;               // ISS-2025-0482
        if (tracing) tracePort("Fail", g, tdepth);
        if (debugging) debugPort(DebugEvent.Port.FAIL, g, tdepth);
        return false;
    }

    /**
     * {@code Module:Goal} (design B.10). The innermost qualification wins, so {@code a:b:goal} runs
     * in {@code b}; {@code system:G} is a built-in dispatch; {@code user:G} and an unknown module
     * are ordinary resolution in that context; and a known module answers only with what it makes
     * visible from outside — the export enforcement of ISS-2025-0314.
     */
    private boolean qualifiedCall(CompoundTerm qc) {
        Term mt = Unify.deref(qc.getArguments().get(0));
        Term inner = Unify.deref(qc.getArguments().get(1));
        while (inner instanceof CompoundTerm && ":".equals(((CompoundTerm) inner).getName())
                && ((CompoundTerm) inner).getArguments().size() == 2) {
            CompoundTerm nested = (CompoundTerm) inner;
            mt = Unify.deref(nested.getArguments().get(0));
            inner = Unify.deref(nested.getArguments().get(1));
        }
        if (!(mt instanceof Atom) || !(inner instanceof Atom || inner instanceof CompoundTerm)) {
            goalStack = mg(inner, cps.size(), goalStack);      // let the drive loop raise the error
            return true;
        }
        final String mod = ((Atom) mt).getName();
        final String name;
        final int n;
        if (inner instanceof Atom) { name = ((Atom) inner).getName(); n = 0; }
        else { name = ((CompoundTerm) inner).getName(); n = ((CompoundTerm) inner).getArguments().size(); }

        Modules ms = engine.modules4();
        if (Modules.SYSTEM.equals(mod)) {
            int r = callSystem(inner, name, n);
            if (r >= 0) return r == 1;
            goalStack = mg(inner, cps.size(), goalStack, null);   // control constructs, call/N, ...
            return true;
        }
        if (Modules.USER.equals(mod) || !ms.isModule(mod)) {
            goalStack = mg(inner, cps.size(), goalStack, modKey(mod));
            return true;
        }
        Clause[] own = ms.localClauses(mod, name, n,
            (inner instanceof CompoundTerm && n > 0)
                ? Clause.argKey(Unify.deref(((CompoundTerm) inner).getArguments().get(0))) : null);
        if (own != null && own.length > 0) {
            // The module DEFINES it, so export visibility decides (ISS-2025-0314).
            if (ms.exports(mod, name, n)) return callInModule(inner, qc, own, mod);
            return flatQualified(qc);
        }
        // The module does not define it: ordinary resolution IN its context —
        // M's imports -> user -> autoload -> system. That is what makes `lists:length/2`
        // (a native), `other:base/1` (a `user` predicate) and `m:maplist/3` all work, without
        // weakening the export check above.
        if (flatQualified(qc)) return true;
        goalStack = mg(inner, cps.size(), goalStack, modKey(mod));
        return true;
    }

    /** A module-qualified CLAUSE HEAD (`m:h(X) :- ...` — how JProlog stores
     *  {@code Module:attr_unify_hook/2}); the {@code :/2} predicate is empty in every program that
     *  writes none, which is the fast path. */
    private boolean flatQualified(CompoundTerm qc) {
        if (engine.store().lookup(":", 2).size() == 0) return false;
        return callUser(qc, qc, true);
    }

    /** Run {@code goal} as a {@code system} built-in: 1 = succeeded, 0 = failed, -1 = not one. */
    private int callSystem(Term goal, String f, int n) {
        List<Term> a = (goal instanceof CompoundTerm)
            ? ((CompoundTerm) goal).getArguments() : java.util.Collections.<Term>emptyList();
        // ISS-2025-0481: the inline table is never skipped now; the ports come from the machine
        if (n > 0 && !debugTraceActive()) {
            int r = solveBuiltin(f, a, n);
            if (r >= 0) return r;
        } else if (n > 0 && isInlineBuiltin(f, n)) {
            final int d = enterPort();
            portCall(goal, d);
            int r = solveBuiltin(f, a, n);
            if (r == 1) portExit(goal, d);
            else if (r == 0) portFail(goal, d);
            if (r >= 0) return r;
        }
        Builtin nat = engine.natives().lookup(f, n);
        if (nat != null) {
            Term[] args = (n == 0) ? LegacyBuiltinAdapter.NO_ARGS : a.toArray(new Term[n]);
            return callNative(nat, goal, f, n, args).intValue();
        }
        return LegacyBuiltinAdapter.run(this, goal, f, n);
    }

    /**
     * Candidate clauses for {@code lookup}. Returns {@code null} when the procedure is UNKNOWN (no
     * clauses anywhere the context can see) and an empty array when the predicate exists but the
     * first-argument index excluded every clause (a plain failure).
     */
    /** Number of usable entries in the array {@link #selectClauses} last returned. */
    private int selLimit;
    /** The module the bodies of those clauses run in ({@code null} == {@code user}). */
    private String selModule;

    private Clause[] selectClauses(Term lookup, Term goal, boolean flat) {
        String f;
        int ar;
        Object key = null;
        if (goal instanceof Atom) { f = ((Atom) goal).getName(); ar = 0; }
        else if (goal instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) goal;
            f = c.getName();
            ar = c.getArguments().size();
            if (ar > 0) key = Clause.argKey(Unify.deref(c.getArguments().get(0)));
        } else {
            selLimit = 0;
            selModule = null;
            return null;
        }
        selModule = null;
        Modules ms = flat ? null : engine.modules4();

        // 1./2. the context module and its imports (skipped entirely for the `user` context,
        //       which is the overwhelmingly common case and costs one null test)
        if (ms != null && ctxModule != null) {
            Clause[] own = ms.localClauses(ctxModule, f, ar, key);
            if (own != null && own.length > 0) { selLimit = own.length; selModule = ctxModule; return own; }
            Modules.Hit h = ms.fromImports(ctxModule, f, ar, key);
            if (h != null) { selLimit = h.clauses.length; selModule = modKey(h.module); return h.clauses; }
        }

        // 3. `user` — the flat clause store, with the first-argument index
        ClauseStore.Predicate p = engine.store().lookup(f, ar);
        int n = p.size();
        if (n > 0) {
            if (key == null) { selLimit = n; return p.rawArray(); }  // no copy: see rawArray's contract
            Clause[] sel = p.select(key);
            selLimit = sel.length;
            return sel;
        }

        if (ms != null) {
            if (ctxModule == null) {                        // `user`'s own use_module/1 imports
                Modules.Hit h = ms.fromImports(Modules.USER, f, ar, key);
                if (h != null) { selLimit = h.clauses.length; selModule = modKey(h.module); return h.clauses; }
            }
            // 4. autoload: the library module that exports f/ar, parsed on first reference
            Modules.Hit lib = ms.autoload(f, ar, key);
            if (lib != null) { selLimit = lib.clauses.length; selModule = modKey(lib.module); return lib.clauses; }
        }
        selLimit = 0;
        return null;                                         // unknown procedure
    }
    // END_CHANGE: ISS-2025-0466

    private void raiseUnknownIfRequired(Term lookup) {
        Term g = Unify.deref(lookup);
        String f;
        int ar;
        if (g instanceof Atom) { f = ((Atom) g).getName(); ar = 0; }
        else if (g instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) g;
            if (":".equals(c.getName()) && c.getArguments().size() == 2) return;
            f = c.getName();
            ar = c.getArguments().size();
        } else {
            return;
        }
        // ISS-2025-0466: the `more than one module exists -> never raise` escape hatch is gone.
        // Resolution is now exact (context module, its imports, user, autoload), so an unknown
        // procedure really is unknown and the ISO `unknown` flag applies as it does without
        // modules. A predicate the context module CAN see never reaches this method.
        if (engine.kb().isDynamic(f, ar)) return;
        Term mode = it.denzosoft.jprolog.core.system.PrologFlags.getFlag("unknown");
        String m = (mode instanceof Atom) ? ((Atom) mode).getName() : "error";
        if ("fail".equals(m)) return;
        Term pi = new CompoundTerm(new Atom("/"), Arrays.asList((Term) new Atom(f), (Term) Number.valueOf(ar)));
        if ("warning".equals(m)) {
            it.denzosoft.jprolog.builtin.io.StreamManager.out().println("Warning: unknown procedure " + f + "/" + ar);
            return;
        }
        throw Errors.existence("procedure", pi, f + "/" + ar);
    }

    private boolean isTabled(Term goal) {
        if (goal instanceof Atom) return engine.tables().isTabled(((Atom) goal).getName(), 0);
        if (goal instanceof CompoundTerm) {
            return engine.tables().isTabled(((CompoundTerm) goal).getName(),
                ((CompoundTerm) goal).getArguments().size());
        }
        return false;
    }

    // START_CHANGE: ISS-2025-0463, ISS-2025-0465 - engine v4 wave W5, design B.8: linear tabling
    // with completion, and the DELETION of `tabledDelegate` (ISS-2025-0465).
    // The W1-W4 `tabledDelegate` handed the resolved goal to the recursive legacy solver, which
    // implemented the bounded 100-iteration re-evaluation of limit L-03 (wrong answers) and dragged
    // the 2 000-deep Java recursion, name-keyed answers and a fixpoint the budget could not see
    // along with it. A tabled call is now an ordinary choice point on THIS machine: see
    // {@link Tabling.TableFrame}.
    /**
     * Call a tabled predicate: install its generator (first call to a variant, or a re-execution in
     * a new completion round) or consumer (a variant that is EVALUATING or COMPLETE) choice point.
     */
    private boolean callTabled(Term unifyGoal, Term lookup, Term g0) {
        // ISS-2025-0488 (LIM-039): the whole decision below — does the variant exist, is it
        // COMPLETE, do I produce it — plus the frame it installs must be atomic against other
        // threads on this engine, and an evaluation this call STARTS keeps the claim until its SCC
        // completes. `exitCall` gives it back when nothing is left evaluating.
        engine.tabling().enterCall();
        try {
            return callTabledClaimed(unifyGoal, lookup, g0);
        } finally {
            engine.tabling().exitCall();
        }
    }

    private boolean callTabledClaimed(Term unifyGoal, Term lookup, Term g0) {
        final Tabling tb = engine.tabling();
        final boolean tracing = it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled();
        final boolean debugging = debugPortsActive();
        final int tdepth = (tracing || debugging) ? enterPort() : 0;   // ISS-2025-0482
        if (tracing) tracePort("Call", unifyGoal, tdepth);
        if (debugging) debugPort(DebugEvent.Port.CALL, unifyGoal, tdepth);

        String key = Tabling.variantKey(g0, guard);
        Tabling.Table table = tb.get(key);
        boolean produce;
        if (table == null) {
            produce = true;
        } else if (table.status == Tabling.COMPLETE) {
            produce = false;
        } else {
            tb.noteIncompleteRead(table);
            produce = !table.producing && table.producedRound < tb.round();
        }

        Clause[] clauses = NO_CLAUSES;
        int limit = 0;
        Term prod = null;
        if (produce) {
            // The production template is a private copy of the call: the clauses are run against
            // it, so the CALLER's goal is untouched until an answer is handed back, and an answer
            // is just a copy of the template. It is created BEFORE the choice point exists, so its
            // cells are older than the barrier and every binding to them is trailed (invariant 1).
            prod = Unify.copy(table != null ? table.template : g0,
                              new IdentityHashMap<Variable, Variable>(), guard);
            clauses = selectClauses(lookup, prod, false);
            if (clauses == null) {
                raiseUnknownIfRequired(lookup);
                if (tracing || debugging) portDepth = tdepth;   // ISS-2025-0482
                if (tracing) tracePort("Fail", unifyGoal, tdepth);
                if (debugging) debugPort(DebugEvent.Port.FAIL, unifyGoal, tdepth);
                return false;
            }
            limit = selLimit;
            if (table == null) {
                table = tb.create(key, Tabling.indicatorOf(g0),
                    Unify.copy(g0, new IdentityHashMap<Variable, Variable>(), guard));
            }
        }

        Tabling.TableFrame frame = new Tabling.TableFrame(this, tb, table, unifyGoal, goalStack,
            produce, prod, clauses, limit, engine.store().generation(),
            (tracing || debugging) ? unifyGoal : null, tdepth);
        frame.defModule = produce ? selModule : null;            // ISS-2025-0466
        CP cp = new CP(CP.GEN, B.mark());
        cp.gen = frame;
        cp.tframe = frame;
        if (tracing || debugging) { cp.traceGoal = unifyGoal; cp.traceDepth = tdepth; cp.traceDebug = debugging; }
        pushCP(cp);
        // A `!` in a tabled clause body is LOCAL to that body: the barrier is ABOVE this frame, so
        // a cut can never discard the generator and abort the production of the table.
        frame.bodyBarrier = cps.size();
        if (advance(cp)) return true;
        popCP();
        B.clearIfUnreachable(cps.isEmpty());
        if (tracing || debugging) portDepth = tdepth;           // ISS-2025-0482
        if (tracing) tracePort("Fail", unifyGoal, tdepth);
        if (debugging) debugPort(DebugEvent.Port.FAIL, unifyGoal, tdepth);
        return false;
    }

    /** Activate {@code cl} against {@code goal}: unify the head into a fresh frame and push the
     *  body in front of {@code after}. Null when the head does not match. */
    Goal buildClauseBody(Clause cl, Term goal, int barrier, Goal after, String defMod) {
        Term[] frame = (cl.nvars == 0) ? LegacyBuiltinAdapter.NO_FRAME : new Term[cl.nvars];
        if (!cl.unifyHead(goal, frame, B)) return null;
        Goal g = after;
        Term[] body = cl.body;
        for (int i = body.length - 1; i >= 0; i--) {
            g = new Goal(body[i], frame, barrier, g);
            g.module = defMod;                                  // ISS-2025-0466
        }
        return g;
    }
    // END_CHANGE: ISS-2025-0463, ISS-2025-0465

    // ------------------------------------------------------------------ database

    // START_CHANGE: ISS-2025-0446 - engine v4, design B.7: asserta/1, assertz/1, assert/1 and
    // retract/1 are native on the v4 machine and write through ClauseStore (which updates the
    // KnowledgeBase in the same step). The ISO validation of the v2 engine is ported verbatim:
    // instantiation_error / type_error(callable, Head) for a bad Clause or head (8.9.1.3/8.9.2.3/
    // 8.9.3.3), type_error(callable, G) for a number or string in goal position in the body
    // (7.6.2), and permission_error(modify, static_procedure, PI) for a procedure the registry
    // claims as a built-in. retract/1 is re-executable over a generation-filtered candidate array
    // (ISS-2025-0396). retractall/1, abolish/1,2, clause/2, listing/0,1, predicate_property/2 and
    // dynamic/1 stay registry built-ins over the KnowledgeBase and are observed correctly through
    // the ClauseStore version re-sync; moving them onto the v4 SPI belongs to wave W3.
    private Term checkClauseArgument(Term clause, String context, boolean checkBody) {
        Term q = Unify.deref(clause);
        if (q instanceof Variable) throw Errors.instantiation(context);
        Term head = q;
        if (q instanceof CompoundTerm && ":-".equals(((CompoundTerm) q).getName())
                && ((CompoundTerm) q).getArguments().size() == 2) {
            head = Unify.deref(((CompoundTerm) q).getArguments().get(0));
            if (head instanceof Variable) throw Errors.instantiation(context);
            if (checkBody) checkBodyGoals(((CompoundTerm) q).getArguments().get(1), context);
        }
        if (!(head instanceof Atom) && !(head instanceof CompoundTerm)) {
            throw Errors.type("callable", head, context);
        }
        return head;
    }

    private void checkBodyGoals(Term body, String context) {
        Term b = Unify.deref(body);
        while (b instanceof CompoundTerm && ((CompoundTerm) b).getArguments().size() == 2) {
            String f = ((CompoundTerm) b).getName();
            if (!",".equals(f) && !";".equals(f) && !"->".equals(f)) break;
            checkBodyGoals(((CompoundTerm) b).getArguments().get(0), context);
            b = Unify.deref(((CompoundTerm) b).getArguments().get(1));
        }
        if (b instanceof Number || b instanceof PrologString) throw Errors.type("callable", b, context);
    }

    // START_CHANGE: ISS-2025-0501 - 4.1 wave B: the protection covers the v4 NATIVE table and the
    // prelude library exports too, not only the legacy registry.
    //
    // 4.1-A deviation 4: deleting the Java `freeze/2`, `when/2`, `dif/2` and attributed-variable
    // built-ins deleted their registry entries with them, and `isBuiltIn` needs a registration AND
    // an arity entry — so `assertz(freeze(X, Y))` silently became legal while calling freeze/2 still
    // ran the prelude clause. Asking the two stores the machine actually dispatches from
    // (`BuiltinTable` and the prelude) restores the error for those seven and, by the same rule,
    // for the 18 other native indicators that had no registry entry (`put_code/2`, `memberchk/2`,
    // `selectchk/3`, `copy_term/3`, `unifiable/3`, `term_string/2`, `findall/4`, ...).
    //
    // It does NOT change the documented library-override rule: a module that DEFINES partition/4 in
    // its source still overrides the library one, because consult checks
    // `Prolog.checkBuiltInConflict` (the registry) and never comes through here.
    /** Is {@code f/n} a procedure the user may not add clauses to, take clauses from, or abolish? */
    boolean isProtectedProcedure(String f, int n) {
        // A one-entry memo: an assert/retract loop asks about the same indicator every iteration,
        // and none of the three stores can change while ONE query runs (the registry changes only
        // through Prolog.enableSafeMode / registerBuiltIn, the native table only at construction,
        // the prelude owner index only once per JVM). A machine lives for one query.
        if (n == protectedArity && f.equals(protectedFunctor)) return protectedAnswer;
        boolean r;
        if (engine.registry() != null && engine.registry().isBuiltIn(f, n)) {
            r = true;
        } else {
            String key = f + "/" + n;                          // built ONCE for both stores
            r = engine.natives().isNativeKey(key) || engine.modules4().isLibraryIndicatorKey(key);
        }
        protectedFunctor = f;
        protectedArity = n;
        protectedAnswer = r;
        return r;
    }

    private String protectedFunctor;
    private int protectedArity = -1;
    private boolean protectedAnswer;

    /** The predicate indicator term {@code f/n}, for an error message. */
    static Term indicator(String f, int n) {
        return new CompoundTerm(new Atom("/"),
            Arrays.asList((Term) new Atom(f), (Term) Number.valueOf(n)));
    }

    private void checkModifiable(Term head, String context) {
        String f;
        int ar;
        if (head instanceof Atom) { f = ((Atom) head).getName(); ar = 0; }
        else if (head instanceof CompoundTerm) {
            f = ((CompoundTerm) head).getName();
            ar = ((CompoundTerm) head).getArguments().size();
        } else return;
        if (isProtectedProcedure(f, ar)) {
            throw Errors.permission("modify", "static_procedure", indicator(f, ar), context);
        }
    }
    // END_CHANGE: ISS-2025-0501

    private void assertClause(Term clause, boolean front) {
        Term checkedHead = checkClauseArgument(clause, front ? "asserta/1" : "assertz/1", true);
        checkModifiable(checkedHead, front ? "asserta/1" : "assertz/1");
        Term c = Unify.copy(clause, new IdentityHashMap<Variable, Variable>(), guard);
        engine.store().assertRule(toRule(c), front);
        invalidateTables(checkedHead);                          // ISS-2025-0464
    }

    // START_CHANGE: ISS-2025-0464 - a table of P is invalidated when P itself is asserted to or
    // retracted from (and never while an evaluation is running). Changes to a NON-tabled predicate
    // that a tabled one depends on are not tracked: abolish_all_tables/0 is the documented tool.
    private void invalidateTables(Term head) {
        Term h = Unify.deref(head);
        if (h instanceof Atom) engine.tabling().invalidate(((Atom) h).getName(), 0);
        else if (h instanceof CompoundTerm) {
            engine.tabling().invalidate(((CompoundTerm) h).getName(),
                ((CompoundTerm) h).getArguments().size());
        }
    }
    // END_CHANGE: ISS-2025-0464

    private boolean retractClause(Term clause) {
        Term q = Unify.deref(clause);
        Term checkedHead = checkClauseArgument(q, "retract/1", false);
        checkModifiable(checkedHead, "retract/1");
        Term head;
        Term queryClause;
        if (q instanceof CompoundTerm && ":-".equals(((CompoundTerm) q).getName())
                && ((CompoundTerm) q).getArguments().size() == 2) {
            head = ((CompoundTerm) q).getArguments().get(0);
            queryClause = q;
        } else {
            head = q;
            queryClause = new CompoundTerm(new Atom(":-"), Arrays.asList(q, (Term) ATOM_TRUE));
        }
        Term h = Unify.deref(head);
        String f;
        int ar;
        if (h instanceof Atom) { f = ((Atom) h).getName(); ar = 0; }
        else { f = ((CompoundTerm) h).getName(); ar = ((CompoundTerm) h).getArguments().size(); }
        final ClauseStore.Predicate p = engine.store().lookup(f, ar);
        // START_CHANGE: ISS-2025-0502 - retract/1 selects its candidates through the first-argument
        // index, exactly as a call does (Machine.selectClauses). It used to scan p.all(), so
        // `retract(item(K))` over an N-clause predicate was O(N) per call and a loop that retracts
        // every clause was O(N^2): 20 000 clauses took 22.7 s. An unbound or unindexable first
        // argument still yields a null key, and select(null) is the full list — an index miss can
        // never drop a clause (the ISS-2025-0340 hazard).
        final Clause[] candidates = p.select(Clause.argKey1(h));
        // END_CHANGE: ISS-2025-0502
        if (candidates.length == 0) return false;
        final long gen = engine.store().generation();
        final Term qc = queryClause;
        final String fName = f;
        final int fArity = ar;
        final Goal cont = goalStack;
        CP cp = new CP(CP.GEN, B.mark());
        final int[] i = {0};
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                while (i[0] < candidates.length) {
                    Clause cl = candidates[i[0]++];
                    if (!cl.isAlive(gen)) continue;
                    if (i[0] >= candidates.length) self.genExhausted = true;
                    if (!Unify.unify(cl.toTerm(), qc, B)) return FAILED;
                    if (!engine.store().retractClause(p, cl)) return FAILED;
                    engine.tabling().invalidate(fName, fArity);            // ISS-2025-0464
                    return cont;
                }
                return EXHAUSTED;
            }
        };
        pushCP(cp);
        if (advance(cp)) return true;
        popCP();
        return false;
    }

    private Rule toRule(Term c) {
        if (c instanceof CompoundTerm && ":-".equals(((CompoundTerm) c).getName())
                && ((CompoundTerm) c).getArguments().size() == 2) {
            CompoundTerm cc = (CompoundTerm) c;
            return new Rule(cc.getArguments().get(0), flattenBody(cc.getArguments().get(1)));
        }
        return new Rule(c, new ArrayList<Term>());
    }

    private List<Term> flattenBody(Term body) {
        List<Term> gs = new ArrayList<Term>();
        Term cur = body;
        while (cur instanceof CompoundTerm && ",".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            gs.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        gs.add(cur);
        return gs;
    }
    // END_CHANGE: ISS-2025-0446

    // ------------------------------------------------------------------ backtracking

    /** Try the next alternative of {@code cp}, undoing the trail first. */
    private boolean advance(CP cp) {
        while (true) {
            B.undo(cp.trailMark);                              // ISS-2025-0492: one trail
            Goal gs = cp.gen.next(cp);
            if (gs == EXHAUSTED) return false;
            if (gs != FAILED) {
                goalStack = gs;
                cp.altsTaken++;                                   // ISS-2025-0482
                // TRUST-ME POP: the frame owes nothing more, so drop it while it is on top (the
                // only position from which removal cannot shift another frame's cut barrier).
                // START_CHANGE: ISS-2025-0482 - while tracing, a frame that handed out exactly one
                // alternative and is exhausted is DETERMINISTIC and owes no Redo/Fail — SWI drops
                // such a frame too (last-call optimisation), and dropping it is what keeps trace
                // memory O(1). A frame with real alternatives still owes Redo/Fail, so it stays.
                if (cp.genExhausted && (cp.traceGoal == null || cp.altsTaken == 1)
                        && !cps.isEmpty() && cps.get(cps.size() - 1) == cp) {
                    popCP();
                    B.clearIfUnreachable(cps.isEmpty());
                }
                // END_CHANGE: ISS-2025-0482
                return true;
            }
        }
    }

    private boolean backtrack(int floor) {
        while (cps.size() > floor) {
            CP cp = cps.get(cps.size() - 1);
            if (cp.kind == CP.CATCH) { popCP(); continue; }
            if (cp.kind == CP.CLEANUP) {
                popCP();
                if (!cp.cleanupDone) { cp.cleanupDone = true; runCleanup(cp.cleanup); }
                continue;
            }
            if (advance(cp)) {
                if (cp.traceGoal != null) {
                    portDepth = cp.traceDepth + 1;                 // ISS-2025-0482: the frame re-opens
                    tracePort("Redo", cp.traceGoal, cp.traceDepth);
                    if (cp.traceDebug) debugPort(DebugEvent.Port.REDO, cp.traceGoal, cp.traceDepth);
                }
                return true;
            }
            if (cp.traceGoal != null) {
                portDepth = cp.traceDepth;                         // ISS-2025-0482
                tracePort("Fail", cp.traceGoal, cp.traceDepth);
                if (cp.traceDebug) debugPort(DebugEvent.Port.FAIL, cp.traceGoal, cp.traceDepth);
            }
            if (cps.isEmpty() || cps.get(cps.size() - 1) != cp) return false;   // defensive: cannot
            popCP();                                                            // happen; never spin
            B.clearIfUnreachable(cps.isEmpty());
        }
        return false;
    }

    // ------------------------------------------------------------------ ports

    boolean debugTraceActive() {
        return debugPortsActive() || it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled();
    }

    // START_CHANGE: ISS-2025-0494 - 4.1 wave A: an ATTACHED controller is not necessarily a
    // LISTENING one. A controller with no listener, no breakpoint, in CONTINUE mode, with tracing
    // off and no Stop pending can observe nothing, so the machine emits no ports for it — the
    // port sites cost one field read and one method call instead of a resolve + a notify.
    /** The controller to report ports to, or null when nothing would observe them. */
    private DebugController debugPortsTarget() {
        DebugController d = debugController;
        return (d != null && d.needsPorts()) ? d : null;
    }

    private boolean debugPortsActive() { return debugPortsTarget() != null; }
    // END_CHANGE: ISS-2025-0494

    // START_CHANGE: ISS-2025-0482 - wave W8: the four-port DEPTH is the machine's own call-nesting
    // level, not `cps.size()`.
    //
    // It used to be the choice-point height, which happened to look like a call depth only because
    // a traced frame was never trust-me popped. Now that a deterministic frame IS popped while
    // tracing (that is what keeps trace memory linear instead of quadratic), the choice-point
    // height no longer nests, and the IDE needs a real depth: DebugController.notifyPort prunes its
    // call stack by depth, and step-over / step-out compare against a target depth.
    //
    // Every port ASSIGNS the depth rather than incrementing/decrementing it — Call takes the
    // current value and moves one deeper, Exit and Fail return to the frame's own value, Redo
    // re-opens it one deeper. Assignment is what makes it self-healing: a frame cut away without
    // an Exit/Fail port (a `!`, a thrown ball) cannot leave the counter drifting, because the next
    // port of any enclosing frame puts it back.
    private int portDepth;

    /** The depth a new Call port reports; moves one level deeper. */
    int enterPort() { return portDepth++; }

    /** The current four-port nesting level (the depth the next Call would report). */
    int portDepth() { return portDepth; }

    void portCall(Term g, int d) { portDepth = d + 1; tracePort("Call", g, d); debugPort(DebugEvent.Port.CALL, g, d); }
    void portExit(Term g, int d) { portDepth = d; tracePort("Exit", g, d); debugPort(DebugEvent.Port.EXIT, g, d); }
    void portFail(Term g, int d) { portDepth = d; tracePort("Fail", g, d); debugPort(DebugEvent.Port.FAIL, g, d); }
    // END_CHANGE: ISS-2025-0482

    private void debugPort(DebugEvent.Port port, Term goal, int depth) {
        DebugController dc = debugPortsTarget();               // ISS-2025-0494
        if (dc == null) return;
        Term g = goal;
        // ISS-2025-0481: only snapshot when somebody will read the term later (see needsGoalSnapshot)
        if (dc.needsGoalSnapshot()) {
            try { g = Unify.resolve(goal, guard); }
            catch (RuntimeException e) { ControlFlow.rethrowIfControl(e); g = goal; }
        }
        dc.notifyPort(port, g, new HashMap<String, Term>(), depth);
    }

    // START_CHANGE: ISS-2025-0482 - the indentation is CAPPED. The depth is a real call depth now,
    // and a deterministic tail recursion reaches it: `loop(1000000)` under trace would otherwise
    // build a 2 000 000-character indent for its last line, i.e. quadratic output. The depth itself
    // is still reported exactly, in the parentheses.
    private static final int MAX_TRACE_INDENT = 40;
    // END_CHANGE: ISS-2025-0482

    private void tracePort(String port, Term goal, int depth) {
        if (!it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled()) return;
        try {
            StringBuilder sb = new StringBuilder();
            int indent = (depth < MAX_TRACE_INDENT) ? depth : MAX_TRACE_INDENT;
            for (int i = 0; i < indent; i++) sb.append("  ");
            // ISS-2025-0482: format the goal DIRECTLY. The writer dereferences every cell and is
            // cycle-safe, so Unify.resolve here only built a throw-away copy of the whole term —
            // the single most expensive thing a trace port did.
            String g = it.denzosoft.jprolog.core.util.TermFormatter
                .format(goal, false, false, false, 1200);
            it.denzosoft.jprolog.builtin.io.StreamManager.out().println(sb + port + ": (" + depth + ") " + g);
        } catch (RuntimeException ignored) {
            ControlFlow.rethrowIfControl(ignored);
        }
    }

    // ------------------------------------------------------------------ attributed variables

    // START_CHANGE: ISS-2025-0457 - engine v4 wave W4, design B.9: the real wake queue.
    /**
     * The attribute hook. It never RUNS anything — it is called from inside a term walk — it only
     * queues one wake goal per attribute module through {@link Machine#wake}, and the drive loop
     * runs the queue before the next goal, in the current binding context. See {@link Coroutining}.
     */
    private static final class WakeHandler implements Unify.AttrHandler {
        private final Machine m;
        WakeHandler(Machine m) { this.m = m; }

        @Override
        public boolean onBind(Variable v, Term value, Bindings b) {
            return Coroutining.onBind(m, v, value, b);
        }
    }

    /**
     * Queue a goal woken by binding an attributed variable. The push is <b>trailed</b>: a head
     * unification that binds an attributed cell and then fails on a later argument must leave no
     * stale wake behind, and backtracking past the binding must re-arm the suspension rather than
     * run it. (Undo is LIFO, so truncating back to the recorded height is exact.)
     */
    void wake(Term goal, Bindings b) {
        final int height = woken.size();
        woken.add(goal);
        b.pushUndo(new Runnable() {
            @Override public void run() {
                while (woken.size() > height) woken.remove(woken.size() - 1);
            }
        });
    }

    /**
     * Is there a user-defined {@code Module:attr_unify_hook/2}? JProlog stores a module-qualified
     * clause head as a {@code :/2} predicate, so this is a scan of that predicate's heads — and
     * {@code :/2} has no clauses at all in every program that does not define one, which is the
     * fast path taken on every attributed binding.
     */
    boolean hasQualifiedHook(String module) {
        ClauseStore.Predicate p = engine.store().lookup(":", 2);
        if (p.size() == 0) return false;
        Clause[] all = p.all();
        for (int i = 0; i < all.length; i++) {
            Term h = all[i].head;
            if (!(h instanceof CompoundTerm)) continue;
            List<Term> as = ((CompoundTerm) h).getArguments();
            if (as.size() != 2 || !":".equals(((CompoundTerm) h).getName())) continue;
            Term mt = as.get(0);
            if (!(mt instanceof Atom) || !module.equals(((Atom) mt).getName())) continue;
            Term g = as.get(1);
            if (g instanceof CompoundTerm && "attr_unify_hook".equals(((CompoundTerm) g).getName())
                    && ((CompoundTerm) g).getArguments().size() == 2) {
                return true;
            }
        }
        return false;
    }

    /**
     * Call {@code Module:Goal} as a plain {@code :/2} user predicate (the flat lookup), bypassing
     * the module-manager resolution {@link #stepN} applies to a {@code :}-qualified goal. That is
     * how {@code attr_unify_hook/2} defined the SWI way reaches its clause in an engine whose
     * module system does not register one module per attribute library.
     */
    boolean callQualified(Term qualifiedGoal) { return callUser(qualifiedGoal, qualifiedGoal, true); }
    // END_CHANGE: ISS-2025-0457
}
// END_CHANGE: ISS-2025-0442
