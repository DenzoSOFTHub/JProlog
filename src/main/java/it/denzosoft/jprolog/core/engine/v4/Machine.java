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
    /** ISS-2025-0749: the end marker of the signal goals running now; null when none is. */
    private Goal sigEnd;
    /** ISS-2025-0661: the table sequence number the innermost negation started at; -1 = none. */
    private long negTableFloor = -1;
    private final Bindings B;
    private final ResourceGuard guard;
    private DebugController debugController;
    private String currentContext = "call";
    // START_CHANGE: ISS-2025-0778 - 4.6 wave Q6.4: the context of a running native is kept as its
    // name and arity and rendered "name/arity" only when an error asks for it (callNative used to
    // concatenate the string on EVERY native call — compare/3 inside every predsort comparison).
    private int currentContextArity = -1;
    // END_CHANGE: ISS-2025-0778

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

    // START_CHANGE: ISS-2025-0540 - a goal over a clause frame (fr == null: a live term).
    private Goal mgf(Term t, Term[] fr, int barrier, Goal next) {
        Goal g = (fr == null) ? new Goal(t, barrier, next) : new Goal(t, fr, barrier, next);
        g.module = ctxModule;
        return g;
    }

    private static Goal mgf(Term t, Term[] fr, int barrier, Goal next, String mod) {
        Goal g = (fr == null) ? new Goal(t, barrier, next) : new Goal(t, fr, barrier, next);
        g.module = mod;
        return g;
    }

    /**
     * Expand a skeleton control construct exactly as {@link #stepN} expands the live one, but
     * with its arguments left as skeletons over {@code g.frame}. A `;` whose left argument is a
     * variable is not expanded here (the caller checks): stepN looks at what it is BOUND to.
     */
    private void stepControlSkel(Clause.Skel s, Goal g) {
        Term[] fr = g.frame;
        String f = s.getName();
        Term a0 = s.arg(0), a1 = s.arg(1);
        if (",".equals(f)) {
            goalStack = mgf(a0, fr, g.cutBarrier, mgf(a1, fr, g.cutBarrier, goalStack));
        } else if (";".equals(f)) {
            if (a0 instanceof CompoundTerm && ((CompoundTerm) a0).arity() == 2
                    && "->".equals(((CompoundTerm) a0).getName())) {
                ite(((CompoundTerm) a0).arg(0), ((CompoundTerm) a0).arg(1), a1, g.cutBarrier, fr);
            } else if (a0 instanceof CompoundTerm && ((CompoundTerm) a0).arity() == 2
                    && "*->".equals(((CompoundTerm) a0).getName())) {
                softCut(((CompoundTerm) a0).arg(0), ((CompoundTerm) a0).arg(1), a1, g.cutBarrier, fr);
            } else {
                disjunction(a0, a1, g.cutBarrier, fr);
            }
        } else if ("->".equals(f)) {
            ite(a0, a1, ATOM_FAIL, g.cutBarrier, fr);
        } else {
            softCut(a0, a1, ATOM_FAIL, g.cutBarrier, fr);
        }
    }
    // END_CHANGE: ISS-2025-0540

    /** A goal that runs in an explicit module ({@code null} == {@code user}). */
    static Goal mg(Term t, int barrier, Goal next, String mod) {   // ISS-2025-0780: package
        Goal g = new Goal(t, barrier, next);
        g.module = mod;
        return g;
    }

    /** {@code null} for {@code user}, so the hot path never stores a redundant string. */
    static String modKey(String name) {                  // ISS-2025-0780: package
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
    public String currentContext() {
        return currentContextArity < 0 ? currentContext : currentContext + "/" + currentContextArity;   // ISS-2025-0778
    }

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

    // START_CHANGE: ISS-2025-0779 - 4.6 wave Q6.2/Q6.4: native iterations that run their user
    // goals ON the goal stack. A Step is pushed as a goal action; when the drive loop reaches it,
    // it inspects what the goal before it produced, updates its Java-side state and pushes the
    // next goal (and itself, or its successor) — no nested drive per element. Returning false is a
    // plain failure: the machine backtracks.
    /** A continuation that may fail. */
    abstract static class Step implements Runnable {
        /** Run the step; {@code false} fails (the machine backtracks). */
        abstract boolean step(Machine m);
        @Override public final void run() { throw new IllegalStateException("a Step runs through step()"); }
    }

    /** Push a Step as the next thing to run. */
    void pushStep(Step s) { goalStack = new Goal(s, goalStack); }

    /** Push {@code goal} to run next in context {@code module} (null == user), with its own cut
     *  barrier (the current choice-point height), as call/1 would. */
    void pushCall(Term goal, String module) { goalStack = mg(goal, cps.size(), goalStack, module); }

    /** The current choice-point height. */
    int cpHeight() { return cps.size(); }

    /** Remove every choice point above {@code height} (running pending cleanups), as a cut. */
    void cutBack(int height) { cut(height); }

    /** The context module of the running goal ({@code null} == user). */
    String ctxModuleKey() { return ctxModule; }

    /**
     * Push a choice point that has no alternative: backtracking into it FAILS the construct that
     * pushed it (and reports its Fail port when {@code traceGoal} is not null). It is the floor a
     * goal-stack iteration fails onto when one of its user goals fails.
     */
    void pushFailFrame(Term traceGoal, int traceDepth) {
        CP cp = new CP(CP.GEN, B.mark());
        cp.gen = EXHAUSTED_GEN;
        cp.genExhausted = true;
        if (traceGoal != null) { cp.traceGoal = traceGoal; cp.traceDepth = traceDepth; cp.traceDebug = debugPortsActive(); }
        pushCP(cp);
    }

    /** Called by a native that emits its own Exit/Fail ports later (from a Step): returns the
     *  depth of its Call port, or -1 when no port is being traced. */
    int claimNativePorts() {
        if (nativeTraceGoal == null) return -1;
        nativePortsHandled = true;
        return nativeTraceDepth;
    }

    /** The goal the running native's ports report (null when not traced). */
    Term nativeTraceGoal() { return nativeTraceGoal; }
    // END_CHANGE: ISS-2025-0779

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
        /** ISS-2025-0528: the four-port depth at which catch/3 was called. */
        int catchDepth;
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

    Goal goalStack;                                   // ISS-2025-0780: package (NativeApply)
    private final ArrayList<CP> cps = new ArrayList<CP>();
    /** Goals woken by binding a frozen/attributed variable; run before the next goal. */
    private final ArrayList<Term> woken = new ArrayList<Term>();

    void pushCP(CP cp) {                                 // ISS-2025-0780: package
        cps.add(cp);
        B.barrierSerial = cp.serialMark;
    }

    CP popCP() {                                         // ISS-2025-0780: package
        CP cp = cps.remove(cps.size() - 1);
        B.barrierSerial = cps.isEmpty() ? 0 : cps.get(cps.size() - 1).serialMark;
        return cp;
    }

    /** Test hook: live choice points. */
    public int choicePointCount() { return cps.size(); }

    // START_CHANGE: ISS-2025-0627 - wave P6.4: the CLI streams answers and, like SWI's toplevel,
    // prints `.` instead of prompting for more when the answer is the last one.
    /**
     * Could backtracking still produce another answer? True when a clause or generator choice
     * point is left; catch and cleanup frames alone are not alternatives. Meaningful inside a
     * solution sink.
     */
    public boolean hasAlternatives() {
        for (int i = cps.size() - 1; i >= 0; i--) {
            int k = cps.get(i).kind;
            if (k == CP.CLAUSES || k == CP.GEN) return true;
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0627
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

    // START_CHANGE: ISS-2025-0710 - wave Q2.1: library(solution_sequences) runs its goal INSIDE
    // the current continuation (lazily — no findall, no nested drive) and looks at each solution
    // as it arrives. A filter answers per solution: pass it on, reject it (backtrack into the goal
    // for the next one), or pass it on as the LAST one — which cuts the goal's remaining choice
    // points exactly as `!` would, so a setup_call_cleanup/3 inside it runs its cleanup now.
    /** A per-solution decision of {@link #pushFiltered}. */
    interface SolutionFilter {
        int ACCEPT = 0, REJECT = 1, ACCEPT_LAST = 2;
        /** Called with the solution's bindings live; may bind (e.g. call_nth/2's counter). */
        int onSolution(Machine m);
    }

    /** {@code call(Goal)} (opaque to cut, in the current context module) with a solution filter. */
    void pushFiltered(Term goal, final SolutionFilter filter) {
        final int barrier = cps.size();
        final Goal cont = goalStack;
        Goal check = new Goal(new Runnable() {
            @Override public void run() {
                int r = filter.onSolution(Machine.this);
                if (r == SolutionFilter.REJECT) goalStack = new Goal(ATOM_FAIL, 0, goalStack);
                else if (r == SolutionFilter.ACCEPT_LAST) cut(barrier);
            }
        }, cont);
        goalStack = mg(goal, barrier, check);
    }
    // END_CHANGE: ISS-2025-0710

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
        sigEnd = null;                                             // ISS-2025-0749
        delays = null;                                             // ISS-2025-0755
        lastAnswerDelays = null;
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
        final List<String> names = new ArrayList<String>(queryVars.size());
        final List<Variable> cells = new ArrayList<Variable>(queryVars.size());
        // START_CHANGE: ISS-2025-0515 - wave P1.2: an anonymous `_` is not an answer variable.
        // Each `_` is its own cell with a generated `_G<n>` name, so it used to reach the answer
        // map as `{_G7=1}`. A NAMED variable that merely starts with `_` (`_Foo`) stays: SWI hides
        // those only when it prints the toplevel answer, and an embedder may well ask for them.
        for (Map.Entry<String, Variable> e : queryVars.entrySet()) {
            if (e.getValue().isAnonymous()) continue;
            names.add(e.getKey());
            cells.add(e.getValue());
        }
        // END_CHANGE: ISS-2025-0515
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
        // START_CHANGE: ISS-2025-0746/0749/0750 - 4.6 wave Q4: a top-level query registers its
        // thread as able to send messages (the load-cycle check) and publishes its guard for
        // thread_statistics/3.
        final it.denzosoft.jprolog.core.engine.ThreadSignals.Box sigBox =
            it.denzosoft.jprolog.core.engine.ThreadSignals.current();
        final it.denzosoft.jprolog.core.engine.ResourceGuard prevSigGuard = sigBox.guard;
        sigBox.guard = guard;
        sigBox.thread = Thread.currentThread();
        if (top) it.denzosoft.jprolog.core.engine.ThreadWaits.enterQuery();
        // END_CHANGE: ISS-2025-0746/0749/0750
        // START_CHANGE: ISS-2025-0492 - this machine takes the bridged built-ins' undo actions
        // while it runs (b_setval/2, op/3, setarg/3, the CLP(FD) store). Saved and restored, so a
        // nested machine hands the role back.
        Machine prevUndoTarget = Undo.enter(this);
        try {
            // START_CHANGE: ISS-2025-0518 - the query is converted to a body like any call/1 goal
            checkBody(q, "call", 1);
            // END_CHANGE: ISS-2025-0518
            drive(new Driver() {
                // ISS-2025-0514: a top-level answer is a COPY (see answer())
                @Override public boolean onSolution() { return sink.onSolution(answer(names, cells)); }
            }, 0);
        } finally {
            // START_CHANGE: ISS-2025-0523 - wave P1.10: a query that is ABANDONED — the sink said
            // stop, the inference budget or a Stop/interrupt aborted it — still owes the cleanup of
            // every setup_call_cleanup/3 frame it left open. They used to be dropped with the
            // machine, so `setup_call_cleanup(open(F,write,S), loop, close(S))` leaked the stream
            // when the budget hit. Cut them away now; the cleanups' own exceptions are swallowed so
            // the exception that is propagating (if any) is the one the caller sees.
            if (!cps.isEmpty()) cutQuietly(0);
            // END_CHANGE: ISS-2025-0523
            if (top) {                                             // ISS-2025-0479
                engine.addInferences(guard.getSteps());           // ISS-2025-0608
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
            sigBox.guard = prevSigGuard;                                          // ISS-2025-0750
            if (top) it.denzosoft.jprolog.core.engine.ThreadWaits.exitQuery();   // ISS-2025-0746
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

    // START_CHANGE: ISS-2025-0514 - wave P1.1: a top-level answer is a COPY, not a resolve.
    // Unify.resolve keeps every unbound cell live, and the machine goes on binding those cells
    // after the answer has been handed out: `X = f(Y) ; Y = 1` delivered X = f(1) as its FIRST
    // answer (the untrailed Y = 1 of the last alternative wrote through it), and `(true ; X = 1)`
    // showed X = 1 twice. The copy uses ONE variable map per answer, so sharing inside the answer
    // is preserved; each fresh cell keeps the original's name, so an answer prints exactly as
    // before. Attributed cells stay live (see Unify.CopyAnswer) so Prolog.residualGoals still
    // sees their constraints. Nested sub-queries (runSubQuery) keep the non-copying snapshot: a
    // legacy built-in maps their answers back onto its caller's cells.
    private Map<String, Term> answer(List<String> names, List<Variable> cells) {
        // ISS-2025-0755: the answer's delayed literals (null: unconditional), for the toplevel
        lastAnswerDelays = (delays == null) ? null
            : Unify.copy(Tabling.Delay.conjunction(delays), new IdentityHashMap<Variable, Variable>(), guard);
        Map<String, Term> m = new HashMap<String, Term>();
        if (names.isEmpty()) return m;
        IdentityHashMap<Variable, Variable> vm = new IdentityHashMap<Variable, Variable>();
        for (int i = 0; i < names.size(); i++) m.put(names.get(i), Unify.copyAnswer(cells.get(i), vm, guard));
        return m;
    }
    // END_CHANGE: ISS-2025-0514

    private interface Driver { boolean onSolution(); }

    // ------------------------------------------------------------------ drive loop

    private static final Atom ATOM_TRUE = new Atom("true");
    private static final Atom ATOM_FAIL = new Atom("fail");
    private static final Atom CUT = new Atom("!");
    private static final Atom DOT = new Atom(".");
    private static final Atom NIL = new Atom("[]");
    private static final Atom COMMA = new Atom(",");
    private static final Atom ATOM_CALL = new Atom("call");               // ISS-2025-0516

    /**
     * Run until exhausted. {@code floor} is the choice-point height this run must not backtrack
     * below, so a nested run (findall, catch, a meta-call) leaves the caller's choice points alone.
     */
    private void drive(Driver onSol, int floor) {
        while (true) {
            guard.step();
            try {
                // START_CHANGE: ISS-2025-0749 - 4.6 wave Q4.1: thread_signal/2. A signal is run on
                // THIS machine's goal stack, in front of the continuation — never asynchronously —
                // so its bindings and trail entries are ordinary ones and an exception it raises
                // unwinds from here, as if the interrupted goal had raised it. One volatile read
                // per step while no signal is pending anywhere in the JVM.
                // (sigEnd: while signal goals run no further signal is taken, so signals run one
                // after the other, in the order they were sent)
                if (it.denzosoft.jprolog.core.engine.ThreadSignals.PENDING.get() != 0 && sigEnd == null
                        && !it.denzosoft.jprolog.core.engine.ThreadSignals.handling()) {
                    Term sig = it.denzosoft.jprolog.core.engine.ThreadSignals.poll();
                    if (sig != null) {
                        // every queued signal, pushed so that they run in the order they were sent
                        ArrayList<Term> sigs = new ArrayList<Term>();
                        for (; sig != null; sig = it.denzosoft.jprolog.core.engine.ThreadSignals.poll()) sigs.add(sig);
                        sigEnd = new Goal(new Runnable() {
                            @Override public void run() { sigEnd = null; }
                        }, goalStack);
                        goalStack = sigEnd;
                        for (int i = sigs.size() - 1; i >= 0; i--) {
                            goalStack = mg(signalGoal(sigs.get(i)), cps.size(), goalStack, null);
                        }
                        continue;
                    }
                }
                // END_CHANGE: ISS-2025-0749
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
                if (g.action != null) {
                    // START_CHANGE: ISS-2025-0779 - a Step is a continuation that can FAIL (the
                    // frame of a native iteration: predsort's merge, the apply family)
                    if (g.action instanceof Step) {
                        if (!((Step) g.action).step(this) && !backtrack(floor)) return;
                        continue;
                    }
                    // END_CHANGE: ISS-2025-0779
                    g.action.run();
                    continue;
                }
                ctxModule = g.module;                         // ISS-2025-0466
                Term raw = g.term;
                // START_CHANGE: ISS-2025-0540 - wave P2.1: a body goal whose call site already
                // resolved to a plain user predicate goes straight to its clauses — no control
                // construct tests, no native/registry/tabling probes, no "name/arity" lookups.
                if (raw instanceof Clause.Skel && g.frame != null) {
                    // START_CHANGE: ISS-2025-0540 - `,`/`;`/`->`/`*->` in a clause body are
                    // expanded over the frame: their branches are pushed as skeleton goals and
                    // instantiated only when reached (an untaken branch is never built), so the
                    // goals inside an if-then-else get call sites too.
                    Clause.Skel sk = (Clause.Skel) raw;
                    if (sk.lazyControl && !(sk.arg(0) instanceof VarRef && ";".equals(sk.getName()))) {
                        stepControlSkel(sk, g);
                        continue;
                    }
                    // END_CHANGE: ISS-2025-0540
                    Object so = ((Clause.Skel) raw).site;
                    // START_CHANGE: ISS-2025-0779 - a body goal that resolved to a v4 native
                    // calls it directly (no stepN name tests, no native-table probe)
                    if (so instanceof NativeSite) {
                        NativeSite ns = (NativeSite) so;
                        if (ns.engine == engine && ns.stamp == engine.dispatchStamp()) {
                            siteHits++;
                            CompoundTerm nc = (CompoundTerm) Clause.instantiate(raw, g.frame);
                            int na = nc.arity();
                            Term[] nargs = new Term[na];
                            for (int i = 0; i < na; i++) nargs[i] = nc.arg(i);
                            if (callNative(ns.builtin, nc, nc.getName(), na, nargs).intValue() != 1
                                    && !backtrack(floor)) return;
                            continue;
                        }
                    }
                    // END_CHANGE: ISS-2025-0779
                    if (so instanceof CallSite) {
                        CallSite site = (CallSite) so;
                        if (site.engine == engine && site.stamp == engine.dispatchStamp()
                                && (site.module == null ? g.module == null : site.module.equals(g.module))) {
                            siteHits++;
                            Term t = Clause.instantiate(raw, g.frame);
                            boolean ok = (site.module == null) ? callSite(site.pred, t) : callModuleSite(site, t);
                            if (!ok && !backtrack(floor)) return;
                            continue;
                        }
                    }
                }
                // END_CHANGE: ISS-2025-0540
                Term t = (g.frame != null) ? Clause.instantiate(raw, g.frame) : raw;
                t = Unify.deref(t);
                // START_CHANGE: ISS-2025-0517 - wave P1.4: a goal written as a VARIABLE is call(G)
                // (ISO 7.6.2), so it is opaque to cut: `G = !, (X = 1 ; X = 2), G` must keep the
                // disjunction's second answer, and `G = (!, fail), (G ; X = alt)` must reach
                // `X = alt`. It used to run with the enclosing clause's barrier, i.e. as if the
                // `!` had been written in the clause. The test is on the goal's SKELETON (a VarRef
                // in a clause body, a Variable in a query or a control construct's argument), so a
                // literal `!` is unaffected; only a variable bound to `!` or to a control construct
                // (the only goals that read the barrier) is re-pushed, with a fresh one.
                if ((raw instanceof VarRef || raw instanceof Variable)
                        && ((t instanceof CompoundTerm && isControlConstruct((CompoundTerm) t))
                            || (t instanceof Atom && "!".equals(((Atom) t).getName())))) {
                    checkBody(t, "call", 1);                  // ISS-2025-0518: call(G) checks G
                    Goal ng = new Goal(t, cps.size(), g.next);
                    ng.module = g.module;
                    g = ng;
                }
                // END_CHANGE: ISS-2025-0517

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
                sigEnd = null;                                // ISS-2025-0749: a signal may have thrown
                Term ball = e.getErrorTerm();
                if (ball == null) throw e;
                if (!handleBall(Unify.copy(ball, new IdentityHashMap<Variable, Variable>(), guard), floor)) throw e;
            } catch (StackOverflowError so) {
                sigEnd = null;                                // ISS-2025-0749
                // The v4 core is iterative, but a legacy built-in can still overflow. Convert it
                // to a catchable ISO error INSIDE the loop so the running program's catch/3 sees
                // it (design B.6, limit L-14) — the v2 engine only converted after unwinding.
                if (!handleBall(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms
                        .resourceError("stack_overflow", currentContext()), floor)) {
                    throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms
                        .resourceError("stack_overflow", currentContext()));
                }
            } catch (OutOfMemoryError oom) {
                cps.clear();                                   // free the frames before doing anything
                B.clearIfUnreachable(true);
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms
                    .resourceError("memory", currentContext()));
            }
        }
    }

    // START_CHANGE: ISS-2025-0749
    /**
     * The goal a signal runs as: {@code \+ \+ G} under {@code ignore/1} — once, its bindings
     * undone, a failure ignored, an exception propagated (SWI runs a signal goal the same way).
     */
    public static Term signalGoal(Term g) {
        Term nn = new CompoundTerm(new Atom("\\+"), new Term[] {
            new CompoundTerm(new Atom("\\+"), new Term[] {g})});
        return new CompoundTerm(new Atom("ignore"), new Term[] {nn});
    }
    // END_CHANGE: ISS-2025-0749

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
        // START_CHANGE: ISS-2025-0542 - wave P2.3: a name this method never special-cases (every
        // user predicate and most natives) skips the ~40 string comparisons below with ONE hash
        // probe (a String caches its hash) and goes straight to the native/registry/user tail.
        if (!SITE_EXCLUDED.contains(f)) return stepPlain(c, g, floor, f, c.arity());
        // END_CHANGE: ISS-2025-0542
        List<Term> a = c.getArguments();
        int n = a.size();

        // START_CHANGE: ISS-2025-0734 - 4.6 wave Q3.5: a goal '|'(A, B) is (A ; B) — SWI keeps
        // '|'/2 as a control construct "equivalent to ;/2" now that the reader no longer turns the
        // bar into `;`. Same cut barrier, so a `!` inside it is transparent like one in `;`.
        if (n == 2 && "|".equals(f)) {
            c = new CompoundTerm(new Atom(";"), a);
            f = ";";
        }
        // END_CHANGE: ISS-2025-0734
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
            checkBody(a.get(0), f, 1);                          // ISS-2025-0518
            // START_CHANGE: ISS-2025-0661 - decision §8: negation over an INCOMPLETE table of the
            // same SCC (`p :- \+ p` tabled) has no answer without the well-founded semantics,
            // which is not implemented (LIM-046). While this machine runs a tabled evaluation the
            // negation is run as an opaque sub-run with the table sequence number as its floor, and
            // a consumer inside it that reads a table created BEFORE the negation started (an
            // incomplete ancestor) raises permission_error instead of answering inconsistently.
            // Outside a tabled evaluation nothing changes (the inline if-then-else below).
            final Tabling tb = tablingHere();                   // ISS-2025-0776: cached per machine
            if (tb.evaluating() && tb.ownedByCurrentThread()) {
                Term neg = a.get(0);
                if (g.module != null) neg = new CompoundTerm(new Atom(":"), new Term[] {new Atom(g.module), neg});
                long savedFloor = negTableFloor;
                negTableFloor = tb.lastSeq();
                final boolean[] found = {false};
                try {
                    forEachSolution(neg, new SolutionVisitor() {
                        @Override public boolean visit() { found[0] = true; return false; }
                    });
                } finally {
                    negTableFloor = savedFloor;
                }
                return found[0] ? backtrack(floor) : true;
            }
            // END_CHANGE: ISS-2025-0661
            ite(a.get(0), ATOM_FAIL, ATOM_TRUE, g.cutBarrier);
            return true;
        }
        if ("call".equals(f) && n >= 1) {
            // START_CHANGE: ISS-2025-0455 - library(yall) lambdas are expanded here, before the
            // arguments are appended: `call([X,Y]>>Body, 1, Y)` must copy the lambda, bind X=1 and
            // run Body — appending would have built the nonexistent `>>/4`.
            Term callee = Unify.deref(a.get(0));
            Term goal;
            if (Lambdas.isLambda(callee)) {
                List<Term> extra = (n == 1) ? java.util.Collections.<Term>emptyList() : a.subList(1, n);
                goal = Lambdas.expand(this, callee, extra);
                if (goal == null) goal = (n == 1) ? a.get(0) : addArgs(callee, extra);
            } else {
                goal = (n == 1) ? a.get(0) : addArgs(callee, c, 1, n);   // ISS-2025-0777
            }
            // END_CHANGE: ISS-2025-0455
            checkBody(goal, "call", n);                         // ISS-2025-0518
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
            checkBody(a.get(1), "findall", 3);                  // ISS-2025-0518
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
            frame.catchDepth = portDepth;                      // ISS-2025-0528
            pushCP(frame);
            goalStack = mg(a.get(0), cps.size(), new Goal(new Runnable() {
                @Override public void run() {
                    // START_CHANGE: ISS-2025-0775 - 4.6 wave Q6 (extra 6): Goal exited leaving no
                    // choice point above the frame, so catch/3 is deterministic (SWI): the frame
                    // is dropped while it is on top (the trust-me position) instead of surviving
                    // as a choice point — `setup_call_cleanup(true, catch(true, _, true), D = 1)`
                    // did not run its cleanup at exit.
                    if (!cps.isEmpty() && cps.get(cps.size() - 1) == frame) {
                        popCP();
                        B.clearIfUnreachable(cps.isEmpty());
                        return;
                    }
                    // END_CHANGE: ISS-2025-0775
                    frame.active = false;                       // ISO 7.8.9: only during Goal's extent
                    B.pushUndo(new Runnable() { @Override public void run() { frame.active = true; } });
                }
            }, goalStack));
            // ISS-2025-0518: Goal is call(Goal) — checked INSIDE the catch scope, which is armed
            checkBody(a.get(0), "call", 1);
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
            checkBody(a.get(0), f, 1);                          // ISS-2025-0518
            if (debugTraceActive()) iteTraced(c, a.get(0), ATOM_TRUE, ATOM_FAIL, g.cutBarrier);
            else ite(a.get(0), ATOM_TRUE, ATOM_FAIL, g.cutBarrier);
            return true;
        }
        if (n == 1 && "ignore".equals(f)) {
            checkBody(a.get(0), f, 1);                          // ISS-2025-0518
            if (debugTraceActive()) iteTraced(c, a.get(0), ATOM_TRUE, ATOM_TRUE, g.cutBarrier);
            else ite(a.get(0), ATOM_TRUE, ATOM_TRUE, g.cutBarrier);
            return true;
        }
        if (n == 2 && "forall".equals(f)) {
            checkBody(a.get(0), f, 2);                          // ISS-2025-0518
            checkBody(a.get(1), f, 2);
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
        return stepPlain(c, g, floor, f, n);
    }

    /** The tail of {@link #stepN}: a native, a registry built-in, or a user predicate. */
    private boolean stepPlain(CompoundTerm c, Goal g, int floor, String f, int n) {   // ISS-2025-0542
        // START_CHANGE: ISS-2025-0777 - 4.6 wave Q6.1: a run-time goal whose functor already
        // resolved (in this context, under this dispatch stamp) to a plain predicate goes straight
        // to its clauses, like a body goal with a call site.
        if (rtName != null && (g.frame == null || !(g.term instanceof Clause.Skel))) {
            CallSite site = rtLookup(f, n, g.module);
            if (site != null) {
                rtSiteHits++;
                boolean ok = (site.module == null) ? callSite(site.pred, c) : callModuleSite(site, c);
                return ok || backtrack(floor);
            }
        }
        // END_CHANGE: ISS-2025-0777
        long natStamp = engine.dispatchStamp();               // ISS-2025-0779: read before the probe
        Builtin nat = engine.natives().lookup(f, n);
        if (nat != null) {                                    // v4 natives run even while debugging:
            // START_CHANGE: ISS-2025-0779 - remember it on the body skeleton (natives are found
            // before anything else, in every context module, so the site is context-free)
            if (g.term instanceof Clause.Skel && g.frame != null && !SITE_EXCLUDED.contains(f)) {
                ((Clause.Skel) g.term).site = new NativeSite(engine, natStamp, nat);
            }
            // END_CHANGE: ISS-2025-0779
            Term[] args = new Term[n];                        // they emit their own ports
            for (int i = 0; i < n; i++) args[i] = c.arg(i);
            Integer r = callNative(nat, c, f, n, args);
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
                installCallSite(g, f, n, true);                   // ISS-2025-0540
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
        installCallSite(g, f, n, false);                       // ISS-2025-0540
        if (!callUser(c, c)) return backtrack(floor);
        return true;
    }

    // START_CHANGE: ISS-2025-0540 - wave P2.1 (design: per-call predicate resolution cost ~30 %
    // of nrev/loop/deriv in the 4.4.0 profile). A compound body goal of a compiled clause is a
    // Clause.Skel, and the first time it reaches this point — the plain-user-predicate exit of
    // stepN — its skeleton remembers the resolved ClauseStore.Predicate. The answer "f/n is a
    // user predicate" depends only on f/n (not on the goal's arguments: every data-dependent
    // branch of stepN is excluded by name below) and on three tables — the natives, the legacy
    // registry, the table declarations — whose modification counters make up
    // Engine.dispatchStamp(); a Predicate object is never replaced, and whether it has clauses is
    // asked on every call. Only the `user` context is cached (a goal running in a module resolves
    // through the module first), and the cache is keyed by engine, since a prelude clause may be
    // shared. A stale or missing site just takes this slow path again.
    /** The resolved call site of a body goal (immutable; replaced as a whole). */
    static final class CallSite {
        final Engine engine;
        final long stamp;
        /** The `user` predicate (module == null). */
        final ClauseStore.Predicate pred;
        /** The context module the site was resolved in, or null for `user`. */
        final String module;
        /** A module site: that module's OWN clauses for the indicator, and its meta spec. */
        final Modules.Pred modPred;
        final int[] meta;
        CallSite(Engine engine, long stamp, ClauseStore.Predicate pred) {
            this(engine, stamp, pred, null, null, null);
        }
        CallSite(Engine engine, long stamp, ClauseStore.Predicate pred, String module,
                 Modules.Pred modPred, int[] meta) {
            this.engine = engine;
            this.stamp = stamp;
            this.pred = pred;
            this.module = module;
            this.modPred = modPred;
            this.meta = meta;
        }
    }

    // START_CHANGE: ISS-2025-0779
    /** The resolved call site of a body goal that is a v4 native (immutable). */
    static final class NativeSite {
        final Engine engine;
        final long stamp;
        final Builtin builtin;
        NativeSite(Engine engine, long stamp, Builtin builtin) {
            this.engine = engine;
            this.stamp = stamp;
            this.builtin = builtin;
        }
    }
    // END_CHANGE: ISS-2025-0779

    /** Names stepN (or the inline table) handles itself, possibly depending on the arguments. */
    private static final java.util.Set<String> SITE_EXCLUDED = new java.util.HashSet<String>(Arrays.asList(
        ",", ";", "|", "->", "*->", "=", "^", ":", Modules.MCTX, "\\+", "not", "call", ">>", "\\", "/",
        "findall", "catch", "throw", "assertz", "assert", "asserta", "retract", "setup_call_cleanup",
        "call_cleanup", "once", "ignore", "forall", "between", "length",
        "is", "<", ">", "=<", ">=", "=:=", "=\\=", "==", "\\==", "@<", "@>", "@=<", "@>=", "\\=",
        "var", "nonvar", "atom", "atomic", "number", "integer", "float", "compound", "callable"));

    /** Number of goals that took the call-site fast path (test hook). */
    long siteHits;
    /** Activations that needed no choice point (one candidate clause; test hook, ISS-2025-0779). */
    long singleClauseActivations;
    /** Nested once-drives (runOnce) started on this machine (test hook, ISS-2025-0779). */
    long runOnceCalls;
    /** Native library(apply) levels entered (test hook, ISS-2025-0780). */
    long applyLevels;

    // START_CHANGE: ISS-2025-0776 - 4.6 wave Q6 (extra): the table space of THIS machine. A machine
    // runs on one thread and inside one Tabling context (a worker machine is created after
    // Tabling.enterWorker and never outlives it), so the per-thread lookup Engine.tabling() does
    // (a ThreadLocal probe) is done once per machine instead of on every negation.
    private Tabling tablingCache;
    private Thread tablingThread;
    /** Number of per-thread table-space lookups this machine made (test hook). */
    long tablingLookups;

    Tabling tablingHere() {
        Thread t = Thread.currentThread();
        if (tablingThread != t) {
            tablingCache = engine.tabling();
            tablingThread = t;
            tablingLookups++;
        }
        return tablingCache;
    }
    // END_CHANGE: ISS-2025-0776

    /** @param overriding the goal names a registry built-in that the context overrides with
     *  clauses (maplist/3 inside library(apply)); that answer is covered by the same stamp. */
    private void installCallSite(Goal g, String f, int n, boolean overriding) {
        // START_CHANGE: ISS-2025-0777 - 4.6 wave Q6.1: a goal built at run time (call/N, the goal
        // of findall/forall/\+/once, a top-level query, a maplist closure) has no skeleton to hold
        // its site; it is remembered in the machine's per-functor cache instead (rtSites).
        boolean skel = g.term instanceof Clause.Skel && g.frame != null;
        // END_CHANGE: ISS-2025-0777
        if (SITE_EXCLUDED.contains(f)) return;
        long stamp = engine.dispatchStamp();                 // read BEFORE the probes it covers
        if (engine.natives().lookup(f, n) != null) return;
        if (!overriding && engine.registry() != null && engine.registry().isBuiltIn(f, n)) return;
        if (engine.tables() != null && engine.tables().isTabled(f, n)) return;
        if (g.module == null && ctxModule == null) {
            CallSite site = new CallSite(engine, stamp, engine.store().lookup(f, n));
            if (skel) ((Clause.Skel) g.term).site = site;
            else rtStore(f, n, site);                                  // ISS-2025-0777
            return;
        }
        // A body goal of a MODULE clause (library(apply)'s maplist recursion, ...): cached only
        // when the context module itself defines the indicator — the first step of
        // selectClauses. Imports, `user` and autoload keep the per-call resolution.
        if (ctxModule == null || !ctxModule.equals(g.module)) return;
        Modules ms = engine.modules4();
        Modules.Pred mp = ms.localPred(ctxModule, f, n);
        if (mp == null) return;
        CallSite msite = new CallSite(engine, stamp, null, ctxModule, mp, ms.metaSpec(ctxModule, f, n));
        if (skel) ((Clause.Skel) g.term).site = msite;
        else rtStore(f, n, msite);                                     // ISS-2025-0777
    }

    // START_CHANGE: ISS-2025-0777 - 4.6 wave Q6.1: the per-functor call-site cache of goals that
    // are built at run time. Direct-mapped by (name, arity); an entry is the same immutable
    // CallSite a body skeleton holds, so it carries the context module it was resolved in and the
    // dispatch stamp that validates it — a stale or colliding entry just takes the slow path and is
    // overwritten. Bounded (RT_SITES entries), per machine (a machine is single-threaded).
    private static final int RT_SITES = 256;
    private String[] rtName;
    private int[] rtArity;
    private CallSite[] rtSite;
    /** Number of run-time goals that took the cached path (test hook). */
    long rtSiteHits;

    private static int rtSlot(String f, int n) {
        int h = f.hashCode() * 31 + n;
        return (h ^ (h >>> 16)) & (RT_SITES - 1);
    }

    private void rtStore(String f, int n, CallSite site) {
        if (rtName == null) {
            rtName = new String[RT_SITES];
            rtArity = new int[RT_SITES];
            rtSite = new CallSite[RT_SITES];
        }
        int i = rtSlot(f, n);
        rtName[i] = f;
        rtArity[i] = n;
        rtSite[i] = site;
    }

    /** The cached site of a run-time goal {@code f/n} in the current context, or null. */
    private CallSite rtLookup(String f, int n, String module) {
        if (rtName == null) return null;
        int i = rtSlot(f, n);
        String nm = rtName[i];
        if (nm == null || rtArity[i] != n || !(nm == f || nm.equals(f))) return null;
        CallSite site = rtSite[i];
        if (site.engine != engine || site.stamp != engine.dispatchStamp()) return null;
        if (site.module == null ? module != null : !site.module.equals(module)) return null;
        return site;
    }
    // END_CHANGE: ISS-2025-0777

    /** callUser for a cached MODULE site: the context module's own clauses for the indicator. */
    private boolean callModuleSite(CallSite site, Term t) {
        Clause[] cs = site.modPred.select(Clause.argKey1(t));
        if (cs.length == 0) return callUser(t, t);            // selectClauses moves on: imports...
        selModule = site.module;
        Term u = (site.meta == null) ? t : Modules.qualifyMetaArgs(t, site.meta, contextModule());
        return activate(u, t, cs, 0, cs.length, site.module, t);
    }

    /** callUser for a cached site: context `user`, not tabled, not a built-in. */
    private boolean callSite(ClauseStore.Predicate p, Term t) {
        p.sync(engine.kb());
        if (p.size() == 0) return callUser(t, t);            // imports, autoload, unknown procedure
        p.view(Clause.argKey1(t), view);
        selModule = null;
        return activate(t, t, view.a, view.from, view.to, null, t);
    }
    // END_CHANGE: ISS-2025-0540

    /** Run a native v4 built-in with the four ports. Returns 1 = succeeded, 0 = failed. */
    private Integer callNative(Builtin nat, Term goal, String f, int n, Term[] args) {
        final int dd = debugTraceActive() ? enterPort() : -1;
        if (dd >= 0) portCall(goal, dd);
        String prev = currentContext;
        int prevArity = currentContextArity;                  // ISS-2025-0778
        currentContext = f;
        currentContextArity = n;
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
            currentContextArity = prevArity;                   // ISS-2025-0778
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

    // START_CHANGE: ISS-2025-0523 - wave P1.10: the teardown cut of an ABANDONED run.
    /**
     * Pop every choice point above {@code floor} running the pending cleanups, for a run that is
     * being abandoned: the top-level query when the sink stopped it or an exception is leaving it,
     * and a nested drive (findall/3, once/1, a sub-query) that a control exception — the inference
     * budget, a Stop interrupt, the debugger's Stop — is unwinding through.
     *
     * <p>Unlike {@link #cut} it never lets a cleanup's exception out: something else is already
     * propagating (or the caller asked to stop), and that is what the caller must see. A cleanup
     * therefore runs with the debugger detached (a pending debugger Stop would abort it at its first
     * port) and with the thread's interrupt flag cleared and then restored (a cancellation would
     * abort it at its first poll); the budget grants it a small allowance of its own (see
     * {@code ResourceGuard}). A {@code PrologException} ball cannot leave frames behind — the
     * unwinding in {@link #handleBall} already ran their cleanups at pop time — so this only ever
     * sees the frames a control exception or an early stop left.
     */
    private void cutQuietly(int floor) {
        DebugController savedDc = debugController;
        boolean interrupted = Thread.interrupted();
        debugController = null;
        try {
            while (cps.size() > floor) {
                CP top = popCP();
                try {
                    if (top.tframe != null) top.tframe.discard();
                    if (top.kind == CP.CLEANUP && !top.cleanupDone) {
                        top.cleanupDone = true;
                        runOnce(top.cleanup);
                    } else if (top.kind == CP.GEN && top.gen instanceof GeneratorGen) {
                        ((GeneratorGen) top.gen).generator.cut();
                    }
                } catch (RuntimeException e) {
                    // deliberately swallowed: a cleanup cannot replace the exception (or the stop)
                    // that is abandoning this run
                } catch (StackOverflowError e) {
                    // idem
                }
            }
            B.clearIfUnreachable(cps.isEmpty());
        } finally {
            debugController = savedDc;
            if (interrupted) Thread.currentThread().interrupt();
        }
    }
    // END_CHANGE: ISS-2025-0523

    private void disjunction(final Term left, final Term right, final int cutBarrier) {
        disjunction(left, right, cutBarrier, null);
    }

    private void disjunction(final Term left, final Term right, final int cutBarrier, final Term[] fr) {
        final Goal cont = goalStack;
        final String mod = ctxModule;                          // ISS-2025-0466
        CP cp = new CP(CP.GEN, B.mark());
        final int[] which = {0};
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                if (which[0] == 0) { which[0] = 1; return mgf(left, fr, cutBarrier, cont, mod); }
                if (which[0] == 1) { which[0] = 2; self.genExhausted = true; return mgf(right, fr, cutBarrier, cont, mod); }
                return EXHAUSTED;
            }
        };
        pushCP(cp);
        advance(cp);
    }

    /** {@code (Cond -> Then ; Else)}: commit to Cond's first solution. A user {@code !} inside Cond
     *  is local (its barrier is ABOVE this choice point). */
    private void ite(final Term cond, final Term then, final Term els, final int cutBarrier) {
        ite(cond, then, els, cutBarrier, null);
    }

    private void ite(final Term cond, final Term then, final Term els, final int cutBarrier, final Term[] fr) {
        final Goal cont = goalStack;
        final int barrier = cps.size();
        final Goal alt1 = mgf(cond, fr, barrier + 1, mg(CUT, barrier, mgf(then, fr, cutBarrier, cont)));
        final Goal alt2 = mgf(els, fr, cutBarrier, cont);
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
        final CP pf = portFrame;
        final Goal exitMark = new Goal(new Runnable() {
            @Override public void run() {
                portExit(goal, d);
                popIfDeterministicTop(pf);                      // ISS-2025-0668
            }
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
        softCut(cond, then, els, cutBarrier, null);
    }

    private void softCut(final Term cond, final Term then, final Term els, final int cutBarrier, final Term[] fr) {
        final Goal cont = goalStack;
        final boolean[] found = {false};
        final String mod = ctxModule;                          // ISS-2025-0466
        final Goal alt1 = mgf(cond, fr, cps.size() + 1, new Goal(new Runnable() {
            @Override public void run() { found[0] = true; }
        }, mgf(then, fr, cutBarrier, cont)));
        CP cp = new CP(CP.GEN, B.mark());
        final int[] which = {0};
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                if (which[0] == 0) { which[0] = 1; return alt1; }
                if (which[0] == 1) { which[0] = 2; self.genExhausted = true; return found[0] ? FAILED : mgf(els, fr, cutBarrier, cont, mod); }
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
        checkBodyCtx(d, ctx);                                   // ISS-2025-0518
    }
    // END_CHANGE: ISS-2025-0509

    // START_CHANGE: ISS-2025-0518 - wave P1.5: ISO 7.6.2 converts a term to a body BEFORE it runs,
    // and a number (or string) anywhere in its control spine — the `,`/`;`/`->`/`*->`/`\+`
    // structure — makes the conversion fail with type_error(callable, Goal) for the WHOLE goal.
    // The machine used to discover the bad leaf only when it reached it, so
    // `call((write(a), nl, 1))` printed `a` first and `call((fail, 1))` simply failed. SWI raises in
    // both cases. Variables in the spine are fine (each is call(V) and is checked when reached).
    // The check is a spine walk only when the goal's principal functor IS a control construct, so
    // an ordinary `call(foo(X))` pays one functor comparison.
    /** Check the control spine of {@code goal}; context {@code name/arity} of the construct. */
    void checkBody(Term goal, String name, int arity) {
        // ISS-2025-0777: the "name/arity" context is built only when there is something to check
        // (it was concatenated on every call/N, once/1, findall/3, ...)
        Term d = Unify.deref(goal);
        if (!(d instanceof CompoundTerm) || !isControlConstruct((CompoundTerm) d)) return;
        checkBodyCtx(d, name + "/" + arity);
    }

    private void checkBodyCtx(Term d, String ctx) {
        if (!(d instanceof CompoundTerm) || !isControlConstruct((CompoundTerm) d)) return;
        // a goal that IS \+ G is a call of \+/1, which checks G itself with G as the culprit
        // (SWI: catch(\+ (fail ; 1), E, true) gives type_error(callable, (fail ; 1)))
        if (((CompoundTerm) d).getArguments().size() == 1) return;
        if (!bodyConvertible((CompoundTerm) d)) throw Errors.type("callable", resolve(d), ctx);
    }

    private static boolean isControlConstruct(CompoundTerm c) {
        int n = c.getArguments().size();
        String f = c.getName();
        if (n == 2) return ",".equals(f) || ";".equals(f) || "->".equals(f) || "*->".equals(f)
            || "|".equals(f);                                   // ISS-2025-0734
        return n == 1 && "\\+".equals(f);
    }

    /** False when a non-callable non-variable occurs in the control spine. Iterative and
     *  cycle-safe (a cyclic spine is walked once per node). */
    private boolean bodyConvertible(CompoundTerm root) {
        ArrayList<Term> work = new ArrayList<Term>();
        work.add(root);
        IdentityHashMap<Term, Boolean> seen = null;
        int n = 0;
        while (!work.isEmpty()) {
            Term t = Unify.deref(work.remove(work.size() - 1));
            if ((++n & 0xFFF) == 0) guard.step();
            if (t instanceof Variable || t instanceof Atom) continue;
            if (!(t instanceof CompoundTerm)) return false;           // number, string, ...
            CompoundTerm c = (CompoundTerm) t;
            if (!isControlConstruct(c)) continue;
            if (n > 1024) {
                if (seen == null) seen = new IdentityHashMap<Term, Boolean>();
                if (seen.put(c, Boolean.TRUE) != null) continue;
            }
            List<Term> as = c.getArguments();
            for (int i = as.size() - 1; i >= 0; i--) work.add(as.get(i));
        }
        return true;
    }
    // END_CHANGE: ISS-2025-0518

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
        runOnceCalls++;                                        // ISS-2025-0779: test hook
        Goal saved = goalStack;
        String savedMod = ctxModule;                           // ISS-2025-0466
        int floor = cps.size();
        goalStack = mg(goal, floor, null);
        final boolean[] found = {false};
        boolean normal = false;                                // ISS-2025-0523
        try {
            drive(new Driver() {
                @Override public boolean onSolution() { found[0] = true; return false; }
            }, floor);
            normal = true;
        } finally {
            if (normal) cutTo(floor); else cutQuietly(floor);  // ISS-2025-0523
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
        boolean normal = false;                                // ISS-2025-0523
        try {
            drive(new Driver() {
                @Override public boolean onSolution() {
                    any[0] = true;
                    return sink.onSolution(snapshot(names, cells));
                }
            }, floor);
            normal = true;
        } finally {
            // START_CHANGE: ISS-2025-0448 - the undo MUST happen while the forced-trail extent is
            // still open. cutTo() ends in Bindings.clearIfUnreachable(), which drops the whole
            // trail when forceTrail == 0 and no choice point is left — so decrementing first threw
            // away exactly the entries this undo needs, and the sub-query's bindings survived it.
            if (normal) cutTo(floor); else cutQuietly(floor);  // ISS-2025-0523
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
        // START_CHANGE: ISS-2025-0521 - wave P1.8: every mode is native, lazy and exact.
        // The generator used to test `next > last` AFTER incrementing, so with last ==
        // Long.MAX_VALUE the counter wrapped to Long.MIN_VALUE and never stopped
        // (`between(9223372036854775806, 9223372036854775807, X)` enumerated forever, and
        // `between(9223372036854775807, inf, X)` wrapped negative); and a bound outside the long
        // range, or a bound third argument, fell through to the EAGER legacy Between, which
        // materialised the whole range (an OOM after 17 s). Now: a long counter that stops exactly
        // at its last value, continuing into BigInteger only when the upper bound lies beyond
        // Long.MAX_VALUE (or is inf); BigInteger bounds counted lazily; a bound third argument is
        // a plain range test.
        final boolean inf = (hi instanceof Atom);                 // `inf` / `infinite` (checked above)
        final Number loN = (Number) lo;
        final java.math.BigInteger bhi = inf ? null : ((Number) hi).bigIntegerValue();
        final boolean traced = debugTraceActive();
        final int depth = traced ? enterPort() : -1;
        if (traced) portCall(goal, depth);
        if (!(v instanceof Variable)) {                           // between(L, H, +X): a range test
            java.math.BigInteger x = ((Number) v).bigIntegerValue();
            boolean in = x.compareTo(loN.bigIntegerValue()) >= 0 && (inf || x.compareTo(bhi) <= 0);
            if (traced) { if (in) portExit(goal, depth); else portFail(goal, depth); }
            return in ? 1 : 0;
        }
        if (!inf && loN.bigIntegerValue().compareTo(bhi) > 0) { if (traced) portFail(goal, depth); return 0; }
        // END_CHANGE: ISS-2025-0521
        // START_CHANGE: ISS-2025-0481 - committed to the native generator: from here the four
        // ports are the machine's, so between/3 no longer has to be routed through the bridge while
        // tracing (limit L-13). Same shape as lengthEnumerate below.
        final Term value = v;
        final Goal cont = goalStack;
        final Goal after = traced ? new Goal(new Runnable() {
            @Override public void run() { portExit(goal, depth); }
        }, cont) : cont;
        // START_CHANGE: ISS-2025-0521
        final boolean hiFits = !inf && ((Number) hi).fitsInLong();
        final long lastLong = hiFits ? ((Number) hi).longValue() : Long.MAX_VALUE;
        final boolean pastLong = !hiFits;                         // the range goes beyond Long.MAX_VALUE
        final long[] cur = {loN.fitsInLong() ? loN.longValue() : 0L};
        final java.math.BigInteger[] big = {loN.fitsInLong() ? null : loN.bigIntegerValue()};
        final boolean[] done = {false};
        CP cp = new CP(CP.GEN, B.mark());
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                if (done[0]) return EXHAUSTED;
                Term n;
                if (big[0] == null) {                             // long mode
                    long i = cur[0];
                    if (i == lastLong) {
                        if (pastLong) big[0] = java.math.BigInteger.valueOf(i).add(java.math.BigInteger.ONE);
                        else { done[0] = true; self.genExhausted = true; }
                    } else {
                        cur[0] = i + 1;
                    }
                    n = Number.valueOf(i);
                } else {                                          // BigInteger mode
                    java.math.BigInteger i = big[0];
                    java.math.BigInteger nx = i.add(java.math.BigInteger.ONE);
                    if (bhi != null && nx.compareTo(bhi) > 0) { done[0] = true; self.genExhausted = true; }
                    big[0] = nx;
                    n = (i.bitLength() <= 63) ? Number.valueOf(i.longValue()) : new Number(i);
                }
                return Unify.unify(value, n, B) ? after : FAILED;
            }
        };
        // END_CHANGE: ISS-2025-0521
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
                guard.charge(k);          // ISS-2025-0624: this solution builds a k-element list
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
                // ISS-2025-0594 - P4.5: errors name the comparison (f/2), not is/2
                return numRel(f, evalCmp(a.get(0), f), evalCmp(a.get(1), f)) ? 1 : 0;
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
            if ("float".equals(f)) return (x instanceof Number && ((Number) x).isFloat()) ? 1 : 0;   // ISS-2025-0712: 1r3 is no float
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

    // START_CHANGE: ISS-2025-0594 - P4.5: evaluation in the context of the calling predicate
    /** Evaluate for a built-in whose errors must name {@code context} (e.g. "sum_list/2"). */
    Number evalNum(Term t, String context) {
        return it.denzosoft.jprolog.core.arith.v2.ArithEvaluator.evalDeref(t, derefFn, context);
    }

    /** Evaluate one side of the comparison {@code op}/2; the context string is built only when
     *  an error is raised. */
    private Number evalCmp(Term t, String op) {
        return it.denzosoft.jprolog.core.arith.v2.ArithEvaluator.evalCompare(t, derefFn, op);
    }
    // END_CHANGE: ISS-2025-0594

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
        // START_CHANGE: ISS-2025-0712 - an integer/rational pair compares exactly (1r3 =\= 0.333..
        // is about floats; 1r3 < 1 is not)
        if (!a.isFloat() && !b.isFloat()) {
            int c = it.denzosoft.jprolog.core.terms.Rational.compareExact(a, b);
            if ("<".equals(op)) return c < 0;
            if (">".equals(op)) return c > 0;
            if ("=<".equals(op)) return c <= 0;
            if (">=".equals(op)) return c >= 0;
            if ("=:=".equals(op)) return c == 0;
            return c != 0;
        }
        // END_CHANGE: ISS-2025-0712
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
        final List<Term> results = new ArrayList<Term>();
        forEachSolution(goal, new SolutionVisitor() {
            @Override public boolean visit() {
                results.add(Unify.copy(template, new IdentityHashMap<Variable, Variable>(), guard));
                return true;
            }
        });
        return results;
    }

    // START_CHANGE: ISS-2025-0522 - wave P1.9: the drive behind findAll, with a per-solution
    // callback instead of a result list, so aggregate_all(count/sum/max/min) accumulates in O(1)
    // memory instead of materialising every solution first.
    /** Called once per solution of {@link #forEachSolution}, while its bindings are live. */
    interface SolutionVisitor {
        /** @return false to stop enumerating */
        boolean visit();
    }

    /**
     * Enumerate the solutions of {@code goal} on this machine as an OPAQUE sub-run: the visitor
     * sees each solution's bindings, and none of them survive the call.
     */
    void forEachSolution(Term goal, final SolutionVisitor visitor) {
        Goal savedGoals = goalStack;
        String savedMod = ctxModule;                           // ISS-2025-0466
        int floor = cps.size();
        int m = B.mark();
        goalStack = mg(goal, floor, null);
        B.forceTrail++;
        boolean normal = false;                                // ISS-2025-0523
        try {
            drive(new Driver() {
                @Override public boolean onSolution() { return visitor.visit(); }
            }, floor);
            normal = true;
        } finally {
            // START_CHANGE: ISS-2025-0448 - findall/3 is OPAQUE: none of Goal's bindings may
            // survive it. Undo BEFORE closing the forced-trail extent — cutTo() ends in
            // Bindings.clearIfUnreachable(), which wipes the trail once forceTrail is back to 0 and
            // no choice point is left, so the old order left the template variable bound to the
            // LAST solution (`findall(X, member(X,[1,2]), L), X == 2` succeeded).
            if (normal) cutTo(floor); else cutQuietly(floor);  // ISS-2025-0523
            B.undo(m);
            B.forceTrail--;
            // END_CHANGE: ISS-2025-0448
            goalStack = savedGoals;
            ctxModule = savedMod;
        }
    }
    // END_CHANGE: ISS-2025-0522

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
                // START_CHANGE: ISS-2025-0528 - wave P1.15: the goals the ball unwound get an
                // Exception port, and the depth returns to the catch/3 call's own level — the
                // recovery used to be traced one level too deep, inside the goal that threw.
                unwindPorts(top.catchDepth);
                // END_CHANGE: ISS-2025-0528
                // START_CHANGE: ISS-2025-0516 - wave P1.3: Recovery runs as call(Recovery)
                // (ISO 7.8.9), so a `!` in it is LOCAL. It used to get the catch/3 goal's own
                // barrier, i.e. it cut the clause that called catch/3: `r(X) :- catch(throw(x),
                // x, (X = 1, !)). r(2).` lost its second clause. Going through call/1 also gives
                // the recovery the body check of ISS-2025-0518 inside the drive loop, where an
                // error it raises is routed like any other.
                goalStack = mg(new CompoundTerm(ATOM_CALL, java.util.Collections.singletonList(top.recovery)),
                               cps.size(), top.cont, top.module);
                // END_CHANGE: ISS-2025-0516
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

    // START_CHANGE: ISS-2025-0777 - 4.6 wave Q6.1: call/N appends its extra arguments into ONE
    // array (no ArrayList, no List-to-array copy); the result adopts it.
    /** {@code goal} with {@code src[from..to)} appended as extra arguments (call/N). */
    Term addArgs(Term goal, CompoundTerm src, int from, int to) {
        int k = to - from;
        if (goal instanceof Atom) {
            Term[] out = new Term[k];
            for (int i = 0; i < k; i++) out[i] = src.arg(from + i);
            return new CompoundTerm((Atom) goal, out);
        }
        if (goal instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) goal;
            if ((":".equals(c.getName()) || Modules.MCTX.equals(c.getName())) && c.arity() == 2) {
                return new CompoundTerm(c.getFunctor(), new Term[] {
                    c.arg(0), addArgs(Unify.deref(c.arg(1)), src, from, to)});
            }
            int m = c.arity();
            Term[] out = new Term[m + k];
            for (int i = 0; i < m; i++) out[i] = c.arg(i);
            for (int i = 0; i < k; i++) out[m + i] = src.arg(from + i);
            return new CompoundTerm(c.getFunctor(), out);
        }
        return goal;
    }
    // END_CHANGE: ISS-2025-0777

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
        // START_CHANGE: ISS-2025-0733 - inside module M, a predicate stored as `M:H` clauses (a
        // qualified clause written outside M, or assertz(M:H)) is M's own predicate
        if (clauses == null && !flat && ctxModule != null && hasFlatQualified(ctxModule, g0)) {
            Term q = new CompoundTerm(new Atom(":"), new Term[] {new Atom(ctxModule), g0});
            return callUser(q, q, true);
        }
        // END_CHANGE: ISS-2025-0733
        return activate(withMetaContext(g0, unifyGoal, selModule), unifyGoal,
                        clauses, selFrom, selLimit, selModule, lookup);      // ISS-2025-0546: a window
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
        return activate(withMetaContext(g0, unifyGoal, dm), traceGoal, cs, 0, cs.length, dm, null);
    }

    /**
     * Install the clause frame: the four ports, the choice point and the clause bodies, whose
     * goals run in {@code defMod} — that is what makes a library predicate's {@code call/N} and a
     * module predicate's helpers resolve in the right place.
     *
     * @param lookupForUnknown non-null to apply the {@code unknown} flag when there is no procedure
     */
    private boolean activate(Term unifyGoal, Term traceGoal, Clause[] clauses, final int from,
                             final int limit, final String defMod, Term lookupForUnknown) {
        // START_CHANGE: ISS-2025-0780 - 4.6 wave Q6.2: library(apply)'s maplist/foldl/include/
        // exclude/partition, reached through ordinary resolution (so a user or module definition
        // of the same name still wins), run as a native frame with the same ports and choice
        // points as their two clauses (NativeApply).
        if (clauses != null && limit - from == 2 && clauses[from].apply != null) {
            return NativeApply.enter(this, (NativeApply.Op) clauses[from].apply,
                                     Unify.deref(unifyGoal), traceGoal, defMod);
        }
        // END_CHANGE: ISS-2025-0780
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
        if (tracing || debugging) noteOpen(g, tdepth);                 // ISS-2025-0528
        if (tracing) tracePort("Call", g, tdepth);
        if (debugging) debugPort(DebugEvent.Port.CALL, g, tdepth);

        if (clauses == null || limit <= from) {
            if (clauses == null && lookupForUnknown != null) raiseUnknownIfRequired(lookupForUnknown);
            if (tracing || debugging) portDepth = tdepth;          // ISS-2025-0482
            if (tracing) tracePort("Fail", g, tdepth);
            if (debugging) debugPort(DebugEvent.Port.FAIL, g, tdepth);
            return false;
        }
        // START_CHANGE: ISS-2025-0779 - 4.6 wave Q6.4/Q6.5: ONE candidate clause and no port to
        // report — the choice point would be pushed only to be trust-me popped at once (no
        // alternative, no traced frame to keep for Exit), so the head is unified and the body
        // pushed directly. Same barrier (the height before the frame), same logical-update view
        // (the clause must be alive in the current generation); a failed head unification leaves
        // its partial bindings to the backtrack that follows, exactly as a popped frame would.
        // A traced/debugged call keeps the frame (its Exit/Fail/Redo bookkeeping lives there).
        if (!(tracing || debugging) && limit - from == 1) {
            Clause cl = clauses[from];
            if (!cl.isAlive(engine.store().generation())) return false;
            Term[] frame = (cl.nvars == 0) ? LegacyBuiltinAdapter.NO_FRAME : new Term[cl.nvars];
            if (!cl.unifyHead(unifyGoal, frame, B)) return false;
            cl.fillBodySlots(frame);
            Goal after = goalStack;
            int bar = cps.size();
            Term[] body = cl.body;
            for (int i = body.length - 1; i >= 0; i--) {
                Goal bg = new Goal(body[i], frame, bar, after);
                bg.module = defMod;
                after = bg;
            }
            goalStack = after;
            singleClauseActivations++;
            return true;
        }
        // END_CHANGE: ISS-2025-0779
        final Goal cont = goalStack;
        final int barrier = cps.size();
        final boolean ftrace = tracing, fdebug = debugging;
        CP cp = new CP(CP.CLAUSES, B.mark());
        cp.goal = unifyGoal;
        cp.clauses = clauses;
        cp.limit = limit;
        cp.idx = from;                                                // ISS-2025-0546
        cp.generation = engine.store().generation();
        cp.cont = cont;
        cp.barrier = barrier;
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                while (self.idx < self.limit) {
                    Clause cl = self.clauses[self.idx++];
                    if (!cl.isAlive(self.generation)) continue;          // logical update view
                    if (self.idx >= self.limit) self.genExhausted = true;
                    // START_CHANGE: ISS-2025-0668 - while tracing, look ahead: when no remaining
                    // live clause can match the goal (a cheap per-argument key clash, checked
                    // BEFORE this clause binds anything), this alternative is the last one and the
                    // frame is deterministic — it must not print a Redo/Fail later. SWI gets the
                    // same effect from its argument indexing (maplist(G, [], []) has no Fail).
                    // START_CHANGE: ISS-2025-0715 - wave Q2.4: the look-ahead runs ALWAYS, not only
                    // while tracing. The first-argument index cannot tell `foldl(_, [], A, A)` from
                    // `foldl(G, [X|Xs], A0, A)` (the closure is the first argument), so the last
                    // step of every apply-library recursion left a choice point behind and
                    // `foldl([X,A0,A]>>(A is A0+X), [1,2,3], 0, S)` answered `S = 6 ;` / `false`
                    // where SWI (JIT multi-argument indexing) is deterministic. It also jumps the
                    // cursor over clauses that cannot match, so it costs no extra head attempts.
                    if (!self.genExhausted) {
                        int nx = nextMayMatch(self);
                        if (nx < 0) { self.genExhausted = true; self.idx = self.limit; }
                        else self.idx = nx;
                    }
                    // END_CHANGE: ISS-2025-0715
                    // END_CHANGE: ISS-2025-0668
                    Term[] frame = (cl.nvars == 0) ? LegacyBuiltinAdapter.NO_FRAME : new Term[cl.nvars];
                    if (!cl.unifyHead(self.goal, frame, B)) return FAILED;
                    cl.fillBodySlots(frame);                             // ISS-2025-0551
                    Goal after = self.cont;
                    if (ftrace || fdebug) {
                        after = new Goal(new Runnable() {
                            @Override public void run() {
                                portDepth = tdepth;             // ISS-2025-0482
                                if (ftrace) tracePort("Exit", g, tdepth);
                                if (fdebug) debugPort(DebugEvent.Port.EXIT, g, tdepth);
                                popIfDeterministicTop(self);    // ISS-2025-0668
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
     * are ordinary resolution in that context; and a known module runs its own definition,
     * exported or not (ISS-2025-0611 reversed the export enforcement of ISS-2025-0314).
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
            // START_CHANGE: ISS-2025-0611 - P4.17: assertz(m3:k(1)) stores a flat m3:k(1) clause
            // (clause/2 finds it); a call m3:k(X) must find it too instead of raising
            // existence_error(procedure, k/1) in the (nonexistent) module's context.
            if (!Modules.USER.equals(mod) && flatQualified(qc)) return true;
            // END_CHANGE: ISS-2025-0611
            goalStack = mg(inner, cps.size(), goalStack, modKey(mod));
            return true;
        }
        Clause[] own = ms.localClauses(mod, name, n,
            (inner instanceof CompoundTerm && n > 0)
                ? Clause.argKey(Unify.deref(((CompoundTerm) inner).getArguments().get(0))) : null);
        if (own != null) {                                   // ISS-2025-0770: an index miss fails
            // START_CHANGE: ISS-2025-0611 - P4.17 (decision §8): the module DEFINES it, so it runs
            // there whether or not it is exported — export only governs IMPORT (SWI). The
            // ISS-2025-0314 export check on qualified calls is reversed.
            return callInModule(inner, qc, own, mod);
            // END_CHANGE: ISS-2025-0611
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

    // START_CHANGE: ISS-2025-0733
    /** Is there an {@code M:H} clause (flat store) for {@code mod} and the predicate of {@code goal}? */
    boolean hasFlatQualified(String mod, Term goal) {
        ClauseStore.Predicate p = engine.store().lookup(":", 2);
        if (p.size() == 0) return false;
        String f;
        int n;
        if (goal instanceof Atom) { f = ((Atom) goal).getName(); n = 0; }
        else if (goal instanceof CompoundTerm) { f = ((CompoundTerm) goal).getName(); n = ((CompoundTerm) goal).arity(); }
        else return false;
        Clause[] all = p.all();
        for (int i = 0; i < all.length; i++) {
            Term h = all[i].head;
            if (!(h instanceof CompoundTerm) || ((CompoundTerm) h).arity() != 2) continue;
            Term mt = ((CompoundTerm) h).arg(0);
            Term g = ((CompoundTerm) h).arg(1);
            if (!(mt instanceof Atom) || !mod.equals(((Atom) mt).getName())) continue;
            if (g instanceof Atom && n == 0 && f.equals(((Atom) g).getName())) return true;
            if (g instanceof CompoundTerm && ((CompoundTerm) g).arity() == n && f.equals(((CompoundTerm) g).getName())) return true;
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0733

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
    // START_CHANGE: ISS-2025-0546 - the selection is a WINDOW [selFrom, selLimit) over a clause
    // store gap buffer (no copy); filled through one reused View.
    private int selFrom;
    private final ClauseStore.View view = new ClauseStore.View();
    // END_CHANGE: ISS-2025-0546
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
            selFrom = 0;
            selLimit = 0;
            selModule = null;
            return null;
        }
        selModule = null;
        selFrom = 0;
        Modules ms = flat ? null : engine.modules4();

        // 1./2. the context module and its imports (skipped entirely for the `user` context,
        //       which is the overwhelmingly common case and costs one null test)
        if (ms != null && ctxModule != null) {
            Clause[] own = ms.localClauses(ctxModule, f, ar, key);
            if (own != null) { selLimit = own.length; selModule = ctxModule; return own; }   // ISS-2025-0770
            Modules.Hit h = ms.fromImports(ctxModule, f, ar, key);
            if (h != null) { selLimit = h.clauses.length; selModule = modKey(h.module); return h.clauses; }
        }

        // 3. `user` — the flat clause store, with the first-argument index
        ClauseStore.Predicate p = engine.store().lookup(f, ar);
        if (p.size() > 0) {
            // ISS-2025-0546: a window over the store's gap buffer, never a copy (see View)
            p.view(key, view);
            selFrom = view.from;
            selLimit = view.to;
            // START_CHANGE: ISS-2025-0733 - the body of an `M:H` clause runs in module M
            if (ar == 2 && ":".equals(f)) {
                Term qm = Unify.deref(((CompoundTerm) goal).arg(0));
                if (qm instanceof Atom) selModule = modKey(((Atom) qm).getName());
            }
            // END_CHANGE: ISS-2025-0733
            return view.a;
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
        // START_CHANGE: ISS-2025-0730 - a multifile predicate is defined: a call with no clause
        // fails (SWI), in user or, for `:- multifile m:p/1`, in module m
        if (engine.kb().isDeclared(f, ar)) return;
        if (ctxModule != null && engine.kb().isQualifiedDeclared(ctxModule, f, ar)) return;
        // END_CHANGE: ISS-2025-0730
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
        final Tabling tb = tablingHere();          // ISS-2025-0752 / ISS-2025-0776: cached per machine
        tb.enterCall();
        try {
            return callTabledClaimed(tb, unifyGoal, lookup, g0);
        } finally {
            tb.exitCall();
        }
    }

    private boolean callTabledClaimed(final Tabling tb, Term unifyGoal, Term lookup, Term g0) {
        final boolean tracing = it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled();
        final boolean debugging = debugPortsActive();
        final int tdepth = (tracing || debugging) ? enterPort() : 0;   // ISS-2025-0482
        if (tracing || debugging) noteOpen(unifyGoal, tdepth);         // ISS-2025-0528
        if (tracing) tracePort("Call", unifyGoal, tdepth);
        if (debugging) debugPort(DebugEvent.Port.CALL, unifyGoal, tdepth);

        // START_CHANGE: ISS-2025-0754 - 4.6 wave Q4.4: a mode-directed table is evaluated with its
        // MODED arguments free (SWI); the caller's bound value then filters the aggregated answer.
        if (engine.tables().hasModes()) g0 = freeModedArguments(g0);
        // END_CHANGE: ISS-2025-0754
        String key = Tabling.variantKey(g0, guard);
        Tabling.Table table = tb.get(key);
        if (table == null) table = tb.getShared(key);                  // ISS-2025-0752: shared, complete
        boolean produce;
        if (table == null) {
            produce = true;
        } else if (table.status == Tabling.COMPLETE) {
            produce = false;
        } else {
            // START_CHANGE: ISS-2025-0661 - an incomplete table older than the innermost negation
            if (negTableFloor >= 0 && table.seq <= negTableFloor) {
                throw Errors.permission("negate", "incomplete_table", resolve(unifyGoal), "\\+/1");
            }
            // END_CHANGE: ISS-2025-0661
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
            if (selFrom != 0) {                                   // ISS-2025-0546: 0-based copy
                clauses = Arrays.copyOfRange(clauses, selFrom, selLimit);
                limit = clauses.length;
            }
            if (table == null) {
                String ind = Tabling.indicatorOf(g0);
                table = tb.create(key, ind,
                    Unify.copy(g0, new IdentityHashMap<Variable, Variable>(), guard),
                    engine.tables().isShared(ind));                               // ISS-2025-0752
                table.modes = engine.tables().getModes(table.indicator);   // ISS-2025-0572
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

    // START_CHANGE: ISS-2025-0755 - 4.6 wave Q4.5: minimal well-founded semantics.
    /** The delayed literals the current derivation depends on (null: unconditional). Trailed. */
    private Tabling.Delay delays;
    /** The delay list of the last top-level answer, as a conjunction (null: unconditional). */
    private Term lastAnswerDelays;

    Tabling.Delay delays() { return delays; }

    /** Replace the delay list; backtracking restores the old one. */
    void setDelays(Tabling.Delay d) {
        final Tabling.Delay old = delays;
        if (old == d) return;
        delays = d;
        B.pushUndo(new Runnable() { @Override public void run() { delays = old; } });
    }

    void addDelay(Tabling.Delay lit) { setDelays(lit.push(delays)); }

    /**
     * The delayed literals of the answer just handed to the solution sink, as a conjunction —
     * non-null means the answer is CONDITIONAL, i.e. undefined in the well-founded semantics.
     */
    public Term answerDelays() { return lastAnswerDelays; }

    /**
     * {@code tnot(G)} for a tabled {@code G} (SWI's algorithm, on linear tabling): an unconditional
     * answer of G makes it fail; no answer in a COMPLETE table makes it succeed; otherwise — G's
     * table is still being evaluated by an ancestor (a loop through negation) or holds only
     * conditional answers — it succeeds with {@code tnot(G)} DELAYED, and the SCC's completion
     * simplifies the resulting conditional answers.
     */
    boolean tnot(Term goal) {
        Term g = Unify.deref(goal);
        if (g instanceof CompoundTerm && ":".equals(((CompoundTerm) g).getName()) && ((CompoundTerm) g).arity() == 2) {
            g = Unify.deref(((CompoundTerm) g).arg(1));
        }
        if (g instanceof Variable) throw Errors.instantiation("tnot/1");
        if (!(g instanceof Atom) && !(g instanceof CompoundTerm)) throw Errors.type("callable", g, "tnot/1");
        if (engine.tables() == null || !isTabled(g)) {
            String n = (g instanceof Atom) ? ((Atom) g).getName() : ((CompoundTerm) g).getName();
            int ar = (g instanceof Atom) ? 0 : ((CompoundTerm) g).arity();
            throw Errors.permission("tnot", "non_tabled_procedure", Errors.pi(n, ar), "tnot/1");
        }
        Tabling tb = engine.tabling();
        String key = Tabling.variantKey(g, guard);
        Tabling.Table t = tb.get(key);
        if (t == null) t = tb.getShared(key);
        if (t == null || t.status != Tabling.COMPLETE) {
            long savedFloor = negTableFloor;
            negTableFloor = -1;                  // tnot handles incomplete tables itself
            try {
                forEachSolution(g, new SolutionVisitor() {
                    @Override public boolean visit() { return true; }
                });
            } finally {
                negTableFloor = savedFloor;
            }
            t = tb.get(key);
            if (t == null) t = tb.getShared(key);
        }
        if (t == null) return true;                                  // no table: no answer
        if (t.hasUnconditional()) return false;
        if (t.status == Tabling.COMPLETE && !t.hasAnswers()) return true;
        if (t.status != Tabling.COMPLETE) tb.noteIncompleteRead(t);   // the SCC must iterate
        Term shown = new CompoundTerm(new Atom("tnot"), new Term[] {
            Unify.copy(g, new IdentityHashMap<Variable, Variable>(), guard)});
        addDelay(new Tabling.Delay(Tabling.Delay.NEG, t, -1, shown, null));
        return true;
    }

    /** {@code call_delays(G, D)}: run G; D is the conjunction of the literals G delayed. */
    void pushCallDelays(Term goal, final Term out) {
        final Tabling.Delay saved = delays;
        setDelays(null);
        Goal after = new Goal(new Runnable() {
            @Override public void run() {
                Term conj = Tabling.Delay.conjunction(delays);
                setDelays(saved);
                if (!Unify.unify(out, conj, B)) goalStack = new Goal(ATOM_FAIL, 0, goalStack);
            }
        }, goalStack);
        goalStack = mg(goal, cps.size(), after);
    }
    // END_CHANGE: ISS-2025-0755

    // START_CHANGE: ISS-2025-0754
    /** {@code g0} with every non-index argument of its table's modes replaced by a fresh variable. */
    private Term freeModedArguments(Term g0) {
        if (!(g0 instanceof CompoundTerm)) return g0;
        CompoundTerm c = (CompoundTerm) g0;
        it.denzosoft.jprolog.core.engine.TableStore.ModeSpec[] modes =
            engine.tables().getModes(c.getName() + "/" + c.arity());
        if (modes == null) return g0;
        Term[] args = null;
        for (int i = 0; i < c.arity() && i < modes.length; i++) {
            if (modes[i].isIndex() || Unify.deref(c.arg(i)) instanceof Variable) continue;
            if (args == null) {
                args = new Term[c.arity()];
                for (int j = 0; j < args.length; j++) args[j] = c.arg(j);
            }
            args[i] = new Variable();
        }
        return (args == null) ? g0 : new CompoundTerm(c.getFunctor(), args);
    }
    // END_CHANGE: ISS-2025-0754

    /** Activate {@code cl} against {@code goal}: unify the head into a fresh frame and push the
     *  body in front of {@code after}. Null when the head does not match. */
    Goal buildClauseBody(Clause cl, Term goal, int barrier, Goal after, String defMod) {
        Term[] frame = (cl.nvars == 0) ? LegacyBuiltinAdapter.NO_FRAME : new Term[cl.nvars];
        if (!cl.unifyHead(goal, frame, B)) return null;
        cl.fillBodySlots(frame);                                        // ISS-2025-0551
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
            if (!",".equals(f) && !";".equals(f) && !"->".equals(f) && !"|".equals(f)) break;   // ISS-2025-0734
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

    // START_CHANGE: ISS-2025-0733 - 4.6 wave Q3 (extra): a module-qualified clause. `M:(H :- B)`
    // is `(M:H :- B)`; `user:H` is the user predicate H (it was stored as a flat ':'/2 clause
    // `user:H` that an unqualified call never found); any other `M:H` stays the `M:H` clause the
    // qualified call, clause/2 and retract/1 find, and module M now exists (current_module/1).
    /** The clause with {@code user:} removed from its head and {@code M:(H:-B)} made {@code (M:H:-B)}. */
    static Term normalizeQualifiedClause(Term clause) {
        Term q = Unify.deref(clause);
        if (q instanceof CompoundTerm && ":".equals(((CompoundTerm) q).getName()) && ((CompoundTerm) q).arity() == 2) {
            Term mt = Unify.deref(((CompoundTerm) q).arg(0));
            Term x = Unify.deref(((CompoundTerm) q).arg(1));
            if (mt instanceof Atom && x instanceof CompoundTerm && ":-".equals(((CompoundTerm) x).getName())
                    && ((CompoundTerm) x).arity() == 2) {
                q = new CompoundTerm(new Atom(":-"), Arrays.asList(
                    (Term) new CompoundTerm(new Atom(":"), Arrays.asList(mt, ((CompoundTerm) x).arg(0))),
                    ((CompoundTerm) x).arg(1)));
            }
        }
        if (q instanceof CompoundTerm && ":-".equals(((CompoundTerm) q).getName()) && ((CompoundTerm) q).arity() == 2) {
            Term h = stripUser(((CompoundTerm) q).arg(0));
            if (h != Unify.deref(((CompoundTerm) q).arg(0))) {
                return new CompoundTerm(new Atom(":-"), Arrays.asList(h, ((CompoundTerm) q).arg(1)));
            }
            return q;
        }
        return stripUser(q);
    }

    /** {@code (M:H :- B)} with B's goals qualified by {@code ctx} when M is another module. */
    private static Term qualifyClauseBody(CompoundTerm c, String ctx) {
        Term h = Unify.deref(c.arg(0));
        if (!(h instanceof CompoundTerm) || !":".equals(((CompoundTerm) h).getName()) || ((CompoundTerm) h).arity() != 2) return c;
        Term mt = Unify.deref(((CompoundTerm) h).arg(0));
        if (!(mt instanceof Atom) || ((Atom) mt).getName().equals(ctx)) return c;
        return new CompoundTerm(new Atom(":-"), Arrays.asList(h, qualifyGoal(c.arg(1), ctx)));
    }

    /** {@code g} with every goal outside a control construct run in module {@code m} ({@code m:G}). */
    public static Term qualifyGoal(Term g0, String m) {
        Term g = Unify.deref(g0);
        if (g instanceof Atom && "!".equals(((Atom) g).getName())) return g;
        if (g instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) g;
            String f = c.getName();
            int n = c.arity();
            if (n == 2 && ":".equals(f)) return g;
            if (n == 2 && (",".equals(f) || ";".equals(f) || "->".equals(f) || "*->".equals(f) || "|".equals(f))) {
                return new CompoundTerm(c.getFunctor(), Arrays.asList(qualifyGoal(c.arg(0), m), qualifyGoal(c.arg(1), m)));
            }
            if (n == 1 && "\\+".equals(f)) {
                return new CompoundTerm(c.getFunctor(), java.util.Collections.singletonList(qualifyGoal(c.arg(0), m)));
            }
        }
        return new CompoundTerm(new Atom(":"), Arrays.asList((Term) new Atom(m), g));
    }

    /** {@code user:H} (also nested, {@code user:user:H}) as {@code H}; anything else unchanged. */
    static Term stripUser(Term t) {
        Term h = Unify.deref(t);
        while (h instanceof CompoundTerm && ":".equals(((CompoundTerm) h).getName()) && ((CompoundTerm) h).arity() == 2) {
            Term mt = Unify.deref(((CompoundTerm) h).arg(0));
            if (!(mt instanceof Atom) || !Modules.USER.equals(((Atom) mt).getName())) break;
            h = Unify.deref(((CompoundTerm) h).arg(1));
        }
        return h;
    }

    /** A qualified head {@code M:H} names module M: create it (current_module/1 sees it). */
    private void noteQualifiedHead(Term head) {
        Term h = Unify.deref(head);
        if (h instanceof CompoundTerm && ":".equals(((CompoundTerm) h).getName()) && ((CompoundTerm) h).arity() == 2) {
            Term mt = Unify.deref(((CompoundTerm) h).arg(0));
            if (mt instanceof Atom && engine.prolog() != null) engine.prolog().ensureModule(((Atom) mt).getName());
        }
    }
    // END_CHANGE: ISS-2025-0733

    private void assertClause(Term clause, boolean front) {
        // START_CHANGE: ISS-2025-0733 - (M:H :- B) keeps B in the CALLER's module (SWI); M:(H :- B)
        // runs B in M
        Term raw = Unify.deref(clause);
        boolean wholeQualified = raw instanceof CompoundTerm && ":".equals(((CompoundTerm) raw).getName())
            && ((CompoundTerm) raw).arity() == 2;
        clause = normalizeQualifiedClause(clause);
        if (!wholeQualified && clause instanceof CompoundTerm && ":-".equals(((CompoundTerm) clause).getName())
                && ((CompoundTerm) clause).arity() == 2) {
            clause = qualifyClauseBody((CompoundTerm) clause, contextModule());
        }
        // END_CHANGE: ISS-2025-0733
        Term checkedHead = checkClauseArgument(clause, front ? "asserta/1" : "assertz/1", true);
        checkModifiable(checkedHead, front ? "asserta/1" : "assertz/1");
        // START_CHANGE: ISS-2025-0527 - wave P1.14: a cyclic clause cannot be stored. The copy
        // cuts a cycle by keeping the ORIGINAL cells at the cut, so `X = f(X), assertz(cyc(X))`
        // stored f(X) with X's binding still on the trail — undone on backtracking, the stored
        // clause became cyc(f(_)) and matched cyc(f(f(f(a)))). SWI refuses cyclic clauses too.
        Term c = Unify.copyAcyclic(clause, new IdentityHashMap<Variable, Variable>(), guard);
        if (c == null) throw Errors.representation("cyclic_term", front ? "asserta/1" : "assertz/1");
        // END_CHANGE: ISS-2025-0527
        engine.store().assertRule(toRule(c), front);
        noteQualifiedHead(checkedHead);                         // ISS-2025-0733
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
        Term q = normalizeQualifiedClause(clause);                  // ISS-2025-0733
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
        // START_CHANGE: ISS-2025-0546 - wave P2.6: a WINDOW over the store's gap buffer. The
        // unbound-key case used to be Predicate.all(), an exact copy of the whole predicate
        // re-made after every write — so `retract(p(_))` in a loop copied the remaining clauses
        // on every iteration (1e5: 6-13 s). The window also starts past the dead prefix.
        p.view(Clause.argKey1(h), view);
        final Clause[] candidates = view.a;
        final int limit = view.to;
        // END_CHANGE: ISS-2025-0546
        // END_CHANGE: ISS-2025-0502
        if (view.from >= limit) return false;
        final long gen = engine.store().generation();
        final Term qc = queryClause;
        final String fName = f;
        final int fArity = ar;
        final Goal cont = goalStack;
        CP cp = new CP(CP.GEN, B.mark());
        final int[] i = {view.from};
        cp.gen = new Gen() {
            @Override public Goal next(CP self) {
                while (i[0] < limit) {
                    Clause cl = candidates[i[0]++];
                    if (!cl.isAlive(gen)) continue;
                    if (i[0] >= limit) self.genExhausted = true;
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

    // START_CHANGE: ISS-2025-0545 - wave P2.5: retractall/1 runs through the clause store, like
    // retract/1 and clause/2 (the three share one selection: Predicate.view over the first-argument
    // index). It used to go to KnowledgeBase.retractAllClauses, which removed each match from the
    // front of three lists (quadratic: 100 000 clauses 3.5 s), and whose version bumps then made
    // the store re-sync the whole predicate on the next call. Now each match is retracted through
    // ClauseStore.retractClause — O(1) in both stores, a death generation (so a running call keeps
    // its logical update view) and no re-sync. A head whose arguments are distinct unbound
    // variables matches every clause and skips the per-clause unification; otherwise each
    // candidate's head is test-unified on a PRIVATE trail with no attribute handler, so the test
    // binds nothing and wakes no frozen goal.
    /** retractall(Head) for a dereferenced, callable, modifiable Head. */
    void retractAllClauses(Term head) {
        String f;
        int ar;
        if (head instanceof Atom) { f = ((Atom) head).getName(); ar = 0; }
        else { f = ((CompoundTerm) head).getName(); ar = ((CompoundTerm) head).arity(); }
        ClauseStore store = engine.store();
        ClauseStore.Predicate p = store.lookup(f, ar);
        engine.kb().markDynamic(p.kbEntry);             // ISS-2025-0347: creates it as dynamic
        ClauseStore.View w = new ClauseStore.View();
        p.view(Clause.argKey1(head), w);
        if (w.size() == 0) return;
        long gen = store.generation();
        boolean every = isMostGeneralHead(head);
        ArrayList<Clause> doomed = new ArrayList<Clause>();
        Bindings tb = every ? null : new Bindings(guard);
        for (int i = w.from; i < w.to; i++) {
            Clause cl = w.a[i];
            if (!cl.isAlive(gen)) continue;
            if (every) { doomed.add(cl); continue; }
            tb.forceTrail++;
            boolean u;
            try {
                Term[] frame = (cl.nvars == 0) ? LegacyBuiltinAdapter.NO_FRAME : new Term[cl.nvars];
                u = cl.unifyHead(head, frame, tb);
            } finally {
                tb.undo(0);                                // invariant 1: undo, THEN close
                tb.forceTrail--;
            }
            if (u) doomed.add(cl);
            guard.step();
        }
        for (int i = 0; i < doomed.size(); i++) store.retractClause(p, doomed.get(i));
        if (!doomed.isEmpty()) engine.tabling().invalidate(f, ar);      // ISS-2025-0464
    }

    /** An atom, or a compound whose arguments are pairwise distinct unbound variables. */
    private static boolean isMostGeneralHead(Term head) {
        if (!(head instanceof CompoundTerm)) return true;
        CompoundTerm c = (CompoundTerm) head;
        int n = c.arity();
        for (int i = 0; i < n; i++) {
            Term a = Unify.deref(c.arg(i));
            if (!(a instanceof Variable)) return false;
            for (int j = 0; j < i; j++) if (Unify.deref(c.arg(j)) == a) return false;
            if (n > 8 && i >= 8) return false;            // keep the distinctness test O(1)
        }
        return true;
    }
    // END_CHANGE: ISS-2025-0545

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
    boolean advance(CP cp) {                             // ISS-2025-0780: package
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
                // START_CHANGE: ISS-2025-0668 - a traced CLAUSES frame is no longer dropped here:
                // it stays until its Exit port (popIfDeterministicTop), so that a failure INSIDE
                // its body still reaches it and prints its Fail port (`q :- p, X > 2` printed
                // `Fail: (1) p(X)` and never `Fail: (0) q(X)`). It is still dropped at the Exit
                // when nothing above it is left, so a deterministic exit owes no phantom Fail and
                // the frames kept are exactly the OPEN calls.
                if (cp.genExhausted
                        && (cp.traceGoal == null || (cp.altsTaken == 1 && cp.kind != CP.CLAUSES))
                        && !cps.isEmpty() && cps.get(cps.size() - 1) == cp) {
                // END_CHANGE: ISS-2025-0668
                    popCP();
                    B.clearIfUnreachable(cps.isEmpty());
                    B.tidy(cp.trailMark);                           // ISS-2025-0787
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
                    noteOpen(cp.traceGoal, cp.traceDepth);         // ISS-2025-0528
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

    boolean debugPortsActive() { return debugPortsTarget() != null; }   // ISS-2025-0780: package
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

    // START_CHANGE: ISS-2025-0668 - trace ports of deterministic frames (see advance()).
    /** At an Exit port: drop {@code cp} when it is exhausted and nothing was left above it. */
    void popIfDeterministicTop(CP cp) {                  // ISS-2025-0780: package
        if ((cp.genExhausted || cp.gen == EXHAUSTED_GEN) && !cps.isEmpty() && cps.get(cps.size() - 1) == cp) {
            popCP();
            B.clearIfUnreachable(cps.isEmpty());
            B.tidy(cp.trailMark);                               // ISS-2025-0787
        }
    }

    // START_CHANGE: ISS-2025-0715 - the allocation-free, always-on form of anyMayMatch.
    /** The index of the first live clause at or after {@code cp.idx} whose head could still match
     *  {@code cp.goal} (no per-argument principal-functor clash), or -1 when there is none. */
    private static int nextMayMatch(CP cp) {
        Term g = Unify.deref(cp.goal);
        if (!(g instanceof CompoundTerm)) return cp.idx;
        CompoundTerm gc = (CompoundTerm) g;
        int n = gc.arity();
        for (int j = cp.idx; j < cp.limit; j++) {
            Clause cl = cp.clauses[j];
            if (!cl.isAlive(cp.generation)) continue;
            if (!(cl.head instanceof CompoundTerm)) return j;
            CompoundTerm h = (CompoundTerm) cl.head;
            if (h.arity() != n) return j;
            boolean clash = false;
            for (int i = 0; i < n && !clash; i++) clash = argClash(Unify.deref(gc.arg(i)), h.arg(i));
            if (!clash) return j;
        }
        return -1;
    }

    /** True when a (dereferenced) goal argument certainly cannot unify with a head argument:
     *  both are non-variables with different principal functors. Conservative everywhere else. */
    private static boolean argClash(Term a, Term h) {
        if (a instanceof Variable || h instanceof VarRef || h instanceof Variable || a == h) return false;
        if (a instanceof Atom) return !(h instanceof Atom) || !((Atom) a).getName().equals(((Atom) h).getName());
        if (a instanceof CompoundTerm) {
            if (!(h instanceof CompoundTerm)) return true;
            CompoundTerm x = (CompoundTerm) a, y = (CompoundTerm) h;
            return x.arity() != y.arity() || !x.getName().equals(y.getName());
        }
        if (h instanceof Atom || h instanceof CompoundTerm) return true;
        Object ka = Clause.argKey(a), kh = Clause.argKey(h);
        return ka != null && kh != null && !ka.equals(kh);
    }
    // END_CHANGE: ISS-2025-0715

    // ISS-2025-0715: anyMayMatch (the tracing-only look-ahead) is replaced by nextMayMatch.

    /** The goal as a port shows it: an engine-internal {@code '$mctx'(M, G)} meta-argument
     *  prints as {@code G} (in {@code user}) or {@code M:G}, not as the wrapper. */
    private static Term portView(Term goal) {
        Term d = Unify.deref(goal);
        if (!(d instanceof CompoundTerm)) return goal;
        CompoundTerm c = (CompoundTerm) d;
        Term[] args = null;
        for (int i = 0; i < c.arity(); i++) {
            Term a = Unify.deref(c.arg(i));
            if (a instanceof CompoundTerm && ((CompoundTerm) a).arity() == 2
                    && Modules.MCTX.equals(((CompoundTerm) a).getName())) {
                if (args == null) {
                    args = new Term[c.arity()];
                    for (int k = 0; k < args.length; k++) args[k] = c.arg(k);
                }
                CompoundTerm w = (CompoundTerm) a;
                Term m = Unify.deref(w.arg(0));
                args[i] = (m instanceof Atom && Modules.USER.equals(((Atom) m).getName()))
                    ? w.arg(1) : new CompoundTerm(new Atom(":"), new Term[] {m, w.arg(1)});
            }
        }
        return args == null ? goal : new CompoundTerm(c.getFunctor(), args);
    }
    // END_CHANGE: ISS-2025-0668

    /** The current four-port nesting level (the depth the next Call would report). */
    int portDepth() { return portDepth; }

    /** ISS-2025-0780: a frame's Exit/Fail puts the depth back to the frame's own level. */
    void setPortDepth(int d) { portDepth = d; }

    void portCall(Term g, int d) { portDepth = d + 1; noteOpen(g, d); tracePort("Call", g, d); debugPort(DebugEvent.Port.CALL, g, d); }
    void portExit(Term g, int d) { portDepth = d; tracePort("Exit", g, d); debugPort(DebugEvent.Port.EXIT, g, d); }
    void portFail(Term g, int d) { portDepth = d; tracePort("Fail", g, d); debugPort(DebugEvent.Port.FAIL, g, d); }
    // END_CHANGE: ISS-2025-0482

    // START_CHANGE: ISS-2025-0528 - wave P1.15: the goal open at each port depth. Every port
    // ASSIGNS the depth (ISS-2025-0482), so the entries below `portDepth` are exactly the calls
    // that are still open; that is what an unwinding ball reports an Exception port for. Written
    // only on a traced/debugged Call or Redo, so an untraced run never touches it.
    private Term[] openGoals;

    void noteOpen(Term g, int d) {                       // ISS-2025-0780: package
        if (d < 0) return;
        if (openGoals == null) openGoals = new Term[Math.max(16, d + 1)];
        else if (d >= openGoals.length) openGoals = Arrays.copyOf(openGoals, Math.max(d + 1, openGoals.length << 1));
        openGoals[d] = g;
    }

    /**
     * A ball was caught by a catch/3 called at depth {@code catchDepth}: report an Exception port
     * for every call it unwound (innermost first) — the tracer prints {@code Exception:}, the
     * debugger gets {@link DebugController#handleException}, which pops its call stack like a Fail —
     * and put the depth back to the catch/3 call's level, where the recovery runs.
     */
    private void unwindPorts(int catchDepth) {
        if (portDepth > catchDepth && openGoals != null) {
            boolean tracing = it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled();
            DebugController dc = debugPortsTarget();
            if (tracing || dc != null) {
                for (int d = Math.min(portDepth, openGoals.length) - 1; d >= catchDepth; d--) {
                    Term g = openGoals[d];
                    if (g == null) continue;
                    if (tracing) tracePort("Exception", g, d);
                    if (dc != null) {
                        try { dc.handleException(snapshotFor(dc, g), d, new HashMap<String, Term>()); }
                        catch (RuntimeException e) { ControlFlow.rethrowIfControl(e); }
                    }
                }
            }
        }
        portDepth = catchDepth;
    }
    // END_CHANGE: ISS-2025-0528

    void debugPort(DebugEvent.Port port, Term goal, int depth) {   // ISS-2025-0780: package
        DebugController dc = debugPortsTarget();               // ISS-2025-0494
        if (dc == null) return;
        dc.notifyPort(port, snapshotFor(dc, portView(goal)), new HashMap<String, Term>(), depth);   // ISS-2025-0668
    }

    // START_CHANGE: ISS-2025-0529 - wave P1.16: the goal is resolved into a snapshot only when the
    // controller can use it — a rendering listener, a stepping mode, or a breakpoint on THIS goal's
    // indicator. "Some breakpoint exists" used to be enough, so one unrelated breakpoint made every
    // port copy its goal: a recursion over a 40 000-element list resolved the list 80 000 times.
    private Term snapshotFor(DebugController dc, Term goal) {
        // ISS-2025-0481: only snapshot when somebody will read the term later (see needsGoalSnapshot)
        if (!dc.needsGoalSnapshot(Unify.deref(goal))) return goal;
        try { return Unify.resolve(goal, guard); }
        catch (RuntimeException e) { ControlFlow.rethrowIfControl(e); return goal; }
    }
    // END_CHANGE: ISS-2025-0529

    // START_CHANGE: ISS-2025-0482 - the indentation is CAPPED. The depth is a real call depth now,
    // and a deterministic tail recursion reaches it: `loop(1000000)` under trace would otherwise
    // build a 2 000 000-character indent for its last line, i.e. quadratic output. The depth itself
    // is still reported exactly, in the parentheses.
    private static final int MAX_TRACE_INDENT = 40;
    // END_CHANGE: ISS-2025-0482

    void tracePort(String port, Term goal, int depth) {  // ISS-2025-0780: package
        if (!it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled()) return;
        try {
            StringBuilder sb = new StringBuilder();
            int indent = (depth < MAX_TRACE_INDENT) ? depth : MAX_TRACE_INDENT;
            for (int i = 0; i < indent; i++) sb.append("  ");
            // ISS-2025-0482: format the goal DIRECTLY. The writer dereferences every cell and is
            // cycle-safe, so Unify.resolve here only built a throw-away copy of the whole term —
            // the single most expensive thing a trace port did.
            String g = it.denzosoft.jprolog.core.util.TermFormatter
                .format(portView(goal), false, false, false, 1200);      // ISS-2025-0668
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
