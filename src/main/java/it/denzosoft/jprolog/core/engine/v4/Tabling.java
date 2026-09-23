package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0463 - engine v4 wave W5, design B.8: linear tabling with completion.
/**
 * The v4 tabling store and the generator/consumer frame that drives a tabled call.
 *
 * <p>This replaces the bounded re-evaluation loop of the recursive solver's tabling (design
 * limit <b>L-03</b>): that algorithm re-ran a tabled goal at most 100 times, kept its answers as
 * name-keyed {@code Map<String,Term>}s and let a consumer read a stale partial list, so
 * {@code path(1, 51)} <b>failed</b> on a left-recursive 3 000-edge chain while
 * {@code findall(Y, path(1,Y), L)} found all 3 000 answers.
 *
 * <h3>The algorithm</h3>
 * Linear tabling with completion (SLD + iterative completion, B-Prolog / DRA style), which is
 * correct and complete for definite programs with left recursion and keeps the machine's SLD
 * structure — no suspension/resumption of continuations, no Java recursion per subgoal.
 *
 * <ul>
 *   <li>A <b>variant table</b> per tabled subgoal: {@code { status, answers, dependencies }}. The
 *       variant key is computed on the <b>cell model</b> ({@link #variantKey}, a numbervars-style
 *       canonical encoding), never on variable names or a binding map, so {@code path(1, 51)} and
 *       {@code path(1, Y)} are simply two different variants of the same machinery.</li>
 *   <li>The first call to a variant becomes its <b>generator</b>: a choice point whose PRODUCE
 *       phase runs the predicate's clauses against a private copy of the call, fail-driven, and
 *       records every answer in the table. When it is done it switches to its CONSUME phase and
 *       hands the table's answers to the caller one per redo.</li>
 *   <li>A call to a variant that is already EVALUATING becomes a <b>consumer</b>: a choice point
 *       that iterates the answers recorded <i>so far</i>, lazily and by index, so answers appended
 *       later in the same round are consumed too. That is what makes left recursion work.</li>
 *   <li><b>Completion</b>: dependencies are tracked with the classic DFN/leader scheme — every
 *       table gets a creation sequence number, a producing frame records the smallest sequence
 *       number it read while incomplete, and a child frame propagates its own to its parent. A
 *       frame whose minimum is its own table's sequence number is the <b>leader</b> of its SCC: it
 *       re-runs its clauses (a new round; every table of the SCC re-produces, deduplication makes
 *       it semi-naive) until a round adds no answer, then marks every table of the SCC COMPLETE.
 *       There is no iteration cap — termination follows from the finite, deduplicated answer set of
 *       variant tabling.</li>
 * </ul>
 *
 * <p>A round is repeated only when the round both <b>grew</b> the answer set and <b>read an
 * incomplete table</b>: an evaluation that only ever read COMPLETE tables (the ordinary
 * memoisation case, e.g. tabled {@code fib/2}) is exact after one pass.
 *
 * <h3>Invalidation</h3>
 * The tables of {@code P} are dropped when {@code P} itself is asserted to or retracted from
 * (and never while an evaluation is running). Changes to a <b>non-tabled</b> predicate that a
 * tabled predicate depends on are not tracked: call {@code abolish_all_tables/0}. Tables survive
 * a query; an evaluation abandoned by an exception, a cut or the inference budget is discarded
 * whole, so a later call recomputes it instead of reading a partial table.
 *
 * <p>Not thread-safe: one {@link Engine} is evaluated by one {@link Machine} on one thread.
 */
final class Tabling {

    static final int EVALUATING = 0, COMPLETE = 1, ABANDONED = 2;

    /** One variant table. */
    static final class Table {
        final String key;
        final String indicator;          // "path/2"
        final Term template;             // a variant-normalised copy of the call
        final long seq;                  // creation order: the DFN of the SCC detection
        int status = EVALUATING;
        /** Answers as variant-normalised terms, in insertion order. */
        final ArrayList<Term> answers = new ArrayList<Term>();
        /** Variant keys of {@link #answers}, for O(1) deduplication. */
        final java.util.HashSet<String> seen = new java.util.HashSet<String>();
        /** A generator frame for this table is inside its PRODUCE phase. */
        boolean producing;
        /** The last round in which this table produced from its clauses. */
        long producedRound;
        // START_CHANGE: ISS-2025-0572 - mode-directed tabling: per-argument modes (null = variant
        // tabling) and the position of the one kept answer per index-argument key. A superseded
        // answer's slot is set to null and the better answer APPENDED, so a consumer that is
        // iterating by position still sees the improvement (and the SCC runs another round).
        String[] modes;
        java.util.HashMap<String, Integer> modedPos;
        // END_CHANGE: ISS-2025-0572

        Table(String key, String indicator, Term template, long seq) {
            this.key = key;
            this.indicator = indicator;
            this.template = template;
            this.seq = seq;
        }
    }

    private final Map<String, Table> tables = new LinkedHashMap<String, Table>();
    /** The EVALUATING tables, in creation (sequence) order. */
    private final ArrayList<Table> evalStack = new ArrayList<Table>();
    /** The frames currently inside their PRODUCE phase, innermost last. */
    private final ArrayList<TableFrame> producing = new ArrayList<TableFrame>();

    private long seqCounter;
    private long round = 1;
    /** Global counters; a frame compares snapshots of them to decide whether to iterate. */
    long answersAdded;
    long incompleteReads;
    /** Answers currently held by the live tables (for the memory cap below). */
    private long liveAnswers;

    // START_CHANGE: ISS-2025-0464 - COMPLETE tables live for the lifetime of the engine, which is
    // the point of a memo, but an embedder must not be able to grow them without bound. The caps
    // are applied ONLY at a query boundary, only to COMPLETE tables and oldest-first, so they can
    // never disturb a running evaluation; a dropped table is simply recomputed on its next call.
    private static final int MAX_TABLES = 100000;
    private static final long MAX_ANSWERS = 4000000L;
    // END_CHANGE: ISS-2025-0464

    long round() { return round; }

    Table get(String key) { return tables.get(key); }

    Table create(String key, String indicator, Term template) {
        Table t = new Table(key, indicator, template, ++seqCounter);
        tables.put(key, t);
        evalStack.add(t);
        return t;
    }

    /** A consumer read a table that is still EVALUATING: record the dependency (design B.8). */
    void noteIncompleteRead(Table t) {
        incompleteReads++;
        if (!producing.isEmpty()) {
            TableFrame top = producing.get(producing.size() - 1);
            if (t.seq < top.minOuterSeq) top.minOuterSeq = t.seq;
        }
    }

    // START_CHANGE: ISS-2025-0488 - LIM-039: A TABLED EVALUATION IS CLAIMED BY ONE THREAD.
    // The store is a single-threaded structure — one `producing` stack, one `evalStack`, one answer
    // list and one `seen` set per table, all plain ArrayList/HashMap — and since v4.0.0 (LIM-024)
    // several worker machines can reach one engine's store. Two rules make that safe:
    //
    //   1. A tabled CALL runs inside `enterCall`/`exitCall`, so the "does this variant exist, is it
    //      complete, do I produce it" decision and the frame it installs are atomic.
    //   2. The claim is HELD for the whole evaluation — from the first table an evaluation creates
    //      until its SCC completes, is abandoned, or the query ends. Without that, a second thread
    //      would see an EVALUATING table, become a consumer of it, and read a half-produced answer
    //      set as if it were the whole one (that is exactly what LIM-039 described).
    //
    // Reading a COMPLETE table stays effectively parallel: a consumer holds the claim only for the
    // duration of the call decision. The wait is bounded, so a runaway producer (or a tabled goal
    // that fans out into workers that are themselves tabled) surfaces as a resource_error instead
    // of hanging, and a Stop interrupt is honoured while waiting.
    private Thread evalOwner;
    private int callDepth;
    private static final long EVAL_WAIT_MS = 60_000L;

    /** Enter a tabled call: no OTHER thread may be inside one, or running an evaluation. */
    synchronized void enterCall() {
        Thread me = Thread.currentThread();
        long deadline = System.currentTimeMillis() + EVAL_WAIT_MS;
        while (evalOwner != null && evalOwner != me) {
            long left = deadline - System.currentTimeMillis();
            if (left <= 0) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError(
                        "tabling_busy", "another thread is evaluating a table on this engine"));
            }
            try {
                wait(left);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new it.denzosoft.jprolog.core.engine.QueryCancelledException();
            }
        }
        evalOwner = me;
        callDepth++;
    }

    /** Leave a tabled call. The claim survives while this thread still has an evaluation open. */
    synchronized void exitCall() {
        if (evalOwner != Thread.currentThread()) return;
        if (--callDepth <= 0) { callDepth = 0; releaseIfIdle(); }
    }

    /** Drop the claim once nothing is left EVALUATING and no call is in flight. */
    private synchronized void releaseIfIdle() {
        if (evalOwner != Thread.currentThread()) return;
        if (callDepth <= 0 && evalStack.isEmpty() && producing.isEmpty()) {
            evalOwner = null;
            notifyAll();
        }
    }

    /** Give the claim back unconditionally (a query or a worker machine has finished). */
    private synchronized void releaseEvaluation() {
        if (evalOwner != Thread.currentThread()) return;
        callDepth = 0;
        evalOwner = null;
        notifyAll();
    }

    /**
     * A WORKER machine has finished. It owns no query boundary (so it must not run
     * {@link #endQuery}), but it must not walk away holding the claim either: whatever it left
     * EVALUATING is abandoned — an incomplete table must never be read as authoritative — and the
     * store is handed back to whoever is waiting.
     */
    synchronized void endWorker() {
        if (evalOwner != Thread.currentThread()) return;
        producing.clear();
        for (int i = evalStack.size() - 1; i >= 0; i--) {
            Table t = evalStack.get(i);
            t.status = ABANDONED;
            t.producing = false;
            liveAnswers -= t.answers.size();
            if (tables.get(t.key) == t) tables.remove(t.key);
        }
        evalStack.clear();
        releaseEvaluation();
    }
    // END_CHANGE: ISS-2025-0488

    void beginProduction(TableFrame f) {
        f.minOuterSeq = Long.MAX_VALUE;
        f.table.producing = true;
        f.table.producedRound = round;
        f.snapAnswers = answersAdded;
        f.snapReads = incompleteReads;
        producing.add(f);
    }

    /** End {@code f}'s PRODUCE phase; true when its table leads its SCC. */
    boolean endProduction(TableFrame f) {
        for (int i = producing.size() - 1; i >= 0; i--) {
            if (producing.get(i) == f) { producing.remove(i); break; }
        }
        f.table.producing = false;
        boolean leader = f.minOuterSeq >= f.table.seq;
        if (!leader && !producing.isEmpty()) {
            TableFrame parent = producing.get(producing.size() - 1);
            if (f.minOuterSeq < parent.minOuterSeq) parent.minOuterSeq = f.minOuterSeq;
        }
        return leader;
    }

    void newRound() { round++; }

    void answerRecorded() { answersAdded++; liveAnswers++; }

    /** Mark the leader's whole SCC complete: every table still EVALUATING that was created at or
     *  after the leader (they can only be reached through it). */
    void completeScc(Table leader) {
        for (int i = evalStack.size() - 1; i >= 0; i--) {
            Table t = evalStack.get(i);
            if (t.seq < leader.seq) break;
            t.status = COMPLETE;
            t.producing = false;
            evalStack.remove(i);
        }
        releaseIfIdle();                                     // ISS-2025-0488
    }

    /**
     * An evaluation was abandoned in mid-production (an exception unwound past it, a cut discarded
     * it, or the budget/interrupt fired). The partial tables must not be readable — a later call
     * has to recompute them — and no table may be left EVALUATING with a live {@code producing}
     * flag.
     */
    void abortProduction(TableFrame f) {
        for (int i = producing.size() - 1; i >= 0; i--) {
            TableFrame x = producing.remove(i);
            x.table.producing = false;
            if (x == f) break;
        }
        for (int i = evalStack.size() - 1; i >= 0; i--) {
            Table t = evalStack.get(i);
            if (t.seq < f.table.seq) break;
            evalStack.remove(i);
            t.status = ABANDONED;
            liveAnswers -= t.answers.size();
            if (tables.get(t.key) == t) tables.remove(t.key);
        }
        releaseIfIdle();                                     // ISS-2025-0488
    }

    /** Query boundary: nothing may stay EVALUATING between queries. COMPLETE tables persist. */
    void endQuery() {
        producing.clear();
        releaseEvaluation();                                 // ISS-2025-0488
        for (int i = evalStack.size() - 1; i >= 0; i--) {
            Table t = evalStack.get(i);
            t.status = ABANDONED;
            t.producing = false;
            liveAnswers -= t.answers.size();
            if (tables.get(t.key) == t) tables.remove(t.key);
        }
        evalStack.clear();
        if (tables.size() > MAX_TABLES || liveAnswers > MAX_ANSWERS) evictOldest();
    }

    /** Drop COMPLETE tables oldest-first until both caps are back under half. */
    private void evictOldest() {
        java.util.Iterator<Map.Entry<String, Table>> it = tables.entrySet().iterator();
        while (it.hasNext() && (tables.size() > MAX_TABLES / 2 || liveAnswers > MAX_ANSWERS / 2)) {
            Table t = it.next().getValue();
            if (t.status != COMPLETE) continue;
            it.remove();
            liveAnswers -= t.answers.size();
            t.status = ABANDONED;
        }
    }

    /** True while a tabled evaluation is in progress on this engine. */
    boolean evaluating() { return !producing.isEmpty(); }

    // START_CHANGE: ISS-2025-0661 - negation over an incomplete table (see Machine.stepN)
    /** True when the calling thread holds the evaluation claim. */
    synchronized boolean ownedByCurrentThread() { return evalOwner == Thread.currentThread(); }

    /** The sequence number of the most recently created table. */
    long lastSeq() { return seqCounter; }
    // END_CHANGE: ISS-2025-0661

    /** {@code abolish_all_tables/0}: drop every answer table (declarations are kept). */
    void abolishAll() {
        tables.clear();
        evalStack.clear();
        producing.clear();
        liveAnswers = 0;
    }

    /** {@code abolish_table/1} and the assert/retract invalidation: drop one predicate's tables. */
    void abolish(String functor, int arity) {
        String ind = functor + "/" + arity;
        java.util.Iterator<Map.Entry<String, Table>> it = tables.entrySet().iterator();
        while (it.hasNext()) {
            Table t = it.next().getValue();
            if (!ind.equals(t.indicator)) continue;
            it.remove();
            evalStack.remove(t);
            liveAnswers -= t.answers.size();
            t.status = ABANDONED;
        }
    }

    /**
     * A clause of {@code functor/arity} was asserted or retracted. Only that predicate's own tables
     * are invalidated, and never while an evaluation is running (see the class comment).
     */
    void invalidate(String functor, int arity) {
        if (!producing.isEmpty() || tables.isEmpty()) return;
        abolish(functor, arity);
    }

    /** Snapshot for {@code current_table/2}. */
    List<Table> snapshot() { return new ArrayList<Table>(tables.values()); }

    /** Test hook: the number of live tables. */
    int tableCount() { return tables.size(); }

    /** Test hook: the answers of a variant, or null. */
    Table lookup(String key) { return tables.get(key); }

    // ------------------------------------------------------------------ variant keys

    /**
     * The canonical (numbervars-style) encoding of {@code t} over the <b>cell model</b>: two terms
     * have the same key exactly when they are variants of each other. Iterative, so a long answer
     * list does not recurse; polls the {@link ResourceGuard}, so a budget or a Stop interrupt
     * aborts it. Length prefixes make the encoding injective (no separator can be forged by an
     * atom's own text).
     */
    static String variantKey(Term t, ResourceGuard g) {
        return appendKey(new StringBuilder(48), t, g).toString();
    }

    /** {@link #variantKey} into a caller-owned buffer (the hot path reuses one per frame). */
    static StringBuilder appendKey(StringBuilder sb, Term t, ResourceGuard g) {
        IdentityHashMap<Variable, Integer> vars = null;
        ArrayList<Object> stack = new ArrayList<Object>(8);
        stack.add(t);
        int n = 0;
        while (!stack.isEmpty()) {
            Object o = stack.remove(stack.size() - 1);
            if (o instanceof String) { sb.append((String) o); continue; }
            Term x = Unify.deref((Term) o);
            if ((++n & 0x3FF) == 0 && g != null) g.step();
            if (x instanceof CompoundTerm) {
                CompoundTerm c = (CompoundTerm) x;
                String s = c.getName();
                List<Term> as = c.getArguments();
                sb.append('c').append(s.length()).append(':').append(s).append('/').append(as.size()).append('(');
                stack.add(")");
                for (int i = as.size() - 1; i >= 0; i--) stack.add(as.get(i));
            } else if (x instanceof Atom) {
                String s = ((Atom) x).getName();
                sb.append('a').append(s.length()).append(':').append(s);
            } else if (x instanceof Number) {
                Number num = (Number) x;
                if (!num.isInteger()) sb.append('f').append(Double.doubleToRawLongBits(num.doubleValue())).append('.');
                else if (num.fitsInLong()) sb.append('i').append(num.longValue()).append('.');
                else sb.append('I').append(num.bigIntegerValue()).append('.');
            } else if (x instanceof Variable) {
                if (vars == null) vars = new IdentityHashMap<Variable, Integer>();
                Integer i = vars.get(x);
                if (i == null) { i = Integer.valueOf(vars.size()); vars.put((Variable) x, i); }
                sb.append('v').append(i.intValue()).append('.');
            } else if (x instanceof PrologString) {
                String s = ((PrologString) x).getStringValue();
                sb.append('s').append(s.length()).append(':').append(s);
            } else {
                String s = String.valueOf(x);
                sb.append('o').append(s.length()).append(':').append(s);
            }
        }
        return sb;
    }

    static String indicatorOf(Term goal) {
        if (goal instanceof Atom) return ((Atom) goal).getName() + "/0";
        if (goal instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) goal;
            return c.getName() + "/" + c.getArguments().size();
        }
        return String.valueOf(goal) + "/0";
    }

    // ------------------------------------------------------------------ built-ins

    // START_CHANGE: ISS-2025-0464 - the tabling built-ins must observe the v4 store. TableStore
    // keeps the DECLARATIONS (the consult-time `:- table` directive and table/1 write them), so
    // abolish_table/1 clears both.
    static void register(BuiltinTable t) {
        t.register("abolish_all_tables", 0, new AbolishAllB());
        t.register("abolish_table", 1, new AbolishOneB());
        t.register("current_table", 2, new CurrentTableB());
    }

    /** Abolishing tables from INSIDE a tabled evaluation would pull the store out from under the
     *  running generator frames, so it is a permission error (XSB refuses it too). */
    private static void checkNotEvaluating(Machine m, String context) {
        if (!m.engine().tabling().evaluating()) return;
        throw Errors.permission("modify", "table", new Atom(context), context);
    }

    /** {@code abolish_all_tables/0}: drop every answer table; declarations survive. */
    private static final class AbolishAllB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            checkNotEvaluating(m, "abolish_all_tables/0");
            m.engine().tabling().abolishAll();
            if (m.engine().tables() != null) m.engine().tables().abolishAllTables();
            return Outcome.SUCCESS;
        }
    }

    /** {@code abolish_table(+Name/Arity)}: drop that predicate's tables AND (as on v2, and as the
     *  guide documents) its {@code table} declaration. */
    private static final class AbolishOneB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term a = m.deref(args[0]);
            if (a instanceof Variable) throw Errors.instantiation("abolish_table/1");
            if (!(a instanceof CompoundTerm) || !"/".equals(((CompoundTerm) a).getName())
                    || ((CompoundTerm) a).getArguments().size() != 2) {
                throw Errors.type("predicate_indicator", m.resolve(a), "abolish_table/1");
            }
            Term ft = m.deref(((CompoundTerm) a).getArguments().get(0));
            Term at = m.deref(((CompoundTerm) a).getArguments().get(1));
            if (ft instanceof Variable || at instanceof Variable) throw Errors.instantiation("abolish_table/1");
            if (!(ft instanceof Atom) || !(at instanceof Number) || !((Number) at).isInteger()) {
                throw Errors.type("predicate_indicator", m.resolve(a), "abolish_table/1");
            }
            String f = ((Atom) ft).getName();
            int ar = (int) ((Number) at).longValue();
            checkNotEvaluating(m, "abolish_table/1");
            m.engine().tabling().abolish(f, ar);
            if (m.engine().tables() != null) m.engine().tables().abolishTable(f, ar);
            return Outcome.SUCCESS;
        }
    }

    /**
     * {@code current_table(?Variant, ?Status)} — enumerate the live tables; {@code Status} is
     * {@code complete} or {@code incomplete}. A v4-only predicate (like {@code partition/4} and
     * {@code unifiable/3}), because only v4 has a real table store to describe.
     */
    private static final class CurrentTableB implements Builtin {
        @Override public Outcome call(final Machine m, final Term[] args) {
            final List<Table> all = m.engine().tabling().snapshot();
            return m.pushGenerator(new Generator() {
                private int i;
                @Override public boolean next(Machine mm) {
                    while (i < all.size()) {
                        Table t = all.get(i++);
                        if (t.status == ABANDONED) continue;
                        Term variant = Unify.copy(t.template, new IdentityHashMap<Variable, Variable>(), mm.guard());
                        Term status = new Atom(t.status == COMPLETE ? "complete" : "incomplete");
                        // one unification, so a partial match leaves nothing bound (invariant 12)
                        Term probe = new CompoundTerm(new Atom("-"), java.util.Arrays.asList(variant, status));
                        Term goal = new CompoundTerm(new Atom("-"), java.util.Arrays.asList(args[0], args[1]));
                        if (!mm.unifyOrUndo(goal, probe)) continue;
                        if (i >= all.size()) mm.lastSolution();
                        return true;
                    }
                    return false;
                }
            }) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0464

    // ------------------------------------------------------------------ the machine frame

    /**
     * The generator/consumer choice point of a tabled call (design B.8, invariant 14: the choice
     * point carries {@code traceGoal}/{@code traceDepth}, so Call/Exit/Redo/Fail are emitted for a
     * tabled call exactly as for an ordinary one).
     *
     * <p>PRODUCE runs the predicate's clauses against {@link #prod}, a private copy of the call, with
     * the continuation {@code record-answer, fail} — the whole phase is fail-driven and returns
     * nothing to the caller, so a solution can never escape while a table is half-built. CONSUME
     * then unifies the caller's goal with one answer per redo.
     */
    static final class TableFrame implements Machine.Gen {

        private static final int PRODUCE = 0, CONSUME = 1, DEAD = 2;
        private static final Atom FAIL = new Atom("fail");

        private final Machine m;
        private final Tabling tb;
        final Table table;
        private final Term callGoal;
        private final Machine.Goal cont;
        private final Term traceGoal;
        private final int traceDepth;

        private final Clause[] clauses;
        private final int limit;
        private final long generation;
        private final Term prod;
        private final Runnable recorder;
        private final StringBuilder keyBuf = new StringBuilder(64);

        private int phase;
        private int ci;
        private int ai;
        private boolean delivered;
        int bodyBarrier;
        long minOuterSeq = Long.MAX_VALUE;
        long snapAnswers, snapReads;

        /** ISS-2025-0466: the module the tabled clauses' bodies run in (null == user). */
        String defModule;

        TableFrame(Machine m, Tabling tb, Table table, Term callGoal, Machine.Goal cont,
                   boolean produce, Term prod, Clause[] clauses, int limit, long generation,
                   Term traceGoal, int traceDepth) {
            this.m = m;
            this.tb = tb;
            this.table = table;
            this.callGoal = callGoal;
            this.cont = cont;
            this.prod = prod;
            this.clauses = clauses;
            this.limit = limit;
            this.generation = generation;
            this.traceGoal = traceGoal;
            this.traceDepth = traceDepth;
            this.phase = produce ? PRODUCE : CONSUME;
            this.recorder = new Runnable() {
                @Override public void run() { record(); }
            };
            if (produce) tb.beginProduction(this);
        }

        /** Record the current instance of the production template as an answer. The key is computed
         *  on the template ITSELF — the copy is only made for an answer that is actually new, which
         *  is what keeps a quadratic transitive closure (millions of duplicate solutions) cheap. */
        private void record() {
            if (table.modes != null) { recordModed(); return; }   // ISS-2025-0572
            keyBuf.setLength(0);
            String k = appendKey(keyBuf, prod, m.guard()).toString();
            if (table.seen.add(k)) {
                table.answers.add(Unify.copy(prod, new IdentityHashMap<Variable, Variable>(), m.guard()));
                tb.answerRecorded();
            }
        }

        // START_CHANGE: ISS-2025-0572
        private void recordModed() {
            Term p = Unify.deref(prod);
            if (!(p instanceof CompoundTerm)) { table.modes = null; record(); return; }
            List<Term> as = ((CompoundTerm) p).getArguments();
            String[] modes = table.modes;
            keyBuf.setLength(0);
            for (int i = 0; i < as.size() && i < modes.length; i++) {
                if ("index".equals(modes[i])) appendKey(keyBuf, as.get(i), m.guard());
                else keyBuf.append('*');
                keyBuf.append('\u0001');
            }
            String k = keyBuf.toString();
            if (table.modedPos == null) table.modedPos = new java.util.HashMap<String, Integer>();
            Integer pos = table.modedPos.get(k);
            if (pos == null) {
                table.answers.add(Unify.copy(prod, new IdentityHashMap<Variable, Variable>(), m.guard()));
                table.modedPos.put(k, table.answers.size() - 1);
                tb.answerRecorded();
                return;
            }
            Term old = table.answers.get(pos);
            List<Term> olds = ((CompoundTerm) old).getArguments();
            boolean better = false;
            for (int i = 0; i < modes.length && i < as.size(); i++) {
                String md = modes[i];
                if ("min".equals(md) || "max".equals(md)) {
                    int c = Unify.compareTerms(Unify.deref(as.get(i)), olds.get(i), m.guard());
                    better = "min".equals(md) ? c < 0 : c > 0;
                    break;
                }
                if ("last".equals(md)) {
                    StringBuilder a = new StringBuilder(), b = new StringBuilder();
                    appendKey(a, as.get(i), m.guard());
                    appendKey(b, olds.get(i), m.guard());
                    better = !a.toString().equals(b.toString());
                    break;
                }
            }
            if (!better) return;
            table.answers.set(pos, null);
            table.answers.add(Unify.copy(prod, new IdentityHashMap<Variable, Variable>(), m.guard()));
            table.modedPos.put(k, table.answers.size() - 1);
            tb.answerRecorded();
        }
        // END_CHANGE: ISS-2025-0572

        @Override
        public Machine.Goal next(Machine.CP cp) {
            // Ports (invariant 14): the PRODUCE phase is internal — it hands nothing to the caller,
            // so it must not emit Redo. cp.traceGoal is therefore armed only once an answer has
            // been delivered (2nd answer onwards -> Redo) and just before exhaustion (-> Fail).
            cp.traceGoal = null;
            while (true) {
                if (phase == PRODUCE) {
                    while (ci < limit) {
                        Clause cl = clauses[ci++];
                        if (!cl.isAlive(generation)) continue;
                        Machine.Goal after = new Machine.Goal(recorder,
                            new Machine.Goal(FAIL, bodyBarrier, null));
                        Machine.Goal gs = m.buildClauseBody(cl, prod, bodyBarrier, after, defModule);
                        if (gs == null) return Machine.FAILED;
                        return gs;
                    }
                    boolean leader = tb.endProduction(this);
                    if (leader) {
                        if (tb.answersAdded != snapAnswers && tb.incompleteReads != snapReads) {
                            tb.newRound();                    // the SCC is not closed: iterate
                            tb.beginProduction(this);
                            ci = 0;
                            continue;
                        }
                        tb.completeScc(table);
                    }
                    phase = CONSUME;
                    ai = 0;
                }
                if (phase == DEAD) return Machine.EXHAUSTED;
                while (ai < table.answers.size()) {
                    Term ans = table.answers.get(ai++);
                    if (ans == null) continue;                            // ISS-2025-0572: superseded
                    Term inst = Unify.copy(ans, new IdentityHashMap<Variable, Variable>(), m.guard());
                    if (!m.unifyOrUndo(callGoal, inst)) { m.guard().step(); continue; }
                    // Only a COMPLETE table can be trusted to have handed out its last answer; an
                    // EVALUATING one may still grow inside this very round (invariant 2/13).
                    if (table.status == COMPLETE && ai >= table.answers.size()) cp.genExhausted = true;
                    if (traceGoal == null) return cont;
                    if (delivered) cp.traceGoal = traceGoal;         // Redo for the 2nd answer on
                    delivered = true;
                    return new Machine.Goal(new Runnable() {
                        @Override public void run() { m.portExit(traceGoal, traceDepth); }
                    }, cont);
                }
                cp.traceGoal = traceGoal;                            // arm the Fail port
                return Machine.EXHAUSTED;
            }
        }

        /** The choice point was destroyed without being driven to exhaustion (cut, exception,
         *  budget). Only a half-built table matters — see {@link Tabling#abortProduction}. */
        void discard() {
            if (phase != PRODUCE) return;
            phase = DEAD;
            tb.abortProduction(this);
        }
    }
}
// END_CHANGE: ISS-2025-0463
