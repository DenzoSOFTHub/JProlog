package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0439 - engine v4, design B.2 (clause skeletons) + B.7 (generations).
/**
 * A clause compiled ONCE into a numbered-variable skeleton.
 *
 * <p>Head and body are the clause's terms with every distinct variable replaced by a
 * {@link VarRef}; {@link #nvars} says how big the activation frame must be. Activating the clause
 * costs one {@code Term[nvars]} — no {@code HashMap<String,Variable>}, no {@code "_R<id>_<name>"}
 * strings, no copy of the clause: {@link #unifyHead} unifies the goal directly against the
 * skeleton, filling frame slots as it goes, and body goals are instantiated one at a time when the
 * machine pushes them ({@link #instantiate}).
 *
 * <p>Variables are numbered <b>by name</b>, deliberately: on the paths that build a {@link Rule}
 * (the legacy parser, {@code JpcReader}, {@code assert} of a term read from a stream) two
 * occurrences of {@code X} can be two objects, and on those paths the name is the identity.
 *
 * <p>{@link #birth}/{@link #death} are the clause's generation interval, the mechanism behind the
 * logical update view in {@link ClauseStore}: a call captures the current generation {@code g} and
 * sees exactly the clauses with {@code birth <= g < death}.
 */
public final class Clause {

    /** Head skeleton (an {@link Atom} for arity 0). */
    public final Term head;
    /** Body goals, flattened over {@code ','/2}; empty for a fact. */
    public final Term[] body;
    /** Size of the activation frame. */
    public final int nvars;
    /** ISS-2025-0551: slots [0, headVars) are the head's variables, [headVars, nvars) body-only. */
    final int headVars;
    /** true when the head contains no variable (activation can skip the frame entirely). */
    public final boolean groundHead;
    /** First-argument index key, or null when the head's first argument is a variable / there is none. */
    public final Object firstArgKey;
    /** The {@link Rule} this was compiled from — the identity {@code retract} removes from the KB. */
    public final Rule rule;
    /** 1-based source line of the clause head, or -1 (IDE breakpoints). */
    public final int sourceLine;

    /** Generation in which this clause became visible (design B.7). */
    long birth;
    /** Generation in which it stopped being visible; {@link Long#MAX_VALUE} while alive. */
    long death = Long.MAX_VALUE;
    // START_CHANGE: ISS-2025-0547 - wave P2.9: the clause's position in its predicate's source
    // order (assertz counts up, asserta counts down, a re-sync renumbers). The first-argument index
    // merges a bucket with the variable-headed clauses by comparing ordinals, in O(|bucket| +
    // |variable-headed|), instead of rescanning the whole predicate after every write.
    long ord;
    // END_CHANGE: ISS-2025-0547

    private Clause(Term head, Term[] body, int nvars, int headVars, boolean groundHead, Object firstArgKey,
                   Rule rule, int sourceLine) {
        this.head = head;
        this.body = body;
        this.nvars = nvars;
        this.headVars = headVars;
        this.groundHead = groundHead;
        this.firstArgKey = firstArgKey;
        this.rule = rule;
        this.sourceLine = sourceLine;
    }

    public boolean isAlive(long generation) { return birth <= generation && generation < death; }

    public boolean isFact() { return body.length == 0; }

    // ------------------------------------------------------------------ compilation

    /** Compile {@code r} into a skeleton. */
    public static Clause compile(Rule r) {
        Map<String, Integer> index = new HashMap<String, Integer>();
        Term head = toSkeleton(r.getHead(), index);
        int headVars = index.size();                                   // ISS-2025-0551
        List<Term> gs = r.getBody();
        ArrayList<Term> flat = new ArrayList<Term>(gs.size());
        for (int i = 0; i < gs.size(); i++) flattenConjunction(gs.get(i), index, flat);
        Term[] body = flat.toArray(new Term[flat.size()]);
        return new Clause(head, body, index.size(), headVars, index.isEmpty() || !containsVarRef(head),
                          firstArgKey(head), r, r.getSourceLine());
    }

    private static void flattenConjunction(Term goal, Map<String, Integer> index, List<Term> out) {
        Term g = goal;
        while (g instanceof CompoundTerm && ",".equals(((CompoundTerm) g).getName())
                && ((CompoundTerm) g).getArguments().size() == 2) {
            flattenConjunction(((CompoundTerm) g).getArguments().get(0), index, out);
            g = ((CompoundTerm) g).getArguments().get(1);
        }
        // START_CHANGE: ISS-2025-0540 - every compound body goal is a Skel, so it can carry its
        // call-site cache; a GROUND goal wraps the original term, which instantiate hands out.
        // The goal positions inside `,`/`;`/`->`/`*->` are wrapped the same way, because the drive
        // loop expands those four constructs lazily (Machine.stepControlSkel): their branches run
        // as skeleton goals over the clause frame, with call sites of their own.
        out.add(goalSkel(toSkeleton(g, index), 0));
        // END_CHANGE: ISS-2025-0540
    }

    // START_CHANGE: ISS-2025-0540
    /** Is {@code name/2} one of the control constructs the drive loop expands lazily? */
    static boolean isLazyControl(String name) {
        return ",".equals(name) || ";".equals(name) || "->".equals(name) || "*->".equals(name);
    }

    /**
     * Wrap a goal-position skeleton: a ground compound becomes a Skel carrying the original term,
     * and a lazy control construct gets its two goal arguments wrapped recursively (iterative
     * along the right spine, where long conjunctions and else-if chains grow).
     */
    private static Term goalSkel(Term sk, int depth) {
        if (!(sk instanceof CompoundTerm)) return sk;
        if (sk instanceof Skel && ((Skel) sk).plain != null) return sk;
        CompoundTerm c = (CompoundTerm) sk;
        boolean control = c.arity() == 2 && isLazyControl(c.getName()) && depth < DEEP;
        if (!control) {
            if (sk instanceof Skel) return sk;                      // non-ground: already a Skel
            Term[] as = new Term[c.arity()];
            for (int i = 0; i < as.length; i++) as[i] = c.arg(i);
            return new Skel(c.getFunctor(), as, c);
        }
        // collect the right spine of control nodes, wrap each left argument, rebuild bottom-up
        ArrayList<CompoundTerm> spine = new ArrayList<CompoundTerm>();
        Term cur = sk;
        while (cur instanceof CompoundTerm && !(cur instanceof Skel && ((Skel) cur).plain != null)
                && ((CompoundTerm) cur).arity() == 2 && isLazyControl(((CompoundTerm) cur).getName())) {
            spine.add((CompoundTerm) cur);
            cur = ((CompoundTerm) cur).arg(1);
        }
        Term below = goalSkel(cur, depth + 1);
        for (int k = spine.size() - 1; k >= 0; k--) {
            CompoundTerm node = spine.get(k);
            Term[] as = { goalSkel(node.arg(0), depth + 1), below };
            below = new Skel(node.getFunctor(), as, (node instanceof Skel) ? null : node);
        }
        return below;
    }
    // END_CHANGE: ISS-2025-0540

    // START_CHANGE: ISS-2025-0543 - wave P2.4: a skeleton compound that must be REBUILT on
    // activation is a {@code Skel}; a sub-term that contains no variable is left as the original
    // (plain) term and shared by every activation. {@link #instantiate} therefore stops at the
    // first non-Skel node instead of walking every ground sub-term of the clause on every call
    // (`deriv(x*x*x+x*x*y+..., x, D)` walked its whole constant expression per call). A Skel never
    // escapes this package (invariant 4): instantiate returns plain CompoundTerms only.
    /** A skeleton compound: contains a {@link VarRef}, or is a ground body goal ({@link #plain}). */
    static final class Skel extends CompoundTerm {
        /** Non-null for a GROUND body goal: the term instantiate returns as it is. */
        final CompoundTerm plain;
        /** The call-site cache of a body goal (ISS-2025-0540); written racily, read as a whole. */
        volatile Object site;
        /** ISS-2025-0540: `,`/`;`/`->`/`*->` — expanded lazily by the drive loop. */
        final boolean lazyControl;
        Skel(it.denzosoft.jprolog.core.terms.Atom functor, Term[] args, CompoundTerm plain) {
            super(functor, args);
            this.plain = plain;
            this.lazyControl = args.length == 2 && isLazyControl(functor.getName());
        }
    }
    // END_CHANGE: ISS-2025-0543

    /** Replace every {@link Variable} by a {@link VarRef}; iterative on the last argument. */
    private static Term toSkeleton(Term t, Map<String, Integer> index) {
        return toSkeleton(t, index, 0);
    }

    private static Term toSkeleton(Term t, final Map<String, Integer> index, int depth) {
        // START_CHANGE: ISS-2025-0524 - past the recursion cap, the fully iterative walker
        if (depth > DEEP) {
            return mapDeep(t, true, new LeafMap() {
                @Override public Term map(Term leaf) { return toSkeleton(leaf, index, 0); }
            });
        }
        // END_CHANGE: ISS-2025-0524
        if (t instanceof Variable) {
            String name = ((Variable) t).getName();
            Integer k = index.get(name);
            if (k == null) { k = Integer.valueOf(index.size()); index.put(name, k); }
            return new VarRef(k.intValue());
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
            int ar = node.arity();
            boolean hasSpineChild = (k < spine.size() - 1);
            Term[] out = null;
            for (int i = 0; i < ar; i++) {
                Term arg = node.arg(i);
                Term res = (i == ar - 1 && hasSpineChild)
                    ? (belowChanged ? below : arg)
                    : toSkeleton(arg, index, depth + 1);
                if (res != arg && out == null) {
                    out = new Term[ar];
                    for (int j = 0; j < i; j++) out[j] = node.arg(j);
                }
                if (out != null) out[i] = res;
            }
            if (out == null) { below = node; belowChanged = false; }
            else { below = new Skel(node.getFunctor(), out, null); belowChanged = true; }   // ISS-2025-0543
        }
        return below;
    }

    // START_CHANGE: ISS-2025-0524 - wave P1.11: a clause nested deeper than DEEP levels in its
    // NON-last arguments used to be compiled and instantiated by Java recursion, so asserting a
    // term a few thousand levels deep raised resource_error(stack_overflow). Past DEEP the walk
    // continues on an explicit stack. Clause terms are acyclic (assert rejects cyclic ones).
    private static final int DEEP = 1000;

    private interface LeafMap { Term map(Term leaf); }

    /** Rebuild {@code t} replacing every non-compound leaf by {@code f.map(leaf)}; sub-terms
     *  that do not change are shared. Iterative: no Java recursion at any depth. */
    private static Term mapDeep(Term t, boolean skel, LeafMap f) {
        if (!(t instanceof CompoundTerm)) return f.map(t);
        ArrayList<CompoundTerm> nodes = new ArrayList<CompoundTerm>();
        ArrayList<List<Term>> outs = new ArrayList<List<Term>>();
        int[] idx = new int[64];
        nodes.add((CompoundTerm) t);
        outs.add(null);
        Term delivered = null;
        boolean hasDelivery = false;
        while (true) {
            int top = nodes.size() - 1;
            CompoundTerm node = nodes.get(top);
            List<Term> as = node.getArguments();
            int ar = as.size();
            boolean descended = false;
            while (idx[top] < ar) {
                int i = idx[top];
                Term arg = as.get(i);
                Term res;
                if (hasDelivery) {
                    res = delivered;
                    delivered = null;
                    hasDelivery = false;
                } else if (arg instanceof CompoundTerm) {
                    nodes.add((CompoundTerm) arg);
                    outs.add(null);
                    if (nodes.size() > idx.length) idx = java.util.Arrays.copyOf(idx, idx.length << 1);
                    idx[nodes.size() - 1] = 0;
                    descended = true;
                    break;
                } else {
                    res = f.map(arg);
                }
                List<Term> out = outs.get(top);
                if (res != arg && out == null) {
                    out = new ArrayList<Term>(ar);
                    for (int j = 0; j < i; j++) out.add(as.get(j));
                    outs.set(top, out);
                }
                if (out != null) out.add(res);
                idx[top]++;
            }
            if (descended) continue;
            List<Term> out = outs.get(top);
            Term built = (out == null) ? node
                : skel ? new Skel(node.getFunctor(), out.toArray(new Term[out.size()]), null)   // ISS-2025-0543
                : new CompoundTerm(node.getFunctor(), out);
            nodes.remove(top);
            outs.remove(top);
            if (top == 0) return built;
            delivered = built;
            hasDelivery = true;
        }
    }
    // END_CHANGE: ISS-2025-0524

    private static boolean containsVarRef(Term t) {
        if (t instanceof VarRef) return true;
        if (!(t instanceof CompoundTerm)) return false;
        ArrayList<Term> work = new ArrayList<Term>();
        work.add(t);
        while (!work.isEmpty()) {
            Term x = work.remove(work.size() - 1);
            if (x instanceof VarRef) return true;
            if (x instanceof CompoundTerm) {
                List<Term> as = ((CompoundTerm) x).getArguments();
                for (int i = 0; i < as.size(); i++) work.add(as.get(i));
            }
        }
        return false;
    }

    /**
     * First-argument index key of a head skeleton: the atom name, the numeric value, the string, or
     * {@code name/arity} for a compound. Null when the first argument is a variable (such a clause
     * matches every key and is merged into every bucket) or the predicate has no arguments.
     */
    static Object firstArgKey(Term head) {
        if (!(head instanceof CompoundTerm)) return null;
        List<Term> as = ((CompoundTerm) head).getArguments();
        if (as.isEmpty()) return null;
        return argKey(as.get(0));
    }

    // START_CHANGE: ISS-2025-0502 - first-argument index key of a GOAL (or of a retract/clause
    // head pattern) whose arguments are live cells: the first argument has to be dereferenced
    // before it is classified, which {@link #firstArgKey} (a skeleton walker, VarRef-based) must
    // not do. Null — "unindexable" — whenever the goal is not compound, has no arguments, or its
    // first argument derefs to an unbound cell; {@code ClauseStore.Predicate.select(null)} is then
    // the full clause list, so a miss never drops a clause.
    /** The index key of a live goal term, or null when the goal cannot be indexed. */
    static Object argKey1(Term goal) {
        if (!(goal instanceof CompoundTerm)) return null;
        CompoundTerm c = (CompoundTerm) goal;                           // ISS-2025-0543
        if (c.arity() == 0) return null;
        return argKey(Unify.deref(c.arg(0)));
    }
    // END_CHANGE: ISS-2025-0502

    /**
     * The index key of a (already dereferenced) goal argument, or null when it is unindexable.
     *
     * <p>START_CHANGE: ISS-2025-0502 - the key of an ATOM is its name and the key of a NUMBER is a
     * boxed {@code Long}/{@code Double}/{@code BigInteger}, not a freshly concatenated string. This
     * method runs once per call with a bound first argument — {@code loop(1000000)} evaluates it a
     * million times — and the old {@code "i" + n.bigIntegerValue().toString()} allocated a
     * {@code BigInteger}, its decimal rendering and a concatenation every single time.
     *
     * <p><b>Type faithfulness</b> is what the key must preserve, and it does: an integer is a
     * {@code Long} (a value wider than 64 bits keeps its {@code BigInteger}, and a {@code Number}
     * that happens to hold a small {@code BigInteger} is narrowed to the same {@code Long}, so the
     * two representations of 5 share a bucket), a float is a {@code Double} — whose
     * {@code equals} agrees with {@code Number.equals}'s {@code Double.compare} on both NaN and
     * -0.0 — and neither can collide with the other or with a {@code String}. {@code 1}, {@code 1.0},
     * {@code '1'} and {@code "1"} therefore still land in four different buckets.
     *
     * <p>A compound and a Prolog string get a small value key of their own ({@link FunctorKey},
     * {@link StringKey}) rather than a prefixed string, so no two kinds of key can ever collide and
     * the per-call cost is one non-escaping object instead of a StringBuilder. Even a collision
     * would be safe — it merges two buckets, which makes the candidate set larger, never smaller,
     * and an index may over-approximate but must never under-approximate (the ISS-2025-0340 hazard,
     * pinned by {@code EngineV4IndexingTest}). END_CHANGE: ISS-2025-0502
     */
    static Object argKey(Term a) {
        if (a instanceof VarRef || a instanceof Variable) return null;
        if (a instanceof Atom) return ((Atom) a).getName();
        if (a instanceof Number) {
            Number n = (Number) a;
            // Type-faithful: 1 and 1.0 are different terms and must land in different buckets.
            if (!n.isInteger()) return Double.valueOf(n.doubleValue());
            if (n.isLongInteger()) return Long.valueOf(n.longValue());
            return n.bigIntegerValue();
        }
        if (a instanceof PrologString) return new StringKey(((PrologString) a).getStringValue());
        if (a instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) a;
            return new FunctorKey(c.getName(), c.arity());
        }
        return null;
    }

    // START_CHANGE: ISS-2025-0502 - two tiny value keys instead of a concatenated string. Building
    // {@code "c" + name + "/" + arity} on every call with a compound first argument (which is every
    // step of `app/3` over a list) costs a StringBuilder, a char array and a String; a FunctorKey is
    // one small object that the JIT can usually scalarise away, because it never escapes the
    // {@code HashMap.get} in {@code ClauseStore.Predicate.select}. They also make the key space
    // exactly type-faithful: no atom name can collide with a compound's or a string's key.
    /** The index key of a compound goal argument: functor name and arity. */
    static final class FunctorKey {
        private final String name;
        private final int arity;
        FunctorKey(String name, int arity) { this.name = name; this.arity = arity; }
        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof FunctorKey)) return false;
            FunctorKey k = (FunctorKey) o;
            return arity == k.arity && name.equals(k.name);
        }
        @Override public int hashCode() { return name.hashCode() * 31 + arity; }
        @Override public String toString() { return name + "/" + arity; }
    }

    /** The index key of a {@code PrologString}, distinct from the atom of the same text. */
    static final class StringKey {
        private final String value;
        StringKey(String value) { this.value = value; }
        @Override public boolean equals(Object o) {
            return (o instanceof StringKey) && value.equals(((StringKey) o).value);
        }
        @Override public int hashCode() { return value.hashCode() ^ 0x5715; }
        @Override public String toString() { return "\"" + value + "\""; }
    }
    // END_CHANGE: ISS-2025-0502

    // ------------------------------------------------------------------ activation

    /**
     * Instantiate a skeleton against {@code frame}, allocating a fresh cell for each frame slot
     * still empty. Shares every sub-term that contains no variable; iterative on the last argument.
     */
    static Term instantiate(Term t, Term[] frame) {
        return instantiate(t, frame, 0);
    }

    private static Term instantiate(Term t, final Term[] frame, int depth) {
        if (t instanceof VarRef) return slot(frame, ((VarRef) t).index);
        // START_CHANGE: ISS-2025-0543 - wave P2.4: only a Skel contains variables; anything else
        // (atoms, numbers, strings, ground compounds) is shared as it is — no walk, no copy.
        if (!(t instanceof Skel)) return t;
        Skel s = (Skel) t;
        if (s.plain != null) return s.plain;
        // END_CHANGE: ISS-2025-0543
        // START_CHANGE: ISS-2025-0524 - past the recursion cap, the fully iterative walker
        if (depth > DEEP) {
            return mapDeep(t, false, new LeafMap() {
                @Override public Term map(Term leaf) {
                    return (leaf instanceof VarRef) ? slot(frame, ((VarRef) leaf).index) : leaf;
                }
            });
        }
        // END_CHANGE: ISS-2025-0524
        // START_CHANGE: ISS-2025-0543 - one argument array per rebuilt node, adopted by the new
        // term; the last-argument spine is walked iteratively by filling the parent's last slot
        // once the child exists (no spine list, no per-node ArrayList, no list copy).
        CompoundTerm root = null;
        Term[] parent = null;
        while (true) {
            int n = s.arity();
            Term[] a = new Term[n];
            for (int i = 0; i < n - 1; i++) a[i] = instantiate(s.arg(i), frame, depth + 1);
            CompoundTerm node = new CompoundTerm(s.getFunctor(), a);
            if (parent == null) root = node; else parent[parent.length - 1] = node;
            if (n == 0) return root;
            Term last = s.arg(n - 1);
            if (last instanceof Skel && ((Skel) last).plain == null) {
                parent = a;
                s = (Skel) last;
                continue;
            }
            a[n - 1] = instantiate(last, frame, depth + 1);
            return root;
        }
        // END_CHANGE: ISS-2025-0543
    }

    // START_CHANGE: ISS-2025-0551 - a clause's body-only variables are created when the clause is
    // ACTIVATED, not when the goal that first mentions them is reached. A cell's age decides
    // whether binding it is trailed (Bindings.bind: only cells older than the newest choice
    // point), and a cell created lazily AFTER a body goal left a choice point was "young" — so
    // `p(Y) :- q(Z), X is Z + 1, Y = X.` bound X untrailed, and on backtracking into q the frame
    // still held the bound X: the second answer failed (`X is 3` against X = 2). 4.4.0 returned
    // only Y = 2. Creating the cells with the frame gives them the frame's age, which is what the
    // WAM does with an environment's permanent variables. (Head variables are aliased or created
    // by the head unification itself, inside the clause's own choice point, which is correct.)
    /** Create the body-only cells of a freshly unified frame (see ISS-2025-0551). */
    void fillBodySlots(Term[] frame) {
        for (int i = headVars; i < nvars; i++) if (frame[i] == null) frame[i] = new Variable();
    }
    // END_CHANGE: ISS-2025-0551

    private static Term slot(Term[] frame, int k) {
        Term v = frame[k];
        if (v == null) { v = new Variable(); frame[k] = v; }
        return v;
    }

    /**
     * Unify {@code goal} against this clause's head skeleton, filling {@code frame}.
     *
     * <p>The win over "copy the clause, then unify" is the first-occurrence rule: a head variable
     * whose slot is still empty is <b>aliased directly to the goal's sub-term</b> — no cell is
     * allocated and nothing is trailed, because the frame itself dies on backtracking. Only
     * repeated head variables and body-only variables ever become real cells.
     */
    boolean unifyHead(Term goal, Term[] frame, Bindings b) {
        if (!(head instanceof CompoundTerm)) return true;              // arity 0: the functor matched
        CompoundTerm hs = (CompoundTerm) head;                          // ISS-2025-0543: no list views
        CompoundTerm gs = (CompoundTerm) Unify.deref(goal);
        int ar = hs.arity();
        for (int i = 0; i < ar; i++) {
            if (!unifyArg(hs.arg(i), frame, gs.arg(i), b)) return false;
        }
        return true;
    }

    private static boolean unifyArg(Term skel, Term[] frame, Term g, Bindings b) {
        while (true) {
            if (skel instanceof VarRef) {
                int k = ((VarRef) skel).index;
                Term cur = frame[k];
                if (cur == null) { frame[k] = Unify.deref(g); return true; }
                return Unify.unify(cur, g, b);
            }
            Term gd = Unify.deref(g);
            if (gd instanceof Variable) {
                return Unify.bindVar((Variable) gd, instantiate(skel, frame), b);
            }
            if (skel instanceof CompoundTerm) {
                if (!(gd instanceof CompoundTerm)) return false;
                CompoundTerm cs = (CompoundTerm) skel, cg = (CompoundTerm) gd;
                int ar = cs.arity();                                    // ISS-2025-0543
                if (ar != cg.arity() || !cs.getName().equals(cg.getName())) return false;
                for (int i = 0; i < ar - 1; i++) {
                    if (!unifyArg(cs.arg(i), frame, cg.arg(i), b)) return false;
                }
                if (ar == 0) return true;
                skel = cs.arg(ar - 1);                                  // iterate down the spine
                g = cg.arg(ar - 1);
                continue;
            }
            if (skel instanceof Atom) return (gd instanceof Atom) && ((Atom) skel).getName().equals(((Atom) gd).getName());
            if (skel instanceof Number) return (gd instanceof Number) && skel.equals(gd);
            if (skel instanceof PrologString) {
                return (gd instanceof PrologString)
                    && ((PrologString) skel).getStringValue().equals(((PrologString) gd).getStringValue());
            }
            return false;
        }
    }

    /** The clause as a {@code (Head :- Body)} term with fresh cells — for {@code clause/2},
     *  {@code retract/1} and {@code listing/1}. */
    Term toTerm() {
        Term[] frame = new Term[nvars];
        Term h = instantiate(head, frame);
        Term bod;
        if (body.length == 0) {
            bod = new Atom("true");
        } else {
            bod = instantiate(body[body.length - 1], frame);
            for (int i = body.length - 2; i >= 0; i--) {
                List<Term> args = new ArrayList<Term>(2);
                args.add(instantiate(body[i], frame));
                args.add(bod);
                bod = new CompoundTerm(new Atom(","), args);
            }
        }
        List<Term> args = new ArrayList<Term>(2);
        args.add(h);
        args.add(bod);
        return new CompoundTerm(new Atom(":-"), args);
    }

    @Override
    public String toString() { return "Clause[" + head + (body.length == 0 ? "" : " :- ...") + "]"; }
}
// END_CHANGE: ISS-2025-0439
