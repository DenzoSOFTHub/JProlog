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
import java.util.List;

// START_CHANGE: ISS-2025-0441 - engine v4, design B.4 (unification and term walkers).
/**
 * Every v4 term walker: unification, {@code ==}, standard order, {@code copy_term},
 * {@code term_variables}, {@code ground}, {@code numbervars}, {@code subsumes_term},
 * {@code cyclic_term} and the full dereference used to hand terms to legacy built-ins.
 *
 * <p>Three properties every walker here has, and that the v3.8.0 engine did not (design limits
 * L-04 and L-14):
 * <ol>
 *   <li><b>No Java recursion on the last argument.</b> A list of N cells is N nested {@code './2'}
 *       terms linked through their LAST argument, so a walker that recurses into it needs N Java
 *       frames. Every loop below iterates on the last argument and recurses (or pushes a work
 *       stack) only for arguments 1..N-1, which is what makes million-element lists work at the
 *       default JVM stack.</li>
 *   <li><b>Cycle safety.</b> {@code unify} switches to a visited set of {@code (compound, compound)}
 *       identity pairs once the work exceeds {@link #CYCLE_THRESHOLD}: a pair already seen is
 *       assumed to unify, which is exactly rational-tree unification, so
 *       {@code X = f(X), Y = f(Y), X = Y} SUCCEEDS in finite time instead of hanging
 *       (design decision 2, approved). {@code ==}, {@code compare}, {@code copy_term} and the
 *       resolver use the same scheme or a Brent spine test.</li>
 *   <li><b>Cancellable.</b> Every loop charges the {@link ResourceGuard} once every 4096 iterations,
 *       so the inference budget and the Stop interrupt reach a runaway walker — on v3.8.0 a cyclic
 *       unification polled nothing and could not be stopped at all.</li>
 * </ol>
 */
public final class Unify {

    private Unify() {}

    /** Work done before a walker starts paying for cycle detection. */
    private static final int CYCLE_THRESHOLD = 1024;

    /** Poll the resource guard once every this many walker iterations (design B.3). */
    private static final int GUARD_MASK = 0xFFF;      // 4096

    private static void poll(ResourceGuard g, int n) {
        if (g != null && (n & GUARD_MASK) == 0) g.step();
    }

    // ------------------------------------------------------------------ dereference

    /** Follow {@link Variable#ref} until unbound or non-variable. */
    public static Term deref(Term t) {
        while (t instanceof Variable) {
            Term r = ((Variable) t).ref;
            if (r == null) return t;
            t = r;
        }
        return t;
    }

    // ------------------------------------------------------------------ unification

    /** Called when an attributed variable is bound; may fail the unification. Installed on the
     *  {@link Bindings} by the {@link Machine} (per machine — never a static, so two engines on two
     *  threads cannot steal each other's hook the way the legacy ThreadLocal hook could). */
    interface AttrHandler { boolean onBind(Variable v, Term value, Bindings b); }

    /**
     * Bind {@code v}, honouring the WAM age rule (bind the younger cell to the older one so
     * dereference chains stay short and trailing stays cheap) and the occurs check.
     */
    static boolean bindVar(Variable v, Term value, Bindings b) {
        if (value instanceof Variable) {
            Variable w = (Variable) value;
            if (w == v) return true;
            if (w.serial > v.serial) {            // bind the YOUNGER (w) to the older (v)
                b.bind(w, v);
                return !w.hasAttributes() || b.attrHandler == null || b.attrHandler.onBind(w, v, b);
            }
            b.bind(v, w);
            return !v.hasAttributes() || b.attrHandler == null || b.attrHandler.onBind(v, w, b);
        }
        if (occursCheckActive() && occurs(v, value, b.guard)) return occursFailure(v, value);
        b.bind(v, value);
        return !v.hasAttributes() || b.attrHandler == null || b.attrHandler.onBind(v, value, b);
    }

    private static boolean occursCheckActive() {
        return Variable.isOccursCheckEnabled();
    }

    /** {@code occurs_check = error} (ISO 7.11.2.4) raises instead of failing. */
    private static boolean occursFailure(Variable v, Term value) {
        Term mode = it.denzosoft.jprolog.core.system.PrologFlags.getFlag("occurs_check");
        if (mode instanceof Atom && "error".equals(((Atom) mode).getName())) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms
                    .representationError("cyclic_term", "unify/2"));
        }
        return false;
    }

    /** Does {@code v} occur in {@code t}? Iterative, guarded, terminates on cyclic structures. */
    public static boolean occurs(Variable v, Term t, ResourceGuard g) {
        ArrayList<Term> work = new ArrayList<Term>();
        work.add(t);
        IdentityHashMap<Term, Boolean> seen = null;
        int n = 0;
        while (!work.isEmpty()) {
            Term x = deref(work.remove(work.size() - 1));
            poll(g, ++n);
            if (x == v) return true;
            if (x instanceof CompoundTerm) {
                if (n > CYCLE_THRESHOLD) {
                    if (seen == null) seen = new IdentityHashMap<Term, Boolean>();
                    if (seen.put(x, Boolean.TRUE) != null) continue;
                }
                List<Term> as = ((CompoundTerm) x).getArguments();
                for (int i = as.size() - 1; i >= 0; i--) work.add(as.get(i));
            }
        }
        return false;
    }

    /** Rational-tree unification: iterative, cycle-safe, cancellable. */
    public static boolean unify(Term x, Term y, Bindings b) {
        Term[] stack = null;
        int sp = 0;
        IdentityHashMap<Term, IdentityHashMap<Term, Boolean>> visited = null;
        int n = 0;
        Term a = x, c = y;
        while (true) {
            a = deref(a);
            c = deref(c);
            if (a != c) {
                if (a instanceof Variable) {
                    if (!bindVar((Variable) a, c, b)) return false;
                } else if (c instanceof Variable) {
                    if (!bindVar((Variable) c, a, b)) return false;
                } else if (a instanceof CompoundTerm && c instanceof CompoundTerm) {
                    CompoundTerm ca = (CompoundTerm) a, cc = (CompoundTerm) c;
                    List<Term> aa = ca.getArguments(), ba = cc.getArguments();
                    int ar = aa.size();
                    if (ar != ba.size() || !ca.getName().equals(cc.getName())) return false;
                    poll(b.guard, ++n);
                    boolean skip = false;
                    if (n > CYCLE_THRESHOLD) {
                        if (visited == null) visited = new IdentityHashMap<Term, IdentityHashMap<Term, Boolean>>();
                        IdentityHashMap<Term, Boolean> s = visited.get(ca);
                        if (s == null) { s = new IdentityHashMap<Term, Boolean>(); visited.put(ca, s); }
                        if (s.put(cc, Boolean.TRUE) != null) skip = true;   // rational tree: assume it unifies
                    }
                    if (!skip && ar > 0) {
                        for (int i = 0; i < ar - 1; i++) {
                            if (stack == null) stack = new Term[32];
                            if (sp + 2 > stack.length) {
                                Term[] bigger = new Term[stack.length << 1];
                                System.arraycopy(stack, 0, bigger, 0, sp);
                                stack = bigger;
                            }
                            stack[sp++] = aa.get(i);
                            stack[sp++] = ba.get(i);
                        }
                        a = aa.get(ar - 1);
                        c = ba.get(ar - 1);
                        continue;                                          // iterate down the spine
                    }
                } else if (!leafEqual(a, c)) {
                    return false;
                }
            }
            if (sp == 0) return true;
            c = stack[--sp];
            a = stack[--sp];
        }
    }

    private static boolean leafEqual(Term a, Term c) {
        if (a instanceof Atom) return (c instanceof Atom) && ((Atom) a).getName().equals(((Atom) c).getName());
        if (a instanceof Number) return (c instanceof Number) && a.equals(c);
        if (a instanceof PrologString) {
            return (c instanceof PrologString)
                && ((PrologString) a).getStringValue().equals(((PrologString) c).getStringValue());
        }
        return false;
    }

    // ------------------------------------------------------------------ == / \== / compare

    /** Structural identity ({@code ==/2}), dereferencing as it goes; cycle-safe. */
    public static boolean equalTerms(Term x, Term y, ResourceGuard g) {
        Term[] stack = null;
        int sp = 0;
        IdentityHashMap<Term, IdentityHashMap<Term, Boolean>> visited = null;
        int n = 0;
        Term a = x, c = y;
        while (true) {
            a = deref(a);
            c = deref(c);
            if (a != c) {
                if (a instanceof CompoundTerm && c instanceof CompoundTerm) {
                    CompoundTerm ca = (CompoundTerm) a, cc = (CompoundTerm) c;
                    List<Term> aa = ca.getArguments(), ba = cc.getArguments();
                    int ar = aa.size();
                    if (ar != ba.size() || !ca.getName().equals(cc.getName())) return false;
                    poll(g, ++n);
                    boolean skip = false;
                    if (n > CYCLE_THRESHOLD) {
                        if (visited == null) visited = new IdentityHashMap<Term, IdentityHashMap<Term, Boolean>>();
                        IdentityHashMap<Term, Boolean> s = visited.get(ca);
                        if (s == null) { s = new IdentityHashMap<Term, Boolean>(); visited.put(ca, s); }
                        if (s.put(cc, Boolean.TRUE) != null) skip = true;
                    }
                    if (!skip && ar > 0) {
                        for (int i = 0; i < ar - 1; i++) {
                            if (stack == null) stack = new Term[32];
                            if (sp + 2 > stack.length) {
                                Term[] bigger = new Term[stack.length << 1];
                                System.arraycopy(stack, 0, bigger, 0, sp);
                                stack = bigger;
                            }
                            stack[sp++] = aa.get(i);
                            stack[sp++] = ba.get(i);
                        }
                        a = aa.get(ar - 1);
                        c = ba.get(ar - 1);
                        continue;
                    }
                } else if (a instanceof Variable || c instanceof Variable) {
                    return false;                              // two DIFFERENT unbound cells
                } else if (!leafEqual(a, c)) {
                    return false;
                }
            }
            if (sp == 0) return true;
            c = stack[--sp];
            a = stack[--sp];
        }
    }

    /**
     * ISO standard order of terms: {@code Var &lt; Number &lt; Atom &lt; String &lt; Compound}.
     * Same ordering as {@code builtin.list.Sort.compareTerms} (so {@code compare/3}, {@code sort/2}
     * and {@code @&lt;} stay one total order) but iterative on the last argument.
     */
    public static int compareTerms(Term x, Term y, ResourceGuard g) {
        Term[] stack = null;
        int sp = 0;
        int n = 0;
        Term a = x, c = y;
        while (true) {
            a = deref(a);
            c = deref(c);
            if (a != c) {
                int r1 = rank(a), r2 = rank(c);
                if (r1 != r2) return (r1 < r2) ? -1 : 1;
                poll(g, ++n);
                if (r1 == 4) {
                    CompoundTerm ca = (CompoundTerm) a, cc = (CompoundTerm) c;
                    List<Term> aa = ca.getArguments(), ba = cc.getArguments();
                    int a1 = aa.size(), a2 = ba.size();
                    if (a1 != a2) return (a1 < a2) ? -1 : 1;
                    int fc = ca.getName().compareTo(cc.getName());
                    if (fc != 0) return fc < 0 ? -1 : 1;
                    if (a1 > 0) {
                        for (int i = a1 - 1; i >= 1; i--) {
                            if (stack == null) stack = new Term[32];
                            if (sp + 2 > stack.length) {
                                Term[] bigger = new Term[stack.length << 1];
                                System.arraycopy(stack, 0, bigger, 0, sp);
                                stack = bigger;
                            }
                            stack[sp++] = ba.get(i);
                            stack[sp++] = aa.get(i);
                        }
                        a = aa.get(0);
                        c = ba.get(0);
                        continue;
                    }
                } else {
                    int lc = compareLeaf(a, c, r1);
                    if (lc != 0) return lc;
                }
            }
            if (sp == 0) return 0;
            a = stack[--sp];
            c = stack[--sp];
        }
    }

    private static int compareLeaf(Term a, Term c, int rank) {
        switch (rank) {
            case 0: {
                long s1 = ((Variable) a).serial, s2 = ((Variable) c).serial;
                return (s1 < s2) ? -1 : (s1 > s2 ? 1 : 0);
            }
            case 1: {
                Number n1 = (Number) a, n2 = (Number) c;
                int r;
                if (n1.isInteger() && n2.isInteger()) r = n1.bigIntegerValue().compareTo(n2.bigIntegerValue());
                else r = Double.compare(n1.doubleValue(), n2.doubleValue());
                if (r != 0) return r < 0 ? -1 : 1;
                if (n1.isInteger() == n2.isInteger()) return 0;
                return n1.isInteger() ? 1 : -1;                       // float sorts before integer
            }
            case 2: {
                int r = ((Atom) a).getName().compareTo(((Atom) c).getName());
                return r < 0 ? -1 : (r > 0 ? 1 : 0);
            }
            case 3: {
                int r = ((PrologString) a).getStringValue().compareTo(((PrologString) c).getStringValue());
                return r < 0 ? -1 : (r > 0 ? 1 : 0);
            }
            default: return 0;
        }
    }

    private static int rank(Term t) {
        if (t instanceof Variable) return 0;
        if (t instanceof Number) return 1;
        if (t instanceof Atom) return 2;
        if (t instanceof PrologString) return 3;
        return 4;
    }

    // ------------------------------------------------------------------ resolve (full dereference)

    /**
     * Fully dereference {@code t}, sharing every unchanged sub-term. This is the handoff used for
     * legacy built-ins (which read bindings out of the term, not out of a store), for the answer
     * snapshot, for tracing and for {@code assert}. Unbound cells stay in the result as themselves.
     *
     * <p>Termination on cyclic terms: the LAST-argument spine (the direction in which lists and
     * right-nested structures grow, and the only one that can be long) is walked iteratively with a
     * Brent tortoise/hare test and cut at the repeat, so {@code X = f(X)} and {@code X = [1|X]}
     * resolve to the finite shared graph instead of expanding forever. The other arguments recurse,
     * bounded by {@link #MAX_ARG_DEPTH}: past that the sub-term is returned as it stands (still a
     * valid term — every v4 walker dereferences — just not flattened), which also removes any risk
     * of a StackOverflowError inside the resolver.
     */
    public static Term resolve(Term t, ResourceGuard g) {
        return resolve(t, g, 0);
    }

    /** Recursion cap for NON-last arguments (the last argument is iterated, never recursed). */
    private static final int MAX_ARG_DEPTH = 2000;

    private static Term resolve(Term t, ResourceGuard g, int depth) {
        t = deref(t);
        if (!(t instanceof CompoundTerm)) return t;
        if (depth > MAX_ARG_DEPTH) return t;

        // phase 1: collect the last-argument spine, with Brent cycle detection
        ArrayList<CompoundTerm> spine = new ArrayList<CompoundTerm>();
        CompoundTerm cur = (CompoundTerm) t;
        CompoundTerm slow = cur;
        boolean truncated = false;
        int steps = 0, power = 1, n = 0;
        while (true) {
            spine.add(cur);
            List<Term> as = cur.getArguments();
            if (as.isEmpty()) break;
            Term last = deref(as.get(as.size() - 1));
            if (!(last instanceof CompoundTerm)) break;
            CompoundTerm next = (CompoundTerm) last;
            poll(g, ++n);
            if (next == slow) { truncated = true; break; }                 // spine cycle
            if (++steps == power) { power <<= 1; steps = 0; slow = next; }
            cur = next;
        }

        // phase 2: rebuild bottom-up, sharing every node whose arguments resolve to themselves
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
                Term res;
                if (i == ar - 1 && hasSpineChild) {
                    res = belowChanged ? below : deref(arg);
                } else if (i == ar - 1 && truncated && k == spine.size() - 1) {
                    res = arg;                                             // cycle: leave it alone
                } else {
                    res = resolve(arg, g, depth + 1);
                }
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

    // ------------------------------------------------------------------ copy_term

    /** {@code copy_term/2}: fresh cells for the unbound variables, shared ground sub-terms; the
     *  same spine/depth discipline as {@link #resolve}. */
    public static Term copy(Term t, IdentityHashMap<Variable, Variable> map, ResourceGuard g) {
        return copy(t, map, g, 0);
    }

    private static Term copy(Term t, IdentityHashMap<Variable, Variable> map, ResourceGuard g, int depth) {
        t = deref(t);
        if (t instanceof Variable) {
            Variable v = (Variable) t;
            Variable c = map.get(v);
            if (c == null) { c = new Variable(); map.put(v, c); }
            return c;
        }
        if (!(t instanceof CompoundTerm)) return t;
        if (depth > MAX_ARG_DEPTH) return t;

        ArrayList<CompoundTerm> spine = new ArrayList<CompoundTerm>();
        CompoundTerm cur = (CompoundTerm) t;
        CompoundTerm slow = cur;
        boolean truncated = false;
        int steps = 0, power = 1, n = 0;
        while (true) {
            spine.add(cur);
            List<Term> as = cur.getArguments();
            if (as.isEmpty()) break;
            Term last = deref(as.get(as.size() - 1));
            if (!(last instanceof CompoundTerm)) break;
            CompoundTerm next = (CompoundTerm) last;
            poll(g, ++n);
            if (next == slow) { truncated = true; break; }
            if (++steps == power) { power <<= 1; steps = 0; slow = next; }
            cur = next;
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
                Term res;
                if (i == ar - 1 && hasSpineChild) {
                    res = belowChanged ? below : deref(arg);
                } else if (i == ar - 1 && truncated && k == spine.size() - 1) {
                    res = arg;
                } else {
                    res = copy(arg, map, g, depth + 1);
                }
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

    // ------------------------------------------------------------------ variables / ground / cyclic

    /** Collect the distinct unbound cells of {@code t}, in depth-first left-to-right order. */
    public static void termVariables(Term t, List<Variable> out, ResourceGuard g) {
        IdentityHashMap<Variable, Boolean> seen = new IdentityHashMap<Variable, Boolean>();
        IdentityHashMap<Term, Boolean> nodes = null;
        ArrayList<Term> work = new ArrayList<Term>();
        work.add(t);
        int n = 0;
        while (!work.isEmpty()) {
            Term x = deref(work.remove(work.size() - 1));
            poll(g, ++n);
            if (x instanceof Variable) {
                if (seen.put((Variable) x, Boolean.TRUE) == null) out.add((Variable) x);
            } else if (x instanceof CompoundTerm) {
                if (n > CYCLE_THRESHOLD) {
                    if (nodes == null) nodes = new IdentityHashMap<Term, Boolean>();
                    if (nodes.put(x, Boolean.TRUE) != null) continue;
                }
                List<Term> as = ((CompoundTerm) x).getArguments();
                for (int i = as.size() - 1; i >= 0; i--) work.add(as.get(i));
            }
        }
    }

    /** True when {@code t} contains no unbound cell. */
    public static boolean isGround(Term t, ResourceGuard g) {
        IdentityHashMap<Term, Boolean> nodes = null;
        ArrayList<Term> work = new ArrayList<Term>();
        work.add(t);
        int n = 0;
        while (!work.isEmpty()) {
            Term x = deref(work.remove(work.size() - 1));
            poll(g, ++n);
            if (x instanceof Variable) return false;
            if (x instanceof CompoundTerm) {
                if (n > CYCLE_THRESHOLD) {
                    if (nodes == null) nodes = new IdentityHashMap<Term, Boolean>();
                    if (nodes.put(x, Boolean.TRUE) != null) continue;
                }
                List<Term> as = ((CompoundTerm) x).getArguments();
                for (int i = as.size() - 1; i >= 0; i--) work.add(as.get(i));
            }
        }
        return true;
    }

    /**
     * {@code cyclic_term/1}: a real test now that rational trees exist. Iterative depth-first with
     * the set of nodes on the CURRENT path — a node reached twice through different branches is
     * sharing, not a cycle — so a million-element (acyclic) list answers false without recursing.
     */
    public static boolean isCyclic(Term t, ResourceGuard g) {
        Term root = deref(t);
        if (!(root instanceof CompoundTerm)) return false;
        ArrayList<CompoundTerm> stack = new ArrayList<CompoundTerm>();
        ArrayList<int[]> pos = new ArrayList<int[]>();
        IdentityHashMap<Term, Boolean> path = new IdentityHashMap<Term, Boolean>();
        stack.add((CompoundTerm) root);
        pos.add(new int[]{0});
        path.put(root, Boolean.TRUE);
        int n = 0;
        while (!stack.isEmpty()) {
            poll(g, ++n);
            int d = stack.size() - 1;
            CompoundTerm node = stack.get(d);
            int[] i = pos.get(d);
            List<Term> as = node.getArguments();
            if (i[0] >= as.size()) {
                path.remove(node);
                stack.remove(d);
                pos.remove(d);
                continue;
            }
            Term child = deref(as.get(i[0]++));
            if (child instanceof CompoundTerm) {
                if (path.containsKey(child)) return true;
                path.put(child, Boolean.TRUE);
                stack.add((CompoundTerm) child);
                pos.add(new int[]{0});
            }
        }
        return false;
    }

    /** {@code numbervars/3}: bind each unbound cell of {@code t} to {@code '$VAR'(N)}. */
    public static int numberVars(Term t, int start, Bindings b) {
        List<Variable> vars = new ArrayList<Variable>();
        termVariables(t, vars, b.guard);
        int n = start;
        Atom dollarVar = new Atom("$VAR");
        for (int i = 0; i < vars.size(); i++) {
            List<Term> args = new ArrayList<Term>(1);
            args.add(Number.valueOf(n++));
            b.bind(vars.get(i), new CompoundTerm(dollarVar, args));
        }
        return n;
    }

    /**
     * {@code subsumes_term(General, Specific)}: does General subsume Specific without <b>changing</b>
     * Specific? Implemented as one-way matching under a trail mark that is always undone.
     *
     * <p>START_CHANGE: ISS-2025-0456 - the old test was "no variable of Specific has a {@code ref}
     * afterwards", which is wrong for the variable-variable case: {@link #bindVar} binds the
     * YOUNGER cell to the older one, so in {@code subsumes_term(f(X), f(Y))} the younger cell —
     * whichever side it is on — ends up with a ref and the test failed half the time. On v4
     * {@code subsumes_term(f(X), f(Y))} therefore answered false where the v2 engine (and every
     * other Prolog) answers true, and bagof/3's variant grouping inherited the bug.
     *
     * <p>The correct condition is SWI's: after the unification the variables of Specific must still
     * dereference to <b>distinct unbound cells</b> — a variable of Specific may be aliased to a
     * variable of General (that changes nothing about Specific), but it may not become bound to a
     * non-variable, and two of them may not collapse into one
     * ({@code subsumes_term(f(A,A), f(B,C))} is false).
     */
    public static boolean subsumes(Term general, Term specific, Bindings b) {
        List<Variable> before = new ArrayList<Variable>();
        termVariables(specific, before, b.guard);
        int m = b.mark();
        b.forceTrail++;
        boolean ok;
        try {
            ok = unify(general, specific, b);
            if (ok) {
                IdentityHashMap<Variable, Boolean> distinct = new IdentityHashMap<Variable, Boolean>();
                for (int i = 0; i < before.size(); i++) {
                    Term d = deref(before.get(i));
                    if (!(d instanceof Variable)) { ok = false; break; }          // became non-var
                    if (distinct.put((Variable) d, Boolean.TRUE) != null) { ok = false; break; }
                }
            }
        } finally {
            b.undo(m);          // ISS-2025-0448: undo inside the extent, then close it
            b.forceTrail--;
        }
        return ok;
    }
    // END_CHANGE: ISS-2025-0456
}
// END_CHANGE: ISS-2025-0441
