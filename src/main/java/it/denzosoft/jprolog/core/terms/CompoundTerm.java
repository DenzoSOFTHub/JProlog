package it.denzosoft.jprolog.core.terms;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;



// START_CHANGE: ISS-2025-0428 - ENG-09: every walker below is TAIL-ITERATIVE on the LAST argument.
// A list of N cells is N nested './2' terms whose SECOND (last) argument is the tail, so a walker
// that recurses into all arguments needs N Java frames and dies with StackOverflowError at ~20-30k
// elements even with -Xss4m. Looping on argument N and recursing only into arguments 1..N-1 makes
// the Java depth equal to the nesting of NON-LAST arguments, which is small for lists and for the
// vast majority of real data — so million-element lists work with the default JVM stack.
// END_CHANGE: ISS-2025-0428
public class CompoundTerm extends Term {

    private Atom functor;
    private List<Term> arguments;
    // START_CHANGE: ISS-2025-0091 - Cache unmodifiable view to avoid wrapping on every call
    private List<Term> unmodifiableArguments;
    // END_CHANGE: ISS-2025-0091

    public CompoundTerm(Atom functor, List<Term> arguments) {
        this.functor = functor;
        this.arguments = new ArrayList<>(arguments); // Make a copy
    }

    public Atom getFunctor() {
        return functor;
    }

    // START_CHANGE: ISS-2025-0091 - Cache unmodifiable view, create once on first access
    @Override
    public List<Term> getArguments() {
        if (unmodifiableArguments == null) {
            unmodifiableArguments = Collections.unmodifiableList(arguments);
        }
        return unmodifiableArguments;
    }

    // START_CHANGE: R1 - setarg/3 support: destructive arg replacement
    /** Replace argument at 1-based index. Used by setarg/3; caller must record undo via Trail. */
    public Term setArgument(int index1based, Term newArg) {
        if (index1based < 1 || index1based > arguments.size()) {
            throw new IndexOutOfBoundsException("setarg: index " + index1based + " out of range 1.." + arguments.size());
        }
        Term old = arguments.get(index1based - 1);
        arguments.set(index1based - 1, newArg);
        // Invalidate cached unmodifiable view
        unmodifiableArguments = null;
        return old;
    }
    // END_CHANGE: R1
    // END_CHANGE: ISS-2025-0091

    @Override
    public String getName() {
        return functor.getName();
    }

    // START_CHANGE: ISS-2025-0096 - Optimized compound unification rollback
    // On success (common case in matching rules): no rollback needed; on failure, one snapshot for
    // the whole spine is restored.
    // ISS-2025-0484 - wave W9: the LayeredMap mark/rollback fast path is gone with the recursive
    // recursive solver, the only caller that ever passed one.
    @Override
    public boolean unify(Term term, Map<String, Term> substitution) {
        if (term instanceof Variable) {
            return term.unify(this, substitution);
        } else if (term instanceof CompoundTerm) {
            CompoundTerm otherCompound = (CompoundTerm) term;
            if (!this.functor.getName().equals(otherCompound.functor.getName()) ||
                this.arguments.size() != otherCompound.arguments.size()) {
                return false;
            }

            // START_CHANGE: ISS-2025-0163 - Correct rollback: snapshot full map, not just keys
            // retainAll(savedKeys) only removes added keys but doesn't restore overwritten values.
            // Full snapshot ensures correct rollback even if values are overwritten.
            // START_CHANGE: ISS-2025-0428 - ENG-09: ONE snapshot for the whole spine. The old code
            // snapshotted the substitution at EVERY nesting level (O(depth x |bindings|) copying on
            // a list) even though the outermost restore already undoes everything.
            java.util.Map<String, Term> snapshot = new java.util.HashMap<>(substitution);
            if (unifySpine(this, otherCompound, substitution)) return true;
            substitution.clear();
            substitution.putAll(snapshot);
            return false;
            // END_CHANGE: ISS-2025-0428
            // END_CHANGE: ISS-2025-0163
        } else {
            return false;
        }
    }
    // END_CHANGE: ISS-2025-0096

    // START_CHANGE: ISS-2025-0428 - ENG-09: unify the last-argument spine iteratively. Caller owns
    // the rollback (one snapshot/mark for the whole spine); this only reports success or failure.
    private static boolean unifySpine(CompoundTerm a, CompoundTerm b, Map<String, Term> substitution) {
        while (true) {
            if (!a.functor.getName().equals(b.functor.getName())
                    || a.arguments.size() != b.arguments.size()) {
                return false;
            }
            int n = a.arguments.size();
            for (int i = 0; i < n - 1; i++) {
                if (!a.arguments.get(i).unify(b.arguments.get(i), substitution)) return false;
            }
            if (n == 0) return true;
            Term la = a.arguments.get(n - 1), lb = b.arguments.get(n - 1);
            if (la.getClass() == CompoundTerm.class && lb.getClass() == CompoundTerm.class) {
                a = (CompoundTerm) la; b = (CompoundTerm) lb;      // iterate down the spine
                continue;
            }
            return la.unify(lb, substitution);                     // leaf (or var): one recursion
        }
    }
    // END_CHANGE: ISS-2025-0428

    @Override
    public boolean isGround() {
        // START_CHANGE: ISS-2025-0428 - ENG-09: iterative on the last argument
        Term t = this;
        while (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            int n = c.arguments.size();
            for (int i = 0; i < n - 1; i++) {
                if (!c.arguments.get(i).isGround()) return false;
            }
            if (n == 0) return true;
            t = c.arguments.get(n - 1);
        }
        return t.isGround();
        // END_CHANGE: ISS-2025-0428
    }

    @Override
    public String toString() {
        // START_CHANGE: ISS-2025-0428 - ENG-09: iterative on the last argument. f(f(f(...))) nested
        // 50 000 deep (and any long list tail reached through a non-'.' functor) used to overflow.
        StringBuilder sb = new StringBuilder();
        appendTerm(sb, this);
        return sb.toString();
    }

    /** Append {@code start}'s text, descending the LAST-argument spine iteratively. */
    private static void appendTerm(StringBuilder sb, Term start) {
        Term t = start;
        int pendingClose = 0;
        while (true) {
            if (!(t instanceof CompoundTerm)) { sb.append(t); break; }
            CompoundTerm c = (CompoundTerm) t;
            int n = c.arguments.size();
            if (n == 0) { sb.append(c.functor); break; }
            // START_CHANGE: ISS-2025-0019 - ISO-compliant list formatting for './2'
            if (n == 2 && ".".equals(c.functor.getName())) { c.appendAsList(sb); break; }
            // END_CHANGE: ISS-2025-0019
            // START_CHANGE: ISS-2025-0091 - StringBuilder instead of a stream
            sb.append(c.functor.getName()).append('(');
            for (int i = 0; i < n - 1; i++) {
                if (i > 0) sb.append(", ");
                appendTerm(sb, c.arguments.get(i));
            }
            if (n > 1) sb.append(", ");
            // END_CHANGE: ISS-2025-0091
            pendingClose++;
            t = c.arguments.get(n - 1);
        }
        for (int i = 0; i < pendingClose; i++) sb.append(')');
        // END_CHANGE: ISS-2025-0428
    }
    
    // START_CHANGE: ISS-2025-0019 - Helper method for formatting lists in ISO-compliant way
    /**
     * Format this compound term as an ISO-compliant Prolog list [a,b,c]
     * Only called when this is a list structure (functor "." with 2 args)
     */
    private void appendAsList(StringBuilder sb) {
        // START_CHANGE: ISS-2025-0428 - ENG-09: append straight into the caller's StringBuilder
        // instead of building a List<String> of every element and joining it (a million-element
        // list allocated a million intermediate Strings plus the joined copy).
        sb.append('[');
        Term current = this;
        boolean firstElem = true;
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.arguments.size() != 2 || !".".equals(compound.functor.getName())) {
                break;                                        // not a proper list structure
            }
            if (!firstElem) sb.append(", ");
            firstElem = false;
            appendTerm(sb, compound.arguments.get(0));
            current = compound.arguments.get(1);
        }
        if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
            sb.append(']');                                   // proper list
        } else {
            sb.append('|');
            appendTerm(sb, current);                          // partial/improper list tail
            sb.append(']');
        }
        // END_CHANGE: ISS-2025-0428
    }
    // END_CHANGE: ISS-2025-0019

    @Override
    public Term copy() {
        // START_CHANGE: ISS-2025-0428 - ENG-09: build the last-argument spine iteratively, filling
        // each parent's last slot once its child copy exists.
        CompoundTerm src = this;
        CompoundTerm root = null, parent = null;
        while (true) {
            int n = src.arguments.size();
            List<Term> args = new ArrayList<>(n);
            for (int i = 0; i < n - 1; i++) args.add(src.arguments.get(i).copy());
            if (n > 0) args.add(src.arguments.get(n - 1));            // provisional; fixed below
            CompoundTerm dst = new CompoundTerm(src.functor, args);
            if (parent == null) root = dst; else parent.replaceLast(dst);
            if (n == 0) return root;
            Term last = src.arguments.get(n - 1);
            if (last.getClass() == CompoundTerm.class) { parent = dst; src = (CompoundTerm) last; continue; }
            dst.replaceLast(last.copy());
            return root;
        }
        // END_CHANGE: ISS-2025-0428
    }

    // START_CHANGE: ISS-2025-0428 - ENG-09: in-place last-slot patch used while a copy is still
    // under construction (never escapes before the spine is complete).
    private void replaceLast(Term value) {
        arguments.set(arguments.size() - 1, value);
        unmodifiableArguments = null;
    }
    // END_CHANGE: ISS-2025-0428
    
    // START_CHANGE: ISS-2025-0100 - Skip allocation when no arguments change
    // START_CHANGE: ISS-2025-0428 - ENG-09: iterative on the last argument. Phase 1 collects the
    // last-argument spine on the heap; phase 2 rebuilds it bottom-up, still returning {@code this}
    // (and sharing every unchanged sub-term) when nothing resolved.
    @Override
    public Term resolveBindings(Map<String, Term> bindings) {
        ArrayList<CompoundTerm> spine = new ArrayList<>();
        ArrayList<Boolean> viaVariable = new ArrayList<>();      // link to spine[k+1] crossed a var?
        java.util.HashSet<String> crossed = null;                // variables the spine walked through
        CompoundTerm cur = this;
        while (true) {
            spine.add(cur);
            int n = cur.arguments.size();
            if (n == 0) { viaVariable.add(Boolean.FALSE); break; }
            Term last = cur.arguments.get(n - 1);
            // Look THROUGH bound variables: a structure built by recursive clauses (f(f(f(...))),
            // or any list whose tail is a bound variable) links its spine by variables, so stopping
            // at the first Variable would put the recursion straight back in.
            boolean deref = false;
            while (last instanceof Variable) {
                String vn = ((Variable) last).getName();
                Term bound = bindings.get(vn);
                if (bound == null) break;                        // unbound: end of the chain
                if (crossed == null) crossed = new java.util.HashSet<>();
                if (!crossed.add(vn)) break;                     // circular chain: let the leaf path handle it
                last = bound;
                deref = true;
            }
            if (last.getClass() != CompoundTerm.class) { viaVariable.add(Boolean.FALSE); break; }
            viaVariable.add(deref);
            cur = (CompoundTerm) last;
        }
        Term below = null;                          // rebuilt child, or null when it is unchanged
        for (int k = spine.size() - 1; k >= 0; k--) {
            CompoundTerm node = spine.get(k);
            int n = node.arguments.size();
            boolean hasSpineChild = (k < spine.size() - 1);
            List<Term> out = null;                  // lazy: allocate only when something changes
            for (int i = 0; i < n; i++) {
                Term arg = node.arguments.get(i);
                Term resolved;
                if (hasSpineChild && i == n - 1) {
                    // The child is unchanged only when the link did NOT cross a bound variable:
                    // if it did, the argument itself resolves to the child (var -> its value).
                    if (below != null) {
                        resolved = below;
                    } else {
                        resolved = viaVariable.get(k) ? spine.get(k + 1) : arg;
                    }
                } else {
                    resolved = arg.resolveBindings(bindings);
                }
                if (resolved != arg && out == null) {
                    out = new ArrayList<>(n);
                    for (int j = 0; j < i; j++) out.add(node.arguments.get(j));
                }
                if (out != null) out.add(resolved);
            }
            below = (out == null) ? null : new CompoundTerm(node.functor, out);
        }
        return (below == null) ? this : below;      // nothing changed -> no allocation
    }
    // END_CHANGE: ISS-2025-0428
    // END_CHANGE: ISS-2025-0100
    
    // START_CHANGE: ISS-2025-0428 - ENG-09: equals and hashCode walk the last-argument spine
    // iteratively. ArrayList.equals / Objects.hash(functor, arguments) both recursed once per list
    // cell, so comparing or hashing a long list overflowed the Java stack.
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Term a = this, b = (Term) obj;
        while (true) {
            if (a == b) return true;
            if (a.getClass() != CompoundTerm.class || b.getClass() != CompoundTerm.class) {
                return a.equals(b);
            }
            CompoundTerm ca = (CompoundTerm) a, cb = (CompoundTerm) b;
            if (!ca.functor.equals(cb.functor)) return false;
            int n = ca.arguments.size();
            if (n != cb.arguments.size()) return false;
            for (int i = 0; i < n - 1; i++) {
                if (!ca.arguments.get(i).equals(cb.arguments.get(i))) return false;
            }
            if (n == 0) return true;
            a = ca.arguments.get(n - 1);
            b = cb.arguments.get(n - 1);
        }
    }

    /** Structure-derived hash, consistent with {@link #equals}: equal terms hash equally. The
     *  concrete VALUES differ from the previous {@code Objects.hash(functor, arguments)} — nothing
     *  may persist or assert them (hash order is not part of any contract). */
    @Override
    public int hashCode() {
        int h = 1;
        Term t = this;
        while (t.getClass() == CompoundTerm.class) {
            CompoundTerm c = (CompoundTerm) t;
            h = 31 * h + c.functor.hashCode();
            int n = c.arguments.size();
            h = 31 * h + n;
            for (int i = 0; i < n - 1; i++) h = 31 * h + c.arguments.get(i).hashCode();
            if (n == 0) return h;
            t = c.arguments.get(n - 1);
        }
        return 31 * h + t.hashCode();
    }
    // END_CHANGE: ISS-2025-0428
}
