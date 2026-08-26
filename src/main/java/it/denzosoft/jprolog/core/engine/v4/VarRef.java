package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.terms.Term;

import java.util.Map;

// START_CHANGE: ISS-2025-0439 - engine v4, design B.2 (clause skeletons).
/**
 * A numbered variable placeholder inside a compiled {@link Clause} skeleton.
 *
 * <p>Consulting compiles every clause ONCE into a skeleton in which each distinct variable of the
 * clause is replaced by {@code VarRef(k)}, {@code k = 0..nvars-1}. Activating the clause allocates
 * one {@code Term[nvars]} frame; the head is unified directly against the skeleton
 * ({@link Clause#unifyHead}) and body goals are instantiated only when they are pushed. That
 * removes the per-activation {@code HashMap<String,Variable>}, the {@code "_R<id>_<name>"} strings
 * and most of the term copying the v2 engine did on every call (design limit L-12).
 *
 * <p><b>Invariant</b>: a VarRef never escapes the {@code core.engine.v4} package. It exists only in
 * skeletons; {@link Clause#instantiate} replaces it with a real {@link it.denzosoft.jprolog.core.terms.Variable}
 * cell (or with whatever the head unification aliased that slot to) before the term reaches the
 * machine, the built-ins or the user.
 */
final class VarRef extends Term {

    final int index;

    VarRef(int index) { this.index = index; }

    @Override
    public boolean unify(Term term, Map<String, Term> substitution) {
        throw new IllegalStateException("VarRef escaped a clause skeleton: _S" + index);
    }

    @Override public boolean isGround() { return false; }
    @Override public Term copy() { return this; }
    @Override public String toString() { return "_S" + index; }
    @Override public boolean equals(Object obj) { return (obj instanceof VarRef) && ((VarRef) obj).index == index; }
    @Override public int hashCode() { return index; }
}
// END_CHANGE: ISS-2025-0439
