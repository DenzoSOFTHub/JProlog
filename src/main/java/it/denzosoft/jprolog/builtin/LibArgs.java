package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.Map;

// START_CHANGE: ISS-2025-0681 - wave Q1.1: the argument checks of the bridged EXTENDED LIBRARIES
/**
 * Argument validation for the legacy-registry ({@code BuiltIn}) extended libraries, raising the ISO
 * formal with SWI's context: {@code error(Formal, context(Name/Arity, Message))} (LIM-038). The
 * arity is taken from the goal itself, so one helper serves every arity of a predicate.
 *
 * <p>Before wave Q1 each library carried its own copy of {@code resolveAtom} throwing a
 * {@code PrologEvaluationException} whose ball was a bare message atom, which
 * {@code catch(G, error(type_error(_, _), _), R)} could not match.
 */
public final class LibArgs {

    private LibArgs() {}

    /**
     * {@code existence_error(procedure, Name/Arity)} for a goal whose arity the built-in does not
     * implement. Since ISS-2025-0685 the registry never dispatches such a goal, so this is only
     * reachable through a direct Java call; it replaces the old "requires N arguments" atoms.
     */
    public static it.denzosoft.jprolog.core.exceptions.PrologException unknownArity(Term goal) {
        String name = goal instanceof Atom ? ((Atom) goal).getName()
            : ((it.denzosoft.jprolog.core.terms.CompoundTerm) goal).getName();
        int n = arity(goal);
        return Errors.existence("procedure", Errors.pi(name, n), name, n, null);
    }

    /**
     * The arity to report in the context of {@code name} when the goal itself is not at hand: the
     * lowest arity the registry declares for it (most library names have exactly one).
     */
    public static int nameArity(String name) {
        java.util.Set<Integer> s = it.denzosoft.jprolog.core.engine.BuiltInRegistry.staticArities(name);
        return s.isEmpty() ? 0 : s.iterator().next();
    }

    /**
     * The error for an argument {@code t} that is not of {@code type}: instantiation_error when it
     * is unbound, type_error(Type, T) otherwise.
     */
    public static it.denzosoft.jprolog.core.exceptions.PrologException notA(String type, Term t, String name,
                                                                           int arity, String what) {
        boolean sentence = what.contains(" must ");               // an old message, kept as is
        if (t instanceof Variable) return Errors.instantiation(name, arity, sentence ? what : what + " must be bound");
        return Errors.type(type, t, name, arity, sentence ? what : what + " must be of type " + type);
    }

    /** {@link #unknownArity(Term)} when only the name and the argument count are at hand. */
    public static it.denzosoft.jprolog.core.exceptions.PrologException unknownArity(String name, int arity) {
        return Errors.existence("procedure", Errors.pi(name, arity), name, arity, null);
    }

    /** The goal's arity (0 for an atom goal). */
    public static int arity(Term goal) {
        return goal.getArguments() == null ? 0 : goal.getArguments().size();
    }

    /** Argument {@code i} (0-based), dereferenced through the eager bindings map. */
    public static Term arg(Term goal, int i, Map<String, Term> bindings) {
        Term t = goal.getArguments().get(i);
        return bindings == null ? t : t.resolveBindings(bindings);
    }

    /** Argument {@code i}, which must be bound (instantiation_error otherwise). */
    public static Term bound(Term goal, int i, Map<String, Term> bindings, String name, String what) {
        Term t = arg(goal, i, bindings);
        if (t instanceof Variable) throw Errors.instantiation(name, arity(goal), what + " must be bound");
        return t;
    }

    /** Argument {@code i} as text: an atom or a string. */
    public static String text(Term goal, int i, Map<String, Term> bindings, String name, String what) {
        Term t = bound(goal, i, bindings, name, what);
        return text(t, name, arity(goal), what);
    }

    /** {@code t} (already dereferenced) as text: an atom or a string. */
    public static String text(Term t, String name, int arity, String what) {
        if (t instanceof Variable) throw Errors.instantiation(name, arity, what + " must be bound");
        if (t instanceof PrologString) return ((PrologString) t).getStringValue();
        if (t instanceof Atom) return ((Atom) t).getName();
        throw Errors.type("atom", t, name, arity, what + " must be an atom or a string");
    }

    /** Argument {@code i} as an atom's name. */
    public static String atom(Term goal, int i, Map<String, Term> bindings, String name, String what) {
        Term t = bound(goal, i, bindings, name, what);
        if (!(t instanceof Atom)) throw Errors.type("atom", t, name, arity(goal), what + " must be an atom");
        return ((Atom) t).getName();
    }

    /** Argument {@code i} as a number. */
    public static Number number(Term goal, int i, Map<String, Term> bindings, String name, String what) {
        Term t = bound(goal, i, bindings, name, what);
        if (!(t instanceof Number)) throw Errors.type("number", t, name, arity(goal), what + " must be a number");
        return (Number) t;
    }

    /** Argument {@code i} as an integer. */
    public static long integer(Term goal, int i, Map<String, Term> bindings, String name, String what) {
        Term t = bound(goal, i, bindings, name, what);
        if (!(t instanceof Number) || !((Number) t).isInteger()) {
            throw Errors.type("integer", t, name, arity(goal), what + " must be an integer");
        }
        return ((Number) t).getValue().longValue();
    }
}
// END_CHANGE: ISS-2025-0681
