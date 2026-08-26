package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0476 - engine v4 wave W7 (design B.12 / B.17 decision 5, approved):
// how the console prints an answer. Limit L-11: the top level used to print canonical, unquoted
// terms through Term.toString() (`-(a b, 1)`, `/(foo,1)`, `,(p,q)`), leak internal variable names
// (`_R1_A`, `_G12`) and never show the constraints an answer still carries.
/**
 * Renders one answer the way a Prolog top level should: quoted operator notation through
 * {@link Writer}, fresh variables as {@code _A}, {@code _B}, ..., and the answer's residual goals
 * ({@code freeze/2}, {@code dif/2}, {@code when/2}, CLP(FD) {@code in/2}) after the bindings.
 *
 * <p>Shared by {@code PrologCLI} and the IDE's {@code RunPanel} so the two agree, and engine-neutral:
 * it takes the {@code Map<String,Term>} both engines produce plus the residual goals the caller got
 * from {@code Prolog.residualGoals(solution)}.
 */
public final class Answer {

    private Answer() { }

    /**
     * The lines of one answer: {@code Var = Value} for each binding, then one line per residual
     * goal. An empty list means the answer is a bare {@code true}.
     *
     * @param solution the answer's bindings (may be empty)
     * @param residual the residual goals, from {@code Prolog.residualGoals(solution)} (may be null)
     */
    public static List<String> lines(Map<String, Term> solution, List<Term> residual) {
        return lines(solution, residual, null);
    }

    // START_CHANGE: ISS-2025-0490 - render against the ENGINE's operator table, explicitly.
    // An answer is rendered by the caller AFTER `Prolog.solve` has returned, i.e. after the engine's
    // EngineState has been uninstalled from the thread — so `Writer`'s fallback to
    // `Ops.current().table()` picked up a default table and a user-declared operator printed in
    // canonical form (`Y = is_bigger(a,b)` instead of `Y = a is_bigger b`), even though `write/1`
    // inside the query printed it correctly. Callers pass `prolog.getOps().table()`.
    /**
     * @param ops the engine's operator table; null falls back to the table current on this thread
     */
    public static List<String> lines(Map<String, Term> solution, List<Term> residual,
                                     it.denzosoft.jprolog.core.operator.OperatorTable ops) {
        List<String> out = new ArrayList<String>();
        Writer.Options o = new Writer.Options();
        o.ops = ops;
        o.quoted = true;
        o.numbervars = true;
        o.portray = true;
        // A query variable that came back unbound keeps ITS OWN name, so `X = f(Y)` prints as
        // `X = f(Y)` rather than `X = f(_A), Y = _A` — and the redundant `Y = Y` line is dropped.
        // The seeding is keyed by the ENGINE's variable NAME, not by object identity: the legacy and
        // v2 engines hand back a renamed copy per binding, so `X = f(Y)` really does contain two
        // distinct Variable objects both called `Y`, and only the name relates them. On v4 a cell's
        // name is unique per cell, so the two views agree.
        Map<String, String> byName = new java.util.HashMap<String, String>();
        if (solution != null) {
            for (Map.Entry<String, Term> e : solution.entrySet()) {
                Term v = Unify.deref(e.getValue());
                if (v instanceof Variable && !e.getKey().startsWith("_")) {
                    // only a REAL query variable lends its name; an engine-internal key such as
                    // `_G17` (an anonymous `_` that came back in the map) must still print as _A
                    String n = ((Variable) v).getName();
                    if (n != null && !byName.containsKey(n)) byName.put(n, e.getKey());
                }
            }
        }
        IdentityHashMap<Variable, String> names = new IdentityHashMap<Variable, String>();
        o.variableNames = nameFreeVariables(solution, residual, names, byName);

        if (solution != null) {
            for (Map.Entry<String, Term> e : solution.entrySet()) {
                Term v = Unify.deref(e.getValue());
                if (v instanceof Variable && e.getKey().equals(names.get(v))) continue;
                if (e.getKey().startsWith("_")) continue;      // never report an internal binding
                // 699: the right argument of =/2 (700 xfx), so a conjunction prints as (p,q)
                out.add(e.getKey() + " = " + Writer.format(e.getValue(), o, 699));
            }
        }
        if (residual != null) {
            for (Term g : residual) out.add(Writer.format(g, o, 999));
        }
        return out;
    }

    /**
     * Assign {@code _A}, {@code _B}, ..., {@code _Z}, {@code _A1}, ... to every distinct free
     * variable of the answer, so the console never shows an engine-internal name.
     */
    public static IdentityHashMap<Variable, String> nameFreeVariables(Map<String, Term> solution,
                                                                     List<Term> residual) {
        return nameFreeVariables(solution, residual, new IdentityHashMap<Variable, String>(),
                                 new java.util.HashMap<String, String>());
    }

    /** As above, seeded with console names already chosen for some engine variable names. */
    public static IdentityHashMap<Variable, String> nameFreeVariables(Map<String, Term> solution,
                                                                     List<Term> residual,
                                                                     IdentityHashMap<Variable, String> names,
                                                                     Map<String, String> byName) {
        int[] next = {0};
        if (solution != null) {
            for (Term t : solution.values()) collect(t, names, byName, next);
        }
        if (residual != null) {
            for (Term t : residual) collect(t, names, byName, next);
        }
        return names;
    }

    /** The console name of the {@code n}-th fresh variable: {@code _A} .. {@code _Z}, {@code _A1}, ... */
    public static String freshName(int n) {
        char letter = (char) ('A' + (n % 26));
        int idx = n / 26;
        return (idx == 0) ? ("_" + letter) : ("_" + letter + idx);
    }

    private static void collect(Term t, IdentityHashMap<Variable, String> names,
                                Map<String, String> byName, int[] next) {
        // iterative, and cycle-safe: the v4 engine builds rational trees happily (design decision 2)
        java.util.ArrayDeque<Term> stack = new java.util.ArrayDeque<Term>();
        stack.push(t);
        int guard = 0;
        IdentityHashMap<Term, Boolean> seen = new IdentityHashMap<Term, Boolean>();
        while (!stack.isEmpty() && guard++ < 5000000) {
            Term cur = Unify.deref(stack.pop());
            if (cur instanceof Variable) {
                Variable v = (Variable) cur;
                if (names.containsKey(v)) continue;
                String n = v.getName();
                String console = (n == null) ? null : byName.get(n);
                if (console == null) {
                    console = freshName(next[0]++);
                    if (n != null) byName.put(n, console);
                }
                names.put(v, console);
                continue;
            }
            if (!(cur instanceof it.denzosoft.jprolog.core.terms.CompoundTerm)) continue;
            if (seen.put(cur, Boolean.TRUE) != null) continue;      // rational trees terminate
            List<Term> args = cur.getArguments();
            if (args == null) continue;
            for (int i = args.size() - 1; i >= 0; i--) stack.push(args.get(i));
        }
    }
    // END_CHANGE: ISS-2025-0490
}
