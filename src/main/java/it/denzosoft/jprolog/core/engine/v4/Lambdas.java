package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

// START_CHANGE: ISS-2025-0455 - engine v4, design B.10: library(yall) lambdas in call/N.
/**
 * {@code library(yall)} lambda expressions, native in the v4 machine's {@code call/N}.
 *
 * <p>Supported forms (SWI semantics):
 * <ul>
 *   <li>{@code Params>>Body} — {@code Params} is a list of formal parameters,
 *       e.g. {@code maplist([X,Y]>>(Y is X*2), [1,2,3], L)};</li>
 *   <li>{@code Free/Params>>Body} — the variables of {@code Free} are <b>shared</b> with the
 *       caller instead of being renamed apart, e.g. {@code N/[X,Y]>>(Y is X*N)};</li>
 *   <li>{@code \X1^...^Xn^Body} and {@code Free/\X1^...^Xn^Body} — the {@code library(lambda)}
 *       spelling of the same thing.</li>
 * </ul>
 *
 * <p><b>Parsing note</b>: {@code >>} and {@code /} are both {@code yfx 400}, so
 * {@code N/[X,Y]>>Body} parses as {@code >>( /(N,[X,Y]), Body)} — the {@code Free/} part is inside
 * the {@code >>} term's first argument, which is exactly how yall reads it. No operator is
 * declared by this class.
 *
 * <p><b>Copy semantics</b>: the lambda is copied before <i>every</i> call, so the same lambda can
 * be applied to different arguments in one {@code maplist}; the variables named in {@code Free}
 * are unified back with the caller's after the copy, which is what makes them shared. A bound
 * variable is copied as its value, so a lambda closing over an already-bound outside variable
 * needs no {@code /} at all.
 */
final class Lambdas {

    private Lambdas() {}

    private static final Atom PAIR = new Atom("$lambda");
    static final Atom FAIL = new Atom("fail");

    /** Is {@code t} something {@link #expand} would handle? Cheap syntactic test. */
    static boolean isLambda(Term t) {
        Term c = Unify.deref(t);
        if (!(c instanceof CompoundTerm)) return false;
        CompoundTerm ct = (CompoundTerm) c;
        int n = ct.getArguments().size();
        if (">>".equals(ct.getName()) && n == 2) return true;
        if ("\\".equals(ct.getName()) && n == 1) return true;
        if ("/".equals(ct.getName()) && n == 2) {
            Term rhs = Unify.deref(ct.getArguments().get(1));
            return (rhs instanceof CompoundTerm) && "\\".equals(((CompoundTerm) rhs).getName())
                && ((CompoundTerm) rhs).getArguments().size() == 1;
        }
        return false;
    }

    /**
     * Apply the lambda {@code callee} to {@code extra}, binding its parameters.
     *
     * @return the goal to run, {@link #FAIL} when a parameter did not unify, or {@code null} when
     *         {@code callee} is not a lambda at all (the caller then applies {@code addArgs}).
     */
    static Term expand(Machine m, Term callee, List<Term> extra) {
        Term c = Unify.deref(callee);
        if (!(c instanceof CompoundTerm)) return null;
        CompoundTerm ct = (CompoundTerm) c;
        String f = ct.getName();
        int ar = ct.getArguments().size();

        Term free = null;
        Term shape;                                  // '>>'(Params, Body) or '\'(X1^..^Body)
        if (">>".equals(f) && ar == 2) {
            Term lhs = Unify.deref(ct.getArguments().get(0));
            if (lhs instanceof CompoundTerm && "/".equals(((CompoundTerm) lhs).getName())
                    && ((CompoundTerm) lhs).getArguments().size() == 2) {
                free = ((CompoundTerm) lhs).getArguments().get(0);
                shape = new CompoundTerm(new Atom(">>"),
                    Arrays.asList(((CompoundTerm) lhs).getArguments().get(1), ct.getArguments().get(1)));
            } else {
                shape = ct;
            }
        } else if ("\\".equals(f) && ar == 1) {
            shape = ct;
        } else if ("/".equals(f) && ar == 2) {
            Term rhs = Unify.deref(ct.getArguments().get(1));
            if (!(rhs instanceof CompoundTerm) || !"\\".equals(((CompoundTerm) rhs).getName())
                    || ((CompoundTerm) rhs).getArguments().size() != 1) {
                return null;
            }
            free = ct.getArguments().get(0);
            shape = rhs;
        } else {
            return null;
        }

        // Copy the lambda apart, then re-share the Free variables with the caller's.
        Term pack = new CompoundTerm(PAIR, Arrays.asList(free == null ? (Term) new Atom("[]") : free, shape));
        CompoundTerm copied = (CompoundTerm) m.copy(pack);
        if (free != null && !m.unify(copied.getArguments().get(0), free)) return FAIL;
        Term lambda = Unify.deref(copied.getArguments().get(1));

        List<Term> params = new ArrayList<Term>();
        Term body;
        CompoundTerm lc = (CompoundTerm) lambda;
        if (">>".equals(lc.getName())) {
            Term plist = Unify.deref(lc.getArguments().get(0));
            body = lc.getArguments().get(1);
            while (plist instanceof CompoundTerm && ".".equals(((CompoundTerm) plist).getName())
                    && ((CompoundTerm) plist).getArguments().size() == 2) {
                params.add(((CompoundTerm) plist).getArguments().get(0));
                plist = Unify.deref(((CompoundTerm) plist).getArguments().get(1));
            }
            if (plist instanceof Variable) {
                // A partial parameter list: yall closes it with the arguments still to come.
                if (!m.unify(plist, new Atom("[]"))) return FAIL;
            }
        } else {                                     // '\'(X1^X2^...^Body)
            Term cur = Unify.deref(lc.getArguments().get(0));
            while (params.size() < extra.size() && cur instanceof CompoundTerm
                    && "^".equals(((CompoundTerm) cur).getName())
                    && ((CompoundTerm) cur).getArguments().size() == 2) {
                params.add(((CompoundTerm) cur).getArguments().get(0));
                cur = Unify.deref(((CompoundTerm) cur).getArguments().get(1));
            }
            body = cur;
        }

        int k = Math.min(params.size(), extra.size());
        for (int i = 0; i < k; i++) {
            if (!m.unify(params.get(i), extra.get(i))) return FAIL;
        }
        if (extra.size() > k) {
            body = m.addArgs(Unify.deref(body), extra.subList(k, extra.size()));
        }
        return body;
    }
}
// END_CHANGE: ISS-2025-0455
