package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.dcg.v2.DCGTranslator;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.Arrays;

// START_CHANGE: ISS-2025-0571 - 4.5 wave P3.11: dcg_translate_rule/2 and expand_term/2 on the
// clean-room v2 DCG translator (the legacy builtin/dcg/DCGUtils produced \+(b,S0,S1), !(S0,S1),
// a wrong push-back and disconnected variables), plus the term_expansion/2 hook.
/**
 * {@code dcg_translate_rule(+Rule, -Clause)} translates a {@code -->} rule exactly as consult
 * does; {@code expand_term(+Term, -Expanded)} first asks a user {@code term_expansion/2} (when one
 * is defined), then translates a grammar rule, and otherwise answers the term itself.
 */
final class NativeExpand {

    private NativeExpand() { }

    static void register(BuiltinTable t) {
        t.register("dcg_translate_rule", 2, new DcgTranslateRuleB());
        t.register("expand_term", 2, new ExpandTermB());
    }

    private static Term translate(Machine m, Term rule, String ctx) {
        Term r = m.resolve(rule);
        if (r instanceof Variable) throw Errors.instantiation(ctx);
        if (!DCGTranslator.isDCGRule(r)) return null;
        return new DCGTranslator().translate((CompoundTerm) r);
    }

    private static final class DcgTranslateRuleB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term clause = translate(m, args[0], "dcg_translate_rule/2");
            if (clause == null) {
                throw Errors.type("dcg_rule", m.resolve(args[0]), "dcg_translate_rule/2");
            }
            return m.unify(args[1], clause) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class ExpandTermB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term t = m.deref(args[0]);
            if (t instanceof Variable) return m.unify(args[1], t) ? Outcome.SUCCESS : Outcome.FAILURE;
            Prolog p = m.engine().prolog();
            if (p != null && !p.getEngineContext().getKnowledgeBase().getRulesForPredicate("term_expansion", 2).isEmpty()) {
                Variable out = new Variable();
                Term goal = new CompoundTerm(new Atom("term_expansion"), Arrays.asList(t, (Term) out));
                if (m.runOnce(goal)) {
                    return m.unify(args[1], out) ? Outcome.SUCCESS : Outcome.FAILURE;
                }
            }
            Term clause = translate(m, t, "expand_term/2");
            return m.unify(args[1], clause != null ? clause : t) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0571
