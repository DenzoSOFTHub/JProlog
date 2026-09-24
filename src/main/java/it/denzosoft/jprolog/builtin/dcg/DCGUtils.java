package it.denzosoft.jprolog.builtin.dcg;

import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

/**
 * Utility predicates for enhanced DCG support per ISO/IEC DTS 13211-3
 */
public class DCGUtils {
    
    /**
     * call_dcg/3 - Call DCG rule with non-list terms
     * call_dcg(DCGBody, InputTerm, OutputTerm)
     */
    // START_CHANGE: ISS-2025-0255 - call_dcg/3 was a stub that ignored the body and merely
    // unified Input with Output. Now it expands the DCG body into a goal that threads the
    // difference list Input -> Output, solves it via the engine, and propagates all solutions.
    public static class CallDCG implements BuiltInWithContext {
        @Override
        public boolean executeWithContext(SolverContext solver, Term query,
                                          Map<String, Term> bindings,
                                          List<Map<String, Term>> solutions) {
            if (query.getArguments().size() != 3) {
                throw LibArgs.unknownArity(query);   // ISS-2025-0695
            }

            Term dcgBody = query.getArguments().get(0).resolveBindings(bindings);
            Term inputTerm = query.getArguments().get(1);
            Term outputTerm = query.getArguments().get(2);

            // Expand the DCG body threading fresh S0 -> S, then bind S0=Input and S=Output.
            Variable s0 = new Variable("_CallDCG_S0");
            Variable s = new Variable("_CallDCG_S");
            // ISS-2025-0670: the v2 translator (the legacy DCGTranslateRule body translation is
            // deleted; it turned \+ into \+(b,S0,S1) and ! into !(S0,S1))
            Term bodyGoal = new it.denzosoft.jprolog.core.dcg.v2.DCGTranslator("_CallDCG_V").body(dcgBody, s0, s);

            Term goal = new CompoundTerm(new Atom(","), Arrays.asList(
                new CompoundTerm(new Atom("="), Arrays.asList(s0, inputTerm)),
                new CompoundTerm(new Atom(","), Arrays.asList(
                    bodyGoal,
                    new CompoundTerm(new Atom("="), Arrays.asList(s, outputTerm))))));

            List<Map<String, Term>> goalSolutions = new ArrayList<>();
            boolean ok = solver.solveMeta(goal, new HashMap<>(bindings), goalSolutions);   // ISS-2025-0485
            if (ok) {
                solutions.addAll(goalSolutions);
                return true;
            }
            return false;
        }

        @Override
        public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
            throw new UnsupportedOperationException("call_dcg/3 requires context");
        }
    }
    // END_CHANGE: ISS-2025-0255
    
    // START_CHANGE: ISS-2025-0670 - DCGTranslateRule deleted: dcg_translate_rule/2 is native
    // (core.engine.v4.NativeExpand, on the v2 DCG translator) and the legacy class answered
    // dcg_translate_rule/4 only with a message atom. END_CHANGE: ISS-2025-0670

    /**
     * dcg_body//2 - Create DCG body with specific input/output
     * This is a higher-order DCG predicate
     */
    public static class DCGBody implements BuiltIn {
        @Override
        public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
            if (query.getArguments().size() != 4) { // DCG predicates get extra args
                throw LibArgs.unknownArity(query);   // ISS-2025-0695
            }
            
            // This is a meta-DCG predicate that would need special handling
            // For now, we'll provide a basic implementation
            solutions.add(new HashMap<>(bindings));
            return true;
        }
    }
}