// START_CHANGE: CR-2025-0009 - profile/0, noprofile/0, profile_data/1
package it.denzosoft.jprolog.builtin.debug;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.Profiler;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Basic profiling predicates:
 *   profile/0      - enable counting
 *   noprofile/0    - disable counting
 *   profile_data/1 - unify list of Name/Arity-Count pairs
 *   reset_profile/0 - clear counters
 */
public class Profile implements BuiltIn {

    public enum Mode { PROFILE, NOPROFILE, PROFILE_DATA, RESET_PROFILE }

    private final Mode mode;

    public Profile(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (mode) {
            case PROFILE:
                Profiler.enable();
                solutions.add(new HashMap<>(bindings));
                return true;
            case NOPROFILE:
                Profiler.disable();
                solutions.add(new HashMap<>(bindings));
                return true;
            case RESET_PROFILE:
                Profiler.reset();
                solutions.add(new HashMap<>(bindings));
                return true;
            case PROFILE_DATA: {
                if (query.getArguments() == null || query.getArguments().size() != 1) {
                    throw new PrologEvaluationException("profile_data/1 requires exactly 1 argument");
                }
                Term out = query.getArguments().get(0);
                List<Term> entries = new ArrayList<>();
                for (Map.Entry<String, Long> e : Profiler.snapshot().entrySet()) {
                    int slash = e.getKey().lastIndexOf('/');
                    if (slash <= 0) continue;
                    String name = e.getKey().substring(0, slash);
                    int arity;
                    try { arity = Integer.parseInt(e.getKey().substring(slash + 1)); }
                    catch (NumberFormatException ex) { continue; }
                    Term pi = new CompoundTerm(new Atom("/"),
                        Arrays.asList(new Atom(name), new Number((long) arity)));
                    Term pair = new CompoundTerm(new Atom("-"),
                        Arrays.asList(pi, new Number(e.getValue())));
                    entries.add(pair);
                }
                Term list = ListUtils.createList(entries);
                Map<String, Term> nb = new HashMap<>(bindings);
                if (out.unify(list, nb)) {
                    solutions.add(nb);
                    return true;
                }
                return false;
            }
        }
        return false;
    }
}
// END_CHANGE: CR-2025-0009
