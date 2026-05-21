// START_CHANGE: v2.9.7 - pairs_keys/2, pairs_values/2, pairs_keys_values/3 (SWI library)
package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Pairs utilities — operate on lists of Key-Value pairs.
 *
 *   pairs_keys(+Pairs, -Keys)
 *   pairs_values(+Pairs, -Values)
 *   pairs_keys_values(?Pairs, ?Keys, ?Values)
 */
public class Pairs implements BuiltIn {

    public enum Mode { KEYS, VALUES, KEYS_VALUES }

    private final Mode mode;

    public Pairs(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (mode) {
            case KEYS: return doKeys(query, bindings, solutions);
            case VALUES: return doValues(query, bindings, solutions);
            case KEYS_VALUES: return doKeysValues(query, bindings, solutions);
        }
        return false;
    }

    private boolean doKeys(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException("pairs_keys/2 requires exactly 2 arguments");
        }
        Term pairsT = query.getArguments().get(0).resolveBindings(bindings);
        Term keysT = query.getArguments().get(1);
        List<Term> pairs = ListUtils.extractElements(pairsT);
        if (pairs == null) return false;
        List<Term> keys = new ArrayList<>();
        for (Term p : pairs) {
            if (p instanceof CompoundTerm && "-".equals(((CompoundTerm) p).getName())
                && p.getArguments().size() == 2) {
                keys.add(p.getArguments().get(0));
            } else {
                return false;
            }
        }
        Map<String, Term> nb = new HashMap<>(bindings);
        if (keysT.unify(ListUtils.createList(keys), nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private boolean doValues(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException("pairs_values/2 requires exactly 2 arguments");
        }
        Term pairsT = query.getArguments().get(0).resolveBindings(bindings);
        Term valsT = query.getArguments().get(1);
        List<Term> pairs = ListUtils.extractElements(pairsT);
        if (pairs == null) return false;
        List<Term> values = new ArrayList<>();
        for (Term p : pairs) {
            if (p instanceof CompoundTerm && "-".equals(((CompoundTerm) p).getName())
                && p.getArguments().size() == 2) {
                values.add(p.getArguments().get(1));
            } else {
                return false;
            }
        }
        Map<String, Term> nb = new HashMap<>(bindings);
        if (valsT.unify(ListUtils.createList(values), nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private boolean doKeysValues(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 3) {
            throw new PrologEvaluationException("pairs_keys_values/3 requires exactly 3 arguments");
        }
        Term pairsT = query.getArguments().get(0).resolveBindings(bindings);
        Term keysT = query.getArguments().get(1).resolveBindings(bindings);
        Term valsT = query.getArguments().get(2).resolveBindings(bindings);

        // Forward mode: pairs → keys + values
        if (pairsT.isGround()) {
            List<Term> pairs = ListUtils.extractElements(pairsT);
            if (pairs == null) return false;
            List<Term> keys = new ArrayList<>();
            List<Term> values = new ArrayList<>();
            for (Term p : pairs) {
                if (p instanceof CompoundTerm && "-".equals(((CompoundTerm) p).getName())
                    && p.getArguments().size() == 2) {
                    keys.add(p.getArguments().get(0));
                    values.add(p.getArguments().get(1));
                } else return false;
            }
            Map<String, Term> nb = new HashMap<>(bindings);
            if (query.getArguments().get(1).unify(ListUtils.createList(keys), nb)
                && query.getArguments().get(2).unify(ListUtils.createList(values), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        }

        // Reverse mode: keys + values → pairs
        List<Term> keys = ListUtils.extractElements(keysT);
        List<Term> values = ListUtils.extractElements(valsT);
        if (keys == null || values == null || keys.size() != values.size()) return false;
        List<Term> pairs = new ArrayList<>();
        for (int i = 0; i < keys.size(); i++) {
            pairs.add(new CompoundTerm(new Atom("-"), Arrays.asList(keys.get(i), values.get(i))));
        }
        Map<String, Term> nb = new HashMap<>(bindings);
        if (query.getArguments().get(0).unify(ListUtils.createList(pairs), nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }
}
// END_CHANGE: v2.9.7
