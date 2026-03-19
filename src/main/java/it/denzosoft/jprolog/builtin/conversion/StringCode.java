package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * string_code(+Index, +String, -Code) - Get character code at 1-based Index.
 */
public class StringCode implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term indexTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term stringTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term codeTerm = query.getArguments().get(2);

        if (!(indexTerm instanceof Number)) return false;

        String str;
        if (stringTerm instanceof Atom) {
            str = ((Atom) stringTerm).getName();
        } else if (stringTerm instanceof PrologString) {
            str = ((PrologString) stringTerm).getStringValue();
        } else {
            return false;
        }

        int index = ((Number) indexTerm).getValue().intValue();
        if (index < 1 || index > str.length()) return false;

        int code = str.charAt(index - 1);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (codeTerm.unify(new Number(code), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
