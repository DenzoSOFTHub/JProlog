// START_CHANGE: ISS-2025-0238 - atom_to_term/3
package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.parser.Parser;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * atom_to_term(+Atom, ?Term, ?Bindings)
 * Parse Atom as a Prolog term; Bindings = list of Name=Var pairs for variables in source.
 */
public class AtomToTerm implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("atom_to_term/3 requires exactly 3 arguments");
        }
        Term atomArg = query.getArguments().get(0).resolveBindings(bindings);
        Term termArg = query.getArguments().get(1);
        Term bindingsArg = query.getArguments().get(2);

        if (!(atomArg instanceof Atom)) {
            throw new PrologEvaluationException("type_error(atom, " + atomArg + ")");
        }
        String src = ((Atom) atomArg).getName();
        try {
            Parser parser = new Parser();
            Term parsed = parser.parseTerm(src);
            if (parsed == null) return false;

            Map<String, Variable> namedVars = new LinkedHashMap<>();
            collectNamedVars(parsed, namedVars);

            Term bindingsList = buildBindingsList(namedVars);

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (termArg.unify(parsed, newBindings) && bindingsArg.unify(bindingsList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        // START_CHANGE: ISS-2025-0473 - a resource error must NEVER become a syntax error
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            throw pe;
        } catch (StackOverflowError so) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError("parser_nesting", "atom_to_term/3"));
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("syntax_error(" + e.getMessage() + ")");
        }
        // END_CHANGE: ISS-2025-0473
    }

    private void collectNamedVars(Term t, Map<String, Variable> out) {
        if (t instanceof Variable) {
            String n = ((Variable) t).getName();
            if (n != null && !n.startsWith("_")) {
                out.putIfAbsent(n, (Variable) t);
            }
        } else if (t instanceof CompoundTerm && t.getArguments() != null) {
            for (Term a : t.getArguments()) collectNamedVars(a, out);
        }
    }

    private Term buildBindingsList(Map<String, Variable> namedVars) {
        Term result = new Atom("[]");
        List<Map.Entry<String, Variable>> entries = new ArrayList<>(namedVars.entrySet());
        for (int i = entries.size() - 1; i >= 0; i--) {
            Map.Entry<String, Variable> e = entries.get(i);
            Term pair = new CompoundTerm(new Atom("="), Arrays.asList(new Atom(e.getKey()), e.getValue()));
            result = new CompoundTerm(new Atom("."), Arrays.asList(pair, result));
        }
        return result;
    }
}
// END_CHANGE: ISS-2025-0238
