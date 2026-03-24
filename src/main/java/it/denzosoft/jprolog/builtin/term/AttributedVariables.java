// START_CHANGE: LIM-002 - Attributed variables built-in predicates
package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implements attributed variable predicates:
 * - put_attr(Var, Module, Value): Set an attribute on a variable
 * - get_attr(Var, Module, Value): Get an attribute from a variable
 * - del_attr(Var, Module): Remove an attribute from a variable
 * - attvar(Var): Check if a variable has attributes
 */
public class AttributedVariables implements BuiltInWithContext {

    public enum Mode {
        PUT_ATTR,   // put_attr/3
        GET_ATTR,   // get_attr/3
        DEL_ATTR,   // del_attr/2
        ATTVAR      // attvar/1
    }

    private final Mode mode;

    public AttributedVariables(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args == null) return false;

        switch (mode) {
            case PUT_ATTR:
                return executePutAttr(args, bindings, solutions);
            case GET_ATTR:
                return executeGetAttr(args, bindings, solutions);
            case DEL_ATTR:
                return executeDelAttr(args, bindings, solutions);
            case ATTVAR:
                return executeAttvar(args, bindings, solutions);
            default:
                return false;
        }
    }

    /**
     * put_attr(Var, Module, Value) - Set attribute on variable.
     * Var must be an unbound variable, Module must be an atom.
     */
    private boolean executePutAttr(List<Term> args, Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) {
        if (args.size() != 3) return false;

        Term varTerm = resolveToVariable(args.get(0), bindings);
        // START_CHANGE: ISS-2025-0182 - Built-in predicate bug fixes
        // Throw type_error(variable, Term) when put_attr is called on a non-variable
        if (!(varTerm instanceof Variable)) {
            throw new PrologException(ISOErrorTerms.typeError("variable", varTerm, "put_attr/3"));
        }
        // END_CHANGE: ISS-2025-0182
        Variable var = (Variable) varTerm;

        Term moduleTerm = args.get(1).resolveBindings(bindings);
        if (!(moduleTerm instanceof Atom)) return false;
        String module = ((Atom) moduleTerm).getName();

        Term value = args.get(2).resolveBindings(bindings);

        var.putAttribute(module, value);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    /**
     * get_attr(Var, Module, Value) - Get attribute from variable.
     * Var must be an attributed variable, Module must be an atom.
     * Unifies Value with the stored attribute.
     */
    private boolean executeGetAttr(List<Term> args, Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) {
        if (args.size() != 3) return false;

        Term varTerm = resolveToVariable(args.get(0), bindings);
        if (!(varTerm instanceof Variable)) return false;
        Variable var = (Variable) varTerm;

        if (!var.hasAttributes()) return false;

        Term moduleTerm = args.get(1).resolveBindings(bindings);
        if (!(moduleTerm instanceof Atom)) return false;
        String module = ((Atom) moduleTerm).getName();

        Term attrValue = var.getAttribute(module);
        if (attrValue == null) return false;

        Term valueTerm = args.get(2);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (valueTerm.unify(attrValue, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    /**
     * del_attr(Var, Module) - Remove attribute from variable.
     */
    private boolean executeDelAttr(List<Term> args, Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) {
        if (args.size() != 2) return false;

        Term varTerm = resolveToVariable(args.get(0), bindings);
        if (!(varTerm instanceof Variable)) return false;
        Variable var = (Variable) varTerm;

        Term moduleTerm = args.get(1).resolveBindings(bindings);
        if (!(moduleTerm instanceof Atom)) return false;
        String module = ((Atom) moduleTerm).getName();

        var.removeAttribute(module);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    /**
     * attvar(Var) - Check if Var is an attributed variable.
     * Succeeds only if Var is an unbound variable with at least one attribute.
     */
    private boolean executeAttvar(List<Term> args, Map<String, Term> bindings,
                                   List<Map<String, Term>> solutions) {
        if (args.size() != 1) return false;

        Term varTerm = resolveToVariable(args.get(0), bindings);
        if (!(varTerm instanceof Variable)) return false;
        Variable var = (Variable) varTerm;

        if (var.hasAttributes()) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        return false;
    }

    /**
     * Resolve a term through bindings, stopping at an unbound variable.
     * Returns the Variable if unbound, or the bound value otherwise.
     */
    private Term resolveToVariable(Term term, Map<String, Term> bindings) {
        Term current = term;
        while (current instanceof Variable) {
            String varName = ((Variable) current).getName();
            Term bound = bindings.get(varName);
            if (bound == null) {
                return current; // Unbound variable
            }
            current = bound;
        }
        return current; // Non-variable (bound)
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException(
            "Context-dependent built-in 'attributed variables' must be invoked with context");
    }
}
// END_CHANGE: LIM-002
