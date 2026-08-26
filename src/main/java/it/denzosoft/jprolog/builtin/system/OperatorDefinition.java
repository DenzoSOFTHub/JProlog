package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.engine.v4.Ops;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Implementation of op/3 predicate for defining operators.
 * 
 * op(+Precedence, +Type, +Name)
 * 
 * Defines an operator with given precedence and associativity type.
 */
public class OperatorDefinition implements BuiltIn {
    
    public enum OperatorType {
        OP("op/3"),
        CURRENT_OP("current_op/3");
        
        private final String name;
        
        OperatorType(String name) {
            this.name = name;
        }
        
        public String getName() {
            return name;
        }
    }
    
    // START_CHANGE: ISS-2025-0474 - engine v4 wave W7 (design B.12, LIM-034): op/3 and
    // current_op/3 no longer own any state. The three process-global stores that lived here — the
    // OPERATORS map, the OP_MODULE map and the sharedOperatorTable the parser read — are replaced
    // by ONE per-engine store, {@link Ops}, reached through the engine current on the calling
    // thread (the same facade pattern PrologFlags has used since ISS-2025-0437). That is what makes
    // current_op/3 see an operator declared by a consulted `:- op/3` directive, keeps two Prolog
    // instances from sharing operators, and keeps a module's operators local to that module.
    private static Ops ops() { return Ops.current(); }

    /** The module {@code op/3} attributes its definitions to. */
    public static void setCurrentModuleContext(String mod) { ops().setModuleContext(mod); }

    /** The module {@code current_op/3} reports operators for. */
    public static String getCurrentModuleContext() { return ops().moduleContext(); }

    /** The operator table the parser of the current engine reads (module-aware). */
    public static OperatorTable getSharedOperatorTable() { return ops().table(); }

    /**
     * Historical hook: the parser table is owned by the engine now, so there is nothing to install.
     * Kept so embedder code that called it still compiles.
     *
     * @deprecated the operator table belongs to the {@code Prolog} instance ({@code getOperatorTable()}).
     */
    @Deprecated
    public static void setSharedOperatorTable(OperatorTable table) { /* per-engine now */ }
    // END_CHANGE: ISS-2025-0474

    private final OperatorType type;

    public OperatorDefinition(OperatorType type) {
        this.type = type;
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (type) {
            case OP:
                return handleOp(query, bindings, solutions);
            case CURRENT_OP:
                return handleCurrentOp(query, bindings, solutions);
            default:
                return false;
        }
    }
    
    private boolean handleOp(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(query instanceof CompoundTerm)) {
            return false;
        }
        
        CompoundTerm compound = (CompoundTerm) query;
        if (compound.getArguments().size() != 3) {
            throw new PrologEvaluationException("op/3 requires exactly 3 arguments.");
        }
        
        Term precedenceTerm = compound.getArguments().get(0);
        Term typeTerm = compound.getArguments().get(1);  
        Term nameTerm = compound.getArguments().get(2);
        
        // Resolve variables
        precedenceTerm = resolveVariable(precedenceTerm, bindings);
        typeTerm = resolveVariable(typeTerm, bindings);
        nameTerm = resolveVariable(nameTerm, bindings);
        
        if (!(precedenceTerm instanceof Number)) {
            throw new PrologEvaluationException("op/3: First argument must be an integer (precedence).");
        }
        // START_CHANGE: ISS-2025-0278 - op/3 precedence must be an integer; a float (e.g. op(700.5,...))
        // must raise type_error(integer, Prec) rather than being silently rounded.
        if (!((Number) precedenceTerm).isInteger()) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", precedenceTerm, "op/3"));
        }
        // END_CHANGE: ISS-2025-0278

        if (!(typeTerm instanceof Atom)) {
            throw new PrologEvaluationException("op/3: Second argument must be an atom (type).");
        }
        
        // START_CHANGE: ISS-2025-0283 - op/3 accepts a single atom OR a proper list of atoms (ISO
        // op(P, T, [n1,n2,...])); each name is defined/removed in turn.
        java.util.List<String> names = extractOpNames(nameTerm);
        if (names == null || names.isEmpty()) {
            throw new PrologEvaluationException("op/3: Third argument must be an atom or a list of atoms (name).");
        }
        // END_CHANGE: ISS-2025-0283

        int precedence = (int) Math.round(((Number) precedenceTerm).getValue());
        String operatorType = ((Atom) typeTerm).getName();

        // START_CHANGE: ISS-2025-0085 - Support precedence 0 for operator removal
        // Validate precedence (0-1200, where 0 means remove)
        if (precedence < 0 || precedence > 1200) {
            throw new PrologEvaluationException("op/3: Precedence must be between 0 and 1200.");
        }

        // Validate operator type
        if (!isValidOperatorType(operatorType)) {
            throw new PrologEvaluationException("op/3: Invalid operator type: " + operatorType);
        }

        // START_CHANGE: ISS-2025-0474 - one store, and op/3 under a choice point is undone on
        // backtracking (R1) through the undo action the store hands back.
        for (String name : names) {
            final Runnable undo = ops().define(precedence, operatorType, name);
            it.denzosoft.jprolog.core.engine.v4.Undo.record(undo);
        }
        // END_CHANGE: ISS-2025-0474

        // END_CHANGE: ISS-2025-0085

        // Success - operator defined/removed
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    
    private boolean handleCurrentOp(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(query instanceof CompoundTerm)) {
            return false;
        }
        
        CompoundTerm compound = (CompoundTerm) query;
        if (compound.getArguments().size() != 3) {
            throw new PrologEvaluationException("current_op/3 requires exactly 3 arguments.");
        }
        
        Term precedenceTerm = compound.getArguments().get(0);
        Term typeTerm = compound.getArguments().get(1);
        Term nameTerm = compound.getArguments().get(2);
        
        boolean foundSolution = false;
        
        // START_CHANGE: ISS-2025-0474 - enumerate the engine's own store: the standard operators,
        // everything op/3 defined, everything a consulted `:- op/3` directive declared, and the
        // operators local to the module currently in context.
        for (Ops.Def def : ops().visible()) {
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (!unifyTerm(precedenceTerm, new Number((long) def.precedence)   /* ISS-2025-0424 */, newBindings)) continue;
            if (!unifyTerm(typeTerm, new Atom(def.type), newBindings)) continue;
            if (!unifyTerm(nameTerm, new Atom(def.name), newBindings)) continue;
            solutions.add(newBindings);
            foundSolution = true;
        }

        return foundSolution;
    }
    
    private boolean isValidOperatorType(String type) {
        return type.equals("fx") || type.equals("fy") ||
               type.equals("xfx") || type.equals("xfy") || type.equals("yfx") ||
               type.equals("yf") || type.equals("xf");
    }

    // START_CHANGE: ISS-2025-0283 - extract op/3 name(s): a single atom, or a proper list of atoms.
    private static java.util.List<String> extractOpNames(Term nameTerm) {
        java.util.List<String> names = new java.util.ArrayList<>();
        if (nameTerm instanceof Atom && !"[]".equals(((Atom) nameTerm).getName())) {
            names.add(((Atom) nameTerm).getName());
            return names;
        }
        Term cur = nameTerm;
        while (cur instanceof it.denzosoft.jprolog.core.terms.CompoundTerm) {
            it.denzosoft.jprolog.core.terms.CompoundTerm c = (it.denzosoft.jprolog.core.terms.CompoundTerm) cur;
            if (!".".equals(c.getName()) || c.getArguments().size() != 2) return null;
            Term head = c.getArguments().get(0);
            if (!(head instanceof Atom)) return null;
            names.add(((Atom) head).getName());
            cur = c.getArguments().get(1);
        }
        if (cur instanceof Atom && "[]".equals(((Atom) cur).getName())) return names;
        return null;
    }
    // END_CHANGE: ISS-2025-0283

    // START_CHANGE: ISS-2025-0474 - the lookup facade now reads the per-engine store.
    /**
     * Operator information for a name: infix first, then prefix, then postfix.
     */
    public static OperatorInfo getOperator(String name) { return info(ops().any(name)); }

    /** The prefix operator for a name, or null. */
    public static OperatorInfo getPrefixOperator(String name) { return info(ops().prefix(name)); }

    /** The infix operator for a name, or null. */
    public static OperatorInfo getInfixOperator(String name) { return info(ops().infix(name)); }

    /** The postfix operator for a name, or null. */
    public static OperatorInfo getPostfixOperator(String name) { return info(ops().postfix(name)); }

    /** True when a name is an operator of any class in the current module. */
    public static boolean isOperatorDefined(String name) { return ops().isDefined(name); }

    /** Every visible operator, keyed by the composite key {@code name:typeClass}. */
    public static Map<String, OperatorInfo> getAllOperators() {
        Map<String, OperatorInfo> out = new HashMap<>();
        for (Map.Entry<String, Ops.Def> e : ops().all().entrySet()) out.put(e.getKey(), info(e.getValue()));
        return out;
    }

    private static OperatorInfo info(Ops.Def d) {
        return (d == null) ? null : new OperatorInfo(d.precedence, d.type, d.name);
    }
    
    private Term resolveVariable(Term term, Map<String, Term> bindings) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Variable) {
            it.denzosoft.jprolog.core.terms.Variable var = (it.denzosoft.jprolog.core.terms.Variable) term;
            Term value = bindings.get(var.getName());
            if (value != null) {
                return resolveVariable(value, bindings);
            }
        }
        return term;
    }
    
    private boolean unifyTerm(Term term1, Term term2, Map<String, Term> bindings) {
        // Simple unification - resolve variables first
        term1 = resolveVariable(term1, bindings);
        term2 = resolveVariable(term2, bindings);
        
        if (term1 instanceof it.denzosoft.jprolog.core.terms.Variable) {
            it.denzosoft.jprolog.core.terms.Variable var = (it.denzosoft.jprolog.core.terms.Variable) term1;
            bindings.put(var.getName(), term2);
            return true;
        }
        
        if (term2 instanceof it.denzosoft.jprolog.core.terms.Variable) {
            it.denzosoft.jprolog.core.terms.Variable var = (it.denzosoft.jprolog.core.terms.Variable) term2;
            bindings.put(var.getName(), term1);
            return true;
        }
        
        return term1.equals(term2);
    }
    
    /**
     * Information about a defined operator.
     */
    public static class OperatorInfo {
        public final int precedence;
        public final String type;  // fx, fy, xfx, xfy, yfx, yf, xf
        public final String name;
        
        public OperatorInfo(int precedence, String type, String name) {
            this.precedence = precedence;
            this.type = type;
            this.name = name;
        }
        
        @Override
        public String toString() {
            return "OperatorInfo{" +
                    "precedence=" + precedence +
                    ", type='" + type + '\'' +
                    ", name='" + name + '\'' +
                    '}';
        }
    }
}