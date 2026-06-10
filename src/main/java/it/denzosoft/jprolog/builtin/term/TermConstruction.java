package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class TermConstruction implements BuiltIn {
    public enum OperationType {
        FUNCTOR("functor/3"),
        ARG("arg/3"),
        UNIV("=../2"),
        COPY_TERM("copy_term/2");
        
        private final String name;
        
        OperationType(String name) {
            this.name = name;
        }
        
        public String getName() {
            return name;
        }
    }
    
    private final OperationType type;
    
    public TermConstruction(OperationType type) {
        this.type = type;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (type) {
            case FUNCTOR:
                return handleFunctor(query, bindings, solutions);
            case ARG:
                return handleArg(query, bindings, solutions);
            case UNIV:
                return handleUniv(query, bindings, solutions);
            case COPY_TERM:
                return handleCopyTerm(query, bindings, solutions);
            default:
                throw new PrologEvaluationException("Unknown term construction operation: " + type);
        }
    }
    
    private boolean handleFunctor(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("functor/3 requires exactly 3 arguments.");
        }

        // START_CHANGE: ISS-2025-0084 - Resolve bindings before type/ground checks
        Term term = query.getArguments().get(0).resolveBindings(bindings);
        Term functor = query.getArguments().get(1).resolveBindings(bindings);
        Term arity = query.getArguments().get(2).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0084

        // START_CHANGE: ISS-2025-0205 - functor/3 must support numbers as 0-ary atomic terms
        if (term.isGround()) {
            // Extract functor and arity
            Term resolvedTerm = term;
            Map<String, Term> newBindings = new HashMap<>(bindings);

            if (resolvedTerm instanceof Atom) {
                if (functor.unify(resolvedTerm, newBindings) &&
                    arity.unify(new Number(0L), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            } else if (resolvedTerm instanceof Number) {
                // ISO §8.5.1: functor(N, N, 0) for any number N
                if (functor.unify(resolvedTerm, newBindings) &&
                    arity.unify(new Number(0L), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            } else if (resolvedTerm instanceof CompoundTerm) {
                CompoundTerm ct = (CompoundTerm) resolvedTerm;
                if (functor.unify(ct.getFunctor(), newBindings) &&
                    arity.unify(new Number((long) ct.getArguments().size()), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            }
            return false;
        } else {
            // Construct term from functor and arity
            Term resolvedFunctor = functor;
            Term resolvedArity = arity;

            // START_CHANGE: Round5 - structured ISO error terms
            if (resolvedFunctor instanceof Variable || resolvedArity instanceof Variable) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("functor/3"));
            }
            if (!(resolvedArity instanceof Number) || !((Number) resolvedArity).isInteger()) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", resolvedArity, "functor/3"));
            }
            int arityValue = (int) ((Number) resolvedArity).longValue();
            if (arityValue < 0) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("not_less_than_zero", resolvedArity, "functor/3"));
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);

            if (arityValue == 0) {
                if (resolvedFunctor instanceof Atom || resolvedFunctor instanceof Number) {
                    if (term.unify(resolvedFunctor, newBindings)) {
                        solutions.add(new HashMap<>(newBindings));
                        return true;
                    }
                    return false;
                }
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("atomic", resolvedFunctor, "functor/3"));
            } else {
                // START_CHANGE: ISS-2025-0364 - ISO 8.5.1.3: a non-atomic Name (compound) raises
                // type_error(atomic, Name); type_error(atom, Name) is only for atomic-but-not-atom
                // Names (numbers, strings) with Arity > 0.
                if (!(resolvedFunctor instanceof Atom || resolvedFunctor instanceof Number
                        || resolvedFunctor instanceof it.denzosoft.jprolog.core.terms.PrologString)) {
                    throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                        it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("atomic", resolvedFunctor, "functor/3"));
                }
                // END_CHANGE: ISS-2025-0364
                if (!(resolvedFunctor instanceof Atom)) {
                    throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                        it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("atom", resolvedFunctor, "functor/3"));
                }
                // END_CHANGE: Round5
                List<Term> args = new ArrayList<>();
                for (int i = 0; i < arityValue; i++) {
                    args.add(new Variable("_G" + i));
                }
                Term constructed = new CompoundTerm((Atom) resolvedFunctor, args);
                if (term.unify(constructed, newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
                return false;
            }
        }
        // END_CHANGE: ISS-2025-0205
    }
    
    private boolean handleArg(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("arg/3 requires exactly 3 arguments.");
        }

        // START_CHANGE: ISS-2025-0084 - Resolve bindings before type/ground checks
        Term indexTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term term = query.getArguments().get(1).resolveBindings(bindings);
        Term arg = query.getArguments().get(2);
        // END_CHANGE: ISS-2025-0084

        // START_CHANGE: ISS-2025-0420 - ISO 8.5.2.3 error terms instead of the silent isGround
        // gate (which also wrongly failed arg/3 on NON-GROUND compounds like arg(1, f(X), A)):
        // N or Term unbound -> instantiation_error; N non-integer -> type_error(integer, N);
        // Term non-compound -> type_error(compound, Term); N < 0 -> domain_error(not_less_than_zero, N).
        if (indexTerm instanceof Variable || term instanceof Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("arg/3"));
        }
        if (!(indexTerm instanceof Number) || !((Number) indexTerm).isInteger()) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", indexTerm, "arg/3"));
        }
        if (!(term instanceof CompoundTerm)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("compound", term, "arg/3"));
        }
        long index = ((Number) indexTerm).longValue();
        if (index < 0) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("not_less_than_zero", indexTerm, "arg/3"));
        }
        CompoundTerm ct = (CompoundTerm) term;
        if (index >= 1 && index <= ct.getArguments().size()) {
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (arg.unify(ct.getArguments().get((int) index - 1).copy(), newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
        }
        return false;   // 0 or out-of-range index: plain failure (ISO 8.5.2.1)
        // END_CHANGE: ISS-2025-0420
    }
    
    private boolean handleUniv(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("=../2 requires exactly 2 arguments.");
        }

        // START_CHANGE: ISS-2025-0084 - Resolve bindings before type/ground checks
        Term term = query.getArguments().get(0).resolveBindings(bindings);
        Term list = query.getArguments().get(1).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0084

        // START_CHANGE: ISS-2025-0420 - ISO 8.5.3.3 error terms; the old isGround gates wrongly
        // raised instantiation_error for f(X) =.. L (decomposition of a non-ground term) and for
        // X =.. [f, Y] (construction with unbound arguments), and silently failed X =.. a.
        if (!(term instanceof Variable)) {
            // Decomposition: Term is bound (possibly non-ground) — convert it to a list
            Term listRepresentation = termToList(term);
            if (listRepresentation == null) {
                return false;
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (list.unify(listRepresentation, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
            return false;
        }

        // Construction: Term is unbound — List must be a proper, non-empty list.
        // Walk the spine: a variable tail (incl. X =.. Y, both unbound) -> instantiation_error
        // (ISO 8.5.3.3 a/b); any other non-list tail -> type_error(list, List) (8.5.3.3 c).
        Term tail = list;
        java.util.IdentityHashMap<Term, Boolean> seen = new java.util.IdentityHashMap<>();
        while (tail instanceof CompoundTerm
                && ".".equals(((CompoundTerm) tail).getName())
                && ((CompoundTerm) tail).getArguments().size() == 2) {
            if (seen.put(tail, Boolean.TRUE) != null) break;   // cyclic spine: not a list
            tail = ((CompoundTerm) tail).getArguments().get(1);
        }
        if (tail instanceof Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("=../2"));
        }
        if (!(tail instanceof Atom) || !"[]".equals(((Atom) tail).getName())) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("list", list, "=../2"));
        }

        List<Term> elements = extractElements(list);
        if (elements.isEmpty()) {
            // X =.. [] -> domain_error(non_empty_list, [])
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("non_empty_list", list, "=../2"));
        }
        Term head = elements.get(0);
        if (head instanceof Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("=../2"));
        }
        Term constructed;
        if (elements.size() == 1) {
            if (!(head instanceof Atom) && !(head instanceof Number)
                    && !(head instanceof it.denzosoft.jprolog.core.terms.PrologString)) {
                // X =.. [f(a)] -> type_error(atomic, f(a)) (ISO 8.5.3.3 f)
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("atomic", head, "=../2"));
            }
            constructed = head;
        } else {
            if (!(head instanceof Atom)) {
                // X =.. [3, x] / X =.. [f(a), a] -> type_error(atom, Head) (ISO 8.5.3.3 e)
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("atom", head, "=../2"));
            }
            constructed = new CompoundTerm((Atom) head, new ArrayList<>(elements.subList(1, elements.size())));
        }
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (term.unify(constructed, newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0420
    }
    
    private Term termToList(Term term) {
        if (term instanceof Atom) {
            List<Term> list = new ArrayList<>();
            list.add(term);
            return buildList(list);
        } else if (term instanceof Number) {
            // START_CHANGE: ISS-2025-0234 - ISO §8.5.3: number =.. [number]
            List<Term> list = new ArrayList<>();
            list.add(term);
            return buildList(list);
            // END_CHANGE: ISS-2025-0234
        } else if (term instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            // START_CHANGE: ISS-2025-0420 - strings are atomic: "s" =.. ["s"] (mirrors numbers)
            List<Term> list = new ArrayList<>();
            list.add(term);
            return buildList(list);
            // END_CHANGE: ISS-2025-0420
        } else if (term instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) term;
            List<Term> list = new ArrayList<>();
            list.add(ct.getFunctor());
            list.addAll(ct.getArguments());
            return buildList(list);
        }
        return null;
    }

    // ISS-2025-0420: the former listToTerm helper (ISS-2025-0234/Round5) was folded into
    // handleUniv above, which now raises the full set of ISO 8.5.3.3 error terms.
    
    // START_CHANGE: ISS-2025-0084 - Consolidate to use ListUtils
    private List<Term> extractElements(Term list) {
        return ListUtils.extractElements(list);
    }

    private Term buildList(List<Term> elements) {
        return ListUtils.createList(elements);
    }
    // END_CHANGE: ISS-2025-0084
    
    // START_CHANGE: ISS-2025-0122 - Fix copy_term to use fresh variable names
    private boolean handleCopyTerm(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("copy_term/2 requires exactly 2 arguments.");
        }

        Term term1 = query.getArguments().get(0);
        Term term2 = query.getArguments().get(1);

        Term resolvedTerm1 = term1.resolveBindings(bindings);
        // Use TermCopier which creates fresh variable names with unique prefix,
        // preventing variable name collisions with the calling context
        Term copy = it.denzosoft.jprolog.util.TermCopier.copyWithFreshVariables(resolvedTerm1);

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (term2.unify(copy, newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0122
}
