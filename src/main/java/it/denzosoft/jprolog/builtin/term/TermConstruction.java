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

        if (term.isGround()) {
            // Extract functor and arity
            Term resolvedTerm = term;
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (resolvedTerm instanceof Atom) {
                if (functor.unify(resolvedTerm, newBindings) && 
                    arity.unify(new Number(0), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            } else if (resolvedTerm instanceof CompoundTerm) {
                CompoundTerm ct = (CompoundTerm) resolvedTerm;
                if (functor.unify(ct.getFunctor(), newBindings) && 
                    arity.unify(new Number(ct.getArguments().size()), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            }
            return false;
        } else {
            // Construct term from functor and arity
            Term resolvedFunctor = functor;
            Term resolvedArity = arity;
            
            if (resolvedFunctor instanceof Atom && resolvedArity instanceof Number) {
                int arityValue = (int) Math.round(((Number) resolvedArity).getValue());
                Map<String, Term> newBindings = new HashMap<>(bindings);
                
                if (arityValue == 0) {
                    if (term.unify(resolvedFunctor, newBindings)) {
                        solutions.add(new HashMap<>(newBindings));
                        return true;
                    }
                } else if (arityValue > 0) {
                    List<Term> args = new ArrayList<>();
                    for (int i = 0; i < arityValue; i++) {
                        args.add(new Variable("_" + i));
                    }
                    Term constructed = new CompoundTerm((Atom) resolvedFunctor, args);
                    if (term.unify(constructed, newBindings)) {
                        solutions.add(new HashMap<>(newBindings));
                        return true;
                    }
                }
            }
            return false;
        }
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

        if (!indexTerm.isGround() || !term.isGround()) {
            return false;
        }

        Term resolvedIndex = indexTerm;
        Term resolvedTerm = term;
        
        if (resolvedIndex instanceof Number && resolvedTerm instanceof CompoundTerm) {
            int index = (int) Math.round(((Number) resolvedIndex).getValue());
            CompoundTerm ct = (CompoundTerm) resolvedTerm;
            
            if (index > 0 && index <= ct.getArguments().size()) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (arg.unify(ct.getArguments().get(index - 1).copy(), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            }
        }
        return false;
    }
    
    private boolean handleUniv(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("=../2 requires exactly 2 arguments.");
        }

        // START_CHANGE: ISS-2025-0084 - Resolve bindings before type/ground checks
        Term term = query.getArguments().get(0).resolveBindings(bindings);
        Term list = query.getArguments().get(1).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0084

        if (term.isGround()) {
            // Convert term to list
            Term resolvedTerm = term;
            Term listRepresentation = termToList(resolvedTerm);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (list.unify(listRepresentation, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
            return false;
        } else if (list.isGround()) {
            // Convert list to term
            Term resolvedList = list;
            Term termRepresentation = listToTerm(resolvedList);
            if (termRepresentation != null) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (term.unify(termRepresentation, newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            }
            return false;
        } else {
            throw new PrologEvaluationException("=../2: At least one argument must be ground.");
        }
    }
    
    private Term termToList(Term term) {
        if (term instanceof Atom) {
            List<Term> list = new ArrayList<>();
            list.add(term);
            return buildList(list);
        } else if (term instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) term;
            List<Term> list = new ArrayList<>();
            list.add(ct.getFunctor());
            list.addAll(ct.getArguments());
            return buildList(list);
        }
        return null;
    }
    
    private Term listToTerm(Term list) {
        List<Term> elements = extractElements(list);
        if (elements.isEmpty()) {
            return null;
        }
        
        if (elements.size() == 1 && elements.get(0) instanceof Atom) {
            return elements.get(0); // Simple atom
        } else if (elements.size() >= 1 && elements.get(0) instanceof Atom) {
            Atom functor = (Atom) elements.get(0);
            List<Term> args = elements.subList(1, elements.size());
            return new CompoundTerm(functor, new ArrayList<>(args));
        }
        return null;
    }
    
    // START_CHANGE: ISS-2025-0084 - Consolidate to use ListUtils
    private List<Term> extractElements(Term list) {
        return ListUtils.extractElements(list);
    }

    private Term buildList(List<Term> elements) {
        return ListUtils.createList(elements);
    }
    // END_CHANGE: ISS-2025-0084
    
    private boolean handleCopyTerm(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("copy_term/2 requires exactly 2 arguments.");
        }

        Term term1 = query.getArguments().get(0);
        Term term2 = query.getArguments().get(1);
        
        Term resolvedTerm1 = term1.resolveBindings(bindings);
        Term copy = resolvedTerm1.copy();
        
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (term2.unify(copy, newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }
        return false;
    }
}
