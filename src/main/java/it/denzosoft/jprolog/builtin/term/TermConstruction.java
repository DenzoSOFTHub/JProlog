package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Number;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

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

        Term term = query.getArguments().get(0);
        Term functor = query.getArguments().get(1);
        Term arity = query.getArguments().get(2);
        
        if (term.isGround()) {
            // Extract functor and arity
            Term resolvedTerm = term.resolveBindings(bindings);
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
            Term resolvedFunctor = functor.resolveBindings(bindings);
            Term resolvedArity = arity.resolveBindings(bindings);
            
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

        Term indexTerm = query.getArguments().get(0);
        Term term = query.getArguments().get(1);
        Term arg = query.getArguments().get(2);
        
        if (!indexTerm.isGround() || !term.isGround()) {
            throw new PrologEvaluationException("arg/3: First two arguments must be ground.");
        }
        
        Term resolvedIndex = indexTerm.resolveBindings(bindings);
        Term resolvedTerm = term.resolveBindings(bindings);
        
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

        Term term = query.getArguments().get(0);
        Term list = query.getArguments().get(1);
        
        if (term.isGround()) {
            // Convert term to list
            Term resolvedTerm = term.resolveBindings(bindings);
            Term listRepresentation = termToList(resolvedTerm);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (list.unify(listRepresentation, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
            return false;
        } else if (list.isGround()) {
            // Convert list to term
            Term resolvedList = list.resolveBindings(bindings);
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
    
    private List<Term> extractElements(Term list) {
        List<Term> elements = new ArrayList<>();
        Term current = list;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                elements.add(compound.getArguments().get(0));
                current = compound.getArguments().get(1);
            } else {
                break;
            }
        }
        
        return elements;
    }
    
    private Term buildList(List<Term> elements) {
        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
    
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
