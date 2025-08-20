package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of atom_codes/2 predicate.
 * 
 * atom_codes(+Atom, ?Codes) - Convert between atom and list of character codes
 * atom_codes(?Atom, +Codes) - Convert between list of character codes and atom
 * 
 * Examples:
 * ?- atom_codes(hello, X).
 * X = [104, 101, 108, 108, 111].
 * 
 * ?- atom_codes(X, [104, 101, 108, 108, 111]).
 * X = hello.
 */
public class AtomCodes implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException("atom_codes/2 requires exactly 2 arguments");
        }
        
        Term atomTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term codesTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        // Case 1: atom_codes(+Atom, ?Codes) - atom to codes
        if (atomTerm instanceof Atom && atomTerm.isGround()) {
            String atomValue = ((Atom) atomTerm).getName();
            List<Term> codes = new ArrayList<>();
            
            for (char c : atomValue.toCharArray()) {
                codes.add(new it.denzosoft.jprolog.core.terms.Number((double) (int) c));
            }
            
            Term codesList = createList(codes);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (codesTerm.unify(codesList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }
        
        // Case 2: atom_codes(?Atom, +Codes) - codes to atom
        if (codesTerm.isGround() && isProperList(codesTerm)) {
            List<Term> codeElements = getListElements(codesTerm);
            StringBuilder atomBuilder = new StringBuilder();
            
            for (Term codeTerm : codeElements) {
                if (!(codeTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
                    throw new PrologEvaluationException("atom_codes/2: codes list must contain only numbers");
                }
                
                double codeValue = ((it.denzosoft.jprolog.core.terms.Number) codeTerm).getValue();
                if (codeValue != Math.floor(codeValue) || codeValue < 0 || codeValue > 1114111) {
                    throw new PrologEvaluationException("atom_codes/2: invalid character code: " + codeValue);
                }
                
                atomBuilder.append((char) (int) codeValue);
            }
            
            Term atomResult = new Atom(atomBuilder.toString());
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (atomTerm.unify(atomResult, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }
        
        // Case 3: Both arguments are variables - not supported
        if (atomTerm instanceof Variable && codesTerm instanceof Variable) {
            throw new PrologEvaluationException("atom_codes/2: at least one argument must be instantiated");
        }
        
        return false;
    }
    
    /**
     * Create a Prolog list from a Java list of terms.
     */
    private Term createList(List<Term> elements) {
        if (elements.isEmpty()) {
            return new Atom("[]");
        }
        
        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
    
    /**
     * Check if a term is a proper list.
     */
    private boolean isProperList(Term term) {
        Term current = term;
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (!compound.getName().equals(".") || compound.getArguments().size() != 2) {
                return false;
            }
            current = compound.getArguments().get(1);
        }
        return current instanceof Atom && ((Atom) current).getName().equals("[]");
    }
    
    /**
     * Extract elements from a Prolog list.
     */
    private List<Term> getListElements(Term listTerm) {
        List<Term> elements = new ArrayList<>();
        Term current = listTerm;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (!compound.getName().equals(".") || compound.getArguments().size() != 2) {
                break;
            }
            elements.add(compound.getArguments().get(0));
            current = compound.getArguments().get(1);
        }
        
        return elements;
    }
}