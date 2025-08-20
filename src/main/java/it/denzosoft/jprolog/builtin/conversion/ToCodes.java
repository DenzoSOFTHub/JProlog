// START_CHANGE: ISS-2025-0009 - Implement missing to_codes/2 built-in
package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.terms.CompoundTerm;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the ISO Prolog to_codes/2 predicate.
 * 
 * to_codes(+Source, ?Codes) is semidet
 * to_codes(?Source, +Codes) is semidet
 * 
 * Converts between atoms/strings and their character code lists.
 * 
 * Modes:
 * - to_codes(atom, Codes) - convert atom to list of character codes
 * - to_codes(list, list) - check if list is already a list of codes
 * - to_codes(Source, codes) - convert codes back to atom (if Source is unbound)
 * 
 * Examples:
 * - to_codes('123', Codes) → Codes = [49,50,51]
 * - to_codes(Codes, Codes) → true (if Codes is valid codes list)
 * - to_codes(X, [49,50,51]) → X = '123'
 * 
 * @author JProlog Team
 * @since ISS-2025-0009
 */
public class ToCodes implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        System.out.println("ToCodes.execute() called with query: " + query);
        
        if (query.getArguments().size() != 2) {
            System.out.println("ToCodes: Wrong number of arguments: " + query.getArguments().size());
            return false;
        }
        
        Term sourceTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term codesTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        // Mode 1: to_codes(+Source, ?Codes) - Source is bound, Codes may be unbound
        if (sourceTerm.isGround() && !codesTerm.isGround()) {
            return sourceToCodesMode(sourceTerm, codesTerm, bindings, solutions);
        }
        
        // Mode 2: to_codes(?Source, +Codes) - Codes is bound, Source may be unbound  
        if (!sourceTerm.isGround() && codesTerm.isGround()) {
            return codesToSourceMode(sourceTerm, codesTerm, bindings, solutions);
        }
        
        // Mode 3: to_codes(+Source, +Codes) - Both bound, check consistency
        if (sourceTerm.isGround() && codesTerm.isGround()) {
            return checkConsistency(sourceTerm, codesTerm, bindings, solutions);
        }
        
        // Mode 4: Both unbound - not supported
        return false;
    }
    
    /**
     * Mode: to_codes(+Source, ?Codes)
     * Convert source (atom) to character codes list
     */
    private boolean sourceToCodesMode(Term sourceTerm, Term codesTerm, 
                                      Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        
        List<Number> codes = new ArrayList<>();
        
        if (sourceTerm instanceof Atom) {
            String atomValue = ((Atom) sourceTerm).getName();
            for (char c : atomValue.toCharArray()) {
                codes.add(new Number((double) (int) c));
            }
        } else if (isListTerm(sourceTerm)) {
            // Already a list - check if it's a valid list of codes
            if (isValidCodesList(sourceTerm)) {
                // It's already a codes list, unify with itself
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (codesTerm.unify(sourceTerm, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            }
            return false;
        } else {
            return false;
        }
        
        // Create list term from codes
        Term codesList = createListFromCodes(codes);
        
        // Unify with the codes term
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (codesTerm.unify(codesList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        
        return false;
    }
    
    /**
     * Mode: to_codes(?Source, +Codes)
     * Convert character codes list to source (atom)
     */
    private boolean codesToSourceMode(Term sourceTerm, Term codesTerm,
                                      Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        
        if (!isListTerm(codesTerm)) {
            return false;
        }
        
        if (!isValidCodesList(codesTerm)) {
            return false;
        }
        
        // Convert codes to string
        StringBuilder sb = new StringBuilder();
        Term current = codesTerm;
        
        while (!isEmptyList(current)) {
            Term head = getListHead(current);
            if (head instanceof Number) {
                int code = ((Number) head).getValue().intValue();
                if (code >= 0 && code <= 255) {
                    sb.append((char) code);
                } else {
                    return false; // Invalid character code
                }
            } else {
                return false;
            }
            
            current = getListTail(current);
            if (current == null) {
                break;
            }
        }
        
        // Create atom from string
        Atom resultAtom = new Atom(sb.toString());
        
        // Unify with source term
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (sourceTerm.unify(resultAtom, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        
        return false;
    }
    
    /**
     * Mode: to_codes(+Source, +Codes)
     * Check if source and codes are consistent
     */
    private boolean checkConsistency(Term sourceTerm, Term codesTerm,
                                     Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        
        // Convert source to codes and compare
        Map<String, Term> tempBindings = new HashMap<>(bindings);
        Variable tempVar = new Variable("_TempCodes");
        List<Map<String, Term>> tempSolutions = new ArrayList<>();
        
        if (sourceToCodesMode(sourceTerm, tempVar, tempBindings, tempSolutions)) {
            if (!tempSolutions.isEmpty()) {
                Term generatedCodes = tempVar.resolveBindings(tempSolutions.get(0));
                if (generatedCodes.unify(codesTerm, new HashMap<>())) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
            }
        }
        
        return false;
    }
    
    /**
     * Check if a Term represents a valid list of character codes
     */
    private boolean isValidCodesList(Term list) {
        Term current = list;
        
        while (!isEmptyList(current)) {
            Term head = getListHead(current);
            if (!(head instanceof Number)) {
                return false;
            }
            
            Number numTerm = (Number) head;
            int code = numTerm.getValue().intValue();
            if (code < 0 || code > 255) {
                return false;
            }
            
            current = getListTail(current);
            if (current == null) {
                return false; // Not a proper list
            }
        }
        
        return true;
    }
    
    /**
     * Create a list Term from a list of Numbers
     */
    private Term createListFromCodes(List<Number> codes) {
        if (codes.isEmpty()) {
            return new Atom("[]"); // Empty list
        }
        
        // Build list from right to left  
        Term result = new Atom("[]");
        for (int i = codes.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(codes.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        
        return result;
    }
    
    /**
     * Check if a term represents a list
     */
    private boolean isListTerm(Term term) {
        return term instanceof CompoundTerm && ".".equals(((CompoundTerm) term).getFunctor()) ||
               "[]".equals(term.toString());
    }
    
    /**
     * Check if a term represents an empty list
     */
    private boolean isEmptyList(Term term) {
        return "[]".equals(term.toString());
    }
    
    /**
     * Get the head of a list term
     */
    private Term getListHead(Term listTerm) {
        if (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (".".equals(compound.getFunctor()) && compound.getArguments().size() >= 1) {
                return compound.getArguments().get(0);
            }
        }
        return null;
    }
    
    /**
     * Get the tail of a list term
     */
    private Term getListTail(Term listTerm) {
        if (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (".".equals(compound.getFunctor()) && compound.getArguments().size() >= 2) {
                return compound.getArguments().get(1);
            }
        }
        return null;
    }
}
// END_CHANGE: ISS-2025-0009