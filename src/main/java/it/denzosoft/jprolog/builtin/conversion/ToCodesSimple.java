// START_CHANGE: ISS-2025-0009 - Simplified to_codes/2 implementation
package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.PrologString;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Simplified implementation of to_codes/2 predicate for testing
 */
public class ToCodesSimple implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 2) {
            return false;
        }
        
        Term sourceTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term codesTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        // If source is atom and codes is variable
        if (sourceTerm instanceof Atom && codesTerm instanceof Variable) {
            return atomToCodesMode((Atom) sourceTerm, codesTerm, bindings, solutions);
        }
        
        // If source is string and codes is variable
        if (sourceTerm instanceof PrologString && codesTerm instanceof Variable) {
            return stringToCodesMode((PrologString) sourceTerm, codesTerm, bindings, solutions);
        }
        
        // If source is variable and codes is compound (list)
        if (sourceTerm instanceof Variable && isListTerm(codesTerm)) {
            return codesToAtomMode(sourceTerm, codesTerm, bindings, solutions);
        }
        
        // If both are bound, check consistency
        if (sourceTerm instanceof Atom && isListTerm(codesTerm)) {
            return checkConsistency((Atom) sourceTerm, codesTerm, bindings, solutions);
        }
        
        return false;
    }
    
    private boolean stringToCodesMode(PrologString string, Term codesTerm,
                                      Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String stringValue = string.getStringValue();
        Term codesList = createCodesListFromString(stringValue);
        
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (codesTerm.unify(codesList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
    
    private boolean atomToCodesMode(Atom atom, Term codesTerm, 
                                    Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String atomValue = atom.getName();
        Term codesList = createCodesListFromString(atomValue);
        
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (codesTerm.unify(codesList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
    
    private boolean codesToAtomMode(Term sourceTerm, Term codesTerm,
                                    Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String atomValue = createStringFromCodesList(codesTerm);
        if (atomValue == null) {
            return false;
        }
        
        Atom resultAtom = new Atom(atomValue);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (sourceTerm.unify(resultAtom, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
    
    private boolean checkConsistency(Atom atom, Term codesTerm,
                                     Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String atomValue = atom.getName();
        Term expectedCodes = createCodesListFromString(atomValue);
        
        Map<String, Term> tempBindings = new HashMap<>();
        if (expectedCodes.unify(codesTerm, tempBindings)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        return false;
    }
    
    private Term createCodesListFromString(String str) {
        if (str.isEmpty()) {
            return new Atom("[]");
        }
        
        Term result = new Atom("[]");
        for (int i = str.length() - 1; i >= 0; i--) {
            char c = str.charAt(i);
            Number code = new Number((double) (int) c);
            
            List<Term> args = new ArrayList<>();
            args.add(code);
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        
        return result;
    }
    
    private String createStringFromCodesList(Term list) {
        StringBuilder sb = new StringBuilder();
        Term current = list;
        
        while (!isEmptyList(current)) {
            Term head = getListHead(current);
            if (!(head instanceof Number)) {
                return null;
            }
            
            int code = ((Number) head).getValue().intValue();
            if (code < 0 || code > 255) {
                return null;
            }
            
            sb.append((char) code);
            current = getListTail(current);
            if (current == null) {
                return null;
            }
        }
        
        return sb.toString();
    }
    
    private boolean isListTerm(Term term) {
        return (term instanceof CompoundTerm && ".".equals(((CompoundTerm) term).getName())) ||
               "[]".equals(term.toString());
    }
    
    private boolean isEmptyList(Term term) {
        return "[]".equals(term.toString());
    }
    
    private Term getListHead(Term listTerm) {
        if (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (".".equals(compound.getName()) && compound.getArguments().size() >= 1) {
                return compound.getArguments().get(0);
            }
        }
        return null;
    }
    
    private Term getListTail(Term listTerm) {
        if (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (".".equals(compound.getName()) && compound.getArguments().size() >= 2) {
                return compound.getArguments().get(1);
            }
        }
        return null;
    }
}
// END_CHANGE: ISS-2025-0009