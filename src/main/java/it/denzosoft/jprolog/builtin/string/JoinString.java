package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

/**
 * Implementation of atomic_list_concat/3 predicate for joining strings.
 * 
 * atomic_list_concat(+List, +Separator, ?Atom)
 * atomic_list_concat(?List, +Separator, +Atom)
 * 
 * Joins a list of atomic terms with a separator, or splits an atom into a list.
 */
public class JoinString implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("atomic_list_concat/3 requires exactly 3 arguments");
        }
        
        Term listTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term separatorTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term atomTerm = query.getArguments().get(2).resolveBindings(bindings);
        
        try {
            if (!(separatorTerm instanceof Atom)) {
                throw new PrologEvaluationException("atomic_list_concat/3: separator must be an atom");
            }
            
            String separator = ((Atom) separatorTerm).getName();
            
            if (listTerm instanceof Variable && !(atomTerm instanceof Variable)) {
                // Split atom into list
                return splitAtomToList((Variable) listTerm, separator, atomTerm, bindings, solutions);
            } else if (!(listTerm instanceof Variable) && atomTerm instanceof Variable) {
                // Join list into atom
                return joinListToAtom(listTerm, separator, (Variable) atomTerm, bindings, solutions);
            } else if (!(listTerm instanceof Variable) && !(atomTerm instanceof Variable)) {
                // Test join/split
                return testJoinSplit(listTerm, separator, atomTerm);
            } else {
                throw new PrologEvaluationException("atomic_list_concat/3: either List or Atom must be instantiated");
            }
        } catch (Exception e) {
            throw new PrologEvaluationException("atomic_list_concat/3 error: " + e.getMessage());
        }
    }
    
    /**
     * Split atom into list using separator.
     */
    private boolean splitAtomToList(Variable listVar, String separator, Term atomTerm, 
                                  Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(atomTerm instanceof Atom)) {
            throw new PrologEvaluationException("atomic_list_concat/3: atom argument must be an atom");
        }
        
        String atomValue = ((Atom) atomTerm).getName();
        
        String[] parts;
        if (separator.isEmpty()) {
            // Empty separator - split into individual characters
            parts = new String[atomValue.length()];
            for (int i = 0; i < atomValue.length(); i++) {
                parts[i] = String.valueOf(atomValue.charAt(i));
            }
        } else {
            parts = atomValue.split(java.util.regex.Pattern.quote(separator), -1);
        }
        
        // Convert array to Prolog list
        List<Term> termList = new ArrayList<>();
        for (String part : parts) {
            termList.add(new Atom(part));
        }
        
        Term resultList = createPrologList(termList);
        
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (listVar.unify(resultList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        
        return false;
    }
    
    /**
     * Join list into atom using separator.
     */
    private boolean joinListToAtom(Term listTerm, String separator, Variable atomVar, 
                                 Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<String> stringList = extractStringList(listTerm);
        if (stringList == null) {
            return false;
        }
        
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < stringList.size(); i++) {
            if (i > 0) {
                result.append(separator);
            }
            result.append(stringList.get(i));
        }
        
        Atom resultAtom = new Atom(result.toString());
        
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (atomVar.unify(resultAtom, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        
        return false;
    }
    
    /**
     * Test if join/split operation is correct.
     */
    private boolean testJoinSplit(Term listTerm, String separator, Term atomTerm) {
        if (!(atomTerm instanceof Atom)) {
            return false;
        }
        
        List<String> stringList = extractStringList(listTerm);
        if (stringList == null) {
            return false;
        }
        
        StringBuilder expected = new StringBuilder();
        for (int i = 0; i < stringList.size(); i++) {
            if (i > 0) {
                expected.append(separator);
            }
            expected.append(stringList.get(i));
        }
        
        String atomValue = ((Atom) atomTerm).getName();
        return expected.toString().equals(atomValue);
    }
    
    /**
     * Extract list of strings from a Prolog list term.
     */
    private List<String> extractStringList(Term listTerm) {
        List<String> result = new ArrayList<>();
        Term current = listTerm;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            
            if (!".".equals(compound.getName()) || compound.getArguments().size() != 2) {
                break;
            }
            
            Term head = compound.getArguments().get(0);
            if (head instanceof Atom) {
                result.add(((Atom) head).getName());
            } else if (head instanceof PrologString) {
                result.add(((PrologString) head).getStringValue());
            } else {
                return null; // Invalid list element
            }
            
            current = compound.getArguments().get(1);
        }
        
        // Check for proper list termination
        if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
            return result;
        }
        
        return null; // Not a proper list
    }
    
    /**
     * Create a Prolog list from a Java list of terms.
     */
    private Term createPrologList(List<Term> terms) {
        Term result = new Atom("[]"); // Empty list
        
        // Build list from right to left
        for (int i = terms.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(
                new Atom("."), 
                Arrays.asList(terms.get(i), result)
            );
        }
        
        return result;
    }
}