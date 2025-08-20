package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of string_codes/2 predicate.
 * 
 * string_codes(+String, ?Codes) - Convert string to list of character codes
 * string_codes(?String, +Codes) - Convert list of character codes to string
 * 
 * Examples:
 * ?- string_codes("hello", X).
 * X = [104, 101, 108, 108, 111].
 * 
 * ?- string_codes(X, [104, 101, 108, 108, 111]).
 * X = "hello".
 */
public class StringCodes implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException("string_codes/2 requires exactly 2 arguments");
        }
        
        Term stringTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term codesTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        // Case 1: string_codes(+String, ?Codes) - string to codes
        if (stringTerm instanceof PrologString && stringTerm.isGround()) {
            String stringValue = ((PrologString) stringTerm).getStringValue();
            List<Term> codes = new ArrayList<>();
            
            for (char c : stringValue.toCharArray()) {
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
        
        // Case 2: string_codes(?String, +Codes) - codes to string
        if (codesTerm.isGround() && isProperList(codesTerm)) {
            List<Term> codeElements = getListElements(codesTerm);
            StringBuilder stringBuilder = new StringBuilder();
            
            for (Term codeTerm : codeElements) {
                if (!(codeTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
                    throw new PrologEvaluationException("string_codes/2: codes list must contain only numbers");
                }
                
                double codeValue = ((it.denzosoft.jprolog.core.terms.Number) codeTerm).getValue();
                if (codeValue != Math.floor(codeValue) || codeValue < 0 || codeValue > 1114111) {
                    throw new PrologEvaluationException("string_codes/2: invalid character code: " + codeValue);
                }
                
                stringBuilder.append((char) (int) codeValue);
            }
            
            Term stringResult = new PrologString(stringBuilder.toString());
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (stringTerm.unify(stringResult, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }
        
        // Case 3: Both arguments are variables - not supported
        if (stringTerm instanceof Variable && codesTerm instanceof Variable) {
            throw new PrologEvaluationException("string_codes/2: at least one argument must be instantiated");
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