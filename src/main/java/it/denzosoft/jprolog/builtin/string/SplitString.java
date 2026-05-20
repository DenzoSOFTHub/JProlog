package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

/**
 * Implementation of split_string/4 predicate for string splitting.
 * 
 * split_string(+String, +SepChars, +PadChars, -SubStrings)
 * 
 * Splits String into SubStrings using SepChars as separators and PadChars as padding.
 */
public class SplitString implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 4) {
            throw new PrologEvaluationException("split_string/4 requires exactly 4 arguments");
        }
        
        Term stringTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term sepCharsTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term padCharsTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term subStringsTerm = query.getArguments().get(3);
        
        try {
            // Validate input arguments
            if (!(stringTerm instanceof PrologString)) {
                throw new PrologEvaluationException("split_string/4: first argument must be a string");
            }
            
            if (!(sepCharsTerm instanceof PrologString)) {
                throw new PrologEvaluationException("split_string/4: second argument must be a string");
            }
            
            if (!(padCharsTerm instanceof PrologString)) {
                throw new PrologEvaluationException("split_string/4: third argument must be a string");
            }
            
            String inputString = ((PrologString) stringTerm).getStringValue();
            String separators = ((PrologString) sepCharsTerm).getStringValue();
            String padChars = ((PrologString) padCharsTerm).getStringValue();
            
            List<String> subStrings = splitString(inputString, separators, padChars);
            Term resultList = createPrologStringList(subStrings);
            
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (subStringsTerm.unify(resultList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
            return false;
        } catch (Exception e) {
            throw new PrologEvaluationException("split_string/4 error: " + e.getMessage());
        }
    }
    
    /**
     * Split string using separators and remove padding characters.
     */
    private List<String> splitString(String input, String separators, String padChars) {
        List<String> result = new ArrayList<>();

        if (input.isEmpty()) {
            return result;
        }

        // START_CHANGE: ISS-2025-0233 - codepoint-aware separators + pad
        Set<Integer> sepSet = new HashSet<>();
        separators.codePoints().forEach(sepSet::add);
        Set<Integer> padSet = new HashSet<>();
        padChars.codePoints().forEach(padSet::add);

        StringBuilder current = new StringBuilder();
        int i = 0;
        while (i < input.length()) {
            int cp = input.codePointAt(i);
            if (sepSet.contains(cp)) {
                String trimmed = trimPadding(current.toString(), padSet);
                if (!trimmed.isEmpty()) {
                    result.add(trimmed);
                }
                current.setLength(0);
            } else {
                current.appendCodePoint(cp);
            }
            i += Character.charCount(cp);
        }

        if (current.length() > 0) {
            String trimmed = trimPadding(current.toString(), padSet);
            if (!trimmed.isEmpty()) {
                result.add(trimmed);
            }
        }

        return result;
        // END_CHANGE: ISS-2025-0233
    }

    /**
     * Remove padding characters from start and end of string (codepoint-aware).
     */
    private String trimPadding(String str, Set<Integer> padChars) {
        if (str.isEmpty()) {
            return str;
        }
        // START_CHANGE: ISS-2025-0233 - codepoint-aware trim
        int start = 0;
        while (start < str.length()) {
            int cp = str.codePointAt(start);
            if (!padChars.contains(cp)) break;
            start += Character.charCount(cp);
        }
        int end = str.length();
        while (end > start) {
            int prev = end - 1;
            if (Character.isLowSurrogate(str.charAt(prev)) && prev > 0
                && Character.isHighSurrogate(str.charAt(prev - 1))) {
                prev--;
            }
            int cp = str.codePointAt(prev);
            if (!padChars.contains(cp)) break;
            end = prev;
        }
        return str.substring(start, end);
        // END_CHANGE: ISS-2025-0233
    }
    
    /**
     * Create a Prolog list from a Java list of strings.
     */
    private Term createPrologStringList(List<String> strings) {
        Term result = new Atom("[]"); // Empty list
        
        // Build list from right to left
        for (int i = strings.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(
                new Atom("."), 
                Arrays.asList(new PrologString(strings.get(i)), result)
            );
        }
        
        return result;
    }
}