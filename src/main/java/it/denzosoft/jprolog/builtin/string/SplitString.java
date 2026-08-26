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
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("split_string/4 error: " + e.getMessage());
        }
    }
    
    /**
     * Split string using separators and remove padding characters.
     */
    private List<String> splitString(String input, String separators, String padChars) {
        // START_CHANGE: ISS-2025-0233 - codepoint-aware separators + pad
        Set<Integer> sepSet = new HashSet<>();
        separators.codePoints().forEach(sepSet::add);
        Set<Integer> padSet = new HashSet<>();
        padChars.codePoints().forEach(padSet::add);
        // END_CHANGE: ISS-2025-0233

        // START_CHANGE: ISS-2025-0267 - SWI semantics: empty substrings are KEPT (split_string(
        // "a,,b", ",", "", X) -> ["a","","b"]) and the (possibly empty) final field is always
        // emitted (split_string("", ",", "", X) -> [""]). Only when a separator char is ALSO a
        // pad char do runs of separators collapse (empty fields dropped), per SWI.
        boolean collapse = false;
        for (int s : sepSet) {
            if (padSet.contains(s)) { collapse = true; break; }
        }

        List<String> fields = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        int i = 0;
        while (i < input.length()) {
            int cp = input.codePointAt(i);
            if (sepSet.contains(cp)) {
                fields.add(trimPadding(current.toString(), padSet));
                current.setLength(0);
            } else {
                current.appendCodePoint(cp);
            }
            i += Character.charCount(cp);
        }
        fields.add(trimPadding(current.toString(), padSet)); // always emit the final field

        if (!collapse) {
            return fields;
        }
        List<String> result = new ArrayList<>();
        for (String f : fields) {
            if (!f.isEmpty()) result.add(f);
        }
        if (result.isEmpty()) result.add(""); // SWI: returns [""] when everything collapses
        return result;
        // END_CHANGE: ISS-2025-0267
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