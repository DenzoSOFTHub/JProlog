// START_CHANGE: ISS-2025-0048 - Implement char_conversion/2 and current_char_conversion/2
package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * char_conversion/2 and current_char_conversion/2 - ISO Prolog predicates.
 * Manages character conversion table used during term reading.
 */
public class CharConversion implements BuiltIn {

    public enum Mode { DEFINE, QUERY }

    private final Mode mode;

    private static final Map<Character, Character> conversionTable = new ConcurrentHashMap<>();

    public CharConversion(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException(
                (mode == Mode.DEFINE ? "char_conversion" : "current_char_conversion") + "/2 requires 2 arguments");
        }

        if (mode == Mode.DEFINE) {
            return defineConversion(query, bindings, solutions);
        } else {
            return queryConversion(query, bindings, solutions);
        }
    }

    private boolean defineConversion(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        Term fromTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term toTerm = query.getArguments().get(1).resolveBindings(bindings);

        if (!(fromTerm instanceof Atom) || !(toTerm instanceof Atom)) {
            throw new PrologEvaluationException("char_conversion/2: both arguments must be single-character atoms");
        }

        String fromStr = ((Atom) fromTerm).getName();
        String toStr = ((Atom) toTerm).getName();

        if (fromStr.length() != 1 || toStr.length() != 1) {
            throw new PrologEvaluationException("char_conversion/2: both arguments must be single-character atoms");
        }

        char from = fromStr.charAt(0);
        char to = toStr.charAt(0);

        if (from == to) {
            conversionTable.remove(from);
        } else {
            conversionTable.put(from, to);
        }

        solutions.add(bindings);
        return true;
    }

    private boolean queryConversion(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        Term fromTerm = query.getArguments().get(0);
        Term toTerm = query.getArguments().get(1);

        Term resolvedFrom = fromTerm.resolveBindings(bindings);
        Term resolvedTo = toTerm.resolveBindings(bindings);

        boolean fromIsVar = resolvedFrom instanceof Variable;
        boolean toIsVar = resolvedTo instanceof Variable;

        if (fromIsVar && toIsVar) {
            // Enumerate all conversions
            for (Map.Entry<Character, Character> entry : conversionTable.entrySet()) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                Atom fromAtom = new Atom(String.valueOf(entry.getKey()));
                Atom toAtom = new Atom(String.valueOf(entry.getValue()));
                if (fromTerm.unify(fromAtom, newBindings) && toTerm.unify(toAtom, newBindings)) {
                    solutions.add(newBindings);
                }
            }
            // Also enumerate identity conversions for all printable chars
            for (char c = 32; c < 127; c++) {
                if (!conversionTable.containsKey(c)) {
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    Atom charAtom = new Atom(String.valueOf(c));
                    if (fromTerm.unify(charAtom, newBindings) && toTerm.unify(charAtom, newBindings)) {
                        solutions.add(newBindings);
                    }
                }
            }
            return !solutions.isEmpty();
        } else if (!fromIsVar) {
            String fromStr = ((Atom) resolvedFrom).getName();
            if (fromStr.length() != 1) return false;
            char from = fromStr.charAt(0);
            Character to = conversionTable.getOrDefault(from, from);
            Atom toAtom = new Atom(String.valueOf(to));
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (toTerm.unify(toAtom, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } else {
            // toIsVar is false, fromIsVar is true — find matching from chars
            String toStr = ((Atom) resolvedTo).getName();
            if (toStr.length() != 1) return false;
            char toChar = toStr.charAt(0);
            for (Map.Entry<Character, Character> entry : conversionTable.entrySet()) {
                if (entry.getValue() == toChar) {
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    Atom fromAtom = new Atom(String.valueOf(entry.getKey()));
                    if (fromTerm.unify(fromAtom, newBindings)) {
                        solutions.add(newBindings);
                    }
                }
            }
            return !solutions.isEmpty();
        }
    }

    public static Map<Character, Character> getConversionTable() {
        return conversionTable;
    }
}
// END_CHANGE: ISS-2025-0048
