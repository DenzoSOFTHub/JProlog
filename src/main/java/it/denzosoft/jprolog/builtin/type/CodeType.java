// START_CHANGE: LIM-006 - code_type/2 built-in predicate
package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.terms.Number;

import java.util.*;

/**
 * Implementation of code_type/2 predicate.
 *
 * code_type(?Code, ?Type)
 *
 * Classifies character codes. Code must be an integer (character code).
 * This is the character-code equivalent of char_type/2.
 *
 * Supported types:
 *   alpha       - letter
 *   alnum       - letter or digit
 *   digit(W)    - digit with numeric weight W
 *   space       - whitespace
 *   upper(L)    - uppercase with lowercase code L
 *   lower(U)    - lowercase with uppercase code U
 *   ascii       - ASCII character (0-127)
 *   white       - whitespace
 *   end_of_line - newline character
 *   csym        - valid in identifiers (letter, digit, underscore)
 *   csymf       - valid as first char of identifier (letter, underscore)
 *   punct       - punctuation
 *   graph       - printable non-space
 *   print       - printable (including space)
 *   cntrl       - control character
 */
public class CodeType implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings,
                          List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("code_type/2 requires exactly 2 arguments");
        }

        Term codeTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term typeTerm = query.getArguments().get(1).resolveBindings(bindings);

        try {
            if (codeTerm instanceof Variable && typeTerm instanceof Variable) {
                // Both unbound: enumerate all code-type pairs in ASCII range
                return generateAllPairs((Variable) codeTerm, (Variable) typeTerm, bindings, solutions);
            } else if (codeTerm instanceof Variable) {
                // Code unbound, Type bound: enumerate matching codes
                return generateCodesOfType((Variable) codeTerm, typeTerm, bindings, solutions);
            } else if (typeTerm instanceof Variable) {
                // Code bound, Type unbound: enumerate types of code
                return findTypesOfCode(codeTerm, (Variable) typeTerm, bindings, solutions);
            } else {
                // Both bound: check
                int code = getCode(codeTerm);
                if (code < 0) return false;
                if (checkType((char) code, typeTerm)) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            }
        } catch (PrologEvaluationException e) {
            throw e;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("code_type/2 error: " + e.getMessage());
        }
    }

    /**
     * Extract integer code from a term.
     * Returns -1 if not a valid code.
     */
    private int getCode(Term term) {
        if (term instanceof Number) {
            double val = ((Number) term).getValue().doubleValue();
            if (val != Math.floor(val) || val < 0 || val > Character.MAX_VALUE) {
                return -1;
            }
            return (int) val;
        }
        return -1;
    }

    /**
     * Check if a character matches a given type term.
     */
    private boolean checkType(char ch, Term typeTerm) {
        if (typeTerm instanceof Atom) {
            String typeName = ((Atom) typeTerm).getName();
            return checkSimpleType(ch, typeName);
        } else if (typeTerm instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) typeTerm;
            String functor = ct.getFunctor().getName();
            List<Term> args = ct.getArguments();
            if (args.size() != 1) return false;

            switch (functor) {
                case "digit": {
                    if (!Character.isDigit(ch)) return false;
                    int weight = Character.getNumericValue(ch);
                    Term argTerm = args.get(0);
                    if (argTerm instanceof Number) {
                        return ((Number) argTerm).getValue().intValue() == weight;
                    } else if (argTerm instanceof Variable) {
                        // For checking mode, variable means "any weight" - always true for a digit
                        return true;
                    }
                    return false;
                }
                case "upper": {
                    if (!Character.isUpperCase(ch)) return false;
                    int lowerCode = (int) Character.toLowerCase(ch);
                    Term argTerm = args.get(0);
                    if (argTerm instanceof Number) {
                        return ((Number) argTerm).getValue().intValue() == lowerCode;
                    } else if (argTerm instanceof Variable) {
                        return true;
                    }
                    return false;
                }
                case "lower": {
                    if (!Character.isLowerCase(ch)) return false;
                    int upperCode = (int) Character.toUpperCase(ch);
                    Term argTerm = args.get(0);
                    if (argTerm instanceof Number) {
                        return ((Number) argTerm).getValue().intValue() == upperCode;
                    } else if (argTerm instanceof Variable) {
                        return true;
                    }
                    return false;
                }
                default:
                    return false;
            }
        }
        return false;
    }

    /**
     * Check a simple (atom) type against a character.
     */
    private boolean checkSimpleType(char ch, String typeName) {
        switch (typeName) {
            case "alpha":       return Character.isLetter(ch);
            case "alnum":       return Character.isLetterOrDigit(ch);
            case "space":       return Character.isWhitespace(ch);
            case "white":       return Character.isWhitespace(ch);
            case "ascii":       return ch >= 0 && ch <= 127;
            case "end_of_line": return ch == '\n' || ch == '\r';
            case "csym":        return Character.isLetterOrDigit(ch) || ch == '_';
            case "csymf":       return Character.isLetter(ch) || ch == '_';
            case "punct":       return isPunctuation(ch);
            case "graph":       return !Character.isWhitespace(ch) && !Character.isISOControl(ch) && ch != ' ';
            case "print":       return !Character.isISOControl(ch);
            case "cntrl":       return Character.isISOControl(ch);
            case "digit":       return Character.isDigit(ch);
            case "upper":       return Character.isUpperCase(ch);
            case "lower":       return Character.isLowerCase(ch);
            default:            return false;
        }
    }

    private boolean isPunctuation(char ch) {
        return "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".indexOf(ch) >= 0;
    }

    /**
     * Simple type names for enumeration (atom forms only).
     */
    private static final String[] SIMPLE_TYPES = {
        "alpha", "alnum", "space", "white", "ascii", "end_of_line",
        "csym", "csymf", "punct", "graph", "print", "cntrl"
    };

    /**
     * Find all types of a given code.
     */
    private boolean findTypesOfCode(Term codeTerm, Variable typeVar,
                                    Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) {
        int code = getCode(codeTerm);
        if (code < 0) return false;
        char ch = (char) code;

        // Check simple types
        for (String typeName : SIMPLE_TYPES) {
            if (checkSimpleType(ch, typeName)) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (typeVar.unify(new Atom(typeName), newBindings)) {
                    solutions.add(newBindings);
                }
            }
        }

        // Check compound types
        if (Character.isDigit(ch)) {
            int weight = Character.getNumericValue(ch);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            Term digitTerm = new CompoundTerm(new Atom("digit"),
                Collections.singletonList(new Number(weight)));
            if (typeVar.unify(digitTerm, newBindings)) {
                solutions.add(newBindings);
            }
        }
        if (Character.isUpperCase(ch)) {
            int lowerCode = (int) Character.toLowerCase(ch);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            Term upperTerm = new CompoundTerm(new Atom("upper"),
                Collections.singletonList(new Number(lowerCode)));
            if (typeVar.unify(upperTerm, newBindings)) {
                solutions.add(newBindings);
            }
        }
        if (Character.isLowerCase(ch)) {
            int upperCode = (int) Character.toUpperCase(ch);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            Term lowerTerm = new CompoundTerm(new Atom("lower"),
                Collections.singletonList(new Number(upperCode)));
            if (typeVar.unify(lowerTerm, newBindings)) {
                solutions.add(newBindings);
            }
        }

        return !solutions.isEmpty();
    }

    /**
     * Generate codes matching a given type.
     */
    private boolean generateCodesOfType(Variable codeVar, Term typeTerm,
                                        Map<String, Term> bindings,
                                        List<Map<String, Term>> solutions) {
        for (int i = 0; i <= 127; i++) {
            char ch = (char) i;
            if (checkType(ch, typeTerm)) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                // For compound types with variables, we need to also bind the argument
                Term resolvedType = resolveTypeForCode(ch, typeTerm);
                if (resolvedType != null) {
                    Term typeArg = query_getArguments_typeTerm(typeTerm);
                    if (typeArg instanceof Variable) {
                        // Need to unify the argument too
                        newBindings = new HashMap<>(bindings);
                        if (codeVar.unify(new Number(i), newBindings) &&
                            typeTerm.unify(resolvedType, newBindings)) {
                            solutions.add(newBindings);
                        }
                        continue;
                    }
                }
                if (codeVar.unify(new Number(i), newBindings)) {
                    solutions.add(newBindings);
                }
            }
        }
        return !solutions.isEmpty();
    }

    /**
     * Helper: get the argument of a compound type term, or null.
     */
    private Term query_getArguments_typeTerm(Term typeTerm) {
        if (typeTerm instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) typeTerm;
            if (ct.getArguments().size() == 1) {
                return ct.getArguments().get(0);
            }
        }
        return null;
    }

    /**
     * Create a fully resolved type term for a given character code.
     * Returns null for simple types.
     */
    private Term resolveTypeForCode(char ch, Term typeTerm) {
        if (!(typeTerm instanceof CompoundTerm)) return null;
        CompoundTerm ct = (CompoundTerm) typeTerm;
        String functor = ct.getFunctor().getName();

        switch (functor) {
            case "digit":
                if (Character.isDigit(ch)) {
                    return new CompoundTerm(new Atom("digit"),
                        Collections.singletonList(new Number(Character.getNumericValue(ch))));
                }
                break;
            case "upper":
                if (Character.isUpperCase(ch)) {
                    return new CompoundTerm(new Atom("upper"),
                        Collections.singletonList(new Number((int) Character.toLowerCase(ch))));
                }
                break;
            case "lower":
                if (Character.isLowerCase(ch)) {
                    return new CompoundTerm(new Atom("lower"),
                        Collections.singletonList(new Number((int) Character.toUpperCase(ch))));
                }
                break;
        }
        return null;
    }

    /**
     * Generate all code-type pairs in ASCII range.
     */
    private boolean generateAllPairs(Variable codeVar, Variable typeVar,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        for (int i = 0; i <= 127; i++) {
            char ch = (char) i;
            // Simple types
            for (String typeName : SIMPLE_TYPES) {
                if (checkSimpleType(ch, typeName)) {
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (codeVar.unify(new Number(i), newBindings) &&
                        typeVar.unify(new Atom(typeName), newBindings)) {
                        solutions.add(newBindings);
                    }
                }
            }
            // Compound types
            if (Character.isDigit(ch)) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                Term digitTerm = new CompoundTerm(new Atom("digit"),
                    Collections.singletonList(new Number(Character.getNumericValue(ch))));
                if (codeVar.unify(new Number(i), newBindings) &&
                    typeVar.unify(digitTerm, newBindings)) {
                    solutions.add(newBindings);
                }
            }
            if (Character.isUpperCase(ch)) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                Term upperTerm = new CompoundTerm(new Atom("upper"),
                    Collections.singletonList(new Number((int) Character.toLowerCase(ch))));
                if (codeVar.unify(new Number(i), newBindings) &&
                    typeVar.unify(upperTerm, newBindings)) {
                    solutions.add(newBindings);
                }
            }
            if (Character.isLowerCase(ch)) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                Term lowerTerm = new CompoundTerm(new Atom("lower"),
                    Collections.singletonList(new Number((int) Character.toUpperCase(ch))));
                if (codeVar.unify(new Number(i), newBindings) &&
                    typeVar.unify(lowerTerm, newBindings)) {
                    solutions.add(newBindings);
                }
            }
        }
        return !solutions.isEmpty();
    }
}
// END_CHANGE: LIM-006
