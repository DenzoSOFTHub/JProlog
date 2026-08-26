package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

// START_CHANGE: ISS-2025-0503 - 4.2 wave C: char_type/2 and code_type/2 as native generators.
/**
 * {@code char_type/2} and {@code code_type/2}: the last eager enumeration of the ISO-core set
 * (§17.6 item 4), and the last two predicates the Reference Manual documented with a
 * "parameterised forms not supported" caveat.
 *
 * <h3>What changed</h3>
 * <ul>
 *   <li>Both are <b>generators</b>. {@code builtin.character.CharType} built every
 *       {@code (char, type)} pair of the ASCII range into a {@code List<Map<String,Term>>} before
 *       the caller saw the first one — 128 x 19 maps for {@code char_type(C, T)}. Here one pair is
 *       produced per redo, so {@code char_type(C, digit), !} costs the walk to {@code '0'}.</li>
 *   <li>The <b>parametric forms work in every mode</b>: {@code digit(Weight)}, {@code upper(Lower)},
 *       {@code lower(Upper)}, {@code to_lower(Lower)} and {@code to_upper(Upper)}. The registry
 *       {@code code_type/2} recognised three of them but could only <em>test</em> them — a goal
 *       like {@code code_type(0'a, lower(U))} succeeded and left {@code U} unbound, because the
 *       eager contract had no way to bind an argument nested inside the type term. A native unifies
 *       the canonical type term for the character with the caller's, so the same goal binds
 *       {@code U = 0'A}, and {@code char_type(X, to_upper('A'))} enumerates the characters whose
 *       uppercase is {@code 'A'}. {@code char_type/2} had none of the parametric forms at all.</li>
 *   <li>Six SWI class names {@code char_type/2} did not know are added — {@code csym},
 *       {@code csymf}, {@code white}, {@code period}, {@code quote}, {@code paren} — and the ten
 *       {@code char_type/2}-only names ({@code xdigit}, {@code newline}, {@code end_of_file},
 *       {@code layout}, {@code meta}, {@code solo}, {@code symbol}, plus the atom forms of
 *       {@code digit}/{@code upper}/{@code lower}) are accepted by {@code code_type/2} too. The two
 *       predicates classify identically; only the <em>shape</em> of the character argument and of a
 *       parametric form's argument differs (an atom for {@code char_type/2}, a code for
 *       {@code code_type/2}), and {@code digit(Weight)}'s weight is an integer in both.</li>
 * </ul>
 *
 * <h3>What did not change</h3>
 * Every classification predicate is the one the two registry classes used, character for character
 * (including the deliberate quirks: {@code space} and {@code white} and {@code layout} are all
 * {@code Character.isWhitespace}; {@code print} is "not an ISO control"; {@code end_of_file} is
 * {@code (char) -1} and therefore never enumerates), and the ENUMERATION ORDER of each predicate
 * keeps its historical prefix — {@code char_type/2} still answers {@code alnum, alpha, ascii, …} and
 * {@code code_type/2} still answers {@code alpha, alnum, space, …} — with the new classes appended.
 * A bound character that is not a one-character atom (or, for {@code code_type/2}, not an integer
 * character code) still fails rather than raising.
 *
 * <h3>Bounds</h3>
 * An unbound character enumerates the ASCII range 0..127 — the range both registry versions used —
 * plus any character named by a bound argument of the type term, so
 * {@code char_type(X, to_lower(é))} can still answer. That keeps the enumeration finite, which is
 * what a generator over an unbounded character set could not be.
 */
final class NativeChars {

    private NativeChars() {}

    static void register(BuiltinTable t) {
        t.register("char_type", 2, new TypeB(true));
        t.register("code_type", 2, new TypeB(false));
    }

    // ------------------------------------------------------------------ the classes

    /** One character class. {@code arity} 1 means a parametric form. */
    private enum K {
        ALNUM("alnum", 0), ALPHA("alpha", 0), ASCII("ascii", 0), CNTRL("cntrl", 0),
        DIGIT("digit", 0), GRAPH("graph", 0), LOWER("lower", 0), PRINT("print", 0),
        PUNCT("punct", 0), SPACE("space", 0), UPPER("upper", 0), XDIGIT("xdigit", 0),
        NEWLINE("newline", 0), END_OF_FILE("end_of_file", 0), END_OF_LINE("end_of_line", 0),
        LAYOUT("layout", 0), META("meta", 0), SOLO("solo", 0), SYMBOL("symbol", 0),
        CSYM("csym", 0), CSYMF("csymf", 0), WHITE("white", 0),
        PERIOD("period", 0), QUOTE("quote", 0), PAREN("paren", 0),
        P_DIGIT("digit", 1), P_UPPER("upper", 1), P_LOWER("lower", 1),
        P_TO_LOWER("to_lower", 1), P_TO_UPPER("to_upper", 1);

        final String name;
        final int arity;
        K(String name, int arity) { this.name = name; this.arity = arity; }
    }

    /** {@code char_type/2}'s enumeration order: the historical one, then the new classes. */
    private static final K[] CHAR_ORDER = {
        K.ALNUM, K.ALPHA, K.ASCII, K.CNTRL, K.DIGIT, K.GRAPH, K.LOWER, K.PRINT, K.PUNCT,
        K.SPACE, K.UPPER, K.XDIGIT, K.NEWLINE, K.END_OF_FILE, K.END_OF_LINE, K.LAYOUT,
        K.META, K.SOLO, K.SYMBOL,
        K.CSYM, K.CSYMF, K.WHITE, K.PERIOD, K.QUOTE, K.PAREN,
        K.P_DIGIT, K.P_UPPER, K.P_LOWER, K.P_TO_LOWER, K.P_TO_UPPER
    };

    /** {@code code_type/2}'s enumeration order: the historical one, then the new classes. */
    private static final K[] CODE_ORDER = {
        K.ALPHA, K.ALNUM, K.SPACE, K.WHITE, K.ASCII, K.END_OF_LINE, K.CSYM, K.CSYMF,
        K.PUNCT, K.GRAPH, K.PRINT, K.CNTRL,
        K.P_DIGIT, K.P_UPPER, K.P_LOWER,
        K.DIGIT, K.UPPER, K.LOWER, K.XDIGIT, K.NEWLINE, K.END_OF_FILE, K.LAYOUT,
        K.META, K.SOLO, K.SYMBOL, K.PERIOD, K.QUOTE, K.PAREN,
        K.P_TO_LOWER, K.P_TO_UPPER
    };

    private static final String PUNCT_CHARS = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
    private static final String META_CHARS = "\\^";
    private static final String SOLO_CHARS = "!();[]{}|";
    private static final String SYMBOL_CHARS = "#$&*+-./:<=>?@^~";
    private static final String PERIOD_CHARS = ".!?";
    private static final String QUOTE_CHARS = "\"'`";
    private static final String PAREN_CHARS = "()";

    /** Does {@code ch} belong to class {@code k}? (A parametric class tests its base condition.) */
    private static boolean holds(char ch, K k) {
        switch (k) {
            case ALNUM:       return Character.isLetterOrDigit(ch);
            case ALPHA:       return Character.isLetter(ch);
            case ASCII:       return ch <= 127;
            case CNTRL:       return Character.isISOControl(ch);
            case DIGIT:       return Character.isDigit(ch);
            case GRAPH:       return !Character.isWhitespace(ch) && !Character.isISOControl(ch) && ch != ' ';
            case LOWER:       return Character.isLowerCase(ch);
            case PRINT:       return !Character.isISOControl(ch);
            case PUNCT:       return PUNCT_CHARS.indexOf(ch) >= 0;
            case SPACE:       return Character.isWhitespace(ch);
            case UPPER:       return Character.isUpperCase(ch);
            case XDIGIT:      return Character.isDigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
            case NEWLINE:     return ch == '\n';
            case END_OF_FILE: return ch == (char) -1;
            case END_OF_LINE: return ch == '\n' || ch == '\r';
            case LAYOUT:      return Character.isWhitespace(ch);
            case META:        return META_CHARS.indexOf(ch) >= 0;
            case SOLO:        return SOLO_CHARS.indexOf(ch) >= 0;
            case SYMBOL:      return SYMBOL_CHARS.indexOf(ch) >= 0;
            case CSYM:        return Character.isLetterOrDigit(ch) || ch == '_';
            case CSYMF:       return Character.isLetter(ch) || ch == '_';
            case WHITE:       return Character.isWhitespace(ch);
            case PERIOD:      return PERIOD_CHARS.indexOf(ch) >= 0;
            case QUOTE:       return QUOTE_CHARS.indexOf(ch) >= 0;
            case PAREN:       return PAREN_CHARS.indexOf(ch) >= 0;
            case P_DIGIT:     return Character.isDigit(ch);
            case P_UPPER:     return Character.isUpperCase(ch);
            case P_LOWER:     return Character.isLowerCase(ch);
            case P_TO_LOWER:  return true;
            case P_TO_UPPER:  return true;
            default:          return false;
        }
    }

    /** A character as {@code char_type/2} reports it (a one-character atom) or {@code code_type/2}
     *  does (an integer code). */
    private static Term charTerm(char ch, boolean chars) {
        return chars ? (Term) new Atom(String.valueOf(ch)) : (Term) Number.valueOf(ch);
    }

    /**
     * The canonical type term of {@code ch} in class {@code k}, or null when {@code ch} is not in
     * it. Unifying this with the caller's type term is what makes every mode of a parametric form
     * work — testing a bound argument and binding an unbound one are the same operation.
     */
    private static Term typeTerm(char ch, K k, boolean chars) {
        if (!holds(ch, k)) return null;
        if (k.arity == 0) return new Atom(k.name);
        Term arg;
        switch (k) {
            case P_DIGIT:    arg = Number.valueOf(Character.digit(ch, 10)); break;
            case P_UPPER:    arg = charTerm(Character.toLowerCase(ch), chars); break;
            case P_LOWER:    arg = charTerm(Character.toUpperCase(ch), chars); break;
            case P_TO_LOWER: arg = charTerm(Character.toLowerCase(ch), chars); break;
            case P_TO_UPPER: arg = charTerm(Character.toUpperCase(ch), chars); break;
            default:         return null;
        }
        return new CompoundTerm(new Atom(k.name), Collections.singletonList(arg));
    }

    // ------------------------------------------------------------------ the built-in

    private static final class TypeB implements Builtin {

        private final boolean chars;

        TypeB(boolean chars) { this.chars = chars; }

        @Override
        public Outcome call(Machine m, final Term[] args) {
            Term cT = m.deref(args[0]);
            Term tT = m.deref(args[1]);

            // --- which classes can possibly match?
            final K[] kinds = kindsFor(tT);
            if (kinds == null || kinds.length == 0) return Outcome.FAILURE;

            // --- which characters can possibly match?
            final int[] cands;
            if (cT instanceof Variable) {
                cands = candidateChars(tT);
            } else {
                int ch = boundChar(cT);
                if (ch < 0) return Outcome.FAILURE;
                cands = new int[] { ch };
            }
            if (cands.length == 0) return Outcome.FAILURE;

            final boolean asChars = chars;
            final int[] ci = {0};
            final int[] ki = {0};
            Generator gen = new Generator() {
                @Override
                public boolean next(Machine mm) {
                    ResourceGuard g = mm.guard();
                    int n = 0;
                    while (ci[0] < cands.length) {
                        if ((++n & 0x3FF) == 0 && g != null) g.step();
                        char ch = (char) cands[ci[0]];
                        K k = kinds[ki[0]];
                        advance(ci, ki, kinds.length);
                        Term tt = typeTerm(ch, k, asChars);
                        if (tt == null) continue;
                        if (ci[0] >= cands.length) mm.lastSolution();
                        // ONE mark/undo extent around the pair (invariant 12): unify binds as it
                        // walks, so a half-bound failure would poison every later alternative.
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(args[0], charTerm(ch, asChars), b)
                              && Unify.unify(args[1], tt, b);
                            if (!ok) b.undo(mark);
                        } finally {
                            b.forceTrail--;
                        }
                        if (ok) return true;
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }

        /** Step the (character, class) cursor: class inner, character outer. */
        private static void advance(int[] ci, int[] ki, int nk) {
            if (++ki[0] >= nk) { ki[0] = 0; ci[0]++; }
        }

        /** The classes a bound type term can be; every class in order when it is unbound. */
        private K[] kindsFor(Term tT) {
            if (tT instanceof Variable) return chars ? CHAR_ORDER : CODE_ORDER;
            String name;
            int arity;
            if (tT instanceof Atom) { name = ((Atom) tT).getName(); arity = 0; }
            else if (tT instanceof CompoundTerm) {
                name = ((CompoundTerm) tT).getName();
                arity = ((CompoundTerm) tT).getArguments().size();
            } else {
                return null;
            }
            for (K k : K.values()) {
                if (k.arity == arity && k.name.equals(name)) return new K[] { k };
            }
            return null;                                   // an unknown class simply fails
        }

        /** The bound character/code argument, or -1 when it is not one this predicate accepts. */
        private int boundChar(Term cT) {
            if (chars) {
                if (!(cT instanceof Atom)) return -1;
                String s = ((Atom) cT).getName();
                if (s.length() != 1) return -1;
                char c = s.charAt(0);
                return c == 0 ? -1 : c;                    // the registry version's own quirk
            }
            if (!(cT instanceof Number)) return -1;
            Number n = (Number) cT;
            if (!n.isInteger()) return -1;
            long v = n.longValue();
            return (v < 0 || v > Character.MAX_VALUE) ? -1 : (int) v;
        }

        /**
         * The characters an unbound first argument enumerates: the ASCII range, plus any character
         * a bound argument of the type term names (so {@code char_type(X, to_upper('É'))} can
         * answer without enumerating the whole of Unicode).
         */
        private int[] candidateChars(Term tT) {
            List<Integer> extra = null;
            if (tT instanceof CompoundTerm && ((CompoundTerm) tT).getArguments().size() == 1) {
                Term arg = Unify.deref(((CompoundTerm) tT).getArguments().get(0));
                int c = -1;
                if (arg instanceof Atom && ((Atom) arg).getName().length() == 1) {
                    c = ((Atom) arg).getName().charAt(0);
                } else if (arg instanceof Number && ((Number) arg).isInteger()) {
                    long v = ((Number) arg).longValue();
                    if (v >= 0 && v <= Character.MAX_VALUE) c = (int) v;
                }
                if (c > 127) {
                    extra = new ArrayList<Integer>(2);
                    extra.add(Integer.valueOf(c));
                    int u = Character.toUpperCase((char) c);
                    int l = Character.toLowerCase((char) c);
                    if (u > 127 && u != c) extra.add(Integer.valueOf(u));
                    if (l > 127 && l != c && l != u) extra.add(Integer.valueOf(l));
                }
            }
            int n = 128 + (extra == null ? 0 : extra.size());
            int[] out = new int[n];
            for (int i = 0; i < 128; i++) out[i] = i;
            for (int i = 128; i < n; i++) out[i] = extra.get(i - 128).intValue();
            return out;
        }
    }

    static {
        // keep the two orders complete: every class must appear in both enumerations
        assert CHAR_ORDER.length == K.values().length : "CHAR_ORDER is incomplete";
        assert CODE_ORDER.length == K.values().length : "CODE_ORDER is incomplete";
        assert new java.util.HashSet<K>(Arrays.asList(CHAR_ORDER)).size() == K.values().length;
        assert new java.util.HashSet<K>(Arrays.asList(CODE_ORDER)).size() == K.values().length;
    }
}
// END_CHANGE: ISS-2025-0503
