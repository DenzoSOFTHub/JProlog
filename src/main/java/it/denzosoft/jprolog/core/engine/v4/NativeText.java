package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.conversion.AtomNumber;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.string.TextTerm;
import it.denzosoft.jprolog.core.engine.ControlFlow;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

// START_CHANGE: ISS-2025-0497 - 4.1 wave B: the atom / string / character / conversion families
// and the three eager list built-ins on the v4 SPI (design B.5).
/**
 * The text families as v4 natives: {@code atom_*}, {@code string_*}, {@code number_*},
 * {@code char_code/2}, {@code upcase_atom/2}, {@code downcase_atom/2},
 * {@code atomic_list_concat/2,3}, {@code split_string/4}, {@code term_to_atom/2} and
 * {@code term_string/2}, plus the three {@code list} built-ins that were still bridged
 * ({@code keysort/2}, {@code delete/3}, {@code flatten/2}).
 *
 * <h3>What changes and what does not</h3>
 * The mode analysis and every ISO error term of the registry versions are reproduced exactly —
 * including the non-ISO evaluation errors a few of them raise ({@code upcase_atom/2} on a non-atom,
 * {@code split_string/4} on a non-string, {@code string_length/2} on an unbound first argument),
 * because programs catch them by shape. What changes is the machinery: the goal is no longer
 * {@link Unify#resolve}d, no {@code Map<String,Term>} is built, and the two split modes
 * ({@code atom_concat(-,-,+)} and {@code string_concat(-,-,+)}) are lazy {@link Generator}s
 * instead of a pre-built list of n+1 solution maps — so {@code once(atom_concat(X, Y, Long))}
 * stops at the first split.
 *
 * <p>A dereferenced cell is not a resolved term: a mode test that the registry version wrote as
 * {@code t.isGround()} is {@link Unify#isGround} here, and every list walk derefs as it goes.
 */
final class NativeText {

    private NativeText() {}

    private static final Atom NIL = new Atom("[]");
    private static final Atom DOT = new Atom(".");

    static void register(BuiltinTable t) {
        // START_CHANGE: ISS-2025-0718 - wave Q2.8: documented but missing. Pure path-text
        // operations (no file-system access), so they are natives and stay in safe mode.
        t.register("file_base_name", 2, new PathPartB(0));
        t.register("file_directory_name", 2, new PathPartB(1));
        t.register("file_extension", 2, new PathPartB(2));
        // END_CHANGE: ISS-2025-0718
        t.register("atom_length", 2, new AtomLengthB());
        t.register("atom_concat", 3, new ConcatB(true));
        t.register("string_concat", 3, new ConcatB(false));
        t.register("atom_chars", 2, new AtomCharsB(true));
        t.register("atom_codes", 2, new AtomCharsB(false));
        t.register("char_code", 2, new CharCodeB());
        t.register("upcase_atom", 2, new CaseB(true));
        t.register("downcase_atom", 2, new CaseB(false));
        // START_CHANGE: ISS-2025-0598 - P4.7: string_upper/2, string_lower/2 (SWI)
        t.register("string_upper", 2, new StringCaseB(true));
        t.register("string_lower", 2, new StringCaseB(false));
        // END_CHANGE: ISS-2025-0598
        t.register("number_chars", 2, new NumberTextB(true));
        t.register("number_codes", 2, new NumberTextB(false));
        t.register("atom_number", 2, new AtomNumberB());
        t.register("atom_string", 2, new AtomStringB());
        t.register("number_string", 2, new NumberStringB());
        t.register("string_to_atom", 2, new StringToAtomB());
        t.register("string_chars", 2, new StringCharsB());
        t.register("string_codes", 2, new StringCodesB());
        t.register("string_length", 2, new StringLengthB());
        t.register("string_code", 3, new StringCodeB());
        t.register("split_string", 4, new SplitStringB());
        t.register("atomic_list_concat", 2, new AtomicListConcatB(2));
        t.register("atomic_list_concat", 3, new AtomicListConcatB(3));
        t.register("term_to_atom", 2, new TermTextB(true));
        // START_CHANGE: ISS-2025-0497 - term_string/2 is NEW (the SWI string twin of term_to_atom/2)
        t.register("term_string", 2, new TermTextB(false));
        // END_CHANGE: ISS-2025-0497
        t.register("keysort", 2, new KeySortB());
        t.register("delete", 3, new DeleteB());
        t.register("flatten", 2, new FlattenB());
    }

    // ------------------------------------------------------------------ helpers

    static boolean ground(Machine m, Term t) { return Unify.isGround(t, m.guard()); }

    static Term listOf(List<Term> elems) { return NativeLibrary.listOf(elems, NIL); }

    static Term charList(String s) {
        List<Term> out = new ArrayList<Term>();
        int i = 0;
        while (i < s.length()) {
            int cp = s.codePointAt(i);
            out.add(new Atom(new String(Character.toChars(cp))));
            i += Character.charCount(cp);
        }
        return listOf(out);
    }

    static Term codeList(String s) {
        List<Term> out = new ArrayList<Term>();
        int i = 0;
        while (i < s.length()) {
            int cp = s.codePointAt(i);
            out.add(Number.valueOf((long) cp));
            i += Character.charCount(cp);
        }
        return listOf(out);
    }

    /** One code point exactly. */
    static boolean isOneChar(String s) {
        return !s.isEmpty() && Character.charCount(s.codePointAt(0)) == s.length();
    }

    /**
     * Text of a proper list of one-character atoms ({@code chars}) or character codes
     * ({@code codes}), or of a {@link PrologString}. Returns null for an unbound argument, an
     * unbound element or a partial list; raises the ISO error of {@code ctx} for a malformed one.
     */
    static String textOfList(Machine m, Term list, boolean chars, String ctx) {
        Term cur = m.deref(list);
        if (cur instanceof PrologString) return ((PrologString) cur).getStringValue();
        if (cur instanceof Variable) return null;
        StringBuilder sb = new StringBuilder();
        int n = 0;
        while (cur instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) cur;
            if (!".".equals(c.getName()) || c.getArguments().size() != 2) {
                throw new PrologException(ISOErrorTerms.typeError("list", m.resolve(list), ctx));
            }
            Term e = m.deref(c.getArguments().get(0));
            if (e instanceof Variable) return null;
            if (chars) {
                if (!(e instanceof Atom) || !isOneChar(((Atom) e).getName())) {
                    throw new PrologException(ISOErrorTerms.typeError("character", e, ctx));
                }
                sb.append(((Atom) e).getName());
            } else {
                if (!(e instanceof Number) || !((Number) e).isInteger()) {
                    throw new PrologException(ISOErrorTerms.representationError("character_code", ctx));
                }
                long code = ((Number) e).longValue();
                if (code < 0 || code > 0x10FFFF) {
                    throw new PrologException(ISOErrorTerms.representationError("character_code", ctx));
                }
                sb.appendCodePoint((int) code);
            }
            cur = m.deref(c.getArguments().get(1));
            if ((++n & 0x3FF) == 0) m.guard().step();
        }
        if (cur instanceof Variable) return null;
        if (NativeLibrary.isNil(cur)) return sb.toString();
        throw new PrologException(ISOErrorTerms.typeError("list", m.resolve(list), ctx));
    }

    // ------------------------------------------------------------------ atom_length/2

    private static final class AtomLengthB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term a = m.deref(args[0]);
            Term l = m.deref(args[1]);
            if (a instanceof Variable || !ground(m, a)) {
                throw new PrologException(ISOErrorTerms.instantiationError("atom_length/2"));
            }
            String text = TextTerm.textOf(a);
            if (text == null) {
                throw new PrologException(ISOErrorTerms.typeError("atom", m.resolve(a), "atom_length/2"));
            }
            if (!(l instanceof Variable)) {
                if (!(l instanceof Number) || !((Number) l).isInteger()) {
                    throw new PrologException(ISOErrorTerms.typeError("integer", l, "atom_length/2"));
                }
                if (((Number) l).longValue() < 0) {
                    throw new PrologException(
                        ISOErrorTerms.domainError("not_less_than_zero", l, "atom_length/2"));
                }
            }
            long len = text.codePointCount(0, text.length());
            return m.unify(args[1], Number.valueOf(len)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ atom_concat/3, string_concat/3

    /**
     * {@code atom_concat(?A, ?B, ?C)} and {@code string_concat/3}. The {@code (-,-,+)} split mode
     * is a lazy generator; every other mode is deterministic.
     */
    private static final class ConcatB implements Builtin {
        private final boolean atoms;
        ConcatB(boolean atoms) { this.atoms = atoms; }

        @Override
        public Outcome call(Machine m, final Term[] args) {
            final String ctx = atoms ? "atom_concat/3" : "string_concat/3";
            Term a = m.deref(args[0]), b = m.deref(args[1]), c = m.deref(args[2]);
            boolean ga, gb, gc;
            if (atoms) {
                ga = ground(m, a); gb = ground(m, b); gc = ground(m, c);
            } else {                                   // string_concat/3 tests var-ness, not ground
                ga = !(a instanceof Variable); gb = !(b instanceof Variable); gc = !(c instanceof Variable);
            }
            if (ga && gb && !gc) {
                String s = text(m, a, ctx) + text(m, b, ctx);
                return m.unify(args[2], make(s)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (ga && !gb && gc) {
                String p = text(m, a, ctx), full = text(m, c, ctx);
                if (p == null || full == null || !full.startsWith(p)) return Outcome.FAILURE;
                return m.unify(args[1], make(full.substring(p.length()))) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!ga && gb && gc) {
                String s = text(m, b, ctx), full = text(m, c, ctx);
                if (s == null || full == null || !full.endsWith(s)) return Outcome.FAILURE;
                return m.unify(args[0], make(full.substring(0, full.length() - s.length())))
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!ga && !gb && gc) {
                final String full = text(m, c, ctx);
                if (full == null) return Outcome.FAILURE;
                final int max = full.length();
                final int[] i = {0};
                Generator gen = new Generator() {
                    @Override public boolean next(Machine mm) {
                        while (i[0] <= max) {
                            int k = i[0];
                            // START_CHANGE: ISS-2025-0599 - split at code-point boundaries only
                            i[0] = (k < max) ? k + Character.charCount(full.codePointAt(k)) : k + 1;
                            // END_CHANGE: ISS-2025-0599
                            if (i[0] > max) mm.lastSolution();
                            Bindings bb = mm.bindings();
                            int mark = bb.mark();
                            bb.forceTrail++;
                            boolean ok;
                            try {
                                ok = Unify.unify(args[0], make(full.substring(0, k)), bb)
                                  && Unify.unify(args[1], make(full.substring(k)), bb);
                                if (!ok) bb.undo(mark);       // ISS-2025-0448 ordering
                            } finally {
                                bb.forceTrail--;
                            }
                            if (ok) return true;
                        }
                        return false;
                    }
                };
                return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
            }
            if (ga && gb && gc) {
                String s1 = text(m, a, ctx), s2 = text(m, b, ctx), s3 = text(m, c, ctx);
                if (s1 == null || s2 == null || s3 == null) return Outcome.FAILURE;
                return (s1 + s2).equals(s3) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            // START_CHANGE: ISS-2025-0506 - atom_concat/3 raises instantiation_error with nothing
            // bound.
            // START_CHANGE: ISS-2025-0596 - P4.7 (decision §8): so does string_concat/3 (SWI);
            // the ISS-2025-0188 "fail gracefully" decision is reversed.
            throw new PrologException(ISOErrorTerms.instantiationError(ctx));
            // END_CHANGE: ISS-2025-0596
            // END_CHANGE: ISS-2025-0506
        }

        private Term make(String s) { return atoms ? (Term) new Atom(s) : (Term) new PrologString(s); }

        /** atom_concat/3 raises type_error(atom, C); string_concat/3 takes any text (SWI). */
        private String text(Machine m, Term t, String ctx) {
            if (atoms) {
                String s = TextTerm.textOf(t);
                if (s == null) throw new PrologException(ISOErrorTerms.typeError("atom", m.resolve(t), ctx));
                return s;
            }
            // START_CHANGE: ISS-2025-0596 - P4.7: string_concat(1, 2, S) gives "12" (the old
            // path answered "nullnull"); a code/char list is text too; anything else raises.
            String s = atomicText(t);
            if (s == null && NativeLibrary.isCons(t)) {
                s = textOfList(m, t, m.deref(NativeLibrary.head(t)) instanceof Atom, ctx);
            }
            if (s == null) throw Errors.type("atomic", m.resolve(t), ctx);
            return s;
            // END_CHANGE: ISS-2025-0596
        }
    }

    // ------------------------------------------------------------------ atom_chars/2, atom_codes/2

    private static final class AtomCharsB implements Builtin {
        private final boolean chars;
        AtomCharsB(boolean chars) { this.chars = chars; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = chars ? "atom_chars/2" : "atom_codes/2";
            Term a = m.deref(args[0]);
            Term l = m.deref(args[1]);
            String atomText = TextTerm.textOf(a);
            if (atomText == null && a instanceof Number) atomText = AtomNumber.formatNumberExact((Number) a);
            if (atomText != null) {
                if (l instanceof PrologString) {
                    return atomText.equals(((PrologString) l).getStringValue())
                        ? Outcome.SUCCESS : Outcome.FAILURE;
                }
                m.guard().charge(atomText.length());     // ISS-2025-0624: O(length) work
                Term built = chars ? charList(atomText) : codeList(atomText);
                return m.unify(args[1], built) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!(a instanceof Variable)) {
                throw new PrologException(ISOErrorTerms.typeError("atom", m.resolve(a), ctx));
            }
            String text = textOfList(m, l, chars, ctx);
            if (text == null) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
            return m.unify(args[0], new Atom(text)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ char_code/2

    private static final class CharCodeB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term ch = m.deref(args[0]);
            Term co = m.deref(args[1]);
            if (ch instanceof Variable && co instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("char_code/2"));
            }
            if (ch instanceof Variable) {
                int code = requireCode(co);
                return m.unify(args[0], new Atom(new String(Character.toChars(code))))
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            int cp = codePointOf(ch);
            if (cp < 0) throw new PrologException(ISOErrorTerms.typeError("character", m.resolve(ch), "char_code/2"));
            if (co instanceof Variable) {
                return m.unify(args[1], Number.valueOf((long) cp)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            return (cp == requireCode(co)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        private int requireCode(Term t) {
            if (!(t instanceof Number) || !((Number) t).isInteger()) {
                throw new PrologException(ISOErrorTerms.typeError("integer", t, "char_code/2"));
            }
            long code = ((Number) t).longValue();
            if (code < 0 || code > 0x10FFFF) {
                throw new PrologException(ISOErrorTerms.representationError("character_code", "char_code/2"));
            }
            return (int) code;
        }

        private int codePointOf(Term t) {
            if (!(t instanceof Atom)) return -1;
            String s = ((Atom) t).getName();
            if (s.isEmpty()) return -1;
            int cp = s.codePointAt(0);
            return (Character.charCount(cp) != s.length()) ? -1 : cp;
        }
    }

    // ------------------------------------------------------------------ upcase_atom/2, downcase_atom/2

    // START_CHANGE: ISS-2025-0598 - string_upper(+Text, -String) / string_lower(+Text, -String)
    private static final class StringCaseB implements Builtin {
        private final boolean up;
        StringCaseB(boolean up) { this.up = up; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ind = up ? "string_upper/2" : "string_lower/2";
            Term in = m.deref(args[0]);
            if (in instanceof Variable) throw Errors.instantiation(ind);
            String s = atomicText(in);
            if (s == null && NativeLibrary.isCons(in)) {
                s = textOfList(m, in, m.deref(NativeLibrary.head(in)) instanceof Atom, ind);
            }
            if (s == null) throw Errors.type("string", m.resolve(in), ind);
            String r = up ? s.toUpperCase(java.util.Locale.ROOT) : s.toLowerCase(java.util.Locale.ROOT);
            return m.unify(args[1], new PrologString(r)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0598

    private static final class CaseB implements Builtin {
        private final boolean up;
        CaseB(boolean up) { this.up = up; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ind = up ? "upcase_atom/2" : "downcase_atom/2";
            Term in = m.deref(args[0]);
            // START_CHANGE: ISS-2025-0506 - instantiation_error / type_error(atom, A), not one
            // message atom for both.
            if (in instanceof Variable) throw Errors.instantiation(ind);
            if (!(in instanceof Atom)) throw Errors.type("atom", m.resolve(in), ind);
            // END_CHANGE: ISS-2025-0506
            String s = ((Atom) in).getName();
            String r = up ? s.toUpperCase(java.util.Locale.ROOT) : s.toLowerCase(java.util.Locale.ROOT);
            return m.unify(args[1], new Atom(r)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ number_chars/2, number_codes/2

    private static final class NumberTextB implements Builtin {
        private final boolean chars;
        NumberTextB(boolean chars) { this.chars = chars; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = chars ? "number_chars/2" : "number_codes/2";
            Term n = m.deref(args[0]);
            if (!(n instanceof Variable) && !(n instanceof Number)) {
                throw new PrologException(ISOErrorTerms.typeError("number", m.resolve(n), ctx));
            }
            String text = textOfList(m, args[1], chars, ctx);
            if (text != null) {
                Number v = AtomNumber.parseNumberToken(text);
                if (v == null) throw new PrologException(ISOErrorTerms.syntaxError("illegal_number", ctx));
                return m.unify(args[0], v) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (n instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
            String s = AtomNumber.formatNumberExact((Number) n);
            return m.unify(args[1], chars ? charList(s) : codeList(s)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ atom_number/2

    private static final class AtomNumberB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term a = m.deref(args[0]), n = m.deref(args[1]);
            boolean ga = ground(m, a), gn = ground(m, n);
            // START_CHANGE: ISS-2025-0506 - ISO 7.12.2: neither argument bound is
            // instantiation_error; a bound first argument that is not text is type_error(atom, A).
            if (!ga && !gn) throw Errors.instantiation("atom_number/2");
            if (ga && !(a instanceof Atom) && !(a instanceof PrologString)) {
                throw Errors.type("atom", m.resolve(a), "atom_number/2");
            }
            // END_CHANGE: ISS-2025-0506
            if (ga && !gn) {
                if (!(a instanceof Atom)) return Outcome.FAILURE;
                Number v = AtomNumber.parsePrologNumber(((Atom) a).getName());
                if (v == null) return Outcome.FAILURE;
                return m.unify(args[1], v) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!ga && gn) {
                if (!(n instanceof Number)) return Outcome.FAILURE;
                return m.unify(args[0], new Atom(AtomNumber.formatNumberExact((Number) n)))
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (ga && gn) {
                if (!(a instanceof Atom) || !(n instanceof Number)) return Outcome.FAILURE;
                Number v = AtomNumber.parsePrologNumber(((Atom) a).getName());
                return (v != null && v.equals(n)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            return Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ atom_string/2

    // START_CHANGE: ISS-2025-0596 - P4.7: atom_string/2 converts any atomic text both ways
    // (SWI: atom_string(42, S) gives S = "42", atom_string(A, 42) gives A = '42').
    private static final class AtomStringB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term a = m.deref(args[0]), s = m.deref(args[1]);
            if (!(a instanceof Variable)) {
                String t = atomicText(a);
                if (t == null) throw Errors.type("atomic", m.resolve(a), "atom_string/2");
                if (s instanceof Variable) {
                    return m.unify(args[1], new PrologString(t)) ? Outcome.SUCCESS : Outcome.FAILURE;
                }
                String u = atomicText(s);
                if (u == null) throw Errors.type("string", m.resolve(s), "atom_string/2");
                return t.equals(u) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (s instanceof Variable) throw Errors.instantiation("atom_string/2");
            String u = atomicText(s);
            if (u == null) throw Errors.type("string", m.resolve(s), "atom_string/2");
            return m.unify(args[0], new Atom(u)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0596

    // ------------------------------------------------------------------ number_string/2

    private static final class NumberStringB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term n = m.deref(args[0]), s = m.deref(args[1]);
            boolean gn = ground(m, n), gs = ground(m, s);
            if (gn && !gs) {
                if (!(n instanceof Number)) return Outcome.FAILURE;
                return m.unify(args[1], new PrologString(AtomNumber.formatNumberExact((Number) n)))
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!gn && gs) {
                if (!(s instanceof PrologString)) return Outcome.FAILURE;
                Number v = AtomNumber.parsePrologNumber(((PrologString) s).getStringValue());
                if (v == null) return Outcome.FAILURE;
                return m.unify(args[0], v) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (gn && gs) {
                if (!(n instanceof Number) || !(s instanceof PrologString)) return Outcome.FAILURE;
                Number v = AtomNumber.parsePrologNumber(((PrologString) s).getStringValue());
                return (v != null && v.equals(n)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            // START_CHANGE: ISS-2025-0506 - neither argument bound is instantiation_error.
            throw Errors.instantiation("number_string/2");
            // END_CHANGE: ISS-2025-0506
        }
    }

    // ------------------------------------------------------------------ string_to_atom/2

    private static final class StringToAtomB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term s = m.deref(args[0]), a = m.deref(args[1]);
            boolean gs = ground(m, s), ga = ground(m, a);
            if (gs && !ga) {
                String v = (s instanceof PrologString) ? ((PrologString) s).getStringValue()
                         : (s instanceof Atom) ? ((Atom) s).getName() : m.resolve(s).toString();
                return m.unify(args[1], new Atom(v)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!gs && ga) {
                String v = (a instanceof Atom) ? ((Atom) a).getName() : m.resolve(a).toString();
                return m.unify(args[0], new PrologString(v)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (gs && ga) {
                String s1 = (s instanceof Atom) ? ((Atom) s).getName()
                          : (s instanceof PrologString) ? ((PrologString) s).getStringValue()
                          : m.resolve(s).toString();
                String s2 = (a instanceof Atom) ? ((Atom) a).getName() : m.resolve(a).toString();
                return s1.equals(s2) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            // ISS-2025-0797 - 4.6 wave Q7: neither side bound raises (SWI), it failed silently
            throw Errors.instantiation("string_to_atom/2");
        }
    }

    // ------------------------------------------------------------------ string_chars/2, string_codes/2

    private static final class StringCharsB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term s = m.deref(args[0]), l = m.deref(args[1]);
            boolean gs = ground(m, s), gl = ground(m, l);
            if (gs && !gl) {
                String v = TextTerm.textOf(s);
                if (v == null) return Outcome.FAILURE;
                return m.unify(args[1], charList(v)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!gs && gl) {
                String v = charsTextLoose(m, l);
                if (v == null) return Outcome.FAILURE;
                return m.unify(args[0], new PrologString(v)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (gs && gl) {
                String v = TextTerm.textOf(s);
                if (v == null) return Outcome.FAILURE;
                String w = charsTextLoose(m, l);
                return (w != null && v.equals(w)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            // START_CHANGE: ISS-2025-0506 - neither argument bound is instantiation_error.
            throw Errors.instantiation("string_chars/2");
            // END_CHANGE: ISS-2025-0506
        }

        /** string_chars/2 FAILS on a malformed char list where atom_chars/2 raises. */
        private String charsTextLoose(Machine m, Term list) {
            StringBuilder sb = new StringBuilder();
            Term cur = m.deref(list);
            int n = 0;
            while (cur instanceof CompoundTerm) {
                CompoundTerm c = (CompoundTerm) cur;
                if (!".".equals(c.getName()) || c.getArguments().size() != 2) break;
                Term e = m.deref(c.getArguments().get(0));
                if (!(e instanceof Atom)) return null;
                String cs = ((Atom) e).getName();
                if (cs.length() != 1
                        && !(cs.length() == 2 && Character.isHighSurrogate(cs.charAt(0)))) return null;
                sb.append(cs);
                cur = m.deref(c.getArguments().get(1));
                if ((++n & 0x3FF) == 0) m.guard().step();
            }
            return NativeLibrary.isNil(cur) ? sb.toString() : null;
        }
    }

    private static final class StringCodesB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term s = m.deref(args[0]), l = m.deref(args[1]);
            if (s instanceof PrologString && ground(m, s)) {
                return m.unify(args[1], codeList(((PrologString) s).getStringValue()))
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (ground(m, l) && isProperList(m, l)) {
                StringBuilder sb = new StringBuilder();
                Term cur = l;
                int n = 0;
                while (NativeLibrary.isCons(cur)) {
                    Term e = m.deref(NativeLibrary.head(cur));
                    // START_CHANGE: ISS-2025-0506
                    if (!(e instanceof Number)) {
                        throw Errors.type("integer", m.resolve(e), "string_codes/2");
                    }
                    double v = ((Number) e).getValue();
                    if (v != Math.floor(v) || v < 0 || v > 1114111) {
                        throw Errors.representation("character_code", "string_codes/2");
                    }
                    // END_CHANGE: ISS-2025-0506
                    sb.append(Character.toChars((int) v));
                    cur = m.deref(NativeLibrary.tail(cur));
                    if ((++n & 0x3FF) == 0) m.guard().step();
                }
                return m.unify(args[0], new PrologString(sb.toString())) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            // START_CHANGE: ISS-2025-0506
            if (s instanceof Variable && l instanceof Variable) {
                throw Errors.instantiation("string_codes/2");
            }
            // END_CHANGE: ISS-2025-0506
            return Outcome.FAILURE;
        }

        private boolean isProperList(Machine m, Term t) {
            Term cur = m.deref(t);
            int n = 0;
            while (cur instanceof CompoundTerm) {
                CompoundTerm c = (CompoundTerm) cur;
                if (!".".equals(c.getName()) || c.getArguments().size() != 2) return false;
                cur = m.deref(c.getArguments().get(1));
                if ((++n & 0x3FF) == 0) m.guard().step();
            }
            return NativeLibrary.isNil(cur);
        }
    }

    private static final class StringLengthB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term s = m.deref(args[0]);
            // START_CHANGE: ISS-2025-0506
            if (s instanceof Variable) throw Errors.instantiation("string_length/2");
            // END_CHANGE: ISS-2025-0506
            // START_CHANGE: ISS-2025-0596 - P4.7: any text (string_length(123, 3)); a code or
            // char list counts too, anything else is type_error(string, S)
            String v = atomicText(s);
            if (v == null && NativeLibrary.isCons(s)) {
                v = textOfList(m, s, m.deref(NativeLibrary.head(s)) instanceof Atom, "string_length/2");
            }
            if (v == null) throw Errors.type("string", m.resolve(s), "string_length/2");
            // END_CHANGE: ISS-2025-0596
            return m.unify(args[1], Number.valueOf((long) v.codePointCount(0, v.length())))
                ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class StringCodeB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term i = m.deref(args[0]), s = m.deref(args[1]);
            // START_CHANGE: ISS-2025-0506
            if (i instanceof Variable || s instanceof Variable) {
                throw Errors.instantiation("string_code/3");
            }
            if (!(i instanceof Number) || !((Number) i).isInteger()) {
                throw Errors.type("integer", m.resolve(i), "string_code/3");
            }
            // END_CHANGE: ISS-2025-0506
            String str = TextTerm.textOf(s);
            if (str == null) return Outcome.FAILURE;
            int idx = ((Number) i).getValue().intValue();
            if (idx < 1 || idx > str.length()) return Outcome.FAILURE;
            return m.unify(args[2], Number.valueOf((long) str.charAt(idx - 1)))
                ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ split_string/4

    private static final class SplitStringB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term s = m.deref(args[0]), sep = m.deref(args[1]), pad = m.deref(args[2]);
            // START_CHANGE: ISS-2025-0506 - instantiation_error for an unbound argument,
            // type_error(string, T) for a bound one that is not a string.
            if (s instanceof Variable || sep instanceof Variable || pad instanceof Variable) {
                throw Errors.instantiation("split_string/4");
            }
            if (!(s instanceof PrologString))   throw Errors.type("string", m.resolve(s), "split_string/4");
            if (!(sep instanceof PrologString)) throw Errors.type("string", m.resolve(sep), "split_string/4");
            if (!(pad instanceof PrologString)) throw Errors.type("string", m.resolve(pad), "split_string/4");
            // END_CHANGE: ISS-2025-0506
            List<String> parts = split(((PrologString) s).getStringValue(),
                                       ((PrologString) sep).getStringValue(),
                                       ((PrologString) pad).getStringValue());
            List<Term> out = new ArrayList<Term>(parts.size());
            for (int i = 0; i < parts.size(); i++) out.add(new PrologString(parts.get(i)));
            return m.unify(args[3], listOf(out)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        private List<String> split(String input, String separators, String padChars) {
            Set<Integer> sepSet = new HashSet<Integer>();
            for (int i = 0; i < separators.length(); ) {
                int cp = separators.codePointAt(i);
                sepSet.add(Integer.valueOf(cp));
                i += Character.charCount(cp);
            }
            Set<Integer> padSet = new HashSet<Integer>();
            for (int i = 0; i < padChars.length(); ) {
                int cp = padChars.codePointAt(i);
                padSet.add(Integer.valueOf(cp));
                i += Character.charCount(cp);
            }
            boolean collapse = false;
            for (Integer sc : sepSet) if (padSet.contains(sc)) { collapse = true; break; }

            List<String> fields = new ArrayList<String>();
            StringBuilder current = new StringBuilder();
            int i = 0;
            while (i < input.length()) {
                int cp = input.codePointAt(i);
                if (sepSet.contains(Integer.valueOf(cp))) {
                    fields.add(trim(current.toString(), padSet));
                    current.setLength(0);
                } else {
                    current.appendCodePoint(cp);
                }
                i += Character.charCount(cp);
            }
            fields.add(trim(current.toString(), padSet));
            if (!collapse) return fields;
            List<String> result = new ArrayList<String>();
            for (int k = 0; k < fields.size(); k++) if (!fields.get(k).isEmpty()) result.add(fields.get(k));
            if (result.isEmpty()) result.add("");
            return result;
        }

        private String trim(String str, Set<Integer> padChars) {
            if (str.isEmpty()) return str;
            int start = 0;
            while (start < str.length()) {
                int cp = str.codePointAt(start);
                if (!padChars.contains(Integer.valueOf(cp))) break;
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
                if (!padChars.contains(Integer.valueOf(cp))) break;
                end = prev;
            }
            return str.substring(start, end);
        }
    }

    // ------------------------------------------------------------------ atomic_list_concat/2,3

    // START_CHANGE: ISS-2025-0597 - P4.7: atomic_list_concat/2,3 after SWI. Join mode needs a
    // proper list of atomic elements (a var element is instantiation_error, a compound
    // type_error(atomic, E), a non-list type_error(list, L)); when the list is not fully
    // instantiated and the third argument is bound, /3 SPLITS — also with holes in a partial
    // list ([a,B,c], '-', 'a-x-c' gives B = x); an empty separator cannot split
    // (domain_error(non_empty_atom, '')).
    private static final class AtomicListConcatB implements Builtin {
        private final int arity;
        AtomicListConcatB(int arity) { this.arity = arity; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "atomic_list_concat/" + arity;
            if (arity == 2) {
                List<String> parts = joinParts(m, args[0], ctx, true);
                return m.unify(args[1], new Atom(join(parts, ""))) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            Term sepT = m.deref(args[1]);
            Term atomT = m.deref(args[2]);
            if (sepT instanceof Variable) throw Errors.instantiation(ctx);
            String sep = atomicText(sepT);
            if (sep == null) throw Errors.type("atomic", m.resolve(sepT), ctx);
            if (atomT instanceof Variable) {
                List<String> parts = joinParts(m, args[0], ctx, true);
                return m.unify(args[2], new Atom(join(parts, sep))) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            String value = atomicText(atomT);
            if (value == null) throw Errors.type("atomic", m.resolve(atomT), ctx);
            List<String> parts = joinParts(m, args[0], ctx, false);
            if (parts != null) return join(parts, sep).equals(value) ? Outcome.SUCCESS : Outcome.FAILURE;
            // split mode
            if (sep.isEmpty()) throw Errors.domain("non_empty_atom", sepT, ctx);
            List<Term> out = new ArrayList<Term>();
            int from = 0;
            while (true) {
                int at = value.indexOf(sep, from);
                if (at < 0) { out.add(new Atom(value.substring(from))); break; }
                out.add(new Atom(value.substring(from, at)));
                from = at + sep.length();
            }
            return m.unify(args[0], listOf(out)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        private static String join(List<String> parts, String sep) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < parts.size(); i++) {
                if (i > 0) sb.append(sep);
                sb.append(parts.get(i));
            }
            return sb.toString();
        }

        /**
         * The texts of a proper list of atomic elements. A compound element or a non-list raise
         * a type_error; a partial list or a var element raise instantiation_error when
         * {@code strict}, and answer null (split mode) otherwise.
         */
        private static List<String> joinParts(Machine m, Term list, String ctx, boolean strict) {
            List<String> out = new ArrayList<String>();
            Term cur = m.deref(list);
            boolean holes = false;
            int n = 0;
            while (NativeLibrary.isCons(cur)) {
                Term h = m.deref(NativeLibrary.head(cur));
                if (h instanceof Variable) {
                    holes = true;
                } else {
                    String t = atomicText(h);
                    if (t == null) throw Errors.type("atomic", m.resolve(h), ctx);
                    out.add(t);
                }
                cur = m.deref(NativeLibrary.tail(cur));
                if ((++n & 0x3FF) == 0) m.guard().step();
            }
            if (cur instanceof Variable) holes = true;
            else if (!NativeLibrary.isNil(cur)) throw Errors.type("list", m.resolve(list), ctx);
            if (holes) {
                if (strict) throw Errors.instantiation(ctx);
                return null;
            }
            return out;
        }
    }
    // END_CHANGE: ISS-2025-0597

    // START_CHANGE: ISS-2025-0596 - the text of an atomic term (atom, string, number), else null
    static String atomicText(Term t) {
        if (t instanceof Atom) return ((Atom) t).getName();
        if (t instanceof PrologString) return ((PrologString) t).getStringValue();
        if (t instanceof Number) return AtomNumber.formatNumberExact((Number) t);
        return null;
    }
    // END_CHANGE: ISS-2025-0596

    // ------------------------------------------------------------------ term_to_atom/2, term_string/2

    private static final class TermTextB implements Builtin {
        private final boolean asAtom;
        TermTextB(boolean asAtom) { this.asAtom = asAtom; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = asAtom ? "term_to_atom/2" : "term_string/2";
            Term term = m.deref(args[0]);
            Term text = m.deref(args[1]);
            String src = asAtom
                ? ((text instanceof Atom) ? ((Atom) text).getName() : null)
                : ((text instanceof PrologString) ? ((PrologString) text).getStringValue()
                   : (text instanceof Atom) ? ((Atom) text).getName() : null);
            if (src != null) {
                // START_CHANGE: ISS-2025-0568 - P3.4: the v2 parser with the engine's operators;
                // a syntax error RAISES error(syntax_error(D), _) (it failed silently before)
                Term parsed = NativeRead.parseText(src, ctx);
                // END_CHANGE: ISS-2025-0568
                return m.unify(args[0], parsed) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            // START_CHANGE: ISS-2025-0506 - with NEITHER argument bound there is nothing to write
            // and nothing to read: ISO 7.12.2 (a).
            if (term instanceof Variable && text instanceof Variable) throw Errors.instantiation(ctx);
            // END_CHANGE: ISS-2025-0506
            if (!ground(m, text)) {
                Writer.Options o = new Writer.Options();
                o.quoted = true;
                o.numbervars = false;                  // term_to_atom/2 has always written '$VAR'(N)
                String s = Writer.format(term, o, 1200);
                return m.unify(args[1], asAtom ? (Term) new Atom(s) : (Term) new PrologString(s))
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            return Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ keysort/2, delete/3, flatten/2

    private static final class KeySortB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term in = m.deref(args[0]);
            List<Term> pairs = NativeLibrary.elements(in, m.guard());
            if (pairs == null) {
                throw NativeLibrary.notAProperList(m, args[0], "keysort/2");
            }
            for (int i = 0; i < pairs.size(); i++) {
                Term p = m.deref(pairs.get(i));
                if (p instanceof Variable) {
                    throw new PrologException(ISOErrorTerms.instantiationError("keysort/2"));
                }
                if (!(p instanceof CompoundTerm) || !"-".equals(((CompoundTerm) p).getName())
                        || ((CompoundTerm) p).getArguments().size() != 2) {
                    throw new PrologException(ISOErrorTerms.typeError("pair", m.resolve(p), "keysort/2"));
                }
                pairs.set(i, p);
            }
            final it.denzosoft.jprolog.core.engine.ResourceGuard g = m.guard();
            List<Term> sorted = new ArrayList<Term>(pairs);
            Collections.sort(sorted, new java.util.Comparator<Term>() {
                @Override public int compare(Term a, Term b) {
                    return Unify.compareTerms(((CompoundTerm) a).getArguments().get(0),
                                              ((CompoundTerm) b).getArguments().get(0), g);
                }
            });
            return m.unify(args[1], listOf(sorted)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class DeleteB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            List<Term> es = NativeLibrary.elements(args[0], m.guard());
            if (es == null) es = Collections.emptyList();
            Term elem = args[1];
            List<Term> kept = new ArrayList<Term>(es.size());
            for (int i = 0; i < es.size(); i++) {
                Term e = es.get(i);
                Bindings b = m.bindings();
                int mark = b.mark();
                b.forceTrail++;
                boolean matched;
                try {
                    matched = Unify.unify(e, elem, b);
                } finally {
                    b.undo(mark);                      // ISS-2025-0448: undo inside the extent
                    b.forceTrail--;
                }
                if (!matched) kept.add(e);
            }
            return m.unify(args[2], listOf(kept)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // START_CHANGE: ISS-2025-0602 - P4.9: flatten/2 walks with an explicit stack (the recursive
    // walk overflowed the Java stack at ~6000 elements and its depth counter answered
    // resource_error(cyclic_term) for any list longer than 10000). SWI semantics: a variable
    // element or tail is kept as an element, a non-list is a one-element list. A cyclic term
    // cannot be flattened: type_error(acyclic_term, L) instead of looping.
    private static final class FlattenB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term root = m.deref(args[0]);
            List<Term> flat = new ArrayList<Term>();
            ArrayList<Term> pending = new ArrayList<Term>();   // tails still to walk
            pending.add(root);
            int n = 0;
            while (!pending.isEmpty()) {
                Term cur = m.deref(pending.remove(pending.size() - 1));
                while (true) {
                    if ((++n & 0x3FF) == 0) {
                        m.guard().step();
                        // a long walk may be a cyclic term: check ONCE (nothing binds meanwhile)
                        if (n == 0x10000 && Unify.isCyclic(root, m.guard())) {
                            throw Errors.type("acyclic_term", root, "flatten/2");
                        }
                    }
                    if (NativeLibrary.isNil(cur)) break;
                    if (!NativeLibrary.isCons(cur)) { flat.add(cur); break; }
                    Term h = m.deref(NativeLibrary.head(cur));
                    Term tl = NativeLibrary.tail(cur);
                    if (NativeLibrary.isCons(h)) {         // descend into the head, tail later
                        pending.add(tl);
                        cur = h;
                        continue;
                    }
                    if (!NativeLibrary.isNil(h)) flat.add(h);
                    cur = m.deref(tl);
                }
            }
            return m.unify(args[1], listOf(flat)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0602

    // START_CHANGE: ISS-2025-0718
    /**
     * {@code file_base_name/2} (the part after the last {@code /}), {@code file_directory_name/2}
     * (the part before it: {@code .} without one, {@code /} for a root file; trailing separators
     * of a directory path are ignored, as SWI does) and {@code file_extension/2} (the text after the
     * last dot of the base name, {@code ''} when there is none). An atom gives an atom, a string a
     * string; an unbound path is an instantiation error, a non-text one a type error.
     */
    private static final class PathPartB implements Builtin {
        private final int part;
        PathPartB(int part) { this.part = part; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ind = (part == 0 ? "file_base_name" : part == 1 ? "file_directory_name" : "file_extension") + "/2";
            Term p = m.deref(args[0]);
            if (p instanceof Variable) throw Errors.instantiation(ind);
            String path;
            if (p instanceof Atom) path = ((Atom) p).getName();
            else if (p instanceof PrologString) path = ((PrologString) p).getStringValue();
            else throw Errors.type("atom", m.resolve(p), ind);
            String r;
            String trimmed = path;
            while (trimmed.length() > 1 && trimmed.endsWith("/")) trimmed = trimmed.substring(0, trimmed.length() - 1);
            int slash = trimmed.lastIndexOf('/');
            if (part == 1) {
                r = (slash < 0) ? "." : (slash == 0) ? "/" : trimmed.substring(0, slash);
            } else {
                String base = (path.endsWith("/")) ? "" : path.substring(path.lastIndexOf('/') + 1);
                if (part == 0) r = base;
                else {
                    int dot = base.lastIndexOf('.');
                    r = (dot <= 0) ? "" : base.substring(dot + 1);
                }
            }
            Term out = (p instanceof PrologString) ? new PrologString(r) : new Atom(r);
            return m.unify(args[1], out) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0718
}
// END_CHANGE: ISS-2025-0497
