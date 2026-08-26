package it.denzosoft.jprolog.core.write.v2;

import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;

/**
 * Clean-room, operator-aware term writer — the inverse of the v2 {@link
 * it.denzosoft.jprolog.core.parser.v2.TermReader} parser. It renders a {@link Term} back to text
 * using the shared {@link OperatorTable}, with correct operator precedence/associativity and
 * parenthesisation, list/curly sugar, atom quoting for {@code writeq}, and {@code write_term/2}-style
 * options. Unlike the engine's default {@code Term.toString} (which is purely functional, e.g.
 * {@code ..(1, 3)}), this emits {@code 1..3}, {@code a+b*c}, {@code [a,b|T]}, etc.
 */
public final class TermWriter {

    /** Rendering options (mirrors {@code write_term/2}). */
    public static final class Options {
        public boolean quoted = false;        // writeq: quote atoms/strings that need it
        public boolean ignoreOps = false;     // write_canonical: functional notation only
        public boolean numbervars = true;     // render '$VAR'(N) as A, B, ... Z, A1, ...

        public static Options write()  { return new Options(); }
        public static Options writeq() { Options o = new Options(); o.quoted = true; return o; }
        public static Options canonical() { Options o = new Options(); o.quoted = true; o.ignoreOps = true; o.numbervars = false; return o; }
    }

    private final OperatorTable ops;
    private final Options opt;

    public TermWriter(OperatorTable ops, Options opt) {
        // ISS-2025-0474 - wave W7: default to the ENGINE's operator store (design B.12: one store
        // read by the parser, current_op/3, write_term, the .jpc writer and the IDE formatter).
        this.ops = ops != null ? ops : it.denzosoft.jprolog.core.engine.v4.Ops.current().table();
        this.opt = opt != null ? opt : Options.write();
    }

    // ---------------------------------------------------------------- facade
    public static String write(Term t)  { return new TermWriter(null, Options.write()).format(t); }
    public static String writeq(Term t) { return new TermWriter(null, Options.writeq()).format(t); }
    public static String write(Term t, OperatorTable ops, Options o) { return new TermWriter(ops, o).format(t); }

    public String format(Term t) {
        StringBuilder sb = new StringBuilder();
        writeTerm(t, 1200, sb);
        return sb.toString();
    }

    // ---------------------------------------------------------------- core
    private void writeTerm(Term t, int maxPrec, StringBuilder sb) {
        if (t instanceof Number) { sb.append(numberText((Number) t)); return; }
        if (t instanceof Variable) { sb.append(variableText((Variable) t)); return; }
        if (t instanceof PrologString) { sb.append(opt.quoted ? quoteString(((PrologString) t).getStringValue()) : ((PrologString) t).getStringValue()); return; }
        if (t instanceof Atom) { sb.append(atomText(((Atom) t).getName())); return; }
        if (t instanceof CompoundTerm) { writeCompound((CompoundTerm) t, maxPrec, sb); return; }
        sb.append(String.valueOf(t));
    }

    private void writeCompound(CompoundTerm c, int maxPrec, StringBuilder sb) {
        String name = c.getName();
        List<Term> args = c.getArguments();
        int arity = args.size();

        // $VAR(N) -> A, B, ...
        if (opt.numbervars && "$VAR".equals(name) && arity == 1 && args.get(0) instanceof Number
                && ((Number) args.get(0)).isInteger()) {
            long n = ((Number) args.get(0)).longValue();
            if (n >= 0) { sb.append((char) ('A' + (int) (n % 26))); if (n >= 26) sb.append(n / 26); return; }
        }

        // list  .(H, T)
        if (".".equals(name) && arity == 2) { writeList(c, sb); return; }

        // {}/1
        if ("{}".equals(name) && arity == 1) { sb.append('{'); writeTerm(args.get(0), 1200, sb); sb.append('}'); return; }

        if (!opt.ignoreOps) {
            if (arity == 2) {
                Operator op = ops.getInfixOperator(name);
                if (op != null) {
                    boolean paren = op.getPrecedence() > maxPrec;
                    if (paren) sb.append('(');
                    writeTerm(args.get(0), op.getLeftPrecedence(), sb);
                    sb.append(infixSep(name));
                    writeTerm(args.get(1), op.getRightPrecedence(), sb);
                    if (paren) sb.append(')');
                    return;
                }
            } else if (arity == 1) {
                Operator pre = ops.getPrefixOperator(name);
                if (pre != null) {
                    boolean paren = pre.getPrecedence() > maxPrec;
                    if (paren) sb.append('(');
                    sb.append(atomText(name));
                    if (needSpaceAfterPrefix(name, args.get(0))) sb.append(' ');
                    writeTerm(args.get(0), pre.getRightPrecedence(), sb);
                    if (paren) sb.append(')');
                    return;
                }
                Operator post = ops.getPostfixOperator(name);
                if (post != null) {
                    boolean paren = post.getPrecedence() > maxPrec;
                    if (paren) sb.append('(');
                    writeTerm(args.get(0), post.getLeftPrecedence(), sb);
                    sb.append(atomText(name));
                    if (paren) sb.append(')');
                    return;
                }
            }
        }

        // functional notation:  name(arg1, arg2, ...)
        sb.append(atomText(name)).append('(');
        for (int i = 0; i < arity; i++) {
            if (i > 0) sb.append(',');
            writeTerm(args.get(i), 999, sb);
        }
        sb.append(')');
    }

    private void writeList(CompoundTerm list, StringBuilder sb) {
        sb.append('[');
        Term cur = list;
        boolean first = true;
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            if (!first) sb.append(',');
            first = false;
            writeTerm(((CompoundTerm) cur).getArguments().get(0), 999, sb);
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        if (!(cur instanceof Atom && "[]".equals(((Atom) cur).getName()))) {
            sb.append('|');
            writeTerm(cur, 999, sb);
        }
        sb.append(']');
    }

    // ---------------------------------------------------------------- pieces
    private String numberText(Number n) {
        // negative numbers as a single token; integer vs float per the flag
        return n.toString();
    }

    private String variableText(Variable v) {
        String name = v.getName();
        if (name == null) return "_";
        // engine-internal names usually already display as _G... ; keep as-is
        return name;
    }

    /** Separator for an infix operator: spaces around alphabetic operators, none around symbolic. */
    private String infixSep(String name) {
        if (",".equals(name)) return ",";
        char c0 = name.charAt(0);
        boolean alpha = Character.isLetter(c0);
        return alpha ? " " + name + " " : name;
    }

    /** A symbolic prefix op followed by a number/symbolic needs a space (to avoid e.g. {@code -1}). */
    private boolean needSpaceAfterPrefix(String name, Term arg) {
        char last = name.charAt(name.length() - 1);
        if (Character.isLetterOrDigit(last)) return true;                 // alpha prefix op: "\+ a" needs space
        if (arg instanceof Number) return true;                          // - (1) must not read back as -1
        if (arg instanceof Atom) {
            String a = ((Atom) arg).getName();
            return !a.isEmpty() && isSymbolicChar(a.charAt(0));          // avoid gluing two symbolic runs
        }
        if (arg instanceof CompoundTerm) {
            String an = ((CompoundTerm) arg).getName();
            return !an.isEmpty() && isSymbolicChar(an.charAt(0)) && ops.getPrefixOperator(an) != null;
        }
        return false;
    }

    // ---------------------------------------------------------------- atom quoting
    private String atomText(String name) {
        if (!opt.quoted || !needsQuote(name)) return name;
        return "'" + name.replace("\\", "\\\\").replace("'", "\\'")
                .replace("\n", "\\n").replace("\t", "\\t") + "'";
    }

    private static boolean needsQuote(String a) {
        if (a.isEmpty()) return true;
        if ("[]".equals(a) || "{}".equals(a) || "!".equals(a) || ";".equals(a)) return false;
        char c0 = a.charAt(0);
        // unquoted alphanumeric atom: lowercase letter, then [a-zA-Z0-9_]
        if (Character.isLowerCase(c0)) {
            for (int i = 1; i < a.length(); i++) {
                char c = a.charAt(i);
                if (!(Character.isLetterOrDigit(c) || c == '_')) return true;
            }
            return false;
        }
        // unquoted symbolic atom: all graphic chars
        boolean allSymbolic = true;
        for (int i = 0; i < a.length(); i++) if (!isSymbolicChar(a.charAt(i))) { allSymbolic = false; break; }
        return !allSymbolic;
    }

    private static boolean isSymbolicChar(char c) { return "#$&*+-./:<=>?@^~\\".indexOf(c) >= 0; }

    private static String quoteString(String s) {
        return "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }
}
