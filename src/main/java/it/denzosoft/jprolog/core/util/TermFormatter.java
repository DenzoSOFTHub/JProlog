// START_CHANGE: ISS-2025-0242 - operator-aware term formatter for write/1, writeq/1, format ~w/~q
package it.denzosoft.jprolog.core.util;

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
 * Operator-aware term-to-string formatter.
 *
 * format(term, quoted, ignoreOps, numbervars, maxContextPrecedence):
 *  - quoted: atoms quoted as needed (for re-readability)
 *  - ignoreOps: never use operator syntax
 *  - numbervars: render '$VAR'(N) as A, B, ..., Z, A1, ...
 *  - maxContextPrecedence: outer precedence (1200 by default)
 */
public final class TermFormatter {

    private TermFormatter() {}

    public static String format(Term t, boolean quoted, boolean ignoreOps, boolean numbervars, int contextPrec) {
        return format(t, quoted, ignoreOps, numbervars, contextPrec, OperatorTable.getDefault());
    }

    public static String format(Term t, boolean quoted, boolean ignoreOps, boolean numbervars, int contextPrec, OperatorTable opTable) {
        if (t == null) return "<null>";
        if (t instanceof Variable) {
            String n = ((Variable) t).getName();
            return n == null ? "_" : n;
        }
        if (t instanceof Atom) {
            String name = ((Atom) t).getName();
            if (!quoted) return name;
            return needsQuoting(name) ? quoteAtom(name) : name;
        }
        if (t instanceof Number) {
            return t.toString();
        }
        if (t instanceof PrologString) {
            String s = ((PrologString) t).getStringValue();
            return quoted ? "\"" + escapeString(s) + "\"" : s;
        }
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            // numbervars: render '$VAR'(N)
            if (numbervars && "$VAR".equals(c.getName()) && c.getArguments() != null && c.getArguments().size() == 1) {
                Term arg = c.getArguments().get(0);
                if (arg instanceof Number) {
                    long n = ((Number) arg).longValue();
                    if (n >= 0) {
                        char letter = (char) ('A' + (n % 26));
                        long idx = n / 26;
                        return idx == 0 ? String.valueOf(letter) : letter + Long.toString(idx);
                    }
                }
            }
            // List notation
            if (".".equals(c.getName()) && c.getArguments() != null && c.getArguments().size() == 2 && !ignoreOps) {
                return formatList(c, quoted, ignoreOps, numbervars, opTable);
            }
            // Curly braces {}
            if ("{}".equals(c.getName()) && c.getArguments() != null && c.getArguments().size() == 1 && !ignoreOps) {
                return "{" + format(c.getArguments().get(0), quoted, ignoreOps, numbervars, 1200, opTable) + "}";
            }
            // Operator notation
            if (!ignoreOps) {
                List<Term> args = c.getArguments();
                String name = c.getName();
                if (args != null) {
                    if (args.size() == 2) {
                        Operator op = opTable.getInfixOperator(name);
                        if (op != null) {
                            int prec = op.getPrecedence();
                            String l = format(args.get(0), quoted, ignoreOps, numbervars, op.getLeftPrecedence(), opTable);
                            String r = format(args.get(1), quoted, ignoreOps, numbervars, op.getRightPrecedence(), opTable);
                            // START_CHANGE: ISS-2025-0387 - insert a space where adjacent symbolic
                            // tokens would merge on re-read (1 - -1 must not print as 1--1); the
                            // ','/2 control operator stays a bare comma (never quoted/spaced).
                            String body;
                            if (",".equals(name)) {
                                body = l + "," + r;
                            } else {
                                String opTok = atomOrName(name, quoted);
                                String sep = opSpace(name);
                                String sepL = !sep.isEmpty() ? sep : (needsTokenSep(l, opTok) ? " " : "");
                                String sepR = !sep.isEmpty() ? sep : (needsTokenSep(opTok, r) ? " " : "");
                                body = l + sepL + opTok + sepR + r;
                            }
                            // END_CHANGE: ISS-2025-0387
                            return prec > contextPrec ? "(" + body + ")" : body;
                        }
                    } else if (args.size() == 1) {
                        Operator pre = opTable.getPrefixOperator(name);
                        if (pre != null) {
                            int prec = pre.getPrecedence();
                            String inner = format(args.get(0), quoted, ignoreOps, numbervars, pre.getRightPrecedence(), opTable);
                            // START_CHANGE: ISS-2025-0387 - a symbolic prefix operator must not glue
                            // to its operand: -(1) is "- 1" (plain -1 re-reads as the integer) and
                            // - -a is "- -a" (--a is one symbolic token under maximal munch).
                            String opTok = atomOrName(name, quoted);
                            String sep = opSpace(name);
                            if (sep.isEmpty()
                                    && (needsTokenSep(opTok, inner)
                                        || (args.get(0) instanceof Number && endsSymbolic(opTok)))) {
                                sep = " ";
                            }
                            String body = opTok + sep + inner;
                            // END_CHANGE: ISS-2025-0387
                            return prec > contextPrec ? "(" + body + ")" : body;
                        }
                        Operator post = opTable.getPostfixOperator(name);
                        if (post != null) {
                            int prec = post.getPrecedence();
                            String inner = format(args.get(0), quoted, ignoreOps, numbervars, post.getLeftPrecedence(), opTable);
                            // START_CHANGE: ISS-2025-0387 - same adjacency guard for postfix
                            String opTok = atomOrName(name, quoted);
                            String sep = opSpace(name);
                            if (sep.isEmpty() && needsTokenSep(inner, opTok)) sep = " ";
                            String body = inner + sep + opTok;
                            // END_CHANGE: ISS-2025-0387
                            return prec > contextPrec ? "(" + body + ")" : body;
                        }
                    }
                }
            }
            // Functional notation fallback
            StringBuilder sb = new StringBuilder();
            sb.append(quoted ? (needsQuoting(c.getName()) ? quoteAtom(c.getName()) : c.getName()) : c.getName());
            if (c.getArguments() != null && !c.getArguments().isEmpty()) {
                sb.append('(');
                for (int i = 0; i < c.getArguments().size(); i++) {
                    if (i > 0) sb.append(',');
                    sb.append(format(c.getArguments().get(i), quoted, ignoreOps, numbervars, 999, opTable));
                }
                sb.append(')');
            }
            return sb.toString();
        }
        return t.toString();
    }

    private static String formatList(CompoundTerm head, boolean quoted, boolean ignoreOps, boolean numbervars, OperatorTable opTable) {
        StringBuilder sb = new StringBuilder("[");
        sb.append(format(head.getArguments().get(0), quoted, ignoreOps, numbervars, 999, opTable));
        Term tail = head.getArguments().get(1);
        while (tail instanceof CompoundTerm) {
            CompoundTerm tc = (CompoundTerm) tail;
            if (".".equals(tc.getName()) && tc.getArguments() != null && tc.getArguments().size() == 2) {
                sb.append(',');
                sb.append(format(tc.getArguments().get(0), quoted, ignoreOps, numbervars, 999, opTable));
                tail = tc.getArguments().get(1);
            } else break;
        }
        if (tail instanceof Atom && "[]".equals(((Atom) tail).getName())) {
            sb.append(']');
        } else {
            sb.append('|').append(format(tail, quoted, ignoreOps, numbervars, 999, opTable)).append(']');
        }
        return sb.toString();
    }

    private static String opSpace(String name) {
        if (name.isEmpty()) return "";
        char c = name.charAt(0);
        // alphabetic operators (is, mod, rem, xor, etc.) need spaces around them
        if (Character.isLetter(c) || c == '_') return " ";
        return "";
    }

    // START_CHANGE: ISS-2025-0387 - token-adjacency helpers: two consecutive symbolic runs would be
    // re-tokenized as a single symbolic atom (maximal munch), so a separating space is required.
    private static boolean needsTokenSep(String left, String right) {
        if (left.isEmpty() || right.isEmpty()) return false;
        return isSymbolic(left.charAt(left.length() - 1)) && isSymbolic(right.charAt(0));
    }

    private static boolean endsSymbolic(String tok) {
        return !tok.isEmpty() && isSymbolic(tok.charAt(tok.length() - 1));
    }
    // END_CHANGE: ISS-2025-0387

    private static String atomOrName(String name, boolean quoted) {
        if (!quoted) return name;
        return needsQuoting(name) ? quoteAtom(name) : name;
    }

    /** Atom needs quoting if not simple lowercase identifier and not a recognized symbolic operator atom. */
    public static boolean needsQuoting(String name) {
        if (name == null || name.isEmpty()) return true;
        // START_CHANGE: ISS-2025-0388 - ',' is a solo char (not an atom token) and a solo '.' forms
        // the end token before layout, so both must be quoted; the ','/2 operator rendering carves
        // out the bare comma explicitly in the infix path.
        if ("[]".equals(name) || "{}".equals(name) || ";".equals(name) || "!".equals(name)) return false;
        if (",".equals(name) || ".".equals(name)) return true;
        // END_CHANGE: ISS-2025-0388
        char c = name.charAt(0);
        // Lowercase identifier
        if (Character.isLowerCase(c) || c == '_') {
            for (int i = 1; i < name.length(); i++) {
                char ch = name.charAt(i);
                if (!Character.isLetterOrDigit(ch) && ch != '_') return true;
            }
            return false;
        }
        // All-symbolic atom (operators)
        boolean allSym = true;
        for (int i = 0; i < name.length(); i++) {
            if (!isSymbolic(name.charAt(i))) { allSym = false; break; }
        }
        // START_CHANGE: ISS-2025-0388 - an unquoted '/*' opens a block comment: quote any symbolic
        // atom containing the comment opener so the output stays parseable.
        if (allSym) return name.contains("/*");
        // END_CHANGE: ISS-2025-0388
        return true;
    }

    private static boolean isSymbolic(char c) {
        return "+-*/\\^<>=~:.?@#$&".indexOf(c) >= 0;
    }

    public static String quoteAtom(String name) {
        StringBuilder sb = new StringBuilder("'");
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (c == '\'') sb.append("''");
            else if (c == '\\') sb.append("\\\\");
            else if (c == '\n') sb.append("\\n");
            else if (c == '\r') sb.append("\\r");
            else if (c == '\t') sb.append("\\t");
            else sb.append(c);
        }
        sb.append('\'');
        return sb.toString();
    }

    private static String escapeString(String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '"') sb.append("\\\"");
            else if (c == '\\') sb.append("\\\\");
            else if (c == '\n') sb.append("\\n");
            else if (c == '\r') sb.append("\\r");
            else if (c == '\t') sb.append("\\t");
            else sb.append(c);
        }
        return sb.toString();
    }
}
// END_CHANGE: ISS-2025-0242
