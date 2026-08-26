package it.denzosoft.jprolog.core.write.v2;

import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.parser.v2.TermReader;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.List;

/**
 * Source-code formatter (pretty-printer) for Prolog programs. It re-renders each clause through the
 * operator-aware v2 {@link TermWriter} with a standard layout — facts on one line, rule bodies with
 * one goal per line indented under the neck, directives compact — and joins clauses with a blank line.
 *
 * <p>Clause splitting is quote/line-comment/block-comment-aware (it reuses the same scanning rules as
 * the v2 lexer). Full-line comments and blank lines <em>between</em> clauses are preserved verbatim.
 * A clause that does not parse is emitted unchanged (the formatter never corrupts source it cannot
 * understand). Limitation: comments written <em>inside</em> a clause are dropped on reformat.
 */
public final class PrologFormatter {

    private static final String INDENT = "    ";

    private PrologFormatter() {}

    /** Format an entire program. Returns the original text unchanged if it cannot be split/parsed. */
    public static String format(String source, OperatorTable ops) {
        if (source == null || source.trim().isEmpty()) return source;
        List<String> chunks;
        try {
            chunks = splitTopLevelClauses(source);
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return source;
        }
        // Each "unit" is the leading comment block (if any) directly above its clause; units are
        // joined by a blank line. A comment sits immediately above its clause (no blank between them).
        List<String> units = new ArrayList<>();
        for (String chunk : chunks) {
            String[] split = splitLeadingComments(chunk);
            String leading = rstripLines(split[0]);   // blank/comment lines before the clause (verbatim)
            String clauseSrc = split[1].trim();
            StringBuilder u = new StringBuilder();
            if (!leading.isEmpty()) u.append(leading);
            if (!clauseSrc.isEmpty()) {
                String formatted;
                try {
                    formatted = formatClause(TermReader.parseTerm(clauseSrc, ops), ops);
                } catch (RuntimeException e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    formatted = clauseSrc;            // leave unparseable clauses unchanged
                }
                if (u.length() > 0) u.append('\n');
                u.append(formatted);
            }
            if (u.length() > 0) units.add(u.toString());
        }
        if (units.isEmpty()) return source;
        return String.join("\n\n", units) + "\n";
    }

    // ----------------------------------------------------------------- clause rendering
    private static String formatClause(Term t, OperatorTable ops) {
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            String f = c.getName();
            int n = c.getArguments().size();
            if (":-".equals(f) && n == 2) {                       // Head :- Body
                String head = TermWriter.write(c.getArguments().get(0), ops, TermWriter.Options.writeq());
                List<Term> goals = flattenConjunction(c.getArguments().get(1));
                StringBuilder sb = new StringBuilder(head).append(" :-\n");
                for (int i = 0; i < goals.size(); i++) {
                    sb.append(INDENT).append(TermWriter.write(goals.get(i), ops, TermWriter.Options.writeq()));
                    sb.append(i < goals.size() - 1 ? ",\n" : ".");
                }
                return sb.toString();
            }
            if ((":-".equals(f) || "?-".equals(f)) && n == 1) {   // directive
                return f + " " + TermWriter.write(c.getArguments().get(0), ops, TermWriter.Options.writeq()) + ".";
            }
            if ("-->".equals(f) && n == 2) {                      // DCG rule
                String head = TermWriter.write(c.getArguments().get(0), ops, TermWriter.Options.writeq());
                String body = TermWriter.write(c.getArguments().get(1), ops, TermWriter.Options.writeq());
                return head + " -->\n" + INDENT + body + ".";
            }
        }
        return TermWriter.write(t, ops, TermWriter.Options.writeq()) + ".";   // fact
    }

    private static List<Term> flattenConjunction(Term body) {
        List<Term> goals = new ArrayList<>();
        Term cur = body;
        while (cur instanceof CompoundTerm && ",".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            goals.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        goals.add(cur);
        return goals;
    }

    // ----------------------------------------------------------------- quote/comment-aware splitting
    /** Split a program into raw clause chunks at each top-level end token ('.' + layout/EOF),
     *  respecting quoted atoms, double-quoted strings, line comments (%) and block comments. */
    private static List<String> splitTopLevelClauses(String s) {
        List<String> chunks = new ArrayList<>();
        int n = s.length(), start = 0, i = 0;
        while (i < n) {
            char ch = s.charAt(i);
            if (ch == '%') {                                      // line comment to EOL
                while (i < n && s.charAt(i) != '\n') i++;
            } else if (ch == '/' && i + 1 < n && s.charAt(i + 1) == '*') {   // block comment
                i += 2;
                while (i + 1 < n && !(s.charAt(i) == '*' && s.charAt(i + 1) == '/')) i++;
                i += 2;
            } else if (ch == '\'' || ch == '"') {                // quoted atom / string
                char q = ch; i++;
                while (i < n) {
                    char d = s.charAt(i);
                    if (d == '\\' && i + 1 < n) { i += 2; continue; }   // escape
                    if (d == q) {
                        if (i + 1 < n && s.charAt(i + 1) == q) { i += 2; continue; }   // doubled quote
                        i++; break;
                    }
                    i++;
                }
            } else if (ch == '0' && i + 1 < n && s.charAt(i + 1) == '\'') {  // 0'c char code
                i += (i + 2 < n && s.charAt(i + 2) == '\\') ? 4 : 3;
            } else if (ch == '.' && (i + 1 >= n || isLayout(s.charAt(i + 1)) || s.charAt(i + 1) == '%')) {
                chunks.add(s.substring(start, i + 1));           // end token: clause complete
                i++;
                start = i;
            } else {
                i++;
            }
        }
        if (start < n && !s.substring(start).trim().isEmpty()) chunks.add(s.substring(start));
        return chunks;
    }

    /** Separate leading blank/comment-only lines from the clause body within a chunk. */
    private static String[] splitLeadingComments(String chunk) {
        String[] lines = chunk.split("\n", -1);
        int firstCode = 0;
        for (; firstCode < lines.length; firstCode++) {
            String t = lines[firstCode].trim();
            if (t.isEmpty() || t.startsWith("%")) continue;
            break;
        }
        StringBuilder lead = new StringBuilder();
        for (int i = 0; i < firstCode; i++) {
            if (lines[i].trim().startsWith("%")) {
                if (lead.length() > 0) lead.append('\n');
                lead.append(lines[i].replaceAll("\\s+$", ""));
            }
        }
        StringBuilder rest = new StringBuilder();
        for (int i = firstCode; i < lines.length; i++) {
            if (rest.length() > 0) rest.append('\n');
            rest.append(lines[i]);
        }
        return new String[]{lead.toString(), rest.toString()};
    }

    private static boolean isLayout(char c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r'; }
    private static String rstripLines(String s) {
        String[] ls = s.split("\n", -1);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < ls.length; i++) {
            if (i > 0) sb.append('\n');
            sb.append(ls[i].replaceAll("\\s+$", ""));
        }
        return sb.toString();
    }
}
