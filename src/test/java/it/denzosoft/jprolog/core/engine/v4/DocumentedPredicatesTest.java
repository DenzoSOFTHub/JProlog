package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import org.junit.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0717 - 4.6 wave Q2.8: the documentation sweep.
/**
 * Every predicate the Reference Manual documents must exist with the documented arity, and every
 * documented evaluable functor must evaluate.
 *
 * <p>The sources are exactly what {@code tools/manual/build_manual.py} puts in the manual: the
 * {@code ## N.} sections 1–28 of {@code docs/references/BUILTIN_PREDICATES_REFERENCE.md} (the
 * first occurrence of each number), its "Java FFI", "Concurrent Execution" and "Module System"
 * sections, and {@code tools/manual/supplement.md}. A predicate is documented by a {@code ###}
 * heading naming it ({@code ### atom_length/2}, {@code ### peek_char/1,2, peek_code/1,2},
 * {@code ### nth0/3 and nth1/3}); a heading marked {@code (directive)} documents a directive, not
 * a callable predicate. Existence is {@code predicate_property(Head, defined)}, which covers the
 * natives, the registry built-ins, the prelude libraries and the control constructs. The
 * evaluables are the back-quoted names of the "Available evaluable functors" list under
 * {@code is/2}; one exists when some arity (the documented one for {@code name/N}) does not
 * raise {@code type_error(evaluable, _)}.
 *
 * <p>When this test fails, either implement the missing predicate or remove it from the
 * documentation, and record the choice (ISS-2025-0718 did both for the misses found in 4.6).
 */
public class DocumentedPredicatesTest {

    private static final Pattern INDICATOR = Pattern.compile(
        "(?<![A-Za-z0-9_$'])([a-z][A-Za-z0-9_]*|'[^']+'|[-+*/\\\\^<>=~:.?@#&$]+)/(\\d+(?:,\\d+)*)");

    private static List<String> lines(String path) throws Exception {
        return Files.readAllLines(new File(path).toPath(), StandardCharsets.UTF_8);
    }

    /** The {@code ###} headings of the manual's predicate chapters. */
    private static List<String> manualHeadings() throws Exception {
        List<String> out = new ArrayList<String>();
        Set<Integer> seen = new java.util.HashSet<Integer>();
        boolean inc = false;
        Pattern sec = Pattern.compile("^## (\\d+)\\. (.*)");
        for (String l : lines("docs/references/BUILTIN_PREDICATES_REFERENCE.md")) {
            if (l.startsWith("## ")) {
                Matcher m = sec.matcher(l);
                if (m.find()) {
                    int n = Integer.parseInt(m.group(1));
                    String t = m.group(2);
                    inc = (n <= 28 && !seen.contains(n)) || t.contains("Java FFI")
                        || t.contains("Concurrent Execution") || t.contains("Module System");
                    seen.add(n);
                } else {
                    inc = false;
                }
                continue;
            }
            if (inc && l.startsWith("### ")) out.add(l);
        }
        for (String l : lines("tools/manual/supplement.md")) {
            if (l.startsWith("### ")) out.add(l);
        }
        return out;
    }

    private static String quoted(String name) {
        if (name.startsWith("'")) return name;
        return "'" + name.replace("\\", "\\\\").replace("'", "\\'") + "'";
    }

    @Test
    public void testISS0717_EveryDocumentedPredicateExists() throws Exception {
        Set<String> indicators = new LinkedHashSet<String>();
        for (String h : manualHeadings()) {
            if (h.contains("(directive)")) continue;
            Matcher m = INDICATOR.matcher(h.substring(4));
            while (m.find()) {
                for (String a : m.group(2).split(",")) indicators.add(m.group(1) + "/" + a);
            }
        }
        assertTrue("the parser found the documented predicates: " + indicators.size(), indicators.size() > 400);
        Prolog prolog = new Prolog();
        List<String> missing = new ArrayList<String>();
        for (String ind : indicators) {
            int slash = ind.lastIndexOf('/');
            String name = ind.substring(0, slash);
            String arity = ind.substring(slash + 1);
            String q = "functor(G, " + quoted(name) + ", " + arity + "), predicate_property(G, defined).";
            boolean ok;
            try {
                ok = !prolog.solve(q).isEmpty();
            } catch (RuntimeException e) {
                ok = false;
            }
            if (!ok) missing.add(ind);
        }
        assertTrue("documented but missing: " + missing, missing.isEmpty());
    }

    @Test
    public void testISS0717_EveryDocumentedEvaluableExists() throws Exception {
        List<String> ref = lines("docs/references/BUILTIN_PREDICATES_REFERENCE.md");
        int start = -1;
        for (int i = 0; i < ref.size(); i++) {
            if (ref.get(i).startsWith("#### Available evaluable functors")) { start = i; break; }
        }
        assertTrue("the evaluable list is in the reference", start >= 0);
        Set<String> names = new LinkedHashSet<String>();
        Pattern tick = Pattern.compile("`([^`]+)`");
        for (int i = start + 1; i < ref.size() && !ref.get(i).startsWith("### "); i++) {
            if (!ref.get(i).startsWith("**")) continue;
            Matcher m = tick.matcher(ref.get(i));
            while (m.find()) names.add(m.group(1));
        }
        assertTrue("the parser found the evaluables: " + names, names.size() > 50);
        Prolog prolog = new Prolog();
        List<String> missing = new ArrayList<String>();
        for (String n : names) {
            String name = n;
            int from = 0, to = 2;
            Matcher pi = Pattern.compile("^(.+)/(\\d)$").matcher(n);
            if (pi.find()) { name = pi.group(1); from = to = Integer.parseInt(pi.group(2)); }
            boolean exists = false;
            for (int a = from; a <= to && !exists; a++) {
                StringBuilder t = new StringBuilder(quoted(name));
                if (a > 0) {
                    t.append('(');
                    for (int k = 0; k < a; k++) t.append(k == 0 ? "1" : ", 1");
                    t.append(')');
                }
                String q = "T = " + t + ", catch(_ is T, E, true), "
                         + "\\+ (nonvar(E), E = error(type_error(evaluable, _), _)).";
                exists = !prolog.solve(q).isEmpty();
            }
            if (!exists) missing.add(n);
        }
        assertTrue("documented evaluables that do not evaluate: " + missing, missing.isEmpty());
    }
}
// END_CHANGE: ISS-2025-0717
