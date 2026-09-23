package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.ControlFlow;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

// START_CHANGE: ISS-2025-0454 - engine v4, design B.10: the Prolog prelude.
// START_CHANGE: ISS-2025-0467 - wave W6: the prelude became a set of LIBRARY MODULES that are
// autoloaded by predicate indicator instead of one flat blob loaded at engine creation.
/**
 * The library modules of the v4 engine, written in Prolog and shipped as classpath resources under
 * {@code /prelude/}.
 *
 * <h3>Two-phase, JVM-wide</h3>
 * <ol>
 *   <li><b>Index</b> — every resource's {@code :- module(Name, [Exports]).} header is extracted by
 *       a textual scan the first time any engine asks. That gives {@code indicator -> module}
 *       without parsing a single clause, which is what makes autoload possible.</li>
 *   <li><b>Parse</b> — a module's clauses (and its {@code :- meta_predicate} declarations) are
 *       parsed the first time one of its predicates is referenced, and the result is cached for
 *       the whole JVM: a second {@code Prolog} pays nothing.</li>
 * </ol>
 * The prelude is parsed with the STANDARD operator table, never the engine's: a user's
 * {@code :- op/3} must not change how the library reads.
 *
 * <h3>Where the clauses go</h3>
 * Into {@link Modules}, <b>not</b> into the {@code KnowledgeBase}: the machine consults a library
 * module only after the calling context and {@code user}, so a user definition simply wins, and
 * the legacy/v2 engines, the IDE and {@code listing/1} never see the prelude at all.
 *
 * <p>A parse failure is never fatal: the engine falls back to the registry built-ins, which is
 * exactly the pre-W3 behaviour.
 */
final class Prelude {

    private Prelude() {}

    /** Classpath resources holding the library modules, in load order. */
    private static final String[] RESOURCES = {
        "/prelude/lists.pl",
        "/prelude/apply.pl",
        "/prelude/pairs.pl",
        "/prelude/coroutining.pl",
        "/prelude/clpfd.pl"            // ISS-2025-0646/0650: the library(clpfd) global predicates
    };

    /** One library module as the index knows it: no clauses, just the header. */
    static final class Lib {
        final String module;
        final String resource;
        final Set<String> exports;
        Lib(String module, String resource, Set<String> exports) {
            this.module = module; this.resource = resource; this.exports = exports;
        }
    }

    /** One parsed library module. */
    static final class Parsed {
        final Map<String, List<Rule>> byIndicator = new LinkedHashMap<String, List<Rule>>();
        final Map<String, int[]> meta = new HashMap<String, int[]>();
    }

    private static volatile List<Lib> libs;
    private static volatile Map<String, String> ownerIndex;      // "f/n" -> module
    private static final ConcurrentHashMap<String, Parsed> PARSED = new ConcurrentHashMap<String, Parsed>();
    private static final ConcurrentHashMap<String, String> SOURCE = new ConcurrentHashMap<String, String>();

    /** The library module headers, read once per JVM. */
    static List<Lib> libraries() {
        List<Lib> l = libs;
        if (l != null) return l;
        synchronized (Prelude.class) {
            if (libs != null) return libs;
            List<Lib> out = new ArrayList<Lib>();
            Map<String, String> idx = new HashMap<String, String>();
            for (int i = 0; i < RESOURCES.length; i++) {
                String src = source(RESOURCES[i]);
                if (src == null) continue;
                Lib lib = header(RESOURCES[i], src);
                if (lib == null) continue;
                out.add(lib);
                for (String pi : lib.exports) if (!idx.containsKey(pi)) idx.put(pi, lib.module);
            }
            ownerIndex = idx;
            libs = Collections.unmodifiableList(out);
            return libs;
        }
    }

    /** The library module that exports {@code f/n}, or null. Never loads anything. */
    static String owner(String f, int n) {
        return ownerKey(f + "/" + n);
    }

    // START_CHANGE: ISS-2025-0501 - the same lookup with a key the caller already built
    /** The library module that exports {@code key} ({@code "name/arity"}), or null. */
    static String ownerKey(String key) {
        if (ownerIndex == null) libraries();
        Map<String, String> idx = ownerIndex;
        return (idx == null) ? null : idx.get(key);
    }
    // END_CHANGE: ISS-2025-0501

    /** Parse (once per JVM) the clauses and meta declarations of one library resource. */
    static Parsed parse(String resource) {
        if (resource == null) return null;
        Parsed p = PARSED.get(resource);
        if (p != null) return p;
        String src = source(resource);
        if (src == null) return null;
        try {
            p = install(src);
        } catch (RuntimeException e) {
            ControlFlow.rethrowIfControl(e);
            p = new Parsed();                     // a broken library must not break the engine
        }
        Parsed prev = PARSED.putIfAbsent(resource, p);
        return (prev != null) ? prev : p;
    }

    private static Parsed install(String src) {
        OperatorTable ops = new OperatorTable();                       // standard operators only
        List<Term> terms = it.denzosoft.jprolog.core.parser.v2.TermReader.parseProgram(src, ops);
        Parsed out = new Parsed();
        for (int i = 0; i < terms.size(); i++) {
            Term t = terms.get(i);
            if (t instanceof CompoundTerm && ":-".equals(((CompoundTerm) t).getName())
                    && ((CompoundTerm) t).getArguments().size() == 1) {
                directive(out, ((CompoundTerm) t).getArguments().get(0));
                continue;
            }
            Rule r = toRule(t);
            Term h = r.getHead();
            String name;
            int arity;
            if (h instanceof Atom) { name = ((Atom) h).getName(); arity = 0; }
            else if (h instanceof CompoundTerm) {
                name = ((CompoundTerm) h).getName();
                arity = ((CompoundTerm) h).getArguments().size();
            } else {
                continue;
            }
            String key = name + "/" + arity;
            List<Rule> rs = out.byIndicator.get(key);
            if (rs == null) { rs = new ArrayList<Rule>(); out.byIndicator.put(key, rs); }
            rs.add(r);
        }
        return out;
    }

    /** {@code :- meta_predicate Spec.} is the only directive the prelude uses beyond
     *  {@code :- module/2} (which the index already read). */
    private static void directive(Parsed out, Term d) {
        if (!(d instanceof CompoundTerm)) return;
        CompoundTerm c = (CompoundTerm) d;
        if (!"meta_predicate".equals(c.getName()) || c.getArguments().size() != 1) return;
        for (Term spec : conjunction(c.getArguments().get(0))) {
            if (!(spec instanceof CompoundTerm)) continue;
            CompoundTerm s = (CompoundTerm) spec;
            List<Term> as = s.getArguments();
            int[] enc = new int[as.size()];
            for (int i = 0; i < as.size(); i++) enc[i] = metaArg(as.get(i));
            out.meta.put(s.getName() + "/" + as.size(), enc);
        }
    }

    private static List<Term> conjunction(Term t) {
        List<Term> out = new ArrayList<Term>();
        Term cur = t;
        while (cur instanceof CompoundTerm && ",".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            out.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        out.add(cur);
        return out;
    }

    private static int metaArg(Term a) {
        if (a instanceof Number && ((Number) a).isInteger()) {
            long v = ((Number) a).longValue();
            return (v >= 0 && v <= 9) ? (int) v : Modules.META_PLAIN;
        }
        if (a instanceof Atom) return Modules.parseMetaSpecAtom(((Atom) a).getName());
        return Modules.META_PLAIN;
    }

    // ------------------------------------------------------------------ header scan

    /**
     * Pull {@code :- module(Name, [f/1, 'g h'/2, ...]).} out of the source text without invoking
     * the parser. The prelude's own headers are written in exactly this shape, and the test
     * {@code EngineV4ModulesTest.testISS0467_PreludeHeadersMatchTheClauses} checks that every
     * exported indicator really is defined by the file (and vice versa).
     */
    private static Lib header(String resource, String src) {
        int i = src.indexOf(":- module(");
        if (i < 0) return null;
        int open = src.indexOf('(', i);
        int comma = -1, depth = 0;
        for (int k = open + 1; k < src.length(); k++) {
            char ch = src.charAt(k);
            if (ch == '\'') { k = skipQuoted(src, k); continue; }
            if (ch == '(' || ch == '[') depth++;
            else if (ch == ')' || ch == ']') { if (depth == 0) break; depth--; }
            else if (ch == ',' && depth == 0) { comma = k; break; }
        }
        if (comma < 0) return null;
        String name = src.substring(open + 1, comma).trim();
        int lb = src.indexOf('[', comma);
        int rb = matching(src, lb, '[', ']');
        if (lb < 0 || rb < 0) return null;
        Set<String> exports = new LinkedHashSet<String>();
        for (String piece : split(src.substring(lb + 1, rb))) {
            String pi = piece.trim();
            if (pi.isEmpty()) continue;
            int slash = pi.lastIndexOf('/');
            if (slash < 0) continue;
            String f = pi.substring(0, slash).trim();
            if (f.length() >= 2 && f.charAt(0) == '(' && f.charAt(f.length() - 1) == ')') {
                f = f.substring(1, f.length() - 1).trim();       // (?=)/2 — an operator atom
            }
            if (f.length() >= 2 && f.charAt(0) == '\'' && f.charAt(f.length() - 1) == '\'') {
                f = f.substring(1, f.length() - 1);
            }
            String ar = pi.substring(slash + 1).trim();
            try { exports.add(f + "/" + Integer.parseInt(ar)); } catch (NumberFormatException ignored) { /* skip */ }
        }
        return new Lib(name, resource, exports);
    }

    private static int skipQuoted(String s, int start) {
        for (int k = start + 1; k < s.length(); k++) {
            if (s.charAt(k) == '\\') { k++; continue; }
            if (s.charAt(k) == '\'') return k;
        }
        return s.length() - 1;
    }

    private static int matching(String s, int open, char o, char c) {
        if (open < 0) return -1;
        int depth = 0;
        for (int k = open; k < s.length(); k++) {
            char ch = s.charAt(k);
            if (ch == '\'') { k = skipQuoted(s, k); continue; }
            if (ch == o) depth++;
            else if (ch == c && --depth == 0) return k;
        }
        return -1;
    }

    private static List<String> split(String s) {
        List<String> out = new ArrayList<String>();
        int depth = 0, start = 0;
        for (int k = 0; k < s.length(); k++) {
            char ch = s.charAt(k);
            if (ch == '\'') { k = skipQuoted(s, k); continue; }
            if (ch == '(' || ch == '[') depth++;
            else if (ch == ')' || ch == ']') depth--;
            else if (ch == ',' && depth == 0) { out.add(s.substring(start, k)); start = k + 1; }
        }
        out.add(s.substring(start));
        return out;
    }

    // ------------------------------------------------------------------ plumbing

    private static Rule toRule(Term t) {
        if (t instanceof CompoundTerm && ":-".equals(((CompoundTerm) t).getName())
                && ((CompoundTerm) t).getArguments().size() == 2) {
            CompoundTerm c = (CompoundTerm) t;
            return new Rule(c.getArguments().get(0), flatten(c.getArguments().get(1)));
        }
        return new Rule(t, new ArrayList<Term>());
    }

    private static List<Term> flatten(Term body) {
        List<Term> gs = new ArrayList<Term>();
        Term cur = body;
        while (cur instanceof CompoundTerm && ",".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            gs.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        gs.add(cur);
        return gs;
    }

    /** The resource text, read once per JVM. */
    static String source(String resource) {
        String s = SOURCE.get(resource);
        if (s != null) return s;
        InputStream in = Prelude.class.getResourceAsStream(resource);
        if (in == null) return null;
        try {
            StringBuilder sb = new StringBuilder();
            BufferedReader r = new BufferedReader(new InputStreamReader(in, "UTF-8"));
            String line;
            while ((line = r.readLine()) != null) sb.append(line).append('\n');
            r.close();
            s = sb.toString();
            SOURCE.putIfAbsent(resource, s);
            return s;
        } catch (java.io.IOException e) {
            return null;
        } finally {
            try { in.close(); } catch (java.io.IOException ignored) { /* nothing to do */ }
        }
    }

    /** Test hook: the resources the index covers. */
    static String[] resources() { return RESOURCES.clone(); }
}
// END_CHANGE: ISS-2025-0467
// END_CHANGE: ISS-2025-0454
