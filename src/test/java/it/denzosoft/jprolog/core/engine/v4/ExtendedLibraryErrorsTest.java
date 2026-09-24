package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInRegistry;
import it.denzosoft.jprolog.core.engine.InferenceLimitException;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.TermFormatter;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.regex.Pattern;

import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0680 - wave Q1.1/Q1.2: the error-term probe harness over EVERY registered
// built-in name (LIM-038).
/**
 * Probes every name registered in the {@link BuiltInRegistry} at arities 0..4 with (a) every
 * argument unbound and (b) a wrong-type first argument ({@code f(x)}), runs each goal under
 * {@code once(catch(G, E, true))}, and classifies what comes back:
 * <ul>
 *   <li><b>error</b> — a proper ISO {@code error(Formal, Context)} term (this includes
 *       {@code existence_error(procedure, Name/Arity)} for an arity the built-in does not
 *       implement, which is what Q1.2 makes of the old arity guards);</li>
 *   <li><b>message</b> — a bare atom carrying an English sentence (the LIM-038 defect: only a
 *       variable catcher can see it);</li>
 *   <li><b>arity</b> — a bare-atom wrong-arity complaint ("... requires 2 arguments") from a
 *       registry entry that claimed an arity the built-in does not implement (Q1.2);</li>
 *   <li><b>ok</b> / <b>fail</b> — the goal succeeded / failed;</li>
 *   <li><b>other</b> — any other ball (e.g. {@code throw(f(x))} throws {@code f(x)} by design),
 *       an escaping Java exception, the budget, or a timeout.</li>
 * </ul>
 * The test fails on any <b>message</b> or <b>arity</b> outcome, and on a timeout.
 *
 * <p><b>No host effects.</b> The probe must never open a socket, spawn a process, touch a file
 * outside a temporary directory, connect to a database, block or sleep. Every name that could do
 * so with the probe arguments is in {@link #SKIP} (all variants) or {@link #SKIP_WRONG_TYPE} (the
 * {@code f(x)} variant only), each with the reason. Reading goes to an empty temporary file
 * (the probe's {@code current_input}), and output is captured and discarded.
 *
 * <p>The per-family table of the last run is written to
 * {@code target/extended-library-errors.txt}. Measured before wave Q1: see the wave record in
 * {@code docs/reports/report-completeness-4.6-2026-09-23.md} section 10.
 */
public class ExtendedLibraryErrorsTest {

    /** Names that are never probed, with the reason. */
    static final Map<String, String> SKIP = new LinkedHashMap<String, String>();
    /** Names whose wrong-type ({@code f(x)}) variant is not probed, with the reason. */
    static final Map<String, String> SKIP_WRONG_TYPE = new LinkedHashMap<String, String>();

    static {
        SKIP.put("halt", "terminates the engine");
        SKIP.put("thread_exit", "ends the calling thread (the probe's own worker)");
        SKIP.put("thread_get_message", "blocks on the calling thread's empty queue");
        // tcp_accept, udp_receive, udp_socket, tcp_server_socket, http_server, tcp_connect, shell*,
        // the file predicates... ARE probed: each validates its first argument (a handle, a port, a
        // path, a command) before any host effect, and the probe's first argument is always unbound
        // or f(x), so they raise instantiation_error / type_error without blocking, binding a
        // socket, spawning a process or touching a file. If one ever regresses into accepting f(x),
        // the ANSWER changes (the test reports it) before an effect can matter.
        SKIP.put("trace", "switches the engine into trace mode");
        SKIP.put("make", "reloads every consulted file");
    }

    private static final Pattern ARITY_GUARD = Pattern.compile(
        "(?is).*(requires?\\s+(exactly\\s+)?(\\d+|one|two|three|four|zero)(\\s+or\\s+\\d+)?\\s+arguments?"
        + "|expects?\\s+(exactly\\s+)?\\d+\\s+arguments?|takes no arguments|wrong number of arguments"
        + "|(wrong|bad|invalid) arity).*");

    private static final long TIMEOUT_MS = 10000;

    static final class Outcome {
        final String kind; final String text;
        Outcome(String kind, String text) { this.kind = kind; this.text = text; }
    }

    static Term goal(String name, int arity, boolean wrongType) {
        if (arity == 0) return new Atom(name);
        List<Term> args = new ArrayList<Term>();
        for (int i = 0; i < arity; i++) {
            if (i == 0 && wrongType) {
                args.add(new CompoundTerm(new Atom("f"), Collections.<Term>singletonList(new Atom("x"))));
            } else {
                args.add(new Variable("QArg" + i));
            }
        }
        return new CompoundTerm(new Atom(name), args);
    }

    static Outcome classify(Term e) {
        Term d = Unify.deref(e);
        if (d == null || d instanceof Variable) return new Outcome("ok", "");
        String text = TermFormatter.format(d, true, false, true, 1200);
        if (d instanceof CompoundTerm && ((CompoundTerm) d).getArguments().size() == 2
                && "error".equals(((CompoundTerm) d).getName())) {
            return new Outcome("error", text);
        }
        if (d instanceof Atom) {
            return ARITY_GUARD.matcher(((Atom) d).getName()).matches()
                ? new Outcome("arity", text) : new Outcome("message", text);
        }
        return new Outcome("other", text);
    }

    private static final PrintStream DISCARD = new PrintStream(new OutputStream() {
        @Override public void write(int b) { }
        @Override public void write(byte[] b, int off, int len) { }
    });

    /** The empty file every probe reads from (its {@code current_input}). */
    static String emptyPath;

    private static Term conj(Term... ts) {
        Term t = ts[ts.length - 1];
        for (int i = ts.length - 2; i >= 0; i--) t = new CompoundTerm(new Atom(","), new Term[]{ts[i], t});
        return t;
    }

    static Outcome probe(final Prolog prolog, ExecutorService ex, String name, int arity,
                         boolean wrongType) throws Exception {
        final Variable err = new Variable("QErr");
        final Term g = goal(name, arity, wrongType);
        // open(Empty, read, In), set_input(In),
        // ( once(catch(G, E, true)) -> R = yes ; R = no ), catch(close(In), _, true)
        final Variable in = new Variable("QIn");
        final Variable res = new Variable("QRes");
        final Term once = new CompoundTerm(new Atom("once"), Collections.<Term>singletonList(
            new CompoundTerm(new Atom("catch"), new Term[]{g, err, new Atom("true")})));
        final Term ite = new CompoundTerm(new Atom(";"), new Term[]{
            new CompoundTerm(new Atom("->"), new Term[]{once,
                new CompoundTerm(new Atom("="), new Term[]{res, new Atom("yes")})}),
            new CompoundTerm(new Atom("="), new Term[]{res, new Atom("no")})});
        final Term query = conj(
            new CompoundTerm(new Atom("open"), new Term[]{new Atom(emptyPath), new Atom("read"), in}),
            new CompoundTerm(new Atom("set_input"), new Term[]{in}),
            ite,
            new CompoundTerm(new Atom("catch"), new Term[]{
                new CompoundTerm(new Atom("close"), new Term[]{in}), new Variable("_"), new Atom("true")}));
        Future<Outcome> f = ex.submit(() -> {
            StreamManager.setThreadLocalOutput(DISCARD);
            try {
                List<Map<String, Term>> sols = prolog.solve(query);
                if (sols.isEmpty()) return new Outcome("other", "probe scaffolding failed");
                Term r = Unify.deref(sols.get(0).get("QRes"));
                if (r instanceof Atom && ((Atom) r).getName().equals("no")) return new Outcome("fail", "");
                return classify(sols.get(0).get("QErr"));
            } catch (InferenceLimitException ile) {
                return new Outcome("other", "inference budget");
            } catch (RuntimeException re) {
                return new Outcome("other", "escaped " + re.getClass().getSimpleName() + ": " + re.getMessage());
            } finally {
                StreamManager.setThreadLocalOutput(null);
            }
        });
        try {
            return f.get(TIMEOUT_MS, TimeUnit.MILLISECONDS);
        } catch (TimeoutException te) {
            f.cancel(true);
            return new Outcome("timeout", "");
        }
    }

    static String family(BuiltIn b) {
        String n = b.getClass().getName();
        String p = "it.denzosoft.jprolog.builtin.";
        if (n.startsWith(p)) {
            String rest = n.substring(p.length());
            int dot = rest.indexOf('.');
            return dot < 0 ? "builtin" : rest.substring(0, dot);
        }
        return "engine";
    }

    /** One probe run: family -> kind -> count, plus the offending goals. */
    static final class Report {
        final Map<String, Map<String, Integer>> counts = new TreeMap<String, Map<String, Integer>>();
        final List<String> bad = new ArrayList<String>();
        final List<String> review = new ArrayList<String>();       // ISS-2025-0796: fail/ok goals
        int goals;
        void add(String fam, String kind) {
            Map<String, Integer> m = counts.get(fam);
            if (m == null) { m = new TreeMap<String, Integer>(); counts.put(fam, m); }
            Integer c = m.get(kind);
            m.put(kind, c == null ? 1 : c + 1);
            goals++;
        }
        int total(String kind) {
            int t = 0;
            for (Map<String, Integer> m : counts.values()) { Integer c = m.get(kind); if (c != null) t += c; }
            return t;
        }
    }

    static Report run() throws Exception {
        File tmp = java.nio.file.Files.createTempDirectory("jprolog-q1-probe").toFile();
        File empty = new File(tmp, "empty.txt");
        new FileOutputStream(empty).close();
        emptyPath = empty.getAbsolutePath();

        Prolog listing = new Prolog();
        BuiltInRegistry reg = listing.getBuiltInRegistry();
        TreeSet<String> names = new TreeSet<String>(reg.getBuiltInNames());
        Report r = new Report();
        for (String name : names) {
            if (name.contains("/")) continue;                 // "listing/0"-style internal keys
            if (SKIP.containsKey(name)) continue;
            String fam = family(reg.getBuiltIn(name));
            Prolog prolog = new Prolog();
            prolog.setInferenceBudget(2000000);
            ExecutorService ex = Executors.newSingleThreadExecutor(rn -> {
                Thread t = new Thread(rn, "q1-probe"); t.setDaemon(true); return t;
            });
            try {
                for (int arity = 0; arity <= 4; arity++) {
                    for (int v = 0; v < 2; v++) {
                        boolean wrong = v == 1;
                        if (wrong && (arity == 0 || SKIP_WRONG_TYPE.containsKey(name))) continue;
                        Outcome o = probe(prolog, ex, name, arity, wrong);
                        r.add(fam, o.kind);
                        if (o.kind.equals("message") || o.kind.equals("arity") || o.kind.equals("timeout")) {
                            r.bad.add(fam + "\t" + o.kind + "\t" + TermFormatter.format(goal(name, arity, wrong), true, false, true, 1200)
                                      + "\t" + o.text);
                        }
                        // START_CHANGE: ISS-2025-0796 - the goals that fail or succeed on the probe
                        // arguments are listed for review (an unbound INPUT should raise)
                        if (o.kind.equals("fail") || o.kind.equals("ok")) {
                            r.review.add(fam + "\t" + o.kind + "\t"
                                + TermFormatter.format(goal(name, arity, wrong), true, false, true, 1200));
                        }
                        // END_CHANGE: ISS-2025-0796
                        if (o.kind.equals("timeout")) {
                            ex.shutdownNow();
                            ex = Executors.newSingleThreadExecutor(rn -> {
                                Thread t = new Thread(rn, "q1-probe"); t.setDaemon(true); return t;
                            });
                        }
                    }
                }
            } finally {
                ex.shutdownNow();
            }
        }
        return r;
    }

    static String render(Report r) {
        StringBuilder sb = new StringBuilder();
        String[] kinds = {"error", "message", "arity", "ok", "fail", "other", "timeout"};
        sb.append(String.format("%-14s", "family"));
        for (String k : kinds) sb.append(String.format("%9s", k));
        sb.append('\n');
        for (Map.Entry<String, Map<String, Integer>> e : r.counts.entrySet()) {
            sb.append(String.format("%-14s", e.getKey()));
            for (String k : kinds) {
                Integer c = e.getValue().get(k);
                sb.append(String.format("%9d", c == null ? 0 : c));
            }
            sb.append('\n');
        }
        sb.append(String.format("%-14s", "TOTAL"));
        for (String k : kinds) sb.append(String.format("%9d", r.total(k)));
        sb.append("\ngoals probed: ").append(r.goals).append('\n');
        sb.append("skipped (all variants): ").append(SKIP.keySet()).append('\n');
        sb.append("skipped (wrong-type variant): ").append(SKIP_WRONG_TYPE.keySet()).append('\n');
        sb.append("\n--- offending goals ---\n");
        for (String b : r.bad) sb.append(b).append('\n');
        sb.append("\n--- goals that fail or succeed on the probe arguments (review list) ---\n");   // ISS-2025-0796
        for (String b : r.review) sb.append(b).append('\n');
        return sb.toString();
    }

    @Test
    public void testISS0680_NoMessageAtomOrArityGuardErrors() throws Exception {
        Report r = run();
        String text = render(r);
        try {
            File out = new File("target", "extended-library-errors.txt");
            if (out.getParentFile().isDirectory()) {
                try (PrintStream ps = new PrintStream(new FileOutputStream(out), true, "UTF-8")) { ps.print(text); }
            }
        } catch (Exception ignored) {
            // the report file is a convenience; the assertion below is the test
        }
        // START_CHANGE: ISS-2025-0796 - 4.6 wave Q7: the FFI raises on every bad argument now
        // (22 probed goals used to fail silently). java_gc/0 and java_to_term/java_from_term of
        // f(x) may SUCCEED (any bound term converts); none may fail.
        List<String> ffi = new ArrayList<String>();
        for (String g : r.review) if (g.startsWith("ffi\tfail")) ffi.add(g);
        if (!ffi.isEmpty()) fail("FFI goals that fail on bad arguments:\n" + String.join("\n", ffi));
        // END_CHANGE: ISS-2025-0796
        int bad = r.total("message") + r.total("arity") + r.total("timeout");
        if (bad != 0) {
            fail(r.total("message") + " message-atom errors, " + r.total("arity")
                 + " arity-guard messages, " + r.total("timeout") + " timeouts:\n" + text);
        }
    }

    // START_CHANGE: ISS-2025-0685 - wave Q1.2: exact registry arities
    /** Every name {@code new Prolog()} registers declares its exact arity set. */
    @Test
    public void testISS0685_EveryRegisteredNameDeclaresItsArities() {
        BuiltInRegistry reg = new Prolog().getBuiltInRegistry();
        List<String> missing = new ArrayList<String>();
        for (String name : new TreeSet<String>(reg.getBuiltInNames())) {
            if (name.contains("/")) continue;                 // "listing/0"-style internal keys
            if (reg.declaredArities(name) == null) missing.add(name);
        }
        if (!missing.isEmpty()) fail("registered names without an arity declaration: " + missing);
    }

    /** An arity a built-in does not implement is an unknown procedure, and the user may define it. */
    @Test
    public void testISS0685_UndeclaredArityIsAnUnknownProcedure() {
        Prolog p = new Prolog();
        String[][] rows = {
            {"char_code(X)", "error(existence_error(procedure,char_code/1),"},
            {"json_parse(X)", "error(existence_error(procedure,json_parse/1),"},
            {"xml_parse(a, b, c)", "error(existence_error(procedure,xml_parse/3),"},
            {"md5_hash(X, Y, Z, W)", "error(existence_error(procedure,md5_hash/4),"},
        };
        for (String[] r : rows) {
            List<Map<String, Term>> sols = p.solve("catch(" + r[0] + ", E, true)");
            String e = TermFormatter.format(Unify.deref(sols.get(0).get("E")), true, false, true, 1200)
                .replace(" ", "");
            if (!e.startsWith(r[1])) fail(r[0] + " -> " + e);
        }
        p.consult("char_code(mine).\n");
        List<Map<String, Term>> sols = p.solve("char_code(X)");
        org.junit.Assert.assertEquals(1, sols.size());
        org.junit.Assert.assertEquals("mine", TermFormatter.format(sols.get(0).get("X"), true, false, true, 1200));
        // ... while the real arity stays the built-in
        org.junit.Assert.assertEquals(1, p.solve("char_code(a, 97)").size());
    }

    /** assertz on EVERY declared arity of every registered name is still a permission error. */
    @Test
    public void testISS0685_AssertzOnEveryRealArityIsAPermissionError() {
        Prolog p = new Prolog();
        BuiltInRegistry reg = p.getBuiltInRegistry();
        List<String> bad = new ArrayList<String>();
        for (String name : new TreeSet<String>(reg.getBuiltInNames())) {
            if (name.contains("/")) continue;
            java.util.Set<Integer> ar = reg.declaredArities(name);
            if (ar == null) continue;
            for (int n : ar) {
                Term head = goal(name, n, false);
                Variable e = new Variable("QErr");
                Term assertz = new CompoundTerm(new Atom("assertz"), Collections.<Term>singletonList(head));
                Term q = new CompoundTerm(new Atom("catch"), new Term[]{assertz, e, new Atom("true")});
                List<Map<String, Term>> sols = p.solve(q);
                String got = sols.isEmpty() ? "FAIL" : classify(sols.get(0).get("QErr")).text.replace(" ", "");
                if (!got.startsWith("error(permission_error(modify,")) bad.add(name + "/" + n + " -> " + got);
            }
        }
        if (!bad.isEmpty()) fail(bad.size() + " real arities accept assertz:\n" + String.join("\n", bad));
    }
    // END_CHANGE: ISS-2025-0685

    // START_CHANGE: ISS-2025-0698 - wave Q1.3
    /** A worker stopped by the inference budget reports an ISO error TERM in its join status. */
    @Test
    public void testISS0698_BudgetStoppedWorkerJoinStatusIsAnErrorTerm() {
        Prolog p = new Prolog();
        p.consult("spin(0) :- !.\nspin(N) :- N1 is N - 1, spin(N1).\n");
        p.setInferenceBudget(20000);
        List<Map<String, Term>> s = p.solve(
            "thread_create(spin(1000000), Id), thread_join(Id, Status), "
            + "Status = exception(error(resource_error(inference_limit), _))");
        org.junit.Assert.assertEquals("join status must be exception(error(resource_error(inference_limit), _))",
                                      1, s.size());
    }
    // END_CHANGE: ISS-2025-0698
}
// END_CHANGE: ISS-2025-0680
