package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.engine.TableStore;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0540..0559 - wave P2 of the 4.5.0 production-readiness program.
/**
 * Wave P2 (performance: call path, clause store, database) of
 * {@code docs/reports/report-production-readiness-2026-09-23.md}: one method per ISS id. Where the
 * property can be observed without a clock it is (a counter, an allocation count, an identity);
 * the few end-to-end size checks use inputs for which 4.4.0 needed ten to a hundred times the
 * bound, so they fail on the old code and stay far from the bound on a loaded machine.
 */
public class EngineV45PerformanceTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private int count(String query) {
        return prolog.solve(query + ".").size();
    }

    private String one(String query, String var) {
        List<Map<String, Term>> s = prolog.solve(query + ".");
        assertFalse("goal failed: " + query, s.isEmpty());
        return it.denzosoft.jprolog.core.util.TermFormatter.format(s.get(0).get(var), true, false, false, 1200);
    }

    private Term parse(String text) {
        return it.denzosoft.jprolog.core.parser.v2.TermReader.parseTerm(text, prolog.getOperatorTable());
    }

    private static Rule fact(String name, Term... args) {
        return new Rule(new CompoundTerm(new Atom(name), Arrays.asList(args)), new ArrayList<Term>());
    }

    // ------------------------------------------------------------------ P2.1

    /**
     * P2.1: a body goal that resolved to a plain user predicate caches the resolution on its
     * skeleton, and every later activation skips the dispatch chain. The cache is invalidated when
     * a dispatch table changes: registering a native of the same indicator re-routes the goal.
     */
    @Test
    public void testISS0540_BodyGoalCallSiteIsCachedAndInvalidated() {
        prolog.consult("loop(0) :- !.\nloop(N) :- N1 is N-1, loop(N1).\n"
            + "who(user).\nask(X) :- who(X).\n");
        Engine e = prolog.getV4Engine();
        Machine m = new Machine(e, new ResourceGuard(0));
        final int[] sols = {0};
        m.solve(parse("loop(2000)"), sol -> { sols[0]++; return true; });
        assertEquals(1, sols[0]);
        assertTrue("the recursive call must take the call-site path, hits = " + m.siteHits,
            m.siteHits >= 1999);

        assertEquals("user", one("ask(X)", "X"));
        assertEquals("user", one("ask(X)", "X"));                // cached now
        e.natives().register("who", 1, (mm, args) ->
            mm.unify(args[0], new Atom("native")) ? Builtin.Outcome.SUCCESS : Builtin.Outcome.FAILURE);
        assertEquals("a new native must win over the cached user predicate", "native", one("ask(X)", "X"));
        e.natives().unregister("who", 1);
        assertEquals("user", one("ask(X)", "X"));
    }

    /** P2.1: a table declaration made after the site was cached re-routes the call. */
    @Test
    public void testISS0540_CallSiteRespectsTablingDeclaredLater() {
        prolog.consult("edge(a,b).\nedge(b,c).\npath(X,Y) :- edge(X,Y).\npath(X,Y) :- edge(X,Z), path(Z,Y).\n"
            + "go(L) :- findall(Y, path(a,Y), L0), msort(L0, L).\n");
        assertEquals("[b,c]", one("go(L)", "L"));
        prolog.solve("table(path/2).");                       // bumps the tabling modCount
        assertEquals("[b,c]", one("go(L)", "L"));
    }

    // ------------------------------------------------------------------ P2.2

    /** P2.2: the store holds the KnowledgeBase's own entry and still notices an external write. */
    @Test
    public void testISS0541_StoreReadsTheKnowledgeBaseHandle() {
        prolog.consult("k(1).\n");
        assertEquals(1, count("k(_)"));
        Engine e = prolog.getV4Engine();
        ClauseStore.Predicate p = e.store().lookup("k", 1);
        assertSame(e.kb().entry("k", 1), p.kbEntry);
        assertSame("the handle is stable", p.kbEntry, e.kb().entry("k", 1));
        e.kb().addRule(fact("k", Number.valueOf(2)));            // behind the store's back
        assertEquals(2, count("k(_)"));
        assertEquals(p.kbEntry.version(), p.kbVersion);
    }

    // ------------------------------------------------------------------ P2.3

    /** P2.3: native lookup by name and arity (no key string), and modification counters. */
    @Test
    public void testISS0542_DispatchTablesAnswerWithoutStringKeys() {
        BuiltinTable t = new BuiltinTable();
        Builtin b0 = (m, a) -> Builtin.Outcome.SUCCESS;
        Builtin b2 = (m, a) -> Builtin.Outcome.FAILURE;
        int mc = t.modCount();
        t.register("foo", 2, b2);
        t.register("foo", 0, b0);
        assertTrue(t.modCount() > mc);
        assertSame(b2, t.lookup("foo", 2));
        assertSame(b0, t.lookup("foo", 0));
        assertNull(t.lookup("foo", 1));
        assertNull(t.lookup("foo", 3));
        assertNull(t.lookup("bar", 0));
        assertTrue(t.isNative("foo", 2));
        mc = t.modCount();
        t.unregister("foo", 2);
        assertNull(t.lookup("foo", 2));
        assertFalse(t.isNative("foo", 2));
        assertTrue(t.modCount() > mc);

        TableStore ts = new TableStore();
        assertFalse(ts.isTabled("p", 1));
        mc = ts.modCount();
        ts.declareTable("p", 1);
        assertTrue(ts.isTabled("p", 1));
        assertFalse(ts.isTabled("p", 2));
        assertTrue(ts.modCount() > mc);
    }

    // ------------------------------------------------------------------ P2.4

    /**
     * P2.4: activating a clause must not walk (or copy) the ground sub-terms of its body. 4.4.0
     * walked the 2000-element list below on every call, allocating ~35 KB per activation; the
     * skeleton now shares it untouched (~0.5 KB per iteration of the whole driver loop).
     */
    @Test
    public void testISS0543_InstantiateDoesNotWalkGroundSubterms() {
        java.lang.management.ThreadMXBean raw = java.lang.management.ManagementFactory.getThreadMXBean();
        Assume.assumeTrue(raw instanceof com.sun.management.ThreadMXBean);
        com.sun.management.ThreadMXBean mx = (com.sun.management.ThreadMXBean) raw;
        Assume.assumeTrue(mx.isThreadAllocatedMemorySupported() && mx.isThreadAllocatedMemoryEnabled());
        StringBuilder big = new StringBuilder("[");
        for (int i = 1; i <= 2000; i++) { if (i > 1) big.append(','); big.append(i); }
        big.append(']');
        prolog.consult("big(X) :- keep(" + big + ", X).\nkeep(_, _).\n"
            + "run(N) :- between(1, N, _), big(_), fail.\nrun(_).\n");
        long tid = Thread.currentThread().getId();
        long best = Long.MAX_VALUE;
        for (int r = 0; r < 4; r++) {
            prolog.solve("run(10).");
            long b0 = mx.getThreadAllocatedBytes(tid);
            prolog.solve("run(1000).");
            long b1 = mx.getThreadAllocatedBytes(tid);
            prolog.solve("run(1).");
            long b2 = mx.getThreadAllocatedBytes(tid);
            best = Math.min(best, ((b1 - b0) - (b2 - b1)) / 999);
        }
        assertTrue("bytes allocated per activation: " + best, best < 5000);
    }

    /** P2.4: the array-backed argument view behaves like the old unmodifiable list. */
    @Test
    public void testISS0543_ArgumentViewIsUnmodifiableAndLive() {
        Term[] args = { new Atom("a"), new Atom("b") };
        CompoundTerm c = new CompoundTerm(new Atom("f"), args);
        assertEquals(2, c.arity());
        assertSame(args[1], c.arg(1));
        List<Term> v = c.getArguments();
        assertSame(v, c.getArguments());
        assertEquals(Arrays.asList(new Atom("a"), new Atom("b")), v);
        try { v.set(0, new Atom("z")); throw new AssertionError("view must be unmodifiable"); }
        catch (UnsupportedOperationException expected) { /* ok */ }
        c.setArgument(1, new Atom("z"));                          // setarg/3 path: view reads through
        assertEquals(new Atom("z"), v.get(0));
        assertEquals(c, new CompoundTerm(new Atom("f"), Arrays.asList((Term) new Atom("z"), new Atom("b"))));
    }

    /**
     * Found while doing P2.1 (ISS-2025-0551): a body-only variable first reached AFTER a body
     * goal left a choice point was created "young", so its binding was not trailed and the frame
     * kept the stale binding on backtracking. 4.4.0 answered only Y = 2 below.
     */
    @Test
    public void testISS0551_BodyVariableFirstBoundAfterAChoicePoint() {
        prolog.consult("q(1). q(2).\n"
            + "p(Y) :- q(Z), X is Z + 1, Y = X.\n"
            + "p3(Y) :- q(Z), ( Z > 1 -> X = big ; X = small ), Y = X-Z.\n"
            + "p4(L) :- q(A), q(B), T = A-B, L = T.\n");
        List<Map<String, Term>> s = prolog.solve("p(Y).");
        assertEquals(2, s.size());
        assertEquals("3", s.get(1).get("Y").toString());
        assertEquals(2, count("p3(_)"));
        assertEquals("[small-1,big-2]", one("findall(Y, p3(Y), L)", "L"));
        assertEquals(4, count("p4(_)"));
    }

    // ------------------------------------------------------------------ P2.5 / P2.7 / P2.8 (KB)

    /**
     * P2.5/P2.7/P2.8 at the KnowledgeBase level: asserta, retract of a stored Rule and retractall
     * are O(1) / O(n). 4.4.0 did asserta on a global list (100 000: 5-16 s), retract by a global
     * identity scan, and retractall by front removals (100 000: 3.5 s) — all quadratic.
     */
    @Test
    public void testISS0544_KnowledgeBaseWritesAreLinear() {
        KnowledgeBase kb = new KnowledgeBase();
        // the global order is unchanged: asserta goes to the front of EVERYTHING
        kb.addRule(fact("a", Number.valueOf(1)));
        kb.asserta(fact("b", Number.valueOf(1)));
        kb.addRule(fact("a", Number.valueOf(2)));
        kb.asserta(fact("a", Number.valueOf(0)));
        assertEquals("[a(0)., b(1)., a(1)., a(2).]", kb.getRules().toString());
        assertEquals("[a(0)., a(1)., a(2).]", kb.getRulesForPredicate("a", 1).toString());

        final int n = 200000;
        long t0 = System.nanoTime();
        List<Rule> rs = new ArrayList<Rule>(n);
        for (int i = 0; i < n; i++) { Rule r = fact("p", Number.valueOf(i)); rs.add(r); kb.asserta(r); }
        assertEquals(n, kb.getRulesForPredicate("p", 1).size());
        for (int i = 0; i < n; i += 2) assertTrue(kb.retract(rs.get(i)));       // O(1) each
        assertEquals(n / 2, kb.getRulesForPredicate("p", 1).size());
        assertFalse("already gone", kb.retract(rs.get(0)));
        assertEquals("p(" + (n - 1) + ").", kb.getRulesForPredicate("p", 1).get(0).toString());
        for (int i = 0; i < n; i++) kb.addRule(fact("q", Number.valueOf(i)));
        assertEquals(n, kb.retractAllClauses(parse("q(_)")));
        assertEquals(0, kb.getRulesForPredicate("q", 1).size());
        assertTrue(kb.isDynamic("q", 1));
        long ms = (System.nanoTime() - t0) / 1000000;
        assertTrue("KnowledgeBase writes took " + ms + " ms", ms < 8000);
        // the equals fallback still finds a freshly built, equal Rule
        assertTrue(kb.retract(fact("p", Number.valueOf(1))));
    }

    // ------------------------------------------------------------------ P2.5 (v4 path)

    /**
     * P2.5: retractall/1 goes through the clause store — each match is retracted in O(1) with a
     * death generation, the store stays in step with the KnowledgeBase (no re-sync of the whole
     * predicate on the next call), and a running call keeps its logical update view.
     */
    @Test
    public void testISS0545_RetractallGoesThroughTheStore() {
        prolog.consult(":- dynamic q/2.\n");
        prolog.solve("between(1, 50000, I), assertz(q(I, x)), fail ; true.");
        Engine e = prolog.getV4Engine();
        ClauseStore.Predicate p = e.store().lookup("q", 2);
        assertEquals(1, count("retractall(q(7, _))"));
        assertEquals("the store is still in step: no re-sync pending",
            p.kbEntry.version(), p.kbVersion);
        assertEquals(0, count("q(7, _)"));
        assertEquals("49999", one("findall(K, q(K, _), L), length(L, N)", "N"));
        assertEquals("[1,2]",
            one("findall(X, (q(X, _), X < 3, retractall(q(_, _))), L)", "L"));
        assertEquals(0, count("q(_, _)"));

        prolog.solve("between(1, 200000, I), assertz(q(I, y)), fail ; true.");
        long t0 = System.nanoTime();
        assertEquals(1, count("retractall(q(_, y))"));
        long ms = (System.nanoTime() - t0) / 1000000;
        assertEquals(0, count("q(_, _)"));
        assertTrue("retractall of 200 000 clauses took " + ms + " ms", ms < 4000);
    }

    // ------------------------------------------------------------------ P2.6 / P2.7 (store)

    /**
     * P2.6/P2.7: retracting the first clause and asserta do not copy the predicate. The candidate
     * window of an unbound call starts past the dead prefix and is taken over the SAME array;
     * asserta fills a front gap, so the backing array changes O(log n) times, not n times.
     */
    @Test
    public void testISS0546_RetractAndAssertaDoNotCopyThePredicate() {
        prolog.consult(":- dynamic r/1.\n");
        prolog.solve("between(1, 20000, I), assertz(r(I)), fail ; true.");
        ClauseStore store = prolog.getV4Engine().store();
        ClauseStore.Predicate p = store.lookup("r", 1);
        ClauseStore.View w = new ClauseStore.View();
        p.view(null, w);
        Clause[] arr = w.a;
        assertEquals(20000, w.size());
        for (int k = 1; k <= 1000; k++) {
            assertTrue(store.retractClause(p, w.a[w.from]));
            p.view(null, w);
            assertSame("a retract must not copy the clause array", arr, w.a);
            assertEquals("the window skips the dead prefix", 20000 - k, w.size());
        }
        assertEquals("[1001]", one("findall(X, (r(X), X < 1002), L)", "L"));

        IdentityHashMap<Clause[], Boolean> arrays = new IdentityHashMap<Clause[], Boolean>();
        for (int i = 0; i < 20000; i++) {
            store.assertRule(fact("s", Number.valueOf(i)), true);
            ClauseStore.Predicate s = store.lookup("s", 1);
            s.view(null, w);
            arrays.put(w.a, Boolean.TRUE);
        }
        assertTrue("asserta must grow the array geometrically, saw " + arrays.size() + " arrays",
            arrays.size() < 40);
        assertEquals("19999", one("s(X), !", "X"));
        assertEquals("20000", one("findall(X, s(X), L), length(L, N)", "N"));
    }

    // ------------------------------------------------------------------ P2.9

    /**
     * P2.9: asserting into a predicate that has a variable-headed clause while calling it with a
     * bound first argument. Each bucket view is merged with the variable-headed clauses by
     * ordinal, in O(|bucket| + |variable-headed|); 4.4.0 rescanned the whole predicate after
     * every write (3e4 iterations: 5.5 s).
     */
    @Test
    public void testISS0547_MergedBucketViewIsIncremental() {
        prolog.consult(":- dynamic w/2.\nw(_, any).\n");
        assertEquals(1, count("between(1, 5000, I), assertz(w(I, I)), w(I, I), fail ; true"));
        ClauseStore.Predicate p = prolog.getV4Engine().store().lookup("w", 2);
        assertTrue("merge work must be linear, merged slots = " + p.mergedSlots,
            p.mergedSlots <= 3L * 5000);
        // source order across the merge: the variable-headed clause came first
        assertEquals("[any,3]", one("findall(V, w(3, V), L)", "L"));
        prolog.solve("asserta(w(3, front)).");
        assertEquals("[front,any,3]", one("findall(V, w(3, V), L)", "L"));
        prolog.solve("asserta(w(_, top)).");
        assertEquals("[top,front,any,3]", one("findall(V, w(3, V), L)", "L"));
        assertEquals("[top,any]", one("findall(V, w(zzz, V), L)", "L"));
    }

    // ------------------------------------------------------------------ P2.10

    private long allocatedBytes(Runnable r) {
        java.lang.management.ThreadMXBean raw = java.lang.management.ManagementFactory.getThreadMXBean();
        Assume.assumeTrue(raw instanceof com.sun.management.ThreadMXBean);
        com.sun.management.ThreadMXBean mx = (com.sun.management.ThreadMXBean) raw;
        Assume.assumeTrue(mx.isThreadAllocatedMemorySupported() && mx.isThreadAllocatedMemoryEnabled());
        long tid = Thread.currentThread().getId();
        long b0 = mx.getThreadAllocatedBytes(tid);
        r.run();
        return mx.getThreadAllocatedBytes(tid) - b0;
    }

    /**
     * P2.10: append(-, ?, +) enumerates its splits in O(1) each. The 4.4.0 native built a fresh
     * n-element prefix for every split n: append(_, [Last], L) over 4 000 elements allocated
     * ~8 million list cells and variables (hundreds of MB); now it is linear.
     */
    @Test
    public void testISS0548_AppendSplitModeIsLinear() {
        assertEquals("[[]-[1,2,3],[1]-[2,3],[1,2]-[3],[1,2,3]-[]]",
            one("findall(X-Y, append(X, Y, [1,2,3]), L)", "L"));
        assertEquals("[a,b]", one("append(X, [c], [a,b,c])", "X"));
        assertEquals(0, count("append(X, [z], [a,b,c])"));
        assertEquals("[b-[c]]", one("findall(H-T, append([a,H|T], [], [a,b,c]), L)", "L"));
        assertEquals("[[1]]", one("findall(X, append(X, [2], [1,2]), L)", "L"));
        prolog.solve("numlist(1, 4000, L), append(_, [Last], L).");        // warm-up
        long bytes = allocatedBytes(() ->
            assertEquals("4000", one("numlist(1, 4000, L), append(_, [Last], L)", "Last")));
        assertTrue("append(_, [Last], L) over 4000 elements allocated " + bytes + " bytes",
            bytes < 40L * 1024 * 1024);
    }

    // ------------------------------------------------------------------ P2.11

    /**
     * P2.11: the body goals of a LIBRARY module's clauses (library(apply)'s maplist recursion) use
     * call sites too — cached per context module, so maplist's own recursion skips the module
     * resolution, the "name/arity" keys and the meta-spec lookup on every element.
     */
    @Test
    public void testISS0549_LibraryRecursionUsesCallSites() {
        Engine e = prolog.getV4Engine();
        prolog.solve("numlist(1, 10, L), maplist(succ, L, _).");          // loads library(apply)
        Machine m = new Machine(e, new ResourceGuard(0));
        final int[] sols = {0};
        m.solve(parse("numlist(1, 3000, L), maplist(succ, L, L2), foldl(plus, L2, 0, S), S =:= 4504500"),
            sol -> { sols[0]++; return true; });
        assertEquals(1, sols[0]);
        assertTrue("maplist/foldl recursion must take the call-site path, hits = " + m.siteHits,
            m.siteHits >= 2 * 2990);
        // the meta-argument still runs in the caller's context
        prolog.consult(":- module(mm, [go/1]).\nhelper(X, Y) :- Y is X * 10.\ngo(L) :- maplist(helper, [1,2,3], L).\n");
        assertEquals("[10,20,30]", one("mm:go(L)", "L"));
    }

    /** P2.11: predsort/3 keeps its semantics on the array merge sort. */
    @Test
    public void testISS0549_PredsortSemantics() {
        prolog.consult("cmp(O, A, B) :- compare(O, A, B).\n"
            + "by_key(O, K1-_, K2-_) :- compare(O1, K1, K2), (O1 == (=) -> O = (<) ; O = O1).\n"
            + "rev(D, O, A, B) :- compare(O0, A, B), (D == desc -> inv(O0, O) ; O = O0).\n"
            + "inv(<, >). inv(>, <). inv(=, =).\n"
            + "odd(foo, _, _).\n");
        assertEquals("[a,b,c,d]", one("predsort(cmp, [c,a,b,a,d,c], L)", "L"));   // '=' drops
        assertEquals("[1-b,1-a,2-c]", one("predsort(by_key, [2-c, 1-b, 1-a], L)", "L")); // stable
        assertEquals("[3,2,1]", one("predsort(rev(desc), [1,3,2], L)", "L"));        // closure args
        assertEquals("[]", one("predsort(cmp, [], L)", "L"));
        assertEquals("[x]", one("predsort(odd, [x], L)", "L"));                       // never compared
        assertEquals(0, count("predsort(odd, [2,1], _)"));                            // bad order: fail
        assertEquals(0, count("predsort(cmp, [a|_], _)"));                            // partial list
        assertEquals("100000", one("numlist(1, 100000, L0), reverse(L0, L1), predsort(cmp, L1, L), length(L, N)", "N"));
    }

    // ------------------------------------------------------------------ P2.12

    /** A term that counts how often it is rendered. */
    private static final class CountingTerm extends CompoundTerm {
        static int rendered;
        CountingTerm() { super(new Atom("c"), new Term[] { new Atom("x") }); }
        @Override public String toString() { rendered++; return super.toString(); }
    }

    /**
     * P2.12: no FINE-level log call builds its message unless FINE is enabled. The rule is checked
     * over the whole source tree (a string concatenation in a LOGGER.fine / Level.FINE call must
     * sit behind isLoggable on the same or the previous line), and behaviourally on one site.
     */
    @Test
    public void testISS0550_FineLogsAreLazy() throws java.io.IOException {
        CountingTerm.rendered = 0;
        it.denzosoft.jprolog.core.utils.ListTerm l1 = new it.denzosoft.jprolog.core.utils.ListTerm(
            Arrays.asList((Term) new CountingTerm()));
        it.denzosoft.jprolog.core.utils.ListTerm l2 = new it.denzosoft.jprolog.core.utils.ListTerm(
            Arrays.asList((Term) new Atom("y")));
        assertFalse(l1.unify(l2, new java.util.HashMap<String, Term>()));
        assertEquals("a failed unification must not render its terms for a disabled log", 0,
            CountingTerm.rendered);

        java.nio.file.Path root = java.nio.file.Paths.get("src/main/java/it/denzosoft/jprolog");
        Assume.assumeTrue(java.nio.file.Files.isDirectory(root));
        final List<String> bad = new ArrayList<String>();
        java.util.regex.Pattern fine = java.util.regex.Pattern.compile(
            "LOGGER\\.(fine|finer|finest)\\(|Level\\.(FINE|FINER|FINEST)\\s*,");
        try (java.util.stream.Stream<java.nio.file.Path> files = java.nio.file.Files.walk(root)) {
            for (java.nio.file.Path f : (Iterable<java.nio.file.Path>) files.filter(x -> x.toString().endsWith(".java"))::iterator) {
                List<String> lines = java.nio.file.Files.readAllLines(f, java.nio.charset.StandardCharsets.UTF_8);
                for (int i = 0; i < lines.size(); i++) {
                    String ln = lines.get(i);
                    if (!fine.matcher(ln).find() || !ln.contains("+")) continue;
                    String prev = (i > 0) ? lines.get(i - 1) : "";
                    if (ln.contains("isLoggable") || prev.contains("isLoggable")) continue;
                    bad.add(f + ":" + (i + 1));
                }
            }
        }
        assertTrue("eager FINE log messages: " + bad, bad.isEmpty());
    }

    // ------------------------------------------------------------------ P2.13

    /**
     * P2.13: get_char/2 answers ASCII bytes of a UTF-8 stream without a CharsetDecoder round trip
     * (4.4.0 decoded every character through a one-char buffer: 1.6 MB took 3 s), and multi-byte
     * characters, peek_char/2 and end of file still come out right.
     */
    @Test
    public void testISS0552_GetCharSkipsTheDecoderForAscii() throws Exception {
        java.io.File f = java.io.File.createTempFile("p2getchar", ".txt");
        f.deleteOnExit();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 20000; i++) sb.append((char) ('a' + i % 26));
        sb.append("\u00e0\u00e9\u20ac\ud83d\ude00x");
        java.nio.file.Files.write(f.toPath(), sb.toString().getBytes(java.nio.charset.StandardCharsets.UTF_8));
        String path = f.getAbsolutePath().replace("\\", "/");
        prolog.consult("rd(S, N0, N, Last) :- get_char(S, C), ( C == end_of_file -> N = N0, Last = [] "
            + "; N1 is N0 + 1, ( N1 > 20000 -> Last = [C|L1] ; Last = L1 ), rd(S, N1, N, L1) ).\n");
        long before = PrologStream.decoderCalls;
        List<Map<String, Term>> r = prolog.solve("open('" + path + "', read, S), rd(S, 0, N, L), "
            + "close(S), atom_chars(A, L).");
        long calls = PrologStream.decoderCalls - before;
        assertEquals(1, r.size());
        assertEquals("20005", r.get(0).get("N").toString());
        assertEquals("\u00e0\u00e9\u20ac\ud83d\ude00x", r.get(0).get("A").toString());
        assertTrue("ASCII characters must not go through the decoder: " + calls + " decoder calls",
            calls < 100);
        assertEquals(1, count("open('" + path + "', read, S), peek_char(S, a), get_char(S, a), "
            + "get_char(S, b), peek_char(S, c), close(S)"));
    }

    // ------------------------------------------------------------------ P2.14

    /**
     * P2.14: the .jpc format is smaller than the source (4.4.0: 24-30 % larger) and loads what
     * consult loads: 64-bit integers as varints, a variable's name only at its first occurrence;
     * a 0x03 file (the 4.4.0 format) is still read.
     */
    @Test
    public void testISS0553_JpcIsCompactAndLoadsTheSameProgram() throws Exception {
        StringBuilder src = new StringBuilder();
        for (int i = 0; i < 2000; i++) {
            src.append("f(").append(i).append(", item_").append(i % 97).append(", [a, b], X, X, ")
               .append(-i * 1000003L).append(").\n");
        }
        src.append("g(9223372036854775807, -9223372036854775808, 123456789012345678901234567890, 2.5, \"str\").\n");
        src.append("h(X, Y) :- f(X, _, _, Y, Y, _), Y = X.\n");
        java.io.File f = java.io.File.createTempFile("p2jpc", ".pl");
        f.deleteOnExit();
        java.nio.file.Files.write(f.toPath(), src.toString().getBytes(java.nio.charset.StandardCharsets.UTF_8));
        String jpc = prolog.compileFile(f.getAbsolutePath());
        new java.io.File(jpc).deleteOnExit();
        long pl = f.length(), jc = new java.io.File(jpc).length();
        assertTrue("jpc " + jc + " bytes must be smaller than the source " + pl, jc < pl);

        Prolog p2 = new Prolog();
        p2.consultCompiled(jpc);
        assertEquals(2000, p2.solve("f(_, _, _, _, _, _).").size());
        assertEquals(1, p2.solve("f(1999, item_59, [a, b], V, W, -1999005997), V == W, var(V).").size());
        assertEquals(1, p2.solve("g(9223372036854775807, -9223372036854775808, "
            + "123456789012345678901234567890, 2.5, \"str\").").size());
        assertEquals(0, p2.solve("g(_, _, _, 2, _).").size());          // the float stays a float
        assertEquals(1, p2.solve("h(3, Y).").size());
        assertEquals(1, p2.solve("h(3, V), V == 3.").size());

        // a hand-made 0x03 file: p(X, X, 5) — every variable occurrence carries slot AND name,
        // integers are 8 fixed bytes
        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        bos.write(new byte[] { 0x4A, 0x50, 0x43, 0x03, 0, 0, 0, 0, 0, 0, 0, 0 });
        bos.write(new byte[] { 2, 1, 'p', 1, 'X' });                 // string table: "p", "X"
        bos.write(0);                                                 // no operators
        bos.write(1);                                                 // one rule
        bos.write(new byte[] { 0x04, 0, 3 });                         // p/3
        bos.write(new byte[] { 0x03, 0, 1 });                         // X (slot 0, name 1)
        bos.write(new byte[] { 0x03, 0, 1 });                         // X again, 0x03 style
        bos.write(new byte[] { 0x02, 0x00, 0, 0, 0, 0, 0, 0, 0, 5 }); // 5 as NUM_LONG
        bos.write(0);                                                 // no body
        bos.write(0);                                                 // source line unknown
        Prolog p3 = new Prolog();
        p3.consultCompiled(new java.io.ByteArrayInputStream(bos.toByteArray()));
        assertEquals(1, p3.solve("p(A, B, 5), A == B, var(A).").size());
    }
}
// END_CHANGE: ISS-2025-0540..0559
