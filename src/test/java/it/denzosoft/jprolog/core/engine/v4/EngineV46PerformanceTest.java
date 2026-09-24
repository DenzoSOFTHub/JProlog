package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * 4.6 wave Q6 — performance residue (LIM-042, LIM-037) and the extras found while verifying
 * Q1–Q5. Every check is counter-, structure- or ratio-based; none is a tight absolute bound.
 */
// START_CHANGE: ISS-2025-0775..0789 - 4.6 wave Q6
public class EngineV46PerformanceTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private String one(String query, String var) {
        List<Map<String, Term>> s = prolog.solve(query + ".");
        assertFalse("goal failed: " + query, s.isEmpty());
        return it.denzosoft.jprolog.core.util.TermFormatter.format(s.get(0).get(var), true, false, false, 1200);
    }

    private void ok(String query) {
        assertFalse("goal failed: " + query, prolog.solve(query + ".").isEmpty());
    }

    private void det(String goal) {
        ok("setup_call_cleanup(true, (" + goal + "), Det = true), Det == true");
    }

    private Term parse(String text) {
        return it.denzosoft.jprolog.core.parser.v2.TermReader.parseTerm(text, prolog.getOperatorTable());
    }

    private Machine run(String query) {
        Machine m = new Machine(prolog.getV4Engine(), new ResourceGuard(0));
        m.solve(parse(query), sol -> true);
        return m;
    }

    // ------------------------------------------------------------------ Q6.1

    /** Q6.1: a goal built at run time (call/N) takes the per-functor call-site cache. */
    @Test
    public void testISS0777_RuntimeGoalsUseTheCallSiteCache() {
        prolog.consult("foo(_).\nbar(X) :- X < 0.\n");
        Machine m = run("( between(1, 3000, I), call(foo, I), fail ; true )");
        assertTrue("call/2 of a user predicate must hit the cache, hits = " + m.rtSiteHits,
            m.rtSiteHits >= 2999);
        Machine m2 = run("( between(1, 3000, I), \\+ bar(I), findall(x, foo(I), _), fail ; true )");
        assertTrue("\\+ and findall goals must hit the cache, hits = " + m2.rtSiteHits,
            m2.rtSiteHits >= 2 * 2999);
    }

    /** Q6.1: the cache is keyed by the dispatch stamp — a native registered in the middle of the
     *  query re-routes the very next run-time call of the same functor. */
    @Test
    public void testISS0777_RuntimeCacheIsInvalidatedByTheDispatchStamp() {
        prolog.consult("who(user).\n");
        final Engine e = prolog.getV4Engine();
        e.natives().register("flip", 0, (mm, args) -> {
            e.natives().register("who", 1, (m2, a2) ->
                m2.unify(a2[0], new Atom("native")) ? Builtin.Outcome.SUCCESS : Builtin.Outcome.FAILURE);
            return Builtin.Outcome.SUCCESS;
        });
        List<Map<String, Term>> s = prolog.solve("call(who, X), call(who, X2), flip, call(who, Y).");
        assertEquals(1, s.size());
        assertEquals("user", s.get(0).get("X").toString());
        assertEquals("native", s.get(0).get("Y").toString());
        e.natives().unregister("who", 1);
        e.natives().unregister("flip", 0);
        assertEquals("user", one("call(who, Z)", "Z"));
    }

    /** Q6.1: the context module is part of the key — the same functor called in two modules. */
    @Test
    public void testISS0777_RuntimeCacheRespectsTheContextModule() {
        prolog.consult(":- module(mq, [go/1]).\nwho(mq).\ngo(L) :- findall(X, (member(G, [who]), call(G, X)), L).\n");
        prolog.consult(":- module(user).\nwho(user).\n");
        assertEquals("[mq]", one("mq:go(L)", "L"));
        assertEquals("[user,mq]", one("findall(X, (call(who, X) ; mq:go([X])), L)", "L"));
    }

    // ------------------------------------------------------------------ Q6.2

    /** The four-port text trace/0 writes for {@code query}, variable names normalised. */
    private String trace(String query) {
        java.io.ByteArrayOutputStream bos = new java.io.ByteArrayOutputStream();
        java.io.PrintStream ps = new java.io.PrintStream(bos, true);
        java.io.PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            prolog.setTracing(true);
            prolog.solve(query);
        } finally {
            prolog.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        return bos.toString().replaceAll("_G\\d+", "_G").replace("\r\n", "\n");
    }

    private static String lines(String... ls) {
        StringBuilder sb = new StringBuilder();
        for (String l : ls) sb.append(l).append('\n');
        return sb.toString();
    }

    /** Q6.2: maplist/foldl/include run as native levels — no library clause is activated per
     *  element — with the answers, choice points and determinism of the Prolog definitions. */
    @Test
    public void testISS0780_ApplyFamilyIsNative() {
        prolog.consult("lt(X, Y) :- Y is X + 1.\nadd(X, A0, A) :- A is A0 + X.\nodd(X) :- X mod 2 =:= 1.\n"
            + "m2(X, Y) :- member(Y, [X, x]).\n");
        Machine m = run("numlist(1, 2000, L), maplist(lt, L, L2), foldl(add, L2, 0, S), include(odd, L, I)");
        assertTrue("one native level per element, levels = " + m.applyLevels, m.applyLevels >= 3 * 2000);
        assertEquals("[2,3,4]", one("maplist(lt, [1,2,3], L)", "L"));
        assertEquals("6", one("foldl(add, [1,2,3], 0, S)", "S"));
        assertEquals("[1,3]", one("include(odd, [1,2,3], L)", "L"));
        assertEquals("[2]", one("exclude(odd, [1,2,3], L)", "L"));
        assertEquals("[1,3]-[2]", one("partition(odd, [1,2,3], I, E), X = I-E", "X"));
        assertEquals("[1]-[2]-[3]", one("partition([X,O]>>compare(O,X,2), [1,2,3], A, B, C), X = A-B-C", "X"));
        // an open list enumerates lengths; a nondeterministic closure keeps its choice points
        assertEquals("[[],[a],[a,a]]", one("findall(L, limit(3, maplist(=(a), L)), R)", "R"));
        assertEquals("[[1,2],[1,x],[x,2],[x,x]]", one("findall(L, maplist(m2, [1,2], L), R)", "R"));
        // a cut in the closure is local to it (call/N), coroutines wake, errors propagate
        assertEquals("[1]", one("findall(X, maplist([Y]>>(member(Y,[1,2]),!), [X]), L)", "L"));
        ok("L = [A,B], freeze(A, A > 0), maplist(=(1), L)");
        ok("L = [A,B], freeze(A, A > 0), \\+ maplist(=(0), L)");
        assertEquals("caught", one("catch(maplist(lt, [1,a], _), error(type_error(_, _), _), R = caught)", "R"));
        // deterministic on proper lists (Q2.4), also through a module context
        det("maplist(lt, [1,2], _)");
        det("foldl(add, [1,2], 0, _)");
        det("include(odd, [1,2], _)");
        det("partition(odd, [1,2], _, _)");
        prolog.consult(":- module(mq6, [go/1]).\nh(X, Y) :- Y is X * 10.\ngo(L) :- maplist(h, [1,2,3], L).\n");
        assertEquals("[10,20,30]", one("mq6:go(L)", "L"));
    }

    /** Q6.2: the ports are those of the Prolog definition — a nondeterministic closure's Redo and
     *  Fail, and include/3's if-then-else unifications (oracles taken from the 4.6 Q5 build). */
    @Test
    public void testISS0780_ApplyPortsAreThoseOfTheClauses() {
        prolog.consult("m2(X,Y) :- member(Y,[X,x]).\nodd(X) :- X mod 2 =:= 1.\n");
        assertEquals(lines(
            "Call: (0) maplist(m2,[1],L)",
            "  Call: (1) m2(1,_G)",
            "    Call: (2) member(_G,[1,x])",
            "    Exit: (2) member(1,[1,x])",
            "  Exit: (1) m2(1,1)",
            "  Call: (1) maplist(m2,[],_G)",
            "  Exit: (1) maplist(m2,[],[])",
            "Exit: (0) maplist(m2,[1],[1])",
            "    Redo: (2) member(x,[1,x])",
            "    Exit: (2) member(x,[1,x])",
            "  Exit: (1) m2(1,x)",
            "  Call: (1) maplist(m2,[],_G)",
            "  Exit: (1) maplist(m2,[],[])",
            "Exit: (0) maplist(m2,[1],[x])",
            "    Fail: (2) member(_G,[1,x])",
            "  Fail: (1) m2(1,_G)",
            "Fail: (0) maplist(m2,[1],L)"), trace("maplist(m2,[1],L)"));
        assertEquals(lines(
            "Call: (0) include(odd,[1,2],L)",
            "  Call: (1) odd(1)",
            "    Call: (2) 1 mod 2=:=1",
            "    Exit: (2) 1 mod 2=:=1",
            "  Exit: (1) odd(1)",
            "  Call: (1) L=[1|_G]",
            "  Exit: (1) [1|_G]=[1|_G]",
            "  Call: (1) include(odd,[2],_G)",
            "    Call: (2) odd(2)",
            "      Call: (3) 2 mod 2=:=1",
            "      Fail: (3) 2 mod 2=:=1",
            "    Fail: (2) odd(2)",
            "    Call: (2) _G=_G",
            "    Exit: (2) _G=_G",
            "    Call: (2) include(odd,[],_G)",
            "    Exit: (2) include(odd,[],[])",
            "  Exit: (1) include(odd,[2],[])",
            "Exit: (0) include(odd,[1,2],[1])"), trace("include(odd,[1,2],L)"));
    }

    // ------------------------------------------------------------------ Q6.3

    /** Q6.3: the stream half of io is native (LIM-037), with the same behaviour. */
    @Test
    public void testISS0785_StreamBuiltinsAreNative() throws Exception {
        String[] ind = {"open/3", "open/4", "close/1", "close/2", "stream_property/2", "current_stream/3",
            "set_stream/2", "seek/4", "set_stream_position/2", "stream_position/2", "stream_position_data/3",
            "character_count/2", "line_count/2", "line_position/2", "get_byte/1", "get_byte/2",
            "peek_byte/1", "peek_byte/2", "put_byte/1", "put_byte/2", "portray_clause/1", "portray_clause/2"};
        BuiltinTable t = prolog.getV4Engine().natives();
        for (String i : ind) {
            int k = i.lastIndexOf('/');
            assertTrue(i + " must be native", t.isNative(i.substring(0, k), Integer.parseInt(i.substring(k + 1))));
        }
        java.io.File f = java.io.File.createTempFile("q6io", ".bin");
        f.deleteOnExit();
        String fp = f.getAbsolutePath().replace("\\", "/");
        ok("open('" + fp + "', write, S, [type(binary)]), put_byte(S, 65), put_byte(S, 66), close(S)");
        assertEquals("[65,65,66,-1]", one("open('" + fp + "', read, S, [type(binary), alias(q6in)]), "
            + "peek_byte(q6in, A), get_byte(S, B), get_byte(S, C), get_byte(S, D), close(S), L = [A,B,C,D]", "L"));
        assertEquals("read", one("open('" + fp + "', read, S, [type(binary), reposition(true)]), "
            + "current_stream(_, M, S), seek(S, 1, bof, P), get_byte(S, 66), stream_position(S, 2), close(S)", "M"));
        det("stream_property(S, alias(user_output))");
        assertEquals("output", one("stream_property(user_output, P), P == output", "P"));
        ok("aggregate_all(count, stream_property(_, _), N), N > 10");
        ok("catch(stream_property(foo, _), error(existence_error(stream, foo), _), true)");
        ok("catch(stream_property(_, bogus(1)), error(domain_error(stream_property, bogus(1)), _), true)");
        ok("catch(open(_, read, _), error(instantiation_error, _), true)");
        ok("catch(open(f, bogus, _), error(domain_error(io_mode, bogus), _), true)");
        ok("catch(close(foo), error(existence_error(stream, foo), _), true)");
        ok("catch(put_byte(user_output, 300), error(type_error(byte, 300), _), true)");
        ok("line_count(user_output, N), integer(N), character_count(user_output, C), integer(C)");
        ok("stream_position_data(line_count, '$stream_position'(1,2,3,4), 2)");
        ok("with_output_to(string(S), portray_clause((foo(X,Y) :- bar(X), baz(Y,_)))), "
            + "S == \"foo(A, B) :-\\n    bar(A),\\n    baz(B, _).\\n\"");
    }

    // ------------------------------------------------------------------ Q6.4

    /** Q6.4: predsort/3 runs its comparisons on the goal stack — no nested drive per comparison —
     *  with the same answers, failure modes and once/1 semantics per comparison. */
    @Test
    public void testISS0779_PredsortRunsOnTheGoalStack() {
        prolog.consult("cmp(O,A,B) :- compare(O,A,B).\nbad(foo,_,_).\n"
            + "nd(O,A,B) :- member(O,[<,>]), (O == < -> A @< B ; true).\n"
            + "rl(0, _, []) :- !.\nrl(N, S, [X|T]) :- X is S mod 1009, S1 is (S*75+74) mod 65537, N1 is N-1, rl(N1, S1, T).\n");
        Machine m = run("rl(2000, 7, L), predsort(cmp, L, S), msort(L, M), sort(M, S)");
        assertEquals("no nested drive per comparison", 0, m.runOnceCalls);
        assertEquals("[a,b,c]", one("predsort(cmp, [c,a,b,a], L)", "L"));
        ok("\\+ predsort(bad, [b,a], _)");
        assertEquals("[]", one("predsort(cmp, [], L)", "L"));
        assertEquals("[1,2,3]", one("predsort([O,A,B]>>compare(O,A,B), [3,1,2], L)", "L"));
        // a comparator with choice points is called as once/1: exactly one answer
        assertEquals("[[1,2]]", one("findall(L, predsort(nd, [2,1], L), Ls)", "Ls"));
        assertEquals("x", one("catch(predsort([_,_,_]>>throw(x), [2,1], _), E, true)", "E"));
        // bindings made by a comparator are undone after each comparison
        ok("predsort([O,f(A),f(_)]>>(A = 1, O = <), [f(X),f(Y)], _), var(X), var(Y)");
        det("predsort(cmp, [3,1,2], _)");
    }

    /** Q6.4/Q6.5: one candidate clause and nothing traced — no choice point is pushed; and a body
     *  goal that is a native keeps a call site. */
    @Test
    public void testISS0779_SingleClauseActivationAndNativeSites() {
        prolog.consult("app([],L,L).\napp([H|T],L,[H|R]) :- app(T,L,R).\n"
            + "cnt(0) :- !.\ncnt(N) :- succ(M, N), cnt(M).\n");
        Machine m = run("numlist(1, 500, L), app(L, [x], R)");
        assertTrue("indexed recursion needs no choice point, got " + m.singleClauseActivations,
            m.singleClauseActivations >= 500);
        Machine m2 = run("cnt(1000)");
        assertTrue("succ/2 and cnt/1 both take a call site, hits = " + m2.siteHits, m2.siteHits >= 1990);
        // semantics: a clause retracted while its caller runs is still seen (logical update view)
        prolog.consult(":- dynamic(q/1).\nq(1).\n");
        assertEquals("[1]", one("findall(X, (q(X), retract(q(1))), L)", "L"));
    }

    // ------------------------------------------------------------------ extras 1-3 (CLP(FD))

    /** Extras 1 and 3: posting and leftmost-labeling a 4000-variable chain look at each variable
     *  a bounded number of times (the full rescans were O(n^2): ~8 million cells here). */
    @Test
    public void testISS0781_ISS0782_ClpfdWorkIsLinearInTheChange() {
        prolog.consult(":- use_module(library(clpfd)).\nchain([_]).\nchain([A,B|T]) :- A #=< B, chain([B|T]).\n"
            + "go(N, L) :- length(L, N), L ins 1..N, chain(L), once(label(L)).\n");
        final Engine e = prolog.getV4Engine();
        e.natives().register("examined", 1, (mm, a) ->
            mm.unify(a[0], it.denzosoft.jprolog.core.terms.Number.valueOf(
                it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.examinedCells())) ? Builtin.Outcome.SUCCESS : Builtin.Outcome.FAILURE);
        long v0 = ClpfdNative.labelCellsVisited;
        assertEquals("1", one("go(4000, L), L = [F|_], examined(X)", "F"));
        long visited = ClpfdNative.labelCellsVisited - v0;
        long examined = Long.parseLong(one("go(4000, _), examined(X)", "X"));
        assertTrue("determined-cell scans must follow the changes, examined " + examined, examined < 20L * 4000);
        assertTrue("leftmost labeling must not rescan the labeled prefix, visited " + visited, visited < 20L * 4000);
    }

    /** Extra 2: abs(X-Y) #\= C prunes as soon as one side is fixed (it waited for both). */
    @Test
    public void testISS0783_AbsDifferenceDisequalityPropagates() {
        prolog.consult(":- use_module(library(clpfd)).\n"
            + "q(N, Qs) :- length(Qs, N), Qs ins 1..N, s(Qs), labeling([ff], Qs).\n"
            + "s([]). s([Q|Qs]) :- n(Q, Qs, 1), s(Qs).\nn(_, [], _).\n"
            + "n(Q, [Q1|Qs], D) :- Q #\\= Q1, abs(Q - Q1) #\\= D, D1 is D + 1, n(Q, Qs, D1).\n");
        assertEquals("1\\/3\\/5", one("X in 1..5, Y in 1..5, abs(X-Y) #\\= 1, X = 3, fd_dom(Y, D)", "D"));
        assertEquals("0..1\\/3", one("X in 0..3, Y in 0..3, 2 #\\= abs(X-Y), X = 0, fd_dom(Y, D)", "D"));
        ok("X in 1..3, Y in 1..3, abs(X-Y) #\\= 0, X = 2, \\+ Y = 2");
        assertEquals("4", one("findall(Q, q(6, Q), L), length(L, N)", "N"));
        assertEquals("92", one("findall(Q, q(8, Q), L), length(L, N)", "N"));
        // 20 queens: forward checking makes the first solution immediate (it took seconds)
        Machine m = run("once(q(20, Qs))");
        assertTrue("20 queens must not need a long search, steps = " + m.guard().getSteps(),
            m.guard().getSteps() < 2000000);
    }

    // ------------------------------------------------------------------ extra 4

    /** Extra 4: a module load that queries the module after every clause (a module-local
     *  goal_expansion/2) mirrors each clause once — it rebuilt the whole module per clause. */
    @Test
    public void testISS0784_ModuleMirrorIsIncremental() {
        StringBuilder sb = new StringBuilder(":- module(mload, [f/1, g/2]).\ngoal_expansion(nothing_here, true).\n");
        for (int i = 0; i < 1500; i++) sb.append("f(").append(i).append(") :- g(").append(i).append(", _).\n")
            .append("g(").append(i).append(", x").append(i).append(").\n");
        Modules ms = prolog.getV4Engine().modules4();
        long before = ms.mirrorCompiles;
        prolog.consult(sb.toString());
        assertEquals("x1400", one("mload:g(1400, X)", "X"));
        long compiled = ms.mirrorCompiles - before;
        assertTrue("each clause mirrored a bounded number of times, compiled " + compiled, compiled < 4L * 3000);
        // appended clauses of a module without an export list are exported; a declaration made
        // after the clauses (a structural change) is mirrored too
        prolog.consult(":- module(mfree).\nh(1).\n");
        prolog.consult(":- module(mfree).\nh(2).\nk(a).\n");
        assertEquals("[1,2]", one("findall(X, mfree:h(X), L)", "L"));
        assertEquals("a", one("mfree:k(X)", "X"));
        assertEquals("1", one("findall(X, mload:f(1), L), length(L, N)", "N"));
    }

    // ------------------------------------------------------------------ extra 7 (LIM-045)

    /** Extra 7: a bridged library call is charged to the inference budget — a catastrophically
     *  backtracking regular expression is stopped by it (and catch/3 cannot swallow the stop). */
    @Test(timeout = 120000)
    public void testISS0786_BridgedLibrariesAreChargedToTheBudget() {
        StringBuilder a = new StringBuilder();
        for (int i = 0; i < 30; i++) a.append('a');
        // ~2 s of backtracking in java.util.regex on JDK 25 when nothing meters it
        String evil = "catch(re_match('(.*a){10}x', '" + a + "'), _, true)";
        prolog.setInferenceBudget(200000);
        try {
            prolog.solve(evil + ".");
            org.junit.Assert.fail("the budget must stop the regular expression");
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException expected) {
            // the trust model: a control exception, never a catchable ball
        }
        // the text input of a bridged call is charged too (one step per 64 characters)
        StringBuilder big = new StringBuilder("[");
        for (int i = 0; i < 20000; i++) big.append(i == 0 ? "" : ",").append(i);
        big.append(']');
        prolog.setInferenceBudget(1000);
        try {
            prolog.solve("json_parse('" + big + "', _).");
            org.junit.Assert.fail("a 100 KB json text is more than 1000 steps");
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException expected) {
        }
        prolog.setInferenceBudget(0);
        ok("re_match('^(a+)+$', aaa), json_parse('[1,2]', _)");
    }

    // ------------------------------------------------------------------ extra 8

    private static final String BN = "app([],L,L).\napp([H|T],L,[H|R]) :- app(T,L,R).\n"
        + "nrev([],[]).\nnrev([H|T],R) :- nrev(T,RT), app(RT,[H],R).\n"
        + "range(N,N,[N]) :- !.\nrange(I,N,[I|T]) :- I<N, I1 is I+1, range(I1,N,T).\n"
        + "bn(0) :- trail_size(S), nb_setval(q6trail, S), !.\n"
        + "bn(K) :- range(1,30,L), nrev(L,_), K1 is K-1, bn(K1).\n";

    /** Extra 8 (the Q2 report, reproduced under trace/0 and the debugger, not in plain runs): a
     *  deterministic recursion whose frames are kept for their Exit ports trailed nearly every
     *  binding and never dropped the entries — ~60 KB of dead terms per level. Popping a
     *  deterministic frame now tidies the trail; the debugger's call stack is persistent. */
    @Test
    public void testISS0787_TracedDeterministicRecursionKeepsTheTrailSmall() {
        prolog.consult(BN);
        prolog.getV4Engine().natives().register("trail_size", 1, (mm, a) ->
            mm.unify(a[0], it.denzosoft.jprolog.core.terms.Number.valueOf(mm.trailSize()))
                ? Builtin.Outcome.SUCCESS : Builtin.Outcome.FAILURE);
        java.io.PrintStream nul = new java.io.PrintStream(new java.io.OutputStream() {
            @Override public void write(int b) { }
            @Override public void write(byte[] b, int o, int l) { }
        });
        java.io.PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(nul);
        long trail;
        try {
            prolog.setTracing(true);
            ok("bn(1500)");
        } finally {
            prolog.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        trail = Long.parseLong(one("nb_getval(q6trail, S)", "S"));
        // 1500 levels x ~500 bindings each were all on the trail at the bottom of the recursion
        assertTrue("the trail must not keep the dead bindings of every level, size " + trail, trail < 20L * 1500);

        // the debugger: the same run with a listening controller, and a correct call stack
        it.denzosoft.jprolog.core.engine.DebugController dc = new it.denzosoft.jprolog.core.engine.DebugController();
        dc.setCurrentMode(it.denzosoft.jprolog.core.engine.DebugEvent.Action.CONTINUE);
        dc.setTraceEnabled(true);
        final int[] maxStack = {0};
        final boolean[] consistent = {true};
        dc.setListener(new it.denzosoft.jprolog.core.engine.DebugController.DebugListener() {
            @Override public void onDebugPaused(it.denzosoft.jprolog.core.engine.DebugEvent e) { }
            @Override public void onTraceEvent(it.denzosoft.jprolog.core.engine.DebugEvent e) {
                if (e.getPort() == it.denzosoft.jprolog.core.engine.DebugEvent.Port.CALL && e.getDepth() < 5) {
                    List<it.denzosoft.jprolog.core.engine.DebugStackEntry> st = e.getCallStack();
                    if (st.isEmpty() || st.get(st.size() - 1).getDepth() != e.getDepth()) consistent[0] = false;
                    maxStack[0] = Math.max(maxStack[0], st.size());
                }
            }
            @Override public void onDebugFinished() { }
        });
        prolog.getEngineContext().setDebugController(dc);
        try {
            ok("bn(1500)");
        } finally {
            prolog.getEngineContext().setDebugController(null);
        }
        trail = Long.parseLong(one("nb_getval(q6trail, S)", "S"));
        assertTrue("debugged: trail size " + trail, trail < 20L * 1500);
        assertTrue("the call stack of an event ends with the event's own call", consistent[0]);
        assertTrue(maxStack[0] >= 3);
    }

    // ------------------------------------------------------------------ extras 5, 6

    /** Extra 5: the table space is looked up once per machine, not once per negation. */
    @Test
    public void testISS0776_NegationInATabledEvaluationDoesNotLookUpTheSpace() {
        prolog.consult(":- table t/1.\nt(X) :- between(1, 2000, X), \\+ bad(X).\nbad(0).\n");
        Machine m = run("( t(_), fail ; true )");
        assertTrue("one per-thread lookup per machine, got " + m.tablingLookups, m.tablingLookups <= 2);
        assertEquals("2000", one("aggregate_all(count, t(_), N)", "N"));
    }

    /** Extra 6: current_op/3 and catch/3 announce their last alternative. */
    @Test
    public void testISS0775_DeterministicGeneratorsLeaveNoChoicePoint() {
        det("current_op(P, T, '|')");
        det("current_op(P, T, mod)");
        det("current_op(700, xfx, =)");
        det("catch(true, _, true)");
        det("catch(member(X, [a]), _, true)");
        assertEquals("2", one("aggregate_all(count, current_op(_, _, -), N)", "N"));
        assertEquals("[a,b]", one("findall(X, catch(member(X, [a,b]), _, true), L)", "L"));
        // a catch that is still open (a choice point inside Goal) keeps catching on backtracking
        assertEquals("caught", one("catch((member(X, [1,2]), X > 1, throw(t)), t, R = caught), true", "R"));
    }
}
// END_CHANGE: ISS-2025-0775..0789
