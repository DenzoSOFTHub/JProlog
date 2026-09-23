package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.DebugEvent;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0481 - engine v4 wave W8 (design B.6, limits L-13 and the trace-memory
// half of ISS-2025-0482): the tracer and the debugger no longer force the bridge path.
/**
 * The <b>trace oracle</b>: the exact four-port output of a representative program set — user
 * predicates, inline built-ins, backtracking with Redo/Fail, cut, negation, catch/throw, findall,
 * once/ignore/forall/between, the native list library, coroutining, tabling, maplist and the
 * database — pinned line for line.
 *
 * <p>It exists because wave W8 re-enabled the machine's fast paths under trace and debug (before
 * it, attaching a {@code DebugController} silently rerouted {@code =/2}, {@code is/2}, the
 * comparisons and the type checks through the legacy bridge, i.e. a debugged run executed
 * different code from an undebugged one). The ports are now emitted by the machine itself, and
 * this class is what proves the resulting trace is the intended one and stays that way.
 *
 * <p>Fresh-variable names are normalised ({@code _G17} -> {@code _G}) because the cell serial is
 * JVM-global; everything else — port, depth, indentation, goal text and ORDER — is exact.
 *
 * <p>What changed against the pre-W8 output, deliberately (see section 14 of
 * {@code docs/reports/report-engine-v4-progress.md}):
 * <ol>
 *   <li>the inline built-ins ({@code =/2}, {@code is/2}, the six arithmetic comparisons, the
 *       standard-order comparisons, {@code \=/2} and the type checks) now have Call/Exit/Fail
 *       ports under {@code trace/0} — before they had none at all, while the IDE debugger did see
 *       them through the bridge;</li>
 *   <li>the depth is the machine's CALL depth, not the choice-point height, so the trace nests
 *       (and step-over/step-out have a depth to compare against);</li>
 *   <li>a deterministic frame — one that handed out a single alternative and is exhausted — is
 *       trust-me popped even while tracing, so it emits no phantom {@code Fail} after its
 *       {@code Exit}. That is SWI's last-call behaviour and it is what keeps trace memory linear
 *       in the number of OPEN calls instead of in the number of inferences.</li>
 * </ol>
 */
public class EngineV4TraceTest {


    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    // ------------------------------------------------------------------ harness

    /** The four-port text {@code trace/0} writes for {@code query}, variable names normalised. */
    private String trace(String program, String query) {
        Prolog p = new Prolog();
        if (!program.isEmpty()) p.consult(program);
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(bos, true);
        PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            p.setTracing(true);
            p.solve(query);
        } finally {
            p.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        return normalise(bos.toString());
    }

    /** The same ports as the IDE debugger sees them: {@code port|depth|goal} per line. */
    private String debugPorts(String program, String query) {
        Prolog p = new Prolog();
        if (!program.isEmpty()) p.consult(program);
        final List<String> events = new ArrayList<String>();
        DebugController dc = new DebugController();
        dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        dc.setTraceEnabled(true);
        dc.setListener(new DebugController.DebugListener() {
            @Override public void onDebugPaused(DebugEvent e) { }
            @Override public void onTraceEvent(DebugEvent e) {
                events.add(e.getPort() + "|" + e.getDepth() + "|"
                    + it.denzosoft.jprolog.core.util.TermFormatter.format(e.getGoal(), false, false, false, 1200));
            }
            @Override public void onDebugFinished() { }
        });
        p.getEngineContext().setDebugController(dc);
        try {
            p.solve(query);
        } finally {
            p.getEngineContext().setDebugController(null);
        }
        StringBuilder sb = new StringBuilder();
        for (String e : events) sb.append(e).append("\n");
        return normalise(sb.toString());
    }

    private static String normalise(String s) {
        return s.replaceAll("_G[0-9]+", "_G").replace("\r\n", "\n").trim();
    }

    private static String lines(String... ls) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < ls.length; i++) {
            if (i > 0) sb.append('\n');
            sb.append(ls[i]);
        }
        return sb.toString();
    }


    // ------------------------------------------------------------------ the pinned oracle


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_UserPred() {
        assertEquals(lines(
            "Call: (0) app([1,2],[3],X)",
            "  Call: (1) app([2],[3],_G)",
            "    Call: (2) app([],[3],_G)",
            "    Exit: (2) app([],[3],[3])",
            "  Exit: (1) app([2],[3],[2,3])",
            "Exit: (0) app([1,2],[3],[1,2,3])"
        ), trace("app([],L,L).\napp([H|T],L,[H|R]) :- app(T,L,R).\n",
                 "app([1,2],[3],X)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_BacktrackRedoFail() {
        assertEquals(lines(
            "Call: (0) q(X)",
            "  Call: (1) p(X)",
            "  Exit: (1) p(1)",
            "  Call: (1) 1>2",
            "  Fail: (1) 1>2",
            "  Redo: (1) p(2)",
            "  Exit: (1) p(2)",
            "  Call: (1) 2>2",
            "  Fail: (1) 2>2",
            "  Redo: (1) p(3)",
            "  Exit: (1) p(3)",
            "  Call: (1) 3>2",
            "  Exit: (1) 3>2",
            // ISS-2025-0668 (4.5 P7.8): no phantom Fail after a deterministic exit (SWI)
            "Exit: (0) q(3)"
        ), trace("p(1).\np(2).\np(3).\nq(X) :- p(X), X > 2.\n",
                 "q(X)"));
    }


    /** ISS-2025-0668 (4.5 P7.8): a failure inside a single-clause predicate's body reaches its
     *  frame, so the predicate's own Fail port is printed (it was lost: the frame had been dropped
     *  at activation, so only `Fail: (1) p(X)` appeared and never `Fail: (0) q(X)`). */
    @Test
    public void testISS0668_Trace_ParentFailIsPrinted() {
        assertEquals(lines(
            "Call: (0) q(X)",
            "  Call: (1) p(X)",
            "  Exit: (1) p(1)",
            "  Call: (1) 1>5",
            "  Fail: (1) 1>5",
            "  Redo: (1) p(2)",
            "  Exit: (1) p(2)",
            "  Call: (1) 2>5",
            "  Fail: (1) 2>5",
            "Fail: (0) q(X)"
        ), trace("p(1).\np(2).\nq(X) :- p(X), X > 5.\n",
                 "q(X)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_Cut() {
        assertEquals(lines(
            "Call: (0) m(3,5,Z)",
            "  Call: (1) 3>=5",
            "  Fail: (1) 3>=5",
            "Redo: (0) m(3,5,5)",
            // ISS-2025-0668 (4.5 P7.8): no phantom Fail after a deterministic exit (SWI)
            "Exit: (0) m(3,5,5)"
        ), trace("m(X,Y,X) :- X >= Y, !.\nm(_,Y,Y).\n",
                 "m(3,5,Z)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_BuiltinsInline() {
        assertEquals(lines(
            "Call: (0) X=1",
            "Exit: (0) 1=1",
            "Call: (0) Y is 1+2",
            "Exit: (0) 3 is 1+2",
            "Call: (0) 3>2",
            "Exit: (0) 3>2",
            "Call: (0) integer(3)",
            "Exit: (0) integer(3)",
            "Call: (0) atom(foo)",
            "Exit: (0) atom(foo)"
        ), trace("",
                 "X = 1, Y is X+2, Y > 2, integer(Y), atom(foo)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_BuiltinsInlineFail() {
        assertEquals(lines(
            "Call: (0) X=1",
            "Exit: (0) 1=1",
            "Call: (0) 1>5",
            "Fail: (0) 1>5"
        ), trace("",
                 "X = 1, X > 5"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_Negation() {
        assertEquals(lines(
            "Call: (0) r(b)",
            "Fail: (0) r(b)"
        ), trace("r(a).\n",
                 "\\+ r(b)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_CatchThrow() {
        // START_CHANGE: ISS-2025-0528 - wave P1.15, a deliberate oracle change: the goal a ball
        // unwinds now reports an Exception port (it used to vanish after its Call).
        assertEquals(lines(
            "Call: (0) boom",
            "Exception: (0) boom"
        ), trace("boom :- throw(oops).\n",
                 "catch(boom, E, true)"));
        // END_CHANGE: ISS-2025-0528
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_Findall() {
        assertEquals(lines(
            "Call: (0) s(X)",
            "Exit: (0) s(1)",
            "Redo: (0) s(2)",
            // ISS-2025-0668 (4.5 P7.8): no phantom Fail after a deterministic exit (SWI)
            "Exit: (0) s(2)"
        ), trace("s(1).\ns(2).\n",
                 "findall(X, s(X), L)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_BetweenOnce() {
        assertEquals(lines(
            "Call: (0) once(between(1,3,X))",
            "  Call: (1) between(1,3,X)",
            "  Exit: (1) between(1,3,1)",
            // ISS-2025-0668 (4.5 P7.8): no phantom Fail after a deterministic exit (SWI)
            "Exit: (0) once(between(1,3,1))"
        ), trace("",
                 "once(between(1,3,X))"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_ForallIgnore() {
        assertEquals(lines(
            "Call: (0) forall(t(X),X>0)",
            "  Call: (1) t(X)",
            "  Exit: (1) t(1)",
            "  Call: (1) 1>0",
            "  Exit: (1) 1>0",
            "  Redo: (1) t(2)",
            "  Exit: (1) t(2)",
            "  Call: (1) 2>0",
            // ISS-2025-0668 (4.5 P7.8): no phantom Fail after a deterministic exit (SWI)
            // (t(2) is the last clause, so t(X) is not re-entered: no `Fail: (1) t(X)` either)
            "  Exit: (1) 2>0",
            "Exit: (0) forall(t(X),X>0)",
            "Call: (0) ignore(t(9))",
            "  Call: (1) t(9)",
            "  Fail: (1) t(9)",
            "Exit: (0) ignore(t(9))"
        ), trace("t(1).\nt(2).\n",
                 "forall(t(X), X > 0), ignore(t(9))"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_Listlib() {
        assertEquals(lines(
            "Call: (0) member(X,[a,b])",
            "Exit: (0) member(a,[a,b])",
            "Call: (0) append([1],[2],L)",
            "Exit: (0) append([1],[2],[1,2])",
            "Redo: (0) member(b,[a,b])",
            "Exit: (0) member(b,[a,b])",
            "Call: (0) append([1],[2],L)",
            "Exit: (0) append([1],[2],[1,2])",
            "Fail: (0) member(X,[a,b])"
        ), trace("",
                 "member(X,[a,b]), append([1],[2],L)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_Coroutining() {
        assertEquals(lines(
            "Call: (0) freeze(X,Y=fired)",
            "  Call: (1) var(X)",
            "  Exit: (1) var(X)",
            "  Call: (1) get_attr(X,freeze,_G)",
            "  Fail: (1) get_attr(X,freeze,_G)",
            "  Call: (1) put_attr(X,freeze,Y=fired)",
            "  Exit: (1) put_attr(X,freeze,Y=fired)",
            "Exit: (0) freeze(X,Y=fired)",
            "Call: (0) X=1",
            "Exit: (0) 1=1",
            "Call: (0) $attr_unify(freeze,Y=fired,1,X)",
            "Exit: (0) $attr_unify(freeze,Y=fired,1,X)",
            "Call: (0) $attr_hook(freeze,Y=fired,1,X)",
            "  Call: (1) var(1)",
            "  Fail: (1) var(1)",
            "  Call: (1) Y=fired",
            "  Exit: (1) fired=fired",
            "Exit: (0) $attr_hook(freeze,fired=fired,1,X)"
        ), trace("",
                 "freeze(X, Y = fired), X = 1"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_Tabling() {
        assertEquals(lines(
            "Call: (0) pa(1,3)",
            "  Call: (1) ed(1,3)",
            "  Fail: (1) ed(1,3)",
            "  Call: (1) pa(1,_G)",
            "    Call: (2) ed(1,_G)",
            "    Exit: (2) ed(1,2)",
            "    Call: (2) pa(1,_G)",
            "    Exit: (2) pa(1,2)",
            "    Call: (2) ed(2,_G)",
            "    Exit: (2) ed(2,3)",
            "    Redo: (2) pa(1,3)",
            "    Exit: (2) pa(1,3)",
            "    Call: (2) ed(3,_G)",
            "    Fail: (2) ed(3,_G)",
            "    Fail: (2) pa(1,_G)",
            "    Call: (2) ed(1,_G)",
            "    Exit: (2) ed(1,2)",
            "    Call: (2) pa(1,_G)",
            "    Exit: (2) pa(1,2)",
            "    Call: (2) ed(2,_G)",
            "    Exit: (2) ed(2,3)",
            "    Redo: (2) pa(1,3)",
            "    Exit: (2) pa(1,3)",
            "    Call: (2) ed(3,_G)",
            "    Fail: (2) ed(3,_G)",
            "    Fail: (2) pa(1,_G)",
            "  Exit: (1) pa(1,2)",
            "  Call: (1) ed(2,3)",
            "  Exit: (1) ed(2,3)",
            "  Redo: (1) pa(1,3)",
            "  Exit: (1) pa(1,3)",
            "  Call: (1) ed(3,3)",
            "  Fail: (1) ed(3,3)",
            "  Fail: (1) pa(1,_G)",
            "  Call: (1) ed(1,3)",
            "  Fail: (1) ed(1,3)",
            "  Call: (1) pa(1,_G)",
            "  Exit: (1) pa(1,2)",
            "  Call: (1) ed(2,3)",
            "  Exit: (1) ed(2,3)",
            "  Redo: (1) pa(1,3)",
            "  Exit: (1) pa(1,3)",
            "  Call: (1) ed(3,3)",
            "  Fail: (1) ed(3,3)",
            "  Fail: (1) pa(1,_G)",
            "Exit: (0) pa(1,3)"
        ), trace(":- table pa/2.\ned(1,2).\ned(2,3).\npa(X,Y) :- ed(X,Y).\npa(X,Y) :- pa(X,Z), ed(Z,Y).\n",
                 "pa(1,3)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_Ifthenelse() {
        assertEquals(lines(
            "Call: (0) u(X)",
            "Exit: (0) u(1)",
            "Call: (0) Y=yes",
            "Exit: (0) yes=yes"
        ), trace("u(1).\n",
                 "( u(X) -> Y = yes ; Y = no )"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_Maplist() {
        assertEquals(lines(
            "Call: (0) maplist(dbl,[1,2],L)",
            "  Call: (1) dbl(1,_G)",
            "    Call: (2) _G is 1*2",
            "    Exit: (2) 2 is 1*2",
            "  Exit: (1) dbl(1,2)",
            // ISS-2025-0668 (4.5 P7.8): the engine-internal '$mctx'(user, dbl) wrapper no longer
            // leaks into the ports, and maplist(dbl, [], _) is deterministic (no Fail after the
            // top-level Exit): the traced frame looks ahead for a clause that can still match
            "  Call: (1) maplist(dbl,[2],_G)",
            "    Call: (2) dbl(2,_G)",
            "      Call: (3) _G is 2*2",
            "      Exit: (3) 4 is 2*2",
            "    Exit: (2) dbl(2,4)",
            "    Call: (2) maplist(dbl,[],_G)",
            "    Exit: (2) maplist(dbl,[],[])",
            "  Exit: (1) maplist(dbl,[2],[4])",
            "Exit: (0) maplist(dbl,[1,2],[2,4])"
        ), trace("dbl(X,Y) :- Y is X*2.\n",
                 "maplist(dbl,[1,2],L)"));
    }


    /** Trace oracle for the query in the body. */
    @Test
    public void testISS0481_Trace_AssertRetract() {
        assertEquals(lines(
            "Call: (0) c(X)",
            "Exit: (0) c(1)"
        ), trace(":- dynamic(c/1).\n",
                 "assertz(c(1)), c(X), retract(c(1))"));
    }


    // ------------------------------------------------------------------ the debugger sees the same


    /** The IDE debugger's port stream is the SAME stream, with the same depths. */
    @Test
    public void testISS0481_DebuggerSeesTheSamePortsAsTheTracer() {
        String ports = debugPorts("p(1).\np(2).\np(3).\nq(X) :- p(X), X > 2.\n", "q(X)");
        assertEquals(lines(
            "call|0|q(X)",
            "call|1|p(X)",
            "exit|1|p(1)",
            "call|1|1>2",
            "fail|1|1>2",
            "redo|1|p(2)",
            "exit|1|p(2)",
            "call|1|2>2",
            "fail|1|2>2",
            "redo|1|p(3)",
            "exit|1|p(3)",
            "call|1|3>2",
            "exit|1|3>2",
            // ISS-2025-0668 (4.5 P7.8): no phantom Fail after a deterministic exit (SWI)
            "exit|0|q(3)"
        ), ports);
    }

    /**
     * L-13: attaching a {@code DebugController} must not change WHICH code runs. Before wave W8 the
     * inline table was skipped whenever one was attached, so {@code =/2} and {@code is/2} took the
     * legacy bridge only while debugging.
     */
    @Test
    public void testISS0481_InlineBuiltinsArePortedNotBridged() {
        String ports = debugPorts("", "X = 1, Y is X+2, integer(Y)");
        assertEquals(lines(
            "call|0|X=1",
            "exit|0|1=1",
            "call|0|Y is 1+2",
            "exit|0|3 is 1+2",
            "call|0|integer(3)",
            "exit|0|integer(3)"
        ), ports);
    }

    // ------------------------------------------------------------------ cost of tracing

    /**
     * ISS-2025-0482: a deterministic frame is trust-me popped even while tracing, so a traced
     * deterministic recursion no longer retains one choice point per inference. Before the fix
     * {@code loop(200000)} under trace did not finish inside this timeout.
     */
    @Test(timeout = 120000)
    public void testISS0482_TracedDeterministicRecursionIsNotQuadratic() {
        Prolog p = new Prolog();
        p.consult("loop(0).\nloop(N) :- N > 0, N1 is N-1, loop(N1).\n");
        PrintStream sink = new PrintStream(new java.io.OutputStream() {
            @Override public void write(int b) { }
        });
        PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(sink);
        long traced;
        try {
            p.setTracing(true);
            long t0 = System.nanoTime();
            assertEquals(1, p.solve("loop(200000).").size());
            traced = (System.nanoTime() - t0) / 1000000L;
        } finally {
            p.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        assertTrue("traced loop(200000) took " + traced + " ms", traced < 100000);
    }

    /**
     * The trace indentation is capped (ISS-2025-0482). The depth itself is exact, but a 200 000-deep
     * tail recursion must not print a 400 000-character indent — that is quadratic output.
     */
    @Test(timeout = 60000)
    public void testISS0482_TraceIndentationIsCapped() {
        String out = trace("d(0).\nd(N) :- N > 0, N1 is N-1, d(N1).\n", "d(60)");
        int longest = 0;
        for (String line : out.split("\n")) {
            int i = 0;
            while (i < line.length() && line.charAt(i) == ' ') i++;
            if (i > longest) longest = i;
        }
        assertTrue("indent " + longest + " is not capped", longest <= 80);
        assertTrue("the real depth is still reported:\n" + out, out.contains("(60)") || out.contains("(59)"));
    }

    // ------------------------------------------------------------------ the IDE debugger contract

    /**
     * The IDE's two-thread model, exercised programmatically (the GUI cannot be driven from a
     * test): the query runs on a background thread, the "EDT" (this thread) sees the pause through
     * the listener and resumes it. This is the contract CLAUDE.md describes — breakpoints by
     * clause source line, stepping, a variable snapshot in the paused event, and Stop.
     */
    private static final class Session {
        final Prolog p;
        final DebugController dc = new DebugController();
        final java.util.concurrent.BlockingQueue<DebugEvent> paused =
            new java.util.concurrent.LinkedBlockingQueue<DebugEvent>();
        final List<String> ports = java.util.Collections.synchronizedList(new ArrayList<String>());
        volatile Throwable thrown;
        Thread solver;

        Session(String program) {
            p = new Prolog();
            if (!program.isEmpty()) p.consultWithDiagnostics(program, "test.pl");
            dc.setTraceEnabled(true);
            dc.setListener(new DebugController.DebugListener() {
                @Override public void onDebugPaused(DebugEvent e) { paused.add(e); }
                @Override public void onTraceEvent(DebugEvent e) {
                    ports.add(e.getPort() + "|" + e.getDepth() + "|" + e.getGoal());
                }
                @Override public void onDebugFinished() { }
            });
            p.getEngineContext().setDebugController(dc);
        }

        void start(final String query) {
            solver = new Thread(new Runnable() {
                @Override public void run() {
                    try { p.solve(query); } catch (Throwable t) { thrown = t; }
                }
            }, "test-solver");
            solver.start();
        }

        DebugEvent awaitPause() throws InterruptedException {
            DebugEvent e = paused.poll(20, java.util.concurrent.TimeUnit.SECONDS);
            assertTrue("no pause within 20 s", e != null);
            return e;
        }

        void finish() throws InterruptedException {
            dc.resumeWithAction(DebugEvent.Action.CONTINUE);
            solver.join(20000);
        }
    }

    /** Breakpoints are set by predicate indicator, which the IDE derives from a clause source line. */
    @Test(timeout = 60000)
    public void testISS0481_BreakpointByClauseSourceLine() throws Exception {
        Session s = new Session("a(1).\nb(X) :- a(X).\nc(X) :- b(X).\n");
        // line 2 is the clause `b(X) :- a(X).` — this is what the IDE's gutter uses
        String pi = s.p.getPredicateIndicatorAtLine(2);
        assertEquals("b/1", pi);
        s.dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        s.dc.addBreakpoint(pi);
        s.start("c(X).");
        DebugEvent e = s.awaitPause();
        assertEquals("b/1", e.getGoal().getName() + "/" + e.getGoal().getArguments().size());
        assertEquals(DebugEvent.Port.CALL, e.getPort());
        s.finish();
        assertTrue("the query must complete", s.thrown == null);
    }

    /** A paused event carries a RESOLVED goal — the IDE renders it on the EDT, later. */
    @Test(timeout = 60000)
    public void testISS0481_PausedEventCarriesAVariableSnapshot() throws Exception {
        Session s = new Session("a(42).\nb(X) :- a(X).\n");
        s.dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        s.dc.addBreakpoint("a/1");
        s.start("b(Y).");
        DebugEvent e = s.awaitPause();
        // still unbound at the CALL port — the query variable keeps its own name
        assertEquals("a(Y)",
            it.denzosoft.jprolog.core.util.TermFormatter.format(e.getGoal(), false, false, false, 1200));
        // the EXIT port of the same goal shows the binding
        s.dc.resumeWithAction(DebugEvent.Action.STEP_INTO);
        DebugEvent exit = s.awaitPause();
        assertEquals(DebugEvent.Port.EXIT, exit.getPort());
        assertEquals("a(42)",
            it.denzosoft.jprolog.core.util.TermFormatter.format(exit.getGoal(), false, false, false, 1200));
        s.finish();
    }

    /**
     * Step-over compares the event's DEPTH against a target, so the depth has to be a real call
     * depth (ISS-2025-0482). Stepping over `b/1` must not stop inside `a/1`.
     */
    @Test(timeout = 60000)
    public void testISS0482_StepOverUsesTheCallDepth() throws Exception {
        Session s = new Session("a(1).\nb(X) :- a(X).\nc(X) :- b(X), d(X).\nd(_).\n");
        s.dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        s.dc.addBreakpoint("b/1");
        s.start("c(X).");
        DebugEvent atB = s.awaitPause();
        assertEquals(DebugEvent.Port.CALL, atB.getPort());
        int depthOfB = atB.getDepth();
        s.dc.resumeWithAction(DebugEvent.Action.STEP_OVER);
        DebugEvent next = s.awaitPause();
        assertTrue("step-over stopped INSIDE b/1 (depth " + next.getDepth()
                   + " > " + depthOfB + "): " + next.getGoal(),
                   next.getDepth() <= depthOfB);
        s.finish();
    }

    /** Stop unwinds the query with a {@code DebugStopException}, which is NOT a PrologException. */
    @Test(timeout = 60000)
    public void testISS0481_StopRaisesDebugStopException() throws Exception {
        Session s = new Session("loop(0).\nloop(N) :- N > 0, N1 is N-1, loop(N1).\n");
        s.dc.setCurrentMode(DebugEvent.Action.CONTINUE);
        s.dc.addBreakpoint("loop/1");
        s.start("loop(100000).");
        s.awaitPause();
        s.dc.stop();
        s.solver.join(20000);
        assertFalse("the solver thread must have stopped", s.solver.isAlive());
        assertTrue("expected DebugStopException, got " + s.thrown,
                   s.thrown instanceof DebugController.DebugStopException);
    }

    /**
     * The RunPanel's output capture: a background solve's output goes to the thread-local stream,
     * and the main thread's output is untouched. That is the whole IDE console contract.
     */
    @Test(timeout = 60000)
    public void testISS0481_BackgroundSolveOutputIsCapturedPerThread() throws Exception {
        final Prolog p = new Prolog();
        final ByteArrayOutputStream bg = new ByteArrayOutputStream();
        final Throwable[] err = new Throwable[1];
        Thread t = new Thread(new Runnable() {
            @Override public void run() {
                PrintStream ps = new PrintStream(bg, true);
                it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
                try { p.solve("write(from_worker), nl."); }
                catch (Throwable e) { err[0] = e; }
                finally { it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(null); }
            }
        }, "run-panel");
        t.start();
        t.join(20000);
        assertTrue(String.valueOf(err[0]), err[0] == null);
        assertEquals("from_worker", bg.toString().trim());
    }
}
// END_CHANGE: ISS-2025-0481
