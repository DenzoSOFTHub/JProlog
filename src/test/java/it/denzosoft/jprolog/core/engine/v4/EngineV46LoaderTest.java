package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0730..0739 - 4.6 wave Q3: loader, modules and reader residue.
/**
 * Wave Q3 of the 4.6 completeness program (report-completeness-4.6-2026-09-23.md §3/§12):
 * multifile/1 and clause ownership (Q3.1), goal_expansion/2 (Q3.2), file_search_path/2 and
 * absolute_file_name/2,3 (Q3.3), use_module/2 import lists (Q3.4), the bar and `as` operators
 * (Q3.5), make/0 over included files and {@code Prolog.runMain()} (Q3.6), subterm positions and
 * comments (Q3.7), the per-file load lock (Q3.8), and the module extras found while verifying Q2
 * (module-qualified clause heads, current_predicate(M:PI), the legacy DCG translator). Every
 * method fails on the 4.6 wave Q2 classes.
 */
public class EngineV46LoaderTest {

    private Prolog prolog;
    private File dir;

    @Before
    public void setUp() throws Exception {
        prolog = new Prolog();
        dir = Files.createTempDirectory("q3load").toFile();
    }

    @After
    public void tearDown() {
        deleteTree(dir);
    }

    private static void deleteTree(File f) {
        File[] kids = f.listFiles();
        if (kids != null) for (File k : kids) deleteTree(k);
        f.delete();
    }

    private File write(String name, String text) throws Exception {
        File f = new File(dir, name);
        f.getParentFile().mkdirs();
        Files.write(f.toPath(), text.getBytes(StandardCharsets.UTF_8));
        return f;
    }

    private static String path(File f) {
        return f.getAbsolutePath().replace("\\", "/");
    }

    private void ok(String goal) {
        assertFalse("expected success: " + goal, prolog.solve(goal + ".").isEmpty());
    }

    private void no(String goal) {
        assertTrue("expected failure: " + goal, prolog.solve(goal + ".").isEmpty());
    }

    private String first(String goal, String var) {
        List<Map<String, Term>> r = prolog.solve(goal + ".");
        assertFalse("expected success: " + goal, r.isEmpty());
        return Writer.format(r.get(0).get(var), Writer.Options.writeq(), 1200);
    }

    /** What running {@code r} writes on System.err. */
    private static String stderrOf(Runnable r) {
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        PrintStream prev = System.err;
        System.setErr(new PrintStream(err, true));
        try {
            r.run();
        } finally {
            System.setErr(prev);
        }
        return new String(err.toByteArray(), StandardCharsets.UTF_8);
    }

    // ------------------------------------------------------------------ Q3.1 multifile/ownership

    @Test
    public void testISS0730_ReconsultRemovesOnlyItsOwnClauses() throws Exception {
        File a = write("a.pl", ":- multifile p/1.\np(a1).\nq(1).\n");
        File b = write("b.pl", ":- multifile p/1.\np(b1).\n");
        File c = write("c.pl", "q(3).\n");
        prolog.consultFile(a.getAbsolutePath());
        prolog.consultFile(b.getAbsolutePath());
        prolog.consultFile(c.getAbsolutePath());
        ok("assertz(q(9)), assertz(p(x9))");
        assertEquals("[a1,b1,x9]", first("findall(X, p(X), L)", "L"));
        write("a.pl", ":- multifile p/1.\np(a2).\nq(2).\n");
        prolog.consultFile(a.getAbsolutePath());
        // b.pl's clause of the multifile predicate survives, and so does c.pl's clause of q/1;
        // an asserted clause survives for the multifile p/1 only (a reload resets q/1)
        // ISS-2025-0794 (4.6 Q7): the reloaded clause keeps a.pl's place (SWI manual 4.3.2) — it
        // was appended ([b1,x9,a2])
        assertEquals("[a2,b1,x9]", first("findall(X, p(X), L)", "L"));
        assertEquals("[3,2]", first("findall(X, q(X), L)", "L"));
        ok("predicate_property(p(_), multifile)");
        no("predicate_property(q(_), multifile)");
    }

    @Test
    public void testISS0730_MultifileDefinesThePredicate() {
        prolog.consult(":- multifile hook/1.\n:- multifile user:uhook/3.\n:- multifile other:ohook/1.\n");
        // declared, no clause: the call FAILS (SWI) instead of raising existence_error
        no("hook(_)");
        no("uhook(_, _, _)");
        no("other:ohook(_)");
        ok("current_predicate(hook/1)");
        ok("current_predicate(other:ohook/1)");
        ok("multifile(dyn_mf/2), \\+ dyn_mf(_, _)");
        assertEquals("error(existence_error(procedure,nothere/0),'nothere/0')",
            first("catch(nothere, E, true)", "E"));
    }

    @Test
    public void testISS0730_DiscontiguousWarning() {
        String w = stderrOf(() -> prolog.consult("d1(1).\nd2(1).\nd1(2).\n"));
        assertTrue(w, w.contains("Clauses of d1/1 are not together in the source-file"));
        String quiet = stderrOf(() -> prolog.consult(":- discontiguous e1/1.\ne1(1).\ne2(1).\ne1(2).\n"));
        assertFalse(quiet, quiet.contains("not together"));
        assertEquals("[1,2]", first("findall(X, e1(X), L)", "L"));
    }

    // ------------------------------------------------------------------ Q3.2 goal_expansion/2

    @Test
    public void testISS0731_GoalExpansion() {
        prolog.consult(
            "goal_expansion(dbl(X, Y), Y is X * 2).\n"
          + "goal_expansion(twice(G), (G, G)).\n"
          + "goal_expansion(same(X), same(X)).\n"
          + "p(A, B) :- dbl(A, B).\n"
          + "q(L) :- findall(Y, (member(X, [1,2]), dbl(X, Y)), L).\n"
          + "r(N) :- twice(nb_setval(k, 1)), (dbl(1, N) -> true ; N = none).\n"
          + "s(X) :- same(X).\n"
          + ":- dbl(3, Z), nb_setval(dirres, Z).\n");
        assertEquals("6", first("p(3, B)", "B"));
        assertEquals("[2,4]", first("q(L)", "L"));
        assertEquals("2", first("r(N)", "N"));
        ok("clause(p(A, C), (C0 is A0 * 2)), C == C0, A == A0");
        assertEquals("same(A)", first("clause(s(A), B)", "B"));       // fixpoint: unchanged
        assertEquals("6", first("nb_getval(dirres, V)", "V"));        // directives are expanded too
    }

    @Test
    public void testISS0731_GoalExpansionDepthCap() {
        try {
            prolog.consult("goal_expansion(grow(X), grow(f(X))).\nt :- grow(a).\n");
            fail("a non-terminating goal_expansion must be a load error");
        } catch (PrologException e) {
            assertTrue(e.getMessage(), e.getMessage().contains("goal_expansion"));
        }
    }

    // ------------------------------------------------------------------ Q3.3 file_search_path

    @Test
    public void testISS0737_AbsoluteFileNameAndFileSearchPath() throws Exception {
        write("lib/mym.pl", ":- module(mym, [hello/1]).\nhello(world).\n");
        String lib = path(new File(dir, "lib"));
        ok("assertz(file_search_path(mine, '" + lib + "'))");
        assertEquals("'" + lib + "/mym.pl'",
            first("absolute_file_name(mine(mym), F, [extensions([pl]), access(read)])", "F"));
        assertEquals("'" + lib + "/zz'", first("absolute_file_name(mine(zz), F)", "F"));
        no("absolute_file_name(mine(nope), _, [file_type(prolog), access(read), file_errors(fail)])");
        assertEquals("error(existence_error(source_sink,mine(nope)),'absolute_file_name/3')",
            first("catch(absolute_file_name(mine(nope), _, [access(read)]), E, true)", "E"));
        assertEquals("error(instantiation_error,'absolute_file_name/2')",
            first("catch(absolute_file_name(_, _), E, true)", "E"));
        // relative_to and file_type(directory)
        assertEquals("'" + lib + "'",
            first("absolute_file_name(lib, F, [file_type(directory), relative_to('" + path(dir) + "')])", "F"));
        // the built-in library default finds the bundled prelude (on disk in a build tree)
        assertTrue(first("absolute_file_name(library(lists), F, [file_type(prolog), access(read)])", "F")
            .endsWith("prelude/lists.pl'"));
        // library(X) loading goes through file_search_path/2
        ok("assertz(file_search_path(library, mine(.)))");
        ok("use_module(library(mym))");
        assertEquals("world", first("hello(X)", "X"));
        // a fresh engine: file_search_path/2 is defined (dynamic, multifile), with no clause
        Prolog fresh = new Prolog();
        assertTrue(fresh.solve("file_search_path(_, _).").isEmpty());
    }

    // ------------------------------------------------------------------ Q3.4 use_module/2

    @Test
    public void testISS0735_UseModuleImportList() throws Exception {
        write("m1.pl", ":- module(m1, [a/1, b/1, c/1]).\na(1).\nb(2).\nc(3).\n");
        File u = write("u.pl",
            ":- use_module(m1, [a/1, b/1 as bb]).\n"
          + "t1(X) :- a(X).\nt2(X) :- bb(X).\n"
          + "t3(E) :- catch(b(_), error(E, _), true).\n"
          + "t4(E) :- catch(c(_), error(E, _), true).\n");
        prolog.consultFile(u.getAbsolutePath());
        assertEquals("1", first("t1(X)", "X"));
        assertEquals("2", first("t2(X)", "X"));
        assertEquals("existence_error(procedure,b/1)", first("t3(E)", "E"));
        assertEquals("existence_error(procedure,c/1)", first("t4(E)", "E"));
        // except/1, from a fresh engine, as a goal
        Prolog p2 = new Prolog();
        String m1 = path(new File(dir, "m1"));
        assertFalse(p2.solve("use_module('" + m1 + "', except([c/1])), a(1), b(2).").isEmpty());
        assertEquals("existence_error(procedure,c/1)", Writer.format(
            p2.solve("catch(c(_), error(E, _), true).").get(0).get("E"), Writer.Options.writeq(), 1200));
        assertEquals("error(type_error(predicate_indicator,foo),'use_module/2')", Writer.format(
            p2.solve("catch(use_module('" + m1 + "', [foo]), E, true).").get(0).get("E"),
            Writer.Options.writeq(), 1200));
    }

    // ------------------------------------------------------------------ Q3.5 '|' and as

    @Test
    public void testISS0734_BarAndAsOperators() {
        assertEquals("['|',a,b]", first("X = (a|b), X =.. L", "L"));
        assertEquals("'|'(a,b)", first("X = (a|b)", "X"));
        ok("current_op(1105, xfy, '|')");
        ok("current_op(700, xfx, as)");
        assertEquals("[as,p/1,subsumptive]", first("X = (p/1 as subsumptive), X =.. L", "L"));
        // as a goal '|'/2 is ;/2, cut-transparent like it
        assertEquals("[1,2]", first("findall(X, (X = 1 | X = 2), L)", "L"));
        prolog.consult(
            "g --> [a] | [b].\n"
          + "c(X) :- (X = 1, ! | X = 2).\n"
          + ":- table tp/1 as subsumptive.\n"
          + "tp(1).\n");
        ok("phrase(g, [b])");
        assertEquals("[1]", first("findall(X, c(X), L)", "L"));
        ok("predicate_property(tp(_), tabled)");
        assertEquals("error(type_error(callable,'|'(fail,1)),'call/1')",
            first("catch(call((fail | 1)), E, true)", "E"));
    }

    // ------------------------------------------------------------------ Q3.6 make/0, runMain

    @Test
    public void testISS0736_MakeReloadsAChangedIncludedFile() throws Exception {
        File inc = write("inc.pl", "incp(1).\n");
        File main = write("main.pl", ":- include('inc.pl').\nmainp(1).\n");
        prolog.consultFile(main.getAbsolutePath());
        assertEquals("[1]", first("findall(X, incp(X), L)", "L"));
        write("inc.pl", "incp(2).\n");
        assertTrue(inc.setLastModified(inc.lastModified() + 5000));
        ok("make");
        assertEquals("[2]", first("findall(X, incp(X), L)", "L"));
    }

    @Test
    public void testISS0736_RunMain() throws Exception {
        Prolog p = new Prolog();
        assertEquals(-1, p.runMain());
        p.setDeferInitializationMain(true);
        p.consultFile(write("m3.pl", ":- initialization(main, main).\nmain :- halt(3).\n").getAbsolutePath());
        assertEquals(3, p.runMain());
        Prolog q = new Prolog();
        q.setDeferInitializationMain(true);
        q.consultFile(write("m0.pl", ":- initialization(main, main).\nmain :- nb_setval(ran, yes).\n").getAbsolutePath());
        assertEquals(0, q.runMain());
        assertFalse(q.solve("nb_getval(ran, yes).").isEmpty());
        Prolog r = new Prolog();
        r.setDeferInitializationMain(true);
        r.consultFile(write("m1f.pl", ":- initialization(main, main).\nmain :- fail.\n").getAbsolutePath());
        final int[] code = {0};
        stderrOf(() -> code[0] = r.runMain());
        assertEquals(1, code[0]);
    }

    // ------------------------------------------------------------------ Q3.7 positions, comments

    @Test
    public void testISS0738_SubtermPositions() {
        assertEquals("term_position(0,21,12,14,[term_position(0,11,0,3,[4-5,7-10]),"
            + "list_position(15,21,[16-17],18-20)])",
            first("open_string(\"foo(X, bar) :- [a|Tl].\", S), read_term(S, _, [subterm_positions(P)]), close(S)", "P"));
        assertEquals("term_position(0,14,8,9,[term_position(0,7,3,4,[brace_term_position(0,3,1-2),"
            + "string_position(5,7)]),parentheses_term_position(10,14,term_position(11,13,11,12,[12-13]))])",
            first("open_string(\"{x}, \\\"\\\" ; (-a).\", S), read_term(S, _, [subterm_positions(P)]), close(S)", "P"));
        assertEquals("11-13", first("open_string(\"a. /* c */ bb.\", S), read(S, _), "
            + "read_term(S, _, [subterm_positions(P)]), close(S)", "P"));
    }

    @Test
    public void testISS0738_Comments() {
        assertEquals("['$stream_position'(0,1,0,0)-\"% one\",'$stream_position'(6,2,0,6)-\"/* two */\"]",
            first("open_string(\"% one\\n/* two */ a.\", S), read_term(S, T, [comments(C)]), close(S)", "C"));
        assertEquals("[]", first("open_string(\"a.\", S), read_term(S, T, [comments(C)]), close(S)", "C"));
    }

    // ------------------------------------------------------------------ Q3.8 per-file load lock

    @Test(timeout = 60000)
    public void testISS0739_LoadsOfDifferentFilesDoNotBlockEachOther() throws Exception {
        File b = write("lb.pl", "q3b(1).\n");
        // the directive waits for a MESSAGE the loading thread sends after its load: the
        // per-engine lock made this a deadlock (only thread_join was recognised)
        File a = write("la.pl", ":- thread_self(Me), thread_create((consult('" + path(b)
            + "'), thread_send_message(Me, loaded)), _, [detached(true)]), thread_get_message(loaded).\n"
            + "q3a(1).\n");
        final Throwable[] err = new Throwable[1];
        Thread t = new Thread(() -> {
            try { prolog.consultFile(a.getAbsolutePath()); } catch (Throwable e) { err[0] = e; }
        });
        t.setDaemon(true);
        t.start();
        t.join(30000);
        assertFalse("the load deadlocked", t.isAlive());
        assertTrue(String.valueOf(err[0]), err[0] == null);
        ok("q3a(1), q3b(1)");
    }

    @Test(timeout = 60000)
    public void testISS0739_LoadCycleRaisesPermissionError() throws Exception {
        File a = write("cyc.pl", "cyc(1).\n:- thread_create(consult('" + path(new File(dir, "cyc.pl"))
            + "'), T, []), thread_join(T, S), nb_setval(cycstatus, S).\n");
        final Throwable[] err = new Throwable[1];
        Thread t = new Thread(() -> {
            try { prolog.loadFile(a.getAbsolutePath()); } catch (Throwable e) { err[0] = e; }
        });
        t.setDaemon(true);
        t.start();
        t.join(30000);
        assertFalse("the load deadlocked", t.isAlive());
        assertTrue(String.valueOf(err[0]), err[0] == null);
        String st = first("nb_getval(cycstatus, S)", "S");
        assertTrue(st, st.startsWith("exception(error(permission_error(load,source_sink,"));
        // the same cycle through a concurrent_* worker (the directive awaits it)
        File c = write("cc.pl", "cc(1).\n:- catch(concurrent_and([consult('" + path(new File(dir, "cc.pl"))
            + "')], []), error(E, _), nb_setval(ccerr, E)).\n");
        prolog.loadFile(c.getAbsolutePath());
        assertTrue(first("nb_getval(ccerr, E)", "E").startsWith("permission_error(load,source_sink,"));
    }

    // ------------------------------------------------------------------ extras (Q2 findings)

    @Test
    public void testISS0733_QualifiedClauseHeads() throws Exception {
        prolog.consult("m:foo(1).\nuser:hook(X) :- X = 1.\nm:bar(X) :- foo(X).\n"
            + "prolog:message(q3) --> [hi].\n");
        ok("current_module(m)");
        ok("current_module(prolog)");
        ok("m:foo(1)");
        ok("hook(1)");
        no("current_predicate(':'/2)");
        // a body stored for module m runs in m (the clause was written in user: user:foo would
        // not see it; SWI keeps the source module) — here the source is user, so bar/1 calls
        // user:foo/1, which does not exist: the module's own foo/1 is not reached from user
        ok("assertz(n:baz(2)), n:baz(2)");
        ok("current_module(n)");
        ok("assertz(n:(qq(X) :- baz(X))), n:qq(2)");
        ok("assertz(user:uu(1)), uu(1), clause(user:uu(X), true), retract(user:uu(1)), \\+ uu(_)");
        ok("predicate_property(m:foo(_), defined)");
        // a module file adds a clause to user; its body keeps the module's context
        File mf = write("mm.pl", ":- module(mm, [go/1]).\ngo(X) :- helper(X).\nhelper(7).\n"
            + "user:uhook(X) :- helper(X).\nother:thing(X) :- helper(X).\n");
        prolog.consultFile(mf.getAbsolutePath());
        assertEquals("7", first("uhook(X)", "X"));
        assertEquals("7", first("other:thing(X)", "X"));
    }

    @Test
    public void testISS0732_CurrentPredicateQualified() {
        prolog.consult(":- module(cpm, [e/1]).\ne(1).\nlocal(2).\n");
        prolog.consult("q3cp(1).\nzz:yy(1).\n");
        ok("current_predicate(cpm:e/1)");
        ok("current_predicate(cpm:local/1)");
        ok("current_predicate(lists:append/3)");
        ok("current_predicate(user:q3cp/1)");
        ok("current_predicate(zz:yy/1)");
        no("current_predicate(cpm:nope/1)");
        assertEquals("[cpm]", first("findall(M, current_predicate(M:local/1), L)", "L"));
        assertEquals("[e/1,local/1]", first("findall(P, current_predicate(cpm:P), L0), msort(L0, L)", "L"));
        assertEquals("error(type_error(predicate_indicator,foo),'current_predicate/1')",
            first("catch(current_predicate(cpm:foo), E, true)", "E"));
    }

    @Test
    public void testISS0733_LegacyDcgQualifiedAndBar() {
        boolean was = Prolog.isUsingV2Dcg();
        Prolog.setUseV2Dcg(false);
        try {
            Prolog p = new Prolog();
            p.consult("lg --> [a] | [b].\nlq --> dm:nt.\ndm:nt --> [x].\n");
            assertFalse(p.solve("phrase(lg, [b]).").isEmpty());
            assertFalse(p.solve("phrase(lq, [x]).").isEmpty());
            assertTrue(p.solve("current_predicate(':'/4).").isEmpty());
        } finally {
            Prolog.setUseV2Dcg(was);
        }
    }
}
// END_CHANGE: ISS-2025-0730..0739
