package it.denzosoft.jprolog.test.cli;

import it.denzosoft.jprolog.PrologCLI;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * 4.5 wave P6.4 — the CLI as a real toplevel (ISS-2025-0627, 0628, 0636, 0637): streamed answers
 * in both modes, no demo facts unless asked, command-line files / goals / options and exit
 * statuses, {@code initialization(G, main)}. Each test fails on the 4.4.0 CLI (which collected
 * every answer first, loaded demo facts, ignored its arguments and never set an exit status).
 * The CLI runs in-process through {@link PrologCLI#run()}, which never calls System.exit.
 */
public class PrologCliToplevelTest {

    private File dir;
    private PrintStream oldOut;
    private ByteArrayOutputStream programOut;

    @Before
    public void setUp() throws Exception {
        dir = Files.createTempDirectory("p6cli").toFile();
        // what the PROGRAM writes goes to user_output (System.out); capture it too
        oldOut = System.out;
        programOut = new ByteArrayOutputStream();
        System.setOut(new PrintStream(programOut, true, "UTF-8"));
    }

    @After
    public void tearDown() {
        System.setOut(oldOut);
        File[] kids = dir.listFiles();
        if (kids != null) for (File k : kids) k.delete();
        dir.delete();
    }

    /** Result of one in-process CLI run. */
    private static final class Run {
        int status;
        String out;
        String err;
    }

    private Run cli(String input, String... args) throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        PrologCLI c = new PrologCLI(args, new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)),
            new PrintStream(out, true, "UTF-8"), new PrintStream(err, true, "UTF-8"));
        Run r = new Run();
        r.status = c.run();
        r.out = new String(out.toByteArray(), StandardCharsets.UTF_8);
        r.err = new String(err.toByteArray(), StandardCharsets.UTF_8);
        return r;
    }

    private File file(String name, String text) throws Exception {
        File f = new File(dir, name);
        Files.write(f.toPath(), text.getBytes(StandardCharsets.UTF_8));
        return f;
    }

    // ================================================================ ISS-2025-0627 streaming

    /**
     * Batch mode streams: an infinite enumeration is cut by --max-solutions instead of running out
     * of memory (4.4.0: {@code last([a|T], b).} and {@code between(1, inf, X).} never answered).
     */
    @Test(timeout = 60000)
    public void testISS0627_BatchStreamsAnswers() throws Exception {
        Run r = cli("last([a|T], b).\nbetween(1, inf, X).\n:quit\n", "-q", "--max-solutions", "3");
        assertEquals(0, r.status);
        assertTrue(r.out, r.out.contains("T = [b] ;"));
        assertTrue(r.out, r.out.contains("T = [_A,b] ;"));
        assertTrue(r.out, r.out.contains("T = [_A,_B,b]."));
        assertTrue(r.out, r.out.contains("X = 1 ;\nX = 2 ;\nX = 3."));
        // a deterministic last answer ends in '.', a choice point that yields nothing more in false.
        r = cli("member(X, [a, b]).\n(X = 1 ; X = 2 ; fail).\n:quit\n", "-q");
        assertTrue(r.out, r.out.contains("X = a ;\nX = b."));
        assertTrue(r.out, r.out.contains("X = 1 ;\nX = 2 ;\nfalse."));
    }

    /**
     * Interactive mode computes the next answer only when ';' is typed: the side effects of the
     * answers that were not asked for never run (4.4.0 ran all of them before the first prompt).
     */
    @Test(timeout = 60000)
    public void testISS0627_InteractiveComputesOnDemand() throws Exception {
        Run r = cli("dynamic(seen/1).\n"
            + "between(1, inf, X), assertz(seen(X)).\n;\n;\n\n"
            + "aggregate_all(count, seen(_), C).\n:quit\n", "--interactive", "-q");
        // (the typed ';' and Enter are not echoed here, so the prompts run together)
        assertTrue(r.out, r.out.contains("X = 1 ;X = 2 ;X = 3 ;."));
        assertTrue("exactly three answers were computed: " + r.out, r.out.contains("C = 3."));
    }

    /** The program's output is interleaved with the answers, not produced all up front. */
    @Test(timeout = 60000)
    public void testISS0627_SideEffectsInterleaveWithAnswers() throws Exception {
        Run r = cli("between(1, 3, X), assertz(p6x(X)), aggregate_all(count, p6x(_), N).\n:quit\n", "-q");
        // the Nth answer sees exactly N asserted facts: it was computed after the previous answers
        assertTrue(r.out, r.out.contains("X = 1,\nN = 1 ;"));
        assertTrue(r.out, r.out.contains("X = 2,\nN = 2 ;"));
        assertTrue(r.out, r.out.contains("X = 3,\nN = 3."));
    }

    /** No demo facts unless --demo (decision table, §8). */
    @Test(timeout = 60000)
    public void testISS0627_DemoFactsOnlyOnRequest() throws Exception {
        Run r = cli("catch(color(X), error(existence_error(procedure, _), _), X = none).\n:quit\n", "-q");
        assertTrue(r.out, r.out.contains("X = none."));
        r = cli("color(X).\n:quit\n", "-q", "--demo");
        assertTrue(r.out, r.out.contains("X = blue ;"));
    }

    /** Files, -g, -t, --safe, --budget and the exit statuses. */
    @Test(timeout = 60000)
    public void testISS0627_CommandLineAndExitStatus() throws Exception {
        File f = file("facts.pl", "fact(42).\n");
        Run r = cli("fact(X).\n:quit\n", "-q", f.getAbsolutePath());
        assertEquals(0, r.status);
        assertTrue(r.out, r.out.contains("X = 42."));

        assertEquals(0, cli("", f.getAbsolutePath(), "-g", "fact(42)", "-t", "halt").status);
        r = cli("", "-g", "fail");
        assertEquals(1, r.status);
        assertTrue(r.err, r.err.contains("goal (fail) failed"));
        r = cli("", "-g", "p6_undefined");
        assertEquals(1, r.status);
        assertTrue(r.err, r.err.contains("existence_error(procedure, p6_undefined/0)"));
        assertEquals(7, cli("", "-g", "halt(7)").status);
        assertEquals(1, cli("", "-t", "fail").status);
        assertEquals(0, cli("", "-t", "true").status);
        assertEquals(5, cli("halt(5).\nwrite(never).\n", "-q").status);
        assertEquals(2, cli("", "--no-such-option").status);
        assertEquals(1, cli("", new File(dir, "missing.pl").getAbsolutePath()).status);

        r = cli("", "--safe", "-g", "catch(open(x, write, _), error(existence_error(procedure, _), _), true)", "-t", "halt");
        assertEquals(0, r.status);
        r = cli("", "--budget", "20000", "-g", "between(1, inf, _), fail");
        assertEquals(1, r.status);
        assertTrue(r.err, r.err.toLowerCase().contains("budget"));
        assertFalse(programOut.toString("UTF-8").contains("never"));
    }

    // ================================================================ ISS-2025-0636 initialization main

    /** initialization(G, main): G runs after the load, then the process ends with G's status. */
    @Test(timeout = 60000)
    public void testISS0636_InitializationMainHalts() throws Exception {
        File ok = file("ok.pl", ":- initialization(main, main).\nmain :- write(p6_main_ran), nl.\n");
        Run r = cli("write(toplevel_reached).\n", ok.getAbsolutePath());
        assertEquals(0, r.status);
        assertTrue(programOut.toString("UTF-8").contains("p6_main_ran"));
        assertFalse("the toplevel must not run after main", r.out.contains("?-"));

        File failing = file("fail.pl", ":- initialization(main, main).\nmain :- fail.\n");
        assertEquals(1, cli("", failing.getAbsolutePath()).status);
        File raising = file("raise.pl", ":- initialization(main, main).\nmain :- X is foo + 1, write(X).\n");
        r = cli("", raising.getAbsolutePath());
        assertEquals(1, r.status);
        assertTrue(r.err, r.err.contains("type_error(evaluable, foo/0)"));
        File halting = file("halt.pl", ":- initialization(main, main).\nmain :- halt(4).\n");
        assertEquals(4, cli("", halting.getAbsolutePath()).status);
    }

    // ================================================================ ISS-2025-0637 / 0628

    /** The unused legacy Parser field is gone; the pom runs and packages the CLI. */
    @Test
    public void testISS0637_NoLegacyParserFieldAndRunnableJar() throws Exception {
        for (java.lang.reflect.Field f : PrologCLI.class.getDeclaredFields()) {
            assertFalse("PrologCLI must not hold a legacy parser: " + f,
                f.getType().getName().equals("it.denzosoft.jprolog.core.parser.Parser"));
        }
        String pom = new String(Files.readAllBytes(new File("pom.xml").toPath()), StandardCharsets.UTF_8);
        assertFalse("the exec default mainClass must exist", pom.contains("<mainClass>it.denzosoft.jprolog.Main</mainClass>"));
        assertTrue(pom.contains("<mainClass>it.denzosoft.jprolog.PrologCLI</mainClass>"));
        assertTrue(pom.contains("<finalName>jprolog</finalName>"));
    }

    // ================================================================ ISS-2025-0671 / 0672 (4.5 P7)

    /** A query that does not parse prints an ISO syntax_error line, not a quoted message atom. */
    @Test(timeout = 60000)
    public void testISS0671_ParseErrorLineIsIsoSyntaxError() throws Exception {
        Run r = cli("X = f(.\nY = 1.\n", "--batch");
        String all = r.out + r.err;
        assertTrue(all, all.contains("error(syntax_error("));
        assertFalse(all, all.contains("Error parsing query"));
        assertTrue("the session continues after the error: " + all, all.contains("Y = 1"));
    }

    /** --safe keeps halt/1 as the CLI's exit (SafeModeOptions.allowHalt), with its status. */
    @Test(timeout = 60000)
    public void testISS0672_SafeCliStillHalts() throws Exception {
        Run r = cli("halt(3).\n", "--safe", "--batch");
        assertEquals(3, r.status);
    }
}
