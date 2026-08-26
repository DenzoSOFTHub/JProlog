package it.denzosoft.jprolog.test.cli;

import it.denzosoft.jprolog.PrologCLI;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0483 - engine v4 wave W8: the non-interactive console.
/**
 * The CLI must never consume the next INPUT LINE as the answer to its "more solutions?" prompt.
 *
 * <p>Before this fix, a piped session (a here-doc, a redirected file, {@code test_all_examples.sh},
 * a CI job) silently lost a query for every query that had more than one solution: the CLI printed
 * the first answer, wrote {@code " ;"}, and read the NEXT QUERY as the user's answer — which was
 * not {@code ";"}, so it printed {@code "."} and threw that query away. {@code between(1,5,X).}
 * followed by {@code digit(X).} ran only the first, and reported one answer instead of five.
 *
 * <p>The console is non-interactive when {@code System.console() == null} (which is the case under
 * surefire, so these tests exercise the real default) or when {@code --batch} / {@code -q} is
 * given. In that mode all solutions are printed at once, separated by {@code " ;"} and terminated
 * by {@code "."}, and nothing is read back. Interactive behaviour is unchanged.
 */
public class PrologCliBatchTest {

    /** Run the CLI over {@code input} and return everything it wrote to stdout. */
    private String run(String[] args, String input) {
        InputStream oldIn = System.in;
        PrintStream oldOut = System.out;
        ByteArrayOutputStream captured = new ByteArrayOutputStream();
        try {
            System.setIn(new ByteArrayInputStream(input.getBytes()));
            System.setOut(new PrintStream(captured, true));
            PrologCLI.main(args);
        } finally {
            System.setIn(oldIn);
            System.setOut(oldOut);
        }
        return captured.toString();
    }

    /** Surefire has no console, so the CLI must decide it is non-interactive by itself. */
    @Test
    public void testISS0483_NoConsoleMeansBatch() {
        InputStream oldIn = System.in;
        try {
            System.setIn(new ByteArrayInputStream(new byte[0]));
            assertTrue("a CLI with no terminal on stdin must be in batch mode",
                       new PrologCLI().isBatch());
            assertTrue("--batch must force it", new PrologCLI(new String[]{"--batch"}).isBatch());
            assertTrue("-q must force it", new PrologCLI(new String[]{"-q"}).isBatch());
        } finally {
            System.setIn(oldIn);
        }
    }

    /** Every solution is printed, separated by {@code ;} and terminated by {@code .} */
    @Test(timeout = 60000)
    public void testISS0483_AllSolutionsArePrintedWithoutReadingTheNextLine() {
        String out = run(new String[0], "between(1, 3, X).\n:quit\n");
        assertTrue("first solution missing:\n" + out, out.contains("X = 1 ;"));
        assertTrue("second solution missing:\n" + out, out.contains("X = 2 ;"));
        assertTrue("last solution missing or not terminated:\n" + out, out.contains("X = 3."));
    }

    /** The regression itself: the query AFTER a multi-solution query must still run. */
    @Test(timeout = 60000)
    public void testISS0483_TheNextQueryIsNotSwallowed() {
        String out = run(new String[0], "between(1, 3, X).\natom(swallowed_marker).\n:quit\n");
        assertTrue("the next query was eaten:\n" + out, out.contains("X = 3."));
        // `atom(swallowed_marker).` is ground and true, so the CLI prints `true.` — and it must do
        // so AFTER the last solution of the previous query (the prompt is on the same line).
        int last = out.indexOf("X = 3.");
        int after = out.indexOf("true.", last);
        assertTrue("the query after the multi-solution one did not run:\n" + out,
                   last >= 0 && after > last);
    }

    /** A single-solution query and a failing one are unaffected. */
    @Test(timeout = 60000)
    public void testISS0483_SingleSolutionAndFailureAreUnchanged() {
        String out = run(new String[]{"--batch"}, "X is 1+1.\natom(1).\n:quit\n");
        assertTrue("binding missing:\n" + out, out.contains("X = 2."));
        assertTrue("failure missing:\n" + out, out.contains("false."));
    }

    /** An explicit {@code ;} line is NOT consumed as an answer in batch mode either. */
    @Test(timeout = 60000)
    public void testISS0483_BatchModeNeverReadsAnAnswerLine() {
        // In interactive mode the `;` line would be the answer to the first prompt. In batch mode
        // all three solutions are printed first, and the `;` line is then read as a QUERY —
        // a syntax error, which is exactly what proves nothing consumed it as an answer.
        String out = run(new String[]{"--batch"}, "between(1, 3, X).\nfoo bar baz.\n:quit\n");
        assertTrue(out.contains("X = 1 ;"));
        assertTrue(out.contains("X = 3."));
        assertFalse("batch mode must not echo an interactive prompt loop", out.contains("X = 1 ;\nX = 1"));
    }

    /** The banner says which mode is in force, so a piped log is self-describing. */
    @Test(timeout = 60000)
    public void testISS0483_BannerNamesTheMode() {
        String out = run(new String[0], ":quit\n");
        assertTrue("banner missing:\n" + out, out.contains("Non-interactive input"));
        assertEquals("the CLI must still exit cleanly", true, out.contains("Goodbye!"));
    }
}
// END_CHANGE: ISS-2025-0483
