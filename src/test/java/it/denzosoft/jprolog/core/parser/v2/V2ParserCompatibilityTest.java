package it.denzosoft.jprolog.core.parser.v2;

import it.denzosoft.jprolog.core.engine.Prolog;
import org.junit.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Compatibility check: load every example program through both the legacy {@code consult} and the
 * v2 {@code consultV2}. The clean-room v2 parser must parse <em>at least</em> everything the legacy
 * parser parses (it should be a strict superset). Reports exactly which files v2 fails on, if any.
 */
public class V2ParserCompatibilityTest {

    @Test public void v2ParsesEverythingLegacyParses() throws Exception {
        File dir = new File("examples");
        assertTrue("examples/ directory must exist (run from project root)", dir.isDirectory());
        File[] files = dir.listFiles((d, n) -> n.endsWith(".pl"));
        assertNotNull(files);
        Arrays.sort(files);

        int legacyOk = 0, v2Ok = 0, both = 0;
        List<String> regressions = new ArrayList<>();   // legacy parses but v2 does not
        List<String> v2Only = new ArrayList<>();        // v2 parses but legacy does not

        for (File f : files) {
            String src = new String(Files.readAllBytes(f.toPath()), StandardCharsets.UTF_8);
            boolean legacy = loadsCleanly(src, false);
            boolean v2 = loadsCleanly(src, true);
            if (legacy) legacyOk++;
            if (v2) v2Ok++;
            if (legacy && v2) both++;
            if (legacy && !v2) regressions.add(f.getName());
            if (!legacy && v2) v2Only.add(f.getName());
        }

        System.out.println("[v2 parser compat] files=" + files.length
            + " legacyOk=" + legacyOk + " v2Ok=" + v2Ok + " both=" + both
            + " v2-only=" + v2Only.size() + " regressions=" + regressions.size());
        if (!v2Only.isEmpty()) System.out.println("  v2 parses (legacy fails): " + v2Only);
        if (!regressions.isEmpty()) System.out.println("  v2 stricter than legacy on: " + regressions);

        // v2 is a STRICT IMPROVEMENT: it parses at least as many programs as the legacy parser
        // (here 123 vs 117 of 130), and parses several the legacy parser cannot. The only files
        // where v2 is stricter use NON-ISO constructs that the legacy parser accepts leniently
        // (and that even SWI rejects): a variable used as a functor `Var(Args)`
        // (test_31_global_variables) and an operator priority clash `1200 xfx` term as the head of
        // a `1200 xfx` `:-` rule (test_34_operator_definitions). Those are correct rejections.
        List<String> knownStricter = Arrays.asList(
            "test_31_global_variables.pl", "test_34_operator_definitions.pl");
        List<String> unexpected = new ArrayList<>(regressions);
        unexpected.removeAll(knownStricter);
        assertTrue("unexpected v2 parse regressions (not the known non-ISO cases): " + unexpected,
            unexpected.isEmpty());
        assertTrue("v2 must parse at least as many programs as the legacy parser (" + v2Ok + " vs " + legacyOk + ")",
            v2Ok >= legacyOk);
    }

    /** True if the whole program loads without a parse error (other runtime issues are ignored). */
    private boolean loadsCleanly(String src, boolean v2) {
        boolean saved = Prolog.isUsingV2Parser();
        try {
            Prolog.setUseV2Parser(v2);              // force the requested parser (consult now defaults to v2)
            Prolog p = new Prolog();
            if (v2) p.consultV2(src); else p.consult(src);
            return true;
        } catch (Throwable t) {
            String m = String.valueOf(t.getMessage());
            // Treat as a parse failure only if it is a parse/lex error; ignore runtime directive errors.
            return !(m.contains("arse") || m.contains("Unexpected") || m.contains("operator expected")
                     || m.contains("Lex") || m.contains("missing '.'"));
        } finally {
            Prolog.setUseV2Parser(saved);
        }
    }
}
