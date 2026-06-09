package it.denzosoft.jprolog.core.dcg.v2;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.parser.v2.TermReader;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.write.v2.TermWriter;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Validates the clean-room {@link DCGTranslator}: structural output (rendered with the v2
 * {@link TermWriter}) and end-to-end behaviour (translate → consult → {@code phrase/2}).
 */
public class DCGTranslatorTest {

    private final OperatorTable ops = OperatorTable.getDefault();

    private String translateToText(String dcg) {
        Term rule = TermReader.parseTerm(dcg, ops);
        Term clause = new DCGTranslator().translate((CompoundTerm) rule);
        return TermWriter.write(clause);
    }

    @Test public void terminalSequence() {
        assertEquals("greeting(_S0,_S1):-_S0=[hello|_S2],_S2=[world|_S1]",
            translateToText("greeting --> [hello], [world]"));
    }

    @Test public void emptyAndPushPrologEscape() {
        assertEquals("empty(_S0,_S1):-_S0=_S1", translateToText("empty --> []"));
        // {Goal} is zero-width
        assertEquals("g(_S0,_S1):-true,_S0=_S1", translateToText("g --> {true}"));
    }

    @Test public void nonTerminalWithArgs() {
        // digits([D|Ds]) --> [D], digits(Ds)
        assertEquals("digits([D|Ds],_S0,_S1):-_S0=[D|_S2],digits(Ds,_S2,_S1)",
            translateToText("digits([D|Ds]) --> [D], digits(Ds)"));
    }

    // ---------------- end-to-end through the engine ----------------
    private Prolog grammar(String... dcgRules) {
        Prolog p = new Prolog();
        StringBuilder sb = new StringBuilder();
        for (String r : dcgRules) {
            Term rule = TermReader.parseTerm(r, ops);
            Term clause = new DCGTranslator().translate((CompoundTerm) rule);
            sb.append(TermWriter.write(clause)).append(".\n");
        }
        p.consultV2(sb.toString());
        return p;
    }

    @Test public void phraseTerminals() {
        Prolog p = grammar("ab --> [a], [b]");
        assertFalse(p.solve("phrase(ab, [a, b]).").isEmpty());
        assertTrue(p.solve("phrase(ab, [a, c]).").isEmpty());
    }

    @Test public void phraseRecursive() {
        Prolog p = grammar("as --> []", "as --> [a], as");
        assertFalse(p.solve("phrase(as, []).").isEmpty());
        assertFalse(p.solve("phrase(as, [a, a, a]).").isEmpty());
        assertTrue(p.solve("phrase(as, [a, b]).").isEmpty());
    }

    @Test public void phraseWithPrologEscapeAndAlternative() {
        // digit(D) --> [D], { D >= 0'0, D =< 0'9 }   (using char codes)
        Prolog p = grammar(
            " digit --> [a] ; [b] ",
            "two --> digit, digit");
        assertFalse(p.solve("phrase(two, [a, b]).").isEmpty());
        assertFalse(p.solve("phrase(two, [b, b]).").isEmpty());
        assertTrue(p.solve("phrase(two, [a, c]).").isEmpty());
    }

    @Test public void phraseGenerates() {
        // generation mode: phrase(ab, L) should produce [a,b]
        Prolog p = grammar("ab --> [a], [b]");
        assertEquals("[a,b]", TermWriter.write(p.solve("phrase(ab, L).").get(0).get("L")));
    }
}
