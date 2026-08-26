package it.denzosoft.jprolog.core.engine.v2;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * End-to-end: drive real programs through the clean-room v2 resolution engine ({@link MachineSolver})
 * via the standard {@link Prolog} API, with {@code -Djprolog.engine=v2} simulated by the flag. Proves
 * the integration — consult populates the live KB, queries run on the new engine over that KB and the
 * shared builtin registry.
 */
public class V2EngineIntegrationTest {

    private Prolog p;
    private boolean prev;
    // START_CHANGE: ISS-2025-0478 - wave W8 made v4 the default, and v4 wins over the v2 flag.
    // This class exists to exercise the v2 MachineSolver, so it must turn v4 OFF as well.
    private boolean prevV4;

    @Before public void on() {
        prev = Prolog.isUsingV2Engine();
        prevV4 = Prolog.isUsingV4Engine();
        Prolog.setUseV4Engine(false);
        Prolog.setUseV2Engine(true);
        p = new Prolog();
    }
    @After public void off() { Prolog.setUseV2Engine(prev); Prolog.setUseV4Engine(prevV4); }
    // END_CHANGE: ISS-2025-0478

    private List<Map<String, Term>> q(String query) { return p.solve(query); }

    @Test public void factsRulesBacktracking() {
        p.consult("parent(tom, bob). parent(bob, ann). parent(bob, pat).");
        p.consult("grandparent(X, Z) :- parent(X, Y), parent(Y, Z).");
        assertEquals(2, q("grandparent(tom, W).").size());     // ann, pat
        assertEquals("ann", q("grandparent(tom, W).").get(0).get("W").toString());
    }

    @Test public void arithmeticRecursion() {
        p.consult("fact(0, 1). fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.");
        assertEquals("120", q("fact(5, F).").get(0).get("F").toString());
    }

    @Test public void listsViaLibraryBuiltins() {
        // append/length are reached through the builtin bridge
        assertEquals(3, q("between(1, 3, X).").size());
        assertFalse(q("append([a, b], [c], L).").isEmpty());
    }

    @Test public void findallThroughEngine() {
        p.consult("col(red). col(green). col(blue).");
        List<Map<String, Term>> s = q("findall(C, col(C), L).");
        assertFalse(s.isEmpty());
        assertEquals("[red,green,blue]",
            it.denzosoft.jprolog.core.write.v2.TermWriter.write(s.get(0).get("L")));
    }

    @Test public void catchThrowThroughEngine() {
        p.consult("risky(X) :- (X > 0 -> true ; throw(negative)).");
        assertFalse(q("catch(risky(5), _, fail).").isEmpty());
        assertFalse(q("catch(risky(-1), negative, true).").isEmpty());
    }

    @Test public void cutThroughEngine() {
        p.consult("first([X|_], X) :- !. first([_|T], Y) :- first(T, Y).");
        assertEquals(1, q("first([a, b, c], X).").size());
        assertEquals("a", q("first([a, b, c], X).").get(0).get("X").toString());
    }

    @Test public void assertDuringQuery() {
        p.consult("seed :- assertz(counter(0)).");
        q("seed.");
        assertEquals("0", q("counter(X).").get(0).get("X").toString());
    }
}
