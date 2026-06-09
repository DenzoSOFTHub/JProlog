package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * End-to-end: drive the clean-room v2 CLP(FD) solver through the engine's standard syntax
 * ({@code in/2}, {@code #<}, {@code #=}, {@code all_different/1}, {@code label/1}) via
 * {@link Prolog#enableV2Clpfd()}. Proves the legacy CLP findings are fixed when running through
 * the engine, not just in isolation.
 */
public class ClpfdV2EngineTest {

    private Prolog clp() {
        Prolog p = new Prolog();
        p.enableV2Clpfd();
        return p;
    }

    @Test public void soundLessThanLabeling() {
        Prolog p = clp();
        List<Map<String, Term>> s = p.solve("X in 1..3, Y in 1..3, X #< Y, label([X, Y]).");
        assertEquals(3, s.size());                              // (1,2),(1,3),(2,3) — no unsound (2,2)
        for (Map<String, Term> m : s) {
            long x = Long.parseLong(m.get("X").toString());
            long y = Long.parseLong(m.get("Y").toString());
            assertTrue("X < Y must hold: " + x + "," + y, x < y);
        }
    }

    @Test public void allDifferentPermutations() {
        Prolog p = clp();
        List<Map<String, Term>> s = p.solve(
            "X in 1..3, Y in 1..3, Z in 1..3, all_different([X,Y,Z]), label([X,Y,Z]).");
        assertEquals(6, s.size());                              // permutations of {1,2,3}
    }

    @Test public void allDifferentPigeonholeFails() {
        Prolog p = clp();
        List<Map<String, Term>> s = p.solve(
            "X in 1..2, Y in 1..2, Z in 1..2, all_different([X,Y,Z]), label([X,Y,Z]).");
        assertTrue("3 vars over {1,2} all_different is unsatisfiable", s.isEmpty());
    }

    @Test public void linearArithmetic() {
        Prolog p = clp();
        // 2*X + 3*Y = 12, X in 0..6, Y in 0..4 -> (0,4),(3,2),(6,0)
        List<Map<String, Term>> s = p.solve(
            "X in 0..6, Y in 0..4, 2*X + 3*Y #= 12, label([X, Y]).");
        assertEquals(3, s.size());
        for (Map<String, Term> m : s) {
            long x = Long.parseLong(m.get("X").toString());
            long y = Long.parseLong(m.get("Y").toString());
            assertEquals(12, 2 * x + 3 * y);
        }
    }

    @Test public void disequalityPropagates() {
        Prolog p = clp();
        // X in 1..3, X #\= 2, X #\= 3  -> X = 1
        List<Map<String, Term>> s = p.solve("X in 1..3, X #\\= 2, X #\\= 3, label([X]).");
        assertEquals(1, s.size());
        assertEquals("1", s.get(0).get("X").toString());
    }

    @Test public void hugeDomainNoOom() {
        Prolog p = clp();
        // a 2-billion-value domain must not OOM (interval representation) and labels to the bound
        List<Map<String, Term>> s = p.solve("X in 1..2000000000, X #=< 1, label([X]).");
        assertEquals(1, s.size());
        assertEquals("1", s.get(0).get("X").toString());
    }

    @Test public void indomainRespectsConstraints() {
        Prolog p = clp();
        // X<Y over 1..3: indomain(X) must skip X=3 (it would wipe out Y), yielding X in {1,2}
        List<Map<String, Term>> s = p.solve("X in 1..3, Y in 1..3, X #< Y, indomain(X).");
        assertEquals(2, s.size());
        // unconstrained: indomain enumerates the whole domain
        assertEquals(3, p.solve("X in 1..3, indomain(X).").size());
    }

    @Test public void fdDomAndSize() {
        Prolog p = clp();
        // fd_dom returns the '..'/2 compound (engine toString is functional: "..(1, 3)").
        assertEquals("..(1, 3)", p.solve("X in 1..3, fd_dom(X, D).").get(0).get("D").toString());
        assertEquals("5", p.solve("X in 1..5, fd_size(X, N).").get(0).get("N").toString());
        // after constraints narrow the domain, fd_dom reflects it
        List<Map<String, Term>> s = p.solve("X in 1..10, X #>= 4, X #=< 6, fd_dom(X, D).");
        assertEquals("..(4, 6)", s.get(0).get("D").toString());
    }

    @Test public void linearNoOverflowUnsoundness() {
        // Regression for the review's CRITICAL: with saturating-long bounds this returned 0 (unsound).
        // 1e11*X + 1e11*Y = 5e18 has X=5e7,Y=0 as a solution (1e11*5e7 = 5e18 exactly).
        Prolog p = clp();
        List<Map<String, Term>> s = p.solve(
            "X in -100000000..100000000, Y in -100000000..100000000, "
          + "100000000000*X + 100000000000*Y #= 5000000000000000000, X #= 50000000, Y #= 0, label([X,Y]).");
        assertEquals(1, s.size());
        assertEquals("50000000", s.get(0).get("X").toString());
    }

    @Test public void labelHugeDomainRaisesResourceError() {
        // Labeling a variable left on a huge domain raises a catchable resource_error (no OOM).
        Prolog p = clp();
        List<Map<String, Term>> s = p.solve(
            "catch((X in 1..3, Y in 1..2000000000, label([X,Y])), error(resource_error(_), _), true).");
        assertEquals(1, s.size());
    }

    @Test public void floatOperandRaisesTypeError() {
        Prolog p = clp();
        List<Map<String, Term>> s = p.solve(
            "catch((X in 1..3, X #= 2.5), error(type_error(integer, _), _), true).");
        assertEquals(1, s.size());
    }

    @Test public void disequalityOverExpression() {
        // ISS-0301: X+1 #\= 5  ->  X #\= 4
        Prolog p = clp();
        List<Map<String, Term>> s = p.solve("X in 1..5, X + 1 #\\= 5, label([X]).");
        assertEquals(4, s.size());                              // 1,2,3,5
        for (Map<String, Term> m : s) assertNotEquals("4", m.get("X").toString());
    }

    @Test public void modConstraintThroughEngine() {
        // ISS-0303: X mod 3 #= 1, X in 0..10 -> X in {1,4,7,10}
        Prolog p = clp();
        List<Map<String, Term>> s = p.solve("X in 0..10, X mod 3 #= 1, label([X]).");
        assertEquals(4, s.size());
        for (Map<String, Term> m : s) assertEquals(1, Long.parseLong(m.get("X").toString()) % 3);
    }

    @Test public void noLeakBetweenQueries() {
        Prolog p = clp();
        p.solve("X in 1..3, X #= 2, label([X]).");              // constrains X in this query only
        List<Map<String, Term>> s = p.solve("X in 1..3, label([X]).");   // fresh query
        assertEquals("a fresh query must not see the prior query's constraint", 3, s.size());
    }
}
