package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.FdVar;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Proves the clean-room CLP(FD) core fixes the legacy store's audit findings:
 * interval domains (no OOM), proper {@code #\=} propagation, sound labeling,
 * identity-keyed per-instance store (no leak), overflow-safe bounds, trail backtracking.
 */
public class ClpfdV2Test {

    // ---- Finding: huge domains caused OutOfMemoryError (TreeSet of billions of ints) ----
    @Test public void hugeDomainIsCheap() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 2147483647L);          // ~2.1 billion values, one interval
        assertEquals(2147483647L, s.dom(x).size());
        assertTrue(s.removeBelow(x, 2000000000L));        // narrowing is O(#intervals), not O(#values)
        assertEquals(2000000000L, s.dom(x).min());
        // intersect with a small range stays an interval
        assertTrue(s.narrow(x, IntervalDomain.interval(2147483000L, 2147483647L)));
        assertEquals(648L, s.dom(x).size());
    }

    // ---- Finding: #\= between two non-singleton vars never propagated ----
    @Test public void disequalityPropagatesWhenSingleton() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 3);
        FdVar y = s.newVar("Y", 2, 2);                    // Y fixed at 2
        assertTrue(s.addConstraint(new Constraint.Cmp(x, Constraint.Rel.NE, y)));
        // X must lose the value 2
        assertFalse(s.dom(x).contains(2));
        assertTrue(s.dom(x).contains(1));
        assertTrue(s.dom(x).contains(3));
    }

    // ---- Finding: all_different missed pigeonhole infeasibility ----
    @Test public void allDifferentPigeonholeFails() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 2);
        FdVar y = s.newVar("Y", 1, 2);
        FdVar z = s.newVar("Z", 1, 2);                    // 3 vars, 2 values -> impossible
        boolean ok = s.addConstraint(new Constraint.AllDifferent(Arrays.asList(x, y, z)));
        assertFalse("all_different over {1,2}^3 must be unsatisfiable", ok);
    }

    // ---- Finding: indomain/labeling produced unsound solutions (e.g. {X=2,Y=2} for X#<Y) ----
    @Test public void labelingIsSound() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 3);
        FdVar y = s.newVar("Y", 1, 3);
        assertTrue(s.addConstraint(new Constraint.Cmp(x, Constraint.Rel.LT, y)));   // X < Y
        List<Map<FdVar, Long>> sols = Labeler.labelAll(s, Arrays.asList(x, y));
        assertFalse(sols.isEmpty());
        for (Map<FdVar, Long> m : sols) {
            assertTrue("every solution must satisfy X < Y, got " + m.get(x) + "<" + m.get(y),
                m.get(x) < m.get(y));
        }
        // X<Y over 1..3 : (1,2),(1,3),(2,3) = 3 solutions
        assertEquals(3, sols.size());
    }

    @Test public void allDifferentLabelingPermutations() {
        ClpStore s = new ClpStore();
        FdVar a = s.newVar("A", 1, 3);
        FdVar b = s.newVar("B", 1, 3);
        FdVar c = s.newVar("C", 1, 3);
        assertTrue(s.addConstraint(new Constraint.AllDifferent(Arrays.asList(a, b, c))));
        List<Map<FdVar, Long>> sols = Labeler.labelAll(s, Arrays.asList(a, b, c));
        assertEquals("3! permutations of {1,2,3}", 6, sols.size());
        for (Map<FdVar, Long> m : sols) {
            assertNotEquals(m.get(a), m.get(b));
            assertNotEquals(m.get(a), m.get(c));
            assertNotEquals(m.get(b), m.get(c));
        }
    }

    // ---- Finding: ADD/SUB bounds overflowed int; v2 uses overflow-safe long ----
    @Test public void sumPropagationAndNoOverflow() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 3);
        FdVar y = s.newVar("Y", 10, 20);
        FdVar z = s.newVar("Z", 0, 100);
        assertTrue(s.addConstraint(new Constraint.Sum(x, y, z)));     // Z = X + Y
        assertEquals(11, s.dom(z).min());                              // 1+10
        assertEquals(23, s.dom(z).max());                             // 3+20

        // huge constant bound must not overflow into a bogus interval
        ClpStore s2 = new ClpStore();
        FdVar a = s2.newVar("A", 1, 3);
        FdVar k = s2.newVar("K", 2147483646L, 2147483646L);
        FdVar sum = s2.newVar("S", 0, 9223372036854775807L);
        assertTrue(s2.addConstraint(new Constraint.Sum(a, k, sum)));
        assertEquals(2147483647L, s2.dom(sum).min());                 // 1 + 2147483646
        assertEquals(2147483649L, s2.dom(sum).max());                 // 3 + 2147483646  (no int overflow!)
    }

    // ---- Finding: global singleton store leaked across queries/engines ----
    @Test public void storesAreIndependent() {
        ClpStore s1 = new ClpStore();
        FdVar x1 = s1.newVar("X", 1, 3);
        s1.assign(x1, 2);
        ClpStore s2 = new ClpStore();                                 // a brand-new store
        FdVar x2 = s2.newVar("X", 1, 3);                              // same name, different identity
        assertEquals("fresh store must not see the other store's assignment", 3, s2.dom(x2).size());
    }

    // ---- product (interval multiplication) ----
    @Test public void productBounds() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 2, 3);
        FdVar y = s.newVar("Y", 4, 5);
        FdVar z = s.newVar("Z", 0, 1000);
        assertTrue(s.addConstraint(new Constraint.Mul(x, y, z)));     // Z = X*Y
        assertEquals(8, s.dom(z).min());                              // 2*4
        assertEquals(15, s.dom(z).max());                            // 3*5
    }

    // ---- absolute value ----
    @Test public void absoluteValue() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", -3, 2);
        FdVar y = s.newVar("Y", -100, 100);
        assertTrue(s.addConstraint(new Constraint.Abs(x, y)));        // Y = |X|
        assertEquals(0, s.dom(y).min());
        assertEquals(3, s.dom(y).max());
    }

    // ---- N-ary linear:  2X + 3Y = 12 ----
    @Test public void linearEquation() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 0, 6);
        FdVar y = s.newVar("Y", 0, 4);
        assertTrue(s.addConstraint(new Constraint.Linear(
            new long[]{2, 3}, new FdVar[]{x, y}, Constraint.Rel.EQ, 12)));
        List<Map<FdVar, Long>> sols = Labeler.labelAll(s, Arrays.asList(x, y));
        for (Map<FdVar, Long> m : sols) {
            assertEquals(12, 2 * m.get(x) + 3 * m.get(y));
        }
        // (0,4),(3,2),(6,0)
        assertEquals(3, sols.size());
    }

    // ---- combined puzzle: X+Y+Z = 6, all_different, in 1..3  -> permutations of {1,2,3} ----
    @Test public void linearPlusAllDifferentPuzzle() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 3);
        FdVar y = s.newVar("Y", 1, 3);
        FdVar z = s.newVar("Z", 1, 3);
        assertTrue(s.addConstraint(new Constraint.Linear(
            new long[]{1, 1, 1}, new FdVar[]{x, y, z}, Constraint.Rel.EQ, 6)));
        assertTrue(s.addConstraint(new Constraint.AllDifferent(Arrays.asList(x, y, z))));
        List<Map<FdVar, Long>> sols = Labeler.labelAll(s, Arrays.asList(x, y, z));
        assertEquals(6, sols.size());
        for (Map<FdVar, Long> m : sols) {
            assertEquals(6L, (long) (m.get(x) + m.get(y) + m.get(z)));
        }
    }

    // ---- modulo:  Z = X mod M ----
    @Test public void modConstraint() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 0, 10);
        FdVar z = s.newVar("Z", -100, 100);
        assertTrue(s.addConstraint(new Constraint.Mod(x, 3, z)));   // Z = X mod 3
        assertEquals(0, s.dom(z).min());                            // Z in 0..2
        assertEquals(2, s.dom(z).max());
        assertTrue(s.assign(x, 7) && s.propagate());
        assertTrue(s.dom(z).isSingleton());
        assertEquals(1, s.dom(z).value());                          // 7 mod 3 = 1
    }

    // ---- reification:  B #<==> (X #< Y) ----
    @Test public void reifiedLessThan() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 3);
        FdVar y = s.newVar("Y", 1, 3);
        FdVar b = s.newVar("B", 0, 1);
        assertTrue(s.addConstraint(new Constraint.Reified(b, new Constraint.Cmp(x, Constraint.Rel.LT, y))));
        List<Map<FdVar, Long>> sols = Labeler.labelAll(s, Arrays.asList(x, y, b));
        assertEquals(9, sols.size());                                // all 3x3 (X,Y), B determined
        for (Map<FdVar, Long> m : sols) {
            long expected = m.get(x) < m.get(y) ? 1 : 0;
            assertEquals("B must equal (X<Y) for X=" + m.get(x) + ",Y=" + m.get(y), expected, (long) m.get(b));
        }
    }

    @Test public void reifiedForcedTrueConstrains() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 3);
        FdVar y = s.newVar("Y", 1, 3);
        FdVar b = s.newVar("B", 1, 1);                               // force B=1  => X < Y must hold
        assertTrue(s.addConstraint(new Constraint.Reified(b, new Constraint.Cmp(x, Constraint.Rel.LT, y))));
        List<Map<FdVar, Long>> sols = Labeler.labelAll(s, Arrays.asList(x, y));
        assertEquals(3, sols.size());                                // (1,2),(1,3),(2,3)
        for (Map<FdVar, Long> m : sols) assertTrue(m.get(x) < m.get(y));
    }

    // ---- trail-based backtracking restores domains exactly ----
    @Test public void trailUndoRestoresDomains() {
        ClpStore s = new ClpStore();
        FdVar x = s.newVar("X", 1, 10);
        int mark = s.mark();
        s.removeBelow(x, 5);
        s.removeAbove(x, 7);
        assertEquals(5, s.dom(x).min());
        assertEquals(7, s.dom(x).max());
        s.undo(mark);
        assertEquals(1, s.dom(x).min());
        assertEquals(10, s.dom(x).max());
    }
}
