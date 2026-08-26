package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0502 - 4.2 wave C: first-argument indexing on every clause-selection path.
/**
 * First-argument indexing acceptance (design B.7).
 *
 * <p>The v4 {@link ClauseStore} has carried an incremental first-argument index since wave W2 and
 * {@link Machine} has selected calls through it since then; wave C put {@code retract/1} and
 * {@code clause/2} on the same index and pinned the property that made ISS-2025-0340 revert
 * indexing on the old engine: <b>an index miss must never drop a clause</b>.
 *
 * <p>The correctness argument here is deliberately not "the index returns the right thing for the
 * cases I thought of". It is a randomised equivalence:
 * <ol>
 *   <li>{@link #testRandomisedCandidateSetIsComplete()} builds a predicate out of a pool of first
 *       arguments (atoms, integers of both widths, floats, strings, compounds, partial lists,
 *       {@code []} and variables) and asserts, for every goal key drawn from the same pool, that
 *       the candidate array {@link ClauseStore.Predicate#select} returns is exactly the full
 *       clause list filtered by an INDEPENDENT "could the first arguments unify?" oracle, plus
 *       possibly clauses the oracle rejects (an index is allowed to over-approximate, never to
 *       under-approximate), and that it is in source order.</li>
 *   <li>{@link #testRandomisedSolutionsMatchUnindexedScan()} runs the same program through the
 *       engine and compares every answer set against a brute-force scan of the clause list.</li>
 * </ol>
 */
public class EngineV4IndexingTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    // ------------------------------------------------------------------ helpers

    private ClauseStore.Predicate pred(String name, int arity) {
        return prolog.getV4Engine().store().lookup(name, arity);
    }

    /**
     * The independent oracle: could the first argument of this clause head unify with {@code goal}?
     * A skeleton variable is a {@link VarRef}, an unbound goal argument a {@link Variable}; either
     * matches anything. Everything else must agree on type and shape.
     */
    private static boolean couldUnify(Term a, Term b) {
        if (a instanceof VarRef || b instanceof VarRef) return true;
        if (a instanceof Variable || b instanceof Variable) return true;
        if (a instanceof Atom) return (b instanceof Atom) && ((Atom) a).getName().equals(((Atom) b).getName());
        if (a instanceof Number) return (b instanceof Number) && a.equals(b);
        if (a instanceof PrologString) {
            return (b instanceof PrologString)
                && ((PrologString) a).getStringValue().equals(((PrologString) b).getStringValue());
        }
        if (a instanceof CompoundTerm) {
            if (!(b instanceof CompoundTerm)) return false;
            CompoundTerm x = (CompoundTerm) a, y = (CompoundTerm) b;
            if (!x.getName().equals(y.getName())) return false;
            if (x.getArguments().size() != y.getArguments().size()) return false;
            for (int i = 0; i < x.getArguments().size(); i++) {
                if (!couldUnify(x.getArguments().get(i), y.getArguments().get(i))) return false;
            }
            return true;
        }
        return false;
    }

    private static Term firstArgOf(Term head) {
        if (!(head instanceof CompoundTerm)) return null;
        List<Term> as = ((CompoundTerm) head).getArguments();
        return as.isEmpty() ? null : as.get(0);
    }

    /** The pool of first arguments, as Prolog source text (index 0..N-1 is stable across runs). */
    private static final String[] POOL = {
        "a", "b", "'Zed'", "[]",
        "0", "1", "42", "-7", "100000000000000000000",
        "1.0", "0.5", "-0.0",
        "\"str\"", "\"other\"",
        "f(1)", "f(a)", "f(X)", "g(1,2)", "g(A,B)", "h",
        "[a|T]", "[a,b]", "[1|_]",
        "V", "_"
    };

    // ------------------------------------------------------------------ the property

    @Test
    public void testRandomisedCandidateSetIsComplete() {
        Random rnd = new Random(20260826L);
        for (int round = 0; round < 12; round++) {
            Prolog p = new Prolog();
            StringBuilder src = new StringBuilder();
            int n = 20 + rnd.nextInt(30);
            List<Integer> chosen = new ArrayList<Integer>();
            for (int i = 0; i < n; i++) {
                int k = rnd.nextInt(POOL.length);
                chosen.add(k);
                src.append("q(").append(POOL[k]).append(", ").append(i).append(").\n");
            }
            p.consult(src.toString());
            ClauseStore.Predicate pr = p.getV4Engine().store().lookup("q", 2);
            Clause[] all = pr.all();
            assertEquals("round " + round, n, all.length);

            for (int k = 0; k < POOL.length; k++) {
                // Parse the goal through the engine so the goal argument is a real live cell.
                Term goal = parseGoal(p, "q(" + POOL[k] + ", _)");
                Term goalArg = Unify.deref(firstArgOf(goal));
                Clause[] sel = pr.select(Clause.argKey1(goal));
                assertNotNull(sel);

                // (a) source order and membership in the full list
                int at = 0;
                for (Clause c : sel) {
                    while (at < all.length && all[at] != c) at++;
                    assertTrue("candidate out of source order for " + POOL[k], at < all.length);
                    at++;
                }
                // (b) COMPLETENESS: nothing the oracle accepts may be missing
                IdentityHashMap<Clause, Boolean> in = new IdentityHashMap<Clause, Boolean>();
                for (Clause c : sel) in.put(c, Boolean.TRUE);
                for (Clause c : all) {
                    Term headArg = firstArgOf(c.head);
                    if (couldUnify(headArg, goalArg) && !in.containsKey(c)) {
                        throw new AssertionError("index dropped a clause: goal q(" + POOL[k]
                            + ", _), clause head " + c.head);
                    }
                }
            }
        }
    }

    @Test
    public void testRandomisedSolutionsMatchUnindexedScan() {
        Random rnd = new Random(7L);
        for (int round = 0; round < 8; round++) {
            Prolog p = new Prolog();
            StringBuilder src = new StringBuilder();
            int n = 25;
            for (int i = 0; i < n; i++) {
                src.append("r(").append(POOL[rnd.nextInt(POOL.length)]).append(", ").append(i).append(").\n");
            }
            p.consult(src.toString());
            ClauseStore.Predicate pr = p.getV4Engine().store().lookup("r", 2);
            Clause[] all = pr.all();

            for (String key : POOL) {
                List<Map<String, Term>> sols = p.solve("r(" + key + ", N).");
                // brute force: every clause whose first argument could unify AND whose second
                // argument (an integer) is therefore a solution; the oracle over-approximates on
                // shape only, so re-check by unifying the whole goal against each clause head.
                int expected = 0;
                for (Clause c : all) {
                    Term goal = parseGoal(p, "r(" + key + ", _)");
                    Term headTerm = c.toTerm();                       // ':-'(Head, Body)
                    Term head = ((CompoundTerm) headTerm).getArguments().get(0);
                    Bindings b = new Bindings(null);
                    b.forceTrail++;
                    if (Unify.unify(goal, head, b)) expected++;
                    b.undo(0);
                    b.forceTrail--;
                }
                assertEquals("goal r(" + key + ", N) in round " + round, expected, sols.size());
            }
        }
    }

    /** Parse one goal with the engine's own parser. */
    private static Term parseGoal(Prolog p, String text) {
        return it.denzosoft.jprolog.core.parser.v2.TermReader.parseTerm(text, p.getOperatorTable());
    }

    // ------------------------------------------------------------------ the ISS-2025-0340 hazard

    /**
     * ISS-2025-0340 reverted indexing on the old engine because
     * {@code KnowledgeBase.getRulesWithFirstArgIndex} answered EMPTY for a predicate whose index had
     * never been populated. Here the KnowledgeBase is written behind the clause store's back, so the
     * store's own index has never seen those clauses: the next lookup must re-sync and answer with
     * every one of them.
     */
    @Test
    public void testIndexMissDegradesToFullClauseList() {
        prolog.consult("s(a, 1).\n");
        KnowledgeBase kb = prolog.getV4Engine().kb();
        for (int i = 2; i <= 6; i++) {
            kb.addRule(new Rule(new CompoundTerm(new Atom("s"),
                Arrays.asList((Term) new Atom("a"), (Term) Number.valueOf(i))), new ArrayList<Term>()));
        }
        assertEquals(6, prolog.solve("s(a, N).").size());
        assertEquals(6, prolog.solve("s(X, N).").size());
        // a key that occurs in no clause head at all: the answer is "no candidates", not "all"
        assertTrue(prolog.solve("s(zzz, _).").isEmpty());
        // and the store agrees
        ClauseStore.Predicate pr = pred("s", 2);
        assertEquals(6, pr.select(Clause.argKey(new Atom("a"))).length);
    }

    /** A variable-headed clause must be a candidate for EVERY key, in source order. */
    @Test
    public void testVariableHeadedClauseIsAlwaysACandidate() {
        prolog.consult("t(a, 1).\nt(X, 2).\nt(b, 3).\nt(_, 4).\n");
        ClauseStore.Predicate pr = pred("t", 2);
        Clause[] sel = pr.select(Clause.argKey(new Atom("a")));
        assertEquals(3, sel.length);                                    // a/1, X/2, _/4
        assertEquals(4, pr.select(null).length);
        assertEquals(3, prolog.solve("t(a, N).").size());
        assertEquals(2, prolog.solve("t(zzz, N).").size());             // only the two var heads
        assertEquals(4, prolog.solve("t(_, N).").size());
    }

    /** Integers, floats and strings are type-faithful keys: 1, 1.0 and "1" are three buckets. */
    @Test
    public void testKeysAreTypeFaithful() {
        prolog.consult("u(1, int).\nu(1.0, float).\nu(\"1\", string).\nu('1', atom).\n");
        assertEquals(1, prolog.solve("u(1, K).").size());
        assertEquals("int", prolog.solve("u(1, K).").get(0).get("K").toString());
        assertEquals("float", prolog.solve("u(1.0, K).").get(0).get("K").toString());
        assertEquals("atom", prolog.solve("u('1', K).").get(0).get("K").toString());
        assertEquals(4, prolog.solve("u(_, K).").size());
    }

    // ------------------------------------------------------------------ retract/1 and clause/2

    @Test
    public void testRetractSelectsThroughTheIndex() {
        StringBuilder sb = new StringBuilder(":- dynamic v/2.\n");
        for (int i = 0; i < 300; i++) sb.append("v(k").append(i).append(", ").append(i).append(").\n");
        prolog.consult(sb.toString());
        assertEquals(1, prolog.solve("retract(v(k150, N)).").size());
        assertEquals(299, prolog.solve("v(_, _).").size());
        assertTrue(prolog.solve("retract(v(k150, _)).").isEmpty());     // gone
        // an unbound first argument still retracts the first clause in source order
        List<Map<String, Term>> r = prolog.solve("once(retract(v(K, _))).");
        assertEquals(1, r.size());
        assertEquals("k0", r.get(0).get("K").toString());
        assertEquals(298, prolog.solve("v(_, _).").size());
    }

    @Test
    public void testRetractSeesVariableHeadedClauses() {
        prolog.consult(":- dynamic w/2.\nw(a, 1).\nw(X, 2).\nw(b, 3).\n");
        // retract/1 is re-satisfiable, and retract(w(a, N)) must consider the variable-headed
        // clause too, in source order: the fact first, then w(X, 2).
        List<Map<String, Term>> r = prolog.solve("retract(w(a, N)).");
        assertEquals(2, r.size());
        assertEquals("1", r.get(0).get("N").toString());
        assertEquals("2", r.get(1).get("N").toString());
        assertTrue(prolog.solve("retract(w(a, _)).").isEmpty());
        assertEquals(1, prolog.solve("w(_, _).").size());
    }

    @Test
    public void testClauseSelectsThroughTheIndex() {
        StringBuilder sb = new StringBuilder(":- dynamic x/2.\n");
        for (int i = 0; i < 300; i++) sb.append("x(k").append(i).append(", ").append(i).append(").\n");
        sb.append("x(Any, generic).\n");
        prolog.consult(sb.toString());
        assertEquals(2, prolog.solve("clause(x(k7, B), true).").size());   // the fact + the var head
        assertEquals(1, prolog.solve("clause(x(zzz, B), true).").size());  // only the var head
        assertEquals(301, prolog.solve("clause(x(_, _), true).").size());
    }

    /** The logical update view still holds when retract/1 walks an indexed candidate array. */
    @Test
    public void testRetractKeepsTheLogicalUpdateView() {
        prolog.consult(":- dynamic y/1.\ny(1).\ny(2).\ny(3).\n");
        List<Map<String, Term>> sols = prolog.solve("y(N), retract(y(N)), fail ; true.");
        assertFalse(sols.isEmpty());
        assertTrue(prolog.solve("y(_).").isEmpty());
    }

    // ------------------------------------------------------------------ maintenance

    /** The index is maintained incrementally by assert and invalidated by compaction. */
    @Test
    public void testIndexIsMaintainedIncrementally() {
        prolog.consult(":- dynamic p/2.\n");
        for (int i = 0; i < 100; i++) prolog.solve("assertz(p(k" + i + ", " + i + ")).");
        assertEquals(1, prolog.solve("p(k99, N).").size());
        prolog.solve("asserta(p(k99, first)).");
        List<Map<String, Term>> s = prolog.solve("p(k99, N).");
        assertEquals(2, s.size());
        assertEquals("first", s.get(0).get("N").toString());
        for (int i = 0; i < 100; i++) prolog.solve("retract(p(k" + i + ", " + i + ")).");
        assertEquals(1, prolog.solve("p(_, _).").size());
        assertEquals(1, prolog.solve("p(k99, _).").size());
    }
}
// END_CHANGE: ISS-2025-0502
