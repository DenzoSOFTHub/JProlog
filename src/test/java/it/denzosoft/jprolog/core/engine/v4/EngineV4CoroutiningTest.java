package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.InferenceLimitException;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0457..0462 - engine v4 wave W4 (coroutining, design B.9).
/**
 * Acceptance tests for wave W4: the coroutining wake queue, the SWI attributed-variable protocol,
 * {@code freeze/frozen/when/dif} as prelude Prolog, CLP(FD) on the attribute hook, and the removal
 * of cross-query coroutining.
 *
 * <p>Like {@link EngineV4Test} and {@link EngineV4LibraryTest}, every test selects v4 in
 * {@link #setUp} and restores the previous selection in {@link #tearDown}, so the class behaves
 * identically under the default profile and under {@code -Pengine-v4}.
 */
public class EngineV4CoroutiningTest {

    private Prolog prolog;
    private boolean prevV4;
    private boolean prevV2;

    @Before
    public void setUp() {
        prevV4 = Prolog.isUsingV4Engine();
        prevV2 = Prolog.isUsingV2Engine();
        Prolog.setUseV4Engine(true);
        prolog = new Prolog();
    }

    @After
    public void tearDown() {
        Prolog.setUseV4Engine(prevV4);
        Prolog.setUseV2Engine(prevV2);
    }

    private void yes(String query) {
        assertFalse("expected a solution: " + query, prolog.solve(query).isEmpty());
    }

    private void no(String query) {
        assertTrue("expected no solution: " + query, prolog.solve(query).isEmpty());
    }

    // ================================================================ ISS-2025-0457 (wake queue)

    /** The bindings a woken goal makes are ordinary bindings, and they propagate. */
    @Test
    public void testISS0457_WokenGoalBindingsPropagate() {
        yes("freeze(X, Y = hello), X = 1, Y == hello.");
        yes("when(nonvar(X), Y = done), X = 1, Y == done.");
        yes("when(ground(X-Y), Z is X+Y), X = 1, Y = 2, Z == 3.");
    }

    /** Backtracking undoes the attribute change, so the suspension is re-armed for each solution. */
    @Test
    public void testISS0457_AttributeChangesAreTrailed() {
        List<Map<String, Term>> s = prolog.solve(
            "findall(Y, (freeze(X, Y = got(X)), member(X, [1,2])), L).");
        assertEquals(1, s.size());
        assertEquals("[got(1), got(2)]", String.valueOf(s.get(0).get("L")));
        // the frozen goal must be re-armed on backtracking, not consumed by the first solution
        assertEquals(2, prolog.solve("freeze(X, true), member(X, [a,b]).").size());
    }

    /**
     * A head unification that binds an attributed cell and then fails on a later argument must
     * leave NO wake behind (the queue push is trailed like any other undo action).
     */
    @Test
    public void testISS0457_FailedUnificationQueuesNothing() {
        prolog.consult(":- dynamic(fired/1).\np(1, b).\n");
        // f(X, a) does not match p(1, b): X is bound to 1 and then the second argument fails.
        no("freeze(X, assertz(fired(yes))), p(X, a).");
        no("fired(yes).");
    }

    /** Var-var aliasing merges the attributes and wakes on the eventual binding. */
    @Test
    public void testISS0457_VarVarAliasingMergesAttributes() {
        yes("freeze(X, G = fired), X = Y, Y = 1, G == fired.");
        yes("freeze(X, G1 = a), freeze(Y, G2 = b), X = Y, Y = 1, G1 == a, G2 == b.");
    }

    /** An exception thrown by a woken goal reaches the enclosing catch/3. */
    @Test
    public void testISS0457_WokenGoalExceptionPropagates() {
        yes("catch((freeze(X, throw(boom)), X = 1), boom, true).");
        yes("catch((when(nonvar(X), throw(bang)), X = 1), bang, true).");
    }

    /** A woken goal is an ordinary goal: the inference budget can abort it. */
    @Test
    public void testISS0457_WokenGoalIsCancellable() {
        prolog.consult("loop :- loop.\n");
        prolog.setInferenceBudget(200000);
        try {
            prolog.solve("freeze(X, loop), X = 1.");
            fail("a runaway woken goal must hit the inference budget");
        } catch (InferenceLimitException expected) {
            // the trust model: NOT a PrologException, so catch/3 cannot swallow it
        } finally {
            prolog.setInferenceBudget(0);
        }
    }

    // ================================================================ ISS-2025-0458 (protocol)

    @Test
    public void testISS0458_PutGetDelAttrAndAttvar() {
        yes("put_attr(X, m, 1), get_attr(X, m, V), V == 1.");
        yes("put_attr(X, m, 1), put_attr(X, m, 2), get_attr(X, m, V), V == 2.");
        yes("put_attr(X, m, 1), del_attr(X, m), \\+ get_attr(X, m, _).");
        yes("del_attr(X, m).");                                  // SWI: succeeds when absent
        yes("put_attr(X, m, 1), attvar(X).");
        no("attvar(X).");
        no("attvar(foo).");
        yes("put_attr(X, m, 1), \\+ var(1), var(X).");
        // put_attr/3 on a non-variable is a type error
        yes("catch(put_attr(foo, m, 1), error(type_error(variable, foo), _), true).");
    }

    @Test
    public void testISS0458_TermAttvars() {
        yes("put_attr(X, m, 1), term_attvars(f(X, Y), L), L == [X].");
        yes("term_attvars(f(A, B), L), L == [].");
        yes("put_attr(X, m, 1), put_attr(Y, m, 2), term_attvars(f(X, g(Y)), L), L = [X, Y].");
    }

    /** copy_term/3: the copy carries no attributes, the residual goals talk about the COPY. */
    @Test
    public void testISS0458_CopyTerm3ReportsResidualGoals() {
        yes("freeze(X, foo(X)), copy_term(X, Y, Gs), Gs == [freeze(Y, foo(Y))].");
        yes("freeze(X, foo(X)), copy_term(X, Y, _), \\+ attvar(Y).");
        yes("copy_term(f(A, B), C, Gs), Gs == [].");
        yes("put_attr(X, mymod, hello), copy_term(X, Y, Gs), Gs == [put_attr(Y, mymod, hello)].");
        yes("dif(X, Y), copy_term(f(X, Y), _, Gs), Gs \\== [].");
    }

    /** A user-defined Module:attr_unify_hook/2 is called through the normal goal stack. */
    @Test
    public void testISS0458_UserDefinedAttrUnifyHook() {
        prolog.consult("mymod:attr_unify_hook(AttValue, Other) :- AttValue == Other.\n");
        yes("put_attr(X, mymod, 7), X = 7.");
        no("put_attr(X, mymod, 7), X = 8.");
        // the hook's own bindings propagate like any other goal's
        prolog.consult("other:attr_unify_hook(Att, Other) :- Att = Other.\n");
        yes("put_attr(X, other, Seen), X = 5, Seen == 5.");
        // freeze must keep working next to a user hook (the :/2 predicate now has clauses)
        yes("freeze(Z, G = fired), Z = 1, G == fired.");
    }

    /** An attribute of a module with no hook is inert data, as on the legacy engines. */
    @Test
    public void testISS0458_UnknownModuleIsInert() {
        yes("put_attr(X, nosuchmodule, 7), X = 8, X == 8.");
    }

    @Test
    public void testISS0458_Unifiable() {
        yes("unifiable(f(X, b), f(a, Y), U), U = [_, _].");
        yes("unifiable(X, Y, U), U = [_].");
        no("unifiable(a, b, _).");
        yes("unifiable(a, a, U), U == [].");
        // unifiable/3 must NOT leave the probe's bindings behind
        yes("unifiable(X, 1, _), var(X).");
    }

    // ================================================================ ISS-2025-0459 (prelude)

    @Test
    public void testISS0459_FreezeAndFrozen() {
        yes("freeze(X, true), frozen(X, G), G == true.");
        yes("freeze(X, foo), frozen(X, G), G == foo.");
        yes("frozen(Z, G), G == true.");
        yes("frozen(a, G), G == true.");
        // several frozen goals aggregate into a conjunction and all fire
        yes("freeze(X, A = 1), freeze(X, B = 2), X = go, A == 1, B == 2.");
        // an already-bound variable runs the goal at once
        yes("X = 1, freeze(X, Y = now), Y == now.");
        // a frozen goal that fails fails the binding
        no("freeze(X, fail), X = 1.");
    }

    @Test
    public void testISS0459_WhenConditions() {
        yes("when(nonvar(X), Y = 1), X = a, Y == 1.");
        yes("when(ground(f(X, Y)), Z = 1), X = a, var(Z), Y = b, Z == 1.");
        yes("when((nonvar(X), nonvar(Y)), Z = 1), X = a, var(Z), Y = b, Z == 1.");
        yes("when(?=(X, Y), Z = 1), X = a, Y = b, Z == 1.");
        // an already-true condition runs the goal immediately
        yes("when(nonvar(a), Y = now), Y == now.");
        // errors
        yes("catch(when(_, true), error(instantiation_error, _), true).");
        yes("catch(when(bogus, true), error(domain_error(when_condition, bogus), _), true).");
    }

    /** A disjunctive condition attached to two variables must run the goal exactly ONCE. */
    @Test
    public void testISS0459_WhenDisjunctionFiresOnce() {
        prolog.consult(":- dynamic(cnt/1).\n");
        prolog.solve("assertz(cnt(0)).");
        prolog.solve("when((nonvar(A) ; nonvar(B)), (retract(cnt(N)), N1 is N+1, assertz(cnt(N1)))),"
                   + " A = 1, B = 2.");
        assertEquals("the goal must have run exactly once", 1, prolog.solve("cnt(1).").size());
    }

    @Test
    public void testISS0459_Dif() {
        yes("dif(X, Y), X = a, Y = b.");
        no("dif(X, a), X = a.");
        no("dif(f(X), f(Y)), X = 1, Y = 1.");
        yes("dif(f(X), f(Y)), X = 1, Y = 2.");
        no("dif(X, X).");
        yes("dif(a, b).");
        no("dif(a, a).");
        // partially instantiated: re-suspends on the remaining unifier variables
        yes("dif(f(X, Y), f(1, 2)), X = 1, Y = 3.");
        no("dif(f(X, Y), f(1, 2)), X = 1, Y = 2.");
        // a dif that can no longer be violated is simply dropped
        yes("dif(f(X), g(Y)), X = 1, Y = 1.");
        // backtracking re-arms the constraint: b is excluded, a and c are not
        assertEquals(2, prolog.solve("dif(X, b), member(X, [a, b, c]).").size());
    }

    // ================================================================ ISS-2025-0460 (CLP(FD))

    /**
     * The two tests that used to fail without the {@code Machine.nameIndex} shim. They pass with
     * the shim deleted because the CLP(FD) bridge now keeps the engine cell of every FD variable
     * it attributed, i.e. an FD variable is an ordinary attributed cell.
     */
    @Test
    public void testISS0460_LabelingBindsFunctionallyDeterminedVariables() {
        List<Map<String, Term>> s = prolog.solve("C in 1..3, D #= C*2+1, label([C]).");
        assertEquals(3, s.size());
        for (Map<String, Term> m : s) {
            long c = Long.parseLong(String.valueOf(m.get("C")));
            assertEquals(String.valueOf(c * 2 + 1), String.valueOf(m.get("D")));
        }
        s = prolog.solve("Y in 1..5, Z #= abs(Y - 3), label([Y]).");
        assertEquals(5, s.size());
        for (Map<String, Term> m : s) {
            long y = Long.parseLong(String.valueOf(m.get("Y")));
            assertEquals(String.valueOf(Math.abs(y - 3)), String.valueOf(m.get("Z")));
        }
    }

    /** The domain is enforced through the v4 attribute hook, not the legacy one. */
    @Test
    public void testISS0460_UnificationRespectsTheDomain() {
        no("X in 1..3, X = 5.");
        yes("X in 1..3, X = 2.");
        no("X in 1..3, X = a.");
        yes("X in 1..3, Y in 3..5, X = Y.");                 // domains intersect: 3
        no("X in 1..3, Y in 5..7, X = Y.");                  // disjoint domains: the alias fails
        assertEquals(2, prolog.solve("A in 1..4, B in 1..4, C in 1..6, A*A + B*B #= C*C, label([A,B,C]).").size());
    }

    /** The compatibility shim is gone: the machine no longer maps a variable NAME to a cell. */
    @Test
    public void testISS0460_TheNameIndexShimIsGone() {
        for (java.lang.reflect.Method m : Machine.class.getDeclaredMethods()) {
            assertFalse("Machine.cellFor must not exist any more", "cellFor".equals(m.getName()));
            assertFalse("Machine.indexCells must not exist any more", "indexCells".equals(m.getName()));
        }
        for (java.lang.reflect.Field f : Machine.class.getDeclaredFields()) {
            assertFalse("Machine.nameIndex must not exist any more", "nameIndex".equals(f.getName()));
        }
    }

    // ================================================================ ISS-2025-0461 (no leak)

    /**
     * Design decision 3 (B.17, approved): a query's variables die with the query. On the v2 engine
     * the same three sequences fire the suspension of the FIRST query inside the second one.
     */
    @Test
    public void testISS0461_NoCrossQueryCoroutining() {
        prolog.solve("when(nonvar(X), throw(leak)).");
        assertEquals("a finished query's when/2 must not fire later", 1, prolog.solve("X = 1.").size());

        prolog.solve("freeze(Y, throw(leak)).");
        assertEquals("a finished query's freeze/2 must not fire later", 1, prolog.solve("Y = 1.").size());

        prolog.solve("dif(Z, a).");
        assertEquals("a finished query's dif/2 must not constrain a later one",
            1, prolog.solve("Z = a.").size());
    }

    // ================================================================ ISS-2025-0462 (residuals)

    @Test
    public void testISS0462_ResidualGoalsAccessor() {
        List<Map<String, Term>> s = prolog.solve("freeze(X, foo(X)).");
        assertEquals(1, s.size());
        List<Term> residual = prolog.residualGoals(s.get(0));
        assertEquals(1, residual.size());
        assertTrue("expected a freeze/2 residual, got " + residual,
            String.valueOf(residual.get(0)).startsWith("freeze("));

        s = prolog.solve("dif(A, b).");
        residual = prolog.residualGoals(s.get(0));
        assertFalse("expected a dif/2 residual", residual.isEmpty());
        assertTrue("expected a dif/2 residual, got " + residual,
            String.valueOf(residual.get(0)).startsWith("dif("));

        s = prolog.solve("B in 1..3.");
        residual = prolog.residualGoals(s.get(0));
        assertFalse("expected a CLP(FD) domain residual", residual.isEmpty());
        assertTrue("expected an in/2 domain residual, got " + residual,
            String.valueOf(residual.get(0)).startsWith("in("));

        assertTrue("an unconstrained answer has no residual goals",
            prolog.residualGoals(prolog.solve("C = 1.").get(0)).isEmpty());
    }

    // ================================================================ four-port trace

    /** A woken goal is traced like any other call (invariant 10 of the progress report). */
    @Test
    public void testISS0457_WokenGoalIsTraced() {
        java.io.ByteArrayOutputStream buf = new java.io.ByteArrayOutputStream();
        java.io.PrintStream ps = new java.io.PrintStream(buf);
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            prolog.consult("woken_probe.\n");
            prolog.setTracing(true);
            prolog.solve("freeze(X, woken_probe), X = 1.");
        } finally {
            prolog.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(null);
        }
        String out = buf.toString();
        assertTrue("the woken goal must show a Call port, got:\n" + out, out.contains("Call: ") && out.contains("woken_probe"));
        assertTrue("the woken goal must show an Exit port, got:\n" + out, out.contains("Exit: "));
    }
}
// END_CHANGE: ISS-2025-0457..0462
