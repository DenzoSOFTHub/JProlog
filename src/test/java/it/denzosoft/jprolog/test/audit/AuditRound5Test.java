package it.denzosoft.jprolog.test.audit;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.AtomTable;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Audit Round 5 — tests for the issues identified in the 5th deep audit.
 * Each test asserts the CORRECT behavior. Tests fail before the
 * corresponding fix is applied.
 */
public class AuditRound5Test {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    // ===================================================================
    // #1 — AtomTable interning broken by GC race
    // ===================================================================

    @Test
    public void test1_atomTable_internReturnsNonNull() {
        // intern() must never return null
        for (int i = 0; i < 100; i++) {
            Atom a = AtomTable.intern("test_atom_" + i);
            assertNotNull("intern must not return null", a);
            assertEquals("test_atom_" + i, a.getName());
        }
    }

    @Test
    public void test1_atomTable_sameInstanceUnderGC() {
        // Two consecutive intern calls return same Atom instance (canonical interning)
        // Forcing GC must not break interning
        Atom a1 = AtomTable.intern("audit_unique_atom_abc");
        System.gc();
        try { Thread.sleep(50); } catch (InterruptedException ignored) {}
        Atom a2 = AtomTable.intern("audit_unique_atom_abc");
        assertNotNull("a1 must not be null", a1);
        assertNotNull("a2 must not be null", a2);
        // Interning guarantees: if a1 is still referenced, a2 == a1
        assertSame("intern must return same instance for live atom", a1, a2);
    }

    // ===================================================================
    // #2 — .jpc serialization of cyclic terms
    // ===================================================================

    @Test
    public void test2_jpc_cyclicTermDoesNotStackOverflow() {
        // Cyclic term serialization must not stack-overflow.
        // Either throws a controlled error or handles cycle via visited set.
        prolog.solve("set_prolog_flag(occurs_check, false).");
        try {
            // X = f(X) creates cyclic term (occurs-check off)
            // We can't directly test JpcWriter from Prolog without consulting,
            // but the writer should handle cycles. Test via assertz which uses
            // KB serialization paths internally.
            prolog.solve("X = f(X), assertz(cyclic_term_test(X)).");
            // No StackOverflowError — good
        } catch (StackOverflowError e) {
            fail("Cyclic term caused StackOverflowError — JpcWriter must use cycle detection");
        } catch (RuntimeException e) {
            // Any controlled error is acceptable
        }
    }

    // ===================================================================
    // #3 — Exception terms must be proper error/2 terms
    // ===================================================================

    @Test
    public void test3_between_typeError_isStructured() {
        // catch must unify the exception with a structured error term
        List<Map<String, Term>> r = prolog.solve(
            "catch(between(1.5, 3, _), error(type_error(integer, _), _), Caught = yes).");
        assertEquals(1, r.size());
        assertEquals("yes", r.get(0).get("Caught").toString());
    }

    @Test
    public void test3_functor_typeError_isStructured() {
        // functor(X, 3.14, 1) should throw type_error(atom, 3.14)
        List<Map<String, Term>> r = prolog.solve(
            "catch(functor(_, 3.14, 1), error(type_error(atom, _), _), Caught = yes).");
        assertEquals(1, r.size());
        assertEquals("yes", r.get(0).get("Caught").toString());
    }

    @Test
    public void test3_univ_typeError_isStructured() {
        // T =.. [3, a, b] should throw type_error(atom, 3) per ISO §8.5.3
        List<Map<String, Term>> r = prolog.solve(
            "catch((_ =.. [3, a, b]), error(type_error(atom, _), _), Caught = yes).");
        assertEquals(1, r.size());
        assertEquals("yes", r.get(0).get("Caught").toString());
    }

    // ===================================================================
    // #4 — Trail cleanup on exception
    // ===================================================================

    @Test
    public void test4_trail_clearedOnException() {
        // After a failed query throws, subsequent queries must not see stale trail entries.
        prolog.solve("nb_setval(probe4, init).");
        try {
            prolog.solve("undefined_predicate_xyz_audit(X).");
        } catch (RuntimeException ignored) {}
        // Now a fresh disjunction with b_setval — failure should rollback
        prolog.solve(
            "b_setval(probe4, modified), (b_setval(probe4, alt), fail ; true), " +
            "b_getval(probe4, V), nb_setval(probe4_result, V).");
        List<Map<String, Term>> r = prolog.solve("nb_getval(probe4_result, V).");
        assertEquals("modified", r.get(0).get("V").toString());
    }

    // ===================================================================
    // #7 — Module-qualified call enforces export
    // ===================================================================

    @Test
    public void test7_moduleQualifiedCall_exportEnforced() {
        prolog.consult(":- module(secret_module_r5, [exported_pred/1]).");
        prolog.consult("exported_pred(seen).");
        prolog.consult("private_pred(hidden).");
        // Module declared back to 'user' for caller
        prolog.consult(":- module(user, []).");

        // exported_pred is accessible
        List<Map<String, Term>> r1 = prolog.solve("secret_module_r5:exported_pred(X).");
        assertEquals("exported predicate must be visible", 1, r1.size());

        // private_pred must NOT be accessible
        try {
            List<Map<String, Term>> r2 = prolog.solve("secret_module_r5:private_pred(X).");
            assertEquals("private predicate must not be visible", 0, r2.size());
        } catch (RuntimeException e) {
            // existence_error is also acceptable per ISO
        }
    }
}
