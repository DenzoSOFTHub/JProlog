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

    // ===================================================================
    // Minor — CLP(FD) library auto-load directive
    // ===================================================================

    @Test
    public void testMinor_useModuleLibraryClpfd_accepted() {
        // :- use_module(library(clpfd)) must be accepted without error
        // (constraints are already registered as built-ins; directive is a no-op)
        prolog.consult(":- use_module(library(clpfd)).");
        // Sanity: regular query still works after the directive
        List<Map<String, Term>> r = prolog.solve("X = 42.");
        assertEquals("42", r.get(0).get("X").toString());
    }

    @Test
    public void testMinor_useModuleLibraryLists() {
        // library(lists) is also auto-accepted (lists predicates are built-in)
        prolog.consult(":- use_module(library(lists)).");
        List<Map<String, Term>> r = prolog.solve("append([a, b], [c, d], L).");
        assertEquals("[a, b, c, d]", r.get(0).get("L").toString());
    }

    // ===================================================================
    // Minor — Zigzag varint round-trip
    // ===================================================================

    @Test
    public void testMinor_zigzagVarintRoundTrip() throws Exception {
        // Negative and positive integers must round-trip
        int[] testValues = {0, 1, -1, 100, -100, Integer.MAX_VALUE, Integer.MIN_VALUE, 1234567, -1234567};
        for (int v : testValues) {
            java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
            java.io.DataOutputStream dos = new java.io.DataOutputStream(baos);
            it.denzosoft.jprolog.core.compiled.JpcWriter.writeSignedVarint(dos, v);
            dos.flush();
            java.io.DataInputStream dis = new java.io.DataInputStream(
                new java.io.ByteArrayInputStream(baos.toByteArray()));
            int back = it.denzosoft.jprolog.core.compiled.JpcReader.readSignedVarint(dis);
            assertEquals("zigzag roundtrip for " + v, v, back);
        }
    }

    // ===================================================================
    // CR-2025-0009 — Debug + profile predicates
    // ===================================================================

    @Test
    public void testCR009_debuggingZero() {
        // debugging/0 always succeeds
        List<Map<String, Term>> r = prolog.solve("debugging.");
        assertEquals(1, r.size());
    }

    @Test
    public void testCR009_spyingEnumerates() {
        prolog.solve("spy(foo/2).");
        prolog.solve("spy(bar/1).");
        List<Map<String, Term>> r = prolog.solve("spying(X).");
        // Should enumerate at least the 2 spy points
        org.junit.Assert.assertTrue("expected >=2 spy points, got " + r.size(), r.size() >= 2);
    }

    @Test
    public void testCR009_profile() {
        prolog.solve("reset_profile.");
        prolog.solve("profile.");
        prolog.consult("counter_test(1). counter_test(2). counter_test(3).");
        prolog.solve("counter_test(_).");
        prolog.solve("counter_test(_).");
        prolog.solve("noprofile.");
        List<Map<String, Term>> r = prolog.solve("profile_data(D).");
        assertEquals(1, r.size());
        String dataStr = r.get(0).get("D").toString();
        // Data should mention counter_test/1
        org.junit.Assert.assertTrue("profile_data must mention counter_test: " + dataStr,
            dataStr.contains("counter_test"));
    }

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
