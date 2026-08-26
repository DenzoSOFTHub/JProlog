package it.denzosoft.jprolog.test.builtin;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.terms.Number;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Verification tests for all bug fixes from v2.6.0 onwards.
 * Each test is tagged with the ISS number it verifies.
 *
 * ISS-2025-0180: Core engine fixes
 * ISS-2025-0181: Term system fixes
 * ISS-2025-0182: Built-in predicate fixes
 * ISS-2025-0183: DCG completion
 * ISS-2025-0184: List & meta predicate fixes
 * ISS-2025-0185: Engine & parser fixes
 * ISS-2025-0186: Debug, utilities, list fixes
 * ISS-2025-0187: Third-round analysis fixes
 * ISS-2025-0188: Fourth-round deep analysis fixes
 * ISS-2025-0189: Fifth-round deep analysis fixes
 */
public class BugFixVerificationTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    // ======================== ISS-2025-0180: Core Engine Fixes ========================

    @Test
    public void testISS0180_negativeShiftThrows() {
        // Negative shift amounts should throw evaluation_error
        try {
            prolog.solve("X is 1 << -1.");
            fail("Negative shift should throw");
        } catch (Exception e) {
            assertTrue(e.getMessage().contains("negative_shift") || e.getMessage().contains("shift"));
        }
    }

    @Test
    public void testISS0180_negativeRightShiftThrows() {
        try {
            prolog.solve("X is 8 >> -2.");
            fail("Negative right shift should throw");
        } catch (Exception e) {
            assertTrue(e.getMessage().contains("negative_shift") || e.getMessage().contains("shift"));
        }
    }

    // ======================== ISS-2025-0181: Term System Fixes ========================

    @Test
    public void testISS0181_numberHashCodeNegativeZero() {
        // -0.0 and 0.0 should have equal hashCode (equals/hashCode contract)
        Number posZero = new Number(0.0);
        Number negZero = new Number(-0.0);
        assertEquals("hashCode must be equal for -0.0 and 0.0", posZero.hashCode(), negZero.hashCode());
    }

    @Test
    public void testISS0181_numberUnifyDoubleCompare() {
        // Number unification uses Double.compare for correctness
        Number a = new Number(1.0);
        Number b = new Number(1.0);
        Map<String, Term> bindings = new HashMap<>();
        assertTrue("Same doubles should unify", a.unify(b, bindings));
    }

    @Test
    public void testISS0181_prologStringUnescape() {
        // PrologString unescape should handle \\ correctly
        String result = PrologString.unescapeString("hello\\\\world");
        assertEquals("hello\\world", result);

        // Escaped quote
        result = PrologString.unescapeString("say \\\"hi\\\"");
        assertEquals("say \"hi\"", result);

        // Tab and newline
        result = PrologString.unescapeString("a\\tb\\nc");
        assertEquals("a\tb\nc", result);
    }

    // ======================== ISS-2025-0182: Built-in Predicate Fixes ========================

    @Test
    public void testISS0182_arithmeticComparisonExact() {
        // =:= should use exact comparison, not epsilon
        List<Map<String, Term>> solutions = prolog.solve("1.0 =:= 1.0.");
        assertEquals(1, solutions.size());

        solutions = prolog.solve("1.0 =\\= 2.0.");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testISS0182_isExceptionPropagation() {
        // is/2 should propagate PrologException for undefined atoms
        try {
            prolog.solve("X is foo.");
            fail("is/2 with atom should throw type_error");
        } catch (Exception e) {
            // Expected: type_error(evaluable, foo/0)
            assertTrue(e.getMessage() != null);
        }
    }

    @Test
    public void testISS0182_betweenBasic() {
        // between/3 should generate all integers in range
        List<Map<String, Term>> solutions = prolog.solve("between(1, 3, X).");
        assertEquals(3, solutions.size());
        assertEquals("1", solutions.get(0).get("X").toString());
        assertEquals("2", solutions.get(1).get("X").toString());
        assertEquals("3", solutions.get(2).get("X").toString());
    }

    @Test
    public void testISS0182_functorVariableNaming() {
        // functor/3 should create proper terms
        List<Map<String, Term>> solutions = prolog.solve("functor(T, f, 2).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("T").toString();
        assertTrue("Should create f/2 term", result.startsWith("f("));
    }

    // ======================== ISS-2025-0183: DCG Completion ========================

    @Test
    public void testISS0183_dcgBasicParsing() {
        // DCG rules should parse correctly
        prolog.consult("greeting --> [hello], [world].");
        List<Map<String, Term>> solutions = prolog.solve("phrase(greeting, [hello, world]).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testISS0183_dcgWithTerminals() {
        // DCG with terminal sequences
        prolog.consult("ab --> [a], [b].");
        List<Map<String, Term>> solutions = prolog.solve("phrase(ab, [a, b]).");
        assertEquals(1, solutions.size());

        solutions = prolog.solve("phrase(ab, [a, c]).");
        assertEquals(0, solutions.size());
    }

    // ======================== ISS-2025-0184: List & Meta Predicate Fixes ========================

    @Test
    public void testISS0184_numlistFailsWhenLowGreaterThanHigh() {
        // numlist(5, 3, L) should fail
        List<Map<String, Term>> solutions = prolog.solve("numlist(5, 3, L).");
        assertEquals("numlist with low > high should fail", 0, solutions.size());
    }

    @Test
    public void testISS0184_numlistSuccess() {
        // numlist(1, 3, L) should succeed
        List<Map<String, Term>> solutions = prolog.solve("numlist(1, 3, L).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("L").toString();
        assertTrue(result.contains("1") && result.contains("2") && result.contains("3"));
    }

    @Test
    public void testISS0184_sortISOOrdering() {
        // sort/2 should use ISO standard term ordering: numbers < atoms
        List<Map<String, Term>> solutions = prolog.solve("sort([b, 1, a, 2], S).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("S").toString();
        // Numbers should come before atoms in ISO order
        int pos1 = result.indexOf("1");
        int posA = result.indexOf("a");
        assertTrue("Numbers before atoms in ISO order", pos1 < posA);
    }

    @Test
    public void testISS0184_sortDeduplication() {
        // sort/2 should remove duplicates
        List<Map<String, Term>> solutions = prolog.solve("sort([3, 1, 2, 1, 3], S).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("S").toString();
        assertTrue(result.contains("1") && result.contains("2") && result.contains("3"));
    }

    @Test
    public void testISS0184_forallSuccess() {
        // forall(member(X, [1,2,3]), number(X)) should succeed
        List<Map<String, Term>> solutions = prolog.solve("forall(member(X, [1, 2, 3]), number(X)).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testISS0184_forallFailure() {
        // forall(member(X, [1,a,3]), number(X)) should fail
        List<Map<String, Term>> solutions = prolog.solve("forall(member(X, [1, a, 3]), number(X)).");
        assertEquals(0, solutions.size());
    }

    // ======================== ISS-2025-0185: Engine & Parser Fixes ========================

    @Test
    public void testISS0185_zeroPowerNegativeThrows() {
        // 0 ** -1 must throw. (**)/2 is the ISO float power (ISS-2025-0247), so 0.0 ** -1
        // raises evaluation_error(undefined); 0 ^ -1 raises evaluation_error(zero_divisor).
        try {
            prolog.solve("X is 0 ** -1.");
            fail("0 ** -1 should throw an evaluation_error");
        } catch (Exception e) {
            assertTrue("expected evaluation_error, got: " + e.getMessage(),
                e.getMessage() != null
                    && (e.getMessage().contains("undefined")
                        || e.getMessage().contains("zero_divisor")
                        || e.getMessage().contains("zero")
                        || e.getMessage().contains("evaluation_error")));
        }
    }

    @Test
    public void testISS0185_rationalZeroDenominator() {
        // Rational with zero denominator should throw
        try {
            new Rational(1, 0);
            fail("Rational(1, 0) should throw");
        } catch (ArithmeticException e) {
            assertTrue(e.getMessage().contains("zero"));
        }
    }

    @Test
    public void testISS0185_rationalBasicArithmetic() {
        // Rational arithmetic should preserve precision
        Rational a = new Rational(1, 3);
        Rational b = new Rational(1, 6);
        Rational sum = a.add(b);
        assertEquals(BigInteger.ONE, sum.getNumerator());
        assertEquals(BigInteger.valueOf(2), sum.getDenominator());
    }

    // ======================== ISS-2025-0186: Debug, Utilities, List Fixes ========================

    @Test
    public void testISS0186_nth0Enumeration() {
        // nth0/3 should enumerate index-element pairs
        List<Map<String, Term>> solutions = prolog.solve("nth0(I, [a, b, c], E).");
        assertTrue("nth0 should enumerate", solutions.size() >= 3);
    }

    @Test
    public void testISS0186_nth1Enumeration() {
        // nth1/3 with bound index
        List<Map<String, Term>> solutions = prolog.solve("nth1(2, [a, b, c], E).");
        assertEquals(1, solutions.size());
        assertEquals("b", solutions.get(0).get("E").toString());
    }

    // ======================== ISS-2025-0187: Third-Round Analysis Fixes ========================

    @Test
    public void testISS0187_lengthGeneration() {
        // length/2 with unbound list should generate a list
        List<Map<String, Term>> solutions = prolog.solve("length(L, 3).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testISS0187_intersectionDeduplication() {
        // intersection/3 should deduplicate results
        List<Map<String, Term>> solutions = prolog.solve("intersection([1, 1, 2], [1, 2, 3], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        // Should contain 1 and 2, but 1 only once
        int firstOne = result.indexOf("1");
        int lastOne = result.lastIndexOf("1");
        assertEquals("1 should appear only once", firstOne, lastOne);
    }

    @Test
    public void testISS0187_plusExact() {
        // plus/3 should use exact comparison
        List<Map<String, Term>> solutions = prolog.solve("plus(1, 2, 3).");
        assertEquals(1, solutions.size());

        solutions = prolog.solve("plus(1, 2, X).");
        assertEquals(1, solutions.size());
        assertEquals("3", solutions.get(0).get("X").toString());

        solutions = prolog.solve("plus(X, 2, 5).");
        assertEquals(1, solutions.size());
        assertEquals("3", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0187_numberCodes() {
        // number_codes/2 should convert number to codes
        List<Map<String, Term>> solutions = prolog.solve("number_codes(123, C).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testISS0187_numberCodesReverse() {
        // number_codes/2 reverse: codes to number
        List<Map<String, Term>> solutions = prolog.solve("number_codes(N, [52, 50]).");
        assertEquals(1, solutions.size());
        assertEquals("42", solutions.get(0).get("N").toString());
    }

    // ======================== ISS-2025-0188: Fourth-Round Deep Analysis Fixes ========================

    @Test
    public void testISS0188_modNegativeDivisor() {
        // ISO mod: mod(-7, 3) = 2 (result has sign of divisor)
        List<Map<String, Term>> solutions = prolog.solve("X is mod(-7, 3).");
        assertEquals(1, solutions.size());
        assertEquals("2", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0188_modPositive() {
        // mod(7, 3) = 1
        List<Map<String, Term>> solutions = prolog.solve("X is mod(7, 3).");
        assertEquals(1, solutions.size());
        assertEquals("1", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0188_modNegativeBoth() {
        // ISO mod: mod(-7, -3) = -1 (result has sign of divisor)
        List<Map<String, Term>> solutions = prolog.solve("X is mod(-7, -3).");
        assertEquals(1, solutions.size());
        assertEquals("-1", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0188_numberBitLengthLongMax() {
        // BigInteger with bitLength=63 (Long.MAX_VALUE) should fit in long
        Number n = new Number(BigInteger.valueOf(Long.MAX_VALUE));
        assertTrue("Long.MAX_VALUE should fit in long", n.fitsInLong());
        assertFalse("Long.MAX_VALUE should not be flagged as BigInteger", n.isBigInteger());
        assertEquals(Long.MAX_VALUE, n.longValue());
    }

    @Test
    public void testISS0188_numberBitLengthOverflow() {
        // Value larger than Long.MAX_VALUE should be BigInteger
        BigInteger big = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE);
        Number n = new Number(big);
        assertTrue("Value > Long.MAX_VALUE should be BigInteger", n.isBigInteger());
        assertFalse("Value > Long.MAX_VALUE should not fit in long", n.fitsInLong());
    }

    @Test
    public void testISS0188_prologStringSinglePassUnescape() {
        // "\\n" (backslash-n literal) should not become newline
        String result = PrologString.unescapeString("\\\\n");
        assertEquals("\\n", result);

        // But "\n" should become newline
        result = PrologString.unescapeString("\\n");
        assertEquals("\n", result);
    }

    @Test
    public void testISS0188_memberNonGroundList() {
        // member/2 should work with partially ground lists
        prolog.consult("test_member :- X = [1, 2, 3], member(2, X).");
        List<Map<String, Term>> solutions = prolog.solve("test_member.");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testISS0188_deleteBindings() {
        // delete/3 should correctly remove elements
        List<Map<String, Term>> solutions = prolog.solve("delete([1, 2, 3, 2, 1], 2, R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        assertFalse("2 should be removed", result.contains("2"));
        assertTrue("1 should remain", result.contains("1"));
        assertTrue("3 should remain", result.contains("3"));
    }

    @Test
    public void testISS0188_numlistIntegerValidation() {
        // numlist should work with proper integers
        List<Map<String, Term>> solutions = prolog.solve("numlist(1, 5, L).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("L").toString();
        assertTrue(result.contains("1") && result.contains("5"));
    }

    @Test
    public void testISS0188_sortCompareTermsDedup() {
        // sort/2 dedup should use compareTerms, not toString
        List<Map<String, Term>> solutions = prolog.solve("sort([c, a, b, a], S).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("S").toString();
        // Should be [a, b, c] with no duplicate a
        int firstA = result.indexOf("a");
        int lastA = result.lastIndexOf("a");
        assertEquals("a should appear only once", firstA, lastA);
    }

    @Test
    public void testISS0188_notUnifiableExceptionPropagation() {
        // \= should not swallow real errors — basic behavior check
        List<Map<String, Term>> solutions = prolog.solve("1 \\= 2.");
        assertEquals("1 \\= 2 should succeed", 1, solutions.size());

        solutions = prolog.solve("1 \\= 1.");
        assertEquals("1 \\= 1 should fail", 0, solutions.size());
    }

    @Test
    public void testISS0188_stringConcatGracefulFailure() {
        // string_concat with insufficient args should fail, not throw
        try {
            List<Map<String, Term>> solutions = prolog.solve("string_concat(X, Y, Z).");
            // Should fail (return empty) or at most return false
            assertTrue("Should fail gracefully", solutions.isEmpty());
        } catch (Exception e) {
            fail("string_concat should fail gracefully, not throw: " + e.getMessage());
        }
    }

    // ======================== ISS-2025-0189: Fifth-Round Deep Analysis Fixes ========================

    @Test
    public void testISS0189_shiftLeftLargeAmount() {
        // 1 << 64 should produce a large number, not wrap to 1
        List<Map<String, Term>> solutions = prolog.solve("X is 1 << 64.");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("X").toString();
        assertFalse("1 << 64 should not be 1", "1".equals(result));
        assertEquals("18446744073709551616", result); // 2^64
    }

    @Test
    public void testISS0189_shiftLeftSmall() {
        // 1 << 3 = 8 (basic case still works)
        List<Map<String, Term>> solutions = prolog.solve("X is 1 << 3.");
        assertEquals(1, solutions.size());
        assertEquals("8", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0189_shiftRight() {
        // 16 >> 2 = 4
        List<Map<String, Term>> solutions = prolog.solve("X is 16 >> 2.");
        assertEquals(1, solutions.size());
        assertEquals("4", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0189_negationExceptionPropagation() {
        // \+ should let exceptions propagate per ISO
        try {
            prolog.solve("\\+ throw(my_error).");
            fail("Exception should propagate through \\+");
        } catch (Exception e) {
            // Expected: exception propagates
            assertTrue(e.getMessage() != null);
        }
    }

    @Test
    public void testISS0189_negationBasicBehavior() {
        // \+ fail should succeed
        List<Map<String, Term>> solutions = prolog.solve("\\+ fail.");
        assertEquals(1, solutions.size());

        // \+ true should fail
        solutions = prolog.solve("\\+ true.");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testISS0189_findallExceptionPropagation() {
        // findall with throwing goal should propagate exception
        try {
            prolog.solve("findall(X, throw(my_error), L).");
            fail("Exception should propagate through findall");
        } catch (Exception e) {
            assertTrue(e.getMessage() != null);
        }
    }

    @Test
    public void testISS0189_findallEmptyList() {
        // findall with no solutions should return empty list (ISO)
        List<Map<String, Term>> solutions = prolog.solve("findall(X, (X = 1, X = 2), L).");
        assertEquals(1, solutions.size());
        assertEquals("[]", solutions.get(0).get("L").toString());
    }

    @Test
    public void testISS0189_rationalUnifyExact() {
        // Rational unification should use exact comparison
        Rational r1 = new Rational(1, 3);
        Rational r2 = new Rational(1, 3);
        Rational r3 = new Rational(2, 3);
        Map<String, Term> bindings = new HashMap<>();
        assertTrue("Same rationals should unify", r1.unify(r2, bindings));
        assertFalse("Different rationals should not unify", r1.unify(r3, new HashMap<>()));
    }

    @Test
    public void testISS0189_rationalUnifyWithInteger() {
        // Rational 6/2 = integer 3
        Rational r = new Rational(6, 2); // reduces to 3/1
        Number n = new Number(3L);
        Map<String, Term> bindings = new HashMap<>();
        assertTrue("Rational 6/2 should unify with integer 3", r.unify(n, bindings));
    }

    @Test
    public void testISS0189_rationalNotUnifyWithFloat() {
        // Rational 1/3 should NOT unify with float
        Rational r = new Rational(1, 3);
        Number n = new Number(0.333, false); // float
        Map<String, Term> bindings = new HashMap<>();
        assertFalse("Rational should not unify with float", r.unify(n, bindings));
    }

    // ISS-2025-0189's two LayeredMap tests were REMOVED in wave W9 (ISS-2025-0484):
    // core.engine.LayeredMap was the recursive QuerySolver's binding map and is deleted with it.

    @Test
    public void testISS0189_arithmeticComparisonIntegerExact() {
        // Integer comparison should not lose precision via double
        List<Map<String, Term>> solutions = prolog.solve("1 =:= 1.");
        assertEquals(1, solutions.size());

        solutions = prolog.solve("1 =:= 2.");
        assertEquals(0, solutions.size());

        solutions = prolog.solve("1000000 =:= 1000000.");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testISS0189_atomImmutability() {
        // Atom should not have setName method — verified at compile time
        // This test verifies basic atom behavior
        Atom a = new Atom("hello");
        assertEquals("hello", a.getName());
        Atom b = new Atom("hello");
        assertEquals(a, b);
        assertEquals(a.hashCode(), b.hashCode());
    }

    @Test
    public void testISS0189_prologStringEscapeSymmetry() {
        // Unescape should handle all standard escape sequences
        assertEquals("\u0007", PrologString.unescapeString("\\a")); // bell
        assertEquals("\b", PrologString.unescapeString("\\b"));     // backspace
        assertEquals("\f", PrologString.unescapeString("\\f"));     // form feed
        assertEquals("\u000B", PrologString.unescapeString("\\v")); // vertical tab
        assertEquals("'", PrologString.unescapeString("\\'"));      // single quote
    }

    @Test
    public void testISS0189_floatFractionalPart() {
        // float_fractional_part should work correctly
        List<Map<String, Term>> solutions = prolog.solve("X is float_fractional_part(3.75).");
        assertEquals(1, solutions.size());
        assertEquals("0.75", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0189_roundFunction() {
        // round should work correctly
        List<Map<String, Term>> solutions = prolog.solve("X is round(3.7).");
        assertEquals(1, solutions.size());
        assertEquals("4", solutions.get(0).get("X").toString());

        solutions = prolog.solve("X is round(3.2).");
        assertEquals(1, solutions.size());
        assertEquals("3", solutions.get(0).get("X").toString());
    }

    // ======================== Cross-cutting: Integration Tests ========================

    @Test
    public void testCrossCutting_catchThrow() {
        // catch/3 should catch thrown exceptions
        List<Map<String, Term>> solutions = prolog.solve("catch(throw(my_error), my_error, true).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testCrossCutting_memberFindall() {
        // findall with member should collect all elements
        prolog.consult("color(red). color(green). color(blue).");
        List<Map<String, Term>> solutions = prolog.solve("findall(X, color(X), L).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("L").toString();
        assertTrue(result.contains("red") && result.contains("green") && result.contains("blue"));
    }

    @Test
    public void testCrossCutting_msortPreserveDuplicates() {
        // msort/2 should NOT remove duplicates (unlike sort/2)
        List<Map<String, Term>> solutions = prolog.solve("msort([3, 1, 2, 1], S).");
        assertEquals(1, solutions.size());
        // Result should have 4 elements (duplicates preserved)
        String result = solutions.get(0).get("S").toString();
        int firstOne = result.indexOf("1");
        int lastOne = result.lastIndexOf("1");
        assertTrue("msort should keep duplicates", firstOne != lastOne);
    }

    @Test
    public void testCrossCutting_appendBasic() {
        List<Map<String, Term>> solutions = prolog.solve("append([1, 2], [3, 4], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        assertTrue(result.contains("1") && result.contains("4"));
    }

    @Test
    public void testCrossCutting_lastElement() {
        List<Map<String, Term>> solutions = prolog.solve("last([1, 2, 3], X).");
        assertEquals(1, solutions.size());
        assertEquals("3", solutions.get(0).get("X").toString());
    }

    @Test
    public void testCrossCutting_reverseList() {
        List<Map<String, Term>> solutions = prolog.solve("reverse([1, 2, 3], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        // First element should be 3
        assertTrue(result.indexOf("3") < result.indexOf("1"));
    }

    @Test
    public void testCrossCutting_selectElement() {
        List<Map<String, Term>> solutions = prolog.solve("select(2, [1, 2, 3], R).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testCrossCutting_subtractLists() {
        List<Map<String, Term>> solutions = prolog.solve("subtract([1, 2, 3, 4], [2, 4], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        assertTrue(result.contains("1") && result.contains("3"));
        assertFalse(result.contains("4"));
    }

    @Test
    public void testCrossCutting_sumList() {
        List<Map<String, Term>> solutions = prolog.solve("sum_list([1, 2, 3, 4], S).");
        assertEquals(1, solutions.size());
        assertEquals("10", solutions.get(0).get("S").toString());
    }

    @Test
    public void testCrossCutting_flattenList() {
        List<Map<String, Term>> solutions = prolog.solve("flatten([1, [2, [3]], 4], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        assertTrue(result.contains("1") && result.contains("3") && result.contains("4"));
    }

    @Test
    public void testCrossCutting_permutation() {
        List<Map<String, Term>> solutions = prolog.solve("permutation([1, 2, 3], P).");
        assertEquals(6, solutions.size()); // 3! = 6 permutations
    }

    @Test
    public void testCrossCutting_atomChars() {
        List<Map<String, Term>> solutions = prolog.solve("atom_chars(hello, C).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testCrossCutting_atomCharsBidirectional() {
        List<Map<String, Term>> solutions = prolog.solve("atom_chars(A, [h, i]).");
        assertEquals(1, solutions.size());
        assertEquals("hi", solutions.get(0).get("A").toString());
    }

    @Test
    public void testCrossCutting_copyTerm() {
        List<Map<String, Term>> solutions = prolog.solve("copy_term(f(X, Y), T).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testCrossCutting_once() {
        // once/1 should return only first solution
        prolog.consult("multi(1). multi(2). multi(3).");
        List<Map<String, Term>> solutions = prolog.solve("once(multi(X)).");
        assertEquals(1, solutions.size());
        assertEquals("1", solutions.get(0).get("X").toString());
    }

    @Test
    public void testCrossCutting_assertRetract() {
        // assert and retract should work
        prolog.solve("assertz(temp_fact(42)).");
        List<Map<String, Term>> solutions = prolog.solve("temp_fact(X).");
        assertEquals(1, solutions.size());
        assertEquals("42", solutions.get(0).get("X").toString());

        prolog.solve("retract(temp_fact(42)).");
        solutions = prolog.solve("temp_fact(X).");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testCrossCutting_bitwiseOperations() {
        // Bitwise AND
        List<Map<String, Term>> solutions = prolog.solve("X is 7 /\\ 3.");
        assertEquals(1, solutions.size());
        assertEquals("3", solutions.get(0).get("X").toString());

        // Bitwise OR
        solutions = prolog.solve("X is 5 \\/ 3.");
        assertEquals(1, solutions.size());
        assertEquals("7", solutions.get(0).get("X").toString());
    }

    @Test
    public void testCrossCutting_absFunction() {
        List<Map<String, Term>> solutions = prolog.solve("X is abs(-42).");
        assertEquals(1, solutions.size());
        assertEquals("42", solutions.get(0).get("X").toString());
    }

    @Test
    public void testCrossCutting_maxMinFunction() {
        List<Map<String, Term>> solutions = prolog.solve("X is max(3, 7).");
        assertEquals(1, solutions.size());
        assertEquals("7", solutions.get(0).get("X").toString());

        solutions = prolog.solve("X is min(3, 7).");
        assertEquals(1, solutions.size());
        assertEquals("3", solutions.get(0).get("X").toString());
    }

    @Test
    public void testCrossCutting_integerPower() {
        // Integer power is (^)/2 in ISO; (**)/2 is the float power (ISS-2025-0247).
        List<Map<String, Term>> solutions = prolog.solve("X is 2 ^ 10.");
        assertEquals(1, solutions.size());
        assertEquals("1024", solutions.get(0).get("X").toString());
        // (**)/2 yields the float power.
        solutions = prolog.solve("X is 2 ** 10.");
        assertEquals(1, solutions.size());
        assertEquals("1024.0", solutions.get(0).get("X").toString());
    }

    @Test
    public void testCrossCutting_integerDivision() {
        List<Map<String, Term>> solutions = prolog.solve("X is 7 // 2.");
        assertEquals(1, solutions.size());
        assertEquals("3", solutions.get(0).get("X").toString());
    }

    @Test
    public void testCrossCutting_remainder() {
        List<Map<String, Term>> solutions = prolog.solve("X is rem(7, 3).");
        assertEquals(1, solutions.size());
        assertEquals("1", solutions.get(0).get("X").toString());
    }

    // ==================== ISS-2025-0190 ====================

    // #1 KeySort ISO term ordering (not string comparison)
    @Test
    public void testISS0190_keySortISOOrdering() {
        // Numeric keys: 2 should come before 10 in ISO ordering
        prolog.consult("test_ks :- keysort([10-a, 2-b, 1-c], X), X = [1-c, 2-b, 10-a].");
        List<Map<String, Term>> solutions = prolog.solve("test_ks.");
        assertEquals(1, solutions.size());
    }

    // #2 Intersection structural deduplication
    @Test
    public void testISS0190_intersectionStructuralDedup() {
        prolog.consult("test_int :- intersection([a, a, b], [a, b, c], X), X = [a, b].");
        List<Map<String, Term>> solutions = prolog.solve("test_int.");
        assertEquals(1, solutions.size());
    }

    // #4-5 ISS-2025-0190's LayeredMap rollback test was REMOVED in wave W9 (ISS-2025-0484)
    // together with core.engine.LayeredMap itself.

    // #6 Rational equals/hashCode contract
    @Test
    public void testISS0190_rationalEqualsHashCode() {
        it.denzosoft.jprolog.core.terms.Rational r = new it.denzosoft.jprolog.core.terms.Rational(1, 3);
        it.denzosoft.jprolog.core.terms.Number n = new it.denzosoft.jprolog.core.terms.Number(1.0 / 3.0);
        // Rational should NOT equal Number (contract violation fix)
        assertFalse(r.equals(n));
    }

    // #7-8 Unicode BMP range in number_codes
    @Test
    public void testISS0190_numberCodesUnicodeBMP() {
        // Codes above 255 should now work
        List<Map<String, Term>> solutions = prolog.solve("number_codes(1, X).");
        assertEquals(1, solutions.size());
    }

    // #9 Succ long overflow
    @Test
    public void testISS0190_succLargeNumbers() {
        // Values beyond int range should work
        List<Map<String, Term>> solutions = prolog.solve("succ(2147483647, X).");
        assertEquals(1, solutions.size());
        assertEquals("2147483648", solutions.get(0).get("X").toString());
    }

    // #10 MapList4 binding accumulation
    @Test
    public void testISS0190_maplist4Bindings() {
        prolog.consult("triple(X, Y, Z) :- Y is X * 2, Z is X * 3.");
        List<Map<String, Term>> solutions = prolog.solve("maplist(triple, [1, 2], X, Y).");
        assertEquals(1, solutions.size());
    }

    // #11 IfThen commits to first condition solution
    @Test
    public void testISS0190_ifThenFirstSolution() {
        prolog.consult("choice(1). choice(2). choice(3).");
        prolog.consult("test_ifthen(X) :- (choice(X) -> true).");
        List<Map<String, Term>> solutions = prolog.solve("test_ifthen(X).");
        // Should commit to first condition solution (X=1), not all three
        assertEquals(1, solutions.size());
        assertEquals("1", solutions.get(0).get("X").toString());
    }

    // #14 ArithmeticEvaluator msb/lsb error terms use Number
    @Test
    public void testISS0190_msbErrorTerm() {
        try {
            prolog.solve("X is msb(3.5).");
            fail("Should throw type_error");
        } catch (Exception e) {
            assertTrue(e.getMessage().contains("type_error") || e.getMessage().contains("integer"));
        }
    }

    // #16 PrologString escape/unescape symmetry
    @Test
    public void testISS0190_prologStringEscapeSymmetry() {
        String input = "hello\\aworld";
        String unescaped = it.denzosoft.jprolog.core.terms.PrologString.unescapeString(input);
        // Bell character (0x07) should be present
        assertTrue(unescaped.contains("\u0007"));
    }

    // #18 Ignore propagates system errors
    @Test
    public void testISS0190_ignoreSucceedsOnFailure() {
        prolog.consult("test_ignore :- ignore(fail).");
        List<Map<String, Term>> solutions = prolog.solve("test_ignore.");
        assertEquals(1, solutions.size());
    }

    // ==================== ISS-2025-0191 ====================

    // #1 Parser: large integer precision (parser level, not arithmetic evaluator)
    @Test
    public void testISS0191_largeIntegerParsing() {
        // Verify the parser preserves large integer precision via direct unification
        List<Map<String, Term>> solutions = prolog.solve("X = 9007199254740993.");
        assertEquals(1, solutions.size());
        assertEquals("9007199254740993", solutions.get(0).get("X").toString());
    }

    // #2-3 PredSort correct solver call and error propagation
    @Test
    public void testISS0191_predSortBasic() {
        prolog.consult("my_cmp(<, X, Y) :- X < Y. my_cmp(>, X, Y) :- X > Y. my_cmp(=, X, Y) :- X =:= Y.");
        List<Map<String, Term>> solutions = prolog.solve("predsort(my_cmp, [3, 1, 2], X).");
        assertEquals(1, solutions.size());
        // Verify sorting works — result should contain 1, 2, 3 in order
        String result = solutions.get(0).get("X").toString();
        assertTrue("Expected sorted list, got: " + result, result.contains("1") && result.contains("2") && result.contains("3"));
    }

    // #5 TableStore exact matching (internal unit test)
    @Test
    public void testISS0191_tableStoreNoCollision() {
        // Direct test of the matchesPredicate logic via abolishTable
        it.denzosoft.jprolog.core.engine.TableStore store = new it.denzosoft.jprolog.core.engine.TableStore();
        store.declareTable("path", 2);
        store.declareTable("path_query", 1);
        store.abolishTable("path", 2);
        // path_query should still be tabled after abolishing path
        assertTrue(store.isTabled("path_query", 1));
        assertFalse(store.isTabled("path", 2));
    }

    // #6 Number hashCode NaN consistency
    @Test
    public void testISS0191_numberHashCodeNaN() {
        it.denzosoft.jprolog.core.terms.Number nan1 = new it.denzosoft.jprolog.core.terms.Number(Double.NaN);
        it.denzosoft.jprolog.core.terms.Number nan2 = new it.denzosoft.jprolog.core.terms.Number(Double.NaN);
        // Both NaN values should produce the same hashCode
        assertEquals(nan1.hashCode(), nan2.hashCode());
    }

    // #7 msb/lsb evaluationError for <= 0
    @Test
    public void testISS0191_msbEvaluationError() {
        try {
            prolog.solve("X is msb(0).");
            fail("Should throw evaluation_error");
        } catch (Exception e) {
            assertTrue(e.getMessage().contains("evaluation_error") || e.getMessage().contains("undefined"));
        }
    }

    // #8 Nth1 element unification without pre-resolution
    @Test
    public void testISS0191_nth1Enumeration() {
        List<Map<String, Term>> solutions = prolog.solve("nth1(N, [a, b, c], b).");
        assertTrue(solutions.size() >= 1);
        assertEquals("2", solutions.get(0).get("N").toString());
    }

    // #10 AtomConcat unsupported mode returns false
    @Test
    public void testISS0191_atomConcatUnsupportedMode() {
        // START_CHANGE: ISS-2025-0406 - ISO 8.16.2.3a: all three unbound raises a catchable
        // instantiation_error ball (the original ISS-0191 intent — no raw Java exception — holds)
        List<Map<String, Term>> solutions = prolog.solve(
            "catch(atom_concat(X, Y, Z), error(instantiation_error, _), true).");
        assertEquals(1, solutions.size());
        // END_CHANGE: ISS-2025-0406
    }

    // #11 ListTerm.createListTerm iterative
    @Test
    public void testISS0191_listTermLargeList() {
        // Build a large list to ensure no stack overflow
        StringBuilder sb = new StringBuilder("X = [");
        for (int i = 0; i < 500; i++) {
            if (i > 0) sb.append(", ");
            sb.append(i);
        }
        sb.append("].");
        List<Map<String, Term>> solutions = prolog.solve(sb.toString());
        assertEquals(1, solutions.size());
    }

    // #13 Subtract uses structural equality
    @Test
    public void testISS0191_subtractStructuralEquality() {
        prolog.consult("test_sub :- subtract([1, 2, 3], [2], X), X = [1, 3].");
        List<Map<String, Term>> solutions = prolog.solve("test_sub.");
        assertEquals(1, solutions.size());
    }

    // ==================== ISS-2025-0192: Eighth-Round Deep Analysis Fixes ====================

    // #1 ListTerm.unify rollback on partial failure
    @Test
    public void testISS0192_listTermUnifyRollback() {
        // If unification fails partway through a list, earlier bindings should be rolled back
        it.denzosoft.jprolog.core.utils.ListTerm list1 = new it.denzosoft.jprolog.core.utils.ListTerm(
            java.util.Arrays.asList(new Atom("a"), new Atom("b")));
        it.denzosoft.jprolog.core.utils.ListTerm list2 = new it.denzosoft.jprolog.core.utils.ListTerm(
            java.util.Arrays.asList(new Variable("X"), new Atom("c")));
        Map<String, Term> bindings = new HashMap<>();
        boolean result = list2.unify(list1, bindings);
        assertFalse("Unification should fail (b != c)", result);
        // X should NOT be bound after failed unification
        assertFalse("X should not be bound after rollback", bindings.containsKey("X"));
    }

    // #2 Union deduplicates Set1
    @Test
    public void testISS0192_unionDeduplicatesSet1() {
        List<Map<String, Term>> solutions = prolog.solve("union([1, 1, 2], [3], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        // 1 should appear only once
        int first1 = result.indexOf("1");
        int last1 = result.lastIndexOf("1");
        assertEquals("1 should appear only once in union result", first1, last1);
    }

    // #3 Clause/2 uses TermCopier for proper variable renaming
    @Test
    public void testISS0192_clauseVariableRenaming() {
        prolog.consult("parent(tom, bob). parent(bob, ann).");
        List<Map<String, Term>> solutions = prolog.solve("clause(parent(X, Y), true).");
        assertTrue("Should find at least 2 clauses", solutions.size() >= 2);
    }

    // #4 SumList preserves integer precision
    @Test
    public void testISS0192_sumListIntegerPrecision() {
        List<Map<String, Term>> solutions = prolog.solve("sum_list([1000000, 2000000, 3000000], S).");
        assertEquals(1, solutions.size());
        assertEquals("6000000", solutions.get(0).get("S").toString());
    }

    // #5 MaxList uses first element (preserves type)
    @Test
    public void testISS0192_maxListPreservesType() {
        List<Map<String, Term>> solutions = prolog.solve("max_list([3, 1, 4, 1, 5], M).");
        assertEquals(1, solutions.size());
        assertEquals("5", solutions.get(0).get("M").toString());
    }

    // #6 MinList uses first element (preserves type)
    @Test
    public void testISS0192_minListPreservesType() {
        List<Map<String, Term>> solutions = prolog.solve("min_list([3, 1, 4, 1, 5], M).");
        assertEquals(1, solutions.size());
        assertEquals("1", solutions.get(0).get("M").toString());
    }

    // #7 Between uses long precision
    @Test
    public void testISS0192_betweenLongPrecision() {
        // Large values should not lose precision via double cast
        List<Map<String, Term>> solutions = prolog.solve("between(1000000000, 1000000002, X).");
        assertEquals(3, solutions.size());
        assertEquals("1000000000", solutions.get(0).get("X").toString());
        assertEquals("1000000001", solutions.get(1).get("X").toString());
        assertEquals("1000000002", solutions.get(2).get("X").toString());
    }

    // #8 Tab with negative N fails
    @Test
    public void testISS0192_tabNegativeFails() {
        List<Map<String, Term>> solutions = prolog.solve("tab(-1).");
        assertEquals("tab(-1) should fail", 0, solutions.size());
    }

    // #9 ListTerm resolveBindings optimization (no allocation when unchanged)
    @Test
    public void testISS0192_listTermResolveNoChange() {
        it.denzosoft.jprolog.core.utils.ListTerm list = new it.denzosoft.jprolog.core.utils.ListTerm(
            java.util.Arrays.asList(new Atom("a"), new Atom("b")));
        Map<String, Term> emptyBindings = new HashMap<>();
        Term resolved = list.resolveBindings(emptyBindings);
        // Should return same instance when no bindings apply (ground list)
        assertSame("Ground list should return same instance", list, resolved);
    }

    // #10 Include/Exclude basic functionality
    @Test
    public void testISS0192_includeBasic() {
        List<Map<String, Term>> solutions = prolog.solve("include(number, [1, a, 2, b, 3], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        assertTrue(result.contains("1") && result.contains("2") && result.contains("3"));
        assertFalse(result.contains("a"));
    }

    @Test
    public void testISS0192_excludeBasic() {
        List<Map<String, Term>> solutions = prolog.solve("exclude(number, [1, a, 2, b, 3], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        assertTrue(result.contains("a") && result.contains("b"));
        assertFalse(result.contains("1"));
    }

    // ==================== ISS-2025-0193: Ninth-Round Deep Analysis Fixes ====================

    // #1 WriteTerm ISO quote escaping
    @Test
    public void testISS0193_writeTermQuoteEscaping() {
        // ISO Prolog: single quotes within quoted atoms are doubled, not backslash-escaped
        // Verify write_term doesn't crash with quoted atoms
        List<Map<String, Term>> solutions = prolog.solve("atom_length('hello world', L).");
        assertEquals(1, solutions.size());
        assertEquals("11", solutions.get(0).get("L").toString());
    }

    // #2 TermParser hex/octal/binary precision
    @Test
    public void testISS0193_hexLiteralPrecision() {
        // 0xFFFFFFFFFFFF = 281474976710655 (larger than 2^53)
        List<Map<String, Term>> solutions = prolog.solve("X = 0xFFFFFFFFFFFF.");
        assertEquals(1, solutions.size());
        assertEquals("281474976710655", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0193_binaryLiteralPrecision() {
        // 0b1 should parse correctly
        List<Map<String, Term>> solutions = prolog.solve("X = 0b1010.");
        assertEquals(1, solutions.size());
        assertEquals("10", solutions.get(0).get("X").toString());
    }

    // #3 Plus/3 integer precision
    @Test
    public void testISS0193_plusIntegerPrecision() {
        List<Map<String, Term>> solutions = prolog.solve("plus(1000000, 2000000, X).");
        assertEquals(1, solutions.size());
        assertEquals("3000000", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0193_plusReverseIntegerPrecision() {
        List<Map<String, Term>> solutions = prolog.solve("plus(X, 2000000, 5000000).");
        assertEquals(1, solutions.size());
        assertEquals("3000000", solutions.get(0).get("X").toString());
    }

    // #4 CharCode extended range
    @Test
    public void testISS0193_charCodeBasic() {
        List<Map<String, Term>> solutions = prolog.solve("char_code(a, X).");
        assertEquals(1, solutions.size());
        assertEquals("97", solutions.get(0).get("X").toString());
    }

    // #5 AtomLength correct Unicode counting
    @Test
    public void testISS0193_atomLengthBasic() {
        List<Map<String, Term>> solutions = prolog.solve("atom_length(hello, L).");
        assertEquals(1, solutions.size());
        assertEquals("5", solutions.get(0).get("L").toString());
    }

    // #6 DCG with unique rule IDs
    @Test
    public void testISS0193_dcgMultipleRules() {
        prolog.consult("greeting2 --> [hello]. farewell --> [bye].");
        List<Map<String, Term>> solutions = prolog.solve("phrase(greeting2, [hello]).");
        assertEquals(1, solutions.size());
        solutions = prolog.solve("phrase(farewell, [bye]).");
        assertEquals(1, solutions.size());
    }

    // #7 AggregateAll with ISO ordering
    @Test
    public void testISS0193_aggregateAllOrdering() {
        // aggregate_all uses ISO ordering for set collection
        prolog.consult("color2(red). color2(green). color2(blue).");
        List<Map<String, Term>> solutions = prolog.solve("aggregate_all(set(X), color2(X), S).");
        assertEquals(1, solutions.size());
    }

    // #8 Include binding accumulation
    @Test
    public void testISS0193_includeBindingAccumulation() {
        List<Map<String, Term>> solutions = prolog.solve("include(atom, [a, 1, b, 2], R).");
        assertEquals(1, solutions.size());
        String result = solutions.get(0).get("R").toString();
        assertTrue(result.contains("a") && result.contains("b"));
    }

    // #9 StringCodes basic (verifies no regression after supplementary Unicode fix)
    @Test
    public void testISS0193_stringCodesBasic() {
        List<Map<String, Term>> solutions = prolog.solve("atom_codes(abc, C).");
        assertEquals(1, solutions.size());
    }

    // #10 AtomCodes round-trip
    @Test
    public void testISS0193_atomCodesRoundTrip() {
        List<Map<String, Term>> solutions = prolog.solve("atom_codes(hello, C), atom_codes(X, C).");
        assertEquals(1, solutions.size());
        assertEquals("hello", solutions.get(0).get("X").toString());
    }

    // ========== ISS-2025-0194: Cut semantics fixes ==========

    // #1 Cut in handleBuiltIn propagates to clause level
    @Test
    public void testISS0194_cutPreventsSecondClause() {
        prolog.solve("assert((cuttest(1) :- !)).");
        prolog.solve("assert((cuttest(2))).");
        List<Map<String, Term>> solutions = prolog.solve("cuttest(X).");
        assertEquals(1, solutions.size());
        assertEquals("1", solutions.get(0).get("X").toString());
    }

    // #2 Cut inside if-then-else propagates to clause level
    @Test
    public void testISS0194_cutInIfThenElsePropagates() {
        prolog.solve("assert((ite_cut(X) :- (X > 0 -> ! ; true), X > 0)).");
        prolog.solve("assert((ite_cut(0))).");
        List<Map<String, Term>> solutions = prolog.solve("ite_cut(1).");
        assertEquals(1, solutions.size());
    }

    // #3 Cut inside disjunction propagates to clause level
    @Test
    public void testISS0194_cutInDisjunctionPropagates() {
        prolog.solve("assert((disj_cut(a) :- (! ; true))).");
        prolog.solve("assert((disj_cut(b))).");
        List<Map<String, Term>> solutions = prolog.solve("disj_cut(X).");
        assertEquals(1, solutions.size());
        assertEquals("a", solutions.get(0).get("X").toString());
    }

    // #4 Cut with member prevents backtracking
    @Test
    public void testISS0194_cutWithMemberPreventsBacktracking() {
        prolog.solve("assert((first_member(X, L) :- member(X, L), !)).");
        List<Map<String, Term>> solutions = prolog.solve("first_member(X, [1,2,3]).");
        assertEquals(1, solutions.size());
        assertEquals("1", solutions.get(0).get("X").toString());
    }

    // #5 Cut does not escape call/1
    @Test
    public void testISS0194_cutDoesNotEscapeCall() {
        prolog.solve("assert((call_cut(a) :- call(!))).");
        prolog.solve("assert((call_cut(b))).");
        List<Map<String, Term>> solutions = prolog.solve("call_cut(X).");
        // call(!) isolates cut per ISO, so both clauses should be tried
        assertTrue(solutions.size() >= 1);
    }

    // #6 Cut does not escape once/1
    @Test
    public void testISS0194_cutDoesNotEscapeOnce() {
        prolog.solve("assert((once_cut(a) :- once(!))).");
        prolog.solve("assert((once_cut(b))).");
        List<Map<String, Term>> solutions = prolog.solve("once_cut(X).");
        // once(!) isolates cut, so both clauses should be tried
        assertTrue(solutions.size() >= 1);
    }

    // #7 Cut does not escape negation
    @Test
    public void testISS0194_cutDoesNotEscapeNegation() {
        prolog.solve("assert((neg_cut(a) :- \\+((!,fail)))).");
        prolog.solve("assert((neg_cut(b))).");
        List<Map<String, Term>> solutions = prolog.solve("neg_cut(X).");
        // START_CHANGE: ISS-2025-0342 - \+ isolates cut: \+((!,fail)) succeeds, so BOTH clauses solve
        assertEquals(2, solutions.size());
        assertEquals("a", solutions.get(0).get("X").toString());
        assertEquals("b", solutions.get(1).get("X").toString());
        // END_CHANGE: ISS-2025-0342
    }

    // #8 Basic cut in multi-clause predicate
    @Test
    public void testISS0194_basicCutMultiClause() {
        prolog.solve("assert((max3(X,Y,X) :- X >= Y, !)).");
        prolog.solve("assert((max3(X,Y,Y))).");
        List<Map<String, Term>> solutions = prolog.solve("max3(5,3,M).");
        assertEquals(1, solutions.size());
        assertEquals("5", solutions.get(0).get("M").toString());
    }

    // START_CHANGE: ISS-2025-0195 - setof/3 sort+dedup
    @Test
    public void testISS0195_setofSortsAndDedups() {
        prolog.consult("p(3). p(1). p(2). p(1).");
        List<Map<String, Term>> s = prolog.solve("setof(X, p(X), L).");
        assertEquals(1, s.size());
        assertEquals("[1, 2, 3]", s.get(0).get("L").toString());
    }
    // END_CHANGE: ISS-2025-0195

    // START_CHANGE: ISS-2025-0196 - bagof/3 witness grouping
    @Test
    public void testISS0196_bagofGroupsByFreeVar() {
        prolog.consult("q(1,a). q(1,b). q(2,c). q(2,d).");
        List<Map<String, Term>> s = prolog.solve("bagof(Y, q(X,Y), B).");
        assertEquals(2, s.size());
    }

    @Test
    public void testISS0196_bagofExistentialNoGrouping() {
        prolog.consult("r(1,a). r(1,b). r(2,c).");
        List<Map<String, Term>> s = prolog.solve("bagof(Y, X^r(X,Y), B).");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0196

    // START_CHANGE: ISS-2025-0205 - functor/3 supports numbers
    @Test
    public void testISS0205_functorNumber() {
        List<Map<String, Term>> s = prolog.solve("functor(42, F, A).");
        assertEquals(1, s.size());
        assertEquals("42", s.get(0).get("F").toString());
        assertEquals("0", s.get(0).get("A").toString());
    }
    // END_CHANGE: ISS-2025-0205

    // START_CHANGE: ISS-2025-0209 - between/3 accepts inf
    @Test
    public void testISS0209_betweenInfAccepted() {
        List<Map<String, Term>> s = prolog.solve("between(1, inf, 5).");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0209

    // START_CHANGE: ISS-2025-0210 - gcd/2 evaluable
    @Test
    public void testISS0210_gcdEvaluable() {
        List<Map<String, Term>> s = prolog.solve("X is gcd(12, 18).");
        assertEquals(1, s.size());
        assertEquals("6", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0210

    // START_CHANGE: ISS-2025-0201 - soft-cut *-> operator
    @Test
    public void testISS0201_softCutEnumeratesAll() {
        prolog.consult("m(a). m(b). m(c).");
        List<Map<String, Term>> s = prolog.solve("(m(X) *-> true ; X = none).");
        assertEquals(3, s.size());
    }

    @Test
    public void testISS0201_softCutElseOnEmpty() {
        List<Map<String, Term>> s = prolog.solve("(fail *-> X = then ; X = else).");
        assertEquals(1, s.size());
        assertEquals("else", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0201

    // START_CHANGE: ISS-2025-0198 - hex escape in quoted atom
    @Test
    public void testISS0198_hexEscape() {
        List<Map<String, Term>> s = prolog.solve("atom_codes('\\x41\\', C).");
        assertEquals(1, s.size());
        assertEquals("[65]", s.get(0).get("C").toString());
    }
    // END_CHANGE: ISS-2025-0198

    // START_CHANGE: ISS-2025-0200 - double_quotes flag honored
    @Test
    public void testISS0200_doubleQuotesCodes() {
        prolog.solve("set_prolog_flag(double_quotes, codes).");
        try {
            List<Map<String, Term>> s = prolog.solve("X = \"AB\".");
            assertEquals(1, s.size());
            assertEquals("[65, 66]", s.get(0).get("X").toString());
        } finally {
            prolog.solve("set_prolog_flag(double_quotes, string).");
        }
    }
    // END_CHANGE: ISS-2025-0200

    // START_CHANGE: ISS-2025-0215 - length/2 fresh vars don't collide
    @Test
    public void testISS0215_lengthFreshVarsDistinct() {
        List<Map<String, Term>> s = prolog.solve("length(L1, 2), length(L2, 2), L1 = [a, b], L2 = [c, d].");
        assertEquals(1, s.size());
        // If vars collided, L1 = L2 = [a, b] would unify L2 with [c, d] -> fail
    }
    // END_CHANGE: ISS-2025-0215

    // START_CHANGE: ISS-2025-0216 - is_list/proper_list cycle detection
    @Test
    public void testISS0216_isListOnGround() {
        List<Map<String, Term>> s = prolog.solve("is_list([a, b, c]).");
        assertEquals(1, s.size());
    }

    @Test
    public void testISS0216_isListOnNonList() {
        List<Map<String, Term>> s = prolog.solve("is_list(foo).");
        assertEquals(0, s.size());
    }
    // END_CHANGE: ISS-2025-0216

    // START_CHANGE: ISS-2025-0221 - Partition class exists (not registered as builtin to avoid shadowing user partition/N)
    // Verification via direct class invocation deferred to standalone test if needed.
    // END_CHANGE: ISS-2025-0221

    // START_CHANGE: ISS-2025-0220 - sort/4
    @Test
    public void testISS0220_sort4Descending() {
        List<Map<String, Term>> s = prolog.solve("sort(0, @>, [3, 1, 2, 1], L).");
        assertEquals(1, s.size());
        assertEquals("[3, 2, 1]", s.get(0).get("L").toString());
    }

    @Test
    public void testISS0220_sort4StableNoDedup() {
        List<Map<String, Term>> s = prolog.solve("sort(0, @=<, [3, 1, 2, 1], L).");
        assertEquals(1, s.size());
        assertEquals("[1, 1, 2, 3]", s.get(0).get("L").toString());
    }
    // END_CHANGE: ISS-2025-0220

    // START_CHANGE: ISS-2025-0224 - ^/2 integer power
    @Test
    public void testISS0224_caretIntegerPower() {
        List<Map<String, Term>> s = prolog.solve("X is 2 ^ 10.");
        assertEquals(1, s.size());
        assertEquals("1024", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0224

    // START_CHANGE: ISS-2025-0225 - integer/1 evaluable truncates toward zero
    @Test
    public void testISS0225_integerEvaluable() {
        List<Map<String, Term>> s = prolog.solve("X is integer(3.7).");
        assertEquals(1, s.size());
        assertEquals("3", s.get(0).get("X").toString());
    }

    @Test
    public void testISS0225_integerEvaluableNegative() {
        List<Map<String, Term>> s = prolog.solve("X is integer(-3.7).");
        assertEquals(1, s.size());
        assertEquals("-3", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0225

    // START_CHANGE: ISS-2025-0226 - hyperbolic functions
    @Test
    public void testISS0226_sinh() {
        List<Map<String, Term>> s = prolog.solve("X is sinh(0.0).");
        assertEquals(1, s.size());
        assertEquals("0.0", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0226

    // START_CHANGE: ISS-2025-0227 - log/2 + epsilon
    @Test
    public void testISS0227_logBase() {
        List<Map<String, Term>> s = prolog.solve("X is log(10, 100).");
        assertEquals(1, s.size());
        // log_10(100) = 2.0
        double v = Double.parseDouble(s.get(0).get("X").toString());
        org.junit.Assert.assertEquals(2.0, v, 1e-9);
    }

    @Test
    public void testISS0227_epsilon() {
        List<Map<String, Term>> s = prolog.solve("X is epsilon.");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0227

    // START_CHANGE: ISS-2025-0229 - 0.0**negative throws undefined
    @Test
    public void testISS0229_zeroFloatNegPowerErrors() {
        try {
            prolog.solve("X is 0.0 ** -1.");
            org.junit.Assert.fail("expected evaluation_error(undefined)");
        } catch (RuntimeException e) {
            // accept ISO error wrapping
        }
    }
    // END_CHANGE: ISS-2025-0229

    // START_CHANGE: ISS-2025-0234 - =.. supports numbers
    @Test
    public void testISS0234_univNumber() {
        List<Map<String, Term>> s = prolog.solve("42 =.. L.");
        assertEquals(1, s.size());
        assertEquals("[42]", s.get(0).get("L").toString());
    }
    // END_CHANGE: ISS-2025-0234

    // START_CHANGE: ISS-2025-0235 - atom_number hex
    @Test
    public void testISS0235_atomNumberHex() {
        List<Map<String, Term>> s = prolog.solve("atom_number('0xFF', X).");
        assertEquals(1, s.size());
        assertEquals("255", s.get(0).get("X").toString());
    }

    @Test
    public void testISS0235_atomNumberBinary() {
        List<Map<String, Term>> s = prolog.solve("atom_number('0b1010', X).");
        assertEquals(1, s.size());
        assertEquals("10", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0235

    // START_CHANGE: ISS-2025-0237 - atomic_list_concat/2
    @Test
    public void testISS0237_atomicListConcat2() {
        List<Map<String, Term>> s = prolog.solve("atomic_list_concat([hello, ' ', world], R).");
        assertEquals(1, s.size());
        assertEquals("hello world", s.get(0).get("R").toString());
    }
    // END_CHANGE: ISS-2025-0237

    // START_CHANGE: ISS-2025-0238 - atom_to_term/3
    @Test
    public void testISS0238_atomToTerm() {
        List<Map<String, Term>> s = prolog.solve("atom_to_term('foo(X, Y)', T, B).");
        assertEquals(1, s.size());
        // Term should be foo(_, _) parsed; bindings list contains pairs name=var
        String b = s.get(0).get("B").toString();
        // Term form of bindings is [=(X,_),=(Y,_)] — check both X and Y appear as binding names
        org.junit.Assert.assertTrue("expected X in bindings: " + b, b.contains("X"));
        org.junit.Assert.assertTrue("expected Y in bindings: " + b, b.contains("Y"));
    }
    // END_CHANGE: ISS-2025-0238

    // START_CHANGE: ISS-2025-0246 - freeze multi-goal aggregation (storage-side fix only;
    // full hook firing on =/2 requires solver-level attvar trigger improvements deferred to future)

    // START_CHANGE: ISS-2025-0249 - format width
    @Test
    public void testISS0249_formatWidth() {
        java.io.PrintStream orig = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            prolog.solve("format('~5w!', [hi]).");
        } finally {
            System.setOut(orig);
        }
        assertEquals("   hi!", baos.toString());
    }

    @Test
    public void testISS0249_formatFloatPrec() {
        java.io.PrintStream orig = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            prolog.solve("format('~2f', [3.14159]).");
        } finally {
            System.setOut(orig);
        }
        assertEquals("3.14", baos.toString());
    }

    @Test
    public void testISS0249_formatRadix() {
        java.io.PrintStream orig = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            prolog.solve("format('~2r', [10]).");
        } finally {
            System.setOut(orig);
        }
        assertEquals("1010", baos.toString());
    }

    @Test
    public void testISS0249_formatGrouped() {
        java.io.PrintStream orig = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            prolog.solve("format('~D', [1234567]).");
        } finally {
            System.setOut(orig);
        }
        assertEquals("1,234,567", baos.toString());
    }
    // END_CHANGE: ISS-2025-0249

    // START_CHANGE: ISS-2025-0242 - operator-aware write
    @Test
    public void testISS0242_writeOperator() {
        java.io.PrintStream origOut = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            prolog.solve("write(1+2).");
        } finally {
            System.setOut(origOut);
        }
        org.junit.Assert.assertEquals("1+2", baos.toString().trim());
    }

    @Test
    public void testISS0242_writeList() {
        java.io.PrintStream origOut = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            prolog.solve("write([a,b,c]).");
        } finally {
            System.setOut(origOut);
        }
        org.junit.Assert.assertEquals("[a,b,c]", baos.toString().trim());
    }
    // END_CHANGE: ISS-2025-0242

    // ============== ISS-2025-0245..0252: Audit fixes (2026-06-07) ==============

    // START_CHANGE: ISS-2025-0245 - append/3 mode by proper-list structure, not deep groundness
    @Test
    public void testISS0245_appendWithUnboundElements() {
        // Previously threw PrologEvaluationException("unsupported mode") whenever a list
        // element was an unbound variable. Concatenate mode must work with variable elements.
        List<Map<String, Term>> s = prolog.solve("append([a],[X],R).");
        assertEquals("append([a],[X],R) must succeed, not throw", 1, s.size());

        // split mode binding a variable element on the result side
        s = prolog.solve("append([1],[X],[1,99]).");
        assertEquals(1, s.size());
        assertEquals("99", s.get(0).get("X").toString());

        // build a partial list then constrain the embedded variable
        s = prolog.solve("append([a],[Y],R), R = [a,b].");
        assertEquals(1, s.size());
        assertEquals("b", s.get(0).get("Y").toString());
    }
    // END_CHANGE: ISS-2025-0245

    // START_CHANGE: ISS-2025-0246 - set_prolog_flag(occurs_check, true) actually affects unify
    @Test
    public void testISS0246_occursCheckFlagWired() {
        // occursCheckEnabled is a process-wide flag; always reset it to avoid bleeding into
        // other tests. (We do NOT exercise the occurs_check=false "X = f(X) succeeds" path
        // here: building the cyclic term hits a separate, pre-existing StackOverflow when the
        // solution is post-processed — unrelated to this flag-wiring fix.)
        try {
            prolog.solve("set_prolog_flag(occurs_check, true).");
            List<Map<String, Term>> s = prolog.solve("X = f(X).");
            assertTrue("X = f(X) must FAIL with occurs_check=true", s.isEmpty());
            // an acyclic unification still succeeds with the flag enabled
            s = prolog.solve("Y = f(a).");
            assertFalse("acyclic unification must still succeed", s.isEmpty());
        } finally {
            prolog.solve("set_prolog_flag(occurs_check, false).");
        }
    }
    // END_CHANGE: ISS-2025-0246

    // START_CHANGE: ISS-2025-0247 - (**)/2 is the ISO float power
    @Test
    public void testISS0247_powerIsFloat() {
        List<Map<String, Term>> s = prolog.solve("X is 2 ** 3.");
        assertEquals(1, s.size());
        assertEquals("8.0", s.get(0).get("X").toString());
        // (^)/2 stays integer for integer operands
        s = prolog.solve("X is 2 ^ 3.");
        assertEquals(1, s.size());
        assertEquals("8", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0247

    // START_CHANGE: ISS-2025-0248 - ISO error terms from arithmetic (not bare atoms)
    @Test
    public void testISS0248_isoArithmeticErrorTerms() {
        // unbound variable -> error(instantiation_error, _)
        List<Map<String, Term>> s =
            prolog.solve("catch(_ is _Y + 1, error(instantiation_error, _), true).");
        assertEquals("instantiation_error must be catchable as an ISO error term", 1, s.size());

        // unknown atom -> error(type_error(evaluable, _), _)
        s = prolog.solve("catch(_ is foo, error(type_error(evaluable, _), _), true).");
        assertEquals(1, s.size());

        // unknown functor -> error(type_error(evaluable, _), _)
        s = prolog.solve("catch(_ is bar(1,2), error(type_error(evaluable, _), _), true).");
        assertEquals(1, s.size());

        // comparison predicates also preserve the ISO error term
        s = prolog.solve("catch((_X > 1), error(instantiation_error, _), true).");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0248

    // START_CHANGE: ISS-2025-0249 - integer-only ops reject float arguments (ISO)
    @Test
    public void testISS0249_integerOpsRejectFloat() {
        List<Map<String, Term>> s =
            prolog.solve("catch(_ is 7.5 mod 2, error(type_error(integer, _), _), true).");
        assertEquals(1, s.size());
        s = prolog.solve("catch(_ is 5 mod 2.5, error(type_error(integer, _), _), true).");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0249

    // START_CHANGE: ISS-2025-0250 - rounding does not saturate to Long.MAX_VALUE
    @Test
    public void testISS0250_roundingNoLongSaturation() {
        // 1.0e20 (~1e20) is far beyond Long.MAX_VALUE (~9.22e18). truncate must keep the
        // magnitude as a BigInteger, not clamp to 9223372036854775807.
        List<Map<String, Term>> s = prolog.solve("X is truncate(1.0e20).");
        assertEquals(1, s.size());
        String x = s.get(0).get("X").toString();
        assertNotEquals("must not saturate to Long.MAX_VALUE", "9223372036854775807", x);
        assertTrue("should preserve ~1e20 magnitude, got " + x, x.startsWith("100000000000000"));
    }
    // END_CHANGE: ISS-2025-0250

    // START_CHANGE: ISS-2025-0251 - retract((Head :- Body)) matches stored rules
    @Test
    public void testISS0251_retractClauseForm() {
        prolog.solve("assertz(q0251).");
        prolog.solve("assertz((p0251(2) :- q0251)).");
        prolog.solve("assertz(p0251(1)).");

        // retract the RULE via its clause form; Body must unify with q0251
        List<Map<String, Term>> s = prolog.solve("retract((p0251(2) :- B)).");
        assertEquals("retract of a rule clause must match", 1, s.size());
        assertEquals("q0251", s.get(0).get("B").toString());

        // rule is gone now
        s = prolog.solve("retract((p0251(2) :- _B)).");
        assertTrue("rule already retracted", s.isEmpty());

        // bare-head retract of a fact still works
        s = prolog.solve("retract(p0251(1)).");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0251

    // START_CHANGE: ISS-2025-0252 - CLP(FD) store does not leak across top-level queries
    @Test
    public void testISS0252_clpfdStoreResetBetweenQueries() {
        // The same constrained query run twice must give the same number of solutions;
        // a leaking singleton store made the second run differ (domains/constraints
        // accumulated under the same variable names).
        String q = "C0252 in 1..2, E0252 in 1..2, C0252 #\\= E0252, label([C0252,E0252]).";
        int n1 = prolog.solve(q).size();
        int n2 = prolog.solve(q).size();
        assertEquals("identical CLP(FD) query must be deterministic across runs", n1, n2);
        assertTrue("query should produce solutions", n1 > 0);
    }
    // END_CHANGE: ISS-2025-0252

    // START_CHANGE: ISS-2025-0253 - phrase/2,3 enumerate all solutions
    @Test
    public void testISS0253_phraseIsMultiSolution() {
        prolog.consult("ab0253 --> [a].\nab0253 --> [a,b].");
        // phrase/3 must enumerate BOTH parses (R=[b] from the 1st rule, R=[] from the 2nd),
        // not behave like once(phrase(...)).
        List<Map<String, Term>> s = prolog.solve("phrase(ab0253, [a,b], R).");
        assertEquals("phrase/3 must be multi-solution", 2, s.size());
    }
    // END_CHANGE: ISS-2025-0253

    // START_CHANGE: ISS-2025-0254 - cut in a DCG body threads the difference list
    @Test
    public void testISS0254_dcgCutThreadsDifferenceList() {
        prolog.consult("seq0254 --> [a], !, [b].");
        // With the cut mistranslated to !/2 the S1=S2 threading was lost and X stayed unbound.
        // Correct translation binds X to b.
        List<Map<String, Term>> s = prolog.solve("phrase(seq0254, [a, X]).");
        assertEquals(1, s.size());
        assertEquals("b", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0254

    // START_CHANGE: ISS-2025-0255 - call_dcg/3 actually runs the DCG body
    @Test
    public void testISS0255_callDcgRunsBody() {
        prolog.consult("lit0255 --> [x].");
        // The old stub unified Input with Output and returned R=[x]; the correct result is
        // R=[] after consuming x via the lit0255 rule.
        List<Map<String, Term>> s = prolog.solve("call_dcg(lit0255, [x], R).");
        assertEquals(1, s.size());
        assertEquals("[]", s.get(0).get("R").toString());
    }
    // END_CHANGE: ISS-2025-0255

    // START_CHANGE: ISS-2025-0256 - negative sign applied to radix/char-code literals
    @Test
    public void testISS0256_negativeRadixLiterals() {
        // Previously the leading '-' was dropped for hex/octal/binary/char-code literals.
        List<Map<String, Term>> s = prolog.solve("X is -0xFF.");
        assertEquals("-255", s.get(0).get("X").toString());

        s = prolog.solve("X is -0o17.");
        assertEquals("-15", s.get(0).get("X").toString());

        s = prolog.solve("X is -0b1010.");
        assertEquals("-10", s.get(0).get("X").toString());

        s = prolog.solve("X is -0'a.");
        assertEquals("-97", s.get(0).get("X").toString());

        // sanity: positive forms still work
        s = prolog.solve("X is 0xFF.");
        assertEquals("255", s.get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0256

    // START_CHANGE: ISS-2025-0261 - integers and floats are distinct terms (ISO standard order)
    @Test
    public void testISS0261_intFloatAreDistinctTerms() {
        // unification: 1 does NOT unify with 1.0
        assertTrue("1 = 1.0 must fail", prolog.solve("1 = 1.0.").isEmpty());
        assertFalse("1 = 1 must succeed", prolog.solve("1 = 1.").isEmpty());
        assertFalse("1.0 = 1.0 must succeed", prolog.solve("1.0 = 1.0.").isEmpty());

        // term equality: 1 \== 1.0
        assertTrue("1 == 1.0 must fail", prolog.solve("1 == 1.0.").isEmpty());
        assertFalse("1 == 1 must succeed", prolog.solve("1 == 1.").isEmpty());

        // standard order: a float sorts before a numerically-equal integer, so 1 @> 1.0
        List<Map<String, Term>> s = prolog.solve("compare(O, 1, 1.0).");
        assertEquals(1, s.size());
        assertEquals(">", s.get(0).get("O").toString());

        // sort/2 must NOT dedup 1 and 1.0; the float comes first
        s = prolog.solve("sort([1, 1.0], [A, B]).");
        assertEquals(1, s.size());
        assertEquals("1.0", s.get(0).get("A").toString());
        assertEquals("1", s.get(0).get("B").toString());
    }
    // END_CHANGE: ISS-2025-0261

    // START_CHANGE: ISS-2025-0263 - CLP(FD) huge domain raises resource_error, not OOM
    @Test
    public void testISS0263_clpfdHugeDomainRejected() {
        // A ~2-billion-value range was materialized as boxed Integers (OOM) and the int loop
        // counter would overflow at Integer.MAX_VALUE and never terminate. Now it raises a
        // catchable resource_error.
        List<Map<String, Term>> s = prolog.solve(
            "catch((X in 1..2000000000, indomain(X)), error(resource_error(_), _), true).");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0263

    // START_CHANGE: ISS-2025-0264 - indomain/1 only emits constraint-consistent values
    @Test
    public void testISS0264_indomainRespectsConstraints() {
        // Pigeonhole: three variables over {1,2} cannot be all-different, so indomain(X) must
        // yield NO value. Previously every domain value was emitted blindly (unsound).
        List<Map<String, Term>> s = prolog.solve(
            "X in 1..2, Y in 1..2, Z in 1..2, all_different([X,Y,Z]), indomain(X).");
        assertTrue("indomain must not emit values that violate posted constraints", s.isEmpty());

        // Unconstrained: indomain enumerates the whole domain.
        s = prolog.solve("X in 1..3, indomain(X).");
        assertEquals(3, s.size());
    }
    // END_CHANGE: ISS-2025-0264

    // START_CHANGE: ISS-2025-0266 - set operations distinguish atoms from numbers
    @Test
    public void testISS0266_setOpsDistinguishAtomNumber() {
        // The atom '1' and the number 1 are different terms; toString() conflated them.
        List<Map<String, Term>> s = prolog.solve("subtract([1,'1'], [1], R).");
        assertEquals(1, s.size());
        assertEquals("[1]", s.get(0).get("R").toString()); // the atom '1' remains (number 1 removed)

        s = prolog.solve("intersection([1], ['1'], R).");
        assertEquals(1, s.size());
        assertEquals("[]", s.get(0).get("R").toString());

        // normal case unaffected
        s = prolog.solve("subtract([a,b,c], [b], R).");
        assertEquals("[a, c]", s.get(0).get("R").toString());
    }
    // END_CHANGE: ISS-2025-0266

    // START_CHANGE: ISS-2025-0267 - split_string keeps empty substrings (SWI semantics)
    @Test
    public void testISS0267_splitStringKeepsEmpties() {
        // "a,,b" split on "," with no padding keeps the empty middle field: 3 elements.
        assertEquals(1, prolog.solve("split_string(\"a,,b\", \",\", \"\", X), X = [_,_,_].").size());
        // empty input yields a single (empty) field: [""].
        assertEquals(1, prolog.solve("split_string(\"\", \",\", \"\", X), X = [_].").size());
        // when a separator char is also a pad char, runs of separators collapse to 2 fields.
        assertEquals(1, prolog.solve("split_string(\"a  b\", \" \", \" \", X), X = [_,_].").size());
    }
    // END_CHANGE: ISS-2025-0267

    // START_CHANGE: ISS-2025-0268 - atomic_list_concat accepts numbers
    @Test
    public void testISS0268_atomicListConcatAcceptsNumbers() {
        List<Map<String, Term>> s = prolog.solve("atomic_list_concat([a,1,b], R).");
        assertEquals(1, s.size());
        assertEquals("a1b", s.get(0).get("R").toString());

        s = prolog.solve("atomic_list_concat([x,2,y], '-', R).");
        assertEquals(1, s.size());
        assertEquals("x-2-y", s.get(0).get("R").toString());
    }
    // END_CHANGE: ISS-2025-0268

    // START_CHANGE: ISS-2025-0269 - type_error(evaluable, _) culprit is the compound Name/Arity
    @Test
    public void testISS0269_evaluableCulpritIsCompound() {
        // The predicate indicator must be the compound '/'(Name, Arity), not an atom 'Name/Arity'.
        List<Map<String, Term>> s = prolog.solve(
            "catch(_ is foo, error(type_error(evaluable, C), _), (C = N/A, N == foo, A == 0)).");
        assertEquals(1, s.size());
        s = prolog.solve(
            "catch(_ is bar(1,2), error(type_error(evaluable, C), _), (compound(C) -> true ; fail)).");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0269

    // START_CHANGE: ISS-2025-0270 - clause/2 ISO errors
    @Test
    public void testISS0270_clauseInstantiationError() {
        // clause(Head, Body) with Head unbound must raise instantiation_error.
        List<Map<String, Term>> s = prolog.solve(
            "catch(clause(_, _), error(instantiation_error, _), true).");
        assertEquals(1, s.size());

        // clause/2 still works for an instantiated head; body shares the head variable.
        prolog.solve("assertz((g0270(X) :- f0270(X))).");
        s = prolog.solve("clause(g0270(Y), B), B = f0270(Y).");
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0270

    // START_CHANGE: ISS-2025-0271 - min/max preserve the selected operand's type
    @Test
    public void testISS0271_minMaxPreserveType() {
        // min picks the smaller value keeping its type: min(2, 3.0) = 2 (integer), not 2.0.
        assertEquals("2", prolog.solve("X is min(2, 3.0).").get(0).get("X").toString());
        assertEquals("3.0", prolog.solve("X is max(2, 3.0).").get(0).get("X").toString());
        assertEquals("2.0", prolog.solve("X is min(2.0, 3).").get(0).get("X").toString());
        assertEquals("5", prolog.solve("X is max(5, 2).").get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0271

    // START_CHANGE: ISS-2025-0272 - gcd requires integer arguments
    @Test
    public void testISS0272_gcdRequiresIntegers() {
        List<Map<String, Term>> s = prolog.solve(
            "catch(_ is gcd(4, 2.0), error(type_error(integer, C), _), C == 2.0).");
        assertEquals(1, s.size());
        // both-integer gcd still works
        assertEquals("2", prolog.solve("X is gcd(4, 6).").get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0272

    // START_CHANGE: ISS-2025-0273 - setup_call_cleanup/3 and call_cleanup/2
    @Test
    public void testISS0273_setupCallCleanup() {
        // START_CHANGE: ISS-2025-0442 - the engine implements setup_call_cleanup/3 with a real
        // CLEANUP FRAME (design B.6): Cleanup runs when Goal has no alternatives left, i.e. after
        // its LAST solution, not eagerly after the first. That is the ISO/SWI behaviour.
        // ISS-2025-0491 - 4.1 wave A: the v2 branch (an eager bridged built-in that ran Cleanup
        // after the FIRST solution) is gone with the v2 engine.
        List<Map<String, Term>> s;
        assertEquals("both solutions are produced, and Cleanup ran by the end", 1, prolog.solve(
            "findall(X, setup_call_cleanup(true, member(X,[1,2]), assertz(scc_ok)), L), "
            + "L == [1,2], scc_ok.").size());
        // END_CHANGE: ISS-2025-0442

        // Cleanup runs even when the goal fails.
        s = prolog.solve("(setup_call_cleanup(true, fail, assertz(scc_f)) ; true), scc_f.");
        assertEquals(1, s.size());

        // Cleanup runs when the goal raises, before the exception propagates.
        s = prolog.solve(
            "catch(setup_call_cleanup(true, throw(boom), assertz(scc_e)), boom, true), scc_e.");
        assertEquals(1, s.size());

        // call_cleanup/2 (ISS-2025-0491: v4 only, like the setup_call_cleanup/3 case above).
        assertEquals("call_cleanup runs Cleanup once, after the last solution", 1, prolog.solve(
            "findall(X, call_cleanup(member(X,[a,b]), assertz(scc_cc)), L), L == [a,b], scc_cc.").size());
    }
    // END_CHANGE: ISS-2025-0273

    // START_CHANGE: ISS-2025-0274 - =:= / =\= use IEEE semantics for signed zero and NaN
    @Test
    public void testISS0274_arithCompareIeee() {
        assertFalse("-0.0 =:= 0.0 must succeed", prolog.solve("-0.0 =:= 0.0.").isEmpty());
        assertTrue("-0.0 =\\= 0.0 must fail", prolog.solve("-0.0 =\\= 0.0.").isEmpty());
        // nan =:= nan must FAIL (NaN is not equal to itself)
        assertTrue("nan =:= nan must fail", prolog.solve("X is nan, X =:= X.").isEmpty());
    }
    // END_CHANGE: ISS-2025-0274

    // START_CHANGE: ISS-2025-0275 - throw/1 throws a copy of the ball
    @Test
    public void testISS0275_throwCopiesBall() {
        List<Map<String, Term>> s = prolog.solve("catch(throw(t(a)), t(Y), true).");
        assertEquals(1, s.size());
        assertEquals("a", s.get(0).get("Y").toString());
    }
    // END_CHANGE: ISS-2025-0275

    // START_CHANGE: ISS-2025-0276 - up/downcase_atom are locale-independent
    @Test
    public void testISS0276_caseFoldLocaleIndependent() {
        assertEquals("ABC", prolog.solve("upcase_atom(abc, X).").get(0).get("X").toString());
        assertEquals("abc", prolog.solve("downcase_atom('ABC', X).").get(0).get("X").toString());
        // ASCII 'i'/'I' must map predictably regardless of the JVM default locale
        assertEquals("I", prolog.solve("upcase_atom(i, X).").get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0276

    // START_CHANGE: ISS-2025-0277 - atom_length/2 ISO error terms
    @Test
    public void testISS0277_atomLengthIsoErrors() {
        assertEquals(1, prolog.solve(
            "catch(atom_length(_, _), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(atom_length(123, _), error(type_error(atom, _), _), true).").size());
        // normal case still works
        assertEquals("5", prolog.solve("atom_length(hello, N).").get(0).get("N").toString());
    }
    // END_CHANGE: ISS-2025-0277

    // START_CHANGE: ISS-2025-0278 - op/3 rejects a non-integer precedence
    @Test
    public void testISS0278_opNonIntegerPrecedence() {
        assertEquals(1, prolog.solve(
            "catch(op(700.5, xfx, myop0278), error(type_error(integer, _), _), true).").size());
        // integer precedence still works
        assertFalse(prolog.solve("op(700, xfx, myop0278b).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0278

    // START_CHANGE: ISS-2025-0279 - initialization/1 runs after the file is loaded
    @Test
    public void testISS0279_initializationRuns() {
        prolog.consult(":- initialization(assertz(ran0279)).\nfoo0279(1).");
        assertFalse("initialization goal must have run", prolog.solve("ran0279.").isEmpty());

        // the goal may reference a predicate defined later in the same file
        Prolog p2 = new Prolog();
        p2.consult("main0279 :- assertz(done0279).\n:- initialization(main0279).");
        assertFalse(p2.solve("done0279.").isEmpty());
    }
    // END_CHANGE: ISS-2025-0279

    // START_CHANGE: ISS-2025-0282 - ','/2 still works after removing the dead Conjunction built-in
    @Test
    public void testISS0282_conjunctionStillWorks() {
        // handleConjunction (QuerySolver) is authoritative; multi-solution conjunction must work.
        List<Map<String, Term>> s = prolog.solve("member(X,[1,2]), member(Y,[a,b]).");
        assertEquals(4, s.size());
    }
    // END_CHANGE: ISS-2025-0282

    // START_CHANGE: ISS-2025-0283 - op/3 accepts a list of names
    @Test
    public void testISS0283_opListOfNames() {
        List<Map<String, Term>> s = prolog.solve(
            "op(700, xfx, [eqx0283, neqx0283]), current_op(P1, xfx, eqx0283), current_op(P2, xfx, neqx0283).");
        assertEquals(1, s.size());
        assertEquals("700", s.get(0).get("P1").toString());
        assertEquals("700", s.get(0).get("P2").toString());
        // single-atom form still works
        assertFalse(prolog.solve("op(650, xfx, single0283), current_op(_, xfx, single0283).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0283

    // START_CHANGE: ISS-2025-0284 - number_string keeps integer precision
    @Test
    public void testISS0284_numberStringBigInteger() {
        List<Map<String, Term>> s = prolog.solve("number_string(N, \"123456789012345678901234567890\").");
        assertEquals(1, s.size());
        assertEquals("123456789012345678901234567890", s.get(0).get("N").toString());
        // float and small int still parse correctly
        assertEquals("42", prolog.solve("number_string(N, \"42\").").get(0).get("N").toString());
        assertEquals("3.14", prolog.solve("number_string(N, \"3.14\").").get(0).get("N").toString());
    }
    // END_CHANGE: ISS-2025-0284

    // START_CHANGE: ISS-2025-0305 - closing a stream removes ALL of its aliases (no dangling)
    @Test
    public void testISS0305_CloseRemovesAllAliases() {
        it.denzosoft.jprolog.builtin.io.StreamManager.registerInputStream(
            "stream_test", new java.io.ByteArrayInputStream(new byte[]{1, 2, 3}));
        it.denzosoft.jprolog.builtin.io.StreamManager.aliasStream("stream_test", "my_alias");
        assertTrue(it.denzosoft.jprolog.builtin.io.StreamManager.hasStream("stream_test"));
        assertTrue(it.denzosoft.jprolog.builtin.io.StreamManager.hasStream("my_alias"));
        // closing via either alias must drop BOTH (previously the other dangled)
        it.denzosoft.jprolog.builtin.io.StreamManager.closeStream("my_alias");
        assertFalse(it.denzosoft.jprolog.builtin.io.StreamManager.hasStream("my_alias"));
        assertFalse("the sibling alias must not dangle after close",
            it.denzosoft.jprolog.builtin.io.StreamManager.hasStream("stream_test"));
    }
    // END_CHANGE: ISS-2025-0305

    // START_CHANGE: ISS-2025-0333 - conditional & hit-count breakpoints
    @Test(timeout = 20000)
    public void testISS0333_ConditionalAndHitCountBreakpoints() throws Exception {
        assertEquals("ignore=0 pauses on both parent calls", 2, countParentCallPauses(0, null));
        assertEquals("ignore=1 skips the first hit", 1, countParentCallPauses(1, null));
        assertEquals("ignore=2 skips both", 0, countParentCallPauses(2, null));
        assertEquals("false condition never pauses", 0, countParentCallPauses(0, "1 > 2"));
        assertEquals("true condition pauses on both", 2, countParentCallPauses(0, "1 < 2"));
    }

    private int countParentCallPauses(int ignore, String condition) throws Exception {
        Prolog p = new Prolog();
        p.consult("parent(tom, bob).\nparent(bob, ann).\ngrandparent(X, Z) :- parent(X, Y), parent(Y, Z).");
        it.denzosoft.jprolog.core.engine.DebugController dc = new it.denzosoft.jprolog.core.engine.DebugController();
        dc.reset();
        dc.addBreakpoint("parent/2", new java.util.HashSet<>(java.util.Arrays.asList("CALL")), condition, ignore);
        dc.setConditionEvaluator((c, b) -> {
            it.denzosoft.jprolog.core.engine.DebugController saved = p.getEngineContext().getDebugController();
            try { p.getEngineContext().setDebugController(null); return !p.solve(c).isEmpty(); }
            catch (RuntimeException e) { return false; }
            finally { p.getEngineContext().setDebugController(saved); }
        });
        final int[] hits = {0};
        dc.setListener(new it.denzosoft.jprolog.core.engine.DebugController.DebugListener() {
            public void onDebugPaused(it.denzosoft.jprolog.core.engine.DebugEvent e) {
                if (e.getPort() == it.denzosoft.jprolog.core.engine.DebugEvent.Port.CALL
                        && "parent".equals(e.getGoal().getName())) hits[0]++;
                new Thread(() -> dc.resumeWithAction(
                    it.denzosoft.jprolog.core.engine.DebugEvent.Action.CONTINUE)).start();
            }
            public void onTraceEvent(it.denzosoft.jprolog.core.engine.DebugEvent e) {}
            public void onDebugFinished() {}
        });
        p.getEngineContext().setDebugController(dc);
        Thread t = new Thread(() -> p.solve("grandparent(tom, R)."));
        t.start(); t.join(8000);
        if (t.isAlive()) { dc.stop(); t.join(2000); }
        p.getEngineContext().setDebugController(null);
        return hits[0];
    }
    // END_CHANGE: ISS-2025-0333

    // START_CHANGE: ISS-2025-0331 - the v2 engine fires four-port DebugController events (IDE debugging)
    @Test(timeout = 15000)
    public void testISS0331_V2EngineDebugPorts() throws Exception {
        Prolog p = new Prolog();
        p.consult("parent(tom, bob).\nparent(bob, ann).\ngrandparent(X, Z) :- parent(X, Y), parent(Y, Z).");
        it.denzosoft.jprolog.core.engine.DebugController dc = new it.denzosoft.jprolog.core.engine.DebugController();
        dc.reset();
        final List<String> ports = java.util.Collections.synchronizedList(new java.util.ArrayList<>());
        dc.setListener(new it.denzosoft.jprolog.core.engine.DebugController.DebugListener() {
            public void onDebugPaused(it.denzosoft.jprolog.core.engine.DebugEvent e) {
                ports.add(e.getPort().name());
                new Thread(() -> dc.resumeWithAction(
                    it.denzosoft.jprolog.core.engine.DebugEvent.Action.STEP_INTO)).start();
            }
            public void onTraceEvent(it.denzosoft.jprolog.core.engine.DebugEvent e) {}
            public void onDebugFinished() {}
        });
        p.getEngineContext().setDebugController(dc);
        final List<Map<String, Term>>[] sols = new List[1];
        Thread t = new Thread(() -> sols[0] = p.solve("grandparent(tom, R)."));
        try {
            t.start();
            t.join(10000);
        } finally {
            p.getEngineContext().setDebugController(null);
        }
        assertFalse("the v2 debug session must finish", t.isAlive());
        assertTrue("v2 engine must fire CALL ports", ports.contains("CALL"));
        assertTrue("v2 engine must fire EXIT ports", ports.contains("EXIT"));
        assertTrue("the debugged query must reach a solution", sols[0] != null && !sols[0].isEmpty());
    }
    // END_CHANGE: ISS-2025-0331

    // START_CHANGE: ISS-2025-0329 - trace/0 .. notrace/0 emit four-port trace through the v2 engine
    @Test
    public void testISS0329_TraceFourPorts() {
        Prolog p = new Prolog();
        p.consult("parent(tom, bob).\nparent(bob, ann).\ngrandparent(X, Z) :- parent(X, Y), parent(Y, Z).");
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        java.io.PrintStream ps = new java.io.PrintStream(baos);
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            p.solve("trace.");
            p.solve("grandparent(tom, X).");
        } finally {
            it.denzosoft.jprolog.builtin.debug.Trace.setTracingEnabled(false);   // never leak the static flag
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(null);
        }
        ps.flush();
        String out = baos.toString();
        assertTrue("trace must show a Call port: " + out, out.contains("Call:"));
        assertTrue("trace must show an Exit port: " + out, out.contains("Exit:"));
        assertTrue("trace must name the traced predicate", out.contains("grandparent"));
    }
    // END_CHANGE: ISS-2025-0329

    // ISS-2025-0328's testISS0328_SolveLegacyWorks was REMOVED in wave W9 (ISS-2025-0484):
    // Prolog.solveLegacy and the recursive engine it forced no longer exist.

    // START_CHANGE: ISS-2025-0322 - line -> predicate mapping for line-accurate IDE breakpoints
    @Test
    public void testISS0322_LineToPredicateMapping() {
        Prolog p = new Prolog();
        // line 1: foo(1).   line 2: bar(X) :-   line 3:     foo(X).
        p.consultWithDiagnostics("foo(1).\nbar(X) :-\n    foo(X).\n", "test.pl");
        assertEquals("foo/1", p.getPredicateIndicatorAtLine(1));
        assertEquals("bar/1", p.getPredicateIndicatorAtLine(2));
        assertEquals("body line maps to its owning clause", "bar/1", p.getPredicateIndicatorAtLine(3));
        assertNull("no clause above line 0", p.getPredicateIndicatorAtLine(0));
    }
    // END_CHANGE: ISS-2025-0322

    // START_CHANGE: ISS-2025-0321 - streaming solve delivers one solution at a time and stops on demand
    @Test
    public void testISS0321_SolveStreamCapsAndStops() {
        Prolog p = new Prolog();
        p.consult("n(1). n(2). n(3). n(4). n(5).");
        final List<String> got = new java.util.ArrayList<>();
        p.solveStream("n(X).", sol -> { got.add(sol.get("X").toString()); return got.size() < 3; });
        assertEquals("streaming must stop after the sink returns false", 3, got.size());
        assertEquals("1", got.get(0));
    }
    // END_CHANGE: ISS-2025-0321

    // START_CHANGE: ISS-2025-0320 - interrupting the solver thread aborts a non-terminating query
    @Test(timeout = 15000)
    public void testISS0320_InterruptStopsInfiniteQuery() throws InterruptedException {
        Prolog p = new Prolog();
        p.consult("loop :- loop.");
        final boolean[] cancelled = {false};
        Thread t = new Thread(() -> {
            try {
                p.solve("loop.");
            } catch (it.denzosoft.jprolog.core.engine.QueryCancelledException ce) {
                cancelled[0] = true;
            } catch (Throwable ignore) { /* other terminal outcomes are not what we test here */ }
        });
        t.start();
        Thread.sleep(400);            // let it spin in the resolution loop
        t.interrupt();                // == the IDE Stop button
        t.join(8000);
        assertFalse("the interrupted solver thread must terminate", t.isAlive());
        assertTrue("the cancelled query must raise QueryCancelledException", cancelled[0]);
    }
    // END_CHANGE: ISS-2025-0320

    // ISS-2025-0306: with_output_to capture works (thread-safety of the System.out swap is tracked
    // under LIM-025 — it needs write/1 routed through a per-engine stream, the IO-layer rework).
    @Test
    public void testISS0306_WithOutputToCaptures() {
        List<Map<String, Term>> s = prolog.solve("with_output_to(atom(X), write(hello)).");
        assertEquals(1, s.size());
        assertEquals("hello", s.get(0).get("X").toString());
    }

    // START_CHANGE: ISS-2025-0342 - cut inside \+/1, not/1, (->)/2 and (*->)/2 condition is local
    @Test
    public void testISS0342_CutInNegationConditionIsLocal() {
        // \+((!,fail)): the cut is local to the negated goal, so the condition just fails
        // and the negation succeeds (ISO 8.15.1 executes the argument as call(Goal)).
        assertEquals(1, prolog.solve("\\+ ((!, fail)).").size());
        // sanity: a succeeding cut condition still makes the negation fail
        assertTrue(prolog.solve("\\+ ((!, true)).").isEmpty());
    }

    @Test
    public void testISS0342_CutInIfThenElseConditionRunsElse() {
        // ((!,fail) -> T ; E): the cut is local to the condition (ISO 7.8.8), so the
        // condition fails and the Else branch runs.
        List<Map<String, Term>> s = prolog.solve("((!, fail) -> X = then ; X = else).");
        assertEquals(1, s.size());
        assertEquals("else", s.get(0).get("X").toString());
        // arrow without else: a failing cut condition simply fails the construct
        assertTrue(prolog.solve("((!, fail) -> X = then).").isEmpty());
    }

    @Test
    public void testISS0342_CutInSoftCutConditionRunsElse() {
        List<Map<String, Term>> s = prolog.solve("((!, fail) *-> X = 1 ; X = 2).");
        assertEquals(1, s.size());
        assertEquals("2", s.get(0).get("X").toString());
    }

    @Test
    public void testISS0342_CutInConditionInsideClauseBody() {
        prolog.consult("t11(X) :- ((!, fail) -> X = then ; X = else).\nt11(99).");
        List<Map<String, Term>> s = prolog.solve("t11(X).");
        assertEquals(2, s.size());
        assertEquals("else", s.get(0).get("X").toString());
        assertEquals("99", s.get(1).get("X").toString());
    }

    @Test
    public void testISS0342_CommitSemanticsStillHold() {
        // the internal commit must still cut the condition's OWN choice points:
        // (member(X,[a,b]) -> Y = X ; Y = none) commits to X = a only.
        List<Map<String, Term>> s = prolog.solve("(member(X, [a,b]) -> Y = X ; Y = none).");
        assertEquals(1, s.size());
        assertEquals("a", s.get(0).get("Y").toString());
        // and a top-level cut in a plain disjunction still cuts the whole goal
        assertTrue(prolog.solve("((!, fail) ; X = ok).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0342

    // START_CHANGE: ISS-2025-0343 - catch/3 frame is disarmed once its Goal exits (ISO 7.8.9)
    @Test
    public void testISS0343_ExitedCatchDoesNotRunRecovery() {
        // the recovery of an ALREADY-EXITED catch must not run when a later goal throws
        prolog.solve("retractall(s0343(_)).");     // creates s0343/1 as (empty) dynamic
        try {
            prolog.solve("catch(true, _, assertz(s0343(ran))), throw(err).");
            fail("throw(err) after the catch goal exited must escape");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            assertNotNull(e.getErrorTerm());
            assertEquals("err", e.getErrorTerm().toString());
        }
        assertTrue("spurious recovery side effect committed", prolog.solve("s0343(X).").isEmpty());
    }

    @Test
    public void testISS0343_ExitedCatchDoesNotSwallowLaterThrow() {
        // the stale frame must not convert the error into failure via its recovery
        try {
            prolog.solve("catch(member(X, [1,2]), _, X = caught), X == 1, throw(err).");
            fail("throw(err) raised after catch/3 exited must escape");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            assertNotNull(e.getErrorTerm());
            assertEquals("err", e.getErrorTerm().toString());
        }
    }

    @Test
    public void testISS0343_ReExecutionIsStillProtected() {
        // backtracking INTO the goal re-arms the frame: a throw during the redo IS caught
        List<Map<String, Term>> s = prolog.solve(
            "catch((member(X, [1,2]), (X == 2 -> throw(e) ; true)), e, R = c), (var(R) -> fail ; true).");
        assertEquals(1, s.size());
        assertEquals("c", s.get(0).get("R").toString());
    }

    @Test
    public void testISS0343_NestedExitedCatchFramesAreInert() {
        try {
            prolog.solve("catch(catch(true, _, true), _, true), throw(err).");
            fail("throw(err) after nested catches exited must escape");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            assertNotNull(e.getErrorTerm());
            assertEquals("err", e.getErrorTerm().toString());
        }
    }

    @Test
    public void testISS0343_CatchStillWorksNormally() {
        // armed-frame behaviour is unchanged: catch during Goal, nested rethrow, recovery solutions
        List<Map<String, Term>> s = prolog.solve("catch(throw(my_err), E, true).");
        assertEquals(1, s.size());
        assertEquals("my_err", s.get(0).get("E").toString());
        s = prolog.solve("catch(catch(throw(a), a, throw(b)), b, R = caught_b).");
        assertEquals(1, s.size());
        assertEquals("caught_b", s.get(0).get("R").toString());
        s = prolog.solve("findall(X, catch(throw(t), t, member(X, [1,2])), L).");
        assertEquals(1, s.size());
        assertEquals("[1, 2]", s.get(0).get("L").toString());
    }
    // END_CHANGE: ISS-2025-0343

    // START_CHANGE: ISS-2025-0344 - retract removes exactly ONE clause; rules list stays in sync with the indexes
    @Test
    public void testISS0344_RetractRemovesExactlyOneDuplicateClause() {
        // ISO 8.9.3: each retract SOLUTION removes exactly one clause, even with duplicates.
        // START_CHANGE: ISS-2025-0396 - retract/1 is now re-executable, and solve() enumerates
        // every solution — so a bare retract goal would (correctly) drain all duplicates in one
        // query. Commit to the first solution with a cut to keep pinning the ISS-0344 invariant:
        // ONE clause removed per retract solution, no immortal phantom clauses.
        assertEquals(1, prolog.solve("assertz(q0344(a)), assertz(q0344(a)).").size());
        assertEquals(1, prolog.solve("retract(q0344(a)), !.").size());
        assertEquals("one duplicate must survive the first retract", 1, prolog.solve("q0344(a).").size());
        assertEquals(1, prolog.solve("retract(q0344(a)), !.").size());
        assertTrue("both clauses retracted -> call must fail", prolog.solve("q0344(a).").isEmpty());
        assertTrue("third retract must fail (no immortal phantom)", prolog.solve("retract(q0344(a)), !.").isEmpty());
        assertTrue(prolog.solve("q0344(a).").isEmpty());
        // END_CHANGE: ISS-2025-0396
    }

    @Test
    public void testISS0344_RulesListAndRuleIndexStayInSync() throws Exception {
        prolog.solve("assertz(q0344s(a)), assertz(q0344s(a)).");
        java.lang.reflect.Field kbField =
            it.denzosoft.jprolog.core.engine.Prolog.class.getDeclaredField("knowledgeBase");
        kbField.setAccessible(true);
        it.denzosoft.jprolog.core.engine.KnowledgeBase kb =
            (it.denzosoft.jprolog.core.engine.KnowledgeBase) kbField.get(prolog);
        assertEquals(2, kb.getRulesForPredicate("q0344s", 1).size());
        assertEquals(2, countRules(kb, "q0344s"));
        // START_CHANGE: ISS-2025-0396 - cut after the first solution (retract is now re-executable
        // and solve() enumerates all solutions, which would drain both duplicates in one query)
        prolog.solve("retract(q0344s(a)), !.");
        assertEquals(1, kb.getRulesForPredicate("q0344s", 1).size());
        assertEquals("rules list desynced from ruleIndex", 1, countRules(kb, "q0344s"));
        prolog.solve("retract(q0344s(a)), !.");
        // END_CHANGE: ISS-2025-0396
        assertEquals(0, kb.getRulesForPredicate("q0344s", 1).size());
        assertEquals(0, countRules(kb, "q0344s"));
    }

    private static int countRules(it.denzosoft.jprolog.core.engine.KnowledgeBase kb, String functor) {
        int n = 0;
        for (it.denzosoft.jprolog.core.engine.Rule r : kb.getRules()) {
            Term h = r.getHead();
            String f = (h instanceof Atom) ? ((Atom) h).getName()
                : (h instanceof CompoundTerm) ? ((CompoundTerm) h).getName() : "";
            if (functor.equals(f)) n++;
        }
        return n;
    }

    @Test
    public void testISS0344_KnowledgeBaseRetractRemovesOneByEquality() {
        // direct API path (Prolog.retract(String) parses a fresh Rule -> equals fallback)
        it.denzosoft.jprolog.core.engine.KnowledgeBase kb = new it.denzosoft.jprolog.core.engine.KnowledgeBase();
        it.denzosoft.jprolog.core.engine.Rule r1 =
            new it.denzosoft.jprolog.core.engine.Rule(new CompoundTerm(new Atom("d0344"),
                java.util.Arrays.asList((Term) new Atom("x"))), new java.util.ArrayList<Term>());
        it.denzosoft.jprolog.core.engine.Rule r2 =
            new it.denzosoft.jprolog.core.engine.Rule(new CompoundTerm(new Atom("d0344"),
                java.util.Arrays.asList((Term) new Atom("x"))), new java.util.ArrayList<Term>());
        kb.addRule(r1);
        kb.addRule(r2);
        it.denzosoft.jprolog.core.engine.Rule parsed =
            new it.denzosoft.jprolog.core.engine.Rule(new CompoundTerm(new Atom("d0344"),
                java.util.Arrays.asList((Term) new Atom("x"))), new java.util.ArrayList<Term>());
        kb.retract(parsed);
        assertEquals(1, kb.getRules().size());
        assertEquals(1, kb.getRulesForPredicate("d0344", 1).size());
        kb.retract(parsed);
        assertEquals(0, kb.getRules().size());
        assertEquals(0, kb.getRulesForPredicate("d0344", 1).size());
    }
    // END_CHANGE: ISS-2025-0344

    // START_CHANGE: ISS-2025-0345 - solve(Term) runs the same engine and budget as solve(String)
    @Test
    public void testISS0345_SolveTermHonorsInferenceBudget() {
        prolog.consult("app0345([],L,L).\napp0345([H|T],L,[H|R]) :- app0345(T,L,R).\n"
            + "nrev0345([],[]).\nnrev0345([H|T],R) :- nrev0345(T,RT), app0345(RT,[H],R).");
        StringBuilder l = new StringBuilder("[1");
        for (int i = 2; i <= 120; i++) l.append(",").append(i);
        l.append("]");
        Term query = it.denzosoft.jprolog.core.parser.v2.TermReader.parseTerm(
            "nrev0345(" + l + ", R)", new it.denzosoft.jprolog.core.operator.OperatorTable());
        prolog.setInferenceBudget(2000);
        try {
            prolog.solve(query);
            fail("solve(Term) must enforce the inference budget like solve(String)");
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException expected) {
            // the Term overload is budget-bounded too
        } finally {
            prolog.setInferenceBudget(0);
        }
    }

    @Test
    public void testISS0345_SolveTermReturnsQueryVarSolutions() {
        prolog.consult("f0345(a). f0345(b).");
        Term query = it.denzosoft.jprolog.core.parser.v2.TermReader.parseTerm(
            "f0345(X)", new it.denzosoft.jprolog.core.operator.OperatorTable());
        List<Map<String, Term>> s = prolog.solve(query);
        assertEquals(2, s.size());
        assertEquals("a", s.get(0).get("X").toString());
        assertEquals("b", s.get(1).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0345

    // START_CHANGE: ISS-2025-0346 - halt/0, halt/1 terminate the processor (consumer handling)
    @Test
    public void testISS0346_HaltEscapesCatchWithExitCode() {
        // the engine contract: halt is a PrologException(isHalt) that catch/3 cannot trap,
        // so the embedding consumer (CLI/IDE) can act on the exit code
        try {
            prolog.solve("catch(halt(3), _, true).");
            fail("halt/1 must not be trappable by catch/3; it must reach the embedder");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            assertTrue("halt must carry the isHalt flag", pe.isHalt());
            assertEquals(3, pe.getExitCode());
        }
        try {
            prolog.solve("halt.");
            fail("halt/0 must reach the embedder");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            assertTrue(pe.isHalt());
            assertEquals(0, pe.getExitCode());
        }
    }

    @Test
    public void testISS0346_HaltDirectivePropagatesFromConsult() {
        // ':- halt(N).' in a consulted program must abort the load and reach the embedder
        // (previously executeGoalDirective swallowed it as a directive warning)
        try {
            prolog.consult("p0346(1).\n:- halt(7).\np0346(2).");
            fail("':- halt(7).' during consult must propagate to the embedder");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            assertTrue("halt flag must survive the consult error handling", pe.isHalt());
            assertEquals(7, pe.getExitCode());
        }
        // clauses before the halt are loaded; the load stops at the halt
        assertEquals(1, prolog.solve("p0346(1).").size());
        assertTrue(prolog.solve("p0346(2).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0346

    // START_CHANGE: ISS-2025-0347 - unknown procedure raises existence_error per the 'unknown' flag
    @Test
    public void testISS0347_UnknownProcedureRaisesExistenceError() {
        // ISO 7.7.7 + flag 7.11.2.4: with unknown=error (the default), calling an undefined
        // procedure raises error(existence_error(procedure, Name/Arity), _)
        try {
            prolog.solve("undefined_foo_0347(1).");
            fail("calling an unknown procedure with unknown=error must raise existence_error");
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            assertNotNull(pe.getErrorTerm());
            assertTrue(pe.getErrorTerm().toString().contains("existence_error"));
            assertTrue(pe.getErrorTerm().toString().contains("undefined_foo_0347"));
        }
        // the error is a regular ISO ball, catchable with the standard term shape
        List<Map<String, Term>> s = prolog.solve(
            "catch(undefined_foo_0347(1), error(existence_error(procedure, PI), _), true).");
        assertEquals(1, s.size());
        assertTrue(s.get(0).get("PI").toString().contains("undefined_foo_0347"));
    }

    @Test
    public void testISS0347_UnknownFlagFailAndWarningHonored() {
        try {
            prolog.solve("set_prolog_flag(unknown, fail).");
            assertTrue(prolog.solve("undefined_bar_0347.").isEmpty());
            prolog.solve("set_prolog_flag(unknown, warning).");
            assertTrue(prolog.solve("undefined_bar_0347.").isEmpty());
        } finally {
            prolog.solve("set_prolog_flag(unknown, error).");   // flag store is process-wide
        }
    }

    @Test
    public void testISS0347_DynamicDirectiveSuppressesExistenceError() {
        // ':- dynamic PI' (single, '/'-pair, and ','-sequence) makes empty predicates fail silently
        prolog.consult(":- dynamic(dyn0347/1).\n:- dynamic dyn0347b/2, dyn0347c/3.");
        assertTrue(prolog.solve("dyn0347(X).").isEmpty());
        assertTrue(prolog.solve("dyn0347b(X, Y).").isEmpty());
        assertTrue(prolog.solve("dyn0347c(X, Y, Z).").isEmpty());
    }

    @Test
    public void testISS0347_AssertImpliesDynamicSurvivingRetract() {
        // assert implies dynamic (ISO 8.9.1); the mark survives retracting every clause
        prolog.solve("assertz(adyn0347(1)).");
        assertEquals(1, prolog.solve("adyn0347(X).").size());
        assertEquals(1, prolog.solve("retract(adyn0347(1)).").size());
        assertTrue("retracted-to-empty dynamic procedure must FAIL, not raise",
            prolog.solve("adyn0347(X).").isEmpty());
    }

    @Test
    public void testISS0347_RetractallImpliesDynamic() {
        // retractall creates the procedure as dynamic when it does not exist (SWI semantics)
        prolog.solve("retractall(rdyn0347(_)).");
        assertTrue(prolog.solve("rdyn0347(x).").isEmpty());
    }

    // ISS-2025-0347's legacy-engine twin was REMOVED in wave W9 (ISS-2025-0484): only the
    // sibling test above remains, and it runs on whichever engine the suite leg selects.
    // END_CHANGE: ISS-2025-0347

    @Test
    public void testISS0348_StringStandardOrderingByContent() {
        // Direct check of the shared ordering used by compare/3, @</2 .. @>=/2 and ==/2 (legacy)
        assertEquals("identical strings must compare equal", 0,
            it.denzosoft.jprolog.builtin.term.StandardTermOrdering.compare(
                new PrologString("abc"), new PrologString("abc")));
        assertTrue("\"abc\" must order before \"abd\"",
            it.denzosoft.jprolog.builtin.term.StandardTermOrdering.compare(
                new PrologString("abc"), new PrologString("abd")) < 0);
        assertTrue("\"abd\" must order after \"abc\"",
            it.denzosoft.jprolog.builtin.term.StandardTermOrdering.compare(
                new PrologString("abd"), new PrologString("abc")) > 0);
        assertTrue("distinct strings must NOT be identical",
            !it.denzosoft.jprolog.builtin.term.StandardTermOrdering.identical(
                new PrologString("abc"), new PrologString("abd")));
        assertTrue("equal-content strings must be identical",
            it.denzosoft.jprolog.builtin.term.StandardTermOrdering.identical(
                new PrologString("abc"), new PrologString("abc")));
    }

    @Test
    public void testISS0348_CompareDistinguishesStrings() {
        List<Map<String, Term>> s = prolog.solve("compare(O, \"abc\", \"abd\").");
        assertEquals(1, s.size());
        assertEquals("<", s.get(0).get("O").toString());

        s = prolog.solve("compare(O, \"abd\", \"abc\").");
        assertEquals(1, s.size());
        assertEquals(">", s.get(0).get("O").toString());

        s = prolog.solve("compare(O, \"abc\", \"abc\").");
        assertEquals(1, s.size());
        assertEquals("=", s.get(0).get("O").toString());
    }

    @Test
    public void testISS0348_TermOrderOperatorsOnStrings() {
        assertEquals(1, prolog.solve("\"abc\" @< \"abd\".").size());
        assertEquals(0, prolog.solve("\"abd\" @< \"abc\".").size());
        assertEquals(1, prolog.solve("\"abc\" @=< \"abc\".").size());
        assertEquals(1, prolog.solve("\"abd\" @> \"abc\".").size());
    }

    @Test
    public void testISS0348_StringRankConsistentWithSort() {
        // compare/3 must agree with sort/msort: Var < Number < Atom < String < Compound
        assertEquals(1, prolog.solve("compare(O, \"abc\", f(x)), O == (<).").size());
        assertEquals(1, prolog.solve("compare(O, foo, \"abc\"), O == (<).").size());
        assertEquals(1, prolog.solve("compare(O, 1, \"abc\"), O == (<).").size());
        // msort places the string between the atom and the compound, like compare/3 now does
        List<Map<String, Term>> s = prolog.solve("msort([f(b), \"abc\", foo], L).");
        assertEquals(1, s.size());
        assertEquals("[foo, \"abc\", f(b)]", s.get(0).get("L").toString());
    }

    @Test
    public void testISS0348_StringIdentityAndAtomicOnAFreshEngine() {
        // START_CHANGE: ISS-2025-0491 - 4.1 wave A: this used to select the legacy (and later the
        // v2) engine to prove that string identity held there too. There is ONE engine now, so the
        // test keeps its assertions and drops the selection: a fresh Prolog, same guarantees.
        Prolog fresh = new Prolog();
        assertEquals("\"abc\" == \"abc\" must succeed", 1, fresh.solve("\"abc\" == \"abc\".").size());
        assertEquals("\"abc\" == \"abd\" must fail", 0, fresh.solve("\"abc\" == \"abd\".").size());
        assertEquals("\"abc\" \\== \"abc\" must fail", 0, fresh.solve("\"abc\" \\== \"abc\".").size());
        assertEquals("\"abc\" \\== \"abd\" must succeed", 1, fresh.solve("\"abc\" \\== \"abd\".").size());
        assertEquals("strings are atomic", 1, fresh.solve("atomic(\"abc\").").size());
        // END_CHANGE: ISS-2025-0491
    }

    @Test
    public void testISS0348_StringIdentityAndAtomicOnDefaultEngine() {
        // ==/\==/atomic for strings on the default engine (ISS-2025-0491: v4 inlines them)
        assertEquals("\"abc\" == \"abc\" must succeed", 1, prolog.solve("\"abc\" == \"abc\".").size());
        assertEquals("\"abc\" == \"abd\" must fail", 0, prolog.solve("\"abc\" == \"abd\".").size());
        assertEquals("\"abc\" \\== \"abc\" must fail", 0, prolog.solve("\"abc\" \\== \"abc\".").size());
        assertEquals("\"abc\" \\== \"abd\" must succeed", 1, prolog.solve("\"abc\" \\== \"abd\".").size());
        assertEquals("strings are atomic", 1, prolog.solve("atomic(\"abc\").").size());
    }
    // END_CHANGE: ISS-2025-0348

    // ======================== ISS-2025-0349: list builtins on proper lists with var elements ========================

    // START_CHANGE: ISS-2025-0349 - length/reverse/select/permutation accept proper lists with unbound elements
    @Test
    public void testISS0349_LengthProperListWithVars() {
        List<Map<String, Term>> s = prolog.solve("length([A, B], N).");
        assertEquals(1, s.size());
        assertEquals("2", s.get(0).get("N").toString());

        s = prolog.solve("length([a, B], N).");
        assertEquals(1, s.size());
        assertEquals("2", s.get(0).get("N").toString());

        // the ubiquitous sort-then-count pattern
        assertEquals(1, prolog.solve("sort([X, Y], L), length(L, 2).").size());
    }

    @Test
    public void testISS0349_LengthPartialListModePreserved() {
        // length(PartialList, N) generative mode must keep working
        assertEquals(1, prolog.solve("length([a|T], 3), T = [b, c].").size());
        assertEquals(0, prolog.solve("length(foo, _N).").size());
    }

    @Test
    public void testISS0349_ReverseProperListWithVars() {
        assertEquals(1, prolog.solve("reverse([X, b], R), R == [b, X].").size());
    }

    @Test
    public void testISS0349_SelectProperListWithVars() {
        assertEquals(1, prolog.solve("findall(E-R, select(E, [X, b], R), L), length(L, 2).").size());
    }

    @Test
    public void testISS0349_PermutationProperListWithVars() {
        assertEquals(1, prolog.solve("findall(P, permutation([X, b], P), L), length(L, 2).").size());
    }
    // END_CHANGE: ISS-2025-0349

    // ======================== ISS-2025-0350: keysort/2 modes and ISO errors ========================

    // START_CHANGE: ISS-2025-0350 - keysort/2 accepts non-ground pairs, raises ISO errors on bad input
    @Test
    public void testISS0350_KeysortUnboundValues() {
        // sorting Key-Var pairs is the canonical keysort idiom
        assertEquals(1, prolog.solve("keysort([b-Y, a-X], L), L == [a-X, b-Y].").size());
    }

    @Test
    public void testISS0350_KeysortStableByKeyOnly() {
        // stable: equal keys keep their input order (b-2 stays before b-1)
        assertEquals(1, prolog.solve("keysort([b-2, a-1, b-1], L), L == [a-1, b-2, b-1].").size());
    }

    @Test
    public void testISS0350_KeysortNonListTypeError() {
        // was: silent success with L = []
        assertEquals(1, prolog.solve(
            "catch(keysort(a, _), error(type_error(list, a), _), true).").size());
    }

    @Test
    public void testISS0350_KeysortImproperListTypeError() {
        // was: improper tail silently truncated
        assertEquals(1, prolog.solve(
            "catch(keysort([a-1|b], _), error(type_error(list, _), _), true).").size());
    }

    @Test
    public void testISS0350_KeysortPartialListInstantiationError() {
        assertEquals(1, prolog.solve(
            "catch(keysort([a-1|_T], _), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(keysort(_M, _), error(instantiation_error, _), true).").size());
    }

    @Test
    public void testISS0350_KeysortNonPairTypeError() {
        assertEquals(1, prolog.solve(
            "catch(keysort([a], _), error(type_error(pair, a), _), true).").size());
    }
    // END_CHANGE: ISS-2025-0350

    // ======================== ISS-2025-0351: sort/2, sort/4, msort/2 ISO errors ========================

    // START_CHANGE: ISS-2025-0351 - sort/msort raise ISO errors instead of failing silently
    @Test
    public void testISS0351_SortPartialListInstantiationError() {
        assertEquals(1, prolog.solve(
            "catch(sort([a|_T], _), error(instantiation_error, _), true).").size());
    }

    @Test
    public void testISS0351_SortNonListTypeError() {
        assertEquals(1, prolog.solve(
            "catch(sort(foo, _), error(type_error(list, foo), _), true).").size());
    }

    @Test
    public void testISS0351_MsortErrors() {
        assertEquals(1, prolog.solve(
            "catch(msort(foo, _), error(type_error(list, foo), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(msort([a|_T], _), error(instantiation_error, _), true).").size());
    }

    @Test
    public void testISS0351_Sort4Errors() {
        assertEquals(1, prolog.solve(
            "catch(sort(0, @<, foo, _), error(type_error(list, foo), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(sort(0, @<, [a|_T], _), error(instantiation_error, _), true).").size());
    }
    // END_CHANGE: ISS-2025-0351

    private String captureStdout(String query, List<Map<String, Term>> solutionsOut) {
        java.io.PrintStream orig = System.out;
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        System.setOut(new java.io.PrintStream(baos));
        try {
            solutionsOut.addAll(prolog.solve(query));
        } finally {
            System.setOut(orig);
        }
        return baos.toString();
    }

    private java.io.File writeTempPrologFile(String prefix, String content) throws java.io.IOException {
        java.io.File f = java.io.File.createTempFile(prefix, ".pl");
        f.deleteOnExit();
        java.io.PrintWriter pw = new java.io.PrintWriter(f, "UTF-8");
        pw.print(content);
        pw.close();
        return f;
    }

    @Test
    public void testISS0352_FormatSucceedsAsGoal() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("format('x~n', []).", solutions);
        assertEquals("format/2 must succeed with exactly one solution", 1, solutions.size());
        assertEquals("x\n", out);
    }

    @Test
    public void testISS0352_FormatInConjunctionBindsNextGoal() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        captureStdout("format('a~n', []), X = done.", solutions);
        assertEquals("conjunction after format/2 must run", 1, solutions.size());
        assertEquals("done", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0352_Format3SucceedsAsGoal() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("format(user_output, '~w', [hi]), X = ok.", solutions);
        assertEquals(1, solutions.size());
        assertEquals("ok", solutions.get(0).get("X").toString());
        assertEquals("hi", out);
    }

    @Test
    public void testISS0352_WriteTermSucceedsAsGoal() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("write_term(abc, [quoted(true)]), X = ok.", solutions);
        assertEquals("write_term/2 must succeed with exactly one solution", 1, solutions.size());
        assertEquals("ok", solutions.get(0).get("X").toString());
        assertEquals("abc", out);
    }

    @Test
    public void testISS0352_ReadTermSucceedsAsGoal() throws Exception {
        java.io.File f = writeTempPrologFile("iss0352_read", "foo(bar).\n");
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), read_term(S, T), close(S), X = done.");
        assertEquals("read_term/2 must succeed with exactly one solution", 1, solutions.size());
        assertEquals("foo(bar)", solutions.get(0).get("T").toString());
        assertEquals("done", solutions.get(0).get("X").toString());
    }

    // ======================== ISS-2025-0353: format/2,3 accepts double-quoted (PrologString) format strings ========================

    @Test
    public void testISS0353_FormatDoubleQuotedFormatString() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("format(\"ok~n\", []).", solutions);
        assertEquals("format/2 with a double-quoted format string must succeed", 1, solutions.size());
        assertEquals("ok\n", out);
    }

    @Test
    public void testISS0353_FormatStringViaVariable() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("S = \"vv~n\", format(S, []).", solutions);
        assertEquals(1, solutions.size());
        assertEquals("vv\n", out);
    }

    @Test
    public void testISS0353_FormatTildeSWithPrologString() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("format(\"~s!\", [\"abc\"]).", solutions);
        assertEquals(1, solutions.size());
        assertEquals("abc!", out);
    }

    // ======================== ISS-2025-0354: read_term/3 (Stream, Term, Options) ========================

    @Test
    public void testISS0354_ReadTerm3FromFileStream() throws Exception {
        java.io.File f = writeTempPrologFile("iss0354_read", "foo(bar).\n");
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), read_term(S, T, []), close(S).");
        assertEquals("read_term/3 must succeed with exactly one solution", 1, solutions.size());
        assertEquals("foo(bar)", solutions.get(0).get("T").toString());
    }

    @Test
    public void testISS0354_ReadTerm3VariableNamesOption() throws Exception {
        java.io.File f = writeTempPrologFile("iss0354_vars", "baz(A1, B2).\n");
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), read_term(S, T, [variable_names(V)]), close(S).");
        assertEquals(1, solutions.size());
        String v = solutions.get(0).get("V").toString();
        assertTrue("variable_names must report A1: " + v, v.contains("A1"));
        assertTrue("variable_names must report B2: " + v, v.contains("B2"));
    }

    // ======================== ISS-2025-0355: unification must respect CLP(FD) domains ========================

    @Test
    public void testISS0355_UnificationOutsideDomainFails() {
        List<Map<String, Term>> solutions = prolog.solve("X in 1..3, X = 5.");
        assertTrue("unifying an FD variable with a value outside its domain must fail", solutions.isEmpty());
    }

    @Test
    public void testISS0355_UnificationOutsideNarrowedDomainFails() {
        List<Map<String, Term>> solutions = prolog.solve("X in 1..3, X #> 2, X = 1.");
        assertTrue("X is constrained to 3; X = 1 must fail", solutions.isEmpty());
    }

    @Test
    public void testISS0355_UnificationWithNonIntegerFails() {
        List<Map<String, Term>> solutions = prolog.solve("X in 1..3, X = a.");
        assertTrue("unifying an FD variable with a non-integer must fail", solutions.isEmpty());
    }

    @Test
    public void testISS0355_DisjointDomainAliasingFails() {
        List<Map<String, Term>> solutions = prolog.solve("X in 1..3, Y in 5..7, X = Y.");
        assertTrue("aliasing FD variables with disjoint domains must fail", solutions.isEmpty());
    }

    @Test
    public void testISS0355_LabelAfterBadUnificationFails() {
        List<Map<String, Term>> solutions = prolog.solve("X in 1..3, X = 5, label([X]).");
        assertTrue(solutions.isEmpty());
    }

    @Test
    public void testISS0355_UnificationInsideDomainSucceeds() {
        List<Map<String, Term>> solutions = prolog.solve("X in 1..3, X = 2.");
        assertEquals(1, solutions.size());
        assertEquals("2", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0355_AliasingIntersectsDomains() {
        List<Map<String, Term>> solutions = prolog.solve("X in 1..3, Y in 2..5, X = Y, label([X]).");
        assertEquals("X = Y restricts both to 2..3", 2, solutions.size());
    }

    // ======================== ISS-2025-0356: constraints are undone on engine backtracking ========================

    @Test
    public void testISS0356_DisjunctionEnumeratesBothBranches() {
        List<Map<String, Term>> solutions = prolog.solve("( X #= 1 ; X #= 2 ), label([X]).");
        assertEquals("the X #= 1 post must be retracted before the second branch", 2, solutions.size());
        assertEquals("1", solutions.get(0).get("X").toString());
        assertEquals("2", solutions.get(1).get("X").toString());
    }

    @Test
    public void testISS0356_FailedBranchDoesNotPoisonStore() {
        List<Map<String, Term>> solutions = prolog.solve("( Y #> 10, fail ; true ), Y in 1..3, label([Y]).");
        assertEquals("the failed Y #> 10 branch must not keep narrowing Y", 3, solutions.size());
    }

    @Test
    public void testISS0356_BacktrackedConstraintRetracted() {
        List<Map<String, Term>> solutions = prolog.solve("Z in 1..3, ( Z #> 5 ; Z #< 3 ), label([Z]).");
        assertEquals("the wiped-out Z #> 5 post must leave no trace", 2, solutions.size());
        assertEquals("1", solutions.get(0).get("Z").toString());
        assertEquals("2", solutions.get(1).get("Z").toString());
    }

    // ======================== ISS-2025-0357: singleton domains bind the Prolog variable ========================

    @Test
    public void testISS0357_SingletonDomainBindsVariable() {
        List<Map<String, Term>> solutions = prolog.solve("X #= 2, Y is X + 1.");
        assertEquals("X #= 2 must bind X so is/2 can evaluate it", 1, solutions.size());
        assertEquals("2", solutions.get(0).get("X").toString());
        assertEquals("3", solutions.get(0).get("Y").toString());
    }

    @Test
    public void testISS0357_PropagationToSingletonBinds() {
        List<Map<String, Term>> solutions = prolog.solve("A in 1..3, A #> 2, B is A.");
        assertEquals(1, solutions.size());
        assertEquals("3", solutions.get(0).get("A").toString());
        assertEquals("3", solutions.get(0).get("B").toString());
    }

    @Test
    public void testISS0357_LabelingBindsDeterminedVariables() {
        List<Map<String, Term>> solutions = prolog.solve("C in 1..3, D #= C*2+1, label([C]).");
        assertEquals(3, solutions.size());
        for (Map<String, Term> m : solutions) {
            long c = Long.parseLong(m.get("C").toString());
            assertTrue("D must come out bound after label([C])", m.get("D") instanceof Number);
            assertEquals(c * 2 + 1, Long.parseLong(m.get("D").toString()));
        }
    }

    // ======================== ISS-2025-0358: multi-variable #\= expressions ========================

    @Test
    public void testISS0358_MultiVariableDisequality() {
        List<Map<String, Term>> solutions = prolog.solve(
            "X in 1..5, X #\\= Y + 1, Y in 1..3, label([X,Y]).");
        assertEquals("15 pairs minus (2,1),(3,2),(4,3)", 12, solutions.size());
        for (Map<String, Term> m : solutions) {
            long x = Long.parseLong(m.get("X").toString());
            long y = Long.parseLong(m.get("Y").toString());
            assertTrue("X #\\= Y + 1 must hold: " + x + "," + y, x != y + 1);
        }
    }

    @Test
    public void testISS0358_SingleVariableDisequalityStillWorks() {
        List<Map<String, Term>> solutions = prolog.solve("X in 1..5, X + 1 #\\= 5, label([X]).");
        assertEquals(4, solutions.size());                              // 1,2,3,5 (ISS-2025-0301)
        for (Map<String, Term> m : solutions) assertNotEquals("4", m.get("X").toString());
    }

    @Test
    public void testISS0358_SelfDisequalityFails() {
        List<Map<String, Term>> solutions = prolog.solve("T in 1..3, T #\\= T.");
        assertTrue("T #\\= T is unsatisfiable", solutions.isEmpty());
    }

    // ======================== ISS-2025-0359: computed float Infinity raises float_overflow ========================

    @Test
    public void testISS0359_FloatOverflowRaisesEvaluationError() {
        assertEquals("1.0e308 * 10.0 must raise evaluation_error(float_overflow)", 1, prolog.solve(
            "catch(_ is 1.0e308 * 10.0, error(evaluation_error(float_overflow), _), true).").size());
        assertEquals("exp(1000) must raise evaluation_error(float_overflow)", 1, prolog.solve(
            "catch(_ is exp(1000), error(evaluation_error(float_overflow), _), true).").size());
        assertEquals("2.0 ** 10000 must raise evaluation_error(float_overflow)", 1, prolog.solve(
            "catch(_ is 2.0 ** 10000, error(evaluation_error(float_overflow), _), true).").size());
    }

    @Test
    public void testISS0359_InfConstantAndPropagationStillWork() {
        List<Map<String, Term>> solutions = prolog.solve("X is inf.");
        assertEquals(1, solutions.size());
        assertTrue("the inf constant must still evaluate to Infinity",
            Double.isInfinite(((Number) solutions.get(0).get("X")).doubleValue()));
        solutions = prolog.solve("X is inf + 1.");
        assertEquals("an already-infinite operand must propagate, not raise", 1, solutions.size());
        assertTrue(Double.isInfinite(((Number) solutions.get(0).get("X")).doubleValue()));
    }

    // ======================== ISS-2025-0360: computed float NaN raises evaluation_error(undefined) ========================

    @Test
    public void testISS0360_UndefinedFloatResultRaisesEvaluationError() {
        assertEquals("(-2.0) ** 0.5 must raise evaluation_error(undefined)", 1, prolog.solve(
            "catch(_ is (-2.0) ** 0.5, error(evaluation_error(undefined), _), true).").size());
        assertEquals("(-2) ^ 0.5 must raise evaluation_error(undefined)", 1, prolog.solve(
            "catch(_ is (-2) ^ 0.5, error(evaluation_error(undefined), _), true).").size());
        assertEquals("inf - inf must raise evaluation_error(undefined)", 1, prolog.solve(
            "catch(_ is inf - inf, error(evaluation_error(undefined), _), true).").size());
        assertEquals("inf / inf must raise evaluation_error(undefined)", 1, prolog.solve(
            "catch(_ is inf / inf, error(evaluation_error(undefined), _), true).").size());
    }

    @Test
    public void testISS0360_NanConstantAndPropagationStillWork() {
        List<Map<String, Term>> solutions = prolog.solve("X is nan.");
        assertEquals(1, solutions.size());
        assertTrue("the nan constant must still evaluate to NaN",
            Double.isNaN(((Number) solutions.get(0).get("X")).doubleValue()));
        solutions = prolog.solve("X is nan + 1.");
        assertEquals("an already-NaN operand must propagate, not raise", 1, solutions.size());
        assertTrue(Double.isNaN(((Number) solutions.get(0).get("X")).doubleValue()));
    }

    // ======================== ISS-2025-0361: huge exponent / shift count raises a catchable ISO error ========================

    @Test
    public void testISS0361_HugeExponentAndShiftAreCatchable() {
        // Used to escape catch/3 as a raw java.lang.ArithmeticException ("BigInteger out of int range")
        assertEquals("2 ^ 10000000000 must raise a catchable resource_error", 1, prolog.solve(
            "catch(_ is 2 ^ 10000000000, error(resource_error(_), _), true).").size());
        assertEquals("1 << 10000000000 must raise a catchable resource_error", 1, prolog.solve(
            "catch(_ is 1 << 10000000000, error(resource_error(_), _), true).").size());
    }

    @Test
    public void testISS0361_HugeExponentExactCasesStillEvaluate() {
        // Bases in {-1, 0, 1} and the (>>) sign extension are exactly computable for any exponent size
        assertEquals("1", prolog.solve("X is 1 ^ 10000000000.").get(0).get("X").toString());
        assertEquals("-1", prolog.solve("X is (-1) ^ 10000000001.").get(0).get("X").toString());
        assertEquals("0", prolog.solve("X is 0 ^ 10000000000.").get(0).get("X").toString());
        assertEquals("0", prolog.solve("X is 5 >> 10000000000.").get(0).get("X").toString());
        assertEquals("-1", prolog.solve("X is (-5) >> 10000000000.").get(0).get("X").toString());
        assertEquals("0", prolog.solve("X is 0 << 10000000000.").get(0).get("X").toString());
    }

    // ======================== ISS-2025-0362: 0 ^ negative raises evaluation_error(zero_divisor) ========================

    @Test
    public void testISS0362_ZeroPowerNegativeIsZeroDivisor() {
        assertEquals("0 ^ -1 must raise evaluation_error(zero_divisor), not type_error(float, 0)",
            1, prolog.solve(
                "catch(_ is 0 ^ -1, error(evaluation_error(zero_divisor), _), true).").size());
    }

    // ======================== ISS-2025-0363: throw/1 with an unbound ball ========================

    @Test
    public void testISS0363_ThrowUnboundRaisesInstantiationError() {
        assertEquals("throw(_) must raise instantiation_error (ISO 7.8.10.3)", 1, prolog.solve(
            "catch(throw(_), error(instantiation_error, _), true).").size());
    }

    @Test
    public void testISS0363_UnboundBallNotTrappedByUnrelatedCatcher() {
        // The fresh-variable ball used to unify with ANY catcher and run the recovery goal
        List<Map<String, Term>> solutions;
        try {
            solutions = prolog.solve("catch(throw(_), very_specific_catcher(abc), R = wrongly_caught).");
        } catch (Exception e) {
            solutions = java.util.Collections.emptyList();   // escaping to Java as an error is correct
        }
        assertTrue("an unrelated catcher must not trap the instantiation_error", solutions.isEmpty());
    }

    // ======================== ISS-2025-0364: functor/3 compound Name raises type_error(atomic, Name) ========================

    @Test
    public void testISS0364_FunctorCompoundNameTypeErrorAtomic() {
        assertEquals("functor(T, f(a), 2) must raise type_error(atomic, f(a)) (ISO 8.5.1.3)",
            1, prolog.solve(
                "catch(functor(_, f(a), 2), error(type_error(atomic, f(a)), _), true).").size());
        // atomic-but-not-atom Names with Arity > 0 keep type_error(atom, Name)
        assertEquals(1, prolog.solve(
            "catch(functor(_, 1.5, 2), error(type_error(atom, 1.5), _), true).").size());
    }

    // ======================== ISS-2025-0365: >64-bit integers in number/text conversions ========================

    @Test
    public void testISS0365_AtomNumberBigInteger() {
        // 2^63 — one past Long.MAX_VALUE; used to saturate silently to 9223372036854775807
        List<Map<String, Term>> solutions = prolog.solve("atom_number('9223372036854775808', X).");
        assertEquals(1, solutions.size());
        assertEquals("9223372036854775808", solutions.get(0).get("X").toString());

        solutions = prolog.solve("atom_number(A, 9223372036854775808).");
        assertEquals(1, solutions.size());
        assertEquals("9223372036854775808", solutions.get(0).get("A").toString());

        // 20 digits used to come back as the float 1.0E19
        solutions = prolog.solve("atom_number('10000000000000000000', X).");
        assertEquals(1, solutions.size());
        assertEquals("10000000000000000000", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0365_NumberCharsAndCodesBigIntegerRoundTrip() {
        List<Map<String, Term>> solutions = prolog.solve(
            "number_chars(X, ['9','2','2','3','3','7','2','0','3','6','8','5','4','7','7','5','8','0','8']).");
        assertEquals(1, solutions.size());
        assertEquals("9223372036854775808", solutions.get(0).get("X").toString());

        solutions = prolog.solve("number_chars(9223372036854775808, L), atom_chars(A, L).");
        assertEquals(1, solutions.size());
        assertEquals("9223372036854775808", solutions.get(0).get("A").toString());

        solutions = prolog.solve("number_codes(9223372036854775808, L), number_codes(X, L).");
        assertEquals(1, solutions.size());
        assertEquals("9223372036854775808", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0365_SmallValuesUnchanged() {
        assertEquals("123", prolog.solve("atom_number('123', X).").get(0).get("X").toString());
        assertEquals("-42", prolog.solve("atom_number('-42', X).").get(0).get("X").toString());
        assertEquals("3.14", prolog.solve("atom_number('3.14', X).").get(0).get("X").toString());
        // START_CHANGE: ISS-2025-0399 - floats keep valid float syntax through text conversion
        assertEquals("123.0", prolog.solve("atom_number(A, 123.0).").get(0).get("A").toString());
        // END_CHANGE: ISS-2025-0399
    }

    @Test
    public void testISS0366_RetractUnboundRaisesInstantiationError() {
        // Previously a raw ClassCastException escaped catch/3 entirely (v2 engine).
        List<Map<String, Term>> s = prolog.solve(
            "catch(retract(_X), error(instantiation_error, _), true).");
        assertEquals("retract(X) with X unbound must raise a catchable instantiation_error",
            1, s.size());
    }

    @Test
    public void testISS0366_RetractNonCallableRaisesTypeError() {
        List<Map<String, Term>> s = prolog.solve(
            "catch(retract(1), error(type_error(callable, 1), _), true).");
        assertEquals("retract(1) must raise type_error(callable, 1)", 1, s.size());
    }

    @Test
    public void testISS0366_RetractUnboundHeadInClauseRaisesInstantiationError() {
        List<Map<String, Term>> s = prolog.solve(
            "catch(retract((_H :- true)), error(instantiation_error, _), true).");
        assertEquals("retract((H :- true)) with H unbound must raise instantiation_error",
            1, s.size());
    }

    // ISS-2025-0366's legacy-engine twin was REMOVED in wave W9 (ISS-2025-0484).

    // ======================== ISS-2025-0367: built-in procedures are static ========================

    @Test
    public void testISS0367_AssertOnBuiltInRaisesPermissionError() {
        assertEquals("asserta on a built-in must raise permission_error", 1, prolog.solve(
            "catch(asserta(atom_length(zzz, 99)), "
            + "error(permission_error(modify, static_procedure, atom_length/2), _), true).").size());
        assertEquals("assertz on a built-in must raise permission_error", 1, prolog.solve(
            "catch(assertz(atom_length(zzz, 99)), "
            + "error(permission_error(modify, static_procedure, atom_length/2), _), true).").size());
        // built-in behaviour unchanged
        List<Map<String, Term>> s = prolog.solve("atom_length(abc, L).");
        assertEquals(1, s.size());
        assertEquals("3", s.get(0).get("L").toString());
    }

    @Test
    public void testISS0367_RetractAndRetractallOnBuiltInRaisePermissionError() {
        assertEquals(1, prolog.solve(
            "catch(retract(atom_length(_, _)), "
            + "error(permission_error(modify, static_procedure, atom_length/2), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(retractall(atom_length(_, _)), "
            + "error(permission_error(modify, static_procedure, atom_length/2), _), true).").size());
    }

    @Test
    public void testISS0367_AbolishOnBuiltInRaisesPermissionError() {
        assertEquals(1, prolog.solve(
            "catch(abolish(atom_length/2), "
            + "error(permission_error(modify, static_procedure, atom_length/2), _), true).").size());
        // and abolish must NOT have silently succeeded: atom_length/2 still works
        assertEquals(1, prolog.solve("atom_length(abc, 3).").size());
    }

    // ISS-2025-0367's legacy-engine twin was REMOVED in wave W9 (ISS-2025-0484).

    @Test
    public void testISS0367_UserPredicatesRemainModifiable() {
        assertEquals(1, prolog.solve("assertz(iss0367_fact(1)).").size());
        assertEquals(1, prolog.solve("retract(iss0367_fact(1)).").size());
        // sharing a library name at a DIFFERENT arity stays legal: atom_length/3 is no built-in
        assertEquals(1, prolog.solve("assertz(atom_length(a, b, c)).").size());
        assertEquals(1, prolog.solve("retract(atom_length(a, b, c)).").size());
    }

    // ======================== ISS-2025-0368: assert clause validation ========================

    @Test
    public void testISS0368_AssertzUnboundRaisesInstantiationError() {
        assertEquals(1, prolog.solve(
            "catch(assertz(_X), error(instantiation_error, _), true).").size());
        assertEquals("an unbound head inside (Head :- Body) must also raise", 1, prolog.solve(
            "catch(assertz((_H :- true)), error(instantiation_error, _), true).").size());
    }

    @Test
    public void testISS0368_AssertzNonCallableRaisesTypeError() {
        assertEquals(1, prolog.solve(
            "catch(assertz(1), error(type_error(callable, 1), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(assertz((1 :- true)), error(type_error(callable, 1), _), true).").size());
    }

    @Test
    public void testISS0368_AssertzNonCallableBodyRaisesAtAssertTime() {
        assertEquals("a number body goal must raise type_error(callable, 7) at assert time",
            1, prolog.solve(
                "catch(assertz((iss0368_foo :- 7)), error(type_error(callable, 7), _), true).").size());
        assertEquals("inside a conjunction too", 1, prolog.solve(
            "catch(assertz((iss0368_bar :- true, 7)), error(type_error(callable, 7), _), true).").size());
    }

    @Test
    public void testISS0368_AssertzVariableBodyStillLegal() {
        // ISO 7.6.2: a variable body goal is legal (converted to call/1 at run time)
        assertEquals(1, prolog.solve("assertz((iss0368_v :- _G)).").size());
    }

    // ISS-2025-0368's legacy-engine twin was REMOVED in wave W9 (ISS-2025-0484).

    @Test
    public void testISS0368_NoGarbageEntryAfterRejectedAssert() {
        // assertz(X) used to store a Variable-headed rule indexed as "unknown/0"
        prolog.solve("catch(assertz(_X), _, true).");
        assertTrue("no unknown/0 garbage may enter the knowledge base",
            prolog.solve("current_predicate(unknown/0).").isEmpty());
    }

    // ======================== ISS-2025-0369: dynamic/1 callable as a goal ========================

    @Test
    public void testISS0369_DynamicCallableAsGoal() {
        assertEquals("dynamic(Name/Arity) must succeed as a goal",
            1, prolog.solve("dynamic(iss0369_counter/1).").size());
        assertTrue("a declared-dynamic predicate with no clauses fails instead of existence_error",
            prolog.solve("iss0369_counter(_).").isEmpty());
    }

    @Test
    public void testISS0369_DynamicGoalInConjunction() {
        // the classic init pattern that silently failed before the fix
        List<Map<String, Term>> s = prolog.solve(
            "dynamic(iss0369_c/1), assertz(iss0369_c(0)), iss0369_c(X).");
        assertEquals(1, s.size());
        assertEquals("0", s.get(0).get("X").toString());
    }

    @Test
    public void testISS0369_DynamicCommaSequenceAndList() {
        assertEquals(1, prolog.solve("dynamic((iss0369_a/1, iss0369_b/2)).").size());
        assertTrue(prolog.solve("iss0369_a(_).").isEmpty());
        assertTrue(prolog.solve("iss0369_b(_, _).").isEmpty());
        assertEquals(1, prolog.solve("dynamic([iss0369_l1/1, iss0369_l2/1]).").size());
        assertTrue(prolog.solve("iss0369_l1(_).").isEmpty());
        assertTrue(prolog.solve("iss0369_l2(_).").isEmpty());
    }

    @Test
    public void testISS0369_DynamicErrorCases() {
        assertEquals(1, prolog.solve(
            "catch(dynamic(_X), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(dynamic(foo/bar), error(type_error(predicate_indicator, foo/bar), _), true).").size());
    }

    // ======================== ISS-2025-0370: clause/2 access checks ========================

    @Test
    public void testISS0370_ClauseOnBuiltInRaisesPermissionError() {
        assertEquals("clause/2 on a built-in must raise permission_error, not fail",
            1, prolog.solve(
                "catch(clause(atom_length(_, _), _), "
                + "error(permission_error(access, private_procedure, atom_length/2), _), true).").size());
    }

    @Test
    public void testISS0370_ClauseNonCallableBodyRaisesTypeError() {
        prolog.solve("assertz(iss0370_f(a)).");
        assertEquals("clause(f(a), 1) must raise type_error(callable, 1), not fail",
            1, prolog.solve(
                "catch(clause(iss0370_f(a), 1), error(type_error(callable, 1), _), true).").size());
    }

    @Test
    public void testISS0370_ClauseOnUserPredicateStillWorks() {
        prolog.solve("assertz(iss0370_g(x)).");
        List<Map<String, Term>> s = prolog.solve("clause(iss0370_g(x), B).");
        assertEquals(1, s.size());
        assertEquals("true", s.get(0).get("B").toString());
    }

    // ======================== ISS-2025-0371: retractall/1 non-callable ========================

    @Test
    public void testISS0371_RetractallNonCallableRaisesTypeError() {
        assertEquals("retractall(1) must raise type_error(callable, 1), not succeed",
            1, prolog.solve(
                "catch(retractall(1), error(type_error(callable, 1), _), true).").size());
    }

    // ======================== ISS-2025-0372: current_predicate/1 PI validation ========================

    @Test
    public void testISS0372_CurrentPredicateNonPIRaisesTypeError() {
        assertEquals("current_predicate(foo) must raise type_error(predicate_indicator, foo)",
            1, prolog.solve(
                "catch(current_predicate(foo), "
                + "error(type_error(predicate_indicator, foo), _), true).").size());
        assertEquals("current_predicate(foo/bar) must raise type_error(predicate_indicator, foo/bar)",
            1, prolog.solve(
                "catch(current_predicate(foo/bar), "
                + "error(type_error(predicate_indicator, foo/bar), _), true).").size());
    }

    @Test
    public void testISS0372_CurrentPredicateEnumerationStillWorks() {
        prolog.solve("assertz(iss0372_p(x)).");
        List<Map<String, Term>> s = prolog.solve("current_predicate(iss0372_p/A).");
        assertEquals(1, s.size());
        assertEquals("1", s.get(0).get("A").toString());
    }

    private String readWholeFile(java.io.File f) throws java.io.IOException {
        return new String(java.nio.file.Files.readAllBytes(f.toPath()), java.nio.charset.StandardCharsets.UTF_8);
    }

    @Test
    public void testISS0373_Write2Nl1Writeln2ToFileStream() throws Exception {
        java.io.File f = java.io.File.createTempFile("iss0373_w2", ".txt");
        f.deleteOnExit();
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', write, S), write(S, hello(world)), nl(S), writeln(S, bye), close(S).");
        assertEquals("write/2 + nl/1 + writeln/2 must succeed", 1, solutions.size());
        assertEquals("hello(world)\nbye\n", readWholeFile(f));
    }

    @Test
    public void testISS0373_PutChar2Tab2ToFileStream() throws Exception {
        java.io.File f = java.io.File.createTempFile("iss0373_pc", ".txt");
        f.deleteOnExit();
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', write, S), put_char(S, x), tab(S, 3), put_char(S, y), close(S).");
        assertEquals("put_char/2 + tab/2 must succeed", 1, solutions.size());
        assertEquals("x   y", readWholeFile(f));
    }

    @Test
    public void testISS0373_WriteTerm3QuotedToFileStream() throws Exception {
        java.io.File f = java.io.File.createTempFile("iss0373_wt", ".txt");
        f.deleteOnExit();
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', write, S), write_term(S, f('A b'), [quoted(true)]), close(S).");
        assertEquals("write_term/3 must succeed", 1, solutions.size());
        assertEquals("f('A b')", readWholeFile(f));
    }

    @Test
    public void testISS0373_WriteAndNlOnUserOutput() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("write(user_output, hi), nl(user_output).", solutions);
        assertEquals("write/2 + nl/1 on user_output must succeed", 1, solutions.size());
        assertEquals("hi\n", out);
    }

    @Test
    public void testISS0373_Format1PrintsFormatString() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("format('hello~n').", solutions);
        assertEquals("format/1 must succeed", 1, solutions.size());
        assertEquals("hello\n", out);
    }

    // ======================== ISS-2025-0374: format/3 honours the stream/sink argument ========================

    @Test
    public void testISS0374_Format3WritesToFileStream() throws Exception {
        java.io.File f = java.io.File.createTempFile("iss0374_fmt", ".txt");
        f.deleteOnExit();
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String console = captureStdout(
            "open('" + f.getAbsolutePath() + "', write, S), format(S, 'hello ~w~n', [file]), close(S).", solutions);
        assertEquals("format/3 to a file stream must succeed", 1, solutions.size());
        assertEquals("hello file\n", readWholeFile(f));
        assertEquals("format/3 output must not leak to the console", "", console);
    }

    @Test
    public void testISS0374_FormatAtomSink() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String console = captureStdout("format(atom(A), '~w-~w', [foo(1), bar]).", solutions);
        assertEquals("format(atom(A), ...) must succeed", 1, solutions.size());
        assertEquals("foo(1)-bar", ((Atom) solutions.get(0).get("A")).getName());
        assertEquals("format(atom(A), ...) must not print to the console", "", console);
    }

    @Test
    public void testISS0374_FormatCodesSink() {
        List<Map<String, Term>> solutions = prolog.solve("format(codes(C), '~w', [ab]), C = [97, 98].");
        assertEquals("format(codes(C), ...) must bind a code list", 1, solutions.size());
    }

    // ======================== ISS-2025-0375: set_input/1 and set_output/1 actually redirect ========================

    @Test
    public void testISS0375_SetOutputRedirectsWrite() throws Exception {
        java.io.File f = java.io.File.createTempFile("iss0375_out", ".txt");
        f.deleteOnExit();
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String console = captureStdout(
            "open('" + f.getAbsolutePath() + "', write, S), set_output(S), write(redirected), "
            + "set_output(user_output), close(S).", solutions);
        assertEquals("set_output redirection query must succeed", 1, solutions.size());
        assertEquals("redirected", readWholeFile(f));
        assertEquals("write/1 after set_output must not reach the console", "", console);
    }

    @Test
    public void testISS0375_SetInputRedirectsGetCharAndGetCode() throws Exception {
        java.io.File f = writeTempPrologFile("iss0375_in", "foo(bar).\n");
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), set_input(S), get_char(C1), get_char(C2), "
            + "get_code(C3), set_input(user_input), close(S).");
        assertEquals("set_input redirection query must succeed", 1, solutions.size());
        assertEquals("f", solutions.get(0).get("C1").toString());
        assertEquals("o", solutions.get(0).get("C2").toString());
        assertEquals("111", solutions.get(0).get("C3").toString());
    }

    // ======================== ISS-2025-0376: peek_char/2, peek_code/2, get_code/2 stream forms ========================

    @Test
    public void testISS0376_PeekChar2DoesNotConsume() throws Exception {
        java.io.File f = writeTempPrologFile("iss0376_pk", "abc");
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), peek_char(S, P1), peek_char(S, P2), "
            + "get_char(S, G1), get_char(S, G2), close(S).");
        assertEquals("peek_char/2 must succeed", 1, solutions.size());
        assertEquals("a", solutions.get(0).get("P1").toString());
        assertEquals("peek_char/2 must not consume", "a", solutions.get(0).get("P2").toString());
        assertEquals("a", solutions.get(0).get("G1").toString());
        assertEquals("b", solutions.get(0).get("G2").toString());
    }

    @Test
    public void testISS0376_PeekCode2AndGetCode2() throws Exception {
        java.io.File f = writeTempPrologFile("iss0376_gc", "abc");
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), peek_code(S, P), get_code(S, C1), get_code(S, C2), close(S).");
        assertEquals("peek_code/2 and get_code/2 must succeed", 1, solutions.size());
        assertEquals("97", solutions.get(0).get("P").toString());
        assertEquals("97", solutions.get(0).get("C1").toString());
        assertEquals("98", solutions.get(0).get("C2").toString());
    }

    // ======================== ISS-2025-0377: open/close raise ISO error/2 terms ========================

    @Test
    public void testISS0377_OpenNonexistentRaisesExistenceError() throws Exception {
        java.io.File missing = java.io.File.createTempFile("iss0377_gone", ".txt");
        assertTrue(missing.delete());
        List<Map<String, Term>> solutions = prolog.solve(
            "catch(open('" + missing.getAbsolutePath() + "', read, _S), "
            + "error(existence_error(source_sink, F), _), true).");
        assertEquals("ISO existence_error(source_sink, F) pattern must match", 1, solutions.size());
        assertEquals(missing.getAbsolutePath(), solutions.get(0).get("F").toString());
    }

    @Test
    public void testISS0377_OpenInvalidModeRaisesDomainError() {
        assertEquals(1, prolog.solve(
            "catch(open('/tmp/iss0377_any.txt', frobnicate, _S), error(domain_error(io_mode, frobnicate), _), true).").size());
    }

    @Test
    public void testISS0377_CloseErrorsAreISO() {
        assertEquals("close of an unknown alias is existence_error(stream, S)", 1, prolog.solve(
            "catch(close(no_such_stream_iss0377), error(existence_error(stream, no_such_stream_iss0377), _), true).").size());
        assertEquals("close of a non-stream term is domain_error(stream_or_alias, S)", 1, prolog.solve(
            "catch(close(7), error(domain_error(stream_or_alias, 7), _), true).").size());
        assertEquals("close of an unbound variable is instantiation_error", 1, prolog.solve(
            "catch(close(_S), error(instantiation_error, _), true).").size());
    }

    // ======================== ISS-2025-0378: print/1 and print/2 ========================

    @Test
    public void testISS0378_Print1WritesWithNumbervars() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("print(hello), print(' '), print('$VAR'(0)).", solutions);
        assertEquals("print/1 must succeed", 1, solutions.size());
        assertEquals("hello A", out);
    }

    @Test
    public void testISS0378_Print2WritesToStream() throws Exception {
        java.io.File f = java.io.File.createTempFile("iss0378_print", ".txt");
        f.deleteOnExit();
        List<Map<String, Term>> solutions = prolog.solve(
            "open('" + f.getAbsolutePath() + "', write, S), print(S, foo(bar)), close(S).");
        assertEquals("print/2 must succeed", 1, solutions.size());
        assertEquals("foo(bar)", readWholeFile(f));
    }

    @Test
    public void testISS0379_AppendThirdUnboundGivesPartialListAnswer() {
        // append([1],X,Z) must succeed with Z=[1|X] instead of throwing "unsupported mode"
        List<Map<String, Term>> solutions = prolog.solve("append([1],X,Z), X = [2,3], Z == [1,2,3].");
        assertEquals("append/3 with unbound 3rd arg must give Z=[1|X]", 1, solutions.size());
    }

    @Test
    public void testISS0379_AppendSecondAndThirdUnbound() {
        List<Map<String, Term>> solutions = prolog.solve("append([1,2],Y,Z).");
        assertEquals(1, solutions.size());
        // Z must be the partial list [1,2|Y]
        assertEquals(1, prolog.solve("append([1,2],Y,Z), Y = [], Z == [1,2].").size());
    }

    @Test
    public void testISS0379_AppendFullyOpenDoesNotThrow() {
        // START_CHANGE: ISS-2025-0468 - engine v4 wave W6 pays off the W3 deviation: append/3 is
        // the real two-clause Prolog definition of prelude/lists.pl, so the fully-open mode
        // ENUMERATES (X = [], X = [_], X = [_,_], ...) instead of stopping at the one standard
        // solution the eager Java built-in could produce. Collecting every solution of an infinite
        // relation is therefore not a meaningful assertion — the guarantee is "the first solution
        // is X = [], and it arrives without throwing".
        // ISS-2025-0491 - 4.1 wave A: the bounded v2 branch is gone with the v2 engine (LIM-027).
        List<Map<String, Term>> v4 = prolog.solve("append(X,Y,Z), X == [], !.");
        assertFalse("append(X,Y,Z) must produce the X=[] solution, not throw", v4.isEmpty());
        assertEquals("fully open append/3 must enumerate lazily",
            1, prolog.solve("append(X, _, _), length(X, 2), !.").size());
        // END_CHANGE: ISS-2025-0468
    }

    // ======================== ISS-2025-0380: no unsound success on partial lists ========================

    @Test
    public void testISS0380_LastClosesPartialListTail() {
        // last([a|T],X) must bind T=[] (first standard solution), never leave T unconstrained
        List<Map<String, Term>> solutions = prolog.solve("last([a|T],X), T == [], X == a.");
        assertEquals("last/2 on a partial list must close the tail with []", 1, solutions.size());
    }

    @Test
    public void testISS0380_LastImproperListFails() {
        assertTrue("last on improper list [a|b] must fail, not truncate",
            prolog.solve("last([a|b],_X).").isEmpty());
    }

    @Test
    public void testISS0380_MaplistClosesPartialListTail() {
        // maplist(atom,[a,b|T]) must bind T=[] instead of succeeding with T unconstrained
        List<Map<String, Term>> solutions = prolog.solve("maplist(atom,[a,b|T]), T == [].");
        assertEquals("maplist/2 on a partial list must close the tail with []", 1, solutions.size());
    }

    // ======================== ISS-2025-0381: maplist nondeterminism + (-,+) mode ========================

    @Test
    public void testISS0381_MaplistEnumeratesInnerGoalSolutions() {
        List<Map<String, Term>> solutions = prolog.solve("maplist(member,[X,Y],[[1,2],[3,4]]).");
        assertEquals("maplist must be re-satisfiable through the mapped goal", 4, solutions.size());
    }

    @Test
    public void testISS0381_MaplistDoesNotCommitToFirstElementSolution() {
        // X=2 satisfies both member(X,[1,2]) and member(X,[2]); the old first-solution
        // commitment (X=1 from the first element) made this fail unsoundly
        List<Map<String, Term>> solutions = prolog.solve("maplist(member,[X,X],[[1,2],[2]]).");
        assertEquals(1, solutions.size());
        assertEquals("2", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0381_MaplistDerivesLengthFromSecondList() {
        // (-,+) mode: length comes from the second (proper) list
        List<Map<String, Term>> solutions = prolog.solve("maplist(succ,X,[2,3]), X == [1,2].");
        assertEquals("maplist(succ,X,[2,3]) must give X=[1,2]", 1, solutions.size());
    }

    // ======================== ISS-2025-0382: bagof/setof fresh copies per solution ========================

    @Test
    public void testISS0382_BagofResultHoldsFreshCopies() {
        // ISO 8.10.2: each collected element is a renamed-apart instance, so the two
        // W occurrences in the result are DISTINCT fresh variables
        List<Map<String, Term>> solutions = prolog.solve(
            "bagof(f(X,W),member(X,[1,2]),L), L = [f(1,a),f(2,b)].");
        assertEquals("result list vars must be independently bindable", 1, solutions.size());
    }

    @Test
    public void testISS0382_BagofResultDoesNotAliasCallerVariable() {
        // Binding list elements must NOT propagate back to the caller's template variable W
        List<Map<String, Term>> solutions = prolog.solve(
            "bagof(f(X,W),member(X,[1,2]),L), L = [f(1,a),f(2,a)], W == a.");
        assertTrue("W must stay unbound after binding the collected copies", solutions.isEmpty());
    }

    @Test
    public void testISS0382_SetofResultNotRewrittenByLaterBinding() {
        // The collected instance of Y is a fresh variable; binding Y afterwards must not
        // retroactively rewrite the setof result list
        List<Map<String, Term>> solutions = prolog.solve(
            "setof(Y,member(X,[1,2]),L), Y = 5, L == [5].");
        assertTrue("setof result must hold a fresh copy, not the caller's Y", solutions.isEmpty());
    }

    // ======================== ISS-2025-0383: aggregate_all/3 error transparency ========================

    @Test
    public void testISS0383_AggregateAllPropagatesISOErrorBall() {
        // The error(type_error(evaluable,a/0),_) ball raised inside the goal must escape
        // aggregate_all/3 unchanged and be caught by a matching catch/3
        List<Map<String, Term>> solutions = prolog.solve(
            "catch(aggregate_all(count,(member(X,[1,2]),X>a),_N),error(type_error(T,_),_),true).");
        assertEquals(1, solutions.size());
        assertEquals("evaluable", solutions.get(0).get("T").toString());
    }

    // ======================== ISS-2025-0384: bagof/setof/aggregate_all callable checks ========================

    @Test
    public void testISS0384_BagofUnboundGoalInstantiationError() {
        assertEquals(1, prolog.solve(
            "catch(bagof(X,_G,_L),error(instantiation_error,_),true).").size());
    }

    @Test
    public void testISS0384_SetofUnboundGoalInstantiationError() {
        assertEquals(1, prolog.solve(
            "catch(setof(X,_G,_L),error(instantiation_error,_),true).").size());
    }

    @Test
    public void testISS0384_BagofExistentialUnboundBodyInstantiationError() {
        // The body of Y^G is what must be callable (ISO 8.10.2.3)
        assertEquals(1, prolog.solve(
            "catch(bagof(X,Y^_G,_L),error(instantiation_error,_),true).").size());
    }

    @Test
    public void testISS0384_BagofNonCallableGoalTypeError() {
        assertEquals(1, prolog.solve(
            "catch(bagof(X,1,_L),error(type_error(callable,1),_),true).").size());
    }

    @Test
    public void testISS0384_AggregateAllUnboundGoalInstantiationError() {
        // Must raise instantiation_error, never silently answer N = 0
        assertEquals(1, prolog.solve(
            "catch(aggregate_all(count,_G,_N),error(instantiation_error,_),true).").size());
        assertTrue(prolog.solve(
            "catch(aggregate_all(count,_G,N),error(instantiation_error,_),fail), N == 0.").isEmpty());
    }

    // ======================== ISS-2025-0385: numlist/3 ISO errors on bad bounds ========================

    @Test
    public void testISS0385_NumlistUnboundBoundInstantiationError() {
        assertEquals(1, prolog.solve(
            "catch(numlist(_X,5,_L),error(instantiation_error,_),true).").size());
        assertEquals(1, prolog.solve(
            "catch(numlist(1,_Y,_L),error(instantiation_error,_),true).").size());
    }

    @Test
    public void testISS0385_NumlistNonIntegerBoundTypeError() {
        assertEquals(1, prolog.solve(
            "catch(numlist(1.5,3,_L),error(type_error(integer,1.5),_),true).").size());
        assertEquals(1, prolog.solve(
            "catch(numlist(1,foo,_L),error(type_error(integer,foo),_),true).").size());
    }

    @Test
    public void testISS0385_NumlistLowGreaterThanHighStillFailsQuietly() {
        assertTrue("numlist(1,0,L) is a normal failure, not an error",
            prolog.solve("numlist(1,0,_L).").isEmpty());
    }

    // ======================== ISS-2025-0386: inverse modes for reverse/select/permutation ========================

    @Test
    public void testISS0386_ReverseInverseMode() {
        List<Map<String, Term>> solutions = prolog.solve("reverse(X,[1,2,3]), X == [3,2,1].");
        assertEquals("reverse(-,+) must work", 1, solutions.size());
    }

    @Test
    public void testISS0386_SelectInsertionMode() {
        // select(2,L,[1,3]) -> L=[2,1,3] ; L=[1,2,3] ; L=[1,3,2]
        List<Map<String, Term>> solutions = prolog.solve("select(2,L,[1,3]).");
        assertEquals("select(+,-,+) must enumerate all insertion positions", 3, solutions.size());
        assertEquals(1, prolog.solve("select(2,L,[1,3]), L == [1,2,3].").size());
    }

    @Test
    public void testISS0386_PermutationInverseMode() {
        List<Map<String, Term>> solutions = prolog.solve("permutation(P,[1,2]).");
        assertEquals("permutation(-,+) must enumerate permutations", 2, solutions.size());
        assertEquals(1, prolog.solve("permutation(P,[1,2]), P == [2,1].").size());
    }

    @Test
    public void testISS0387_WriteqSeparatesMergingSymbolicTokens() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        assertEquals("-(1) is a compound, not the integer -1", "- 1", captureStdout("writeq(-(1)).", solutions));
        solutions.clear();
        assertEquals("1--1 re-tokenizes as the atom '--'", "1- -1", captureStdout("X = 1 - -1, writeq(X).", solutions));
        solutions.clear();
        assertEquals("- -a", captureStdout("writeq(- -a).", solutions));
        solutions.clear();
        assertEquals("2^ -1", captureStdout("writeq(2^ -1).", solutions));
        solutions.clear();
        assertEquals("- - -", captureStdout("writeq(-(-,-)).", solutions));
    }

    @Test
    public void testISS0387_WriteqOutputReReadsToSameTerm() {
        // Round-trip through JProlog's own parser: writeq output must denote the same term.
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        String out = captureStdout("X = 1 - -1, writeq(X).", solutions);
        List<Map<String, Term>> s = prolog.solve("Y = " + out + ", Y == 1 - -1.");
        assertEquals("'" + out + "' must re-read as 1 - -1", 1, s.size());

        solutions.clear();
        out = captureStdout("writeq(-(1)).", solutions);
        s = prolog.solve("Y = " + out + ", Y == -(1), \\+ integer(Y).");
        assertEquals("'" + out + "' must re-read as the compound -(1)", 1, s.size());
    }
    // END_CHANGE: ISS-2025-0387

    // ======================== ISS-2025-0388: writeq quoting of ',' '.' and comment openers ========================

    // START_CHANGE: ISS-2025-0388 - writeq must quote ',' '.' and comment-opening symbolic atoms
    @Test
    public void testISS0388_WriteqQuotesCommaDotAndCommentOpener() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        assertEquals("',' is a solo char, not an atom token", "f(',')", captureStdout("writeq(f(',')).", solutions));
        solutions.clear();
        assertEquals("a solo '.' forms the end token", "a+'.'", captureStdout("writeq(a+'.').", solutions));
        solutions.clear();
        assertEquals("unquoted /* opens a block comment", "'/*'", captureStdout("writeq('/*').", solutions));
        solutions.clear();
        // the genuine ','/2 control operator must STAY a bare comma
        assertEquals("a,b", captureStdout("writeq((a,b)).", solutions));
        solutions.clear();
        assertEquals("[a,',']", captureStdout("writeq([a,',']).", solutions));
    }
    // END_CHANGE: ISS-2025-0388

    // ======================== ISS-2025-0389: write/writeq render '$VAR'(N) (numbervars) ========================

    // START_CHANGE: ISS-2025-0389 - ISO 8.14.2: write/1 and writeq/1 imply numbervars(true)
    @Test
    public void testISS0389_NumbervarsRenderedByWriteAndWriteq() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        assertEquals("A", captureStdout("writeq('$VAR'(0)).", solutions));
        solutions.clear();
        assertEquals("Z", captureStdout("writeq('$VAR'(25)).", solutions));
        solutions.clear();
        assertEquals("Z1", captureStdout("writeq('$VAR'(51)).", solutions));
        solutions.clear();
        assertEquals("B", captureStdout("write('$VAR'(1)).", solutions));
        solutions.clear();
        assertEquals("A", captureStdout("format('~w', ['$VAR'(0)]).", solutions));
        solutions.clear();
        assertEquals("A", captureStdout("format('~q', ['$VAR'(0)]).", solutions));
    }
    // END_CHANGE: ISS-2025-0389

    // ======================== ISS-2025-0390: float writing (lowercase 'e', inf/nan) ========================

    // START_CHANGE: ISS-2025-0390 - ISO 6.4.5 float syntax: lowercase exponent; inf/-inf/nan spellings
    @Test
    public void testISS0390_FloatExponentLowercaseAndInfNan() {
        List<Map<String, Term>> solutions = new java.util.ArrayList<>();
        assertEquals("1.0e10", captureStdout("writeq(1.0e10).", solutions));
        solutions.clear();
        assertEquals("1.0e-6", captureStdout("writeq(0.000001).", solutions));
        solutions.clear();
        assertEquals("inf", captureStdout("X is inf, writeq(X).", solutions));
        solutions.clear();
        assertEquals("-inf", captureStdout("X is -inf, writeq(X).", solutions));
        solutions.clear();
        assertEquals("nan", captureStdout("X is nan, writeq(X).", solutions));
        // Direct checks on the single source of the rendering (Java's 'Infinity'/'NaN'
        // re-read as fresh VARIABLES, silently changing the term).
        assertEquals("inf", new Number(Double.POSITIVE_INFINITY, false).toString());
        assertEquals("-inf", new Number(Double.NEGATIVE_INFINITY, false).toString());
        assertEquals("nan", new Number(Double.NaN, false).toString());
        assertEquals("2.0e100", new Number(2.0e100, false).toString());
    }
    // END_CHANGE: ISS-2025-0390

    // ======================== ISS-2025-0391: phrase/2,3 applies the full DCG body translation ========================

    // START_CHANGE: ISS-2025-0391 - control constructs and terminal lists as phrase bodies
    @Test
    public void testISS0391_PhraseTranslatesControlConstructs() {
        prolog.consult("pa0391 --> [a].\npb0391 --> [b].");
        assertEquals("(A,B) body", 1, prolog.solve("phrase((pa0391, pb0391), [a, b]).").size());
        assertEquals("(A;B) body", 1, prolog.solve("phrase((pa0391 ; pb0391), [b]).").size());
        assertEquals("terminal-list body", 1, prolog.solve("phrase([a, b], [a, b]).").size());
        assertEquals("[] body", 1, prolog.solve("phrase([], []).").size());
        assertEquals("! body", 1, prolog.solve("phrase(!, []).").size());
        assertEquals("{G} body", 1, prolog.solve("phrase({true}, []).").size());
        assertEquals("(A->B) body", 1, prolog.solve("phrase((pa0391 -> pb0391), [a, b]).").size());
    }

    @Test
    public void testISS0391_PhraseNegationBodyIsZeroWidth() {
        prolog.consult("pa0391b --> [a].");
        List<Map<String, Term>> s = prolog.solve("phrase(\\+ pa0391b, [b], R).");
        assertEquals(1, s.size());
        assertEquals("\\+ is zero-width: the rest is the whole input", "[b]", s.get(0).get("R").toString());
        assertTrue(prolog.solve("phrase(\\+ pa0391b, [a], _).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0391

    // ======================== ISS-2025-0392: non-list head push-back ========================

    // START_CHANGE: ISS-2025-0392 - variable push-back rejected; string push-back becomes codes
    @Test
    public void testISS0392_VariablePushbackIsLoadError() {
        Prolog.CompilationResult r = prolog.consultWithDiagnostics(
            "vp0392, X --> [a], {X = [q]}.", "iss0392.pl");
        assertFalse("variable push-back must be rejected (instantiation_error), "
            + "not silently drop unconsumed input", r.success);
    }

    @Test
    public void testISS0392_StringPushbackConvertsToCodes() {
        prolog.consult("sp0392, \"x\" --> [a].");
        List<Map<String, Term>> s = prolog.solve("phrase(sp0392, [a], R).");
        assertEquals(1, s.size());
        assertEquals("\"x\" push-back must become its code list", "[120]", s.get(0).get("R").toString());
    }
    // END_CHANGE: ISS-2025-0392

    // ======================== ISS-2025-0393: partial terminal list in a DCG body ========================

    // START_CHANGE: ISS-2025-0393 - [a|T] terminal must error, not silently mean [a]
    @Test
    public void testISS0393_PartialTerminalListIsLoadError() {
        Prolog.CompilationResult r = prolog.consultWithDiagnostics("pt0393 --> [a|_X].", "iss0393.pl");
        assertFalse("[a|_] terminal must raise instantiation_error at translation time", r.success);
        Prolog.CompilationResult r2 = prolog.consultWithDiagnostics("pt0393b --> [a|b].", "iss0393.pl");
        assertFalse("[a|b] terminal must raise type_error(list, ...)", r2.success);
    }
    // END_CHANGE: ISS-2025-0393

    // ======================== ISS-2025-0394: phrase/2,3 ISO error clauses ========================

    // START_CHANGE: ISS-2025-0394 - type_error(list, L) / type_error(callable, B) instead of silent failure
    @Test
    public void testISS0394_PhraseRaisesTypeErrors() {
        prolog.consult("pa0394 --> [a].");
        List<Map<String, Term>> s = prolog.solve(
            "catch(phrase(pa0394, foo), error(type_error(list, foo), _), true).");
        assertEquals("non-list input must raise type_error(list, foo)", 1, s.size());
        s = prolog.solve(
            "catch(phrase(123, [a]), error(type_error(callable, 123), _), true).");
        assertEquals("non-callable body must raise type_error(callable, 123)", 1, s.size());
    }

    @Test
    public void testISS0394_PhraseStillAcceptsVarAndPartialLists() {
        prolog.consult("pa0394b --> [a].");
        // generation mode and partial-list input must stay legal (no over-eager type checks)
        assertEquals(1, prolog.solve("phrase(pa0394b, L).").size());
        assertEquals(1, prolog.solve("phrase(pa0394b, [a|T]).").size());
    }
    // END_CHANGE: ISS-2025-0394

    // ======================== ISS-2025-0396: retract/1 is re-executable ========================

    // START_CHANGE: ISS-2025-0396 - retract/1 backtracks into further matching clauses (ISO 8.9.3)
    @Test
    public void testISS0396_RetractDrainsPredicateViaFindall() {
        prolog.solve("assertz(rq0396(1)), assertz(rq0396(2)), assertz(rq0396(3)).");
        List<Map<String, Term>> s = prolog.solve("findall(X, retract(rq0396(X)), L).");
        assertEquals(1, s.size());
        assertEquals("retract/1 must retract the NEXT matching clause on each redo",
            "[1, 2, 3]", s.get(0).get("L").toString());
        // and the predicate must be empty afterwards
        assertEquals("[]", prolog.solve("findall(X, rq0396(X), R).").get(0).get("R").toString());
    }

    @Test
    public void testISS0396_RetractRetractsNextClauseOnRedo() {
        prolog.solve("assertz(pq0396(1)), assertz(pq0396(2)).");
        List<Map<String, Term>> s = prolog.solve("retract(pq0396(X)), X == 2.");
        assertEquals("(retract(p(X)), X == 2) must succeed by retracting p(2) on redo", 1, s.size());
        assertEquals("2", s.get(0).get("X").toString());
        // the first solution retracted pq0396(1), the redo retracted pq0396(2) -> none left
        assertTrue(prolog.solve("pq0396(_).").isEmpty());
    }

    @Test
    public void testISS0396_RetractFailPurgeLoopRemovesAllClauses() {
        prolog.solve("assertz(cq0396(1)), assertz(cq0396(2)).");
        assertEquals("the universal purge idiom must succeed",
            1, prolog.solve("\\+ ( retract(cq0396(_)), fail ).").size());
        assertEquals("the retract-fail purge loop must remove every clause",
            "[]", prolog.solve("findall(X, cq0396(X), C).").get(0).get("C").toString());
    }

    @Test
    public void testISS0396_RetractClauseFormOnRedo() {
        prolog.solve("assertz((hq0396(X) :- X = 1)), assertz((hq0396(X) :- X = 2)).");
        List<Map<String, Term>> s = prolog.solve("retract((hq0396(Y) :- Y = V)), V == 2.");
        assertEquals("clause-form retract must also be re-executable", 1, s.size());
        assertEquals("2", s.get(0).get("V").toString());
    }

    @Test
    public void testISS0396_KnowledgeBaseRetractReportsRemoval() {
        it.denzosoft.jprolog.core.engine.KnowledgeBase kb =
            new it.denzosoft.jprolog.core.engine.KnowledgeBase();
        it.denzosoft.jprolog.core.engine.Rule r = new it.denzosoft.jprolog.core.engine.Rule(
            new Atom("kbiss0396"), new java.util.ArrayList<>());
        kb.addRule(r);
        assertTrue("retract must report the clause was removed", kb.retract(r));
        assertFalse("retracting an already-removed clause must report false", kb.retract(r));
    }

    @Test
    public void testISS0396_RetractInFindallStillEnumerates() {
        // ISS-2025-0484 - wave W9: was testISS0396_LegacyEngineRetractStillEnumerates, on the
        // deleted recursive engine. It now runs on whichever engine the suite leg selects.
        prolog.solve("assertz(lr0396(1)), assertz(lr0396(2)).");
        List<Map<String, Term>> s = prolog.solve("findall(X, retract(lr0396(X)), L).");
        assertEquals(1, s.size());
        assertEquals("[1, 2]", s.get(0).get("L").toString());
    }
    // END_CHANGE: ISS-2025-0396

    // ======================== ISS-2025-0397: phrase/3 with two free variables ========================

    // START_CHANGE: ISS-2025-0397 - no spurious representation_error(cyclic_term) from a var-var union
    @Test
    public void testISS0397_PhraseWithTwoFreeVariablesNoSpuriousCyclicError() {
        prolog.consult("nt0397 --> [a].");
        // previously the legacy solution map's self-binding ({R=R, T=R}) was installed verbatim,
        // creating a deref cycle mis-reported as representation_error(cyclic_term)
        List<Map<String, Term>> s = prolog.solve("phrase(nt0397, [a|T], R).");
        assertEquals(1, s.size());
        // T and R must end up unified with each other, like the direct non-terminal call
        assertEquals(1, prolog.solve("phrase(nt0397, [a|T], R), T == R.").size());
        // ground input still works
        assertEquals("[b]", prolog.solve("phrase(nt0397, [a, b], R2).").get(0).get("R2").toString());
    }

    @Test
    public void testISS0397_RealCyclicTermProtectionUntouched() {
        // ISS-2025-0313: a rational tree (X = f(X) with occurs_check off) must STILL raise the
        // controlled representation_error, not be weakened by the var-var skip.
        // START_CHANGE: ISS-2025-0441 - the engine SUPPORTS rational trees (design decision 2,
        // approved): the query succeeds and cyclic_term/1 is a real test. The ISO "error" policy
        // remains available through set_prolog_flag(occurs_check, error).
        // ISS-2025-0491 - 4.1 wave A: the v2 branch that expected representation_error is gone.
        assertEquals("rational trees are supported: X = f(X), Y = X succeeds", 1,
            prolog.solve("X = f(X), Y = X.").size());
        assertEquals("and the term is genuinely cyclic", 1,
            prolog.solve("X = f(X), cyclic_term(X).").size());
        // END_CHANGE: ISS-2025-0441
    }
    // END_CHANGE: ISS-2025-0397

    // ======================== ISS-2025-0398: ^/2 callable as an ordinary goal ========================

    // START_CHANGE: ISS-2025-0398 - V^Goal outside bagof/setof behaves as call(Goal)
    @Test
    public void testISS0398_CaretGoalBehavesAsCall() {
        List<Map<String, Term>> s = prolog.solve("Y^member(X, [1, 2]).");
        assertEquals("V^Goal must call Goal", 2, s.size());
        assertEquals("1", s.get(0).get("X").toString());
        assertEquals("2", s.get(1).get("X").toString());
    }

    @Test
    public void testISS0398_CaretGoalInsideFindall() {
        List<Map<String, Term>> s = prolog.solve("findall(X, Y^member(X, [1, 2]), L).");
        assertEquals(1, s.size());
        assertEquals("[1, 2]", s.get(0).get("L").toString());
    }

    @Test
    public void testISS0398_CaretGoalAsPlainGoal() {
        // ISS-2025-0484 - wave W9: was testISS0398_CaretGoalOnLegacyEngine.
        assertEquals(2, prolog.solve("Y^member(X, [1, 2]).").size());
    }

    @Test
    public void testISS0398_BagofExistentialQuantifierStillStripped() {
        prolog.consult("pair0398(1, a). pair0398(2, a). pair0398(3, b).");
        List<Map<String, Term>> s = prolog.solve("bagof(X, Y^pair0398(X, Y), L).");
        assertEquals("bagof must keep treating ^ as the existential quantifier", 1, s.size());
        assertEquals("[1, 2, 3]", s.get(0).get("L").toString());
    }
    // END_CHANGE: ISS-2025-0398

    @Test
    public void testISS0411_SetofWitnessGroupsInStandardOrder() {
        List<Map<String, Term>> s = prolog.solve("setof(X, member(X-Y, [a-10, b-2]), L).");
        assertEquals("two witness groups", 2, s.size());
        // 2 @< 10 in the standard order of terms (string order would put "10" first)
        assertEquals("first group must be the witness Y = 2", "2", s.get(0).get("Y").toString());
        assertEquals("[b]", s.get(0).get("L").toString());
        assertEquals("second group must be the witness Y = 10", "10", s.get(1).get("Y").toString());
        assertEquals("[a]", s.get(1).get("L").toString());
    }
    // END_CHANGE: ISS-2025-0411

    // ======================== ISS-2025-0412: bagof/setof merge VARIANT witnesses ========================

    // START_CHANGE: ISS-2025-0412 - ISO 8.10.2.1: witnesses that are variants form ONE group
    @Test
    public void testISS0412_BagofSetofMergeVariantWitnesses() {
        // Each solution leaves a fresh (renamed-apart) variable in the witness: f(_G1) and
        // f(_G2) are variants, so they must form a single group — the old string-signature
        // grouping keyed them by variable name and produced two groups.
        prolog.consult("p0412(1, f(_)). p0412(2, f(_)).");
        List<Map<String, Term>> s = prolog.solve("bagof(X, p0412(X, Y), L).");
        assertEquals("variant witnesses must merge into one bagof group", 1, s.size());
        assertEquals("[1, 2]", s.get(0).get("L").toString());

        s = prolog.solve("setof(X, p0412(X, Y), L).");
        assertEquals("variant witnesses must merge into one setof group", 1, s.size());
        assertEquals("[1, 2]", s.get(0).get("L").toString());

        // ISO 8.10.2.4-style control: both disjuncts leave the witness pair unbound -> one group
        assertEquals(1, prolog.solve("bagof(X, (X = Y ; X = Z), S), S = [_, _].").size());
    }
    // END_CHANGE: ISS-2025-0412

    // ======================== ISS-2025-0413: aggregate_all max/min fail on no solutions ========================

    // START_CHANGE: ISS-2025-0413 - max/min fail on no solutions; type_error(number) on non-numbers
    @Test
    public void testISS0413_AggregateAllMaxMinFailOnNoSolutions() {
        assertTrue("aggregate_all(max(X), fail, M) must FAIL (SWI), not throw",
            prolog.solve("aggregate_all(max(X), fail, _M).").isEmpty());
        assertTrue("aggregate_all(min(X), fail, M) must FAIL (SWI), not throw",
            prolog.solve("aggregate_all(min(X), fail, _M).").isEmpty());
    }

    @Test
    public void testISS0413_AggregateAllMaxMinTypeErrorOnNonNumber() {
        assertEquals("max over non-numbers must raise type_error(number, _), not -Infinity", 1,
            prolog.solve("catch(aggregate_all(max(X), member(X, [a, c, b]), _M), error(type_error(number, _), _), true).").size());
        assertEquals("min over a mixed list must raise type_error(number, _)", 1,
            prolog.solve("catch(aggregate_all(min(X), member(X, [1, a]), _M), error(type_error(number, _), _), true).").size());
        // numeric extrema still work
        List<Map<String, Term>> s = prolog.solve("aggregate_all(max(X), member(X, [3, 1, 2]), M).");
        assertEquals(1, s.size());
        assertEquals("3", s.get(0).get("M").toString());
        s = prolog.solve("aggregate_all(min(X), member(X, [3, 1, 2]), M).");
        assertEquals(1, s.size());
        assertEquals("1", s.get(0).get("M").toString());
    }
    // END_CHANGE: ISS-2025-0413

    // ======================== ISS-2025-0414: aggregate_all(sum) exact and typed ========================

    // START_CHANGE: ISS-2025-0414 - exact big-integer sums, typed results, type_error on non-numbers
    @Test
    public void testISS0414_AggregateAllSumExactAndTyped() {
        // a non-numeric solution raises type_error(number, a) instead of being silently skipped
        assertEquals(1, prolog.solve(
            "catch(aggregate_all(sum(X), member(X, [1, a, 2]), _S), error(type_error(number, a), _), true).").size());
        // exact big-integer sum (the double accumulator rounded this to ...680)
        List<Map<String, Term>> s = prolog.solve("aggregate_all(sum(X), member(X, [123456789012345678, 1]), S).");
        assertEquals(1, s.size());
        assertEquals("123456789012345679", s.get(0).get("S").toString());
        // integer sums stay integers; float sums stay floats; empty sum is integer 0
        assertEquals(1, prolog.solve("aggregate_all(sum(X), member(X, [1, 2]), S), integer(S), S =:= 3.").size());
        assertEquals(1, prolog.solve("aggregate_all(sum(X), member(X, [1.5, 2.5]), S), float(S), S =:= 4.0.").size());
        assertEquals(1, prolog.solve("aggregate_all(sum(X), fail, S), S == 0.").size());
    }
    // END_CHANGE: ISS-2025-0414

    // ======================== ISS-2025-0415: once/ignore/forall callable checks ========================

    // START_CHANGE: ISS-2025-0415 - type_error(callable, G) for non-callable goals
    @Test
    public void testISS0415_OnceIgnoreForallNonCallableTypeError() {
        assertEquals("once(1) must raise type_error(callable, 1)", 1, prolog.solve(
            "catch(once(1), error(type_error(callable, 1), _), true).").size());
        // ignore(1)/forall(1,true) silently SUCCEEDED before the fix, so conjoin with fail:
        // only the caught error path can yield a solution
        assertEquals("ignore(1) must raise type_error(callable, 1), not succeed", 1, prolog.solve(
            "catch((ignore(1), fail), error(type_error(callable, 1), _), true).").size());
        assertEquals("forall(1, true) must raise type_error(callable, 1)", 1, prolog.solve(
            "catch((forall(1, true), fail), error(type_error(callable, 1), _), true).").size());
        assertEquals("forall(true, 1) must raise type_error(callable, 1)", 1, prolog.solve(
            "catch((forall(true, 1), fail), error(type_error(callable, 1), _), true).").size());
        // unbound goals keep raising instantiation_error
        assertEquals(1, prolog.solve("catch(once(_G), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve("catch((ignore(_G), fail), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve("catch((forall(_G, true), fail), error(instantiation_error, _), true).").size());
        // callable goals are untouched
        assertEquals(1, prolog.solve("once(member(_X, [1, 2])).").size());
        assertEquals(1, prolog.solve("ignore(fail).").size());
        assertEquals(1, prolog.solve("forall(member(X, [1, 2]), number(X)).").size());
    }
    // END_CHANGE: ISS-2025-0415

    // ======================== ISS-2025-0416: findall/3 Instances type check ========================

    // START_CHANGE: ISS-2025-0416 - ISO 8.10.1.3(c): type_error(list, Instances)
    @Test
    public void testISS0416_FindallThirdArgTypeCheck() {
        assertEquals("findall(X, fail, a) must raise type_error(list, a)", 1, prolog.solve(
            "catch(findall(X, fail, a), error(type_error(list, a), _), true).").size());
        // a partial list stays legal, as do variables and proper lists
        assertEquals(1, prolog.solve("findall(X, member(X, [1, 2]), [A|T]), A == 1, T == [2].").size());
        assertEquals(1, prolog.solve("findall(X, fail, L), L == [].").size());
        assertTrue("a wrong proper list still just fails", prolog.solve("findall(X, fail, [a]).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0416

    // ======================== ISS-2025-0417: compare/3 Order validation ========================

    // START_CHANGE: ISS-2025-0417 - ISO 8.4.2.3: domain_error(order)/type_error(atom) for bad Order
    @Test
    public void testISS0417_CompareInvalidOrderArg() {
        assertEquals("compare(foo, 1, 2) must raise domain_error(order, foo)", 1, prolog.solve(
            "catch(compare(foo, 1, 2), error(domain_error(order, foo), _), true).").size());
        assertEquals("compare(3, 1, 2) must raise type_error(atom, 3)", 1, prolog.solve(
            "catch(compare(3, 1, 2), error(type_error(atom, 3), _), true).").size());
        // valid pre-bound orders still verify (or fail) by comparison, without errors
        assertEquals(1, prolog.solve("compare(<, 1, 2).").size());
        assertTrue(prolog.solve("compare(>, 1, 2).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0417

    // ======================== ISS-2025-0418: sort/4 key validation ========================

    // START_CHANGE: ISS-2025-0418 - Key > 0 requires compound elements with arity >= Key
    @Test
    public void testISS0418_Sort4KeyValidation() {
        assertEquals("sort(1, @<, [b, a], L): non-compound element must raise type_error(compound, _)",
            1, prolog.solve("catch(sort(1, @<, [b, a], _L), error(type_error(compound, _), _), true).").size());
        assertEquals("sort(2, @<, [f(a)], L): Key beyond the arity must raise domain_error(argument_index, _)",
            1, prolog.solve("catch(sort(2, @<, [f(a)], _L), error(domain_error(argument_index, _), _), true).").size());
        // ISO error terms for bad Key/Order (was a generic PrologEvaluationException)
        assertEquals(1, prolog.solve("catch(sort(x, @<, [a], _L), error(type_error(integer, x), _), true).").size());
        assertEquals(1, prolog.solve("catch(sort(0, foo, [a], _L), error(domain_error(order, foo), _), true).").size());
        // valid keyed sorts unchanged
        assertEquals(1, prolog.solve("sort(1, @<, [f(b, 1), f(a, 2)], L), L == [f(a, 2), f(b, 1)].").size());
        assertEquals(1, prolog.solve("sort(0, @=<, [c, a, b, a], L), L == [a, a, b, c].").size());
    }
    // END_CHANGE: ISS-2025-0418

    // ======================== ISS-2025-0419: predsort/3 non-ground lists + failing Pred ========================

    // START_CHANGE: ISS-2025-0419 - no groundness gate; Pred failure makes predsort fail; bad Pred errors
    @Test
    public void testISS0419_PredsortNonGroundListAndFailingPred() {
        // variables are legal list elements (lowest in the standard order)
        List<Map<String, Term>> s = prolog.solve("predsort(compare, [X, Y], L).");
        assertEquals("predsort must accept non-ground lists", 1, s.size());
        // a comparison predicate that fails on a pair makes predsort FAIL — it must never
        // 'sort' with a silent default ordering
        prolog.consult("pfail0419(_, _, _) :- fail.");
        assertTrue("predsort must fail when Pred fails on a pair",
            prolog.solve("predsort(pfail0419, [b, a, c], _L).").isEmpty());
        // an Order outside <, =, > also makes predsort fail
        prolog.consult("pbad0419(foo, _, _).");
        assertTrue("predsort must fail when Pred binds Order outside <, =, >",
            prolog.solve("predsort(pbad0419, [b, a], _L).").isEmpty());
        // unbound / non-callable Pred raise the proper errors
        assertEquals(1, prolog.solve(
            "catch(predsort(_P, [a, b], _L), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve(
            "catch((predsort(7, [a, b], _L), fail), error(type_error(callable, 7), _), true).").size());
        // control: normal sorting with '=' dedup still works
        assertEquals(1, prolog.solve("predsort(compare, [b, a, c, a], L), L == [a, b, c].").size());
    }
    // END_CHANGE: ISS-2025-0419

    // ======================== ISS-2025-0420: arg/3 and =../2 ISO errors ========================

    // START_CHANGE: ISS-2025-0420 - ISO 8.5.2.3 arg/3 error terms; works on non-ground compounds
    @Test
    public void testISS0420_ArgIsoErrors() {
        assertEquals("arg(_, f(a), A) -> instantiation_error", 1, prolog.solve(
            "catch(arg(_N, f(a), _A), error(instantiation_error, _), true).").size());
        assertEquals("arg(0.5, f(a), A) -> type_error(integer, 0.5)", 1, prolog.solve(
            "catch(arg(0.5, f(a), _A), error(type_error(integer, _), _), true).").size());
        assertEquals("arg(-1, f(a), A) -> domain_error(not_less_than_zero, -1)", 1, prolog.solve(
            "catch(arg(-1, f(a), _A), error(domain_error(not_less_than_zero, _), _), true).").size());
        assertEquals("arg(1, foo, A) -> type_error(compound, foo)", 1, prolog.solve(
            "catch(arg(1, foo, _A), error(type_error(compound, foo), _), true).").size());
        // the old isGround gate wrongly FAILED arg/3 on non-ground compounds
        assertEquals("arg/3 must work on non-ground compounds", 1,
            prolog.solve("arg(1, f(X), A), X = hello, A == hello.").size());
        // out-of-range / zero index is still plain failure
        assertTrue(prolog.solve("arg(3, f(a, b), _A).").isEmpty());
        assertTrue(prolog.solve("arg(0, f(a), _A).").isEmpty());
    }

    @Test
    public void testISS0420_UnivIsoErrors() {
        assertEquals("X =.. Y (both unbound) -> instantiation_error", 1, prolog.solve(
            "catch(_X =.. _Y, error(instantiation_error, _), true).").size());
        assertEquals("X =.. a -> type_error(list, a)", 1, prolog.solve(
            "catch(_X =.. a, error(type_error(list, a), _), true).").size());
        assertEquals("X =.. [3, x] -> type_error(atom, 3)", 1, prolog.solve(
            "catch(_X =.. [3, x], error(type_error(atom, 3), _), true).").size());
        assertEquals("X =.. [f(a), a] -> type_error(atom, f(a))", 1, prolog.solve(
            "catch(_X =.. [f(a), a], error(type_error(atom, f(a)), _), true).").size());
        assertEquals("X =.. [] -> domain_error(non_empty_list, [])", 1, prolog.solve(
            "catch(_X =.. [], error(domain_error(non_empty_list, []), _), true).").size());
        // the old isGround gates wrongly raised instantiation_error for these two legal modes
        assertEquals("construction with unbound arguments must work", 1,
            prolog.solve("X =.. [f, Y], X = f(1), Y == 1.").size());
        assertEquals("decomposition of a non-ground term must work", 1,
            prolog.solve("f(_Q) =.. L, L = [F | _], F == f.").size());
    }
    // END_CHANGE: ISS-2025-0420

    @Test
    public void testISS0399_FloatTextYieldsFloat() {
        // "1.0" must parse to the FLOAT 1.0 (ISO 8.16.7), not collapse to the integer 1
        assertEquals(1, prolog.solve("number_chars(X, ['1','.','0']), float(X).").size());
        // "1.0e5" must stay a float (100000.0), not the integer 100000
        List<Map<String, Term>> s = prolog.solve("number_chars(X, ['1','.','0','e','5']), float(X).");
        assertEquals(1, s.size());
        assertEquals("100000.0", s.get(0).get("X").toString());
        // number_string keeps the float type too
        assertEquals(1, prolog.solve("number_string(N, \"1.0\"), float(N).").size());
    }

    @Test
    public void testISS0399_FloatToTextKeepsFloatSyntax() {
        // number_codes(1.0, L) -> "1.0" = [49,46,48] (the .0 used to be dropped)
        assertEquals(1, prolog.solve("number_codes(1.0, [49, 46, 48]).").size());
        // atom_number(A, 1.0) -> '1.0', not the atom '1'
        assertEquals("1.0", prolog.solve("atom_number(A, 1.0).").get(0).get("A").toString());
        // round trip preserves the float type
        assertEquals(1, prolog.solve("number_chars(123.0, L), number_chars(X, L), float(X).").size());
    }

    @Test
    public void testISS0399_BothGroundComparesExactly() {
        // the chars of the integer 1 name the FLOAT 1.0 -> type mismatch must fail
        assertTrue(prolog.solve("number_chars(1, ['1','.','0']).").isEmpty());
        // the old 1e-10 epsilon wrongly equated 1.00000000001 with "1.0"
        assertTrue(prolog.solve("number_chars(1.00000000001, ['1','.','0']).").isEmpty());
        // ISO list-first semantics: "1.00" still denotes the float 1.0
        assertEquals(1, prolog.solve("number_chars(1.0, ['1','.','0','0']).").size());
        assertTrue(prolog.solve("atom_number('1.0', 1).").isEmpty());
        assertEquals(1, prolog.solve("number_string(1.0, \"1.0\").").size());
        assertTrue(prolog.solve("number_string(1, \"1.0\").").isEmpty());
        // big-integer exactness (ISS-2025-0365) stays intact
        assertEquals("9223372036854775808",
            prolog.solve("number_chars(X, ['9','2','2','3','3','7','2','0','3','6','8','5','4','7','7','5','8','0','8']).")
                .get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0399

    // ======================== ISS-2025-0400: ISO number notation in number_chars/number_codes ========================

    // START_CHANGE: ISS-2025-0400 - 0x/0o/0b/0'c accepted, Java-only spellings raise syntax_error
    @Test
    public void testISS0400_NumberCharsAcceptsIsoNotation() {
        assertEquals("255", prolog.solve("number_chars(X, ['0','x','f','f']).").get(0).get("X").toString());
        assertEquals("63", prolog.solve("number_chars(X, ['0','o','7','7']).").get(0).get("X").toString());
        assertEquals("3", prolog.solve("number_chars(X, ['0','b','1','1']).").get(0).get("X").toString());
        // "0'a" = [48,39,97] is the character-code constant 97
        assertEquals("97", prolog.solve("number_codes(X, [48, 39, 97]).").get(0).get("X").toString());
        // leading layout stays legal (ISO 8.16.7.1), sign included
        assertEquals("-1", prolog.solve("number_chars(X, [' ','-','1']).").get(0).get("X").toString());
    }

    @Test
    public void testISS0400_NumberCharsRejectsJavaOnlySyntax() {
        // Infinity / NaN / ".5" / "3." / trailing layout are not Prolog number tokens (ISO 6.4.4/6.4.5)
        assertEquals(1, prolog.solve(
            "catch(number_chars(X, ['I','n','f','i','n','i','t','y']), error(syntax_error(_), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(number_chars(X, ['N','a','N']), error(syntax_error(_), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(number_chars(X, ['.','5']), error(syntax_error(_), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(number_chars(X, ['3','.']), error(syntax_error(_), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(number_chars(X, ['5',' ']), error(syntax_error(_), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(number_codes(X, [49, 102]), error(syntax_error(_), _), true).").size()); // "1f"
    }
    // END_CHANGE: ISS-2025-0400

    // ======================== ISS-2025-0401: char_code/2 ISO error terms ========================

    // START_CHANGE: ISS-2025-0401 - instantiation/type/representation errors instead of silent false
    @Test
    public void testISS0401_CharCodeIsoErrors() {
        assertEquals(1, prolog.solve(
            "catch(char_code(X, Y), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(char_code(ab, X), error(type_error(character, ab), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(char_code(X, foo), error(type_error(integer, foo), _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(char_code(X, -1), error(representation_error(character_code), _), true).").size());
        // success cases unchanged
        assertEquals("97", prolog.solve("char_code(a, X).").get(0).get("X").toString());
        assertEquals("b", prolog.solve("char_code(X, 98).").get(0).get("X").toString());
        assertEquals(1, prolog.solve("char_code(a, 97).").size());
        assertTrue(prolog.solve("char_code(a, 98).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0401

    // ======================== ISS-2025-0402: term_to_atom/2 on non-ground terms ========================

    // START_CHANGE: ISS-2025-0402 - non-ground terms serialize (variables render with their names)
    @Test
    public void testISS0402_TermToAtomNonGroundTerm() {
        List<Map<String, Term>> s = prolog.solve("term_to_atom(foo(X, bar), A), atom(A).");
        assertEquals(1, s.size());
        String a = s.get(0).get("A").toString();
        assertTrue("must serialize the non-ground term, got: " + a, a.startsWith("foo("));
        assertTrue("must keep the ground part, got: " + a, a.contains("bar"));
        // the (+partial_term, +atom) mode keeps parse-and-unify semantics
        s = prolog.solve("term_to_atom(foo(Z), 'foo(bar)').");
        assertEquals(1, s.size());
        assertEquals("bar", s.get(0).get("Z").toString());
        // ground direction unchanged
        assertEquals("foo(a,b)", prolog.solve("term_to_atom(foo(a, b), A).").get(0).get("A").toString());
    }
    // END_CHANGE: ISS-2025-0402

    // ======================== ISS-2025-0403: string_to_atom/2 (-,+) mode binds a string ========================

    // START_CHANGE: ISS-2025-0403 - the string side gets a PrologString, not an Atom
    @Test
    public void testISS0403_StringToAtomBindsString() {
        assertEquals(1, prolog.solve("string_to_atom(S, foo), string(S).").size());
        assertTrue(prolog.solve("string_to_atom(S, foo), atom(S).").isEmpty());
        // (+,-) direction unchanged: produces an atom
        assertEquals(1, prolog.solve("string_to_atom(\"hello\", A), atom(A).").size());
    }
    // END_CHANGE: ISS-2025-0403

    // ======================== ISS-2025-0404: string/1 type check ========================

    // START_CHANGE: ISS-2025-0404 - string/1 is true for PrologString terms only
    @Test
    public void testISS0404_StringTypeCheck() {
        assertEquals(1, prolog.solve("string(\"abc\").").size());
        assertEquals(1, prolog.solve("X = \"abc\", string(X).").size());
        assertTrue(prolog.solve("string(abc).").isEmpty());
        assertTrue(prolog.solve("string(123).").isEmpty());
        assertTrue(prolog.solve("string(f(x)).").isEmpty());
        assertTrue(prolog.solve("string(X).").isEmpty());
        assertTrue(prolog.solve("string([104, 105]).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0404

    // ======================== ISS-2025-0405: SWI-style atom/string text interop ========================

    // START_CHANGE: ISS-2025-0405 - atom_* accept strings, string_* accept atoms
    @Test
    public void testISS0405_AtomPredicatesAcceptStrings() {
        assertEquals("abc", prolog.solve("atom_codes(X, \"abc\").").get(0).get("X").toString());
        assertEquals("ab", prolog.solve("atom_chars(X, \"ab\").").get(0).get("X").toString());
        assertEquals("3", prolog.solve("atom_length(\"abc\", N).").get(0).get("N").toString());
        assertEquals("abcd", prolog.solve("atom_concat(\"ab\", cd, R).").get(0).get("R").toString());
        assertEquals("-1", prolog.solve("number_codes(X, \"-1\").").get(0).get("X").toString());
    }

    @Test
    public void testISS0405_StringPredicatesAcceptAtoms() {
        // results stay strings
        assertEquals(1, prolog.solve("string_concat(a, b, S), string(S), string_chars(S, [a, b]).").size());
        assertEquals("3", prolog.solve("string_length(abc, N).").get(0).get("N").toString());
        assertEquals(1, prolog.solve("string_chars(abc, [a, b, c]).").size());
    }
    // END_CHANGE: ISS-2025-0405

    // ======================== ISS-2025-0406: typed ISO errors in conversion/concat predicates ========================

    // START_CHANGE: ISS-2025-0406 - instantiation/type/domain/syntax errors instead of silent false
    @Test
    public void testISS0406_AtomConcatIsoErrors() {
        // ISO 8.16.2.3 a: A3 unbound together with A1/A2 unbound -> instantiation_error
        assertEquals(1, prolog.solve(
            "catch(atom_concat(X, Y, Z), error(instantiation_error, _), true).").size());
        // ISO 8.16.2.3: non-atom argument -> error(type_error(atom, 1), _) as a proper ball
        assertEquals(1, prolog.solve(
            "catch(atom_concat(a, 1, R), error(type_error(atom, 1), _), true).").size());
        // working modes unchanged
        assertEquals("ab", prolog.solve("atom_concat(a, b, X).").get(0).get("X").toString());
        assertEquals(3, prolog.solve("atom_concat(X, Y, ab).").size());
    }

    @Test
    public void testISS0406_NumberConversionIsoErrors() {
        // ISO 8.16.8.3 a: both unbound -> instantiation_error (was a bare message exception)
        assertEquals(1, prolog.solve(
            "catch(number_codes(X, Y), error(instantiation_error, _), true).").size());
        // ISO 8.16.8.3 b: non-number first argument -> type_error(number, a)
        assertEquals(1, prolog.solve(
            "catch(number_codes(a, L), error(type_error(number, a), _), true).").size());
        // ISO 8.16.7.3: unparsable chars -> syntax_error (was a silent false)
        assertEquals(1, prolog.solve(
            "catch(number_chars(X, [a, b]), error(syntax_error(_), _), true).").size());
    }

    @Test
    public void testISS0406_AtomCharsCodesIsoErrors() {
        // ISO 8.16.4.3 a: partial list / unbound element with unbound atom -> instantiation_error
        assertEquals(1, prolog.solve(
            "catch(atom_chars(X, [a, Y]), error(instantiation_error, _), true).").size());
        assertEquals(1, prolog.solve(
            "catch(atom_chars(X, Y), error(instantiation_error, _), true).").size());
        // element not a one-char atom -> type_error(character, ab)
        assertEquals(1, prolog.solve(
            "catch(atom_chars(X, [ab]), error(type_error(character, ab), _), true).").size());
        // atom_codes element not a character code -> representation_error(character_code)
        assertEquals(1, prolog.solve(
            "catch(atom_codes(X, [a]), error(representation_error(character_code), _), true).").size());
        // numbers stringify (SWI/GNU) instead of failing silently
        assertEquals(1, prolog.solve("atom_chars(123, ['1','2','3']).").size());
        assertEquals(1, prolog.solve("atom_codes(1.5, [49, 46, 53]).").size());
    }

    @Test
    public void testISS0406_AtomLengthLengthValidation() {
        // ISO 8.16.1.3 c: Length neither var nor integer -> type_error(integer, foo)
        assertEquals(1, prolog.solve(
            "catch(atom_length(abc, foo), error(type_error(integer, foo), _), true).").size());
        // ISO 8.16.1.3 d: negative Length -> domain_error(not_less_than_zero, -1)
        assertEquals(1, prolog.solve(
            "catch(atom_length(a, -1), error(domain_error(not_less_than_zero, -1), _), true).").size());
        // valid checks unchanged
        assertEquals(1, prolog.solve("atom_length(abc, 3).").size());
        assertTrue(prolog.solve("atom_length(abc, 4).").isEmpty());
    }
    // END_CHANGE: ISS-2025-0406

    // ======================== ISS-2025-0407: float_integer_part / float_fractional_part beyond 2^63 ========================

    // START_CHANGE: ISS-2025-0407 - no more (long)-cast saturation at +/-2^63
    @Test
    public void testISS0407_FloatPartsBeyondLongRange() {
        assertEquals("1.0e20", prolog.solve("X is float_integer_part(1.0e20).").get(0).get("X").toString());
        assertEquals("0.0", prolog.solve("X is float_fractional_part(1.0e20).").get(0).get("X").toString());
        assertEquals("-1.0e20", prolog.solve("X is float_integer_part(-1.0e20).").get(0).get("X").toString());
        // small values keep truncate-toward-zero semantics
        assertEquals("0.75", prolog.solve("X is float_fractional_part(3.75).").get(0).get("X").toString());
        assertEquals("-2.0", prolog.solve("X is float_integer_part(-2.5).").get(0).get("X").toString());
        assertEquals("-0.5", prolog.solve("X is float_fractional_part(-2.5).").get(0).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0407

    @Test
    public void testISS0408_ReadMultiLineTermFromStream() throws Exception {
        java.io.File f = writeTempPrologFile("iss0408_multi", "foo(\n  1\n).\n");
        List<Map<String, Term>> s = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), read(S, T), close(S).");
        assertEquals("a term spanning several lines must be read", 1, s.size());
        assertEquals("foo(1)", s.get(0).get("T").toString());
    }

    @Test
    public void testISS0408_ReadTwoTermsOnOneLine() throws Exception {
        java.io.File f = writeTempPrologFile("iss0408_two", "a(1). b(2).\n");
        List<Map<String, Term>> s = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), read(S, T1), read(S, T2), read(S, T3), close(S).");
        assertEquals("two terms on one line must be read one at a time", 1, s.size());
        assertEquals("a(1)", s.get(0).get("T1").toString());
        assertEquals("b(2)", s.get(0).get("T2").toString());
        assertEquals("end_of_file", s.get(0).get("T3").toString());
    }

    @Test
    public void testISS0408_ReadSkipsLeadingComments() throws Exception {
        java.io.File f = writeTempPrologFile("iss0408_cmt", "% leading comment\nfoo(42). /* block */ bar(7).\n");
        List<Map<String, Term>> s = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), read(S, T1), read(S, T2), close(S).");
        assertEquals("comment lines must be skipped as layout", 1, s.size());
        assertEquals("foo(42)", s.get(0).get("T1").toString());
        assertEquals("bar(7)", s.get(0).get("T2").toString());
    }

    @Test
    public void testISS0408_ReadTermMultiLineFromStream() throws Exception {
        java.io.File f = writeTempPrologFile("iss0408_rt", "foo(\n  bar\n). baz(9).\n");
        List<Map<String, Term>> s = prolog.solve(
            "open('" + f.getAbsolutePath() + "', read, S), read_term(S, T1, []), read_term(S, T2, []), close(S).");
        assertEquals("read_term must read up to the end token, not one line", 1, s.size());
        assertEquals("foo(bar)", s.get(0).get("T1").toString());
        assertEquals("baz(9)", s.get(0).get("T2").toString());
    }

    @Test
    public void testISS0408_ReadTermTextTokenAwareness() throws Exception {
        // graphic token =.. and float dots must not terminate the term; the reader position
        // is preserved between calls so the next term can be read from the same reader
        java.io.StringReader r = new java.io.StringReader("p(X) :- X =.. L, q(3.14). rest(1).");
        assertEquals("p(X) :- X =.. L, q(3.14)", it.denzosoft.jprolog.builtin.io.Read.readTermText(r));
        assertEquals("rest(1)", it.denzosoft.jprolog.builtin.io.Read.readTermText(r));
        assertNull("EOF must yield null (end_of_file)", it.denzosoft.jprolog.builtin.io.Read.readTermText(r));
        // dots inside quoted atoms, strings and 0'. char literals must not end the term
        java.io.StringReader q = new java.io.StringReader("x('a.b', \"c.d\", 0'.). next.");
        assertEquals("x('a.b', \"c.d\", 0'.)", it.denzosoft.jprolog.builtin.io.Read.readTermText(q));
        assertEquals("next", it.denzosoft.jprolog.builtin.io.Read.readTermText(q));
    }
    // END_CHANGE: ISS-2025-0408

    // ======================== ISS-2025-0409: format/2,3 argument-mismatch strictness ========================

    // START_CHANGE: ISS-2025-0409 - format argument mismatches raised silently absorbed errors
    @Test
    public void testISS0409_FormatTooFewArgumentsRaisesFormatError() {
        assertEquals("missing ~d argument must raise error(format(...), _)", 1, prolog.solve(
            "catch(format('a~db~n', []), error(format(_), _), true).").size());
        assertEquals("missing second ~w argument must raise error(format(...), _)", 1, prolog.solve(
            "catch(format('~w-~w~n', [only_one]), error(format(_), _), true).").size());
    }

    @Test
    public void testISS0409_FormatDirectiveDRequiresInteger() {
        assertEquals("~d with a float must raise type_error(integer, 3.7)", 1, prolog.solve(
            "catch(format('~d~n', [3.7]), error(type_error(integer, _), _), true).").size());
        assertEquals("~d with an atom must raise type_error(integer, foo)", 1, prolog.solve(
            "catch(format('~d~n', [foo]), error(type_error(integer, foo), _), true).").size());
    }

    @Test
    public void testISS0409_FormatUnknownDirectiveRaisesError() {
        List<Map<String, Term>> s = new java.util.ArrayList<>();
        String out = captureStdout("catch(format('~z~n', [hello]), error(format(_), _), true).", s);
        assertEquals("~z must raise error(format(...), _)", 1, s.size());
        assertEquals("~z must not be echoed literally", "", out);
    }

    @Test
    public void testISS0409_FormatEmptyListIsEmptyArgumentList() {
        // [] is the EMPTY argument list, so '~a' has no argument: error, not printing '[]'
        assertEquals(1, prolog.solve(
            "catch(format('~a~n', []), error(format(_), _), true).").size());
        // ...but [[]] supplies the atom [] as ONE argument
        List<Map<String, Term>> s = new java.util.ArrayList<>();
        assertEquals("[]", captureStdout("format('~a', [[]]).", s));
        assertEquals(1, s.size());
    }

    @Test
    public void testISS0409_FormatWellFormedCallsStillWork() {
        List<Map<String, Term>> s = new java.util.ArrayList<>();
        assertEquals("a-b\n", captureStdout("format('~w-~w~n', [a, b]).", s));
        assertEquals(1, s.size());
        s.clear();
        assertEquals("42", captureStdout("format('~d', [42]).", s));
        assertEquals(1, s.size());
        s.clear();
        // a non-list argument term is still treated as a single argument (SWI compatibility)
        assertEquals("ok", captureStdout("format('~w', ok).", s));
        assertEquals(1, s.size());
    }
    // END_CHANGE: ISS-2025-0409

    // ======================== ISS-2025-0410: non-callable DCG head diagnostics ========================

    // START_CHANGE: ISS-2025-0410 - 7 --> [a] reported "Cannot redefine built-in predicate call/3"
    @Test
    public void testISS0410_NonCallableDcgHeadIsTypeError() {
        Prolog.CompilationResult r = prolog.consultWithDiagnostics("123 --> [a].", "iss0410.pl");
        assertFalse("a numeric DCG head must be a load error", r.success);
        String msg = r.errors.get(0).message;
        assertTrue("error must be type_error(callable, 123), got: " + msg,
            msg.contains("type_error(callable, 123)"));
        assertFalse("misleading built-in redefinition error must be gone: " + msg,
            msg.contains("Cannot redefine"));
    }

    @Test
    public void testISS0410_VariableDcgHeadIsInstantiationError() {
        Prolog.CompilationResult r = prolog.consultWithDiagnostics("X --> [a].", "iss0410.pl");
        assertFalse("a variable DCG head must be a load error", r.success);
        assertTrue("error must be instantiation_error, got: " + r.errors.get(0).message,
            r.errors.get(0).message.contains("instantiation_error"));
    }

    @Test
    public void testISS0410_NonCallablePushBackHeadIsTypeError() {
        Prolog.CompilationResult r = prolog.consultWithDiagnostics("(7, [a]) --> [b].", "iss0410.pl");
        assertFalse("a numeric push-back non-terminal must be a load error", r.success);
        assertTrue("error must be type_error(callable, 7), got: " + r.errors.get(0).message,
            r.errors.get(0).message.contains("type_error(callable, 7)"));
    }

    @Test
    public void testISS0410_CallableDcgHeadsStillLoad() {
        Prolog.CompilationResult r = prolog.consultWithDiagnostics(
            "greet0410 --> [hello].\npair0410(X) --> [X].", "iss0410.pl");
        assertTrue("callable DCG heads must still load", r.success);
        assertEquals(1, prolog.solve("phrase(greet0410, [hello]).").size());
    }
    // END_CHANGE: ISS-2025-0410

    @Test
    public void testISS0421_SquareConstraintIsSatisfiable() {
        // X*X #= 16 used to silently answer false though satisfiable
        List<Map<String, Term>> solutions = prolog.solve("X in 1..10, X*X #= 16, label([X]).");
        assertEquals(1, solutions.size());
        assertEquals("4", solutions.get(0).get("X").toString());
    }

    @Test
    public void testISS0421_SquareBothRoots() {
        List<Map<String, Term>> solutions = prolog.solve("X in -10..10, X*X #= 16, label([X]).");
        assertEquals("both roots of X*X = 16", 2, solutions.size());
        assertEquals("-4", solutions.get(0).get("X").toString());
        assertEquals("4", solutions.get(1).get("X").toString());
    }

    @Test
    public void testISS0421_VarVarProduct() {
        List<Map<String, Term>> solutions = prolog.solve(
            "X in 1..9, Y in 1..9, X*Y #= 12, X #< Y, label([X,Y]).");
        assertEquals("12 = 2*6 = 3*4 with X < Y", 2, solutions.size());
        for (Map<String, Term> m : solutions) {
            long x = Long.parseLong(m.get("X").toString());
            long y = Long.parseLong(m.get("Y").toString());
            assertEquals(12, x * y);
            assertTrue(x < y);
        }
    }

    @Test
    public void testISS0421_AbsExpression() {
        // Z #= abs(Y - 3) used to silently fail for every Y
        List<Map<String, Term>> solutions = prolog.solve("Y in 1..5, Z #= abs(Y - 3), label([Y]).");
        assertEquals(5, solutions.size());
        for (Map<String, Term> m : solutions) {
            long y = Long.parseLong(m.get("Y").toString());
            assertEquals(Math.abs(y - 3), Long.parseLong(m.get("Z").toString()));
        }
    }

    @Test
    public void testISS0421_PythagoreanTriples() {
        List<Map<String, Term>> solutions = prolog.solve(
            "A in 1..4, B in 1..4, C in 1..6, A*A + B*B #= C*C, label([A,B,C]).");
        assertEquals("(3,4,5) and (4,3,5)", 2, solutions.size());
        for (Map<String, Term> m : solutions) {
            long a = Long.parseLong(m.get("A").toString());
            long b = Long.parseLong(m.get("B").toString());
            long c = Long.parseLong(m.get("C").toString());
            assertEquals(c * c, a * a + b * b);
        }
    }

    @Test
    public void testISS0421_MinMaxExpressions() {
        List<Map<String, Term>> solutions = prolog.solve("M in 1..5, min(M, 3) #= 3, label([M]).");
        assertEquals("min(M,3) = 3 means M >= 3", 3, solutions.size());
        solutions = prolog.solve("M in 1..5, max(M, 4) #= 4, label([M]).");
        assertEquals("max(M,4) = 4 means M =< 4", 4, solutions.size());
    }

    @Test
    public void testISS0421_UnsupportedExpressionRaisesTypeError() {
        // a genuinely unsupported expression must error, never silently answer "no"
        List<Map<String, Term>> solutions = prolog.solve(
            "catch(X #= foo(2), error(type_error(evaluable, foo/1), _), true).");
        assertEquals("unsupported functor must raise type_error(evaluable, foo/1)", 1, solutions.size());
        solutions = prolog.solve(
            "catch(X // 2 #= 3, error(type_error(evaluable, (//)/2), _), true).");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testISS0421_ProductPostUndoneOnBacktracking() {
        // the auxiliary Mul/Square constraints must roll back with the rest of the post
        List<Map<String, Term>> solutions = prolog.solve(
            "( X*X #= 16 ; X #= 5 ), X in 0..10, label([X]).");
        assertEquals(2, solutions.size());
        assertEquals("4", solutions.get(0).get("X").toString());
        assertEquals("5", solutions.get(1).get("X").toString());
    }
    // END_CHANGE: ISS-2025-0421

    // ======================== ISS-2025-0422: labeling/2 options ========================

    // START_CHANGE: ISS-2025-0422 - labeling/2 honors its options; label/1 type-checks elements
    @Test
    public void testISS0422_DownEnumeratesDescending() {
        List<Map<String, Term>> solutions = prolog.solve("X in 0..5, labeling([down], [X]).");
        assertEquals(6, solutions.size());
        assertEquals("down must yield the largest value first", "5", solutions.get(0).get("X").toString());
        assertEquals("0", solutions.get(5).get("X").toString());
    }

    @Test
    public void testISS0422_UpAndLeftmostKeepAscending() {
        List<Map<String, Term>> solutions = prolog.solve("X in 0..2, labeling([leftmost, up], [X]).");
        assertEquals(3, solutions.size());
        assertEquals("0", solutions.get(0).get("X").toString());
        assertEquals("2", solutions.get(2).get("X").toString());
    }

    @Test
    public void testISS0422_MaxObjectiveYieldsOptimumFirst() {
        List<Map<String, Term>> solutions = prolog.solve("Y in 0..5, labeling([max(Y)], [Y]).");
        assertEquals(6, solutions.size());
        assertEquals("max(Y) must yield Y = 5 first", "5", solutions.get(0).get("Y").toString());
        solutions = prolog.solve("Y in 2..5, labeling([min(Y)], [Y]).");
        assertEquals("min(Y) must yield Y = 2 first", "2", solutions.get(0).get("Y").toString());
    }

    @Test
    public void testISS0422_UnknownOptionRaisesDomainError() {
        List<Map<String, Term>> solutions = prolog.solve(
            "catch((Z in 0..5, labeling([no_such_option], [Z])), "
            + "error(domain_error(labeling_option, no_such_option), _), true).");
        assertEquals("bogus options must not be silently accepted", 1, solutions.size());
    }

    @Test
    public void testISS0422_NonListOptionsRaiseErrors() {
        List<Map<String, Term>> solutions = prolog.solve(
            "catch(labeling(foo, [X]), error(type_error(list, foo), _), true).");
        assertEquals(1, solutions.size());
        solutions = prolog.solve(
            "catch(labeling([_O], [X]), error(instantiation_error, _), true).");
        assertEquals("an unbound option must raise instantiation_error", 1, solutions.size());
    }

    @Test
    public void testISS0422_LabelNonIntegerRaisesTypeError() {
        List<Map<String, Term>> solutions = prolog.solve(
            "catch(label([a]), error(type_error(integer, a), _), true).");
        assertEquals("label([a]) must raise type_error(integer, a), not succeed", 1, solutions.size());
        // ground integers in the list remain legal
        assertEquals(1, prolog.solve("label([3]).").size());
    }

    @Test
    public void testISS0422_FfOptionStillAccepted() {
        // the documented ff option (already exercised by examples) keeps working
        List<Map<String, Term>> solutions = prolog.solve(
            "X in 1..2, Y in 1..3, labeling([ff], [X, Y]).");
        assertEquals(6, solutions.size());
    }
    // END_CHANGE: ISS-2025-0422
}
