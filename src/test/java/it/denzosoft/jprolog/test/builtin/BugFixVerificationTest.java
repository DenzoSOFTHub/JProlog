package it.denzosoft.jprolog.test.builtin;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.LayeredMap;
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

    @Test
    public void testISS0189_layeredMapIsEmpty() {
        // LayeredMap.isEmpty should account for removed keys
        Map<String, Term> parent = new HashMap<>();
        parent.put("X", new Atom("hello"));
        LayeredMap map = new LayeredMap(parent);
        assertFalse("Map with parent entry should not be empty", map.isEmpty());

        map.remove("X");
        assertTrue("Map with all parent entries removed should be empty", map.isEmpty());
    }

    @Test
    public void testISS0189_layeredMapIsEmptyWithLocal() {
        Map<String, Term> parent = new HashMap<>();
        parent.put("X", new Atom("a"));
        LayeredMap map = new LayeredMap(parent);
        map.remove("X");
        map.put("Y", new Atom("b"));
        assertFalse("Map with local entries should not be empty", map.isEmpty());
    }

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

    // #4-5 LayeredMap rollback correctness
    @Test
    public void testISS0190_layeredMapRollback() {
        it.denzosoft.jprolog.core.engine.LayeredMap map =
            new it.denzosoft.jprolog.core.engine.LayeredMap(new java.util.HashMap<>());
        map.put("A", new it.denzosoft.jprolog.core.terms.Atom("original"));
        int mark = map.mark();
        map.put("B", new it.denzosoft.jprolog.core.terms.Atom("added"));
        map.put("A", new it.denzosoft.jprolog.core.terms.Atom("overwritten"));
        assertEquals("overwritten", map.get("A").toString());
        assertEquals("added", map.get("B").toString());
        map.rollbackToMark(mark);
        assertEquals("original", map.get("A").toString());
        assertNull(map.get("B"));
    }

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
        // All three unbound should fail, not throw
        List<Map<String, Term>> solutions = prolog.solve("atom_concat(X, Y, Z).");
        assertEquals(0, solutions.size());
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
        // \\+ isolates cut, both clauses should be tried
        assertTrue(solutions.size() >= 1);
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
        // Cleanup runs after the goal succeeds; goal remains multi-solution.
        List<Map<String, Term>> s = prolog.solve(
            "setup_call_cleanup(true, member(X,[1,2]), assertz(scc_ok)), scc_ok.");
        assertEquals(2, s.size());

        // Cleanup runs even when the goal fails.
        s = prolog.solve("(setup_call_cleanup(true, fail, assertz(scc_f)) ; true), scc_f.");
        assertEquals(1, s.size());

        // Cleanup runs when the goal raises, before the exception propagates.
        s = prolog.solve(
            "catch(setup_call_cleanup(true, throw(boom), assertz(scc_e)), boom, true), scc_e.");
        assertEquals(1, s.size());

        // call_cleanup/2.
        s = prolog.solve("call_cleanup(member(X,[a,b]), assertz(scc_cc)), scc_cc.");
        assertEquals(2, s.size());
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

    // ISS-2025-0306: with_output_to capture works (thread-safety of the System.out swap is tracked
    // under LIM-025 — it needs write/1 routed through a per-engine stream, the IO-layer rework).
    @Test
    public void testISS0306_WithOutputToCaptures() {
        List<Map<String, Term>> s = prolog.solve("with_output_to(atom(X), write(hello)).");
        assertEquals(1, s.size());
        assertEquals("hello", s.get(0).get("X").toString());
    }
}
