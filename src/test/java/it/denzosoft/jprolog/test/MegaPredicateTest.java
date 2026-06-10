package it.denzosoft.jprolog.test;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;

/**
 * Mega test suite covering ALL implemented predicates in JProlog.
 * Tests parsing, semantics, and edge cases for 107 built-in predicates.
 *
 * NOTE: Tests marked with comments like "PARSING_ISSUE" or "OPT" indicate
 * problems or optimization opportunities discovered during testing.
 */
public class MegaPredicateTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    // ===================================================================
    // 1. UNIFICATION PREDICATES
    // ===================================================================

    @Test
    public void testUnify() {
        // =/2 basic
        List<Map<String, Term>> s = prolog.solve("X = hello.");
        assertEquals(1, s.size());
        assertEquals("hello", s.get(0).get("X").toString());

        // Compound unification
        s = prolog.solve("f(X, b) = f(a, Y).");
        assertEquals(1, s.size());
        assertEquals("a", s.get(0).get("X").toString());
        assertEquals("b", s.get(0).get("Y").toString());

        // List unification
        s = prolog.solve("[H|T] = [1,2,3].");
        assertEquals(1, s.size());
        assertEquals("1", s.get(0).get("H").toString());

        // Fail case
        s = prolog.solve("a = b.");
        assertTrue(s.isEmpty());
    }

    @Test
    public void testNotUnify() {
        // \=/2
        List<Map<String, Term>> s = prolog.solve("a \\= b.");
        assertEquals(1, s.size());

        s = prolog.solve("a \\= a.");
        assertTrue(s.isEmpty());

        // Variables
        s = prolog.solve("X = hello, X \\= world.");
        assertEquals(1, s.size());
    }

    @Test
    public void testUnifyWithOccursCheck() {
        List<Map<String, Term>> s = prolog.solve("unify_with_occurs_check(f(a), f(a)).");
        assertEquals(1, s.size());

        s = prolog.solve("unify_with_occurs_check(a, b).");
        assertTrue(s.isEmpty());
    }

    // ===================================================================
    // 2. TYPE CHECKING PREDICATES (13)
    // ===================================================================

    @Test
    public void testVarNonvar() {
        // var/1 - unbound
        List<Map<String, Term>> s = prolog.solve("var(X).");
        assertEquals(1, s.size());

        // var/1 - bound should fail
        s = prolog.solve("X = hello, var(X).");
        assertTrue(s.isEmpty());

        // nonvar/1 - atom
        s = prolog.solve("nonvar(hello).");
        assertEquals(1, s.size());

        // nonvar/1 - bound variable
        s = prolog.solve("X = 42, nonvar(X).");
        assertEquals(1, s.size());

        // nonvar/1 - unbound should fail
        s = prolog.solve("nonvar(X).");
        assertTrue(s.isEmpty());
    }

    @Test
    public void testAtomCheck() {
        assertEquals(1, prolog.solve("atom(hello).").size());
        assertEquals(1, prolog.solve("atom(a).").size());
        assertTrue(prolog.solve("atom(42).").isEmpty());
        assertTrue(prolog.solve("atom(f(x)).").isEmpty());
        assertTrue(prolog.solve("atom([1,2]).").isEmpty());
        // Empty list is an atom in ISO Prolog
        assertEquals(1, prolog.solve("atom([]).").size());
    }

    @Test
    public void testIntegerFloat() {
        assertEquals(1, prolog.solve("integer(42).").size());
        assertTrue(prolog.solve("integer(3.14).").isEmpty());
        assertTrue(prolog.solve("integer(hello).").isEmpty());

        assertEquals(1, prolog.solve("float(3.14).").size());
        assertTrue(prolog.solve("float(42).").isEmpty());
        assertTrue(prolog.solve("float(hello).").isEmpty());
    }

    @Test
    public void testAtomicCompoundNumber() {
        // atomic/1
        assertEquals(1, prolog.solve("atomic(hello).").size());
        assertEquals(1, prolog.solve("atomic(42).").size());
        assertTrue(prolog.solve("atomic(f(x)).").isEmpty());

        // compound/1
        assertEquals(1, prolog.solve("compound(f(x)).").size());
        assertEquals(1, prolog.solve("compound([1,2]).").size());
        assertTrue(prolog.solve("compound(hello).").isEmpty());
        assertTrue(prolog.solve("compound(42).").isEmpty());

        // number/1
        assertEquals(1, prolog.solve("number(42).").size());
        assertEquals(1, prolog.solve("number(3.14).").size());
        assertTrue(prolog.solve("number(hello).").isEmpty());
    }

    @Test
    public void testCallableCheck() {
        assertEquals(1, prolog.solve("callable(hello).").size());
        assertEquals(1, prolog.solve("callable(f(x)).").size());
        assertTrue(prolog.solve("callable(42).").isEmpty());
    }

    @Test
    public void testIsListCheck() {
        assertEquals(1, prolog.solve("is_list([]).").size());
        assertEquals(1, prolog.solve("is_list([1,2,3]).").size());
        assertTrue(prolog.solve("is_list(hello).").isEmpty());
        assertTrue(prolog.solve("is_list(42).").isEmpty());
    }

    @Test
    public void testGroundCheck() {
        assertEquals(1, prolog.solve("ground(hello).").size());
        assertEquals(1, prolog.solve("ground(f(a,b)).").size());
        assertEquals(1, prolog.solve("ground([1,2,3]).").size());
        // Unbound variable is not ground
        assertTrue(prolog.solve("ground(X).").isEmpty());
    }

    @Test
    public void testSimpleCheck() {
        assertEquals(1, prolog.solve("simple(hello).").size());
        assertEquals(1, prolog.solve("simple(42).").size());
        assertTrue(prolog.solve("simple(f(x)).").isEmpty());
    }

    // ===================================================================
    // 3. TERM COMPARISON (6)
    // ===================================================================

    @Test
    public void testTermComparison() {
        // Standard term ordering: numbers < atoms < compound terms
        assertEquals(1, prolog.solve("a @< b.").size());
        assertEquals(1, prolog.solve("a @=< a.").size());
        assertEquals(1, prolog.solve("b @> a.").size());
        assertEquals(1, prolog.solve("b @>= a.").size());
        assertEquals(1, prolog.solve("hello == hello.").size());
        assertTrue(prolog.solve("hello == world.").isEmpty());
        assertEquals(1, prolog.solve("hello \\== world.").size());
        assertTrue(prolog.solve("hello \\== hello.").isEmpty());
    }

    // ===================================================================
    // 4. TERM CONSTRUCTION & MANIPULATION (7)
    // ===================================================================

    @Test
    public void testFunctor() {
        // functor/3 decomposition
        List<Map<String, Term>> s = prolog.solve("functor(f(a,b), F, A).");
        assertFalse(s.isEmpty());
        assertEquals("f", s.get(0).get("F").toString());
        assertEquals("2", s.get(0).get("A").toString());

        // functor/3 for atom
        s = prolog.solve("functor(hello, F, A).");
        assertFalse(s.isEmpty());
        assertEquals("hello", s.get(0).get("F").toString());
        assertEquals("0", s.get(0).get("A").toString());
    }

    @Test
    public void testArg() {
        List<Map<String, Term>> s = prolog.solve("arg(1, f(a,b,c), X).");
        assertFalse(s.isEmpty());
        assertEquals("a", s.get(0).get("X").toString());

        s = prolog.solve("arg(2, f(a,b,c), X).");
        assertFalse(s.isEmpty());
        assertEquals("b", s.get(0).get("X").toString());
    }

    @Test
    public void testUniv() {
        // =../2 decomposition
        List<Map<String, Term>> s = prolog.solve("f(a,b) =.. X.");
        assertFalse(s.isEmpty());

        // =../2 construction
        s = prolog.solve("X =.. [g, 1, 2].");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testCopyTerm() {
        List<Map<String, Term>> s = prolog.solve("copy_term(f(X,Y), Copy), var(X).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testCompare() {
        List<Map<String, Term>> s = prolog.solve("compare(Order, a, b).");
        assertFalse(s.isEmpty());
        assertEquals("<", s.get(0).get("Order").toString());

        s = prolog.solve("compare(Order, b, a).");
        assertFalse(s.isEmpty());
        assertEquals(">", s.get(0).get("Order").toString());

        s = prolog.solve("compare(Order, a, a).");
        assertFalse(s.isEmpty());
        assertEquals("=", s.get(0).get("Order").toString());
    }

    @Test
    public void testSubsumesTerm() {
        // f(X) subsumes f(a) because X can be instantiated to a
        List<Map<String, Term>> s = prolog.solve("subsumes_term(f(X), f(a)).");
        assertFalse(s.isEmpty());
    }

    // ===================================================================
    // 5. ARITHMETIC EVALUATION & COMPARISON (7)
    // ===================================================================

    @Test
    public void testIsBasic() {
        List<Map<String, Term>> s = prolog.solve("X is 3 + 4.");
        assertEquals("7", s.get(0).get("X").toString());

        s = prolog.solve("X is 10 - 3.");
        assertEquals("7", s.get(0).get("X").toString());

        s = prolog.solve("X is 3 * 4.");
        assertEquals("12", s.get(0).get("X").toString());

        s = prolog.solve("X is 10 / 2.");
        assertEquals("5", s.get(0).get("X").toString());

        s = prolog.solve("X is 7 mod 3.");
        assertEquals("1", s.get(0).get("X").toString());

        s = prolog.solve("X is 7 rem 3.");
        assertEquals("1", s.get(0).get("X").toString());

        s = prolog.solve("X is 7 // 2.");
        assertEquals("3", s.get(0).get("X").toString());

        s = prolog.solve("X is 2 ** 10.");
        assertEquals("1024.0", s.get(0).get("X").toString()); // (**)/2 is float power, ISO §9.3.1 (ISS-2025-0247)
    }

    @Test
    public void testIsWithFunctions() {
        // abs, max, min, sign
        List<Map<String, Term>> s = prolog.solve("X is max(3, 7).");
        assertEquals("7", s.get(0).get("X").toString());

        s = prolog.solve("X is min(3, 7).");
        assertEquals("3", s.get(0).get("X").toString());
    }

    @Test
    public void testIsBitwise() {
        // Bitwise operations
        List<Map<String, Term>> s = prolog.solve("X is 5 /\\ 3."); // AND
        assertEquals("1", s.get(0).get("X").toString());

        s = prolog.solve("X is 5 \\/ 3."); // OR
        assertEquals("7", s.get(0).get("X").toString());

        s = prolog.solve("X is 5 xor 3.");
        assertEquals("6", s.get(0).get("X").toString());

        s = prolog.solve("X is 1 << 3."); // left shift
        assertEquals("8", s.get(0).get("X").toString());

        s = prolog.solve("X is 8 >> 2."); // right shift
        assertEquals("2", s.get(0).get("X").toString());
    }

    @Test
    public void testArithmeticComparison() {
        assertEquals(1, prolog.solve("3 + 2 =:= 5.").size());
        assertEquals(1, prolog.solve("3 + 2 =\\= 6.").size());
        assertEquals(1, prolog.solve("3 < 5.").size());
        assertEquals(1, prolog.solve("5 > 3.").size());
        assertEquals(1, prolog.solve("3 =< 3.").size());
        assertEquals(1, prolog.solve("3 >= 3.").size());
        assertTrue(prolog.solve("5 < 3.").isEmpty());
        assertTrue(prolog.solve("3 =:= 4.").isEmpty());
    }

    @Test
    public void testNumberFormats() {
        // Hex
        List<Map<String, Term>> s = prolog.solve("X is 0xFF.");
        assertEquals("255", s.get(0).get("X").toString());

        // Octal
        s = prolog.solve("X is 0o77.");
        assertEquals("63", s.get(0).get("X").toString());

        // Binary
        s = prolog.solve("X is 0b1010.");
        assertEquals("10", s.get(0).get("X").toString());
    }

    @Test
    public void testArithmeticPrecedence() {
        // 2 + 3 * 4 should be 14 (not 20)
        List<Map<String, Term>> s = prolog.solve("X is 2 + 3 * 4.");
        assertEquals("14", s.get(0).get("X").toString());

        // (2 + 3) * 4 should be 20
        s = prolog.solve("X is (2 + 3) * 4.");
        assertEquals("20", s.get(0).get("X").toString());
    }

    // ===================================================================
    // 6. ADVANCED ARITHMETIC (3)
    // ===================================================================

    @Test
    public void testBetween() {
        List<Map<String, Term>> s = prolog.solve("findall(X, between(1, 5, X), Xs).");
        assertFalse(s.isEmpty());
        String list = s.get(0).get("Xs").toString();
        assertTrue(list.contains("1"));
        assertTrue(list.contains("5"));
    }

    @Test
    public void testSucc() {
        List<Map<String, Term>> s = prolog.solve("succ(3, X).");
        assertFalse(s.isEmpty());
        assertEquals("4", s.get(0).get("X").toString());

        s = prolog.solve("succ(X, 5).");
        assertFalse(s.isEmpty());
        assertEquals("4", s.get(0).get("X").toString());
    }

    @Test
    public void testPlus() {
        List<Map<String, Term>> s = prolog.solve("plus(2, 3, X).");
        assertFalse(s.isEmpty());
        assertEquals("5", s.get(0).get("X").toString());

        s = prolog.solve("plus(2, X, 5).");
        assertFalse(s.isEmpty());
        assertEquals("3", s.get(0).get("X").toString());
    }

    // ===================================================================
    // 7. LIST OPERATIONS (10)
    // ===================================================================

    @Test
    public void testAppend() {
        List<Map<String, Term>> s = prolog.solve("append([1,2], [3,4], X).");
        assertFalse(s.isEmpty());
        assertEquals("[1, 2, 3, 4]", s.get(0).get("X").toString());
    }

    @Test
    public void testLength() {
        List<Map<String, Term>> s = prolog.solve("length([a,b,c], X).");
        assertFalse(s.isEmpty());
        assertEquals("3", s.get(0).get("X").toString());

        s = prolog.solve("length([], X).");
        assertFalse(s.isEmpty());
        assertEquals("0", s.get(0).get("X").toString());
    }

    @Test
    public void testMember() {
        List<Map<String, Term>> s = prolog.solve("member(2, [1,2,3]).");
        assertFalse(s.isEmpty());

        // member with backtracking
        s = prolog.solve("findall(X, member(X, [a,b,c]), Xs).");
        assertFalse(s.isEmpty());
        assertEquals("[a, b, c]", s.get(0).get("Xs").toString());

        // member should fail for non-member
        assertTrue(prolog.solve("member(4, [1,2,3]).").isEmpty());
    }

    @Test
    public void testNth() {
        List<Map<String, Term>> s = prolog.solve("nth0(0, [a,b,c], X).");
        assertFalse(s.isEmpty());
        assertEquals("a", s.get(0).get("X").toString());

        s = prolog.solve("nth1(1, [a,b,c], X).");
        assertFalse(s.isEmpty());
        assertEquals("a", s.get(0).get("X").toString());

        s = prolog.solve("nth0(2, [a,b,c], X).");
        assertFalse(s.isEmpty());
        assertEquals("c", s.get(0).get("X").toString());
    }

    @Test
    public void testSort() {
        List<Map<String, Term>> s = prolog.solve("sort([3,1,2,1], X).");
        assertFalse(s.isEmpty());
        assertEquals("[1, 2, 3]", s.get(0).get("X").toString());
    }

    @Test
    public void testMsort() {
        List<Map<String, Term>> s = prolog.solve("msort([3,1,2,1], X).");
        assertFalse(s.isEmpty());
        assertEquals("[1, 1, 2, 3]", s.get(0).get("X").toString());
    }

    @Test
    public void testReverse() {
        List<Map<String, Term>> s = prolog.solve("reverse([1,2,3], X).");
        assertFalse(s.isEmpty());
        assertEquals("[3, 2, 1]", s.get(0).get("X").toString());
    }

    @Test
    public void testSelect() {
        List<Map<String, Term>> s = prolog.solve("select(2, [1,2,3], X).");
        assertFalse(s.isEmpty());
        assertEquals("[1, 3]", s.get(0).get("X").toString());

        // select with backtracking
        s = prolog.solve("findall(H-R, select(H, [a,b,c], R), Xs).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testKeysort() {
        List<Map<String, Term>> s = prolog.solve("keysort([b-2, a-1, c-3], X).");
        assertFalse(s.isEmpty());
    }

    // ===================================================================
    // 8. CONTROL FLOW (7)
    // ===================================================================

    @Test
    public void testCut() {
        prolog.consult("first(X) :- member(X, [a,b,c]), !.");
        List<Map<String, Term>> s = prolog.solve("first(X).");
        assertEquals(1, s.size());
        assertEquals("a", s.get(0).get("X").toString());
    }

    @Test
    public void testRepeat() {
        // repeat/0 should generate at least 2 solutions
        prolog.consult("test_repeat(X) :- repeat, X = done, !.");
        List<Map<String, Term>> s = prolog.solve("test_repeat(X).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testNegationAsFailure() {
        assertEquals(1, prolog.solve("\\+ fail.").size());
        assertTrue(prolog.solve("\\+ true.").isEmpty());

        prolog.consult("likes(mary, wine).");
        assertEquals(1, prolog.solve("\\+ likes(mary, beer).").size());
        assertTrue(prolog.solve("\\+ likes(mary, wine).").isEmpty());
    }

    @Test
    public void testIfThenElse() {
        // Direct
        List<Map<String, Term>> s = prolog.solve("(5 > 0 -> X = positive ; X = negative).");
        assertEquals(1, s.size());
        assertEquals("positive", s.get(0).get("X").toString());

        s = prolog.solve("(-1 > 0 -> X = positive ; X = negative).");
        assertEquals(1, s.size());
        assertEquals("negative", s.get(0).get("X").toString());
    }

    @Test
    public void testIfThenElseInRules() {
        prolog.consult("classify(X, pos) :- X > 0.");
        prolog.consult("classify(X, zero) :- X =:= 0.");
        prolog.consult("classify(X, neg) :- X < 0.");

        List<Map<String, Term>> s = prolog.solve("classify(5, R).");
        assertFalse(s.isEmpty());
        assertEquals("pos", s.get(0).get("R").toString());

        s = prolog.solve("classify(-3, R).");
        assertFalse(s.isEmpty());
        assertEquals("neg", s.get(0).get("R").toString());
    }

    @Test
    public void testDisjunction() {
        prolog.consult("color(red).");
        prolog.consult("color(blue).");
        prolog.consult("shape(circle).");

        List<Map<String, Term>> s = prolog.solve("(color(X) ; shape(X)).");
        assertTrue(s.size() >= 3);
    }

    // ===================================================================
    // 9. I/O PREDICATES (basic testing - no stdin)
    // ===================================================================

    @Test
    public void testWriteNl() {
        // write/1 and nl/0 should succeed without error
        List<Map<String, Term>> s = prolog.solve("write(hello).");
        assertEquals(1, s.size());

        s = prolog.solve("nl.");
        assertEquals(1, s.size());

        s = prolog.solve("writeln(test).");
        assertEquals(1, s.size());
    }

    // ===================================================================
    // 10. ATOM OPERATIONS (3)
    // ===================================================================

    @Test
    public void testAtomLength() {
        List<Map<String, Term>> s = prolog.solve("atom_length(hello, X).");
        assertEquals("5", s.get(0).get("X").toString());

        s = prolog.solve("atom_length('', X).");
        assertEquals("0", s.get(0).get("X").toString());
    }

    @Test
    public void testAtomConcat() {
        List<Map<String, Term>> s = prolog.solve("atom_concat(hello, world, X).");
        assertEquals("helloworld", s.get(0).get("X").toString());

        s = prolog.solve("atom_concat('', hello, X).");
        assertEquals("hello", s.get(0).get("X").toString());
    }

    @Test
    public void testSubAtom() {
        List<Map<String, Term>> s = prolog.solve("sub_atom(abcdef, 2, 3, _, X).");
        assertFalse(s.isEmpty());
        assertEquals("cde", s.get(0).get("X").toString());
    }

    // ===================================================================
    // 11. TYPE CONVERSION (6)
    // ===================================================================

    @Test
    public void testAtomNumber() {
        List<Map<String, Term>> s = prolog.solve("atom_number('42', X).");
        assertFalse(s.isEmpty());
        assertEquals("42", s.get(0).get("X").toString());
    }

    @Test
    public void testAtomChars() {
        List<Map<String, Term>> s = prolog.solve("atom_chars(hello, X).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testAtomCodes() {
        List<Map<String, Term>> s = prolog.solve("atom_codes(hello, X).");
        assertFalse(s.isEmpty());
        String result = s.get(0).get("X").toString();
        assertTrue(result.contains("104")); // 'h'
        assertTrue(result.contains("111")); // 'o'
    }

    @Test
    public void testNumberChars() {
        List<Map<String, Term>> s = prolog.solve("number_chars(42, X).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testNumberCodes() {
        List<Map<String, Term>> s = prolog.solve("number_codes(42, X).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testCharCode() {
        List<Map<String, Term>> s = prolog.solve("char_code(a, X).");
        assertEquals("97", s.get(0).get("X").toString());
    }

    // ===================================================================
    // 12. STRING OPERATIONS (7)
    // ===================================================================

    @Test
    public void testStringLength() {
        List<Map<String, Term>> s = prolog.solve("string_length(\"hello\", X).");
        assertFalse(s.isEmpty());
        assertEquals("5", s.get(0).get("X").toString());
    }

    @Test
    public void testStringConcat() {
        List<Map<String, Term>> s = prolog.solve("string_concat(\"hello\", \" world\", X).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testAtomString() {
        List<Map<String, Term>> s = prolog.solve("atom_string(hello, X).");
        assertFalse(s.isEmpty());
    }

    // ===================================================================
    // 13. COLLECTION PREDICATES (3)
    // ===================================================================

    @Test
    public void testFindall() {
        prolog.consult("fruit(apple).");
        prolog.consult("fruit(banana).");
        prolog.consult("fruit(cherry).");

        List<Map<String, Term>> s = prolog.solve("findall(X, fruit(X), Xs).");
        assertFalse(s.isEmpty());
        String result = s.get(0).get("Xs").toString();
        assertTrue(result.contains("apple"));
        assertTrue(result.contains("banana"));
        assertTrue(result.contains("cherry"));

        // findall with no solutions returns empty list
        // START_CHANGE: ISS-2025-0347 - declare vegetable/1 dynamic: an UNDECLARED unknown
        // procedure now raises existence_error per ISO 7.7.7 (unknown=error), as in SWI.
        prolog.consult(":- dynamic(vegetable/1).");
        // END_CHANGE: ISS-2025-0347
        s = prolog.solve("findall(X, vegetable(X), Xs).");
        assertFalse(s.isEmpty());
        assertEquals("[]", s.get(0).get("Xs").toString());
    }

    @Test
    public void testBagof() {
        prolog.consult("age(peter, 7).");
        prolog.consult("age(ann, 11).");
        prolog.consult("age(pat, 8).");

        List<Map<String, Term>> s = prolog.solve("bagof(X, age(X, _), Xs).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testSetof() {
        prolog.consult("score(alice, 90).");
        prolog.consult("score(bob, 85).");
        prolog.consult("score(alice, 95).");

        List<Map<String, Term>> s = prolog.solve("setof(X, Y^score(X, Y), Xs).");
        assertFalse(s.isEmpty());
    }

    // ===================================================================
    // 14. EXCEPTION HANDLING (2)
    // ===================================================================

    @Test
    public void testCatchThrow() {
        List<Map<String, Term>> s = prolog.solve("catch(throw(my_error), my_error, true).");
        assertFalse(s.isEmpty());

        // catch without exception
        s = prolog.solve("catch(true, _, fail).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testCatchWithRecovery() {
        prolog.consult("safe_div(X, Y, R) :- catch((R is X / Y), _, R = error).");
        List<Map<String, Term>> s = prolog.solve("safe_div(10, 2, R).");
        assertFalse(s.isEmpty());
        assertEquals("5", s.get(0).get("R").toString());
    }

    // ===================================================================
    // 15. META-PREDICATES (4)
    // ===================================================================

    @Test
    public void testCall() {
        prolog.consult("my_pred(hello).");
        List<Map<String, Term>> s = prolog.solve("call(my_pred(X)).");
        assertFalse(s.isEmpty());
        assertEquals("hello", s.get(0).get("X").toString());
    }

    @Test
    public void testOnce() {
        prolog.consult("multi(1).");
        prolog.consult("multi(2).");
        prolog.consult("multi(3).");

        List<Map<String, Term>> s = prolog.solve("once(multi(X)).");
        assertEquals(1, s.size());
        assertEquals("1", s.get(0).get("X").toString());
    }

    @Test
    public void testIgnore() {
        // ignore/1 always succeeds
        List<Map<String, Term>> s = prolog.solve("ignore(fail).");
        assertFalse(s.isEmpty());

        s = prolog.solve("ignore(true).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testForall() {
        prolog.consult("even(2).");
        prolog.consult("even(4).");
        prolog.consult("even(6).");

        List<Map<String, Term>> s = prolog.solve("forall(even(X), number(X)).");
        assertFalse(s.isEmpty());
    }

    // ===================================================================
    // 16. DYNAMIC DATABASE (8)
    // ===================================================================

    @Test
    public void testAssertRetract() {
        prolog.solve("assertz(dynamic_fact(hello)).");
        List<Map<String, Term>> s = prolog.solve("dynamic_fact(X).");
        assertFalse(s.isEmpty());
        assertEquals("hello", s.get(0).get("X").toString());

        prolog.solve("retract(dynamic_fact(hello)).");
        s = prolog.solve("dynamic_fact(X).");
        assertTrue(s.isEmpty());
    }

    @Test
    public void testAssertaAssertz() {
        prolog.solve("assertz(order_test(1)).");
        prolog.solve("assertz(order_test(2)).");
        prolog.solve("asserta(order_test(0)).");

        List<Map<String, Term>> s = prolog.solve("findall(X, order_test(X), Xs).");
        assertFalse(s.isEmpty());
        String list = s.get(0).get("Xs").toString();
        // 0 should come first (asserta), then 1, then 2 (assertz)
        int pos0 = list.indexOf("0");
        int pos1 = list.indexOf("1");
        int pos2 = list.indexOf("2");
        assertTrue(pos0 < pos1);
        assertTrue(pos1 < pos2);
    }

    @Test
    public void testRetractall() {
        prolog.solve("assertz(temp(1)).");
        prolog.solve("assertz(temp(2)).");
        prolog.solve("assertz(temp(3)).");
        prolog.solve("retractall(temp(_)).");

        List<Map<String, Term>> s = prolog.solve("temp(X).");
        assertTrue(s.isEmpty());
    }

    // ===================================================================
    // 17. CHARACTER PREDICATES (2)
    // ===================================================================

    @Test
    public void testCharCodeBidi() {
        List<Map<String, Term>> s = prolog.solve("char_code(a, X).");
        assertEquals("97", s.get(0).get("X").toString());

        s = prolog.solve("char_code(z, X).");
        assertEquals("122", s.get(0).get("X").toString());
    }

    // ===================================================================
    // 18. CHARACTER & STRING PROCESSING (4)
    // ===================================================================

    @Test
    public void testUpcaseDowncase() {
        List<Map<String, Term>> s = prolog.solve("upcase_atom(hello, X).");
        assertFalse(s.isEmpty());
        assertEquals("HELLO", s.get(0).get("X").toString());

        s = prolog.solve("downcase_atom('HELLO', X).");
        assertFalse(s.isEmpty());
        assertEquals("hello", s.get(0).get("X").toString());
    }

    // ===================================================================
    // 19. FAMOUS PROLOG PROGRAMS
    // ===================================================================

    @Test
    public void testFactorial() {
        prolog.consult("factorial(0, 1).");
        prolog.consult("factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.");

        List<Map<String, Term>> s = prolog.solve("factorial(6, X).");
        assertFalse(s.isEmpty());
        assertEquals("720", s.get(0).get("X").toString());
    }

    @Test
    public void testFibonacci() {
        prolog.consult("fib(0, 0).");
        prolog.consult("fib(1, 1).");
        prolog.consult("fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.");

        List<Map<String, Term>> s = prolog.solve("fib(10, X).");
        assertFalse(s.isEmpty());
        assertEquals("55", s.get(0).get("X").toString());
    }

    @Test
    public void testTowerOfHanoi() {
        prolog.consult("hanoi(1, From, To, _) :- true.");
        prolog.consult("hanoi(N, From, To, Via) :- N > 1, N1 is N - 1, hanoi(N1, From, Via, To), hanoi(N1, Via, To, From).");

        List<Map<String, Term>> s = prolog.solve("hanoi(4, left, right, center).");
        assertFalse(s.isEmpty());
    }

    @Test
    public void testQueen4() {
        prolog.consult("permute([], []).");
        prolog.consult("permute(List, [H|Perm]) :- select(H, List, Rest), permute(Rest, Perm).");
        prolog.consult("safe([]).");
        prolog.consult("safe([Q|Qs]) :- no_attack(Q, Qs, 1), safe(Qs).");
        prolog.consult("no_attack(_, [], _).");
        prolog.consult("no_attack(Q, [Q1|Qs], D) :- Diff is Q - Q1, AbsDiff is Diff * Diff, D2 is D * D, AbsDiff =\\= D2, D1 is D + 1, no_attack(Q, Qs, D1).");
        prolog.consult("queen4(Qs) :- permute([1,2,3,4], Qs), safe(Qs).");

        List<Map<String, Term>> s = prolog.solve("queen4(Qs).");
        assertEquals(2, s.size()); // 4-queens has exactly 2 solutions
    }

    @Test
    public void testListReversalAccumulator() {
        prolog.consult("rev([], Acc, Acc).");
        prolog.consult("rev([H|T], Acc, R) :- rev(T, [H|Acc], R).");
        prolog.consult("my_reverse(L, R) :- rev(L, [], R).");

        List<Map<String, Term>> s = prolog.solve("my_reverse([1,2,3,4,5], X).");
        assertFalse(s.isEmpty());
        assertEquals("[5, 4, 3, 2, 1]", s.get(0).get("X").toString());
    }

    @Test
    public void testMapColoring() {
        prolog.consult("color(red).");
        prolog.consult("color(green).");
        prolog.consult("color(blue).");
        prolog.consult("adjacent(X, Y) :- color(X), color(Y), X \\= Y.");
        // Color a simple map: 3 countries, each pair adjacent
        prolog.consult("color_map(A, B, C) :- adjacent(A, B), adjacent(A, C), adjacent(B, C).");

        List<Map<String, Term>> s = prolog.solve("color_map(A, B, C).");
        assertFalse(s.isEmpty());
        // Should have 6 solutions (3! permutations of 3 colors)
        assertEquals(6, s.size());
    }

    @Test
    public void testFlatten() {
        prolog.consult("my_flatten([], []).");
        prolog.consult("my_flatten([H|T], Flat) :- is_list(H), !, my_flatten(H, FH), my_flatten(T, FT), append(FH, FT, Flat).");
        prolog.consult("my_flatten([H|T], [H|FT]) :- my_flatten(T, FT).");

        List<Map<String, Term>> s = prolog.solve("my_flatten([1,[2,3],[4,[5]]], X).");
        assertFalse(s.isEmpty());
        assertEquals("[1, 2, 3, 4, 5]", s.get(0).get("X").toString());
    }

    @Test
    public void testGCD() {
        prolog.consult("gcd(X, 0, X) :- X > 0.");
        prolog.consult("gcd(X, Y, G) :- Y > 0, R is X mod Y, gcd(Y, R, G).");

        List<Map<String, Term>> s = prolog.solve("gcd(12, 8, X).");
        assertFalse(s.isEmpty());
        assertEquals("4", s.get(0).get("X").toString());

        s = prolog.solve("gcd(100, 75, X).");
        assertFalse(s.isEmpty());
        assertEquals("25", s.get(0).get("X").toString());
    }

    @Test
    public void testQuicksort() {
        prolog.consult("qsort([], []).");
        prolog.consult("qsort([H|T], Sorted) :- partition(H, T, Less, Greater), qsort(Less, SortedLess), qsort(Greater, SortedGreater), append(SortedLess, [H|SortedGreater], Sorted).");
        prolog.consult("partition(_, [], [], []).");
        prolog.consult("partition(Pivot, [H|T], [H|Less], Greater) :- H =< Pivot, partition(Pivot, T, Less, Greater).");
        prolog.consult("partition(Pivot, [H|T], Less, [H|Greater]) :- H > Pivot, partition(Pivot, T, Less, Greater).");

        List<Map<String, Term>> s = prolog.solve("qsort([5,3,8,1,9,2,7], X).");
        assertFalse(s.isEmpty());
        assertEquals("[1, 2, 3, 5, 7, 8, 9]", s.get(0).get("X").toString());
    }

    @Test
    public void testListMaximum() {
        prolog.consult("list_max([X], X).");
        prolog.consult("list_max([H|T], Max) :- list_max(T, TMax), (H >= TMax -> Max = H ; Max = TMax).");

        List<Map<String, Term>> s = prolog.solve("list_max([3,1,4,1,5,9,2,6], X).");
        assertFalse(s.isEmpty());
        assertEquals("9", s.get(0).get("X").toString());
    }

    @Test
    public void testAncestorTransitive() {
        prolog.consult("parent(tom, bob).");
        prolog.consult("parent(tom, liz).");
        prolog.consult("parent(bob, ann).");
        prolog.consult("parent(bob, pat).");
        prolog.consult("parent(pat, jim).");

        prolog.consult("ancestor(X, Y) :- parent(X, Y).");
        prolog.consult("ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).");

        List<Map<String, Term>> s = prolog.solve("ancestor(tom, jim).");
        assertFalse(s.isEmpty());

        s = prolog.solve("findall(X, ancestor(tom, X), Desc).");
        assertFalse(s.isEmpty());
        String desc = s.get(0).get("Desc").toString();
        assertTrue(desc.contains("bob"));
        assertTrue(desc.contains("jim"));
    }

    // ===================================================================
    // 20. ADVANCED PARSING TESTS
    // ===================================================================

    @Test
    public void testNestedArithmetic() {
        // Nested parentheses
        List<Map<String, Term>> s = prolog.solve("X is ((2 + 3) * (4 - 1)).");
        assertEquals("15", s.get(0).get("X").toString());
    }

    @Test
    public void testNegativeNumbers() {
        List<Map<String, Term>> s = prolog.solve("X is -5 + 3.");
        assertEquals("-2", s.get(0).get("X").toString());

        s = prolog.solve("X is 5 + (-3).");
        assertEquals("2", s.get(0).get("X").toString());
    }

    @Test
    public void testQuotedAtoms() {
        List<Map<String, Term>> s = prolog.solve("X = 'hello world'.");
        assertFalse(s.isEmpty());
        assertEquals("hello world", s.get(0).get("X").toString());

        s = prolog.solve("atom_length('hello world', X).");
        assertEquals("11", s.get(0).get("X").toString());
    }

    @Test
    public void testNestedLists() {
        List<Map<String, Term>> s = prolog.solve("X = [[1,2],[3,4],[5]].");
        assertFalse(s.isEmpty());

        s = prolog.solve("length([[1,2],[3,4],[5]], X).");
        assertEquals("3", s.get(0).get("X").toString());
    }

    @Test
    public void testHeadTailPattern() {
        List<Map<String, Term>> s = prolog.solve("[H|T] = [a,b,c,d].");
        assertEquals("a", s.get(0).get("H").toString());

        s = prolog.solve("[A,B|T] = [1,2,3,4,5].");
        assertEquals("1", s.get(0).get("A").toString());
        assertEquals("2", s.get(0).get("B").toString());
    }

    @Test
    public void testOperatorParsing() {
        // Multiple operators in sequence
        List<Map<String, Term>> s = prolog.solve("X is 1 + 2 + 3 + 4.");
        assertEquals("10", s.get(0).get("X").toString());

        // Mixed operators
        s = prolog.solve("X is 2 * 3 + 4 * 5.");
        assertEquals("26", s.get(0).get("X").toString());
    }

    @Test
    public void testCompoundTermParsing() {
        List<Map<String, Term>> s = prolog.solve("X = f(g(a), h(b, c)).");
        assertFalse(s.isEmpty());

        s = prolog.solve("functor(f(g(a), h(b,c)), F, A).");
        assertEquals("f", s.get(0).get("F").toString());
        assertEquals("2", s.get(0).get("A").toString());
    }

    @Test
    public void testEmptyListParsing() {
        List<Map<String, Term>> s = prolog.solve("X = [].");
        assertFalse(s.isEmpty());

        s = prolog.solve("append([], [], X).");
        assertEquals("[]", s.get(0).get("X").toString());
    }

    @Test
    public void testStringParsing() {
        List<Map<String, Term>> s = prolog.solve("X = \"hello\".");
        assertFalse(s.isEmpty());
    }

    // ===================================================================
    // 21. EDGE CASES AND REGRESSION TESTS
    // ===================================================================

    @Test
    public void testDeepRecursion() {
        prolog.consult("sum_to(0, 0).");
        prolog.consult("sum_to(N, S) :- N > 0, N1 is N - 1, sum_to(N1, S1), S is S1 + N.");

        List<Map<String, Term>> s = prolog.solve("sum_to(100, X).");
        assertFalse(s.isEmpty());
        assertEquals("5050", s.get(0).get("X").toString());
    }

    @Test
    public void testMultipleSolutions() {
        prolog.consult("animal(cat).");
        prolog.consult("animal(dog).");
        prolog.consult("animal(fish).");

        List<Map<String, Term>> s = prolog.solve("animal(X).");
        assertEquals(3, s.size());
    }

    @Test
    public void testConjunctionInQuery() {
        prolog.consult("a(1). a(2). a(3).");
        prolog.consult("b(2). b(3). b(4).");

        List<Map<String, Term>> s = prolog.solve("a(X), b(X).");
        assertFalse(s.isEmpty());
        // X should be 2 or 3 (intersection)
        for (Map<String, Term> sol : s) {
            String val = sol.get("X").toString();
            assertTrue(val.equals("2") || val.equals("3"));
        }
    }

    @Test
    public void testVariableBindingChain() {
        // Test that variables get properly resolved through chains
        List<Map<String, Term>> s = prolog.solve("X = Y, Y = Z, Z = hello.");
        assertFalse(s.isEmpty());
        assertEquals("hello", s.get(0).get("X").toString());
    }

    @Test
    public void testComplexUnification() {
        List<Map<String, Term>> s = prolog.solve("f(X, g(Y)) = f(a, g(b)).");
        assertFalse(s.isEmpty());
        assertEquals("a", s.get(0).get("X").toString());
        assertEquals("b", s.get(0).get("Y").toString());
    }
}
