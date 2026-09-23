package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * 4.5 wave P4 — built-in conformance (arithmetic, text, format, I/O, lists, database). One method
 * per defect, named after its ISS id (ISS-2025-0590..0619). The reference is ISO 13211-1 first and
 * SWI-Prolog 9 where ISO is silent (report-production-readiness-2026-09-23.md §8).
 */
public class EngineV45ConformanceTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    /** Asserts the query succeeds (the query carries its own {@code ==} checks). */
    private void ok(String query) {
        List<Map<String, Term>> s = prolog.solve(query + ".");
        assertTrue("expected success: " + query, !s.isEmpty());
    }

    private void no(String query) {
        List<Map<String, Term>> s = prolog.solve(query + ".");
        assertTrue("expected failure: " + query, s.isEmpty());
    }

    /** Asserts {@code goal} raises {@code error(Formal, _)} with Formal unifying {@code formal}. */
    private void err(String goal, String formal) {
        ok("catch((" + goal + "), error(E__, _), true), nonvar(E__), E__ = " + formal);
    }

    private int count(String query) {
        return prolog.solve(query + ".").size();
    }

    /** Output a query prints, through the thread-local capture every built-in honours. */
    private String output(String query) {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(bos, true);
        PrintStream prev = it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            prolog.solve(query + ".");
        } finally {
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prev);
        }
        ps.flush();
        return bos.toString();
    }

    // ------------------------------------------------------------------ P4.1 .. P4.5 arithmetic

    @Test
    public void testISS0590_RoundingFunctionsAndBigIntegerDivision() {
        ok("X is integer(2.5), X == 3");
        ok("X is integer(3.7), X == 4");
        ok("X is integer(-2.5), X == -3");
        ok("X is round(0.49999999999999994), X == 0");
        ok("X is round(4503599627370497.0), X == 4503599627370497");
        ok("X is round(-2.5), X == -3");
        ok("X is round(2.5), X == 3");
        ok("X is truncate(12345678901234567891), X == 12345678901234567891");
        ok("X is integer(12345678901234567891), X == 12345678901234567891");
        ok("X is floor(-12345678901234567891), X == -12345678901234567891");
        ok("X is truncate(-3.7), X == -3");
        ok("X is round(1.0e20), X == 100000000000000000000");
        ok("X is (10^400+1)/10^400, X == 1.0");
        ok("X is 10^400/(3*10^399), X > 3.333, X < 3.334");
        ok("X is 7/2, X == 3.5");
        err("X is 10^400/7", "evaluation_error(float_overflow)");
    }

    @Test
    public void testISS0591_NegativeShiftShiftsTheOtherWay() {
        ok("X is 1 << -1, X == 0");
        ok("X is 8 >> -2, X == 32");
        ok("X is -8 >> 1, X == -4");
        ok("X is -1 >> 100, X == -1");
        ok("X is 1 << 70, X == 1180591620717411303424");
        ok("X is (1 << 70) >> 69, X == 2");
        err("X is 5 >> -10000000000", "resource_error(memory)");
        ok("X is 5 << -10000000000, X == 0");
    }

    @Test
    public void testISS0592_DeepExpressionsEvaluateIteratively() {
        ok("numlist(1, 100000, L), foldl([X,A0,A]>>(A = A0+X), L, 0, E), S is E, S == 5000050000");
        ok("numlist(1, 20000, L), foldl([X,A0,A]>>(A = X+A0), L, 0, E), S is E, S == 200010000");
        ok("numlist(1, 20000, L), foldl([_,A0,A]>>(A = -(A0)), L, 7, E), S is E, S == 7");
        ok("numlist(1, 20000, L), foldl([X,A0,A]>>(A = A0+X), L, 0, E), E < 10^20");
        err("numlist(1, 20000, L), foldl([X,A0,A]>>(A = A0+X), L, foo, E), S is E",
            "type_error(evaluable, foo/0)");
    }

    @Test
    public void testISS0593_PlusIsIntegerOnly() {
        err("plus(1.5, 2.5, _)", "type_error(integer, 1.5)");
        err("plus(_, 1.5, 3)", "type_error(integer, 1.5)");
        err("plus(1, 2, 3.0)", "type_error(integer, 3.0)");
        ok("plus(1, 2, X), X == 3");
        ok("plus(X, 2, 5), X == 3");
        ok("plus(9223372036854775807, 1, X), X == 9223372036854775808");
    }

    @Test
    public void testISS0594_ArithmeticErrorContextAndOneElementList() {
        ok("catch(1 =:= a, error(type_error(evaluable, a/0), C), true), C == '=:=/2'");
        ok("catch(_ < 1, error(instantiation_error, C), true), C == '</2'");
        ok("catch(X is foo, error(type_error(evaluable, foo/0), C), true), C == 'is/2'");
        err("X is [1,2]", "type_error(evaluable, '[|]'/2)");
        ok("X is [3], X == 3");
        ok("X is [3]+1, X == 4");
        ok("X is \"a\", X == 97");
        err("X is []", "type_error(evaluable, []/0)");
    }

    // ------------------------------------------------------------------ P4.7 text

    @Test
    public void testISS0596_StringBuiltinsTakeAnyText() {
        ok("string_concat(1, 2, S), S == \"12\"");
        ok("string_concat(abc, \"def\", S), S == \"abcdef\"");
        err("string_concat(_, _, _)", "instantiation_error");
        err("string_concat(a, _, _)", "instantiation_error");
        err("string_concat(f(x), a, _)", "type_error(_, f(x))");
        ok("string_length(123, L), L == 3");
        ok("string_length(abc, L), L == 3");
        err("string_length(f(x), _)", "type_error(_, f(x))");
        ok("atom_string(42, S), S == \"42\"");
        ok("atom_string(A, 42), A == '42'");
        ok("atom_string(abc, abc)");
        ok("atom_string(A, \"xy\"), A == xy");
        err("atom_string(_, _)", "instantiation_error");
        ok("sub_string(abc, 1, 1, A, S), A == 1, S == \"b\"");
        ok("sub_string(\"hello world\", B, _, 0, world), B == 6");
    }

    @Test
    public void testISS0597_AtomicListConcatModesAndErrors() {
        ok("atomic_list_concat([a,B,c], '-', 'a-x-c'), B == x");
        ok("atomic_list_concat([a,X], '-', 'a-b'), X == b");
        ok("atomic_list_concat(L, '-', 'a-b-c'), L == [a,b,c]");
        ok("atomic_list_concat(L, ', ', 'a, b'), L == [a,b]");
        ok("atomic_list_concat([a,1,\"s\",2.5], X), X == 'a1s2.5'");
        ok("atomic_list_concat([a,b], '-', X), X == 'a-b'");
        no("atomic_list_concat([a,b], '-', 'a-c')");
        err("atomic_list_concat([f(x)], _)", "type_error(atomic, f(x))");
        err("atomic_list_concat([a,_], _)", "instantiation_error");
        err("atomic_list_concat(abc, _)", "type_error(list, abc)");
        err("atomic_list_concat([a|_], '-', _)", "instantiation_error");
        err("atomic_list_concat(_, '', abc)", "domain_error(non_empty_atom, '')");
    }

    @Test
    public void testISS0598_StringUpperLower() {
        ok("string_upper(\"abC\", U), U == \"ABC\"");
        ok("string_lower(abC, L), L == \"abc\"");
        ok("string_upper(123, U), U == \"123\"");
        err("string_upper(_, _)", "instantiation_error");
    }

    @Test
    public void testISS0599_SubAtomCodePointsAndNegativePositions() {
        no("sub_atom(abc, -1, 1, _, _)");
        no("sub_atom(abc, _, -1, _, _)");
        no("sub_atom(abc, _, _, -2, _)");
        err("sub_atom(abc, a, _, _, _)", "type_error(integer, a)");
        ok("sub_atom('a\\x1F600\\b', 1, 1, A, S), A == 1, atom_length(S, 1), atom_codes(S, [128512])");
        ok("sub_atom('a\\x1F600\\b', B, L, A, '\\x1F600\\'), B == 1, L == 1, A == 1");
        ok("findall(X-Y, atom_concat(X, Y, '\\x1F600\\b'), L), length(L, 3)");
        ok("findall(S, sub_atom('\\x1F600\\\\x1F601\\', _, 1, _, S), L), length(L, 2), L = [A, _], atom_length(A, 1)");
        ok("findall(X-Y, string_concat(X, Y, \"\\x1F600\\\"), L), length(L, 2)");
    }

    @Test
    public void testISS0600_PrintQuotes() {
        assertEquals("'A b'", output("print('A b')"));
        assertEquals("f('X',\"s\")", output("print(f('X', \"s\"))"));
        assertEquals("'A b'", output("format('~p', ['A b'])"));
    }

    // ------------------------------------------------------------------ P4.8 .. P4.10

    @Test
    public void testISS0601_CharTypeConformance() {
        ok("char_type(f, xdigit(W)), W == 15");
        ok("code_type(0'7, xdigit(W)), W == 7");
        ok("findall(X, code_type(X, space), L), L == [9,10,11,12,13,32]");
        no("code_type(28, space)");
        ok("code_type(-1, end_of_file)");
        ok("char_type('_', prolog_var_start), char_type('A', prolog_var_start)");
        ok("char_type(a, prolog_atom_start), \\+ char_type('A', prolog_atom_start)");
        ok("char_type('1', prolog_identifier_continue), char_type('_', prolog_identifier_continue)");
        ok("char_type(+, prolog_symbol), \\+ char_type(a, prolog_symbol)");
        err("char_type(a, bogus)", "domain_error(char_type, bogus)");
        err("code_type(0'a, bogus(_))", "domain_error(char_type, bogus(_))");
        ok("char_type(' ', white), \\+ char_type('\\n', white)");
    }

    @Test
    public void testISS0602_FlattenIsIterative() {
        ok("numlist(1, 20000, L), flatten(L, F), length(F, 20000)");
        ok("numlist(1, 6000, L), flatten([L, [L, [L]]], F), length(F, 18000)");
        ok("flatten([a, [b, [c|T]], []], F), F = [a, b, c, V], V == T");
        ok("flatten(a, F), F == [a]");
        ok("flatten([[], [[]]], F), F == []");
        err("X = [a|X], flatten(X, _)", "type_error(acyclic_term, _)");
    }

    @Test
    public void testISS0603_ListLibraryConformance() {
        err("sum_list([a], _)", "type_error(evaluable, a/0)");
        err("max_list([1, a], _)", "type_error(evaluable, a/0)");
        err("min_list([1, a], _)", "type_error(evaluable, a/0)");
        ok("sum_list([9223372036854775807, 1], S), S == 9223372036854775808");
        ok("sum_list([1, 2.5], S), S == 3.5");
        ok("max_list([1, 2^70, 3], M), M == 1180591620717411303424");
        no("max_list([], _)");
        no("nth0(-1, [a], _)");
        no("length(L, L)");
        ok("nth0(1, L, x), L = [_, X|T], X == x, var(T)");
        ok("nth1(2, L, x), L = [_, X|_], X == x");
        ok("last(L, x), !, L == [x]");
        ok("last(L, x), length(L, 3), !, L = [_, _, X], X == x");
        ok("L = [a|_], last(L, Y), !, L == [a], Y == a");
        ok("L = [a|_], nth0(I, L, x), I >= 2, !, I == 2, L = [a, _, X|_], X == x");
        ok("findall(P, permutation([1,2,3], P), L), "
         + "L == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]");
        ok("permutation(X, [a,b]), X == [a,b]");
        ok("intersection([1,1,2], [1,2], X), X == [1,1,2]");
        ok("union([1,1,2], [2,3], X), X == [1,1,2,3]");
        ok("subtract([1,1,2,3], [2], X), X == [1,1,3]");
    }

    // ------------------------------------------------------------------ P4.11 .. P4.14 I/O

    @Test
    public void testISS0604_TabEvaluates() {
        assertEquals("  x", output("tab(1+1), write(x)"));
        assertEquals("x", output("tab(-1), write(x)"));
        err("tab(a)", "type_error(evaluable, a/0)");
        err("tab(1.5)", "type_error(integer, 1.5)");
        err("tab(_)", "instantiation_error");
    }

    private String tmp(String content) throws java.io.IOException {
        java.io.File f = java.io.File.createTempFile("p4conf", ".txt");
        f.deleteOnExit();
        java.nio.file.Files.write(f.toPath(), content.getBytes("UTF-8"));
        return f.getAbsolutePath().replace("\\", "/");
    }

    @Test
    public void testISS0605_StreamConformance() throws Exception {
        String ab = tmp("ab");
        String pl = tmp("foo(1).\n");
        ok("open('" + ab + "', read, S, [eof_action(error)]), get_char(S, a), get_char(S, b), "
         + "get_char(S, C), C == end_of_file, "
         + "catch(get_char(S, _), error(permission_error(input, past_end_of_stream, S1), _), true), "
         + "S1 == S, close(S)");
        ok("open('" + pl + "', read, S, [eof_action(error)]), read(S, T1), read(S, T2), "
         + "T1 == foo(1), T2 == end_of_file, "
         + "catch(read(S, _), error(permission_error(input, past_end_of_stream, _), _), R = raised), "
         + "R == raised, close(S)");
        ok("open('" + pl + "', read, S), read(S, _), read(S, E1), read(S, E2), close(S), "
         + "E1 == end_of_file, E2 == end_of_file");
        ok("open('" + ab + "', read, S), close(S), "
         + "catch(get_char(S, _), error(existence_error(stream, S1), _), true), S1 == S");
        err("open('" + ab + "', read, s)", "uninstantiation_error(s)");
        ok("open('" + ab + "', read, S, [type(binary)]), "
         + "catch(get_char(S, _), error(permission_error(input, binary_stream, S1), _), true), S1 == S, "
         + "get_byte(S, B), B == 97, close(S)");
        ok("open('" + ab + "', read, S), "
         + "catch(get_byte(S, _), error(permission_error(input, text_stream, S1), _), true), S1 == S, close(S)");
        ok("open('" + ab + "', read, S), "
         + "catch(stream_property(S, badprop(x)), error(domain_error(stream_property, P), _), true), "
         + "P == badprop(x), close(S)");
        ok("open('" + ab + "', read, S), "
         + "catch(get_char(S, foo(x)), error(type_error(in_character, C), _), true), C == foo(x), close(S)");
    }

    @Test
    public void testISS0606_WithOutputToYieldsToExplicitSetOutput() throws Exception {
        String f = tmp("");
        ok("with_output_to(atom(A), (write(a), open('" + f + "', write, S), set_output(S), write(x), "
         + "close(S), write(b))), A == ab");
        ok("open('" + f + "', read, R), get_char(R, C), close(R), C == x");
        ok("with_output_to(codes(C), write('\\x1F600\\')), C == [128512]");
    }

    @Test
    public void testISS0607_PrintMessageFormatsMessages() {
        assertEquals("% hello world\n", output("print_message(informational, format('hello ~w', [world]))"));
        assertEquals("% Unknown message: foo\n", output("print_message(informational, foo)"));
        assertEquals("", output("print_message(silent, format('x', []))"));
        java.io.ByteArrayOutputStream err = new java.io.ByteArrayOutputStream();
        java.io.PrintStream prev = System.err;
        System.setErr(new java.io.PrintStream(err, true));
        try {
            prolog.solve("print_message(warning, format('careful ~d', [3])).");
            prolog.solve("print_message(error, error(type_error(integer, a), context(foo/1, _))).");
        } finally {
            System.setErr(prev);
        }
        String text = err.toString();
        assertTrue(text, text.contains("Warning: careful 3\n"));
        assertTrue(text, text.contains("ERROR: foo/1: Type error: `integer' expected, found `a'"));
    }

    // ------------------------------------------------------------------ P4.15 + extra: time

    @Test
    public void testISS0608_StatisticsSwiShapes() {
        ok("statistics(walltime, [T0, _]), integer(T0)");
        ok("statistics(runtime, [T|_]), integer(T)");
        ok("statistics(real_time, [T, _]), integer(T)");
        ok("statistics(system_time, [T, _]), integer(T)");
        ok("statistics(cputime, C), float(C)");
        ok("statistics(process_cputime, C), float(C)");
        ok("statistics(epoch, E), float(E)");
        ok("statistics(inferences, I0), numlist(1, 1000, L), sum_list(L, _), "
         + "foldl([X, A, B]>>(B is A + X), L, 0, _), statistics(inferences, I1), I1 - I0 > 1000");
        ok("statistics(stack, S), integer(S)");
        err("statistics(foo, _)", "domain_error(statistics_key, foo)");
        err("statistics(_, _)", "instantiation_error");
    }

    @Test
    public void testISS0609_GetTimeIsFloatSeconds() {
        ok("get_time(T), float(T), T > 1.7e9, T < 1.0e11");
        ok("stamp_date_time(0, D, 'UTC'), D == date(1970,1,1,0,0,0.0,0,'UTC',-)");
        ok("stamp_date_time(86400.5, date(Y,M,D,H,Mn,S,_,_,_), 'UTC'), "
         + "Y == 1970, M == 1, D == 2, H == 0, Mn == 0, S == 0.5");
        ok("date_time_stamp(date(1970,1,2,0,0,0,0,-,-), S), S =:= 86400");
        ok("get_time(T), stamp_date_time(T, date(Y,_,_,_,_,_,_,_,_), 'UTC'), Y >= 2025");
        ok("format_time(atom(A), '%Y-%m-%d', 0), sub_atom(A, 0, 4, _, Y), (Y == '1970' ; Y == '1969')");
        ok("get_time(T), format_time(atom(A), '%Y', T), atom_length(A, 4)");
    }

    // ------------------------------------------------------------------ P4.16 .. P4.18 database

    @Test
    public void testISS0610_AbolishAndDynamicProperties() {
        err("assertz(ab(1)), abolish(ab/1), ab(_)", "existence_error(procedure, ab/1)");
        err("abolish(foo/1.5)", "type_error(integer, 1.5)");
        err("abolish(foo/100000000000)", "representation_error(max_arity)");
        ok("dynamic(z/1), predicate_property(z(_), dynamic), current_predicate(z/1)");
        ok("dynamic(z0/1), predicate_property(z0(_), number_of_clauses(N)), N == 0");
        ok("assertz(w(1)), assertz(w(2)), predicate_property(w(_), number_of_clauses(N)), N == 2");
        no("predicate_property(nosuch_pred(_), _)");
        ok("predicate_property(atom_length(_, _), built_in)");
        no("findall(P, predicate_property(append(_,_,_), P), L), memberchk(undefined, L)");
    }

    @Test
    public void testISS0611_QualifiedCallsRunInTheModule() {
        prolog.consult(":- module(p4mod, [pub/1]).\npub(X) :- priv(X).\npriv(1).\npriv(2).\n");
        ok("findall(X, p4mod:priv(X), L), L == [1,2]");
        ok("findall(X, p4mod:pub(X), L), L == [1,2]");
        ok("assertz(m3:k(1)), m3:k(X), X == 1");
        ok("clause(m3:k(X), true), X == 1");
    }

    @Test
    public void testISS0612_OpIsPermanent() {
        ok("forall(member(O, [zfoo, zbar]), op(700, xfx, O))");
        ok("current_op(700, xfx, zfoo), current_op(700, xfx, zbar)");
        ok("(op(710, xfy, zbaz), fail ; true), current_op(710, xfy, zbaz)");
    }

    @Test
    public void testISS0613_DocumentedEvaluablesExist() {
        ok("X is cot(1.0), abs(X - 0.6420926159343306) < 1.0e-12");
        ok("X is acot(1.0), abs(X - 0.7853981633974483) < 1.0e-12");
        ok("X is lsb(12), X == 2");
        ok("X is popcount(255), X == 8");
        err("X is lsb(0)", "evaluation_error(undefined)");
    }
}
