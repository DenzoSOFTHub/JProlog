package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0710..0717 - 4.6 wave Q2: the missing standard predicates.
/**
 * Wave Q2 of the 4.6 completeness program (report-completeness-4.6-2026-09-23.md §2/§11):
 * library(solution_sequences) (Q2.1), the memory-management no-ops (Q2.2), rational numbers
 * (Q2.3), deterministic exits of the apply library and of any predicate whose clauses differ
 * outside the first argument (Q2.4), the print_message/2 extension points (Q2.5), format/2 column
 * stops relative to the stream's column (Q2.6) and aggregate/3,4 (Q2.7). The documentation sweep
 * (Q2.8) is {@link DocumentedPredicatesTest}. Every method fails on the 4.5.0 classes.
 */
public class EngineV46StandardPredicatesTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private void ok(String goal) {
        assertFalse("expected success: " + goal, prolog.solve(goal + ".").isEmpty());
    }

    private void no(String goal) {
        assertTrue("expected failure: " + goal, prolog.solve(goal + ".").isEmpty());
    }

    private String first(String goal, String var) {
        List<Map<String, Term>> r = prolog.solve(goal + ".");
        assertFalse("expected success: " + goal, r.isEmpty());
        return Writer.format(r.get(0).get(var), new Writer.Options(), 1200);
    }

    /** What {@code goal} writes on user_error. */
    private String stderrOf(String goal) {
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        PrintStream prev = System.err;
        System.setErr(new PrintStream(err, true));
        try {
            prolog.solve(goal + ".");
        } finally {
            System.setErr(prev);
        }
        return new String(err.toByteArray(), StandardCharsets.UTF_8);
    }

    // ------------------------------------------------------------------ Q2.1 solution_sequences

    @Test
    public void testISS0710_LimitOffsetCallNth() {
        assertEquals("[a,b]", first("findall(X, limit(2, member(X, [a,b,c])), L)", "L"));
        assertEquals("[b,c]", first("findall(X, offset(1, member(X, [a,b,c])), L)", "L"));
        assertEquals("[b]", first("findall(X, limit(1, offset(1, member(X, [a,b,c]))), L)", "L"));
        assertEquals("[a-1,b-2,c-3]", first("findall(X-N, call_nth(member(X, [a,b,c]), N), L)", "L"));
        assertEquals("[b]", first("findall(X, call_nth(member(X, [a,b,c]), 2), L)", "L"));
        // lazy: an infinite generator is abandoned
        assertEquals("[1,2,3]", first("findall(X, limit(3, between(1, inf, X)), L)", "L"));
        assertEquals("[a,b]", first("findall(X, limit(infinite, member(X, [a,b])), L)", "L"));
        no("limit(0, true)");
        no("call_nth(true, 0)");
        // the cut in the goal is local to it (call/1)
        assertEquals("[a]", first("findall(X, limit(2, (member(X, [a,b,c]), !)), L)", "L"));
    }

    @Test
    public void testISS0710_LimitCutsTheGoalAndRunsItsCleanup() {
        // limit/2 stops the goal at its last solution: the cleanup runs right then, and the
        // limit call leaves no choice point (the Det idiom)
        ok("nb_setval(q2c, none), limit(1, setup_call_cleanup(true, member(_, [1,2,3]), nb_setval(q2c, done))), "
         + "nb_getval(q2c, V), V == done");
        assertEquals("[a-u,b-true]",
            first("findall(X-D, (setup_call_cleanup(true, limit(2, member(X, [a,b,c])), D = true), "
                + "(var(D) -> D = u ; true)), L)", "L"));
    }

    @Test
    public void testISS0710_DistinctAndOrderBy() {
        assertEquals("[a,b,c]", first("findall(X, distinct(X, member(X, [a,b,a,c,b])), L)", "L"));
        // variant check of the witness: f(A) and f(B) are the same witness
        assertEquals("2", first("findall(X, distinct(X, member(X, [f(_),f(_),g])), L), length(L, N)", "N"));
        assertEquals("[1-a,2-b]", first("findall(P, distinct(member(P, [1-a,2-b,1-a])), L)", "L"));
        assertEquals("[3-b,2-c,1-a]", first("findall(X-Y, order_by([desc(X)], member(X-Y, [1-a,3-b,2-c])), L)", "L"));
        assertEquals("[1-b,1-a,2-c,2-a]",
            first("findall(X-Y, order_by([asc(X),desc(Y)], member(X-Y, [2-a,1-b,2-c,1-a])), L)", "L"));
        // stable for equal keys
        assertEquals("[1-z,1-a,2-q]", first("findall(X-Y, order_by([asc(X)], member(X-Y, [2-q,1-z,1-a])), L)", "L"));
    }

    @Test
    public void testISS0710_ArgumentErrors() {
        ok("catch(limit(_, true), error(instantiation_error, _), true)");
        ok("catch(limit(a, true), error(type_error(integer, a), _), true)");
        ok("catch(offset(-1, true), error(domain_error(not_less_than_zero, -1), _), true)");
        ok("catch(order_by([], true), error(domain_error(non_empty_list, []), _), true)");
        ok("catch(order_by([foo(_)], true), error(domain_error(order_specifier, foo(_)), _), true)");
        ok("catch(distinct(_), error(instantiation_error, _), true)");
        ok("catch(call_nth(true, a), error(type_error(integer, a), _), true)");
    }

    // ------------------------------------------------------------------ Q2.2

    @Test
    public void testISS0711_MemoryManagementPredicatesSucceed() {
        ok("garbage_collect, garbage_collect_atoms, trim_stacks");
    }

    // ------------------------------------------------------------------ Q2.3 rationals

    @Test
    public void testISS0712_RationalArithmetic() {
        assertEquals("1r3", first("X is 1 rdiv 3", "X"));
        assertEquals("1r2", first("X is 1r3 + 1r6", "X"));
        assertEquals("1", first("X is 1r3 * 3", "X"));                 // normalised to an integer
        assertEquals("1r2", first("X = 2r4", "X"));                   // the reader normalises
        assertEquals("9r4", first("X is 2r3 ^ -2", "X"));
        assertEquals("-1r3", first("X is -(1r3)", "X"));
        assertEquals("3", first("X is round(5r2)", "X"));
        assertEquals("-1", first("X is floor(-1r3)", "X"));
        assertEquals("7", first("X is numerator(3r4) + denominator(3r4)", "X"));
        assertEquals("1r10", first("X is rationalize(0.1)", "X"));
        assertEquals("3602879701896397r36028797018963968", first("X is rational(0.1)", "X"));
        // prefer_rationals = false: integer division stays a float
        assertEquals("0.3333333333333333", first("X is 1/3", "X"));
        ok("current_prolog_flag(prefer_rationals, false)");
        // a float operand makes the result a float
        ok("X is 1r2 + 0.5, X == 1.0");
        ok("catch(_ is 1.0 rdiv 2, error(type_error(rational, 1.0), _), true)");
        ok("catch(_ is 1 rdiv 0, error(evaluation_error(zero_divisor), _), true)");
        ok("catch(_ is 1r3 mod 2, error(type_error(integer, 1r3), _), true)");
        // exact comparison
        ok("1r3 < 1, 1r3 =:= 1 rdiv 3, 1r3 > 0.3, 2r3 > 1r2");
        ok("set_prolog_flag(prefer_rationals, true), X is 1/3, X == 1r3, set_prolog_flag(prefer_rationals, false)");
    }

    @Test
    public void testISS0712_RationalTypesOrderAndText() {
        ok("rational(1r3), rational(2), \\+ rational(0.5), number(1r3), atomic(1r3)");
        no("float(1r3)");
        no("integer(1r3)");
        assertEquals("3-4", first("rational(3r4, N, D), X = N-D", "X"));
        ok("rational(5, 5, 1)");
        // standard order: by value, float first on a tie
        assertEquals("[0,1r3,0.5,1r2,2r3,1]", first("msort([1, 0.5, 1r2, 1r3, 0, 2r3], L)", "L"));
        ok("1r3 == 1r3, 1r3 \\== 0.3333333333333333, 1r3 \\= 1");
        // the writer prints what the reader reads back
        assertEquals("1r3 -1r3", first("with_output_to(atom(A), (writeq(1r3), write(' '), writeq(-1r3)))", "A"));
        ok("term_to_atom(T, '1r3'), T == 1r3");
        ok("number_codes(X, \"2r6\"), X == 1r3");
        ok("atom_number('-1r3', X), X == -1r3");
        ok("must_be(rational, 1r3), catch(must_be(float, 1r3), error(type_error(float, _), _), true)");
    }

    @Test
    public void testISS0712_RationalsInTheDatabase() throws Exception {
        // assert + first-argument index (a rational is its own key: 1r2 never matches 0.5)
        ok("assertz(r(1r2, half)), assertz(r(0.5, float_half)), assertz(r(1, one))");
        assertEquals("half", first("r(1r2, X)", "X"));
        assertEquals("float_half", first("r(0.5, X)", "X"));
        assertEquals("1", first("findall(X, r(1r2, X), L), length(L, N)", "N"));
        ok("copy_term(f(1r3, X), C), C = f(R, _), R == 1r3");
        ok("aggregate_all(sum(X), member(X, [1r3, 1r6]), S), S == 1r2");
        // .jpc round trip
        File dir = Files.createTempDirectory("q2rat").toFile();
        File src = new File(dir, "rat.pl");
        Files.write(src.toPath(), "q(1r3).\nq(-2r5).\n".getBytes(StandardCharsets.UTF_8));
        String jpc = prolog.compileFile(src.getPath());
        Prolog p2 = new Prolog();
        p2.consultCompiled(jpc);
        assertFalse(p2.solve("q(X), X == 1r3.").isEmpty());
        assertFalse(p2.solve("q(X), X == -2r5.").isEmpty());
    }

    // ------------------------------------------------------------------ Q2.4 determinism

    @Test
    public void testISS0715_ApplyLibraryIsDeterministic() {
        // SWI's `deterministic` idiom: the cleanup has run when the call exits without a choice point
        String[] goals = {
            "foldl([X,A0,A]>>(A is A0+X), [1,2,3], 0, S)",
            "foldl(plus, [1,2,3], 0, S)",
            "maplist([X]>>atom(X), [a,b])",
            "maplist([X,Y]>>(Y is X*2), [1,2], L)",
            "include([X]>>(X > 1), [1,2,3], L)",
            "exclude([X]>>(X > 1), [1,2,3], L)",
            "partition([X]>>(X > 1), [1,2,3], I, E)",
            "aggregate_all(count, member(_, [a,b]), C)",
        };
        for (String g : goals) {
            ok("setup_call_cleanup(true, (" + g + "), Det = true), Det == true");
        }
    }

    @Test
    public void testISS0715_ClausesDifferingOutsideTheFirstArgument() {
        prolog.consult("walk(_, []).\nwalk(G, [_|T]) :- walk(G, T).\n"
                     + "col(red, 1). col(green, 2). col(blue, 3).\n");
        ok("setup_call_cleanup(true, walk(x, [1,2,3]), Det = true), Det == true");
        // a bound second argument discriminates too
        ok("setup_call_cleanup(true, col(C, 3), Det = true), Det == true, C == blue");
        // and nothing is lost: every matching clause is still found
        assertEquals("[red,green,blue]", first("findall(C, col(C, _), L)", "L"));
        assertEquals("2", first("findall(T, limit(2, walk(_, T)), L), length(L, N)", "N"));
    }

    // ------------------------------------------------------------------ Q2.5 print_message

    @Test
    public void testISS0713_MessageHookAndPrologMessage() {
        prolog.consult(":- dynamic(seen/3).\n"
            + "message_hook(intercept(X), Kind, Lines) :- assertz(seen(X, Kind, Lines)).\n"
            + ":- multifile(prolog:message//1).\n"
            + "prolog:message(hello(X)) --> ['Hello ~w'-[X], nl, 'second line'].\n");
        // message_hook/3 intercepts (also a silent message); nothing is printed
        assertEquals("", stderrOf("print_message(error, intercept(1))"));
        ok("seen(1, error, Lines), is_list(Lines)");
        ok("print_message(silent, intercept(2)), seen(2, silent, _)");
        // prolog:message//1 translates, on user_error, with the kind's prefix on every line
        assertEquals("% Hello world\n% second line\n", stderrOf("print_message(informational, hello(world))"));
        assertEquals("Warning: Hello w\nWarning: second line\n", stderrOf("print_message(warning, hello(w))"));
        // verbose = silent suppresses informational messages only
        assertEquals("", stderrOf("set_prolog_flag(verbose, silent), print_message(informational, hello(x))"));
        assertTrue(stderrOf("print_message(warning, hello(y))").contains("Warning: Hello y"));
    }

    // ------------------------------------------------------------------ Q2.6 column stops

    @Test
    public void testISS0714_FormatColumnStopsStartAtTheStreamColumn() throws Exception {
        assertEquals("abc      x", first("with_output_to(atom(A), (write(abc), format(\"~t~w~10|\", [x])))", "A"));
        assertEquals("name      value", first("format(atom(A), \"~a~t~10|~a\", [name, value])", "A"));
        // user_output tracks its column (line_position/2) and format/2 honours it
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        PrintStream prev = System.out;
        System.setOut(new PrintStream(bos, true, "UTF-8"));
        List<Map<String, Term>> r;
        try {
            r = prolog.solve("write(abc), line_position(user_output, P), format(\"~t~w~10|~n\", [x]), "
                           + "line_position(user_output, P2).");
        } finally {
            System.setOut(prev);
        }
        assertFalse(r.isEmpty());
        assertEquals("3", r.get(0).get("P").toString());
        assertEquals("0", r.get(0).get("P2").toString());
        assertEquals("abc      x\n", new String(bos.toByteArray(), StandardCharsets.UTF_8));
        // a file stream too
        File f = File.createTempFile("q2col", ".txt");
        ok("open('" + f.getPath() + "', write, S), write(S, abc), format(S, \"~t~w~10|\", [x]), close(S)");
        assertEquals("abc      x", new String(Files.readAllBytes(f.toPath()), StandardCharsets.UTF_8));
    }

    // ------------------------------------------------------------------ Q2.7 aggregate/3,4

    @Test
    public void testISS0716_AggregateGroupsLikeBagof() {
        assertEquals("[a-1,b-1]", first("findall(X-C, aggregate(count, member(X, [a,b]), C), L)", "L"));
        assertEquals("2", first("aggregate(count, X^member(X, [a,b]), C)", "C"));
        assertEquals("[a-4,b-2]", first("findall(K-S, aggregate(sum(X), member(K-X, [a-1,b-2,a-3]), S), L)", "L"));
        assertEquals("max(3,b)", first("aggregate(max(X, Y), member(X-Y, [1-a,3-b,2-c]), M)", "M"));
        assertEquals("r(3,6,[1,2,3])", first("aggregate(r(count, sum(X), bag(X)), member(X, [1,2,3]), R)", "R"));
        assertEquals("[a,b,c]", first("aggregate(set(X), member(X, [c,a,b,a]), S)", "S"));
        no("aggregate(count, member(_, []), _)");                     // bagof: no solution fails
        // aggregate/4 and aggregate_all/4 count distinct discriminators
        assertEquals("2", first("aggregate(count, P, member(P, [a,a,b]), C)", "C"));
        assertEquals("2", first("aggregate_all(count, P, member(P-_, [a-1,a-2,b-3]), C)", "C"));
        assertEquals("0", first("aggregate_all(count, P, member(P, []), C)", "C"));
        ok("catch(aggregate(foo, true, _), error(domain_error(aggregate_spec, foo), _), true)");
        ok("catch(aggregate(_, true, _), error(instantiation_error, _), true)");
    }
}
// END_CHANGE: ISS-2025-0710..0717
