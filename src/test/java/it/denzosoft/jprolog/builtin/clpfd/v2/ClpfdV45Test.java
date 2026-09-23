package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

// START_CHANGE: ISS-2025-0640 - 4.5 wave P5 (CLP(FD)) regression suite, ISS-2025-0640..0652.
/**
 * Wave P5 of the production-readiness program (report-production-readiness-2026-09-23.md §5):
 * propagation after plain unification, domains with holes, lazy labeling with the SWI options,
 * branch and bound, big integers, fast failure of cyclic inequalities, and the missing library
 * predicates (ins/2, sum/3, scalar_product/4, reification, //, rem, mod, ^, fd_* reflection,
 * element/3, tuples_in/2, global_cardinality/2, a stronger all_distinct/1).
 *
 * <p>Semantics follow SWI-Prolog library(clpfd). The timeouts are deliberately generous (the
 * suite runs on loaded CI machines); the measured timings are in the wave record (§11).
 */
public class ClpfdV45Test {

    private Prolog p;

    @Before public void setUp() { p = new Prolog(); }

    private List<Map<String, Term>> q(String goal) { return p.solve(goal); }

    private void ok(String goal) { assertFalse("must succeed: " + goal, p.solve(goal).isEmpty()); }

    private void no(String goal) { assertTrue("must fail: " + goal, p.solve(goal).isEmpty()); }

    private String val(String goal, String var) {
        List<Map<String, Term>> s = p.solve(goal);
        assertFalse("must succeed: " + goal, s.isEmpty());
        return s.get(0).get(var).toString();
    }

    private static final String QUEENS =
        "queens(N, Qs) :- length(Qs, N), Qs ins 1..N, safe(Qs).\n"
      + "safe([]).\n"
      + "safe([Q|Qs]) :- no_attack(Q, Qs, 1), safe(Qs).\n"
      + "no_attack(_, [], _).\n"
      + "no_attack(Q, [Q1|Qs], D) :- Q #\\= Q1, Q #\\= Q1 + D, Q #\\= Q1 - D, D1 is D + 1, no_attack(Q, Qs, D1).\n";

    // ------------------------------------------------------------------ P5.1

    @Test(timeout = 20000)
    public void testISS0640_PlainUnificationPropagatesAndBindsSingletons() {
        // the three repros of the spec (SWI answers in every case)
        assertEquals("5", val("X in 0..9, Y in 0..9, X+Y #= 9, X = 4.", "Y"));
        ok("X #= Y+Z, Y = 1, Z = 2, X == 3.");
        assertEquals("4", val("X in 1..5, Y in 1..5, X #= Y+1, Y = 3.", "X"));
        // a chain: binding one end determines the other through two constraints
        ok("A #= B + 1, B #= C * 2, C = 3, A == 7, B == 6.");
        // an inconsistent plain binding still fails
        no("X in 0..9, Y in 0..9, X+Y #= 9, X = 4, Y = 6.");
    }

    // ------------------------------------------------------------------ P5.2

    @Test(timeout = 20000)
    public void testISS0641_DisequalityRemovesInteriorValues() {
        ok("X in 1..5, Y in 1..5, X #\\= Y, X #= 2, fd_dom(Y, D), D == 1\\/(3..5).");
        // the removed value is really gone: labeling never produces it
        assertEquals(4, q("X in 1..5, Y in 1..5, X #\\= Y, X #= 2, label([Y]).").size());
        // multi-variable disequality prunes as soon as all but one variable are fixed
        ok("X in 1..5, Y in 1..5, Z in 1..5, X + Y #\\= Z, X = 1, Y = 1, fd_dom(Z, D), D == 1\\/(3..5).");
    }

    @Test(timeout = 20000)
    public void testISS0641_DomainsWithHolesAndUnions() {
        ok("X in 1..3 \\/ 5..7, fd_dom(X, D), D == (1..3)\\/(5..7).");
        assertEquals(6, q("X in 1..3 \\/ 5..7, label([X]).").size());
        no("X in 1..3 \\/ 5..7, X = 4.");
        ok("X in 5, X == 5.");
        ok("X in 1..2 \\/ 4 \\/ 6..7, fd_size(X, S), S == 5.");
        ok("[X,Y] ins 0..1 \\/ 3, X #= Y + 3, X == 3, Y == 0.");
        // X #= Y keeps holes on both sides
        ok("X in 1..10, X #\\= 5, Y in 3..7, X #= Y, fd_dom(Y, D), D == (3..4)\\/(6..7).");
        // domain errors
        ok("catch(X in foo, error(type_error(clpfd_domain, foo), _), true).");
        ok("catch(X in _, error(instantiation_error, _), true).");
    }

    // ------------------------------------------------------------------ P5.3

    @Test(timeout = 20000)
    public void testISS0642_TwentyQueensFirstSolutionIsLazy() {
        p.consult(QUEENS);
        long t0 = System.nanoTime();
        List<Map<String, Term>> s = q("queens(20, Qs), labeling([ff], Qs), !.");
        long ms = (System.nanoTime() - t0) / 1000000;
        assertEquals(1, s.size());
        assertTrue("first 20-queens solution took " + ms + " ms", ms < 15000);
        // a labeling over a huge domain hands out its first answer at once (it used to enumerate)
        assertEquals("1", val("X in 1..3, Y in 1..2000000000, once(label([X, Y])).", "Y"));
    }

    @Test(timeout = 20000)
    public void testISS0642_EightQueensAllSolutions() {
        p.consult(QUEENS);
        assertEquals("92", val("queens(8, Qs), findall(Qs, label(Qs), L), length(L, N).", "N"));
        assertEquals("92", val("queens(8, Qs), findall(Qs, labeling([ff], Qs), L), length(L, N).", "N"));
        // the first solution in leftmost/up order is SWI's
        assertEquals("[1,5,8,6,3,7,2,4]", val("queens(8, Qs), label(Qs), !, term_to_atom(Qs, A).", "A"));
    }

    @Test(timeout = 20000)
    public void testISS0642_LabelingOptions() {
        // label/1 is leftmost (SWI), not first-fail: X is labelled before the smaller Y
        assertEquals("1", val("X in 1..3, Y in 1..2, label([X, Y]).", "X"));
        List<Map<String, Term>> ff = q("X in 1..3, Y in 5..6, labeling([ff], [X, Y]).");
        assertEquals(6, ff.size());
        assertEquals("5", ff.get(0).get("Y").toString());
        assertEquals("5", ff.get(1).get("Y").toString());           // Y chosen first, X varies inside
        List<Map<String, Term>> down = q("X in 1..3, labeling([down], [X]).");
        assertEquals("3", down.get(0).get("X").toString());
        // min/max select the VARIABLE by its bound (SWI): max labels Y (sup 7) before X (sup 3),
        // so X varies inside the first Y value; min labels Y (inf 1) before X (inf 2)
        List<Map<String, Term>> mx = q("X in 1..3, Y in 1..7, labeling([max], [X, Y]).");
        assertEquals("1", mx.get(1).get("Y").toString());
        assertEquals("2", mx.get(1).get("X").toString());
        List<Map<String, Term>> mn = q("X in 2..3, Y in 1..7, labeling([min], [X, Y]).");
        assertEquals("1", mn.get(1).get("Y").toString());
        assertEquals("3", mn.get(1).get("X").toString());
        // every branching strategy enumerates the same set
        for (String b : new String[]{"step", "enum", "bisect"}) {
            assertEquals(b, 6, q("X in 1..3, Y in 1..2, labeling([" + b + "], [X, Y]).").size());
            assertEquals(b, 3, q("X in 1..6, X #\\= 2, X #\\= 4, X #\\= 5, labeling([" + b + ", down], [X]).").size());
        }
        assertEquals("6", val("X in 1..6, labeling([bisect, down], [X]).", "X"));
        assertEquals(6, q("X in 1..3, Y in 1..2, labeling([ffc], [X, Y]).").size());
        // SWI: labeling needs finite domains
        ok("catch(label([X]), error(instantiation_error, _), true).");
        ok("catch((X #> 3, label([X])), error(instantiation_error, _), true).");
        ok("catch(labeling([foo], [X]), error(domain_error(labeling_option, foo), _), true).");
        // SWI: one option per category
        ok("X in 1..3, catch(labeling([ff, ff], [X]), error(domain_error(nonrepeating_labeling_options, [ff, ff]), _), true).");
        ok("X in 1..3, catch(labeling([up, down], [X]), error(domain_error(consistent_labeling_options, [up, down]), _), true).");
        assertEquals(3, q("X in 1..3, labeling([ff, down, bisect], [X]).").size());
        // deterministic exit once the last variable is labelled
        ok("X in 3..3, label([X]), X == 3.");
        // indomain/1 is label([X])
        assertEquals(3, q("X in 1..3, indomain(X).").size());
    }

    @Test(timeout = 20000)
    public void testISS0643_BranchAndBoundObjectiveOrder() {
        List<Map<String, Term>> s = q("X in 1..5, Y in 1..5, X + Y #= 6, labeling([max(X*Y)], [X, Y]).");
        assertEquals(5, s.size());
        assertEquals("3", s.get(0).get("X").toString());               // 3*3 = 9 is the optimum
        assertEquals("3", s.get(0).get("Y").toString());
        s = q("X in 1..5, Y in 1..5, X + Y #= 6, labeling([min(X*Y)], [X, Y]).");
        assertEquals("1", s.get(0).get("X").toString());               // 1*5 = 5 first (labeling order)
        assertEquals("5", s.get(1).get("X").toString());               // then 5*1 = 5
        // lexicographic objectives
        s = q("[X,Y] ins 0..3, labeling([max(X), min(Y)], [X, Y]).");
        assertEquals("3", s.get(0).get("X").toString());
        assertEquals("0", s.get(0).get("Y").toString());
        no("X in 1..3, X #> 5, labeling([min(X)], [X]).");
    }

    // ------------------------------------------------------------------ P5.4

    @Test(timeout = 20000)
    public void testISS0644_BigIntegerArithmetic() {
        assertEquals("1000000000000000000000000", val("X #= 1000000000000*1000000000000.", "X"));
        assertEquals("1267650600228229401496703205376", val("X #= 2^100.", "X"));
        ok("X #= 1000000000000*1000000000000, Y #= X // 1000000000000, Y == 1000000000000.");
        ok("1000000000000*1000000000000 #> 999999999999*1000000000000.");
        // an unbounded variable accepts a big binding; a bounded one rejects it
        ok("X #> 3, X = 100000000000000000000000.");
        no("X in 0..10, X = 100000000000000000000000.");
        // the unconstrained domain is inf..sup (SWI)
        ok("X #> 3, fd_dom(X, D), D == 4..sup.");
        // non-ground: a value determined beyond the 64-bit range is bound exactly (SWI)
        ok("X #= Y*1000000000000, Y = 1000000000000, X == 1000000000000000000000000.");
        ok("X #= Y*Z, Y = 1000000000000, Z = 1000000000000, X == 1000000000000000000000000.");
        ok("X #= Y + 1, Y = 9223372036854775807, X == 9223372036854775808.");
        ok("X #= Y + 1, X = 100000000000000000000, Y == 99999999999999999999.");
        ok("X #= Y ^ 70, Y = 2, X == 1180591620717411303424.");
        ok("X #= Y + Z, Y = 100000000000000000000, Z = -100000000000000000000, X == 0.");
        no("X #= Y + 1, Y = 100000000000000000000, X = 5.");
        no("X #= Y + 1, Y = 100000000000000000000, X = 100000000000000000005.");
    }

    @Test(timeout = 20000)
    public void testISS0645_CyclicInequalitiesFailFast() {
        long t0 = System.nanoTime();
        no("X #> Y, Y #> X.");
        no("X #> Y, Y #> Z, Z #> X.");
        no("X in 0..1000000000, Y in 0..1000000000, X #> Y, Y #> X.");
        long ms = (System.nanoTime() - t0) / 1000000;
        assertTrue("cyclic inequalities took " + ms + " ms", ms < 5000);
        // a zero-weight cycle is satisfiable
        ok("X #>= Y, Y #>= X.");
        ok("X #> Y, Y #>= Z.");
    }

    // ------------------------------------------------------------------ P5.5

    @Test(timeout = 20000)
    public void testISS0646_InsSumScalarProduct() {
        assertEquals(2, q("[X, Y] ins 1..3, sum([X, Y], #=, 5), label([X, Y]).").size());
        assertEquals(3, q("[X, Y] ins 0..6, scalar_product([2, 3], [X, Y], #=, 12), label([X, Y]).").size());
        ok("sum([], #=, 0).");
        ok("[A,B,C] ins 0..1, sum([A,B,C], #>=, 3), A == 1, B == 1, C == 1.");
        ok("catch(sum([X], foo, 1), error(domain_error(scalar_product_relation, foo), _), true).");
        ok("catch(_ ins 1..3, error(instantiation_error, _), true).");
        // a user program's own sum/3 wins over the library one
        p.consult("sum(A, B, C) :- C is A + B.\n");
        assertEquals("5", val("sum(2, 3, X).", "X"));
    }

    @Test(timeout = 20000)
    public void testISS0647_ReificationAndConnectives() {
        ok("X in 1..5, B #<==> (X #> 3), X = 4, B == 1.");
        ok("X in 1..5, B #<==> (X #> 3), B = 0, fd_dom(X, D), D == 1..3.");
        assertEquals(2, q("X in 1..5, X #= 1 #\\/ X #= 3, label([X]).").size());
        ok("X in 1..3, #\\ X #= 2, fd_dom(X, D), D == 1\\/3.");
        ok("X in 0..1, Y in 0..1, X #==> Y, X = 1, Y == 1.");
        ok("X in 0..1, Y in 0..1, X #<== Y, Y = 1, X == 1.");
        ok("X in 0..1, Y in 0..1, X #\\ Y, X = 1, Y == 0.");
        ok("X in 0..9, Y in 0..9, (X #> 5) #/\\ (Y #< 2), fd_dom(X, D), D == 6..9.");
        ok("B #<==> (X in 1..3), X = 7, B == 0.");
        // reification counting: exactly two of three variables equal 1
        p.consult("count_eq(Xs, V, N) :- maplist(reif_eq(V), Xs, Bs), sum(Bs, #=, N).\n"
                + "reif_eq(V, X, B) :- B #<==> (X #= V).\n");
        assertEquals("6", val("length(Xs, 3), Xs ins 0..2, count_eq(Xs, 1, 2), "
                            + "findall(Xs, label(Xs), L), length(L, N).", "N"));
        ok("catch(B #<==> foo, error(domain_error(clpfd_reifiable_expression, foo), _), true).");
    }

    @Test(timeout = 20000)
    public void testISS0648_ExpressionFunctions() {
        assertEquals(3, q("X in 0..20, X // 3 #= 2, label([X]).").size());          // 6, 7, 8
        ok("X #= -7 // 2, X == -3.");                                               // truncating
        ok("X #= -7 div 2, X == -4.");                                              // floored
        ok("X #= -7 rem 2, X == -1.");
        ok("X #= -7 mod 2, X == 1.");
        ok("X #= 7 mod -2, X == -1.");
        assertEquals(2, q("X in -10..10, X^2 #= 49, label([X]).").size());          // -7, 7
        ok("X in -10..10, X^3 #= -27, X == -3.");
        assertEquals(3, q("X in 0..10, X rem 4 #= 1, label([X]).").size());          // 1, 5, 9
        ok("X in 1..5, Y #= max(X, 3), X = 1, Y == 3.");
        ok("X in -5..5, Y #= abs(X), Y #>= 4, fd_dom(X, D), D == (-5.. -4)\\/(4..5).");
        ok("Y in 1..3, X #= 10 // Y, Y = 3, X == 3.");
        no("X #= 1 // 0.");
        ok("catch(X #= foo(1), error(type_error(evaluable, foo/1), _), true).");
    }

    @Test(timeout = 20000)
    public void testISS0649_DomainReflection() {
        ok("X in 3..7, fd_inf(X, 3), fd_sup(X, 7), fd_size(X, 5).");
        ok("X #> 3, fd_inf(X, I), fd_sup(X, S), fd_size(X, N), I == 4, S == sup, N == sup.");
        ok("fd_dom(X, D), D == inf..sup.");
        ok("fd_dom(4, D), D == 4..4, fd_size(4, 1), fd_inf(4, 4).");
        ok("X in 1..3, fd_var(X).");
        no("fd_var(_).");
        no("fd_var(3).");
        ok("catch(fd_dom(a, _), error(type_error(integer, a), _), true).");
    }

    @Test(timeout = 20000)
    public void testISS0650_ElementTuplesGcc() {
        ok("element(I, [10, 20, 30], V), V #> 15, fd_dom(I, D), D == 2..3.");
        ok("element(2, [10, X, 30], 7), X == 7.");
        assertEquals(3, q("element(I, [1, 2, 3], V), label([I, V]).").size());
        assertEquals(2, q("tuples_in([[X, Y]], [[1, 2], [2, 3], [5, 5]]), X #< 3, label([X, Y]).").size());
        ok("tuples_in([[X, Y]], [[1, 2], [2, 3], [5, 5]]), Y = 5, X == 5.");
        assertEquals(3, q("Vs = [A, B, C], global_cardinality(Vs, [1-2, 2-1]), label(Vs).").size());
        ok("Vs = [A, B, C], global_cardinality(Vs, [1-N1, 2-N2]), A = 1, B = 1, C = 2, N1 == 2, N2 == 1.");
        no("global_cardinality([A, B], [1-0, 2-0]).");
    }

    @Test(timeout = 20000)
    public void testISS0651_AllDistinctIsStrongerThanAllDifferent() {
        // Hall set {1,2} for X, Y forces Z = 3 without any labeling
        ok("X in 1..2, Y in 1..2, Z in 1..3, all_distinct([X, Y, Z]), Z == 3.");
        ok("X in 1..2, Y in 1..2, Z in 1..3, all_different([X, Y, Z]), var(Z).");
        no("[X, Y, Z] ins 1..2, all_distinct([X, Y, Z]).");
        assertEquals(6, q("[X, Y, Z] ins 1..3, all_distinct([X, Y, Z]), label([X, Y, Z]).").size());
    }

    @Test(timeout = 20000)
    public void testISS0652_ReificationOperatorsParse() {
        ok("current_op(760, yfx, #<==>), current_op(750, xfy, #==>), current_op(750, yfx, #<==).");
        ok("current_op(740, yfx, #\\/), current_op(730, yfx, #\\), current_op(720, yfx, #/\\), current_op(710, fy, #\\).");
        ok("T = (a #<==> b #/\\ c), T = #<==>(a, #/\\(b, c)).");
    }

    // ------------------------------------------------------------------ puzzles (§5 targets)

    private static final String SEND_MORE =
        "puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-\n"
      + "  Vars = [S,E,N,D,M,O,R,Y], Vars ins 0..9, all_different(Vars),\n"
      + "  S*1000 + E*100 + N*10 + D + M*1000 + O*100 + R*10 + E #=\n"
      + "  M*10000 + O*1000 + N*100 + E*10 + Y, M #\\= 0, S #\\= 0, label(Vars).\n";

    @Test(timeout = 20000)
    public void testISS0646_SendMoreMoneyUniqueAndFast() {
        p.consult(SEND_MORE);
        q("puzzle(P).");                                                            // warm up
        long t0 = System.nanoTime();
        List<Map<String, Term>> s = q("puzzle(P).");
        long ms = (System.nanoTime() - t0) / 1000000;
        assertEquals("exactly one solution", 1, s.size());
        ok("puzzle([S,E,N,D]+[M,O,R,E]=[M,O,N,E,Y]), [S,E,N,D,M,O,R,Y] == [9,5,6,7,1,0,8,2].");
        assertTrue("SEND+MORE took " + ms + " ms", ms < 5000);
    }

    private static final String SUDOKU =
        "sudoku(Rows) :- length(Rows, 9), maplist(len9, Rows), rows_vars(Rows, Vs), Vs ins 1..9,\n"
      + "  maplist(all_distinct, Rows), transpose(Rows, Cols), maplist(all_distinct, Cols),\n"
      + "  Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is], blocks(As,Bs,Cs), blocks(Ds,Es,Fs), blocks(Gs,Hs,Is).\n"
      + "len9(L) :- length(L, 9).\n"
      + "rows_vars([], []).\n"
      + "rows_vars([R|Rs], Vs) :- append(R, Ws, Vs), rows_vars(Rs, Ws).\n"
      + "blocks([], [], []).\n"
      + "blocks([A,B,C|R1], [D,E,F|R2], [G,H,I|R3]) :- all_distinct([A,B,C,D,E,F,G,H,I]), blocks(R1, R2, R3).\n"
      // Arto Inkala's "world's hardest sudoku"
      + "problem(1, [[8,_,_,_,_,_,_,_,_],[_,_,3,6,_,_,_,_,_],[_,7,_,_,9,_,2,_,_],\n"
      + "            [_,5,_,_,_,7,_,_,_],[_,_,_,_,4,5,7,_,_],[_,_,_,1,_,_,_,3,_],\n"
      + "            [_,_,1,_,_,_,_,6,8],[_,_,8,5,_,_,_,1,_],[_,9,_,_,_,_,4,_,_]]).\n"
      // the 17-clue puzzle of the SWI-Prolog library(clpfd) documentation
      + "problem(2, [[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,3,_,8,5],[_,_,1,_,2,_,_,_,_],\n"
      + "            [_,_,_,5,_,7,_,_,_],[_,_,4,_,_,_,1,_,_],[_,9,_,_,_,_,_,_,_],\n"
      + "            [5,_,_,_,_,_,_,7,3],[_,_,2,_,1,_,_,_,_],[_,_,_,_,4,_,_,_,9]]).\n";

    @Test(timeout = 30000)
    public void testISS0651_HardSudoku() {
        p.consult(SUDOKU);
        long t0 = System.nanoTime();
        List<Map<String, Term>> s = q("problem(1, R), sudoku(R), rows_vars(R, Vs), labeling([ff], Vs).");
        long ms = (System.nanoTime() - t0) / 1000000;
        assertEquals("the puzzle has a unique solution", 1, s.size());
        assertEquals("[[8,1,2,7,5,3,6,4,9],[9,4,3,6,8,2,1,7,5],[6,7,5,4,9,1,2,8,3],"
                   + "[1,5,4,2,3,7,8,9,6],[3,6,9,8,4,5,7,2,1],[2,8,7,1,6,9,5,3,4],"
                   + "[5,2,1,9,7,4,3,6,8],[4,3,8,5,2,6,9,1,7],[7,9,6,3,1,8,4,5,2]]",
            val("problem(1, R), sudoku(R), rows_vars(R, Vs), labeling([ff], Vs), term_to_atom(R, A).", "A"));
        assertTrue("hard sudoku took " + ms + " ms", ms < 20000);
        assertEquals(1, q("problem(2, R), sudoku(R), rows_vars(R, Vs), labeling([ff], Vs).").size());
    }

    @Test(timeout = 20000)
    public void testISS0646_MagicSquare3x3() {
        p.consult("magic(Sq) :- Sq = [A,B,C,D,E,F,G,H,I], Sq ins 1..9, all_different(Sq),\n"
                + "  A+B+C #= 15, D+E+F #= 15, G+H+I #= 15, A+D+G #= 15, B+E+H #= 15, C+F+I #= 15,\n"
                + "  A+E+I #= 15, C+E+G #= 15, label(Sq).\n");
        assertEquals("8", val("findall(S, magic(S), L), length(L, N).", "N"));
        ok("magic([2,7,6,9,5,1,4,3,8]).");
        // the centre is always 5
        ok("forall(magic(S), nth1(5, S, 5)).");
    }
}
// END_CHANGE: ISS-2025-0640
