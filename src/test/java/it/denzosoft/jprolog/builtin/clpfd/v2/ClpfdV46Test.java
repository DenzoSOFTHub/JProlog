package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.v4.Answer;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

// START_CHANGE: ISS-2025-0760 - 4.6 wave Q5 (CLP(FD) residue) regression suite, ISS-2025-0760..0770.
/**
 * Wave Q5 of the 4.6 completeness program (report-completeness-4.6-2026-09-23.md §5 / §14):
 * residual constraints in answers and {@code copy_term/3} (SWI's printed forms), the missing
 * global constraints ({@code circuit/1}, {@code cumulative/1,2}, {@code disjoint2/1},
 * {@code lex_chain/1}, {@code chain/2}, {@code automaton/3,8}, {@code zcompare/3},
 * {@code fd_degree/2}, {@code global_cardinality/3}), coefficients beyond 64 bits, and an
 * iterative branch and bound. Timeouts are generous (loaded CI machines); the measured timings
 * are in the wave record.
 */
public class ClpfdV46Test {

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

    /** The first answer as the toplevel prints it (bindings, then residual goals), joined by ", ". */
    private String answer(String query) {
        final String[] out = {null};
        p.solveStream(query, new Prolog.AnswerSink() {
            @Override public boolean onAnswer(Map<String, Term> a, boolean more) {
                out[0] = String.join(", ", Answer.lines(a, p.residualGoals(a), p.getOps().table()));
                return false;
            }
        });
        assertNotNull("must succeed: " + query, out[0]);
        return out[0];
    }

    // ================================================================ Q5.1 residual goals

    @Test(timeout = 20000)
    public void testISS0760_ResidualLinearFormsAreSwis() {
        assertEquals("Y#=<X+ -1", answer("X #> Y."));
        assertEquals("X#=<Y+ -1", answer("X #< Y."));
        assertEquals("X#>=Y", answer("X #>= Y."));
        assertEquals("Y#>=X", answer("X #=< Y."));
        assertEquals("Y+Z#=X", answer("X #= Y+Z."));
        assertEquals("Y+2#=X", answer("X #= Y+2."));
        assertEquals("2*Y#=X", answer("X #= 2*Y."));
        assertEquals("X#\\=Y", answer("X #\\= Y."));
        assertEquals("X#\\=Y+1", answer("X #\\= Y+1."));
        assertEquals("X in 1..2\\/4..5", answer("X in 1..5, X #\\= 3."));
        // a constraint that became entailed is gone; the domain says the rest
        assertEquals("X = 5, Y in inf..4", answer("X #> Y, X = 5."));
    }

    @Test(timeout = 20000)
    public void testISS0760_ResidualGlobalsAndReification() {
        assertEquals("all_different([X,Y])", answer("all_different([X,Y])."));
        assertEquals("X in 1..3, all_different([X,Y]), Y in 1..3",
                     answer("X in 1..3, Y in 1..3, all_different([X,Y])."));
        assertEquals("B in 0..1, X#>=4#<==>B", answer("B #<==> X #> 3."));
        // B decided: the reification collapses into its (entailed or posted) comparison
        assertEquals("B = 1, X in 4..sup", answer("B #<==> X #> 3, B = 1."));
        // an auxiliary variable of the compiled store prints as a fresh variable
        assertEquals("W+_A#=X, Y*Z#=_A", answer("X #= Y*Z+W."));
    }

    @Test(timeout = 20000)
    public void testISS0760_CopyTerm3AndAttributeGoals() {
        ok("X #> Y, copy_term(X-Y, C, Gs), C = A-B, Gs == [B #=< A + -1].");
        ok("X #> 3, clpfd:attribute_goals(X, G, []), G == [X in 4..sup].");
        ok("X in 1..3, copy_term(X, C, Gs), Gs == [C in 1..3], \\+ fd_var(C).");
        // the goals restore the constraints on the copy
        ok("X #> Y, copy_term(X-Y, A-B, Gs), maplist(call, Gs), A = 3, fd_sup(B, 2).");
    }

    @Test(timeout = 20000)
    public void testISS0760_ResidualsCoexistWithFreezeAndDif() {
        String a = answer("freeze(X, true), dif(Y, a), X #> Z.");
        assertTrue(a, a.contains("freeze(X,true)"));
        assertTrue(a, a.contains("dif(Y,a)"));
        assertTrue(a, a.contains("Z#=<X+ -1"));
    }

    @Test(timeout = 20000)
    public void testISS0761_EqualityOfTwoVariablesUnifies() {
        ok("X #= Y, X == Y.");
        ok("X #= Y, X in 1..3, fd_dom(Y, D), D == 1..3.");
        String a = answer("X #= Y.");
        assertTrue(a, a.startsWith("Y = X") || a.startsWith("X = Y"));
    }

    // ================================================================ Q5.2 globals

    @Test(timeout = 20000)
    public void testISS0762_CircuitForbidsSubtours() {
        assertEquals("6", val("length(L, 4), circuit(L), findall(L, label(L), Ls), length(Ls, N).", "N"));
        no("circuit([2,1,_]).");                              // 1-2 is a subtour
        no("circuit([A,B,C,D]), A = 2, B = 1.");
        no("circuit([A,B,C]), A = 1.");                       // a self loop
        ok("circuit([1]).");
        assertEquals("[2,3,1]", val("circuit([A,B,C]), A = 2, term_to_atom([A,B,C], T).", "T"));
    }

    private static final String KNIGHT =
        "n_tour(N, Ts) :- length(Ts, N), maplist(kt_len(N), Ts), kt_app(Ts, Vs), kt_succ(Vs, N, 1), circuit(Vs).\n"
      + "kt_len(N, L) :- length(L, N).\n"
      + "kt_app([], []).\n"
      + "kt_app([L|Ls], As) :- append(L, Ws, As), kt_app(Ls, Ws).\n"
      + "kt_succ([], _, _).\n"
      + "kt_succ([V|Vs], N, K0) :- findall(Num, n_k_next(N, K0, Num), [Next|Nexts]),\n"
      + "    foldl(kt_dom, Nexts, Next, Dom), V in Dom, K1 is K0 + 1, kt_succ(Vs, N, K1).\n"
      + "kt_dom(N, D0, D0\\/N).\n"
      + "n_x_y_k(N, X, Y, K) :- [X,Y] ins 1..N, K #= N*(Y-1) + X.\n"
      + "n_k_next(N, K0, K) :- n_x_y_k(N, X0, Y0, K0), [DX,DY] ins -2 \\/ -1 \\/ 1 \\/ 2,\n"
      + "    abs(DX) + abs(DY) #= 3, [X,Y] ins 1..N, X #= X0 + DX, Y #= Y0 + DY,\n"
      + "    n_x_y_k(N, X, Y, K), label([DX,DY]).\n"
      // an independent check: a permutation, every step a knight move, one cycle through all
      + "tour_ok(N, Vs) :- NN is N*N, msort(Vs, S), findall(I, between(1, NN, I), S),\n"
      + "    kt_walk(Vs, N, 1, NN).\n"
      + "kt_walk(Vs, N, K, Left) :- nth1(K, Vs, Next), kt_move(N, K, Next), Left1 is Left - 1,\n"
      + "    ( Left1 =:= 0 -> Next =:= 1 ; Next =\\= 1, kt_walk(Vs, N, Next, Left1) ).\n"
      + "kt_move(N, A, B) :- XA is (A-1) mod N, YA is (A-1) // N, XB is (B-1) mod N, YB is (B-1) // N,\n"
      + "    DX is abs(XA-XB), DY is abs(YA-YB), ( DX =:= 1, DY =:= 2 -> true ; DX =:= 2, DY =:= 1 ).\n";

    @Test(timeout = 60000)
    public void testISS0762_KnightsTour6x6ViaCircuit() {
        p.consult(KNIGHT);
        long t0 = System.nanoTime();
        List<Map<String, Term>> s = q("n_tour(6, Ts), kt_app(Ts, Vs), labeling([ff], Vs), !, tour_ok(6, Vs).");
        long ms = (System.nanoTime() - t0) / 1_000_000;
        assertEquals("a closed 6x6 knight's tour", 1, s.size());
        assertTrue("the 6x6 tour took " + ms + " ms", ms < 30000);
        no("n_tour(5, Ts), kt_app(Ts, Vs), labeling([ff], Vs).");   // odd boards have none
    }

    @Test(timeout = 30000)
    public void testISS0763_CumulativeJobShop() {
        // three jobs, two machines, operations in job order; the M1 load (9) is the optimum
        String js =
            "jobshop(End, Ss) :- Ss = [A1,A2,B1,B2,C1,C2], Ss ins 0..20,\n"
          + "  A1 + 3 #=< A2, B1 + 2 #=< B2, C1 + 2 #=< C2,\n"
          + "  cumulative([task(A1,3,_,1,a1), task(B2,4,_,1,b2), task(C1,2,_,1,c1)], [limit(1)]),\n"
          + "  cumulative([task(A2,2,_,1,a2), task(B1,2,_,1,b1), task(C2,3,_,1,c2)]),\n"
          + "  End #= max(A2+2, max(B2+4, C2+3)).\n";
        p.consult(js);
        assertEquals("9", val("jobshop(End, Ss), once(labeling([min(End)], Ss)).", "End"));
        no("jobshop(End, Ss), End #< 9, label(Ss).");
        // the time-table prunes before labeling: a compulsory part blocks the machine
        ok("S in 1..10, T in 2..3, cumulative([task(S,3,_,1,x), task(T,2,_,1,y)]),"
           + " fd_dom(S, D), D == 4..10.");
        no("cumulative([task(0,3,_,2,a), task(1,3,_,2,b)], [limit(3)]).");
        ok("cumulative([task(0,3,_,1,a), task(1,3,_,2,b)], [limit(3)]).");
    }

    @Test(timeout = 20000)
    public void testISS0764_ChainLexChainDisjoint2() {
        assertEquals("2", val("chain([A,B,C], #<), A = 1, C = 3.", "B"));
        ok("catch(chain([_,_], foo), error(domain_error(chain_relation, foo), _), true).");
        ok("lex_chain([[A],[1]]), A in 0..5, fd_dom(A, D), D == 0..1.");
        ok("lex_chain([[1,A],[1,B]]), A = 3, fd_inf(B, 3).");
        no("lex_chain([[1,2],[1,1]]).");
        assertEquals("6", val("disjoint2([r(X,2,0,2), r(Y,2,0,2)]), [X,Y] ins 0..3,"
                              + " findall(X-Y, label([X,Y]), L), length(L, N).", "N"));
    }

    @Test(timeout = 20000)
    public void testISS0765_Automaton() {
        // SWI's documentation example: sequences of 0/1 with two consecutive 1s
        p.consult("sequence(Vs) :- automaton(Vs, [source(a),sink(c)],"
            + " [arc(a,0,a), arc(a,1,b), arc(b,0,a), arc(b,1,c), arc(c,0,c), arc(c,1,c)]).\n");
        ok("sequence([0,0,1,1]).");
        no("sequence([0,1,0,1]).");
        assertEquals("[[0,1,1],[1,1,0],[1,1,1]]",
                     val("length(Vs, 3), sequence(Vs), findall(Vs, label(Vs), L), term_to_atom(L, T).", "T"));
        // automaton/8 with a counter: exactly two 1s among four
        assertEquals("6", val("length(Vs, 4), Vs ins 0..1, automaton(Vs, _, Vs, [source(s),sink(s)],"
            + " [arc(s,0,s), arc(s,1,s,[C+1])], [C], [0], [2]), findall(Vs, label(Vs), L), length(L, N).", "N"));
    }

    @Test(timeout = 20000)
    public void testISS0766_ZcompareAndFdDegree() {
        assertEquals("<", val("zcompare(O, 1, 2).", "O"));
        assertEquals("<", val("X in 1..3, Y in 5..7, zcompare(O, X, Y).", "O"));
        assertEquals("=", val("zcompare(O, X, Y), X = 3, Y = 3.", "O"));
        ok("zcompare(<, X, Y), X = 3, fd_inf(Y, 4).");
        ok("X #> Y, fd_degree(X, 1), fd_degree(3, 0), fd_degree(_, 0).");
        ok("X #> Y, Y #> Z, fd_degree(Y, 2), Z = 1, X = 9, fd_degree(Y, 0).");
    }

    @Test(timeout = 20000)
    public void testISS0767_GlobalCardinalityOptions() {
        assertEquals("4", val("global_cardinality([X,Y,Z], [1-_, 2-_], [cost(C, [[1,5],[1,5],[2,2]])]),"
                              + " once(labeling([min(C)], [X,Y,Z])).", "C"));
        ok("global_cardinality([X,Y], [1-1, 2-1], [consistency(value)]), X = 1, Y == 2.");
        ok("catch(global_cardinality([_], [1-_], [foo]),"
           + " error(domain_error(global_cardinality_option, foo), _), true).");
    }

    /** Found while writing automaton/8: a module predicate whose first-argument index selected
     *  no clause raised existence_error instead of failing. */
    @Test(timeout = 20000)
    public void testISS0770_ModuleIndexMissFails() {
        p.consult(":- module(m770, [t770/2]).\n"
                + "t770(X, R) :- ( p770(X) -> R = yes ; R = no ).\n"
                + "p770([_|_]).\n");
        assertEquals("no", val("t770([], R).", "R"));
        assertEquals("yes", val("t770([a], R).", "R"));
        no("m770:p770([]).");
        // and inside a library module (the prelude helper that exposed it)
        ok("length(Vs, 2), automaton(Vs, [source(a),sink(a)], [arc(a,0,a)]), Vs == [0,0].");
    }

    // ================================================================ Q5.3 big coefficients

    @Test(timeout = 20000)
    public void testISS0768_CoefficientsBeyond64Bits() {
        // 4.5: representation_error(max_integer) at post time
        assertEquals("300000000000000000000", val("X*10^20 #= Y, X = 3.", "Y"));
        assertEquals("3", val("X*10^20 #= Y, Y = 300000000000000000000.", "X"));
        ok("X in 1..10, 10^20*X #< 5*10^20, fd_dom(X, D), D == 1..4.");
        ok("X in 0..9, 10^20*X #\\= 5*10^20, fd_dom(X, D), D == 0..4\\/6..9.");
        ok("X in 1..3, B #<==> (10^20*X #>= 2*10^20), X = 1, B == 0.");
        // the fast (long) path is still what an ordinary constraint gets
        ok("X in 1..10, 3*X #< 10, fd_dom(X, D), D == 1..3.");
    }

    // ================================================================ Q5.4 branch and bound

    @Test(timeout = 60000)
    public void testISS0769_BranchAndBoundIsIterative() {
        // 60 variables: a strict chain, the optimum is 59
        assertEquals("59", val("length(Vs, 60), Vs ins 0..100, chain(Vs, #<), last(Vs, L),"
                               + " once(labeling([min(L)], Vs)).", "L"));
        // 10000 variables: the recursive labeler recursed once per variable (StackOverflowError)
        assertEquals("1", val("length(Vs, 10000), Vs ins 0..1, chain(Vs, #=<), last(Vs, L),"
                              + " once(labeling([max(L)], Vs)), sum(Vs, #=, S).", "S"));
        // the solutions still come in objective order
        assertEquals("[3-3,2-4,4-2,1-5,5-1]",
                     val("X in 1..5, Y in 1..5, X+Y #= 6, findall(X-Y, labeling([max(X*Y)], [X,Y]), L),"
                         + " term_to_atom(L, T).", "T"));
    }
}
// END_CHANGE: ISS-2025-0760
