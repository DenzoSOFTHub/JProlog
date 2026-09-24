package it.denzosoft.jprolog.test.integration;

import it.denzosoft.jprolog.core.engine.Prolog;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Classic Prolog programs, run whole, with their FULL answers asserted.
 *
 * <p>START_CHANGE: ISS-2025-0666 - the previous version of this class "adapted" every program to
 * a toy (a Hanoi that checked {@code hanoi_move(1, from_a, to_b)} was a fact, queens that looked
 * up {@code queen_position(2, X)}, a cryptarithmetic that checked {@code digit(3)}), so nothing
 * of the engine's search was exercised. These are the real programs: all 92 solutions of the
 * 8-queens problem, the Hanoi move list, quicksort against msort, the zebra puzzle,
 * SEND+MORE=MONEY through CLP(FD), Ackermann, a sieve of Eratosthenes and a DCG expression
 * parser/evaluator. Every expected value is checked with {@code ==} inside the query, so a wrong
 * binding fails the query instead of being compared as text. Each test has a JUnit timeout and
 * the whole class runs in well under 1 GB. END_CHANGE: ISS-2025-0666
 */
public class FamousPrologProgramsTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private void holds(String query) {
        assertEquals(query, 1, prolog.solve(query).size());
    }

    @Test(timeout = 30000)
    public void testFactorialAndFibonacci() {
        prolog.consult(
            "factorial(0, 1) :- !.\n"
          + "factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.\n"
          + "fib(0, 0).\n"
          + "fib(1, 1).\n"
          + "fib(N, F) :- N > 1, A is N - 1, B is N - 2, fib(A, FA), fib(B, FB), F is FA + FB.\n");
        holds("factorial(5, F), F == 120.");
        holds("factorial(25, F), F == 15511210043330985984000000.");   // past 64 bits, exact
        holds("findall(F, (between(0, 15, N), fib(N, F)), L), "
            + "L == [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610].");
        assertEquals("fib/2 is deterministic in its answers", 1, prolog.solve("fib(10, F).").size());
    }

    @Test(timeout = 60000)
    public void testEightQueensAllSolutions() {
        prolog.consult(
            "queens(N, Qs) :- numlist(1, N, Ns), place(Ns, [], Qs).\n"
          + "place([], Qs, Qs).\n"
          + "place(Unplaced, Safe, Qs) :- select(Q, Unplaced, R), no_attack(Q, Safe, 1),\n"
          + "    place(R, [Q|Safe], Qs).\n"
          + "no_attack(_, [], _).\n"
          + "no_attack(Q, [Q1|Qs], D) :- Q =\\= Q1 + D, Q =\\= Q1 - D, D1 is D + 1,\n"
          + "    no_attack(Q, Qs, D1).\n"
          // an independent checker: a permutation of 1..8 with no two queens on a diagonal
          + "valid(Qs) :- msort(Qs, [1,2,3,4,5,6,7,8]),\n"
          + "    \\+ ( nth1(I, Qs, A), nth1(J, Qs, B), I < J, abs(A - B) =:= J - I ).\n");
        holds("findall(Qs, queens(8, Qs), All), length(All, 92), sort(All, Set), length(Set, 92), "
            + "forall(member(Q, All), valid(Q)).");
        holds("findall(Qs, queens(6, Qs), L), length(L, 4).");
        holds("\\+ queens(3, _).");
        // the 92 are exactly the valid permutations (brute force over all 8! orders)
        holds("findall(P, (numlist(1, 8, Ns), permutation(Ns, P), valid(P)), B), length(B, 92), "
            + "msort(B, SB), findall(Q, queens(8, Q), A), msort(A, SA), SA == SB.");
    }

    @Test(timeout = 30000)
    public void testTowerOfHanoiMoveList() {
        prolog.consult(
            "hanoi(0, _, _, _, []) :- !.\n"
          + "hanoi(N, From, To, Via, Moves) :- N1 is N - 1,\n"
          + "    hanoi(N1, From, Via, To, Before), hanoi(N1, Via, To, From, After),\n"
          + "    append(Before, [From-To|After], Moves).\n");
        holds("hanoi(3, a, c, b, M), M == [a-c, a-b, c-b, a-c, b-a, b-c, a-c].");
        // 2^10 - 1 moves; the largest disk moves once, in the middle; an even tower starts on
        // the spare peg and the smallest disk's last hop comes from it
        holds("hanoi(10, a, c, b, M), length(M, 1023), M = [First|_], last(M, Last), nth1(512, M, Mid), "
            + "First == a-b, Last == b-c, Mid == a-c.");
        assertEquals("one answer", 1, prolog.solve("hanoi(4, a, c, b, M).").size());
    }

    @Test(timeout = 30000)
    public void testQuicksort() {
        prolog.consult(
            "quicksort([], []).\n"
          + "quicksort([H|T], S) :- partition(H, T, L, G), quicksort(L, SL), quicksort(G, SG),\n"
          + "    append(SL, [H|SG], S).\n"
          + "partition(_, [], [], []).\n"
          + "partition(P, [X|Xs], [X|L], G) :- X =< P, !, partition(P, Xs, L, G).\n"
          + "partition(P, [X|Xs], L, [X|G]) :- partition(P, Xs, L, G).\n"
          // a deterministic pseudo-random list (LCG) for the comparison with msort/2
          + "lcg(0, _, []) :- !.\n"
          + "lcg(N, S, [X|Xs]) :- S1 is (S * 1103515245 + 12345) mod 2147483648, X is S1 mod 1000,\n"
          + "    N1 is N - 1, lcg(N1, S1, Xs).\n");
        holds("quicksort([3,1,4,1,5,9,2,6,5,3,5], S), S == [1,1,2,3,3,4,5,5,5,6,9].");
        holds("quicksort([], S), S == [].");
        holds("lcg(3000, 42, L), quicksort(L, Q), msort(L, M), Q == M, length(Q, 3000).");
    }

    @Test(timeout = 60000)
    public void testZebraPuzzle() {
        prolog.consult(
            "right_of(X, Y, [Y,X|_]).\n"
          + "right_of(X, Y, [_|T]) :- right_of(X, Y, T).\n"
          + "next_to(X, Y, L) :- right_of(X, Y, L) ; right_of(Y, X, L).\n"
          + "zebra(Owner, Water, Hs) :-\n"
          + "    Hs = [h(_,norwegian,_,_,_), _, h(_,_,_,milk,_), _, _],\n"
          + "    member(h(red,english,_,_,_), Hs),\n"
          + "    member(h(_,spanish,dog,_,_), Hs),\n"
          + "    member(h(green,_,_,coffee,_), Hs),\n"
          + "    member(h(_,ukrainian,_,tea,_), Hs),\n"
          + "    right_of(h(green,_,_,_,_), h(ivory,_,_,_,_), Hs),\n"
          + "    member(h(_,_,snails,_,oldgold), Hs),\n"
          + "    member(h(yellow,_,_,_,kools), Hs),\n"
          + "    next_to(h(_,_,_,_,chesterfield), h(_,_,fox,_,_), Hs),\n"
          + "    next_to(h(_,_,_,_,kools), h(_,_,horse,_,_), Hs),\n"
          + "    member(h(_,_,_,orange_juice,luckystrike), Hs),\n"
          + "    member(h(_,japanese,_,_,parliament), Hs),\n"
          + "    next_to(h(_,norwegian,_,_,_), h(blue,_,_,_,_), Hs),\n"
          + "    member(h(_,Owner,zebra,_,_), Hs),\n"
          + "    member(h(_,Water,_,water,_), Hs).\n");
        holds("findall(O-W, zebra(O, W, _), L), L == [japanese-norwegian].");
        holds("zebra(_, _, Hs), Hs == [h(yellow,norwegian,fox,water,kools), "
            + "h(blue,ukrainian,horse,tea,chesterfield), h(red,english,snails,milk,oldgold), "
            + "h(ivory,spanish,dog,orange_juice,luckystrike), h(green,japanese,zebra,coffee,parliament)].");
    }

    @Test(timeout = 60000)
    public void testSendMoreMoneyWithClpfd() {
        prolog.consult(
            "puzzle([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]) :-\n"
          + "    Vars = [S,E,N,D,M,O,R,Y], Vars ins 0..9, all_different(Vars),\n"
          + "    S*1000 + E*100 + N*10 + D + M*1000 + O*100 + R*10 + E\n"
          + "        #= M*10000 + O*1000 + N*100 + E*10 + Y,\n"
          + "    M #\\= 0, S #\\= 0, label(Vars).\n");
        holds("findall(X+Y=Z, puzzle(X, Y, Z), L), "
            + "L == [[9,5,6,7]+[1,0,8,5]=[1,0,6,5,2]].");
    }

    @Test(timeout = 30000)
    public void testAckermann() {
        prolog.consult(
            "ack(0, N, R) :- !, R is N + 1.\n"
          + "ack(M, 0, R) :- !, M1 is M - 1, ack(M1, 1, R).\n"
          + "ack(M, N, R) :- M1 is M - 1, N1 is N - 1, ack(M, N1, R1), ack(M1, R1, R).\n");
        holds("ack(2, 3, R), R == 9.");
        holds("ack(3, 3, R), R == 61.");
        holds("ack(3, 6, R), R == 509.");
        holds("findall(R, (between(0, 4, N), ack(1, N, R)), L), L == [2,3,4,5,6].");
    }

    @Test(timeout = 30000)
    public void testPrimesSieve() {
        prolog.consult(
            "primes(N, Ps) :- numlist(2, N, L), sieve(L, Ps).\n"
          + "sieve([], []).\n"
          + "sieve([P|Xs], [P|Ps]) :- remove_multiples(P, Xs, Ys), sieve(Ys, Ps).\n"
          + "remove_multiples(_, [], []).\n"
          + "remove_multiples(P, [X|Xs], Ys) :- 0 is X mod P, !, remove_multiples(P, Xs, Ys).\n"
          + "remove_multiples(P, [X|Xs], [X|Ys]) :- remove_multiples(P, Xs, Ys).\n"
          + "sieve_lambda([], []).\n"
          + "sieve_lambda([P|Xs], [P|Ps]) :- exclude([X]>>(0 is X mod P), Xs, Ys), sieve_lambda(Ys, Ps).\n");
        holds("primes(100, Ps), Ps == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,"
            + "73,79,83,89,97].");
        holds("primes(3000, Ps), length(Ps, 430), last(Ps, L), L == 2999.");
        holds("numlist(2, 100, L), sieve_lambda(L, Ps), primes(100, Qs), Ps == Qs.");
    }

    @Test(timeout = 30000)
    public void testDcgExpressionParserEvaluator() {
        prolog.consult(
            "expr(V) --> term(T), expr_rest(T, V).\n"
          + "expr_rest(Acc, V) --> [0'+], !, term(T), {A is Acc + T}, expr_rest(A, V).\n"
          + "expr_rest(Acc, V) --> [0'-], !, term(T), {A is Acc - T}, expr_rest(A, V).\n"
          + "expr_rest(V, V) --> [].\n"
          + "term(V) --> factor(F), term_rest(F, V).\n"
          + "term_rest(Acc, V) --> [0'*], !, factor(F), {A is Acc * F}, term_rest(A, V).\n"
          + "term_rest(Acc, V) --> [0'/], !, factor(F), {A is Acc // F}, term_rest(A, V).\n"
          + "term_rest(V, V) --> [].\n"
          + "factor(V) --> [0'(], !, expr(V), [0')].\n"
          + "factor(V) --> digits(Ds), {Ds \\== [], number_codes(V, Ds)}.\n"
          + "digits([D|T]) --> [D], {code_type(D, digit)}, !, digits(T).\n"
          + "digits([]) --> [].\n"
          + "calc(Atom, V) :- atom_codes(Atom, Cs), phrase(expr(V), Cs).\n");
        holds("calc('2*(3+4)-5', V), V == 9.");
        holds("calc('10-2-3', V), V == 5.");                 // left associative
        holds("calc('100/7/2', V), V == 7.");
        holds("calc('2+3*4', V), V == 14.");                 // precedence
        holds("calc('((((1))))', V), V == 1.");
        holds("\\+ calc('2+*3', _).");                       // a syntax error fails the parse
        holds("atom_codes('1+2', Cs), phrase(expr(V), Cs, Rest), V == 3, Rest == [].");
    }

    // START_CHANGE: ISS-2025-0798 - 4.6 wave Q7: four programs that use what 4.6 added —
    // library(solution_sequences), file_search_path/2 library loading, mode-directed tabling and
    // the CLP(FD) circuit/1 global — run whole, like the classics above.

    @Test(timeout = 30000)
    public void testSolutionSequencesPipeline() {
        prolog.consult(
            "sale(apple, 3).  sale(pear, 5).  sale(apple, 3).  sale(fig, 9).\n"
          + "sale(kiwi, 2).   sale(pear, 5).  sale(plum, 4).   sale(date, 7).\n"
          // the distinct sales, cheapest first, second page of two
          + "page(N, Size, P-C) :- Off is N*Size,\n"
          + "    limit(Size, offset(Off, order_by([asc(C), asc(P)], distinct(P-C, sale(P, C))))).\n");
        holds("findall(X, page(0, 2, X), L), L == [kiwi-2, apple-3].");
        holds("findall(X, page(1, 2, X), L), L == [plum-4, pear-5].");
        holds("findall(X, page(3, 2, X), L), L == [].");
        holds("findall(P-C, order_by([desc(C)], distinct(P-C, sale(P, C))), L), "
            + "L == [fig-9, date-7, pear-5, plum-4, apple-3, kiwi-2].");
        // the third distinct product in source order, and its position
        holds("call_nth(distinct(P, sale(P, _)), 3), P == fig.");
        holds("findall(N-P, call_nth(distinct(P, sale(P, _)), N), L), last(L, Last), Last == 6-date.");
        // limit/2 stops an infinite generator
        holds("findall(X, limit(4, (repeat, X = r)), L), L == [r,r,r,r].");
    }

    @Test(timeout = 30000)
    public void testFileSearchPathLibraryLoad() throws Exception {
        java.io.File dir = java.nio.file.Files.createTempDirectory("jprolog-q7-fsp").toFile();
        java.io.File lib = new java.io.File(dir, "geometry");
        assertEquals(true, lib.mkdir());
        java.nio.file.Files.write(new java.io.File(lib, "shapes.pl").toPath(), (
              ":- module(shapes, [area/2]).\n"
            + "area(square(S), A) :- A is S*S.\n"
            + "area(rect(W, H), A) :- A is W*H.\n"
            + "area(circle(R), A) :- A is pi*R*R.\n").getBytes("UTF-8"));
        java.nio.file.Files.write(new java.io.File(dir, "totals.pl").toPath(), (
              ":- module(totals, [total_area/2]).\n"
            + ":- use_module(geo(shapes)).\n"
            + "total_area(Shapes, T) :- foldl([S, A0, A]>>(area(S, X), A is A0 + X), Shapes, 0, T).\n")
            .getBytes("UTF-8"));
        String d = dir.getAbsolutePath().replace("\\", "/");
        // an alias to a directory, and an alias defined through another alias
        holds("assertz(user:file_search_path(mylibs, '" + d + "')), "
            + "assertz(user:file_search_path(geo, mylibs(geometry))).");
        holds("use_module(mylibs(totals)).");
        holds("total_area([square(2), rect(2, 3)], T), T =:= 10.");
        holds("absolute_file_name(geo(shapes), F, [file_type(prolog), access(read)]), "
            + "sub_atom(F, _, _, 0, 'geometry/shapes.pl').");
        // library(X) is searched on the user's library directories too
        holds("assertz(user:file_search_path(library, mylibs(geometry))).");
        holds("use_module(library(shapes)), area(circle(1), A), abs(A - pi) < 1.0e-9.");
        holds("catch(use_module(mylibs(nosuch)), error(existence_error(source_sink, _), _), true).");
    }

    @Test(timeout = 30000)
    public void testTabledShortestPathWithMin() {
        prolog.consult(
            ":- table path(_, _, min).\n"
          + "edge(a, b, 4). edge(a, c, 1). edge(c, b, 2). edge(b, d, 5).\n"
          + "edge(c, d, 8). edge(d, e, 3). edge(e, a, 1). edge(b, e, 9).\n"
          + "path(X, Y, C) :- edge(X, Y, C).\n"
          + "path(X, Y, C) :- path(X, Z, C0), edge(Z, Y, C1), C is C0 + C1.\n");
        // the graph is cyclic (a -> ... -> e -> a): only tabling makes the left recursion terminate
        holds("path(a, b, C), C == 3.");
        holds("path(a, d, C), C == 8.");
        holds("path(a, e, C), C == 11.");
        holds("path(a, a, C), C == 12.");
        holds("findall(Y-C, path(a, Y, C), L), msort(L, S), S == [a-12, b-3, c-1, d-8, e-11].");
        // one answer per target: the aggregate replaces the worse ones
        holds("aggregate_all(count, path(a, _, _), N), N == 5.");
        // a bound moded argument is evaluated free and then compared (SWI)
        holds("path(a, b, 3).");
        holds("\\+ path(a, b, 4).");
    }

    @Test(timeout = 60000)
    public void testKnightsTour6x6WithClpfd() {
        prolog.consult(
            ":- use_module(library(clpfd)).\n"
          + "n_tour(N, Ts) :- length(Ts, N), maplist(kt_len(N), Ts), append(Ts, Vs),\n"
          + "    kt_succ(Vs, N, 1), circuit(Vs).\n"
          + "kt_len(N, L) :- length(L, N).\n"
          + "kt_succ([], _, _).\n"
          + "kt_succ([V|Vs], N, K0) :- findall(Num, n_k_next(N, K0, Num), [Next|Nexts]),\n"
          + "    foldl(kt_dom, Nexts, Next, Dom), V in Dom, K1 is K0 + 1, kt_succ(Vs, N, K1).\n"
          + "kt_dom(N, D0, D0\\/N).\n"
          + "n_x_y_k(N, X, Y, K) :- [X,Y] ins 1..N, K #= N*(Y-1) + X.\n"
          + "n_k_next(N, K0, K) :- n_x_y_k(N, X0, Y0, K0), [DX,DY] ins -2 \\/ -1 \\/ 1 \\/ 2,\n"
          + "    abs(DX) + abs(DY) #= 3, [X,Y] ins 1..N, X #= X0 + DX, Y #= Y0 + DY,\n"
          + "    n_x_y_k(N, X, Y, K), label([DX,DY]).\n"
          // an independent check: a permutation, every step a knight move, one cycle through all
          + "tour_ok(N, Vs) :- NN is N*N, msort(Vs, S), numlist(1, NN, S), kt_walk(Vs, N, 1, NN).\n"
          + "kt_walk(Vs, N, K, Left) :- nth1(K, Vs, Next), kt_move(N, K, Next), Left1 is Left - 1,\n"
          + "    ( Left1 =:= 0 -> Next =:= 1 ; Next =\\= 1, kt_walk(Vs, N, Next, Left1) ).\n"
          + "kt_move(N, A, B) :- XA is (A-1) mod N, YA is (A-1) // N, XB is (B-1) mod N,\n"
          + "    YB is (B-1) // N, DX is abs(XA-XB), DY is abs(YA-YB),\n"
          + "    ( DX =:= 1, DY =:= 2 -> true ; DX =:= 2, DY =:= 1 ).\n");
        holds("once((n_tour(6, Ts), append(Ts, Vs), labeling([ff], Vs))), tour_ok(6, Vs).");
        holds("\\+ (n_tour(5, Ts), append(Ts, Vs), labeling([ff], Vs)).");   // odd boards have none
    }
    // END_CHANGE: ISS-2025-0798
}
