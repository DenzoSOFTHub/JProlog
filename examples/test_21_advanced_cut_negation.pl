% ===================================================================
% TEST 21: Advanced Cut, Failure, and Negation Tests
% ===================================================================
% Comprehensive tests for cut (!), fail, negation-as-failure (\+),
% if-then-else (-> ;), and their interactions.

% =====================
% SECTION 1: CUT BASICS
% =====================

% 1.1 Cut prevents backtracking to alternative clauses
first_match(X, Y) :- X = a, Y = 1, !.
first_match(X, Y) :- X = b, Y = 2.
first_match(X, Y) :- X = c, Y = 3.

% 1.2 Cut in the first clause should stop after first solution
color(red).
color(green).
color(blue).

first_color(X) :- color(X), !.

% 1.3 Green cut: doesn't change logical meaning but improves efficiency
max_g(X, Y, X) :- X >= Y, !.
max_g(_, Y, Y).

% 1.4 Red cut: changes logical meaning (dangerous)
classify(X, positive) :- X > 0, !.
classify(0, zero) :- !.
classify(_, negative).

% 1.5 Cut with multiple body goals before it
find_first_even(X, [X|_]) :- 0 is X mod 2, !.
find_first_even(X, [_|T]) :- find_first_even(X, T).

% 1.6 Deterministic member (no backtracking after first match)
det_member(X, [X|_]) :- !.
det_member(X, [_|T]) :- det_member(X, T).

% =========================
% SECTION 2: NEGATION (\+)
% =========================

% 2.1 Basic negation
is_not_zero(X) :- \+ X =:= 0.

% 2.2 Negation with compound goal
safe_divide(X, Y, R) :-
    \+ Y =:= 0,
    R is X / Y.

% 2.3 Negation should not bind variables
% \+ should succeed but not export bindings
test_neg_no_bind(X) :-
    X = hello,
    \+ X = goodbye.

% 2.4 Double negation
double_neg_test(X) :- \+ \+ X = a.

% 2.5 Negation in list processing
not_in_list(_, []).
not_in_list(X, [H|T]) :-
    \+ X = H,
    not_in_list(X, T).

% 2.6 Difference of two lists using negation
list_diff([], _, []).
list_diff([H|T], L2, [H|Diff]) :-
    \+ member(H, L2),
    list_diff(T, L2, Diff).
list_diff([H|T], L2, Diff) :-
    member(H, L2),
    list_diff(T, L2, Diff).

% 2.7 Exclusive elements (in one list but not the other)
exclusive(L1, L2, Excl) :-
    list_diff(L1, L2, D1),
    list_diff(L2, L1, D2),
    append(D1, D2, Excl).

% =================================
% SECTION 3: CUT + NEGATION COMBO
% =================================

% 3.1 once/1 simulation: find exactly one solution
my_once(Goal) :- call(Goal), !.

% 3.2 not/1 defined via cut and fail
my_not(Goal) :- call(Goal), !, fail.
my_not(_).

% 3.3 Equivalence of \+ and my_not
animal(cat).
animal(dog).
animal(bird).
plant(rose).
plant(tulip).

% Test: \+ animal(rose) should succeed
% Test: my_not(animal(rose)) should succeed
% Test: \+ plant(cat) should succeed
% Test: my_not(plant(cat)) should succeed

% 3.4 Cut inside negation should not escape
% Now that \+ (conjunction) parsing is fixed, test directly
test_cut_in_neg :- \+ (true, !, fail).

% =================================
% SECTION 4: IF-THEN-ELSE (-> ;)
% =================================

% 4.1 Basic if-then-else
abs_val(X, Y) :-
    (X >= 0 -> Y = X ; Y is -X).

% 4.2 Chained if-then-else (multiway branch)
category(X, tiny)   :- X < 10, !.
category(X, small)  :- X < 100, !.
category(X, medium) :- X < 1000, !.
category(_, large).

% Same with if-then-else
category2(X, Cat) :-
    (X < 10  -> Cat = tiny
    ; X < 100  -> Cat = small
    ; X < 1000 -> Cat = medium
    ; Cat = large).

% 4.3 If-then-else commitment: condition with multiple solutions
% ISO semantics: commits to FIRST solution of condition
vowel(a). vowel(e). vowel(i). vowel(o). vowel(u).
consonant(b). consonant(c). consonant(d). consonant(f).

classify_letter(L, Type) :-
    (vowel(L) -> Type = vowel ; Type = consonant).

% 4.4 Nested if-then-else
sign(X, Sign) :-
    (X > 0 -> Sign = positive
    ; (X < 0 -> Sign = negative
    ; Sign = zero)).

% 4.5 Disjunction without if-then (pure ;)
either(X) :- (X = left ; X = right).

% ====================================
% SECTION 5: FAIL AND FAILURE PATTERNS
% ====================================

% 5.1 Count solutions using findall with meta-variable goal
count_solutions(Goal, N) :-
    findall(yes, Goal, L),
    length(L, N).

% 5.2 Generate-and-test with failure (uses built-in between/3)
first_prime_in_range(Low, High, P) :-
    between(Low, High, P),
    P > 1,
    is_prime(P),
    !.

is_prime(2).
is_prime(3).
is_prime(N) :- N > 3, N mod 2 =\= 0, check_prime(N, 3).
check_prime(N, D) :- D * D > N, !.
check_prime(N, D) :- N mod D =\= 0, D2 is D + 2, check_prime(N, D2).

% 5.3 Failure-driven loop (collect via findall)
squares(Max, Squares) :-
    findall(X-S, (between(1, Max, X), S is X * X), Squares).

% ======================================
% SECTION 6: COMPLEX INTERACTION TESTS
% ======================================

% 6.1 Cut in nested calls
outer(X) :- inner(X).
inner(1) :- !.
inner(2).
inner(3).
% outer(X) should give X=1 only (cut in inner stops inner, but outer has one clause)

% 6.2 Cut scope: cut only affects the clause it appears in
parent(tom, bob).
parent(bob, ann).
parent(bob, pat).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% first_ancestor should find first descendant only
first_ancestor(X, Y) :- ancestor(X, Y), !.

% 6.3 Multiple cuts in same clause (second cut is redundant)
multi_cut(X) :- X > 0, !, X < 100, !.
multi_cut(X) :- X =:= 0.

% 6.4 Cut with backtracking in body before cut
fruit(apple).
fruit(banana).
fruit(cherry).

first_two_fruits(X, Y) :-
    fruit(X), fruit(Y), X \= Y, !.

% 6.5 Negation with cut interaction
safe_classify(X, Type) :-
    (\+ X > 0 -> Type = non_positive ; Type = positive).

% 6.6 Exhaustive search then cut
find_pair(X, Y) :-
    member(X, [1, 2, 3]),
    member(Y, [a, b, c]),
    X =:= 2,
    Y = b,
    !.

% ======================================
% SECTION 7: EDGE CASES
% ======================================

% 7.1 Cut in the first goal of body
immediate_cut(X) :- !, X = found.
immediate_cut(X) :- X = not_found.

% 7.2 Cut with empty alternative
only_positive(X, X) :- X > 0, !.

% 7.3 Negation of always-succeeding goal
neg_true :- \+ true.

% 7.4 Negation of always-failing goal
neg_fail :- \+ fail.

% 7.5 Deeply nested negation
deep_neg :- \+ \+ \+ fail.

% =====================
% TEST QUERIES
% =====================
% Section 1: Cut basics
% ?- first_match(a, Y).
% Expected: Y = 1 (only one solution due to cut)

% ?- first_color(X).
% Expected: X = red (only one solution)

% ?- max_g(5, 3, M).
% Expected: M = 5

% ?- max_g(3, 5, M).
% Expected: M = 5

% ?- classify(5, C).
% Expected: C = positive

% ?- classify(0, C).
% Expected: C = zero

% ?- classify(-3, C).
% Expected: C = negative

% ?- find_first_even(X, [1, 3, 4, 6, 8]).
% Expected: X = 4

% ?- det_member(b, [a, b, c, b]).
% Expected: true (one solution only)

% Section 2: Negation
% ?- is_not_zero(5).
% Expected: true

% ?- safe_divide(10, 2, R).
% Expected: R = 5

% ?- test_neg_no_bind(X).
% Expected: X = hello

% ?- not_in_list(d, [a, b, c]).
% Expected: true

% ?- list_diff([1, 2, 3, 4], [2, 4], D).
% Expected: D = [1, 3]

% Section 3: Cut + Negation combo
% ?- my_once(member(X, [a, b, c])).
% Expected: X = a (one solution only)

% ?- my_not(animal(rose)).
% Expected: true

% ?- my_not(animal(cat)).
% Expected: false (fails)

% ?- test_cut_in_neg.
% Expected: true

% Section 4: If-then-else
% ?- abs_val(-7, Y).
% Expected: Y = 7

% ?- abs_val(3, Y).
% Expected: Y = 3

% ?- category2(5, C).
% Expected: C = tiny

% ?- category2(50, C).
% Expected: C = small

% ?- category2(500, C).
% Expected: C = medium

% ?- category2(5000, C).
% Expected: C = large

% ?- classify_letter(a, T).
% Expected: T = vowel

% ?- classify_letter(b, T).
% Expected: T = consonant

% ?- sign(5, S).
% Expected: S = positive

% ?- sign(-3, S).
% Expected: S = negative

% ?- sign(0, S).
% Expected: S = zero

% ?- either(X).
% Expected: X = left ; X = right

% Section 5: Failure patterns
% ?- first_prime_in_range(10, 20, P).
% Expected: P = 11

% ?- squares(5, S).
% Expected: S = [1-1, 2-4, 3-9, 4-16, 5-25]

% Section 6: Complex interactions
% ?- outer(X).
% Expected: X = 1

% ?- first_ancestor(tom, Y).
% Expected: Y = bob

% ?- multi_cut(50).
% Expected: true

% ?- multi_cut(0).
% Expected: true

% ?- first_two_fruits(X, Y).
% Expected: X = apple, Y = banana

% ?- safe_classify(-3, T).
% Expected: T = non_positive

% ?- safe_classify(5, T).
% Expected: T = positive

% ?- find_pair(X, Y).
% Expected: X = 2, Y = b

% Section 7: Edge cases
% ?- immediate_cut(X).
% Expected: X = found

% ?- neg_true.
% Expected: false (fails)

% ?- neg_fail.
% Expected: true

% ?- deep_neg.
% Expected: true
