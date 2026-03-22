% Test 21: Last Call Optimization (LCO) Tests
% Tests that deeply recursive tail-call predicates don't overflow the stack

% Simple tail-recursive counter
count_down(0) :- !.
count_down(N) :-
    N > 0,
    N1 is N - 1,
    count_down(N1).

% Tail-recursive sum
sum_to(0, Acc, Acc) :- !.
sum_to(N, Acc, Result) :-
    N > 0,
    NewAcc is Acc + N,
    N1 is N - 1,
    sum_to(N1, NewAcc, Result).

% Tail-recursive list length
my_length([], Acc, Acc) :- !.
my_length([_|T], Acc, Result) :-
    NewAcc is Acc + 1,
    my_length(T, NewAcc, Result).

% Generate a list of N elements
gen_list(0, []) :- !.
gen_list(N, [N|T]) :-
    N > 0,
    N1 is N - 1,
    gen_list(N1, T).

% Tail-recursive last element
my_last([X], X) :- !.
my_last([_|T], X) :- my_last(T, X).

% === TEST QUERIES ===
% Deep recursion - would overflow without LCO
:- write('Test 1: count_down(10000)... '),
   (count_down(10000) -> write('PASS') ; write('FAIL')), nl.

:- write('Test 2: sum_to(1000)... '),
   (sum_to(1000, 0, Result), Result =:= 500500 -> write('PASS') ; write('FAIL')), nl.

:- write('Test 3: gen_list(1000) + length... '),
   (gen_list(1000, L), my_length(L, 0, Len), Len =:= 1000 -> write('PASS') ; write('FAIL')), nl.

:- write('All LCO tests completed.'), nl.
