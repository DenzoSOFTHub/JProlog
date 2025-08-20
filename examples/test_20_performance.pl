% ===================================================================
% TEST 20: Performance and Stress Tests
% ===================================================================
% Tests: Large data processing, recursion limits, performance edge cases

% Large list processing
generate_large_list(0, []) :- !.
generate_large_list(N, [N|Rest]) :-
    N > 0,
    N1 is N - 1,
    generate_large_list(N1, Rest).

sum_large_list(List, Sum) :-
    sum_large_list(List, 0, Sum).

sum_large_list([], Acc, Acc).
sum_large_list([H|T], Acc, Sum) :-
    NewAcc is Acc + H,
    sum_large_list(T, NewAcc, Sum).

% Test tail recursion optimization
tail_recursive_count(0, Acc, Acc) :- !.
tail_recursive_count(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    NewAcc is Acc + 1,
    tail_recursive_count(N1, NewAcc, Result).

non_tail_recursive_count(0, 0) :- !.
non_tail_recursive_count(N, Result) :-
    N > 0,
    N1 is N - 1,
    non_tail_recursive_count(N1, SubResult),
    Result is SubResult + 1.

% Deep recursion test
deep_recursion(0, base_case) :- !.
deep_recursion(N, recursive_case(Sub)) :-
    N > 0,
    N1 is N - 1,
    deep_recursion(N1, Sub).

% Memory stress test with large terms
create_large_term(0, base) :- !.
create_large_term(N, compound(Sub)) :-
    N > 0,
    N1 is N - 1,
    create_large_term(N1, Sub).

% Backtracking stress test
generate_choices(1).
generate_choices(2).
generate_choices(3).
generate_choices(4).
generate_choices(5).

stress_backtrack(N, Choices) :-
    findall(Choice, (between(1, N, _), generate_choices(Choice)), AllChoices),
    length(AllChoices, Choices).

% Unification stress test
complex_unification_test(N) :-
    create_large_term(N, Term1),
    create_large_term(N, Term2),
    (Term1 = Term2 -> 
        writeln('Unification succeeded')
    ;   writeln('Unification failed')
    ).

% Mathematical computation stress
prime_check(2) :- !.
prime_check(N) :-
    N > 2,
    N mod 2 =\= 0,
    Max is floor(sqrt(N)),
    \+ has_divisor(N, 3, Max).

has_divisor(N, D, Max) :-
    D =< Max,
    (N mod D =:= 0 ->
        true
    ;   D2 is D + 2,
        has_divisor(N, D2, Max)
    ).

find_primes_up_to(Max, Primes) :-
    findall(P, (between(2, Max, P), prime_check(P)), Primes).

% List processing performance
reverse_large_list(List, Reversed) :-
    reverse_acc(List, [], Reversed).

reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, Reversed) :-
    reverse_acc(T, [H|Acc], Reversed).

% Sorting performance test
insertion_sort([], []).
insertion_sort([H|T], Sorted) :-
    insertion_sort(T, SortedT),
    insert_sorted(H, SortedT, Sorted).

insert_sorted(X, [], [X]).
insert_sorted(X, [H|T], [X,H|T]) :-
    X @=< H, !.
insert_sorted(X, [H|T], [H|Sorted]) :-
    insert_sorted(X, T, Sorted).

% Performance measurement helpers
time_operation(Goal, Time) :-
    get_time(Start),
    call(Goal),
    get_time(End),
    Time is End - Start.

% Compound performance test
performance_suite(Size) :-
    writeln(starting_performance_tests(Size)),
    
    % Test 1: List generation and summation
    time_operation(
        (generate_large_list(Size, List), sum_large_list(List, _)),
        Time1
    ),
    writeln(list_generation_and_sum(Time1)),
    
    % Test 2: Tail vs non-tail recursion
    SmallSize is min(Size, 1000),  % Limit for non-tail recursion
    time_operation(tail_recursive_count(SmallSize, 0, _), Time2),
    writeln(tail_recursion(Time2)),
    
    % Test 3: Prime finding
    PrimeLimit is min(Size, 100),
    time_operation(find_primes_up_to(PrimeLimit, _), Time3),
    writeln(prime_finding(Time3)),
    
    writeln(performance_tests_completed).

% Memory usage test (approximation)
memory_stress_test(N) :-
    writeln(creating_large_structures(N)),
    findall(Term, (between(1, N, I), create_large_term(I, Term)), Terms),
    length(Terms, Count),
    writeln(created_terms(Count)).

% Test queries:
% ?- generate_large_list(1000, L), length(L, Len).
% ?- tail_recursive_count(10000, 0, Result).
% ?- deep_recursion(100, Result).      % Test recursion depth
% ?- stress_backtrack(10, Choices).
% ?- find_primes_up_to(50, Primes).
% ?- performance_suite(1000).
% ?- memory_stress_test(100).