% ===================================================================
% TEST 36: Lazy Evaluation and Infinite Structures
% ===================================================================
% Tests: Lazy lists, infinite sequences, delayed evaluation

% Infinite list generation
lazy_list(X, [X|T]) :-
    freeze(T, (X1 is X + 1, lazy_list(X1, T))).

% Infinite sequence of natural numbers
naturals(N) :-
    lazy_list(1, N).

% Take first N elements from lazy list
take(0, _, []) :- !.
take(N, [H|T], [H|Result]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Result).

% Drop first N elements from lazy list
drop(0, List, List) :- !.
drop(N, [_|T], Result) :-
    N > 0,
    N1 is N - 1,
    drop(N1, T, Result).

% Lazy map operation
lazy_map(_, [], []).
lazy_map(Pred, [H|T], [H1|T1]) :-
    call(Pred, H, H1),
    freeze(T1, lazy_map(Pred, T, T1)).

% Lazy filter operation
lazy_filter(_, [], []).
lazy_filter(Pred, [H|T], Result) :-
    (   call(Pred, H)
    ->  Result = [H|FilteredT],
        freeze(FilteredT, lazy_filter(Pred, T, FilteredT))
    ;   freeze(Result, lazy_filter(Pred, T, Result))
    ).

% Fibonacci sequence (lazy)
lazy_fib([0,1|Rest]) :-
    freeze(Rest, fib_rest(0, 1, Rest)).

fib_rest(A, B, [C|Rest]) :-
    C is A + B,
    freeze(Rest, fib_rest(B, C, Rest)).

% Prime number sieve (lazy)
lazy_primes(Primes) :-
    lazy_list(2, Numbers),
    sieve(Numbers, Primes).

sieve([H|T], [H|Primes]) :-
    lazy_filter(not_divisible_by(H), T, Filtered),
    freeze(Primes, sieve(Filtered, Primes)).

not_divisible_by(N, X) :-
    X mod N =\= 0.

% Delayed computation with memoization
delayed_computation(Input, Result) :-
    freeze(Result, compute_when_needed(Input, Result)).

compute_when_needed(Input, Result) :-
    (   memo(Input, CachedResult)
    ->  Result = CachedResult
    ;   expensive_computation(Input, ComputedResult),
        assert(memo(Input, ComputedResult)),
        Result = ComputedResult
    ).

expensive_computation(N, Result) :-
    sleep(0.1),  % Simulate expensive operation
    Result is N * N.

% Lazy tree structures
lazy_tree(Value, Left, Right) :-
    Tree = tree(Value, Left, Right),
    freeze(Left, generate_left_subtree(Value, Left)),
    freeze(Right, generate_right_subtree(Value, Right)).

generate_left_subtree(Value, Left) :-
    LeftValue is Value * 2,
    lazy_tree(LeftValue, LeftLeft, LeftRight),
    Left = tree(LeftValue, LeftLeft, LeftRight).

generate_right_subtree(Value, Right) :-
    RightValue is Value * 2 + 1,
    lazy_tree(RightValue, RightLeft, RightRight),
    Right = tree(RightValue, RightLeft, RightRight).

% Lazy evaluation with coroutining
lazy_producer_consumer :-
    lazy_producer(Stream),
    lazy_consumer(Stream).

lazy_producer([Item|Rest]) :-
    produce_item(Item),
    freeze(Rest, lazy_producer(Rest)).

produce_item(Item) :-
    get_time(Time),
    Item is round(Time) mod 100.

lazy_consumer([]).
lazy_consumer([Item|Rest]) :-
    consume_item(Item),
    lazy_consumer(Rest).

consume_item(Item) :-
    format('Consumed: ~w~n', [Item]).

% Delayed goal execution
delay_until(Condition, Goal) :-
    freeze(Condition, (call(Condition) -> call(Goal) ; true)).

% Lazy string operations
lazy_string_chars(String, Chars) :-
    atom_chars(String, CharList),
    make_lazy_list(CharList, Chars).

make_lazy_list([], []).
make_lazy_list([H|T], [H|LazyT]) :-
    freeze(LazyT, make_lazy_list(T, LazyT)).

% Test lazy evaluation
test_lazy_evaluation :-
    naturals(N),
    take(10, N, First10),
    format('First 10 naturals: ~w~n', [First10]).

test_lazy_fibonacci :-
    lazy_fib(Fib),
    take(15, Fib, First15),
    format('First 15 Fibonacci: ~w~n', [First15]).

test_lazy_primes :-
    lazy_primes(Primes),
    take(10, Primes, First10Primes),
    format('First 10 primes: ~w~n', [First10Primes]).

test_lazy_map :-
    naturals(N),
    lazy_map(square, N, Squares),
    take(5, Squares, First5Squares),
    format('First 5 squares: ~w~n', [First5Squares]).

square(X, Y) :- Y is X * X.

test_lazy_filter :-
    naturals(N),
    lazy_filter(even, N, Evens),
    take(5, Evens, First5Evens),
    format('First 5 evens: ~w~n', [First5Evens]).

even(X) :- X mod 2 =:= 0.

% Test queries:
% ?- test_lazy_evaluation.
% ?- test_lazy_fibonacci.
% ?- test_lazy_primes.
% ?- test_lazy_map.
% ?- test_lazy_filter.
% ?- delayed_computation(10, R).
% ?- lazy_tree(1, L, R), L = tree(LV, _, _), R = tree(RV, _, _).