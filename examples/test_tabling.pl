% Test tabling (memoization) support
% ISS-2025-0092

% Test 1: Fibonacci with tabling - should be dramatically faster
:- table fib/2.

fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

% Test 2: Path finding with tabling - handles cycles
:- table path/2.

edge(a, b).
edge(b, c).
edge(c, d).
edge(b, d).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

% Test 3: Basic tabling via built-in predicate
:- table ancestor/2.

parent(tom, bob).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Queries:
% ?- fib(10, F).
% Expected: F = 55

% ?- fib(15, F).
% Expected: F = 610

% ?- path(a, X).
% Expected: X = b ; X = c ; X = d

% ?- ancestor(tom, X).
% Expected: X = bob ; X = ann ; X = pat ; X = jim
