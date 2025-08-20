% ===================================================================
% TEST 01: Basic Facts and Simple Queries
% ===================================================================
% Tests: Basic fact storage, simple queries, unification

% Facts about family relationships
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Facts about gender
male(tom).
male(bob).
male(jim).
female(liz).
female(ann).
female(pat).

% Simple rule
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).

% Test queries to run:
% ?- parent(tom, bob).          % Should be true
% ?- parent(bob, X).            % Should find ann and pat  
% ?- father(tom, liz).          % Should be false
% ?- father(tom, bob).          % Should be true
% ?- mother(X, ann).            % Should find no solutions (ann has father bob)