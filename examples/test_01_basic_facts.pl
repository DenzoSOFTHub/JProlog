% ===================================================================
% TEST 01: Basic Facts and Simple Queries
% ===================================================================
% Tests: Basic fact storage, simple queries, unification

% Clear any existing facts first to avoid conflicts
% Note: In actual testing, use :clear command before loading

% Facts about family relationships (using test_ prefix to avoid conflicts)
test_parent(tom, bob).
test_parent(tom, liz).
test_parent(bob, ann).
test_parent(bob, pat).
test_parent(pat, jim).

% Facts about gender
test_male(tom).
test_male(bob).
test_male(jim).
test_female(liz).
test_female(ann).
test_female(pat).

% Simple rules
test_father(X, Y) :- test_parent(X, Y), test_male(X).
test_mother(X, Y) :- test_parent(X, Y), test_female(X).

% Test queries to run:
% ?- test_parent(tom, bob).     % Should be true
% ?- test_parent(bob, X).       % Should find ann and pat  
% ?- test_father(tom, liz).     % Should be false (liz is female)
% ?- test_father(tom, bob).     % Should be true
% ?- test_mother(X, ann).       % Should find no solutions (ann has father bob only)