% Main file for testing DCG compilation

% Simple facts for testing
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).

% Simple rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% Test predicate
test_main :- 
    write('Main file loaded successfully'), nl.