% File di esempio: famiglia.pl
% Fatti sulla famiglia

% Genitori
father(antonio, marco).
father(antonio, lucia).
father(marco, giulia).
mother(maria, marco).
mother(maria, lucia).
mother(anna, giulia).

% Età
age(antonio, 65).
age(maria, 62).
age(marco, 35).
age(anna, 33).
age(lucia, 30).
age(giulia, 8).

% Regole
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \== Y.

% Predicati derivati
child(X, Y) :- parent(Y, X).
adult(X) :- age(X, A), A >= 18.
young(X) :- age(X, A), A < 18.