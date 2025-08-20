% Esempio di programma Prolog per testare l'IDE
% Fatti sui genitori
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Fatti sui sessi
male(tom).
male(bob).
male(jim).
female(liz).
female(ann).
female(pat).

% Regole
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \== Y.

% Fatti sui colori
color(red).
color(blue).
color(green).

% Fatti sulle forme
shape(circle).
shape(square).
shape(triangle).

% Numeri per test aritmetici
number_fact(1).
number_fact(2).
number_fact(3).
number_fact(4).
number_fact(5).

% Regola per numeri pari
even(X) :- number_fact(X), 0 =:= X mod 2.

% Liste di esempio
list_member(X, [X|_]).
list_member(X, [_|T]) :- list_member(X, T).

% Regola condizionale
classify_number(X, positive) :- X > 0.
classify_number(X, negative) :- X < 0.
classify_number(0, zero).