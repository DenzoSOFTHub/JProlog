parent(tom, bob).
male(tom).
father(X, Y) :- parent(X, Y), male(X).