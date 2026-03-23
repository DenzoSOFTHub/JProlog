% Test global variables
test_nb_setval_getval :-
    nb_setval(counter, 42),
    nb_getval(counter, X),
    X =:= 42.

test_nb_overwrite :-
    nb_setval(myvar, hello),
    nb_setval(myvar, world),
    nb_getval(myvar, X),
    X == world.

test_nb_delete :-
    nb_setval(temp, value),
    nb_delete(temp),
    \+ nb_getval(temp, _).

test_b_setval_getval :-
    b_setval(bvar, 99),
    b_getval(bvar, X),
    X =:= 99.

test_nb_current :-
    nb_setval(test_var_a, alpha),
    nb_setval(test_var_b, beta),
    nb_current(test_var_a, V),
    V == alpha.

test_nb_compound_value :-
    nb_setval(data, point(3, 4)),
    nb_getval(data, X),
    X = point(3, 4).

:- write('=== Global Variables Tests ==='), nl.
:- (test_nb_setval_getval -> write('PASS: nb_setval/nb_getval') ; write('FAIL: nb_setval/nb_getval')), nl.
:- (test_nb_overwrite -> write('PASS: nb overwrite') ; write('FAIL: nb overwrite')), nl.
:- (test_nb_delete -> write('PASS: nb_delete') ; write('FAIL: nb_delete')), nl.
:- (test_b_setval_getval -> write('PASS: b_setval/b_getval') ; write('FAIL: b_setval/b_getval')), nl.
:- (test_nb_current -> write('PASS: nb_current') ; write('FAIL: nb_current')), nl.
:- (test_nb_compound_value -> write('PASS: nb compound value') ; write('FAIL: nb compound value')), nl.
:- write('=== Tests Complete ==='), nl.
