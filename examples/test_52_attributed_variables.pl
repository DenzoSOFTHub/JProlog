% Test attributed variables and coroutining
% Tests for LIM-002 (Attributed Variables) and LIM-001 (Coroutining)

test_put_get_attr :-
    put_attr(X, mymod, hello),
    get_attr(X, mymod, V),
    V == hello.

test_attvar :-
    put_attr(X, mymod, val),
    attvar(X).

test_not_attvar :-
    \+ attvar(foo),
    \+ attvar(_X).

% Test freeze
test_freeze_basic :-
    freeze(X, Y = hello),
    X = anything,
    Y == hello.

% Test dif
test_dif_success :-
    dif(a, b).

test_dif_delayed :-
    dif(X, Y),
    X = a,
    Y = b.

test_dif_fail :-
    \+ (dif(X, Y), X = a, Y = a).

:- write('=== Attributed Variables & Coroutining Tests ==='), nl.
:- (test_put_get_attr -> write('PASS: put_attr/get_attr') ; write('FAIL: put_attr/get_attr')), nl.
:- (test_attvar -> write('PASS: attvar') ; write('FAIL: attvar')), nl.
:- (test_not_attvar -> write('PASS: not attvar') ; write('FAIL: not attvar')), nl.
:- (test_freeze_basic -> write('PASS: freeze basic') ; write('FAIL: freeze basic')), nl.
:- (test_dif_success -> write('PASS: dif success') ; write('FAIL: dif success')), nl.
:- (test_dif_delayed -> write('PASS: dif delayed') ; write('FAIL: dif delayed')), nl.
:- (test_dif_fail -> write('PASS: dif fail') ; write('FAIL: dif fail')), nl.
:- write('=== Tests Complete ==='), nl.
