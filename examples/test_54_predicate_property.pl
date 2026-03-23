% Test predicate_property/2 and code_type/2
% LIM-005: predicate_property/2
% LIM-006: code_type/2

test_builtin_property :-
    predicate_property(append(_,_,_), built_in).

test_defined_property :-
    assert(my_fact(hello)),
    predicate_property(my_fact(_), defined),
    retract(my_fact(hello)).

test_dynamic_property :-
    assert(dyn_pred(1)),
    predicate_property(dyn_pred(_), dynamic),
    retract(dyn_pred(1)).

% Test code_type
test_code_type_alpha :-
    code_type(97, alpha).  % 'a'

test_code_type_digit :-
    code_type(48, digit(0)).  % '0'

test_code_type_upper :-
    code_type(65, upper(97)).  % 'A' -> 'a'

test_code_type_space :-
    code_type(32, space).

test_code_type_ascii :-
    code_type(65, ascii).

test_code_type_alnum :-
    code_type(97, alnum).

test_code_type_csym :-
    code_type(95, csym).  % underscore

:- write('=== Predicate Property & Code Type Tests ==='), nl.
:- (test_builtin_property -> write('PASS: built_in property') ; write('FAIL: built_in property')), nl.
:- (test_defined_property -> write('PASS: defined property') ; write('FAIL: defined property')), nl.
:- (test_dynamic_property -> write('PASS: dynamic property') ; write('FAIL: dynamic property')), nl.
:- (test_code_type_alpha -> write('PASS: code_type alpha') ; write('FAIL: code_type alpha')), nl.
:- (test_code_type_digit -> write('PASS: code_type digit') ; write('FAIL: code_type digit')), nl.
:- (test_code_type_upper -> write('PASS: code_type upper') ; write('FAIL: code_type upper')), nl.
:- (test_code_type_space -> write('PASS: code_type space') ; write('FAIL: code_type space')), nl.
:- (test_code_type_ascii -> write('PASS: code_type ascii') ; write('FAIL: code_type ascii')), nl.
:- (test_code_type_alnum -> write('PASS: code_type alnum') ; write('FAIL: code_type alnum')), nl.
:- (test_code_type_csym -> write('PASS: code_type csym') ; write('FAIL: code_type csym')), nl.
:- write('=== Tests Complete ==='), nl.
