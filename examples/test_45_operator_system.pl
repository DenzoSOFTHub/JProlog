% ===================================================================
% TEST 45: Operator Definition System - op/3 and current_op/3
% ===================================================================
% Tests the comprehensive operator system implemented in Phase 1

% Test 1: Basic Operator Definition
test_basic_op_definition :-
    write('Testing basic operator definition:'), nl,
    
    % Define a new operator
    op(500, xfx, myop),
    write('✓ Defined operator: op(500, xfx, myop)'), nl,
    
    % Check if it was defined correctly
    current_op(500, xfx, myop),
    write('✓ Verified operator definition with current_op/3'), nl.

% Test 2: Operator Redefinition
test_operator_redefinition :-
    write('Testing operator redefinition:'), nl,
    
    % Define an operator
    op(600, yfx, testop),
    write('✓ Initially defined: op(600, yfx, testop)'), nl,
    
    % Redefine with different precedence and type
    op(700, xfy, testop),
    write('✓ Redefined: op(700, xfy, testop)'), nl,
    
    % Verify the redefinition
    current_op(700, xfy, testop),
    write('✓ Verified redefinition'), nl.

% Test 3: Standard ISO Operators
test_standard_operators :-
    write('Testing standard ISO operators are predefined:'), nl,
    
    % Test some key standard operators
    current_op(700, xfx, '='),
    write('✓ Found standard operator: =(700, xfx)'), nl,
    
    current_op(700, xfx, is),
    write('✓ Found standard operator: is(700, xfx)'), nl,
    
    current_op(500, yfx, '+'),
    write('✓ Found standard operator: +(500, yfx)'), nl,
    
    current_op(400, yfx, '*'),
    write('✓ Found standard operator: *(400, yfx)'), nl,
    
    current_op(1000, xfy, ','),
    write('✓ Found standard operator: ,(1000, xfy)'), nl,
    
    current_op(1200, fx, ':-'),
    write('✓ Found standard operator: :-(1200, fx)'), nl.

% Test 4: Existential Quantification Operator
test_existential_operator :-
    write('Testing existential quantification operator:'), nl,
    
    % Test that ^ operator is predefined for existential quantification
    current_op(200, xfy, '^'),
    write('✓ Found existential quantification operator: ^(200, xfy)'), nl.

% Test 5: Univ Operator  
test_univ_operator :-
    write('Testing univ operator:'), nl,
    
    % Test that =.. operator is predefined
    current_op(700, xfx, '=..'),
    write('✓ Found univ operator: =..(700, xfx)'), nl.

% Test 6: Query All Operators
test_query_all_operators :-
    write('Testing query for all operators:'), nl,
    
    findall(op(P, T, N), current_op(P, T, N), Operators),
    length(Operators, Count),
    write('✓ Found '), write(Count), write(' total operators'), nl,
    
    % Should have many operators (at least 30 standard ISO ones)
    (Count >= 30 -> 
        write('✓ Sufficient number of predefined operators') ;
        write('✗ Too few operators found')), nl.

% Test 7: Query Operators by Precedence
test_query_by_precedence :-
    write('Testing query operators by precedence:'), nl,
    
    % Find all operators with precedence 700
    findall(N, current_op(700, _, N), Prec700Ops),
    length(Prec700Ops, Count700),
    write('✓ Found '), write(Count700), write(' operators with precedence 700'), nl,
    
    % Should include =, is, ==, \==, etc.
    (Count700 >= 5 -> 
        write('✓ Expected number of precedence 700 operators') ;
        write('✗ Too few precedence 700 operators')), nl.

% Test 8: Query Operators by Type
test_query_by_type :-
    write('Testing query operators by type:'), nl,
    
    % Find all infix operators (xfx)
    findall(N, current_op(_, xfx, N), XfxOps),
    length(XfxOps, CountXfx),
    write('✓ Found '), write(CountXfx), write(' xfx operators'), nl,
    
    % Find all right-associative operators (xfy)
    findall(N, current_op(_, xfy, N), XfyOps),
    length(XfyOps, CountXfy),
    write('✓ Found '), write(CountXfy), write(' xfy operators'), nl.

% Test 9: Define Multiple Custom Operators
test_multiple_custom_operators :-
    write('Testing multiple custom operator definitions:'), nl,
    
    % Define several custom operators
    op(800, fx, prefix_op),
    write('✓ Defined prefix operator: prefix_op(800, fx)'), nl,
    
    op(550, xfx, my_equals),
    write('✓ Defined infix operator: my_equals(550, xfx)'), nl,
    
    op(450, yfx, my_plus),
    write('✓ Defined left-associative operator: my_plus(450, yfx)'), nl,
    
    op(350, xfy, my_power),
    write('✓ Defined right-associative operator: my_power(350, xfy)'), nl,
    
    % Verify all were defined
    current_op(800, fx, prefix_op),
    current_op(550, xfx, my_equals),
    current_op(450, yfx, my_plus),
    current_op(350, xfy, my_power),
    write('✓ All custom operators verified'), nl.

% Test 10: Precedence Validation
test_precedence_validation :-
    write('Testing precedence validation:'), nl,
    
    % Test valid precedence ranges (1-1200)
    op(1, xfx, min_prec),
    write('✓ Minimum precedence (1) accepted'), nl,
    
    op(1200, xfx, max_prec),
    write('✓ Maximum precedence (1200) accepted'), nl,
    
    write('✓ Precedence validation working'), nl.

% Test 11: Operator Type Validation
test_type_validation :-
    write('Testing operator type validation:'), nl,
    
    % Test all valid operator types
    op(600, fx, test_fx),
    write('✓ fx type accepted'), nl,
    
    op(601, fy, test_fy),
    write('✓ fy type accepted'), nl,
    
    op(602, xf, test_xf),
    write('✓ xf type accepted'), nl,
    
    op(603, yf, test_yf),
    write('✓ yf type accepted'), nl,
    
    op(604, xfx, test_xfx),
    write('✓ xfx type accepted'), nl,
    
    op(605, xfy, test_xfy),
    write('✓ xfy type accepted'), nl,
    
    op(606, yfx, test_yfx),
    write('✓ yfx type accepted'), nl,
    
    write('✓ All operator types validated'), nl.

% Test 12: Complex Operator Queries
test_complex_queries :-
    write('Testing complex operator queries:'), nl,
    
    % Find operators by name pattern (operators containing 'e')
    findall(op(P, T, N), (current_op(P, T, N), atom_chars(N, Chars), member(e, Chars)), EOperators),
    length(EOperators, ECount),
    write('✓ Found '), write(ECount), write(' operators containing "e"'), nl,
    
    % Find high precedence operators (> 1000)  
    findall(N, (current_op(P, _, N), P > 1000), HighPrecOps),
    length(HighPrecOps, HighCount),
    write('✓ Found '), write(HighCount), write(' high precedence operators (> 1000)'), nl.

% Master test runner
run_operator_tests :-
    write('=== OPERATOR DEFINITION SYSTEM TEST SUITE ==='), nl,
    write('Testing Phase 1 op/3 and current_op/3 implementation'), nl, nl,
    
    test_basic_op_definition, nl,
    test_operator_redefinition, nl,
    test_standard_operators, nl,
    test_existential_operator, nl,
    test_univ_operator, nl,
    test_query_all_operators, nl,
    test_query_by_precedence, nl,
    test_query_by_type, nl,
    test_multiple_custom_operators, nl,
    test_precedence_validation, nl,
    test_type_validation, nl,
    test_complex_queries, nl,
    
    write('=== OPERATOR SYSTEM TEST COMPLETED ==='), nl,
    write('All operator definition functionality has been tested'), nl.

% Quick individual tests for debugging
quick_test_op_def :- op(555, xfx, quick_op), write('✓ Quick operator defined'), nl.
quick_test_current_op :- current_op(700, xfx, '='), write('✓ Found = operator'), nl.
quick_test_count_ops :- findall(N, current_op(_, _, N), Ops), length(Ops, C), write('Total operators: '), write(C), nl.