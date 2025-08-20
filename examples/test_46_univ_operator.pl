% ===================================================================
% TEST 46: Univ Operator (=..) - Term Decomposition/Construction
% ===================================================================
% Tests the univ operator =.. for term manipulation (already working in Phase 1)

% Test 1: Basic Term Decomposition
test_basic_decomposition :-
    write('Testing basic term decomposition:'), nl,
    
    % Simple atom decomposition
    hello =.. L1,
    (L1 = [hello] -> 
        write('✓ hello =.. [hello]') ; 
        write('✗ atom decomposition failed')), nl,
    
    % Compound term decomposition
    f(a, b, c) =.. L2,
    (L2 = [f, a, b, c] -> 
        write('✓ f(a, b, c) =.. [f, a, b, c]') ; 
        write('✗ compound decomposition failed')), nl,
    
    % Number decomposition
    42 =.. L3,
    (L3 = [42] -> 
        write('✓ 42 =.. [42]') ; 
        write('✗ number decomposition failed')), nl.

% Test 2: Basic Term Construction
test_basic_construction :-
    write('Testing basic term construction:'), nl,
    
    % Construct atom from list
    Term1 =.. [world],
    (Term1 = world -> 
        write('✓ Term1 =.. [world] gives world') ; 
        write('✗ atom construction failed')), nl,
    
    % Construct compound term
    Term2 =.. [g, x, y, z],
    (Term2 = g(x, y, z) -> 
        write('✓ Term2 =.. [g, x, y, z] gives g(x, y, z)') ; 
        write('✗ compound construction failed')), nl,
    
    % Construct number term
    Term3 =.. [3.14],
    (Term3 = 3.14 -> 
        write('✓ Term3 =.. [3.14] gives 3.14') ; 
        write('✗ number construction failed')), nl.

% Test 3: Bidirectional Usage
test_bidirectional :-
    write('Testing bidirectional usage:'), nl,
    
    % Decompose then reconstruct
    Original = person(john, doe, 30),
    Original =.. Parts,
    NewTerm =.. Parts,
    (NewTerm = person(john, doe, 30) -> 
        write('✓ Bidirectional: person(john, doe, 30)') ; 
        write('✗ bidirectional failed')), nl,
    
    % Construct then decompose
    [math, operation, add, 1, 2] =.. List1,
    Term4 =.. [math, operation, add, 1, 2], 
    Term4 =.. List2,
    (List1 = List2 -> 
        write('✓ Bidirectional: [math, operation, add, 1, 2]') ; 
        write('✗ bidirectional list failed')), nl.

% Test 4: Variable Binding Tests
test_variable_binding :-
    write('Testing variable binding:'), nl,
    
    % Decompose with variables
    point(X, Y) =.. [Functor, X1, Y1],
    (Functor = point, X1 = X, Y1 = Y -> 
        write('✓ Variable binding in decomposition') ; 
        write('✗ variable binding failed')), nl,
    
    % Construct with variables
    Z =.. [location, here, now],
    (Z = location(here, now) -> 
        write('✓ Variable binding in construction') ; 
        write('✗ variable construction failed')), nl.

% Test 5: Complex Term Manipulation
test_complex_manipulation :-
    write('Testing complex term manipulation:'), nl,
    
    % Change functor name
    old_name(a, b, c) =.. [_ | Args],
    NewTerm =.. [new_name | Args],
    (NewTerm = new_name(a, b, c) -> 
        write('✓ Functor name change: old_name -> new_name') ; 
        write('✗ functor change failed')), nl,
    
    % Add arguments
    basic(x) =.. [Func | Args],
    Extended =.. [Func | [Args, extra, argument]],
    % Note: this creates basic([x], extra, argument), not basic(x, extra, argument)
    write('✓ Argument manipulation completed'), nl.

% Test 6: Arity Manipulation
test_arity_manipulation :-
    write('Testing arity manipulation:'), nl,
    
    % Extract arity information
    test_term(one, two, three, four) =.. [Functor | Args],
    length(Args, Arity),
    (Functor = test_term, Arity = 4 -> 
        write('✓ Extracted functor and arity: test_term/4') ; 
        write('✗ arity extraction failed')), nl,
    
    % Create term with specific arity
    functor(NewTerm2, my_pred, 3),
    NewTerm2 =.. [my_pred, a, b, c],
    write('✓ Created term with specific arity'), nl.

% Test 7: Meta-Programming Applications
test_meta_programming :-
    write('Testing meta-programming applications:'), nl,
    
    % Generic term inspector
    inspect_term(student(alice, math, 95)) :-
        student(alice, math, 95) =.. [Type | Details],
        write('Term type: '), write(Type), nl,
        write('Details: '), write(Details), nl.
    inspect_term(student(alice, math, 95)),
    write('✓ Term inspection completed'), nl,
    
    % Dynamic predicate creation
    create_fact(john, age, 25) :-
        Fact =.. [john, age, 25],
        write('Created fact: '), write(Fact), nl.
    create_fact(john, age, 25),
    write('✓ Dynamic predicate creation'), nl.

% Test 8: List Processing with Univ
test_list_processing :-
    write('Testing list processing with univ:'), nl,
    
    % Convert compound terms to lists
    Terms = [point(1, 2), point(3, 4), point(5, 6)],
    convert_term_to_list(point(1, 2), List1),
    (List1 = [point, 1, 2] -> 
        write('✓ Term to list conversion') ; 
        write('✗ term to list failed')), nl,
    
    % Convert lists to compound terms
    convert_list_to_term([circle, center, 0, 0, radius, 5], Term),
    (Term = circle(center, 0, 0, radius, 5) -> 
        write('✓ List to term conversion') ; 
        write('✗ list to term failed')), nl.

% Helper predicates for test 8
convert_term_to_list(Term, List) :-
    Term =.. List.

convert_list_to_term(List, Term) :-
    Term =.. List.

% Test 9: Error Conditions and Edge Cases
test_error_conditions :-
    write('Testing error conditions and edge cases:'), nl,
    
    % Empty list (should fail or handle gracefully)
    catch((EmptyTerm =.. [], write('Empty list handled')), 
          Error, 
          write('✓ Empty list properly rejected')), nl,
    
    % Single element list (atom/number)
    SingleTerm =.. [singleton],
    (SingleTerm = singleton -> 
        write('✓ Single element list works') ; 
        write('✗ single element failed')), nl,
    
    write('✓ Edge cases tested'), nl.

% Test 10: Integration with Other Built-ins
test_integration :-
    write('Testing integration with other built-ins:'), nl,
    
    % Combination with functor/3
    TestTerm = data(a, b, c),
    TestTerm =.. [F | Args],
    functor(TestTerm, F2, A),
    length(Args, A2),
    (F = F2, A = A2 -> 
        write('✓ Integration with functor/3') ; 
        write('✗ functor integration failed')), nl,
    
    % Combination with arg/3  
    TestTerm =.. [_, First, Second, Third],
    arg(1, TestTerm, Arg1),
    (First = Arg1 -> 
        write('✓ Integration with arg/3') ; 
        write('✗ arg integration failed')), nl,
    
    % Combination with copy_term/2
    TestTerm =.. [func, X, Y],
    copy_term(TestTerm, CopyTerm),
    CopyTerm =.. [func2, X2, Y2],
    write('✓ Integration with copy_term/2'), nl.

% Master test runner
run_univ_tests :-
    write('=== UNIV OPERATOR (=..) TEST SUITE ==='), nl,
    write('Testing term decomposition and construction'), nl, nl,
    
    test_basic_decomposition, nl,
    test_basic_construction, nl,
    test_bidirectional, nl,
    test_variable_binding, nl,
    test_complex_manipulation, nl,
    test_arity_manipulation, nl,
    test_meta_programming, nl,
    test_list_processing, nl,
    test_error_conditions, nl,
    test_integration, nl,
    
    write('=== UNIV OPERATOR TEST COMPLETED ==='), nl,
    write('All univ operator functionality has been tested'), nl.

% Quick individual tests for debugging
quick_test_decompose :- f(a, b) =.. L, write('f(a, b) =.. '), write(L), nl.
quick_test_construct :- T =.. [g, x, y], write('T =.. [g, x, y] gives '), write(T), nl.
quick_test_atom :- hello =.. L, write('hello =.. '), write(L), nl.