% ===================================================================
% TEST 47: Existential Quantification (^) in Meta-Predicates
% ===================================================================
% Tests the existential quantification operator ^ in meta-programming contexts

% Test data for existential quantification tests
student(alice, math, 95).
student(alice, physics, 88).
student(alice, chemistry, 92).
student(bob, math, 78).
student(bob, physics, 82).
student(charlie, math, 91).
student(charlie, chemistry, 89).

employee(john, engineering, 50000).
employee(mary, engineering, 55000).
employee(bob, marketing, 45000).
employee(alice, engineering, 52000).

likes(mary, wine).
likes(mary, food).
likes(john, wine).
likes(bob, beer).
likes(alice, tea).

% Test 1: Basic Existential Quantification with bagof/3
test_basic_existential :-
    write('Testing basic existential quantification:'), nl,
    
    % Without existential quantification - gets separate results for each student
    bagof(Grade, student(Student, math, Grade), MathGrades1),
    write('Without ^: bagof(Grade, student(Student, math, Grade), Grades)'), nl,
    write('Result: '), write(MathGrades1), nl,
    
    % With existential quantification - collects all grades regardless of student
    bagof(Grade, Student^student(Student, math, Grade), MathGrades2),
    write('With ^: bagof(Grade, Student^student(Student, math, Grade), Grades)'), nl,
    write('Result: '), write(MathGrades2), nl,
    
    write('✓ Basic existential quantification tested'), nl.

% Test 2: Multiple Variable Quantification
test_multiple_quantification :-
    write('Testing multiple variable quantification:'), nl,
    
    % Quantify over both student and subject
    bagof(Grade, (Student^Subject)^student(Student, Subject, Grade), AllGrades),
    write('All grades (any student, any subject): '), write(AllGrades), nl,
    
    write('✓ Multiple variable quantification tested'), nl.

% Test 3: Partial Quantification
test_partial_quantification :-
    write('Testing partial quantification:'), nl,
    
    % Quantify over student but not subject - group by subject
    bagof(Grade, Student^student(Student, Subject, Grade), GradesBySubject),
    write('Grades by subject (quantified over student): '), write(GradesBySubject), nl,
    
    write('✓ Partial quantification tested'), nl.

% Test 4: Existential Quantification with setof/3
test_setof_existential :-
    write('Testing existential quantification with setof/3:'), nl,
    
    % Without existential quantification
    setof(Grade, student(Student, math, Grade), MathGradesSet1),
    write('Without ^: setof grades in math = '), write(MathGradesSet1), nl,
    
    % With existential quantification
    setof(Grade, Student^student(Student, math, Grade), MathGradesSet2),
    write('With ^: setof grades in math = '), write(MathGradesSet2), nl,
    
    write('✓ setof with existential quantification tested'), nl.

% Test 5: Complex Goal with Existential Quantification
test_complex_goals :-
    write('Testing complex goals with existential quantification:'), nl,
    
    % Find all salaries in engineering, regardless of employee name
    bagof(Salary, Person^employee(Person, engineering, Salary), EngineeringSalaries),
    write('Engineering salaries: '), write(EngineeringSalaries), nl,
    
    % Find all departments, regardless of person or salary
    setof(Dept, (Person^Salary)^employee(Person, Dept, Salary), AllDepartments),
    write('All departments: '), write(AllDepartments), nl,
    
    write('✓ Complex goals with quantification tested'), nl.

% Test 6: Nested Quantification
test_nested_quantification :-
    write('Testing nested quantification patterns:'), nl,
    
    % Double quantification: find items liked by anyone
    bagof(Item, Person^likes(Person, Item), LikedItems),
    write('Items liked by anyone: '), write(LikedItems), nl,
    
    % Group by person, quantify over item
    bagof(Person-Item, Item^likes(Person, Item), PersonItemPairs),
    write('Person-item pairs (quantified over item): '), write(PersonItemPairs), nl,
    
    write('✓ Nested quantification tested'), nl.

% Test 7: Quantification with Arithmetic Goals
test_arithmetic_quantification :-
    write('Testing quantification with arithmetic goals:'), nl,
    
    % Find all grades above 90, regardless of student or subject
    bagof(Grade, (Student^Subject)^(student(Student, Subject, Grade), Grade > 90), HighGrades),
    write('All grades > 90: '), write(HighGrades), nl,
    
    % Find students with any grade above 90
    setof(Student, (Subject^Grade)^(student(Student, Subject, Grade), Grade > 90), TopStudents),
    write('Students with grades > 90: '), write(TopStudents), nl,
    
    write('✓ Arithmetic with quantification tested'), nl.

% Test 8: Quantification in Compound Goals
test_compound_goals :-
    write('Testing quantification in compound goals:'), nl,
    
    % Find all math students who also study physics
    bagof(Student, (MathGrade^PhysicsGrade)^(student(Student, math, MathGrade), 
                                             student(Student, physics, PhysicsGrade)), 
          MathPhysicsStudents),
    write('Students taking both math and physics: '), write(MathPhysicsStudents), nl,
    
    write('✓ Compound goals with quantification tested'), nl.

% Test 9: Operator Precedence with ^
test_operator_precedence :-
    write('Testing operator precedence with ^:'), nl,
    
    % Test that ^ has correct precedence (200, higher than most operators)
    current_op(Prec, Type, '^'),
    write('^ operator precedence: '), write(Prec), write(', type: '), write(Type), nl,
    (Prec =:= 200 -> 
        write('✓ Correct precedence for ^ operator') ; 
        write('✗ Incorrect precedence')), nl,
    
    write('✓ Operator precedence tested'), nl.

% Test 10: Error Conditions and Edge Cases  
test_error_conditions :-
    write('Testing error conditions and edge cases:'), nl,
    
    % Test quantification over non-existent variable
    catch(bagof(X, Y^nonexistent(X, Y), _), 
          Error, 
          write('✓ Handled quantification over non-existent predicate')), nl,
    
    % Test multiple quantification patterns
    catch(bagof(Result, (A^B^C)^complex_goal(A, B, C, Result), _),
          Error,
          write('✓ Handled complex quantification')), nl,
    
    write('✓ Error conditions tested'), nl.

% Test 11: Integration with findall/3
test_findall_integration :-
    write('Testing integration with findall/3:'), nl,
    
    % findall doesn't typically use existential quantification
    % but should handle ^ in the goal if present
    findall(Grade, Student^student(Student, math, Grade), AllMathGrades),
    write('findall with ^ (though not typically needed): '), write(AllMathGrades), nl,
    
    % Compare with regular findall
    findall(Grade, student(_, math, Grade), RegularMathGrades),
    write('Regular findall: '), write(RegularMathGrades), nl,
    
    write('✓ findall integration tested'), nl.

% Test 12: Practical Examples
test_practical_examples :-
    write('Testing practical examples:'), nl,
    
    % Example 1: Collect all unique subjects taught
    setof(Subject, (Student^Grade)^student(Student, Subject, Grade), AllSubjects),
    write('All subjects taught: '), write(AllSubjects), nl,
    
    % Example 2: Get average salary by department (conceptual)
    findall(Dept-Salary, employee(_, Dept, Salary), DeptSalaryPairs),
    write('Department-salary pairs: '), write(DeptSalaryPairs), nl,
    
    % Example 3: Find who likes what, grouped by item
    bagof(Person, likes(Person, Item), PeopleWhoLike),
    write('People who like specific items: '), write(PeopleWhoLike), nl,
    
    write('✓ Practical examples completed'), nl.

% Master test runner
run_existential_tests :-
    write('=== EXISTENTIAL QUANTIFICATION (^) TEST SUITE ==='), nl,
    write('Testing ^ operator in meta-predicates'), nl, nl,
    
    test_basic_existential, nl,
    test_multiple_quantification, nl,
    test_partial_quantification, nl,
    test_setof_existential, nl,
    test_complex_goals, nl,
    test_nested_quantification, nl,
    test_arithmetic_quantification, nl,
    test_compound_goals, nl,
    test_operator_precedence, nl,
    test_error_conditions, nl,
    test_findall_integration, nl,
    test_practical_examples, nl,
    
    write('=== EXISTENTIAL QUANTIFICATION TEST COMPLETED ==='), nl,
    write('All ^ operator functionality has been tested'), nl.

% Quick individual tests for debugging
quick_test_bagof :- bagof(Grade, Student^student(Student, math, Grade), Grades), write('Math grades: '), write(Grades), nl.
quick_test_setof :- setof(Student, Grade^student(Student, math, Grade), Students), write('Math students: '), write(Students), nl.
quick_test_precedence :- current_op(P, T, '^'), write('^ precedence: '), write(P), write(' type: '), write(T), nl.