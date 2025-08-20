% ===================================================================
% TEST 09: Meta-Predicates
% ===================================================================
% Tests: findall/3, bagof/3, setof/3, call/1, once/1, forall/2

% Test data
student(john, math, 85).
student(john, physics, 90).
student(mary, math, 92).
student(mary, chemistry, 88).
student(bob, physics, 78).
student(bob, math, 80).

likes(mary, wine).
likes(mary, food).
likes(john, wine).
likes(john, mary).

% Collection predicates
all_students(Students) :-
    findall(Student, student(Student, _, _), AllStudents),
    sort(AllStudents, Students).

subjects_for_student(Student, Subjects) :-
    findall(Subject, student(Student, Subject, _), Subjects).

grades_by_subject(Subject, Grades) :-
    bagof(Grade, Student^student(Student, Subject, Grade), Grades).

unique_subjects(Subjects) :-
    setof(Subject, Student^Grade^student(Student, Subject, Grade), Subjects).

% Meta-call predicates
apply_to_list(_, [], []).
apply_to_list(Pred, [H|T], [NewH|NewT]) :-
    call(Pred, H, NewH),
    apply_to_list(Pred, T, NewT).

double(X, Y) :- Y is X * 2.
square(X, Y) :- Y is X * X.

% Once predicate - deterministic execution
first_student(Student) :-
    once(student(Student, _, _)).

% Forall predicate - universal quantification  
all_students_have_grades :-
    forall(student(Student, _, _), 
           (student(Student, _, Grade), number(Grade))).

check_all_positive(List) :-
    forall(member(X, List), X > 0).

% Higher-order predicates
map_list(_, [], []).
map_list(Pred, [H|T], [MH|MT]) :-
    call(Pred, H, MH),
    map_list(Pred, T, MT).

filter_list(_, [], []).
filter_list(Pred, [H|T], [H|FT]) :-
    call(Pred, H), !,
    filter_list(Pred, T, FT).
filter_list(Pred, [_|T], FT) :-
    filter_list(Pred, T, FT).

positive(X) :- number(X), X > 0.

% Test queries:
% ?- all_students(Students).
% ?- subjects_for_student(john, Subjects).
% ?- grades_by_subject(math, Grades).
% ?- unique_subjects(Subjects).
% ?- apply_to_list(double, [1, 2, 3], Result).
% ?- first_student(Student).
% ?- check_all_positive([1, 2, 3]).
% ?- filter_list(positive, [-1, 2, -3, 4], Positive).