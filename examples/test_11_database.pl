% ===================================================================
% TEST 11: Dynamic Database Operations
% ===================================================================
% Tests: assert/1, retract/1, retractall/1, abolish/1

% Initial dynamic facts
:- dynamic(score/2).
:- dynamic(temporary_fact/1).
:- dynamic(counter/1).

score(john, 85).
score(mary, 92).
counter(0).

% Database manipulation predicates
add_score(Student, Score) :-
    assertz(score(Student, Score)).

remove_score(Student) :-
    retract(score(Student, _)).

remove_all_scores(Student) :-
    retractall(score(Student, _)).

update_score(Student, NewScore) :-
    retract(score(Student, _)),
    assertz(score(Student, NewScore)).

% Counter operations
increment_counter :-
    retract(counter(N)),
    N1 is N + 1,
    assertz(counter(N1)).

get_counter(N) :-
    counter(N).

reset_counter :-
    retractall(counter(_)),
    assertz(counter(0)).

% Temporary facts management
add_temp_fact(Fact) :-
    assertz(temporary_fact(Fact)).

clear_temp_facts :-
    retractall(temporary_fact(_)).

list_temp_facts(Facts) :-
    findall(Fact, temporary_fact(Fact), Facts).

% Database queries
average_score(Average) :-
    findall(Score, score(_, Score), Scores),
    sum_list(Scores, Sum),
    length(Scores, Count),
    Count > 0,
    Average is Sum / Count.

top_student(Student) :-
    score(Student, Score),
    \+ (score(_, OtherScore), OtherScore > Score).

% Predicate existence checking
predicate_exists(Name/Arity) :-
    current_predicate(Name/Arity).

% Clean database
cleanup_database :-
    abolish(score/2),
    abolish(temporary_fact/1),
    retractall(counter(_)),
    assertz(counter(0)).

% Test queries:
% ?- add_score(bob, 78).
% ?- score(bob, Score).
% ?- update_score(john, 90).
% ?- increment_counter.
% ?- get_counter(N).
% ?- add_temp_fact(test1).
% ?- add_temp_fact(test2).
% ?- list_temp_facts(Facts).
% ?- average_score(Avg).
% ?- top_student(Student).