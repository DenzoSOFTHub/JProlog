% ===================================================================
% TEST 32: Term Expansion and Macros
% ===================================================================
% Tests: term_expansion/2, goal_expansion/2, macro systems

% Basic term expansion
term_expansion(simple_rule(X), (complex_rule(X) :- condition(X))).

simple_rule(valid_input).

condition(X) :-
    X = valid_input.

% Goal expansion for optimization
goal_expansion(inefficient_member(X, List), efficient_member(X, List)).

inefficient_member(X, [X|_]).
inefficient_member(X, [_|T]) :-
    inefficient_member(X, T).

efficient_member(X, List) :-
    member(X, List).

% Macro system for domain-specific language
term_expansion(rule(Name, Conditions, Actions), 
               (Name :- process_conditions(Conditions), execute_actions(Actions))).

rule(fire_alarm, [smoke_detected, temperature_high], [sound_alarm, call_fire_dept]).

process_conditions([]).
process_conditions([Condition|Rest]) :-
    call(Condition),
    process_conditions(Rest).

execute_actions([]).
execute_actions([Action|Rest]) :-
    call(Action),
    execute_actions(Rest).

smoke_detected :- true.  % Simulated sensor
temperature_high :- true. % Simulated sensor
sound_alarm :- write('ALARM SOUNDING'), nl.
call_fire_dept :- write('Calling fire department'), nl.

% Syntax sugar expansion
term_expansion(for_each(Var, List, Goal), 
               (for_each_impl(List, Var, Goal))).

for_each_impl([], _, _).
for_each_impl([H|T], Var, Goal) :-
    Var = H,
    call(Goal),
    for_each_impl(T, Var, Goal).

% Test using syntax sugar
test_for_each :-
    for_each(X, [1,2,3,4,5], (Y is X * 2, format('~w * 2 = ~w~n', [X, Y]))).

% Attribute expansion for object-oriented style
term_expansion(class(ClassName, Attributes, Methods),
               (class_def(ClassName, Attributes, Methods))).

class(person, [name, age], [greet, birthday]).

class_def(ClassName, Attributes, Methods) :-
    assert(class_attributes(ClassName, Attributes)),
    assert(class_methods(ClassName, Methods)).

% Method call expansion
goal_expansion(Object.Method(Args), 
               call_method(Object, Method, Args)).

call_method(Object, Method, Args) :-
    object_type(Object, Type),
    class_methods(Type, Methods),
    member(Method, Methods),
    MethodCall =.. [Method, Object|Args],
    call(MethodCall).

% Conditional compilation
term_expansion(ifdef(Flag, Code), Code) :-
    current_prolog_flag(Flag, true).

term_expansion(ifdef(Flag, _), true) :-
    \+ current_prolog_flag(Flag, true).

% Debug mode expansion
term_expansion(debug(Goal), (write('DEBUG: '), write(Goal), nl, call(Goal))) :-
    current_prolog_flag(debug, true).

term_expansion(debug(_), true) :-
    \+ current_prolog_flag(debug, true).

% Pattern matching expansion
term_expansion(match(Value, Patterns), 
               match_impl(Value, Patterns)).

match_impl(Value, [Pattern-Action|_]) :-
    Value = Pattern,
    call(Action).
match_impl(Value, [_|Rest]) :-
    match_impl(Value, Rest).

test_pattern_matching :-
    match(color(red), [
        color(red) - write('Red color detected'),
        color(blue) - write('Blue color detected'),
        _ - write('Unknown color')
    ]).

% Test queries:
% ?- simple_rule(valid_input).
% ?- inefficient_member(2, [1,2,3]).
% ?- fire_alarm.
% ?- test_for_each.
% ?- test_pattern_matching.