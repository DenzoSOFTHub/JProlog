% ===================================================================
% TEST 27: Advanced Module System
% ===================================================================
% Tests: Module interfaces, qualified calls, module transparency

:- module(test_module, [public_pred/2, exported_util/1]).

% Module interface definition
:- interface.
public_pred(X, Y) :- internal_logic(X, Y).
exported_util(List) :- process_list(List).

% Private predicates (not exported)
internal_logic(X, Y) :-
    transform(X, Temp),
    validate(Temp, Y).

transform(Input, Output) :-
    Output is Input * 2.

validate(Value, Value) :-
    Value > 0.

process_list([]).
process_list([H|T]) :-
    process_item(H),
    process_list(T).

process_item(Item) :-
    write(Item), nl.

% Module with qualified imports
:- module(client_module, [test_client/0]).
:- use_module(test_module, [public_pred/2]).

test_client :-
    test_module:public_pred(5, Result),
    write('Result: '), write(Result), nl.

% Meta-predicate with module awareness
:- meta_predicate map_module(2, ?, ?).

map_module(_, [], []).
map_module(Pred, [H1|T1], [H2|T2]) :-
    call(Pred, H1, H2),
    map_module(Pred, T1, T2).

% Module-transparent predicate
:- module_transparent debug_call/1.

debug_call(Goal) :-
    write('Calling: '), write(Goal), nl,
    call(Goal).

% Dynamic module loading
load_dynamic_module(ModuleName, FileName) :-
    use_module(FileName),
    current_module(ModuleName).

% Test queries:
% ?- public_pred(3, X).
% ?- test_client.
% ?- map_module(transform, [1,2,3], Results).
% ?- debug_call(member(2, [1,2,3])).