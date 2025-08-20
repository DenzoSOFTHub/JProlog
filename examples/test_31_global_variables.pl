% ===================================================================
% TEST 31: Global Variables and State Management
% ===================================================================
% Tests: Global variables, nb_setval/2, nb_getval/2, state persistence

% Global variable operations
test_global_vars :-
    nb_setval(counter, 0),
    increment_global_counter,
    increment_global_counter,
    increment_global_counter,
    nb_getval(counter, Value),
    format('Final counter value: ~w~n', [Value]).

increment_global_counter :-
    nb_getval(counter, Current),
    Next is Current + 1,
    nb_setval(counter, Next).

% Persistent state across calls
test_persistent_state :-
    initialize_state,
    add_item(apple),
    add_item(banana),
    add_item(cherry),
    get_all_items(Items),
    format('Items: ~w~n', [Items]).

initialize_state :-
    nb_setval(item_list, []).

add_item(Item) :-
    nb_getval(item_list, Current),
    nb_setval(item_list, [Item|Current]).

get_all_items(Items) :-
    nb_getval(item_list, Items).

% Global configuration management
setup_config :-
    nb_setval(config, [
        debug_mode(true),
        max_depth(100),
        timeout(30)
    ]).

get_config(Key, Value) :-
    nb_getval(config, Config),
    member(Key(Value), Config).

set_config(Key, Value) :-
    nb_getval(config, Config),
    select(Key(_), Config, RestConfig),
    nb_setval(config, [Key(Value)|RestConfig]).

% Global statistics tracking
initialize_stats :-
    nb_setval(stats, [
        calls(0),
        successes(0),
        failures(0)
    ]).

track_call(Goal) :-
    increment_stat(calls),
    (   call(Goal)
    ->  increment_stat(successes)
    ;   increment_stat(failures),
        fail
    ).

increment_stat(Stat) :-
    nb_getval(stats, Stats),
    select(Stat(N), Stats, RestStats),
    N1 is N + 1,
    nb_setval(stats, [Stat(N1)|RestStats]).

get_stats(Stats) :-
    nb_getval(stats, Stats).

% Session management
start_session(SessionId) :-
    get_time(Time),
    nb_setval(session, [
        id(SessionId),
        start_time(Time),
        actions([])
    ]).

add_action(Action) :-
    nb_getval(session, Session),
    select(actions(Actions), Session, RestSession),
    nb_setval(session, [actions([Action|Actions])|RestSession]).

get_session_info(Info) :-
    nb_getval(session, Info).

% Global caching mechanism
cache_result(Key, Value) :-
    (   nb_current(cache, Cache)
    ->  true
    ;   Cache = [],
        nb_setval(cache, Cache)
    ),
    select(Key-_, Cache, RestCache),
    nb_setval(cache, [Key-Value|RestCache]).

get_cached_result(Key, Value) :-
    nb_getval(cache, Cache),
    member(Key-Value, Cache).

cached_computation(Input, Result) :-
    (   get_cached_result(Input, Result)
    ->  format('Cache hit for ~w~n', [Input])
    ;   expensive_computation(Input, Result),
        cache_result(Input, Result),
        format('Computed and cached ~w~n', [Input])
    ).

expensive_computation(Input, Result) :-
    sleep(1),  % Simulate expensive computation
    Result is Input * Input.

% Test queries:
% ?- test_global_vars.
% ?- test_persistent_state.
% ?- setup_config, get_config(debug_mode, Value).
% ?- initialize_stats, track_call(member(2, [1,2,3])), get_stats(S).
% ?- start_session(session_1), add_action(login), get_session_info(Info).
% ?- cached_computation(5, R1), cached_computation(5, R2).