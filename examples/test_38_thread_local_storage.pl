% ===================================================================
% TEST 38: Thread-Local Storage and Concurrent State Management
% ===================================================================
% Tests: thread_local/1, thread-specific state, concurrent data structures

% Thread-local storage declarations
:- thread_local(thread_counter/1).
:- thread_local(thread_data/2).
:- thread_local(thread_state/1).

% Initialize thread-local storage
init_thread_storage :-
    asserta(thread_counter(0)),
    asserta(thread_state(initialized)).

% Thread-safe counter operations
increment_counter(NewValue) :-
    (   thread_counter(Current)
    ->  retract(thread_counter(Current)),
        NewValue is Current + 1
    ;   NewValue = 1
    ),
    asserta(thread_counter(NewValue)).

get_counter(Value) :-
    (   thread_counter(Value)
    ->  true
    ;   Value = 0
    ).

reset_counter :-
    retractall(thread_counter(_)),
    asserta(thread_counter(0)).

% Thread-local data storage
store_thread_data(Key, Value) :-
    retractall(thread_data(Key, _)),
    asserta(thread_data(Key, Value)).

retrieve_thread_data(Key, Value) :-
    thread_data(Key, Value).

% Thread-local workspace
create_workspace(WorkspaceId) :-
    store_thread_data(workspace, WorkspaceId),
    store_thread_data(workspace_items, []).

add_to_workspace(Item) :-
    retrieve_thread_data(workspace_items, Items),
    append(Items, [Item], NewItems),
    store_thread_data(workspace_items, NewItems).

get_workspace_items(Items) :-
    retrieve_thread_data(workspace_items, Items).

clear_workspace :-
    store_thread_data(workspace_items, []).

% Thread-local caching system
:- thread_local(cache/2).

cache_put(Key, Value) :-
    retractall(cache(Key, _)),
    asserta(cache(Key, Value)).

cache_get(Key, Value) :-
    cache(Key, Value).

cache_invalidate(Key) :-
    retractall(cache(Key, _)).

cache_clear :-
    retractall(cache(_, _)).

% Thread-local statistics collection
:- thread_local(stats/2).

init_stats :-
    retractall(stats(_, _)),
    asserta(stats(operations, 0)),
    asserta(stats(start_time, 0)).

record_operation :-
    (   retract(stats(operations, Count))
    ->  NewCount is Count + 1
    ;   NewCount = 1
    ),
    asserta(stats(operations, NewCount)).

get_operation_count(Count) :-
    (   stats(operations, Count)
    ->  true
    ;   Count = 0
    ).

% Thread-local session management
:- thread_local(session/2).

start_session(SessionId) :-
    get_time(StartTime),
    retractall(session(_, _)),
    asserta(session(id, SessionId)),
    asserta(session(start_time, StartTime)).

end_session(Duration) :-
    session(start_time, StartTime),
    get_time(EndTime),
    Duration is EndTime - StartTime,
    retractall(session(_, _)).

get_session_info(SessionId, Duration) :-
    session(id, SessionId),
    session(start_time, StartTime),
    get_time(CurrentTime),
    Duration is CurrentTime - StartTime.

% Thread-local configuration
:- thread_local(config/2).

set_config(Key, Value) :-
    retractall(config(Key, _)),
    asserta(config(Key, Value)).

get_config(Key, Value) :-
    (   config(Key, Value)
    ->  true
    ;   default_config(Key, Value)
    ).

default_config(timeout, 30).
default_config(max_retries, 3).
default_config(buffer_size, 1024).

% Thread-local error handling
:- thread_local(error_log/2).

log_error(Error) :-
    get_time(Time),
    asserta(error_log(Time, Error)).

get_errors(Errors) :-
    findall(Time-Error, error_log(Time, Error), Errors).

clear_errors :-
    retractall(error_log(_, _)).

% Thread-local temporary storage
:- thread_local(temp_storage/3).

temp_store(Namespace, Key, Value) :-
    retractall(temp_storage(Namespace, Key, _)),
    asserta(temp_storage(Namespace, Key, Value)).

temp_retrieve(Namespace, Key, Value) :-
    temp_storage(Namespace, Key, Value).

temp_clear_namespace(Namespace) :-
    retractall(temp_storage(Namespace, _, _)).

temp_clear_all :-
    retractall(temp_storage(_, _, _)).

% Concurrent data structure: thread-local stack
:- thread_local(local_stack/1).

stack_init :-
    retractall(local_stack(_)),
    asserta(local_stack([])).

stack_push(Item) :-
    (   retract(local_stack(Stack))
    ->  true
    ;   Stack = []
    ),
    NewStack = [Item|Stack],
    asserta(local_stack(NewStack)).

stack_pop(Item) :-
    retract(local_stack([Item|Rest])),
    asserta(local_stack(Rest)).

stack_peek(Item) :-
    local_stack([Item|_]).

stack_size(Size) :-
    (   local_stack(Stack)
    ->  length(Stack, Size)
    ;   Size = 0
    ).

% Thread-local priority queue
:- thread_local(priority_queue/1).

pq_init :-
    retractall(priority_queue(_)),
    asserta(priority_queue([])).

pq_insert(Priority, Item) :-
    (   retract(priority_queue(Queue))
    ->  true
    ;   Queue = []
    ),
    insert_by_priority(Priority-Item, Queue, NewQueue),
    asserta(priority_queue(NewQueue)).

insert_by_priority(Item, [], [Item]).
insert_by_priority(P1-I1, [P2-I2|Rest], [P1-I1, P2-I2|Rest]) :-
    P1 >= P2, !.
insert_by_priority(Item, [H|T], [H|NewT]) :-
    insert_by_priority(Item, T, NewT).

pq_extract_max(Priority, Item) :-
    retract(priority_queue([Priority-Item|Rest])),
    asserta(priority_queue(Rest)).

% Test thread-local storage
test_thread_storage :-
    init_thread_storage,
    increment_counter(C1),
    increment_counter(C2),
    get_counter(Final),
    format('Counter progression: ~w -> ~w -> ~w~n', [C1, C2, Final]).

test_workspace :-
    create_workspace(ws1),
    add_to_workspace(item1),
    add_to_workspace(item2),
    get_workspace_items(Items),
    format('Workspace items: ~w~n', [Items]).

test_cache :-
    cache_put(key1, value1),
    cache_put(key2, value2),
    cache_get(key1, V1),
    cache_get(key2, V2),
    format('Cached values: ~w, ~w~n', [V1, V2]).

test_stack :-
    stack_init,
    stack_push(a),
    stack_push(b),
    stack_push(c),
    stack_pop(Item1),
    stack_peek(Item2),
    format('Popped: ~w, Peek: ~w~n', [Item1, Item2]).

test_priority_queue :-
    pq_init,
    pq_insert(3, task_low),
    pq_insert(1, task_high),
    pq_insert(2, task_medium),
    pq_extract_max(P1, T1),
    pq_extract_max(P2, T2),
    format('Extracted: ~w:~w, ~w:~w~n', [P1, T1, P2, T2]).

% Test queries:
% ?- test_thread_storage.
% ?- test_workspace.
% ?- test_cache.
% ?- test_stack.
% ?- test_priority_queue.
% ?- start_session(session1), get_session_info(Id, Duration).
% ?- set_config(timeout, 60), get_config(timeout, Value).