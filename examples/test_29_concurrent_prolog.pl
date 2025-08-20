% ===================================================================
% TEST 29: Concurrent and Parallel Prolog
% ===================================================================
% Tests: Concurrent execution, message passing, synchronization

% Concurrent goal execution
test_concurrent :-
    concurrent([
        goal1(X),
        goal2(Y),
        goal3(Z)
    ], [X, Y, Z]).

goal1(result1) :- sleep(1).
goal2(result2) :- sleep(1).
goal3(result3) :- sleep(1).

% Producer-consumer pattern
producer_consumer(Buffer, N) :-
    thread_create(producer(Buffer, N), ProducerId, []),
    thread_create(consumer(Buffer, N), ConsumerId, []),
    thread_join(ProducerId, _),
    thread_join(ConsumerId, _).

producer(Buffer, 0) :-
    buffer_close(Buffer).
producer(Buffer, N) :-
    N > 0,
    buffer_put(Buffer, N),
    N1 is N - 1,
    producer(Buffer, N1).

consumer(Buffer, N) :-
    (   buffer_get(Buffer, Item)
    ->  process_item(Item),
        consumer(Buffer, N)
    ;   true
    ).

process_item(Item) :-
    format('Processing: ~w~n', [Item]).

% Message passing between agents
agent_system :-
    thread_create(agent(agent1, [agent2]), Id1, [alias(agent1)]),
    thread_create(agent(agent2, [agent1]), Id2, [alias(agent2)]),
    thread_send_message(agent1, start),
    thread_join(Id1, _),
    thread_join(Id2, _).

agent(Name, Others) :-
    thread_get_message(Message),
    handle_message(Name, Message, Others),
    agent(Name, Others).

handle_message(Name, start, Others) :-
    format('~w starting~n', [Name]),
    send_to_others(Others, hello(Name)).

handle_message(Name, hello(From), _) :-
    format('~w received hello from ~w~n', [Name, From]).

send_to_others([], _).
send_to_others([Agent|Rest], Message) :-
    thread_send_message(Agent, Message),
    send_to_others(Rest, Message).

% Parallel map operation
parallel_map(Pred, List, Results) :-
    length(List, N),
    create_workers(N, Pred, List, Workers),
    collect_results(Workers, Results).

create_workers(0, _, [], []).
create_workers(N, Pred, [Item|Items], [Worker|Workers]) :-
    N > 0,
    thread_create(call(Pred, Item, Result), Worker, []),
    N1 is N - 1,
    create_workers(N1, Pred, Items, Workers).

collect_results([], []).
collect_results([Worker|Workers], [Result|Results]) :-
    thread_join(Worker, Result),
    collect_results(Workers, Results).

% Synchronization primitives
test_synchronization :-
    mutex_create(Mutex),
    shared_counter(Mutex, 0, 100).

shared_counter(Mutex, Current, Max) :-
    Current < Max,
    with_mutex(Mutex, (
        increment_counter(Current, Next),
        format('Counter: ~w~n', [Next])
    )),
    shared_counter(Mutex, Next, Max).
shared_counter(_, Max, Max).

increment_counter(X, Y) :- Y is X + 1.

% Test queries:
% ?- test_concurrent.
% ?- producer_consumer(buffer1, 5).
% ?- agent_system.
% ?- parallel_map(double, [1,2,3,4], Results).
% ?- test_synchronization.