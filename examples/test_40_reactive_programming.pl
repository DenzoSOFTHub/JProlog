% ===================================================================
% TEST 40: Reactive Programming and Event-Driven Systems
% ===================================================================
% Tests: Event streams, reactive predicates, publish-subscribe, observers

% Event system infrastructure
:- dynamic event_queue/2.
:- dynamic event_listener/3.
:- dynamic reactive_state/2.

% Event publishing and subscription
publish_event(EventType, Data) :-
    get_time(Timestamp),
    EventId is round(Timestamp * 1000000),
    Event = event(EventId, EventType, Data, Timestamp),
    assert(event_queue(EventId, Event)),
    notify_listeners(Event),
    process_reactive_rules(Event).

subscribe(ListenerId, EventType, Handler) :-
    assert(event_listener(ListenerId, EventType, Handler)).

unsubscribe(ListenerId, EventType) :-
    retract(event_listener(ListenerId, EventType, _)).

notify_listeners(Event) :-
    Event = event(_, EventType, Data, _),
    forall(
        event_listener(ListenerId, EventType, Handler),
        call_listener(ListenerId, Handler, Event)
    ).

call_listener(ListenerId, Handler, Event) :-
    catch(
        call(Handler, ListenerId, Event),
        Error,
        handle_listener_error(ListenerId, Error)
    ).

handle_listener_error(ListenerId, Error) :-
    format('Error in listener ~w: ~w~n', [ListenerId, Error]).

% Reactive state management
set_reactive_state(Key, Value) :-
    OldValue = undefined,
    (   retract(reactive_state(Key, OldValue))
    ->  true
    ;   true
    ),
    assert(reactive_state(Key, Value)),
    publish_event(state_change, state(Key, OldValue, Value)).

get_reactive_state(Key, Value) :-
    reactive_state(Key, Value).

% Event stream processing
event_stream(StreamId, FilterPredicate) :-
    findall(Event, (
        event_queue(_, Event),
        call(FilterPredicate, Event)
    ), Events),
    process_stream(StreamId, Events).

process_stream(_, []).
process_stream(StreamId, [Event|Events]) :-
    handle_stream_event(StreamId, Event),
    process_stream(StreamId, Events).

handle_stream_event(StreamId, Event) :-
    format('Stream ~w processing: ~w~n', [StreamId, Event]).

% Event aggregation and windowing
sliding_window(EventType, WindowSize, AggregateFunction, Result) :-
    findall(Event, (
        event_queue(_, Event),
        Event = event(_, EventType, _, _)
    ), AllEvents),
    take_last(AllEvents, WindowSize, WindowEvents),
    apply_aggregate(AggregateFunction, WindowEvents, Result).

take_last(List, N, LastN) :-
    length(List, Len),
    (   Len =< N
    ->  LastN = List
    ;   Skip is Len - N,
        skip_elements(List, Skip, LastN)
    ).

skip_elements(List, 0, List) :- !.
skip_elements([_|Rest], N, Result) :-
    N > 0,
    N1 is N - 1,
    skip_elements(Rest, N1, Result).

apply_aggregate(count, Events, Count) :-
    length(Events, Count).
apply_aggregate(sum, Events, Sum) :-
    maplist(extract_numeric_data, Events, Values),
    sum_list(Values, Sum).
apply_aggregate(average, Events, Average) :-
    maplist(extract_numeric_data, Events, Values),
    sum_list(Values, Sum),
    length(Values, Count),
    (   Count > 0
    ->  Average is Sum / Count
    ;   Average = 0
    ).

extract_numeric_data(event(_, _, Data, _), Value) :-
    (   number(Data)
    ->  Value = Data
    ;   Data = value(Value), number(Value)
    ->  true
    ;   Value = 0
    ).

% Observer pattern implementation
:- dynamic observer/2.

add_observer(Subject, Observer) :-
    assert(observer(Subject, Observer)).

remove_observer(Subject, Observer) :-
    retract(observer(Subject, Observer)).

notify_observers(Subject, Change) :-
    forall(
        observer(Subject, Observer),
        call_observer(Observer, Subject, Change)
    ).

call_observer(Observer, Subject, Change) :-
    catch(
        call(Observer, Subject, Change),
        Error,
        format('Observer ~w error: ~w~n', [Observer, Error])
    ).

% Reactive data structures
reactive_list(ListId, InitialList) :-
    set_reactive_state(list(ListId), InitialList).

reactive_list_append(ListId, Item) :-
    get_reactive_state(list(ListId), CurrentList),
    append(CurrentList, [Item], NewList),
    set_reactive_state(list(ListId), NewList),
    publish_event(list_append, list_change(ListId, append, Item)).

reactive_list_remove(ListId, Item) :-
    get_reactive_state(list(ListId), CurrentList),
    select(Item, CurrentList, NewList),
    set_reactive_state(list(ListId), NewList),
    publish_event(list_remove, list_change(ListId, remove, Item)).

reactive_list_get(ListId, List) :-
    get_reactive_state(list(ListId), List).

% Event-driven workflow system
:- dynamic workflow_state/2.
:- dynamic workflow_rule/3.

define_workflow_rule(WorkflowId, TriggerEvent, Action) :-
    assert(workflow_rule(WorkflowId, TriggerEvent, Action)).

start_workflow(WorkflowId, InitialState) :-
    assert(workflow_state(WorkflowId, InitialState)),
    publish_event(workflow_start, workflow(WorkflowId, InitialState)).

process_reactive_rules(Event) :-
    forall(
        workflow_rule(WorkflowId, TriggerEvent, Action),
        (   matches_trigger(Event, TriggerEvent)
        ->  execute_workflow_action(WorkflowId, Action, Event)
        ;   true
        )
    ).

matches_trigger(Event, TriggerPattern) :-
    Event = event(_, EventType, Data, _),
    TriggerPattern = event_pattern(EventType, DataPattern),
    matches_data_pattern(Data, DataPattern).

matches_data_pattern(Data, Pattern) :-
    Data = Pattern.

execute_workflow_action(WorkflowId, Action, Event) :-
    (   workflow_state(WorkflowId, State)
    ->  call(Action, WorkflowId, State, Event, NewState),
        retract(workflow_state(WorkflowId, State)),
        assert(workflow_state(WorkflowId, NewState))
    ;   true
    ).

% Complex event processing
complex_event_pattern(PatternId, EventSequence, TimeWindow) :-
    find_event_sequence(EventSequence, TimeWindow, MatchedEvents),
    (   MatchedEvents \= []
    ->  publish_event(complex_pattern_detected, 
                     pattern(PatternId, MatchedEvents))
    ;   true
    ).

find_event_sequence([], _, []).
find_event_sequence([Pattern|Patterns], TimeWindow, [Event|Events]) :-
    event_queue(_, Event),
    matches_pattern(Event, Pattern),
    find_remaining_sequence(Patterns, Event, TimeWindow, Events).

find_remaining_sequence([], _, _, []).
find_remaining_sequence([Pattern|Patterns], PreviousEvent, TimeWindow, [Event|Events]) :-
    event_queue(_, Event),
    matches_pattern(Event, Pattern),
    within_time_window(PreviousEvent, Event, TimeWindow),
    find_remaining_sequence(Patterns, Event, TimeWindow, Events).

matches_pattern(Event, Pattern) :-
    Event = event(_, EventType, Data, _),
    Pattern = pattern(EventType, DataPattern),
    matches_data_pattern(Data, DataPattern).

within_time_window(Event1, Event2, MaxSeconds) :-
    Event1 = event(_, _, _, Time1),
    Event2 = event(_, _, _, Time2),
    TimeDiff is abs(Time2 - Time1),
    TimeDiff =< MaxSeconds.

% Test reactive system
test_reactive_system :-
    % Subscribe to events
    subscribe(logger, user_action, log_user_action),
    subscribe(counter, user_action, count_actions),
    
    % Initialize reactive state
    set_reactive_state(action_count, 0),
    
    % Publish some events
    publish_event(user_action, login(user1)),
    publish_event(user_action, click(button1)),
    publish_event(user_action, logout(user1)),
    
    % Check final state
    get_reactive_state(action_count, Count),
    format('Total actions: ~w~n', [Count]).

log_user_action(logger, Event) :-
    Event = event(Id, Type, Data, Time),
    format('LOG: ~w - ~w: ~w at ~w~n', [Id, Type, Data, Time]).

count_actions(counter, Event) :-
    Event = event(_, user_action, _, _),
    get_reactive_state(action_count, Current),
    Next is Current + 1,
    set_reactive_state(action_count, Next).

test_reactive_list :-
    reactive_list(mylist, []),
    reactive_list_append(mylist, item1),
    reactive_list_append(mylist, item2),
    reactive_list_get(mylist, List),
    format('Reactive list: ~w~n', [List]).

test_workflow :-
    define_workflow_rule(order_process, 
                        event_pattern(order_placed, _),
                        process_order),
    start_workflow(order_process, pending),
    publish_event(order_placed, order(12345, customer1)).

process_order(WorkflowId, State, Event, NewState) :-
    Event = event(_, _, order(OrderId, Customer), _),
    format('Processing order ~w for ~w (workflow: ~w, state: ~w)~n', 
           [OrderId, Customer, WorkflowId, State]),
    NewState = processing.

% Test queries:
% ?- test_reactive_system.
% ?- test_reactive_list.
% ?- test_workflow.
% ?- sliding_window(user_action, 5, count, Count).
% ?- complex_event_pattern(login_logout, [pattern(user_action, login(_)), pattern(user_action, logout(_))], 60).