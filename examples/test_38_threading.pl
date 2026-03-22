% Test 38: Threading predicates
% Tests: thread_create, thread_join, thread_self, thread_sleep,
%        thread_detach, message_queue_create,
%        thread_send_message, thread_get_message, thread_peek_message

% ============================================================
% Test framework
% ============================================================
:- dynamic(test_passed/1).
:- dynamic(test_failed/2).
:- dynamic(test_count/1).
:- assert(test_count(0)).

run_test(Name, Goal) :-
    retract(test_count(N)), N1 is N + 1, assert(test_count(N1)),
    copy_term(Goal, GoalCopy),
    ( catch(call(GoalCopy), E, (assert(test_failed(Name, E)), fail))
    -> assert(test_passed(Name)),
       write('  PASS: '), write(Name), nl
    ;  ( \+ test_failed(Name, _) -> assert(test_failed(Name, failed)) ; true ),
       write('  FAIL: '), write(Name), nl
    ).

run_all_tests :-
    write('=== Test 38: Threading Predicates ==='), nl, nl,
    test_thread_self,
    test_thread_create_join,
    test_thread_sleep,
    test_thread_detach,
    test_message_queues,
    test_multiple_threads,
    nl, write('--- Results ---'), nl,
    aggregate_all(count, test_passed(_), Passed),
    aggregate_all(count, test_failed(_, _), Failed),
    test_count(Total),
    write('Passed: '), write(Passed), write('/'), write(Total), nl,
    write('Failed: '), write(Failed), nl,
    ( Failed > 0
    -> forall(test_failed(N, R), (write('  '), write(N), write(': '), write(R), nl))
    ; true
    ).

% ============================================================
% 1. thread_self
% ============================================================
test_thread_self :-
    write('--- thread_self ---'), nl,
    run_test(thread_self_returns_number,
        (thread_self(Id), number(Id))),
    run_test(thread_self_positive,
        (thread_self(Id), Id > 0)),
    run_test(thread_self_consistent,
        (thread_self(Id1), thread_self(Id2), Id1 == Id2)).

% ============================================================
% 2. thread_create and thread_join
% ============================================================
test_thread_create_join :-
    write('--- thread_create/join ---'), nl,
    run_test(thread_create_returns_id,
        (thread_create(true, Id), number(Id))),
    run_test(thread_join_returns_status,
        (thread_create(true, Id),
         thread_sleep(0.1),
         thread_join(Id, Status),
         nonvar(Status))),
    run_test(thread_join_completed,
        (thread_create(true, Id),
         thread_sleep(0.1),
         thread_join(Id, Status),
         atom(Status))).

% ============================================================
% 3. thread_sleep
% ============================================================
test_thread_sleep :-
    write('--- thread_sleep ---'), nl,
    run_test(thread_sleep_short,
        thread_sleep(0.01)),
    run_test(thread_sleep_zero,
        thread_sleep(0.0)).

% ============================================================
% 4. thread_detach
% ============================================================
test_thread_detach :-
    write('--- thread_detach ---'), nl,
    run_test(thread_detach_succeeds,
        (thread_create(true, Id),
         thread_detach(Id))).

% ============================================================
% 5. Message queues
% ============================================================
test_message_queues :-
    write('--- message queues ---'), nl,
    run_test(mq_create_returns_id,
        (message_queue_create(Qid), number(Qid))),
    run_test(mq_send_then_receive,
        (message_queue_create(Q),
         thread_send_message(Q, hello),
         thread_get_message(Q, Msg),
         Msg == hello)),
    run_test(mq_fifo_order,
        (message_queue_create(Q),
         thread_send_message(Q, first),
         thread_send_message(Q, second),
         thread_get_message(Q, M1),
         thread_get_message(Q, M2),
         M1 == first, M2 == second)),
    run_test(mq_peek_nondestructive,
        (message_queue_create(Q),
         thread_send_message(Q, peek_msg),
         thread_peek_message(Q, M1),
         thread_peek_message(Q, M2),
         M1 == peek_msg, M2 == peek_msg)),
    run_test(mq_peek_empty_fails,
        (message_queue_create(Q),
         \+ thread_peek_message(Q, _))),
    run_test(mq_send_multiple_receive_all,
        (message_queue_create(Q),
         thread_send_message(Q, a),
         thread_send_message(Q, b),
         thread_send_message(Q, c),
         thread_get_message(Q, a),
         thread_get_message(Q, b),
         thread_get_message(Q, c))).

% ============================================================
% 6. Multiple threads
% ============================================================
test_multiple_threads :-
    write('--- multiple threads ---'), nl,
    run_test(two_threads_different_ids,
        (thread_create(true, Id1),
         thread_create(true, Id2),
         Id1 \== Id2,
         thread_sleep(0.1),
         thread_join(Id1, _),
         thread_join(Id2, _))),
    run_test(three_threads_all_different,
        (thread_create(true, Id1),
         thread_create(true, Id2),
         thread_create(true, Id3),
         Id1 \== Id2, Id2 \== Id3, Id1 \== Id3,
         thread_sleep(0.1),
         thread_join(Id1, _),
         thread_join(Id2, _),
         thread_join(Id3, _))).

:- run_all_tests.
