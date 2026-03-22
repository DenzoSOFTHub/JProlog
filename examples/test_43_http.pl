% Test 43: HTTP Server and Client Predicates
% Tests: http_server/2, http_stop/1, http_client_get/2, http_client_post/3,
%        url_encode/2, url_decode/2, http_open/3

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
    write('=== Test 43: HTTP Predicates ==='), nl, nl,
    test_url_encoding,
    test_server_lifecycle,
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
% 1. URL encoding/decoding
% ============================================================
test_url_encoding :-
    write('--- URL Encoding ---'), nl,
    run_test(url_encode_simple,
        (url_encode('hello world', E), atom(E))),
    run_test(url_encode_space,
        (url_encode('hello world', E), atom_codes(E, Codes),
         ( member(43, Codes) ; member(37, Codes) ))),
    run_test(url_encode_special,
        (url_encode('a&b=c', E), atom(E))),
    run_test(url_decode_simple,
        (url_encode(hello, E), url_decode(E, D), D == hello)),
    run_test(url_roundtrip,
        (url_encode('hello world', E), url_decode(E, D), D == 'hello world')),
    run_test(url_roundtrip_special,
        (url_encode('a&b=c', E), url_decode(E, D), D == 'a&b=c')),
    run_test(url_decode_plain,
        (url_decode(hello, D), D == hello)).

% ============================================================
% 2. Server lifecycle
% ============================================================
test_server_lifecycle :-
    write('--- Server Lifecycle ---'), nl,
    run_test(server_start,
        (http_server(18765, Handle), atom(Handle))),
    run_test(server_stop,
        (http_server(18766, Handle), http_stop(Handle))),
    run_test(server_start_stop_start,
        (http_server(18767, H1), http_stop(H1),
         http_server(18767, H2), http_stop(H2), atom(H2))),
    % Clean up any remaining servers
    catch(http_stop(server_18765), _, true).

:- run_all_tests.
