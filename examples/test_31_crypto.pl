% Test 31: Cryptographic predicates
% Tests: md5_hash, sha256_hash, sha512_hash, crypto_hash,
%        hmac, base64_encode, base64_decode, uuid, random_token, crypto_random_int

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
    write('=== Test 31: Cryptographic Predicates ==='), nl, nl,
    test_md5,
    test_sha256,
    test_sha512,
    test_crypto_hash,
    test_hmac,
    test_base64,
    test_uuid,
    test_random_token,
    test_crypto_random_int,
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
% 1. MD5 hash
% ============================================================
test_md5 :-
    write('--- MD5 Hash ---'), nl,
    run_test(md5_returns_atom,
        (md5_hash(hello, H), atom(H))),
    run_test(md5_deterministic,
        (md5_hash(hello, H1), md5_hash(hello, H2), H1 == H2)),
    run_test(md5_length_32,
        (md5_hash(hello, H), atom_length(H, 32))),
    run_test(md5_different_inputs,
        (md5_hash(hello, H1), md5_hash(world, H2), H1 \== H2)),
    run_test(md5_empty_atom,
        (md5_hash('', H), atom(H), atom_length(H, 32))),
    run_test(md5_long_input,
        (md5_hash('this is a longer input string for testing', H), atom_length(H, 32))).

% ============================================================
% 2. SHA-256 hash
% ============================================================
test_sha256 :-
    write('--- SHA-256 Hash ---'), nl,
    run_test(sha256_returns_atom,
        (sha256_hash(hello, H), atom(H))),
    run_test(sha256_deterministic,
        (sha256_hash(test, H1), sha256_hash(test, H2), H1 == H2)),
    run_test(sha256_length_64,
        (sha256_hash(hello, H), atom_length(H, 64))),
    run_test(sha256_different_inputs,
        (sha256_hash(hello, H1), sha256_hash(world, H2), H1 \== H2)).

% ============================================================
% 3. SHA-512 hash
% ============================================================
test_sha512 :-
    write('--- SHA-512 Hash ---'), nl,
    run_test(sha512_returns_atom,
        (sha512_hash(hello, H), atom(H))),
    run_test(sha512_length_128,
        (sha512_hash(hello, H), atom_length(H, 128))),
    run_test(sha512_deterministic,
        (sha512_hash(data, H1), sha512_hash(data, H2), H1 == H2)).

% ============================================================
% 4. crypto_hash (generic)
% ============================================================
test_crypto_hash :-
    write('--- crypto_hash ---'), nl,
    run_test(crypto_hash_md5,
        (crypto_hash('MD5', hello, H), atom_length(H, 32))),
    run_test(crypto_hash_sha256,
        (crypto_hash('SHA-256', hello, H), atom_length(H, 64))),
    run_test(crypto_hash_md5_matches_md5_hash,
        (crypto_hash('MD5', hello, H1), md5_hash(hello, H2), H1 == H2)).

% ============================================================
% 5. HMAC
% ============================================================
test_hmac :-
    write('--- HMAC ---'), nl,
    run_test(hmac_sha256_returns_atom,
        (hmac('SHA256', mysecret, mydata, Mac), atom(Mac))),
    run_test(hmac_deterministic,
        (hmac('SHA256', key, data, M1), hmac('SHA256', key, data, M2), M1 == M2)),
    run_test(hmac_different_keys,
        (hmac('SHA256', key1, data, M1), hmac('SHA256', key2, data, M2), M1 \== M2)),
    run_test(hmac_different_data,
        (hmac('SHA256', key, data1, M1), hmac('SHA256', key, data2, M2), M1 \== M2)).

% ============================================================
% 6. Base64
% ============================================================
test_base64 :-
    write('--- Base64 ---'), nl,
    run_test(base64_encode_returns_atom,
        (base64_encode(hello, E), atom(E))),
    run_test(base64_known_value,
        (base64_encode(hello, E), E == 'aGVsbG8=')),
    run_test(base64_roundtrip,
        (base64_encode(hello, E), base64_decode(E, D), D == hello)),
    run_test(base64_roundtrip_longer,
        (base64_encode('Hello World!', E), base64_decode(E, D), D == 'Hello World!')),
    run_test(base64_decode_known,
        (base64_decode('aGVsbG8=', D), D == hello)).

% ============================================================
% 7. UUID
% ============================================================
test_uuid :-
    write('--- UUID ---'), nl,
    run_test(uuid_generates_atom,
        (uuid(U), atom(U))),
    run_test(uuid_length_36,
        (uuid(U), atom_length(U, 36))),
    run_test(uuid_unique,
        (uuid(U1), uuid(U2), U1 \== U2)).

% ============================================================
% 8. Random Token
% ============================================================
test_random_token :-
    write('--- Random Token ---'), nl,
    run_test(random_token_16_length_32,
        (random_token(16, T), atom(T), atom_length(T, 32))),
    run_test(random_token_8_length_16,
        (random_token(8, T), atom(T), atom_length(T, 16))),
    run_test(random_token_unique,
        (random_token(16, T1), random_token(16, T2), T1 \== T2)).

% ============================================================
% 9. Crypto Random Int
% ============================================================
test_crypto_random_int :-
    write('--- Crypto Random Int ---'), nl,
    run_test(crypto_random_int_range,
        (crypto_random_int(1, 100, N), N >= 1, N < 100)),
    run_test(crypto_random_int_small_range,
        (crypto_random_int(0, 2, N), (N == 0 ; N == 1))),
    run_test(crypto_random_int_negative,
        (crypto_random_int(-10, 10, N), N >= -10, N < 10)).

:- run_all_tests.
