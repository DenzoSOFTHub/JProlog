% Test 34: Usability and New Feature Tests
% ISS-2025-0176 - Usability improvements

% === Test 1: Graph SCC ===
test_graph_scc :-
    Edges = [edge(a,b), edge(b,c), edge(c,a), edge(d,e), edge(e,d)],
    graph_scc(Edges, SCCs),
    length(SCCs, N),
    N >= 2.  % At least 2 SCCs: {a,b,c} and {d,e}

% === Test 2: Crypto hashing ===
test_crypto_hash :-
    crypto_hash(sha256, hello, Hash),
    atom_length(Hash, Len),
    Len > 0.

% === Test 3: Error context quality ===
test_error_context :-
    catch(
        (X is foo + 1, fail),
        error(type_error(evaluable, _), Context),
        (Context \== '', true)
    ).

% === Test 4: Number to atom conversion ===
test_number_atom :-
    number_codes(42, Codes),
    atom_codes(A, Codes),
    A = '42'.

% === Test 5: Atom manipulation ===
test_atom_concat :-
    atom_concat(hello, world, R),
    R = helloworld.

test_atom_length :-
    atom_length(hello, 5).

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Usability Tests ==='), nl.
:- run_test('Error context quality', test_error_context).
:- run_test('Number to atom', test_number_atom).
:- run_test('Atom concat', test_atom_concat).
:- run_test('Atom length', test_atom_length).
:- write('=== Usability Tests Complete ==='), nl.
