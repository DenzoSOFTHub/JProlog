% test_refactor_issues.pl
%
% Each query below demonstrates a behavioral defect that the corresponding
% refactor (R1..R10 in CHANGELOG / track-issues.md) is intended to correct.
% Run manually in the JProlog CLI / IDE — comments document the expected
% result that should hold once the refactor is implemented.
%
% Usage:
%   :consult examples/test_refactor_issues.pl
%   ?- r1_b_setval_backtrack(V).      % expect V = 1
%   ?- r2_module_local_op.            % expect existence_error or "no"
%   etc.

%% ============================================================
%% R1 — Trail engine (b_setval backtrackable, op/3 undo, setarg)
%% ============================================================

% Expected: V = 1   (b_setval inside fail-branch is rolled back)
% Current : V = 2   (no trail; assignment persists)
r1_b_setval_backtrack(V) :-
    b_setval(x, 1),
    (b_setval(x, 2), fail ; true),
    b_getval(x, V).

% Expected: false  (op redefinition undone after fail)
% Current : true   (operator persists globally after fail)
r1_op_undo :-
    (op(700, xfx, myundoop), fail ; true),
    current_op(_, _, myundoop).

% Expected: X = x  (destructive 2nd-arg replacement)
% Current : not implemented; fails with existence_error
r1_setarg(X) :-
    T = f(a, b, c),
    setarg(2, T, x),
    arg(2, T, X).

%% ============================================================
%% R2 — Module-local operators
%% ============================================================

% Expected: existence_error or no solution
% Current : succeeds (operator leaks across modules)
r2_module_local_op :-
    op(800, xfx, leakop),  % defined "globally" in JProlog today
    current_op(_, _, leakop).

% Expected: secret:hidden(_) is unreachable from default module
% Current : succeeds (auto-export of all when [] given)
r2_empty_export_hides_all :-
    \+ catch(secret:hidden(_), _, fail).

%% ============================================================
%% R3 — Stream encoding + EOF action + binary type
%% ============================================================

% Expected: succeeds with C = 'c' (first char of UTF-8 "café")
% Current : open/4 options ignored; encoding may misinterpret
r3_open_utf8(C) :-
    open('utf8_test.txt', read, S, [encoding(utf8)]),
    get_char(S, C),
    close(S).

% Expected: throws permission_error/past_end_of_stream on 2nd EOF read
% Current : returns end_of_file silently
r3_eof_action_error :-
    open('one_byte.txt', read, S, [eof_action(error)]),
    get_char(S, _),
    catch(get_char(S, _), _, (close(S), !, fail)),
    close(S).

% Expected: B = 255 (read 0xFF byte from binary stream)
% Current : type(binary) ignored; may decode as char
r3_binary_byte(B) :-
    open('bin.bin', read, S, [type(binary)]),
    get_byte(S, B),
    close(S).

%% ============================================================
%% R4 — Format column tabbing + portray hook
%% ============================================================

% Expected: "        hi" (right-aligned at column 10)
% Current : prints raw ~t~w~10| with no column control
r4_column_tab :-
    format('~t~w~10|', [hi]), nl.

% Expected: "a    b" (col 0..5 padded with spaces)
% Current : not implemented
r4_relative_tab :-
    format('~w~t~5|~w', [a, b]), nl.

% Expected: "<3,4>" via portray hook
% Current : prints "point(3, 4)" (portray hook never called)
:- dynamic(portray/1).
portray(point(X, Y)) :- format('<~w,~w>', [X, Y]).

r4_portray :- format('~p', [point(3, 4)]), nl.

%% ============================================================
%% R5 — Tabling well-founded semantics
%% ============================================================

% Expected: terminates with 1 solution (path(a, d))
% Current : may stack overflow on left-recursion under tabling
:- table path/2.
edge(a, b). edge(b, c). edge(c, d).
path(X, Y) :- edge(X, Y).
path(X, Y) :- path(X, Z), edge(Z, Y).

r5_left_recursion :- path(a, d).

% Expected: terminates (undefined or fail under WFS)
% Current : infinite recursion
:- table neg/1.
neg(X) :- \+ neg(X).

r5_negation_terminates :-
    catch(call_with_inference_limit(neg(a), 10000, _),
          _, true).

%% ============================================================
%% Coroutining — freeze hook firing, when re-suspension
%% (covered by R1 fixes + further integration work)
%% ============================================================

% Expected: triggered = stored as fact after X = 1
% Current : hook does not fire on =/2 reliably
:- dynamic(probe/1).

c_freeze_fires :-
    retractall(probe(_)),
    freeze(X, assertz(probe(triggered))),
    X = 1,
    probe(triggered).

% Expected: probe(fired) only after BOTH X and Y bound
% Current : when may fire too early or never
c_when_resuspends :-
    retractall(probe(_)),
    when(ground(f(X, Y)), assertz(probe(fired))),
    X = 1,
    \+ probe(fired),    % must NOT have fired yet
    Y = 2,
    probe(fired).        % must fire now

%% ============================================================
%% R8 — ListTerm vs cons-cell representation
%% ============================================================

% Expected: == succeeds (single canonical representation)
% Current : may fail if parser produces ListTerm but rhs is cons-cells
r8_list_canonical :-
    X = [a, b, c],
    X == '.'(a, '.'(b, '.'(c, []))).
