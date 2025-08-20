% DCG Test 02: Variable Binding in DCG Rules
% Tests: Variable passing between DCG rules, parameter binding

% Rule with variable parameter
single(X) --> [X].

% Rule that captures and returns value
capture_first([H|_], H) --> [H].
capture_first([H|T], Result) --> [H], capture_first(T, Result).

% Rule with variable binding and arithmetic
digit(D) --> [C], { C >= 48, C =< 57, D is C - 48 }.

% Multiple digit number
digits([D]) --> digit(D).
digits([D|Ds]) --> digit(D), digits(Ds).

% Rule that builds a list
collect_as([]) --> [].
collect_as([a|As]) --> [a], collect_as(As).
collect_as([X|As]) --> [X], { X \= a }, collect_as(As).

% Test queries:
% ?- phrase(single(X), [hello]).         % Expected: X = hello
% ?- phrase(single(world), [world]).     % Expected: true
% ?- phrase(digit(D), [53]).             % Expected: D = 5
% ?- phrase(digits(Ds), [49,50,51]).     % Expected: Ds = [1,2,3]
% ?- phrase(collect_as(As), [a,b,a,c]).  % Expected: As = [a,b,a,c]