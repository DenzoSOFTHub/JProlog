% DCG Test 03: Parsing Arithmetic Expressions
% Tests: Complex DCG grammar for mathematical expressions

% Numbers
digit(D) --> [C], { C >= 48, C =< 57, D is C - 48 }.
number(N) --> digits(Ds), { number_codes(N, Ds) }.

digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).

% Basic operators
plus --> [43].      % '+'
minus --> [45].     % '-'
times --> [42].     % '*'
divide --> [47].    % '/'

% Simple arithmetic expressions (no precedence)
expr(N) --> number(N).
expr(add(L,R)) --> expr(L), plus, expr(R).
expr(sub(L,R)) --> expr(L), minus, expr(R).
expr(mul(L,R)) --> expr(L), times, expr(R).
expr(div(L,R)) --> expr(L), divide, expr(R).

% Evaluate expression
eval(N, N) :- number(N).
eval(add(L,R), Result) :- eval(L, LVal), eval(R, RVal), Result is LVal + RVal.
eval(sub(L,R), Result) :- eval(L, LVal), eval(R, RVal), Result is LVal - RVal.
eval(mul(L,R), Result) :- eval(L, LVal), eval(R, RVal), Result is LVal * RVal.
eval(div(L,R), Result) :- eval(L, LVal), eval(R, RVal), Result is LVal / RVal.

% Test queries:
% ?- phrase(number(N), [53,49]).         % Expected: N = 51
% ?- phrase(expr(E), [53,43,49]).        % Expected: E = add(5,1)
% ?- phrase(expr(E), [53,43,49]), eval(E, R).  % Expected: E = add(5,1), R = 6