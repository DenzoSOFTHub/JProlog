% DCG Test 08: Calculator with Precedence
% Tests: Operator precedence, expression evaluation

% Whitespace handling
ws --> [].
ws --> [32], ws.  % space

% Numbers
digit(D) --> [C], { C >= 48, C =< 57, D is C - 48 }.
number(N) --> digits(Ds), { calculate_number(Ds, N) }.

digits([D]) --> digit(D).
digits([D|Ds]) --> digit(D), digits(Ds).

calculate_number([D], D).
calculate_number([D|Ds], N) :- 
    calculate_number(Ds, N1),
    N is D * 10 + N1.

% Operators
plus --> ws, [43], ws.      % +
minus --> ws, [45], ws.     % -
times --> ws, [42], ws.     % *
divide --> ws, [47], ws.    % /
lparen --> ws, [40], ws.    % (
rparen --> ws, [41], ws.    % )

% Expression grammar with precedence
expr(E) --> term(E).
expr(add(L,R)) --> expr(L), plus, term(R).
expr(sub(L,R)) --> expr(L), minus, term(R).

term(T) --> factor(T).
term(mul(L,R)) --> term(L), times, factor(R).
term(div(L,R)) --> term(L), divide, factor(R).

factor(N) --> number(N).
factor(E) --> lparen, expr(E), rparen.
factor(neg(E)) --> minus, factor(E).

% Evaluation
eval(N, N) :- number(N).
eval(add(L,R), Result) :- eval(L, LV), eval(R, RV), Result is LV + RV.
eval(sub(L,R), Result) :- eval(L, LV), eval(R, RV), Result is LV - RV.
eval(mul(L,R), Result) :- eval(L, LV), eval(R, RV), Result is LV * RV.
eval(div(L,R), Result) :- eval(L, LV), eval(R, RV), Result is LV / RV.
eval(neg(E), Result) :- eval(E, V), Result is -V.

% Calculate expression from string
calculate(String, Result) :-
    phrase(expr(E), String),
    eval(E, Result).

% Test queries:
% ?- phrase(expr(E), [50,43,51]).                   % Expected: E = add(2,3)
% ?- phrase(expr(E), [50,42,51,43,52]), eval(E,R). % Expected: E = add(mul(2,3),4), R = 10
% ?- calculate([40,50,43,51,41,42,52], R).         % Expected: R = 20