/* ============================================
   DCG per espressioni aritmetiche (ISO-Prolog)
   VERSIONE CORRETTA - Evita conflitti con built-in
   ============================================ */

:- module(dcg_calc, [parse_expr/2, eval/2, calc/2]).

/* ---------- API ---------- */

% parse_expr(+CodesOrAtom, -AST)
%   Esempio: ?- parse_expr("1 + 2*3", AST).
parse_expr(Input, AST) :-
    to_codes(Input, Codes),
    phrase((ws0, expr(AST), ws0), Codes).

% eval(+AST, -Value) - Proper evaluation with specific patterns
%   Esempio: ?- parse_expr("1+2*3", AST), eval(AST, V).
% Compound expressions - these must come first to avoid catch-all
eval(plus(A,B),  V)     :- eval(A, VA), eval(B, VB), V is VA + VB.
eval(minus(A,B), V)     :- eval(A, VA), eval(B, VB), V is VA - VB.
eval(times(A,B), V)     :- eval(A, VA), eval(B, VB), V is VA * VB.
eval(div(A,B),   V)     :- eval(A, VA), eval(B, VB), V is VA / VB.
eval(neg(A),     V)     :- eval(A, VA), V is -VA.
% Numbers evaluate to themselves - this must be last
eval(N, N).

% calc(+CodesOrAtom, -Value)  -- comodità: parse + eval in un colpo
%   Esempio: ?- calc("(1+2)*(-3+5)/2", V).
calc(Input, Value) :-
    parse_expr(Input, AST),
    eval(AST, Value).

/* ---------- DCG: grammatica ---------- */

expr(T)         --> term(T0), expr_tail(T0, T).
expr_tail(Acc,T)--> tok("+"), term(T1), {Acc1 = plus(Acc,T1)},  expr_tail(Acc1,T).
expr_tail(Acc,T)--> tok("-"), term(T1), {Acc1 = minus(Acc,T1)}, expr_tail(Acc1,T).
expr_tail(Acc,Acc) --> [].

term(T)         --> factor(F0), term_tail(F0, T).
term_tail(Acc,T)--> tok("*"), factor(F1), {Acc1 = times(Acc,F1)}, term_tail(Acc1,T).
term_tail(Acc,T)--> tok("/"), factor(F1), {Acc1 = div(Acc,F1)},   term_tail(Acc1,T).
term_tail(Acc,Acc) --> [].

factor(N)       --> num(N).  % Rinominato da 'number' a 'num' per evitare conflitti
factor(E)       --> tok("("), expr(E), tok(")").
factor(neg(F))  --> tok("-"), factor(F).   % meno unario

/* ---------- DCG: lessico ---------- */

% num//1: interi (>= 0) - Rinominato da 'number' a 'num'
num(N) --> ws0, digits(Ds), ws0, { Ds \= [], number_codes(N, Ds) }.

digits([D|Ds]) --> digit(D), digits_rest(Ds).
digits_rest([D|Ds]) --> digit(D), !, digits_rest(Ds).
digits_rest([]) --> [].

digit(D) --> [D], { D >= 0'0, D =< 0'9 }.

% spazi bianchi (zero o più)
ws0 --> [C], { is_space(C) }, !, ws0.
ws0 --> [].

is_space(9).    % \t
is_space(10).   % \n
is_space(11).   % \v
is_space(12).   % \f
is_space(13).   % \r
is_space(32).   % ' '

% token simbolico con spazi attorno (parametro è una lista di codici)
tok(S) --> ws0, S, ws0.

/* ---------- utilità ---------- */

to_codes(Codes, Codes) :- is_list(Codes), !.
to_codes(Atom, Codes)  :- atom(Atom), !, atom_codes(Atom, Codes).

/* ---------- esempi ----------
?- parse_expr("1 + 2*3 - 4", AST).
AST = minus(plus(1, times(2, 3)), 4).

?- calc("(1+2)*(-3+5)/2", V).
V = 3.0.

?- phrase((ws0, expr(AST), ws0), " -3 * (2 + 1) ").
AST = times(neg(3), plus(2, 1)).
-------------------------------- */