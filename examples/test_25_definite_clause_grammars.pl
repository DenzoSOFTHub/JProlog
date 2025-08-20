% ===================================================================
% TEST 25: Advanced Definite Clause Grammars (DCG)
% ===================================================================
% Tests: Complex DCG patterns, pushback, difference lists, grammar composition

% Advanced expression parser with precedence
expr(E) --> term(T), expr_rest(T, E).

expr_rest(T, E) --> [+], term(T1), { E1 is T + T1 }, expr_rest(E1, E).
expr_rest(T, E) --> [-], term(T1), { E1 is T - T1 }, expr_rest(E1, E).
expr_rest(E, E) --> [].

term(T) --> factor(F), term_rest(F, T).

term_rest(F, T) --> [*], factor(F1), { T1 is F * F1 }, term_rest(T1, T).
term_rest(F, T) --> [/], factor(F1), { T1 is F / F1 }, term_rest(T1, T).
term_rest(T, T) --> [].

factor(F) --> number(F).
factor(F) --> ['('], expr(F), [')'].

% Number parsing with multiple digits
number(N) --> digit(D), number_rest(D, N).

number_rest(Acc, N) --> digit(D), { Acc1 is Acc * 10 + D }, number_rest(Acc1, N).
number_rest(N, N) --> [].

digit(0) --> ['0'].
digit(1) --> ['1'].
digit(2) --> ['2'].
digit(3) --> ['3'].
digit(4) --> ['4'].
digit(5) --> ['5'].
digit(6) --> ['6'].
digit(7) --> ['7'].
digit(8) --> ['8'].
digit(9) --> ['9'].

% Context-sensitive grammar with conditions
conditional_rule(Result) --> 
    condition(C), 
    { C = true -> Action = accept ; Action = reject },
    action(Action, Result).

condition(true) --> [valid].
condition(false) --> [invalid].

action(accept, success) --> [process].
action(reject, failure) --> [ignore].

% Grammar with semantic actions and symbol table
declaration(var(Name, Type)) -->
    [var], identifier(Name), [':'], type_spec(Type), [';'],
    { assert(declared(Name, Type)) }.

identifier(Name) --> [Name], { atom(Name) }.

type_spec(integer) --> [int].
type_spec(boolean) --> [bool].
type_spec(real) --> [float].

% Pushback and lookahead
lookahead_test --> peek(X), { X = expected }, consume_expected.

peek(X), [X] --> [X].

consume_expected --> [expected].

% Test queries:
% ?- phrase(expr(Result), ['(', '2', '+', '3', ')', '*', '4']).
% ?- phrase(conditional_rule(R), [valid, process]).
% ?- phrase(declaration(Decl), [var, x, ':', int, ';']).