% DCG Test 20: Compiler Frontend (Scanner + Parser)
% Tests: Complete language parsing with semantic actions

% Lexical tokens
token_ws --> [32].  % space
token_ws --> [9].   % tab
token_ws --> [10].  % newline

skip_ws --> [].
skip_ws --> token_ws, skip_ws.

% Keywords
keyword(if) --> [105,102], \+ identifier_char.
keyword(then) --> [116,104,101,110], \+ identifier_char.
keyword(else) --> [101,108,115,101], \+ identifier_char.
keyword(while) --> [119,104,105,108,101], \+ identifier_char.
keyword(var) --> [118,97,114], \+ identifier_char.
keyword(function) --> [102,117,110,99,116,105,111,110], \+ identifier_char.
keyword(return) --> [114,101,116,117,114,110], \+ identifier_char.

% Identifiers
identifier(id(Name)) --> identifier_start(C), identifier_rest(Cs),
    { append([C], Cs, Name) },
    { \+ is_keyword(Name) }.

identifier_start(C) --> [C], { (C >= 97, C =< 122); (C >= 65, C =< 90); C = 95 }.
identifier_char --> [C], { (C >= 97, C =< 122); (C >= 65, C =< 90); (C >= 48, C =< 57); C = 95 }.

identifier_rest([]) --> [].
identifier_rest([C|Cs]) --> identifier_char, identifier_rest(Cs).

is_keyword([105,102]).              % if
is_keyword([116,104,101,110]).      % then
is_keyword([101,108,115,101]).      % else
is_keyword([119,104,105,108,101]).  % while

% Numbers
number(num(N)) --> digits(Ds), { number_codes(N, Ds) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [C], { C >= 48, C =< 57, D = C }.

% Operators and punctuation
operator(assign) --> [61].         % =
operator(plus) --> [43].           % +
operator(minus) --> [45].          % -
operator(times) --> [42].          % *
operator(divide) --> [47].         % /
operator(equals) --> [61,61].      % ==
operator(less) --> [60].           % <
operator(greater) --> [62].        % >

punctuation(semicolon) --> [59].   % ;
punctuation(comma) --> [44].       % ,
punctuation(lparen) --> [40].      % (
punctuation(rparen) --> [41].      % )
punctuation(lbrace) --> [123].     % {
punctuation(rbrace) --> [125].     % }

% Expressions
expression(E) --> skip_ws, expr_or(E), skip_ws.

expr_or(or(L,R)) --> expr_and(L), skip_ws, [124,124], skip_ws, expr_or(R).  % ||
expr_or(E) --> expr_and(E).

expr_and(and(L,R)) --> expr_eq(L), skip_ws, [38,38], skip_ws, expr_and(R).  % &&
expr_and(E) --> expr_eq(E).

expr_eq(eq(L,R)) --> expr_rel(L), skip_ws, operator(equals), skip_ws, expr_eq(R).
expr_eq(E) --> expr_rel(E).

expr_rel(lt(L,R)) --> expr_add(L), skip_ws, operator(less), skip_ws, expr_rel(R).
expr_rel(gt(L,R)) --> expr_add(L), skip_ws, operator(greater), skip_ws, expr_rel(R).
expr_rel(E) --> expr_add(E).

expr_add(add(L,R)) --> expr_mul(L), skip_ws, operator(plus), skip_ws, expr_add(R).
expr_add(sub(L,R)) --> expr_mul(L), skip_ws, operator(minus), skip_ws, expr_add(R).
expr_add(E) --> expr_mul(E).

expr_mul(mul(L,R)) --> expr_primary(L), skip_ws, operator(times), skip_ws, expr_mul(R).
expr_mul(div(L,R)) --> expr_primary(L), skip_ws, operator(divide), skip_ws, expr_mul(R).
expr_mul(E) --> expr_primary(E).

expr_primary(E) --> number(E).
expr_primary(E) --> identifier(E).
expr_primary(E) --> punctuation(lparen), skip_ws, expression(E), skip_ws, punctuation(rparen).

% Statements
statement(S) --> skip_ws, stmt(S), skip_ws.

stmt(assign(Id, Expr)) --> 
    identifier(Id), skip_ws, operator(assign), skip_ws, 
    expression(Expr), skip_ws, punctuation(semicolon).

stmt(if(Cond, Then, Else)) -->
    keyword(if), skip_ws, punctuation(lparen), skip_ws,
    expression(Cond), skip_ws, punctuation(rparen), skip_ws,
    statement(Then), skip_ws,
    keyword(else), skip_ws,
    statement(Else).

stmt(while(Cond, Body)) -->
    keyword(while), skip_ws, punctuation(lparen), skip_ws,
    expression(Cond), skip_ws, punctuation(rparen), skip_ws,
    statement(Body).

stmt(block(Stmts)) -->
    punctuation(lbrace), skip_ws,
    statements(Stmts), skip_ws,
    punctuation(rbrace).

stmt(return(Expr)) -->
    keyword(return), skip_ws,
    expression(Expr), skip_ws,
    punctuation(semicolon).

% Statement list
statements([]) --> [].
statements([S|Ss]) --> statement(S), skip_ws, statements(Ss).

% Program
program(program(Stmts)) --> skip_ws, statements(Stmts), skip_ws.

% Test queries:
% ?- phrase(identifier(Id), [120,121,122]).                 % Expected: Id = id([120,121,122])
% ?- phrase(number(N), [49,50,51]).                         % Expected: N = num(123)
% ?- phrase(expression(E), [120,43,49]).                    % Expected: E = add(id([120]),num(1))
% ?- phrase(stmt(S), [120,61,49,59]).                      % Expected: S = assign(id([120]),num(1))