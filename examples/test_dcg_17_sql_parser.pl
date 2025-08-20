% DCG Test 17: SQL Parser (Simplified)
% Tests: SQL statement parsing, keywords, expressions

% Whitespace
ws --> [].
ws --> [32], ws.  % space
ws --> [9], ws.   % tab

% SQL Keywords (case-insensitive simplified)
select_kw --> [115,101,108,101,99,116].     % SELECT
from_kw --> [102,114,111,109].              % FROM  
where_kw --> [119,104,101,114,101].         % WHERE
and_kw --> [97,110,100].                    % AND
or_kw --> [111,114].                        % OR
order_kw --> [111,114,100,101,114].         % ORDER
by_kw --> [98,121].                         % BY

% Identifiers (table names, column names)
identifier([C|Cs]) --> alpha(C), identifier_rest(Cs).
identifier([C]) --> alpha(C).

identifier_rest([]) --> [].
identifier_rest([C|Cs]) --> alpha(C), identifier_rest(Cs).
identifier_rest([C|Cs]) --> digit(C), identifier_rest(Cs).
identifier_rest([95|Cs]) --> [95], identifier_rest(Cs).  % underscore

alpha(C) --> [C], { (C >= 97, C =< 122); (C >= 65, C =< 90) }.
digit(C) --> [C], { C >= 48, C =< 57 }.

% Numbers
number(N) --> digits(Ds), { number_codes(N, Ds) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).

% String literals
string_literal(Chars) --> [39], string_chars(Chars), [39].  % 'string'

string_chars([]) --> [].
string_chars([C|Cs]) --> [C], { C \= 39 }, string_chars(Cs).

% Operators
equals --> [61].
less_than --> [60].
greater_than --> [62].
comma --> [44].

% Expressions
expression(column(Name)) --> identifier(Name).
expression(number(N)) --> number(N).
expression(string(S)) --> string_literal(S).

% Comparison expressions
comparison(eq(L,R)) --> expression(L), ws, equals, ws, expression(R).
comparison(lt(L,R)) --> expression(L), ws, less_than, ws, expression(R).
comparison(gt(L,R)) --> expression(L), ws, greater_than, ws, expression(R).

% WHERE conditions
condition(C) --> comparison(C).
condition(and(L,R)) --> condition(L), ws, and_kw, ws, condition(R).
condition(or(L,R)) --> condition(L), ws, or_kw, ws, condition(R).

% Column lists
column_list([Col]) --> identifier(Col).
column_list([Col|Cols]) --> identifier(Col), ws, comma, ws, column_list(Cols).

% SELECT statement
select_stmt(select(Columns, Table)) -->
    select_kw, ws, column_list(Columns), ws,
    from_kw, ws, identifier(Table).

select_stmt(select(Columns, Table, Condition)) -->
    select_kw, ws, column_list(Columns), ws,
    from_kw, ws, identifier(Table), ws,
    where_kw, ws, condition(Condition).

% ORDER BY clause
order_by(order_by(Columns)) -->
    order_kw, ws, by_kw, ws, column_list(Columns).

select_with_order(select_order(Select, Order)) -->
    select_stmt(Select), ws, order_by(Order).

% SQL statement
sql_statement(S) --> select_stmt(S).
sql_statement(S) --> select_with_order(S).

% Test queries:
% ?- phrase(identifier(I), [117,115,101,114,115]).            % Expected: I = [117,115,101,114,115] ("users")  
% ?- phrase(select_stmt(S), [115,101,108,101,99,116,32,110,97,109,101,32,102,114,111,109,32,117,115,101,114,115]). % SELECT name FROM users
% ?- phrase(comparison(C), [97,103,101,61,50,53]).            % Expected: C = eq(column([97,103,101]),number(25))