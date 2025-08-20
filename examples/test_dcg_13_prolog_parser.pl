% DCG Test 13: Simple Prolog Parser
% Tests: Parsing Prolog terms, clauses, rules

% Whitespace and comments
ws --> [].
ws --> [32], ws.  % space
ws --> [9], ws.   % tab
ws --> [10], ws.  % newline
ws --> [13], ws.  % carriage return

% Prolog identifiers
lower --> [C], { C >= 97, C =< 122 }.
upper --> [C], { C >= 65, C =< 90 }.
digit --> [C], { C >= 48, C =< 57 }.
underscore --> [95].

% Atoms (start with lowercase or quoted)
atom_name([C|Cs]) --> lower, identifier_rest(Cs).
atom_name(Chars) --> [39], quoted_atom(Chars), [39].  % 'quoted atom'

identifier_rest([]) --> [].
identifier_rest([C|Cs]) --> lower, identifier_rest(Cs).
identifier_rest([C|Cs]) --> upper, identifier_rest(Cs).
identifier_rest([C|Cs]) --> digit, identifier_rest(Cs).
identifier_rest([C|Cs]) --> underscore, identifier_rest(Cs).

quoted_atom([]) --> [].
quoted_atom([C|Cs]) --> [C], { C \= 39 }, quoted_atom(Cs).

% Variables (start with uppercase or underscore)
variable([C|Cs]) --> upper, identifier_rest(Cs).
variable([C|Cs]) --> underscore, identifier_rest(Cs).

% Numbers (simplified)
number(Ds) --> digits(Ds).
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).

% Terms
term(atom(Name)) --> atom_name(Name).
term(var(Name)) --> variable(Name).
term(number(Num)) --> number(Ds), { number_codes(Num, Ds) }.
term(compound(Functor, Args)) --> 
    atom_name(Functor), ws, [40], ws, arg_list(Args), ws, [41].

% Argument lists
arg_list([Arg]) --> term(Arg).
arg_list([Arg|Args]) --> term(Arg), ws, [44], ws, arg_list(Args).  % comma
arg_list([]) --> [].

% Clauses
fact(fact(Head)) --> term(Head), ws, [46].  % period

rule(rule(Head, Body)) --> 
    term(Head), ws, [58,45], ws, term(Body), ws, [46].  % :-

clause(C) --> fact(C).
clause(C) --> rule(C).

% Program (list of clauses)
program([]) --> [].
program([C|Cs]) --> ws, clause(C), ws, program(Cs).

% Test queries:
% ?- phrase(atom_name(A), [102,111,111]).           % Expected: A = [102,111,111] ("foo")
% ?- phrase(variable(V), [88]).                     % Expected: V = [88] ("X")  
% ?- phrase(term(T), [102,40,88,41]).               % Expected: T = compound([102,111,111],[var([88])])
% ?- phrase(fact(F), [102,111,111,46]).             % Expected: F = fact(atom([102,111,111]))