% DCG Test 15: Lexical Analyzer (Tokenizer)
% Tests: Token recognition, lexical categories

% Token types
token(number(N), Chars) --> number_token(Ds), { number_codes(N, Ds), append(Ds, Rest, Chars), Rest = [] }.
token(identifier(Id), Chars) --> identifier_token(Cs), { append(Cs, Rest, Chars), Rest = [] }.
token(operator(Op), Chars) --> operator_token(Op), { append(Op, Rest, Chars), Rest = [] }.
token(keyword(Kw), Chars) --> keyword_token(Kw), { append(Kw, Rest, Chars), Rest = [] }.
token(punctuation(P), Chars) --> punctuation_token(P), { append(P, Rest, Chars), Rest = [] }.

% Number tokens
number_token([D|Ds]) --> digit(D), number_token(Ds).
number_token([D]) --> digit(D).

digit(D) --> [C], { C >= 48, C =< 57, D = C }.

% Identifier tokens
identifier_token([C|Cs]) --> alpha(C), identifier_rest(Cs).

alpha(C) --> [C], { (C >= 97, C =< 122); (C >= 65, C =< 90) }.

identifier_rest([]) --> [].
identifier_rest([C|Cs]) --> alpha(C), identifier_rest(Cs).
identifier_rest([C|Cs]) --> digit(C), identifier_rest(Cs).
identifier_rest([95|Cs]) --> [95], identifier_rest(Cs).  % underscore

% Operator tokens
operator_token([43]) --> [43].      % +
operator_token([45]) --> [45].      % -
operator_token([42]) --> [42].      % *
operator_token([47]) --> [47].      % /
operator_token([61]) --> [61].      % =
operator_token([60]) --> [60].      % <
operator_token([62]) --> [62].      % >
operator_token([60,61]) --> [60,61]. % <=
operator_token([62,61]) --> [62,61]. % >=
operator_token([61,61]) --> [61,61]. % ==

% Keyword tokens (predefined identifiers)
keyword_token([105,102]) --> [105,102], { \+ alpha(_) }.           % if
keyword_token([116,104,101,110]) --> [116,104,101,110], { \+ alpha(_) }. % then
keyword_token([101,108,115,101]) --> [101,108,115,101], { \+ alpha(_) }. % else
keyword_token([119,104,105,108,101]) --> [119,104,105,108,101], { \+ alpha(_) }. % while

% Punctuation tokens
punctuation_token([40]) --> [40].   % (
punctuation_token([41]) --> [41].   % )
punctuation_token([123]) --> [123]. % {
punctuation_token([125]) --> [125]. % }
punctuation_token([59]) --> [59].   % ;
punctuation_token([44]) --> [44].   % ,

% Whitespace (ignored)
whitespace --> [32].  % space
whitespace --> [9].   % tab
whitespace --> [10].  % newline
whitespace --> [13].  % carriage return

skip_whitespace --> [].
skip_whitespace --> whitespace, skip_whitespace.

% Tokenize input
tokenize([]) --> skip_whitespace.
tokenize([Token|Tokens]) --> 
    skip_whitespace,
    token(Token, _),
    skip_whitespace,
    tokenize(Tokens).

% Helper for single character tokens
single_char_token(Type, Char) -->
    [Char],
    { token_type(Type, [Char]) }.

% Token classification
token_type(number, Chars) :- forall(member(C, Chars), (C >= 48, C =< 57)).
token_type(identifier, [C|_]) :- (C >= 97, C =< 122); (C >= 65, C =< 90).

% Test queries:
% ?- phrase(number_token(N), [49,50,51]).          % Expected: N = [49,50,51]
% ?- phrase(identifier_token(Id), [104,101,108,108,111]). % Expected: Id = [104,101,108,108,111]
% ?- phrase(tokenize(Tokens), [49,50,43,120]).     % Expected: Tokens = [number(12),operator([43]),identifier([120])]