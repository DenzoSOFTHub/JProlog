% ===================================================================
% TEST 41: Complex DCG Examples (JProlog Compatible)
% ===================================================================
% Advanced DCG patterns simplified for JProlog parser

% Example 1: Simple Arithmetic Expression Parser
% Handles basic expressions: number + number, number - number
simple_expr(Result) --> 
    number_token(Left), 
    [+], 
    number_token(Right),
    { Result is Left + Right }.
simple_expr(Result) --> 
    number_token(Left), 
    [-], 
    number_token(Right),
    { Result is Left - Right }.
simple_expr(Result) --> number_token(Result).

number_token(N) --> [N], { number(N) }.

% Test: phrase(simple_expr(X), [5, +, 3]).
% Expected: X = 8

% Example 2: Basic English Grammar
% Simple sentence structure: determiner noun verb
basic_sentence --> determiner_word, noun_word, verb_word.

determiner_word --> [the].
determiner_word --> [a].
noun_word --> [cat].
noun_word --> [dog].
verb_word --> [runs].
verb_word --> [sits].

% Test: phrase(basic_sentence, [the, cat, runs]).
% Expected: true

% Example 3: List Pattern Parser (Fixed for parser compatibility)
% Parses patterns like: [a, b, c] or [x | xs]
list_pattern(list_elements(Elements)) --> [lbracket], element_list(Elements), [rbracket].
list_pattern(list_head_tail(Head, Tail)) --> [lbracket], element_token(Head), [pipe], element_token(Tail), [rbracket].

element_list([Element|Rest]) --> element_token(Element), element_rest(Rest).
element_list([]) --> [].

element_rest([Element|Rest]) --> [comma], element_token(Element), element_rest(Rest).
element_rest([]) --> [].

element_token(Token) --> [Token], { (atom(Token) ; number(Token)) }.

% Test: phrase(list_pattern(P), [lbracket, a, comma, b, comma, c, rbracket]).
% Expected: P = list_elements([a, b, c])

% Example 4: Simple Command Parser
% Parses commands like: move left, turn right, stop
command(move(Direction)) --> [move], direction(Direction).
command(turn(Direction)) --> [turn], direction(Direction).
command(stop) --> [stop].

direction(left) --> [left].
direction(right) --> [right].
direction(up) --> [up].
direction(down) --> [down].

% Test: phrase(command(C), [move, left]).
% Expected: C = move(left)

% Example 5: Variable Assignment Parser  
% Parses: var = value
assignment(assign(Var, Value)) --> variable_name(Var), [equals], value_expr(Value).

variable_name(Var) --> [Var], { atom(Var) }.
value_expr(number(N)) --> [N], { number(N) }.
value_expr(atom(A)) --> [A], { atom(A) }.

% Test: phrase(assignment(A), [x, equals, 42]).
% Expected: A = assign(x, number(42))

% Testing predicates
test_simple_expr :-
    phrase(simple_expr(Result), [5, +, 3]),
    write('Expression result: '), write(Result), nl.

test_basic_sentence :-
    phrase(basic_sentence, [the, cat, runs]),
    write('Sentence parsed successfully'), nl.

test_list_pattern :-
    phrase(list_pattern(Pattern), ['[', a, ',', b, ',', c, ']']),
    write('List pattern: '), write(Pattern), nl.

test_command :-
    phrase(command(Cmd), [move, left]),
    write('Command parsed: '), write(Cmd), nl.

test_assignment :-
    phrase(assignment(Assign), [x, '=', 42]),
    write('Assignment parsed: '), write(Assign), nl.

% Run all DCG tests
run_dcg_tests :-
    write('=== DCG Tests ==='), nl,
    test_simple_expr,
    test_basic_sentence,
    test_list_pattern,
    test_command,
    test_assignment,
    write('DCG tests completed'), nl.