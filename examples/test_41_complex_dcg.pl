% ===================================================================
% TEST 41: Complex DCG (Definite Clause Grammar) Examples
% ===================================================================
% Advanced DCG patterns for parsing complex structures

% Example 1: Mathematical Expression Parser
% Handles arithmetic expressions with precedence: +, -, *, /, parentheses
expression(Result) --> term(Left), expression_rest(Left, Result).

expression_rest(Left, Result) --> 
    [+], term(Right), 
    { Result1 is Left + Right },
    expression_rest(Result1, Result).
expression_rest(Left, Result) --> 
    [-], term(Right),
    { Result1 is Left - Right }, 
    expression_rest(Result1, Result).
expression_rest(Result, Result) --> [].

term(Result) --> factor(Left), term_rest(Left, Result).

term_rest(Left, Result) --> 
    [*], factor(Right),
    { Result1 is Left * Right },
    term_rest(Result1, Result).
term_rest(Left, Result) --> 
    [/], factor(Right), 
    { Result1 is Left / Right },
    term_rest(Result1, Result).
term_rest(Result, Result) --> [].

factor(Result) --> [N], { number(N), Result = N }.
factor(Result) --> ['('], expression(Result), [')'].

% Test: phrase(expression(X), [2, +, 3, *, 4]).
% Expected: X = 14

% Example 2: Simple English Sentence Parser
% Parses: "The cat sits on the mat"
sentence --> noun_phrase, verb_phrase.

noun_phrase --> determiner, noun.
noun_phrase --> noun.

verb_phrase --> verb.
verb_phrase --> verb, prepositional_phrase.

prepositional_phrase --> preposition, noun_phrase.

determiner --> [the].
determiner --> [a].
determiner --> [an].

noun --> [cat].
noun --> [dog].
noun --> [mat].
noun --> [table].

verb --> [sits].
verb --> [runs].
verb --> [sleeps].

preposition --> [on].
preposition --> [under].
preposition --> [near].

% Test: phrase(sentence, [the, cat, sits, on, the, mat]).
% Expected: true

% Example 3: JSON-like Structure Parser  
% Parses simple key-value structures: {key: value, key2: value2}
json_object(Object) --> ['{'], key_value_pairs(Pairs), ['}'], { Object = object(Pairs) }.

key_value_pairs([Pair|Rest]) --> key_value_pair(Pair), kvp_rest(Rest).
key_value_pairs([]) --> [].

kvp_rest([Pair|Rest]) --> [comma], key_value_pair(Pair), kvp_rest(Rest).
kvp_rest([]) --> [].

key_value_pair(pair(Key, Value)) --> atom_token(Key), [':'], value_token(Value).

atom_token(Atom) --> [Token], { atom(Token), Atom = Token }.
value_token(Value) --> [Token], { (atom(Token) ; number(Token)), Value = Token }.

% Test: phrase(json_object(X), ['{', name, ':', john, comma, age, ':', 25, '}']).
% Expected: X = object([pair(name, john), pair(age, 25)])

% Example 4: XML-like Tag Parser
% Parses nested tags: <tag>content</tag>
xml_element(element(Tag, Content)) --> 
    start_tag(Tag), 
    xml_content(Content), 
    end_tag(Tag).

start_tag(Tag) --> ['<'], [Tag], ['>'], { atom(Tag) }.
end_tag(Tag) --> ['<'], ['/'], [Tag], ['>'], { atom(Tag) }.

xml_content([]) --> [].
xml_content([Element|Rest]) --> xml_element(Element), xml_content(Rest).
xml_content([text(Text)|Rest]) --> [Text], { atom(Text), \\+ (Text = '<'), \\+ (Text = '>') }, xml_content(Rest).

% Test: phrase(xml_element(X), ['<', html, '>', '<', body, '>', content, '<', '/', body, '>', '<', '/', html, '>']).
% Expected: X = element(html, [element(body, [text(content)])])

% Example 5: Prolog Clause Parser
% Parses simple Prolog facts and rules
prolog_clause(fact(Head)) --> term_dcg(Head), ['.'].
prolog_clause(rule(Head, Body)) --> term_dcg(Head), [':-'], body_dcg(Body), ['.'].

term_dcg(Term) --> [Functor], ['('], arg_list(Args), [')'], { Term =.. [Functor|Args] }.
term_dcg(Atom) --> [Atom], { atom(Atom) }.

arg_list([Arg|Rest]) --> term_dcg(Arg), arg_rest(Rest).
arg_list([]) --> [].

arg_rest([Arg|Rest]) --> [','], term_dcg(Arg), arg_rest(Rest).
arg_rest([]) --> [].

body_dcg(Goal) --> term_dcg(Goal).
body_dcg(and(Left, Right)) --> term_dcg(Left), [','], body_dcg(Right).

% Test: phrase(prolog_clause(X), [parent, '(', tom, ',', bob, ')', '.']).
% Expected: X = fact(parent(tom, bob))

% Utility predicates for testing
test_math_expression :-
    phrase(expression(Result), [2, +, 3, *, 4]),
    write('Math result: '), write(Result), nl.

test_english_sentence :-
    phrase(sentence, [the, cat, sits, on, the, mat]),
    write('Sentence parsed successfully'), nl.

test_json_parsing :-
    phrase(json_object(Object), ['{', name, ':', john, comma, age, ':', 25, '}']),
    write('JSON object: '), write(Object), nl.

test_xml_parsing :-
    phrase(xml_element(XML), ['<', div, '>', hello, '<', '/', div, '>']),
    write('XML element: '), write(XML), nl.

test_prolog_parsing :-
    phrase(prolog_clause(Clause), [likes, '(', mary, ',', wine, ')', '.']),
    write('Prolog clause: '), write(Clause), nl.

% Run all DCG tests
run_all_dcg_tests :-
    write('=== Complex DCG Tests ==='), nl,
    test_math_expression,
    test_english_sentence, 
    test_json_parsing,
    test_xml_parsing,
    test_prolog_parsing,
    write('All DCG tests completed'), nl.