% ===================================================================
% TEST 14: Simple DCG (Definite Clause Grammars)
% ===================================================================
% Tests: DCG syntax (-->), phrase/2, basic grammar rules
% NOTE: DCG is NOT ISO standard but widely supported extension

% Simple number grammar
digit(D) --> [D], { D >= 48, D =< 57 }.
number([D]) --> digit(D).
number([D|Ds]) --> digit(D), number(Ds).

% Basic arithmetic expression grammar  
expr --> term.
expr --> term, [+], expr.
expr --> term, [-], expr.

term --> factor.
term --> factor, [*], term.
term --> factor, [/], term.

factor --> number(_).
factor --> ['('], expr, [')'].

% Simple sentence grammar
sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb, noun_phrase.

determiner --> [the].
determiner --> [a].
noun --> [cat].
noun --> [dog].  
noun --> [mouse].
verb --> [chases].
verb --> [sees].

% List processing with DCG
list_of_as --> [].
list_of_as --> [a], list_of_as.

balanced --> [].
balanced --> ['('], balanced, [')'], balanced.

% DCG with Prolog goals
vowel(V) --> [V], { member(V, [a, e, i, o, u]) }.
vowels --> [].
vowels --> vowel(_), vowels.

% Simple tokenizer
word([C|Cs]) --> letter(C), word(Cs).
word([C]) --> letter(C).

letter(C) --> [C], { C >= 97, C =< 122 }.  % lowercase letters

spaces --> [].
spaces --> [' '], spaces.

tokens([]) --> [].
tokens([Word|Words]) --> 
    spaces, 
    word(Word), 
    spaces,
    tokens(Words).

% Test with phrase/2 (if DCG is supported):
% ?- phrase(number(N), [49, 50, 51], []).    % "123"  
% ?- phrase(expr, [49, +, 50], []).          % "1+2"
% ?- phrase(sentence, [the, cat, chases, a, mouse], []).
% ?- phrase(balanced, ['(', '(', ')', ')'], []).
% ?- phrase(vowels, [a, e, i], []).

% Manual DCG transformation (what should happen):
number_manual([D], [D|Rest], Rest) :- 
    D >= 48, D =< 57.
number_manual([D|Ds], [D|S0], S) :-
    D >= 48, D =< 57,
    number_manual(Ds, S0, S).

% Test manual version:
% ?- number_manual(N, [49, 50, 51], []).