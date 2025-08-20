% DCG Test 07: Context-Free Grammar for Simple Language
% Tests: Recursive grammar rules, sentence structure

% Lexical rules
noun --> [cat].
noun --> [dog].
noun --> [mouse].
noun --> [cheese].

verb --> [chases].
verb --> [eats].
verb --> [sees].
verb --> [likes].

article --> [the].
article --> [a].

adjective --> [big].
adjective --> [small].
adjective --> [hungry].
adjective --> [fast].

preposition --> [in].
preposition --> [on].
preposition --> [under].
preposition --> [with].

% Phrase structure rules
noun_phrase --> article, noun.
noun_phrase --> article, adjective, noun.
noun_phrase --> noun.

verb_phrase --> verb.
verb_phrase --> verb, noun_phrase.
verb_phrase --> verb, noun_phrase, prep_phrase.

prep_phrase --> preposition, noun_phrase.

% Sentence structure
sentence --> noun_phrase, verb_phrase.

% More complex structures
compound_sentence --> sentence, [and], sentence.
compound_sentence --> sentence, [or], sentence.

question --> [does], noun_phrase, verb, noun_phrase, [?].
question --> [what], verb, noun_phrase, [?].

% Story structure
story --> sentence.
story --> sentence, [.], story.

% Test queries:
% ?- phrase(noun_phrase, [the, big, cat]).          % Expected: true
% ?- phrase(verb_phrase, [chases, the, mouse]).     % Expected: true
% ?- phrase(sentence, [the, cat, chases, the, mouse]). % Expected: true
% ?- phrase(question, [does, the, cat, eat, cheese, ?]). % Expected: true