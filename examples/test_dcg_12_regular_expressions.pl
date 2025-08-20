% DCG Test 12: Regular Expression Patterns
% Tests: Pattern matching, repetition, alternation

% Basic character matching
char(C) --> [C].

% Any character (except specific)
any_except(Excluded) --> [C], { \+ member(C, Excluded) }.

% Character classes
alpha --> [C], { (C >= 97, C =< 122); (C >= 65, C =< 90) }.
numeric --> [C], { C >= 48, C =< 57 }.
alphanumeric --> alpha.
alphanumeric --> numeric.

% Repetition patterns
zero_or_more(_) --> [].
zero_or_more(Pattern) --> call(Pattern), zero_or_more(Pattern).

one_or_more(Pattern) --> call(Pattern).
one_or_more(Pattern) --> call(Pattern), one_or_more(Pattern).

optional(_) --> [].
optional(Pattern) --> call(Pattern).

% Specific repetitions
exactly(0, _) --> [].
exactly(N, Pattern) --> { N > 0 }, call(Pattern), { N1 is N - 1 }, exactly(N1, Pattern).

at_least(0, _) --> [].
at_least(N, Pattern) --> { N > 0 }, call(Pattern), { N1 is N - 1 }, at_least(N1, Pattern).
at_least(N, Pattern) --> at_least(N, Pattern), call(Pattern).

% Alternation (choice)
choice([Pattern]) --> call(Pattern).
choice([Pattern|_]) --> call(Pattern).
choice([_|Patterns]) --> choice(Patterns).

% Common patterns
word --> one_or_more(alpha).
number_pattern --> one_or_more(numeric).
identifier --> alpha, zero_or_more(alphanumeric).

% Email pattern (simplified)
email_pattern --> 
    one_or_more(alphanumeric),
    [64],  % @
    one_or_more(alphanumeric),
    [46],  % .
    one_or_more(alpha).

% Phone pattern (digits with optional hyphens)
phone_digit_or_hyphen --> numeric.
phone_digit_or_hyphen --> [45].  % hyphen

phone_pattern --> one_or_more(phone_digit_or_hyphen).

% URL pattern
url_pattern -->
    choice([[104,116,116,112], [104,116,116,112,115]]),  % http or https
    [58,47,47],  % ://
    one_or_more(alphanumeric),
    [46],  % .
    one_or_more(alpha).

% Test queries:
% ?- phrase(zero_or_more(alpha), [97,98,99]).       % Expected: true
% ?- phrase(one_or_more(numeric), [49,50,51]).      % Expected: true
% ?- phrase(exactly(3, alpha), [97,98,99]).         % Expected: true
% ?- phrase(word, [104,101,108,108,111]).           % Expected: true
% ?- phrase(identifier, [118,97,114,49,50,51]).     % Expected: true