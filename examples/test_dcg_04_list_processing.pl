% DCG Test 04: List Processing with DCG
% Tests: DCG rules for list manipulation and processing

% Match specific elements
vowel --> [a].
vowel --> [e].
vowel --> [i].
vowel --> [o].
vowel --> [u].

consonant --> [C], { member(C, [b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y,z]) }.

% Count vowels and consonants
count_vowels(0) --> [].
count_vowels(N) --> vowel, count_vowels(N1), { N is N1 + 1 }.
count_vowels(N) --> consonant, count_vowels(N).

% Separate vowels and consonants
separate([], []) --> [].
separate([V|Vs], Cs) --> vowel, { V = vowel }, separate(Vs, Cs).
separate(Vs, [C|Cs]) --> consonant, { C = consonant }, separate(Vs, Cs).

% Reverse a list using DCG
reverse_dcg(List, Reversed) --> { reverse_helper(List, [], Reversed) }.

reverse_helper([], Acc, Acc).
reverse_helper([H|T], Acc, Result) :- reverse_helper(T, [H|Acc], Result).

% Pattern matching in lists
pattern_ab --> [a], [b].
pattern_ab --> [a], pattern_ab.
pattern_ab --> [b], pattern_ab.

% Find all positions of element X
find_positions(_, _, []) --> [].
find_positions(X, Pos, [Pos|Rest]) --> [X], { Pos1 is Pos + 1 }, find_positions(X, Pos1, Rest).
find_positions(X, Pos, Rest) --> [Y], { X \= Y, Pos1 is Pos + 1 }, find_positions(X, Pos1, Rest).

% Test queries:
% ?- phrase(vowel, [a]).                 % Expected: true
% ?- phrase(consonant, [b]).             % Expected: true
% ?- phrase(count_vowels(N), [a,b,e,c]). % Expected: N = 2
% ?- phrase(pattern_ab, [a,b,a,b]).      % Expected: true