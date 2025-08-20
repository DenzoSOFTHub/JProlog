% ===================================================================
% TEST 10: String and Atom Operations
% ===================================================================
% Tests: atom_codes/2, atom_chars/2, atom_concat/3, sub_atom/5

% Atom manipulation
build_greeting(Name, Greeting) :-
    atom_concat('Hello, ', Name, Temp),
    atom_concat(Temp, '!', Greeting).

split_name(FullName, FirstName, LastName) :-
    atom_chars(FullName, Chars),
    append(FirstChars, [' '|LastChars], Chars),
    atom_chars(FirstName, FirstChars),
    atom_chars(LastName, LastChars).

% Character and code conversion
caesar_cipher(Char, Shifted) :-
    atom_codes(Char, [Code]),
    (Code >= 97, Code =< 122 ->  % lowercase
        NewCode is ((Code - 97 + 3) mod 26) + 97
    ; Code >= 65, Code =< 90 ->   % uppercase  
        NewCode is ((Code - 65 + 3) mod 26) + 65
    ; NewCode = Code
    ),
    atom_codes(Shifted, [NewCode]).

% String processing
count_vowels(Atom, Count) :-
    atom_chars(Atom, Chars),
    count_vowels_in_list(Chars, Count).

count_vowels_in_list([], 0).
count_vowels_in_list([H|T], Count) :-
    (member(H, [a, e, i, o, u, 'A', 'E', 'I', 'O', 'U']) ->
        count_vowels_in_list(T, RestCount),
        Count is RestCount + 1
    ;   count_vowels_in_list(T, Count)
    ).

% Sub-atom operations
extract_prefix(Atom, Length, Prefix) :-
    sub_atom(Atom, 0, Length, _, Prefix).

extract_suffix(Atom, Length, Suffix) :-
    atom_length(Atom, AtomLen),
    Start is AtomLen - Length,
    sub_atom(Atom, Start, Length, 0, Suffix).

contains_substring(Atom, SubAtom) :-
    sub_atom(Atom, _, _, _, SubAtom).

% Word processing
reverse_word(Word, Reversed) :-
    atom_chars(Word, Chars),
    reverse(Chars, RevChars),
    atom_chars(Reversed, RevChars).

palindrome(Word) :-
    reverse_word(Word, Word).

% Case conversion (simplified)
to_lowercase(Atom, Lower) :-
    atom_codes(Atom, Codes),
    map_to_lower(Codes, LowerCodes),
    atom_codes(Lower, LowerCodes).

map_to_lower([], []).
map_to_lower([C|Rest], [LC|LRest]) :-
    (C >= 65, C =< 90 ->
        LC is C + 32
    ;   LC = C
    ),
    map_to_lower(Rest, LRest).

% Test queries:
% ?- build_greeting(world, Greeting).
% ?- split_name('John Doe', First, Last).
% ?- caesar_cipher(a, Shifted).
% ?- count_vowels(hello, Count).
% ?- extract_prefix(programming, 4, Prefix).
% ?- contains_substring(hello, ell).
% ?- palindrome(radar).
% ?- to_lowercase('HELLO', Lower).