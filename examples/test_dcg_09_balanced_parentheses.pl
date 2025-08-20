% DCG Test 09: Balanced Parentheses and Brackets
% Tests: Nested structure validation, multiple bracket types

% Basic balanced parentheses
balanced --> [].
balanced --> [40], balanced, [41], balanced.  % ( )

% Multiple bracket types
bracket_pair --> [40], [41].  % ()
bracket_pair --> [91], [93].  % []
bracket_pair --> [123], [125]. % {}

% Mixed balanced brackets
mixed_balanced --> [].
mixed_balanced --> [40], mixed_balanced, [41], mixed_balanced.    % ( )
mixed_balanced --> [91], mixed_balanced, [93], mixed_balanced.    % [ ]
mixed_balanced --> [123], mixed_balanced, [125], mixed_balanced.  % { }

% Count nesting depth
depth(0) --> [].
depth(D) --> [40], depth(D1), [41], depth(D2), { D is max(D1+1, D2) }.

% Validate and count brackets
validate_count(0, 0, 0) --> [].
validate_count(P, B, C) --> [40], validate_count(P1, B, C), [41], { P is P1 + 1 }.
validate_count(P, B, C) --> [91], validate_count(P, B1, C), [93], { B is B1 + 1 }.
validate_count(P, B, C) --> [123], validate_count(P, B, C1), [125], { C is C1 + 1 }.

% Properly nested check
properly_nested --> nested_structure.

nested_structure --> [].
nested_structure --> [40], nested_structure, [41], nested_structure.
nested_structure --> [91], nested_structure, [93], nested_structure.
nested_structure --> [123], nested_structure, [125], nested_structure.

% Match bracket types
match_type(paren) --> [40].
match_type(square) --> [91].
match_type(curly) --> [123].

close_type(paren) --> [41].
close_type(square) --> [93].
close_type(curly) --> [125].

% Validate matching types
typed_balanced --> [].
typed_balanced --> match_type(T), typed_balanced, close_type(T), typed_balanced.

% Test queries:
% ?- phrase(balanced, [40,40,41,41]).               % Expected: true
% ?- phrase(balanced, [40,41,40]).                  % Expected: false  
% ?- phrase(mixed_balanced, [40,91,93,41]).         % Expected: true
% ?- phrase(depth(D), [40,40,41,41]).               % Expected: D = 2
% ?- phrase(typed_balanced, [40,91,93,41]).         % Expected: true