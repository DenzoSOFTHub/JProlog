digit_code(48, 0).
digit_code(49, 1).
digit_code(50, 2).
digit_code(51, 3).

parse_digit([Code|Rest], Digit, Rest) :- digit_code(Code, Digit).

test_basic :- atom_codes('1', Codes), parse_digit(Codes, D, _), write(D).
