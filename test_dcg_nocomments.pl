digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".

number(N) --> digit(N).

spaces --> [].
spaces --> [32], spaces.

op_plus --> "+".
op_minus --> "-".

expr(N) --> number(N).
expr(plus(E1, E2)) --> expr(E1), spaces, op_plus, spaces, expr(E2).
expr(minus(E1, E2)) --> expr(E1), spaces, op_minus, spaces, expr(E2).