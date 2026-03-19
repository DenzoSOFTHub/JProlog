test_var_bound :- X = hello, nonvar(X).
test_var_unbound :- var(Y).
test_nonvar_atom :- nonvar(hello).
test_member :- member(2, [1,2,3]).
test_repeat_count :- findall(X, between(1,5,X), Xs), length(Xs, 5).
test_integer_check :- integer(42).
test_float_check :- float(3.14).
test_not_float_int :- \+ float(42).
test_not_integer_float :- \+ integer(3.14).
test_int_div :- X is 7 // 2, X =:= 3.
test_mod :- X is 7 mod 3, X =:= 1.
test_rem_positive :- X is 7 rem 3, X =:= 1.
test_abs :- X is abs(-5), X =:= 5.
test_max :- X is max(3, 7), X =:= 7.
test_min :- X is min(3, 7), X =:= 3.
test_hex :- X is 0xFF, X =:= 255.
test_octal :- X is 0o77, X =:= 63.
test_binary :- X is 0b1010, X =:= 10.
test_keysort :- keysort([b-2, a-1, c-3], Sorted), Sorted = [a-1, b-2, c-3].
test_copy_term :- copy_term(f(X, Y), f(A, B)), var(A), var(B).
test_term_compare :- a @< b.
test_term_equal :- hello == hello.
test_term_not_equal :- hello \== world.
test_arith_equal :- 3 + 2 =:= 5.
test_arith_not_equal :- 3 + 2 =\= 6.
test_unify :- X = hello, X == hello.
test_not_unify :- a \= b.
test_atom_check :- atom(hello).
test_compound_check :- compound(f(x)).
test_atomic_check :- atomic(42).
test_number_check :- number(3.14).
test_callable_check :- callable(hello).
test_append :- append([1,2], [3,4], [1,2,3,4]).
test_length :- length([a,b,c], 3).
test_reverse :- reverse([1,2,3], [3,2,1]).
test_sort :- sort([3,1,2,1], [1,2,3]).
test_msort :- msort([3,1,2,1], [1,1,2,3]).
test_findall :- findall(X, member(X, [a,b,c]), Xs), Xs = [a, b, c].
test_assert_retract :- assertz(temp_fact(42)), temp_fact(42), retract(temp_fact(42)), \+ temp_fact(42).
test_atom_length :- atom_length(hello, 5).
test_atom_concat :- atom_concat(hel, lo, hello).
test_catch :- catch(throw(my_error), my_error, true).
test_between :- findall(X, between(1, 5, X), Xs), Xs = [1, 2, 3, 4, 5].
test_succ :- succ(3, 4).
test_once :- findall(X, once(member(X, [a,b,c])), [a]).
test_char_code :- char_code(a, 97).
test_sub_atom :- sub_atom(abcdef, 2, 3, _, Sub), Sub = cde.
