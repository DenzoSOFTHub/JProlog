% ===================================================================
% TEST 23: First-Order Logic & Grammar Rules
% ===================================================================

% =====================================================
% SECTION 1: FIRST-ORDER LOGIC
% =====================================================

% 1.1 Universal quantification (forall via \+)
% forall(Cond, Action) succeeds if for every solution of Cond, Action holds
my_forall(Cond, Action) :- \+ (call(Cond), \+ call(Action)).

% 1.2 Existential quantification (exists = at least one solution)
my_exists(Goal) :- call(Goal), !.

% 1.3 Knowledge base for FOL tests
person(alice).
person(bob).
person(charlie).
person(diana).

age(alice, 30).
age(bob, 25).
age(charlie, 35).
age(diana, 28).

likes_food(alice, pizza).
likes_food(alice, pasta).
likes_food(bob, pizza).
likes_food(bob, sushi).
likes_food(charlie, pasta).
likes_food(charlie, sushi).
likes_food(diana, pizza).

food(pizza). food(pasta). food(sushi).

works_at(alice, google).
works_at(bob, google).
works_at(charlie, amazon).
works_at(diana, amazon).

salary(alice, 100).
salary(bob, 80).
salary(charlie, 120).
salary(diana, 90).

% 1.4 FOL-style queries

% "Everyone likes at least one food"
everyone_likes_food :- my_forall(person(X), (likes_food(X, _))).

% "There exists someone who likes both pizza and pasta"
exists_pizza_pasta_lover :-
    my_exists((person(X), likes_food(X, pizza), likes_food(X, pasta))).

% "All Google employees earn >= 80"
all_google_rich :-
    my_forall((person(X), works_at(X, google)), (salary(X, S), S >= 80)).

% "No one likes all foods" (negation of universal)
no_one_likes_all :-
    \+ (person(X), my_forall(food(F), likes_food(X, F))).

% 1.5 Implication: if X works at google then X likes pizza
google_likes_pizza :-
    my_forall((person(X), works_at(X, google)), likes_food(X, pizza)).

% 1.6 Counting with quantifiers
count_persons_liking(Food, N) :-
    findall(X, (person(X), likes_food(X, Food)), L),
    length(L, N).

% 1.7 Subset relation: every element of L1 is in L2
subset([], _).
subset([H|T], L2) :- member(H, L2), subset(T, L2).

% 1.8 Set equality
set_equal(L1, L2) :- subset(L1, L2), subset(L2, L1).

% 1.9 Logical connectives as terms
eval_fol(true, _) :- !.
eval_fol(false, _) :- !, fail.
eval_fol(and(A, B), Env) :- eval_fol(A, Env), eval_fol(B, Env).
eval_fol(or(A, _), Env) :- eval_fol(A, Env), !.
eval_fol(or(_, B), Env) :- eval_fol(B, Env).
eval_fol(not(A), Env) :- \+ eval_fol(A, Env).
eval_fol(implies(A, B), Env) :- eval_fol(or(not(A), B), Env).
eval_fol(iff(A, B), Env) :- eval_fol(and(implies(A, B), implies(B, A)), Env).

% Atomic propositions checked against environment
eval_fol(prop(P), Env) :- member(P, Env).

% 1.10 Propositional logic truth table generation
truth_values(true).
truth_values(false).

eval_prop(true, true) :- !.
eval_prop(false, false) :- !.
eval_prop(and(A, B), true) :- eval_prop(A, true), eval_prop(B, true), !.
eval_prop(and(_, _), false) :- !.
eval_prop(or(A, _), true) :- eval_prop(A, true), !.
eval_prop(or(_, B), true) :- eval_prop(B, true), !.
eval_prop(or(_, _), false) :- !.
eval_prop(not(A), true) :- eval_prop(A, false), !.
eval_prop(not(A), false) :- eval_prop(A, true).
eval_prop(implies(A, B), V) :- eval_prop(or(not(A), B), V).

% Is tautology? True for all truth value assignments
is_tautology(Expr, Vars) :-
    my_forall(assign_all(Vars), check_true(Expr, Vars)).

assign_all([]).
assign_all([_=true|T]) :- assign_all(T).
assign_all([_=false|T]) :- assign_all(T).

check_true(Expr, Vars) :- eval_with_vars(Expr, Vars, true).

eval_with_vars(var(X), Vars, V) :- member(X=V, Vars), !.
eval_with_vars(true, _, true) :- !.
eval_with_vars(false, _, false) :- !.
eval_with_vars(and(A, B), Vars, true) :-
    eval_with_vars(A, Vars, true), eval_with_vars(B, Vars, true), !.
eval_with_vars(and(_, _), _, false) :- !.
eval_with_vars(or(A, _), Vars, true) :- eval_with_vars(A, Vars, true), !.
eval_with_vars(or(_, B), Vars, true) :- eval_with_vars(B, Vars, true), !.
eval_with_vars(or(_, _), _, false) :- !.
eval_with_vars(not(A), Vars, true) :- eval_with_vars(A, Vars, false), !.
eval_with_vars(not(A), Vars, false) :- eval_with_vars(A, Vars, true).
eval_with_vars(implies(A, B), Vars, V) :- eval_with_vars(or(not(A), B), Vars, V).

% 1.11 Transitive closure
tc(R, X, Y) :- call(R, X, Y).
tc(R, X, Y) :- call(R, X, Z), tc(R, Z, Y).

% Relation for tc tests
bigger(elephant, horse).
bigger(horse, dog).
bigger(dog, cat).
bigger(cat, mouse).

% =====================================================
% SECTION 2: GRAMMAR RULES (Recursive Descent Parsers)
% =====================================================

% 2.1 Simple English sentence grammar
% S -> NP VP
% NP -> det noun
% VP -> verb NP | verb
% det -> the | a
% noun -> cat | dog | mouse | fish
% verb -> chases | eats | sees | likes

sentence(s(NP, VP)) --> noun_phrase(NP), verb_phrase(VP).

noun_phrase(np(Det, Noun)) --> det(Det), noun(Noun).

verb_phrase(vp(Verb, NP)) --> verb(Verb), noun_phrase(NP).
verb_phrase(vp(Verb)) --> verb(Verb).

det(det(the)) --> [the].
det(det(a)) --> [a].

noun(noun(cat)) --> [cat].
noun(noun(dog)) --> [dog].
noun(noun(mouse)) --> [mouse].
noun(noun(fish)) --> [fish].

verb(verb(chases)) --> [chases].
verb(verb(eats)) --> [eats].
verb(verb(sees)) --> [sees].
verb(verb(likes)) --> [likes].

% 2.2 Parse sentence using difference lists (manual, not DCG)
parse_sentence(s(NP, VP), S0, S) :-
    parse_np(NP, S0, S1),
    parse_vp(VP, S1, S).

parse_np(np(Det, N), S0, S) :-
    parse_det(Det, S0, S1),
    parse_noun(N, S1, S).

parse_vp(vp(V, NP), S0, S) :-
    parse_verb(V, S0, S1),
    parse_np(NP, S1, S).
parse_vp(vp(V), S0, S) :-
    parse_verb(V, S0, S).

parse_det(det(the), [the|S], S).
parse_det(det(a), [a|S], S).

parse_noun(noun(cat), [cat|S], S).
parse_noun(noun(dog), [dog|S], S).
parse_noun(noun(mouse), [mouse|S], S).
parse_noun(noun(fish), [fish|S], S).

parse_verb(verb(chases), [chases|S], S).
parse_verb(verb(eats), [eats|S], S).
parse_verb(verb(sees), [sees|S], S).
parse_verb(verb(likes), [likes|S], S).

% 2.3 Arithmetic expression grammar (recursive descent, using tokens)
% expr -> term ((add|sub) term)*
% term -> number | '(' expr ')'
arith_parse(Tokens, AST) :- a_expr(AST, Tokens, []).

a_expr(E, S0, S) :- a_term(T, S0, S1), a_expr_rest(T, E, S1, S).
a_expr_rest(T, E, [add|S0], S) :- a_term(T2, S0, S1), a_expr_rest(plus(T, T2), E, S1, S).
a_expr_rest(T, E, [sub|S0], S) :- a_term(T2, S0, S1), a_expr_rest(minus(T, T2), E, S1, S).
a_expr_rest(T, T, S, S).

a_term(T, S0, S) :- a_factor(F, S0, S1), a_term_rest(F, T, S1, S).
a_term_rest(F, T, [mul|S0], S) :- a_factor(F2, S0, S1), a_term_rest(times(F, F2), T, S1, S).
a_term_rest(F, F, S, S).

a_factor(num(N), [N|S], S) :- number(N).

% 2.4 Regular expression matcher (simplified: a, b, cat(A,B), alt(A,B), star(A))
% Matches a string (list of chars) against a pattern
re_match(char(C), [C|Rest], Rest).
re_match(cat(A, B), S0, S) :- re_match(A, S0, S1), re_match(B, S1, S).
re_match(alt(A, _), S0, S) :- re_match(A, S0, S).
re_match(alt(_, B), S0, S) :- re_match(B, S0, S).
re_match(star(_), S, S).
re_match(star(A), S0, S) :- re_match(A, S0, S1), S1 \= S0, re_match(star(A), S1, S).
re_match(epsilon, S, S).

% 2.5 JSON-like structure validator
% json_value -> json_string | json_number | json_object | json_array | json_bool | json_null
valid_json(str(_)).
valid_json(num(N)) :- number(N).
valid_json(bool(true)).
valid_json(bool(false)).
valid_json(null).
valid_json(object(Pairs)) :- valid_pairs(Pairs).
valid_json(array(Elements)) :- valid_elements(Elements).

valid_pairs([]).
valid_pairs([pair(Key, Value)|Rest]) :-
    atom(Key),
    valid_json(Value),
    valid_pairs(Rest).

valid_elements([]).
valid_elements([H|T]) :- valid_json(H), valid_elements(T).

% 2.6 Simple type checker
% Types: int, bool, fun(ArgType, RetType)
% Expressions: num(N), true, false, var(X), app(F, Arg), lam(X, Type, Body)
type_of(_, num(_), int).
type_of(_, true, bool).
type_of(_, false, bool).
type_of(Env, var(X), T) :- member(X-T, Env).
type_of(Env, app(F, Arg), RetT) :-
    type_of(Env, F, fun(ArgT, RetT)),
    type_of(Env, Arg, ArgT).
type_of(Env, lam(X, ArgT, Body), fun(ArgT, RetT)) :-
    type_of([X-ArgT|Env], Body, RetT).
type_of(Env, plus_expr(A, B), int) :-
    type_of(Env, A, int),
    type_of(Env, B, int).
type_of(Env, if_expr(Cond, Then, Else), T) :-
    type_of(Env, Cond, bool),
    type_of(Env, Then, T),
    type_of(Env, Else, T).

% 2.7 Simple CFG recognizer
% S -> a S b | empty
anbn(S) :- anbn(S, []).
anbn(S, S).
anbn([a|S0], S) :- anbn(S0, [b|S]).

% 2.8 Bracket matcher with multiple types
bracket_match(L) :- bracket_match(L, []).
bracket_match([], []).
bracket_match(['('|T], Stack) :- bracket_match(T, [')'|Stack]).
bracket_match(['['|T], Stack) :- bracket_match(T, [']'|Stack]).
bracket_match(['{'|T], Stack) :- bracket_match(T, ['}'|Stack]).
bracket_match([C|T], [C|Stack]) :- member(C, [')', ']', '}']), bracket_match(T, Stack).
