# DCG Guide - Definite Clause Grammars in JProlog

## Table of Contents
1. [Introduction to DCG](#introduction-to-dcg)
2. [Basic DCG Syntax](#basic-dcg-syntax)
3. [Simple Examples](#simple-examples)
4. [Advanced DCG Features](#advanced-dcg-features)
5. [Parsing with DCG](#parsing-with-dcg)
6. [DCG with Actions](#dcg-with-actions)
7. [Complex Grammar Examples](#complex-grammar-examples)
8. [Best Practices](#best-practices)
9. [Troubleshooting](#troubleshooting)

---

## Introduction to DCG

**Definite Clause Grammars (DCG)** are a powerful feature in Prolog for defining and parsing context-free grammars. DCGs provide a clean, readable syntax for describing language patterns and parsing structured text.

### Key Benefits of DCG:
- **Readable Grammar Rules**: Natural syntax for grammar definitions
- **Automatic Translation**: DCG rules are translated to standard Prolog predicates
- **Built-in Parsing**: Automatic handling of input consumption
- **Backtracking Support**: Multiple parsing paths explored automatically
- **Integration**: Seamless integration with Prolog's logic programming

### How DCG Works:
DCG rules are syntactic sugar that gets translated into regular Prolog predicates with two additional arguments representing the input list and the remaining unconsumed input.

```prolog
% DCG rule:
sentence --> noun_phrase, verb_phrase.

% Gets translated to:
sentence(S0, S) :- noun_phrase(S0, S1), verb_phrase(S1, S).
```

---

## Basic DCG Syntax

### DCG Rule Structure

```prolog
% Basic DCG rule syntax:
rule_name --> body.

% Multiple alternatives:
rule_name --> alternative1.
rule_name --> alternative2.

% Or using semicolon:
rule_name --> alternative1 ; alternative2.
```

### Terminal Symbols

```prolog
% Terminal symbols are represented as lists:
word --> [hello].
greeting --> [hello], [world].

% Multiple words:
phrase --> [the], [quick], [brown], [fox].
```

### Non-Terminal Symbols

```prolog
% Non-terminals are rule names without brackets:
sentence --> subject, predicate.
subject --> noun_phrase.
predicate --> verb_phrase.
```

### Variables in DCG

```prolog
% Variables can be used to pass information:
number(N) --> [N], { number(N) }.
word(W) --> [W], { atom(W) }.
```

### Actions in DCG

```prolog
% Actions are enclosed in curly braces { }:
number_check(N) --> [N], { N > 0 }.
type_check(X) --> [X], { atom(X) }.
```

---

## Simple Examples

### Example 1: Basic Word Recognition

```prolog
% Simple word matching
hello --> [hello].
world --> [world].
greeting --> hello, world.

% Usage:
% ?- phrase(hello, [hello]).
% true.

% ?- phrase(greeting, [hello, world]).
% true.

% ?- phrase(greeting, [hello, world, extra]).
% true.
```

### Example 2: Simple Sentence Structure

```prolog
% Basic sentence grammar
article --> [the].
article --> [a].
article --> [an].

noun --> [cat].
noun --> [dog].
noun --> [mouse].

verb --> [chases].
verb --> [catches].
verb --> [sees].

noun_phrase --> article, noun.
verb_phrase --> verb, noun_phrase.
sentence --> noun_phrase, verb_phrase.

% Usage:
% ?- phrase(sentence, [the, cat, chases, a, mouse]).
% true.

% ?- phrase(sentence, [a, dog, sees, the, cat]).
% true.
```

### Example 3: Number Recognition

```prolog
% Digit recognition
digit(D) --> [D], { member(D, [0,1,2,3,4,5,6,7,8,9]) }.

% Multi-digit numbers
number(N) --> digit(N).
number(N) --> digit(D), number(Rest), { N is D * 10 + Rest }.

% Usage:
% ?- phrase(digit(X), [5]).
% X = 5.

% ?- phrase(number(N), [1,2,3]).
% N = 123.
```

---

## Advanced DCG Features

### Optional Elements

```prolog
% Optional elements using empty alternative
optional_article --> [the].
optional_article --> [].

noun_phrase --> optional_article, noun.

% Usage:
% ?- phrase(noun_phrase, [cat]).          % Works without article
% ?- phrase(noun_phrase, [the, cat]).     % Works with article
```

### Repetition

```prolog
% Zero or more repetitions
adjectives --> [].
adjectives --> adjective, adjectives.

adjective --> [big].
adjective --> [small].
adjective --> [red].
adjective --> [blue].

% One or more repetitions
adjectives_plus --> adjective.
adjectives_plus --> adjective, adjectives_plus.

noun_phrase --> optional_article, adjectives, noun.

% Usage:
% ?- phrase(noun_phrase, [the, big, red, cat]).
% true.
```

### Variables and Information Passing

```prolog
% Passing information through DCG rules
determiner(singular) --> [the].
determiner(singular) --> [a].
determiner(plural) --> [the].

noun(singular, cat) --> [cat].
noun(singular, dog) --> [dog].
noun(plural, cats) --> [cats].
noun(plural, dogs) --> [dogs].

noun_phrase(Number, Animal) --> 
    determiner(Number), 
    noun(Number, Animal).

% Usage:
% ?- phrase(noun_phrase(N, A), [the, cat]).
% N = singular, A = cat.

% ?- phrase(noun_phrase(N, A), [the, cats]).
% N = plural, A = cats.
```

---

## Parsing with DCG

### Example 4: Mathematical Expression Parser

```prolog
% Simple arithmetic expression parser
expr(X) --> term(X).
expr(X) --> term(L), [+], expr(R), { X is L + R }.
expr(X) --> term(L), [-], expr(R), { X is L - R }.

term(X) --> factor(X).
term(X) --> factor(L), [*], term(R), { X is L * R }.
term(X) --> factor(L), [/], term(R), { X is L / R }.

factor(X) --> number(X).
factor(X) --> ['('], expr(X), [')'].

number(N) --> [N], { number(N) }.

% Usage:
% ?- phrase(expr(Result), [2, +, 3, *, 4]).
% Result = 14.

% ?- phrase(expr(Result), ['(', 2, +, 3, ')', *, 4]).
% Result = 20.
```

### Example 5: Simple Language Parser

```prolog
% Simple programming language statements
statement --> assignment.
statement --> if_statement.
statement --> while_loop.

assignment --> variable, ['='], expression.
if_statement --> [if], condition, [then], statement.
while_loop --> [while], condition, [do], statement.

variable --> [X], { atom(X) }.
expression --> variable.
expression --> number.
condition --> variable, comparison_op, expression.

comparison_op --> ['=='].
comparison_op --> ['<'].
comparison_op --> ['>'].

number --> [N], { number(N) }.

% Usage:
% ?- phrase(statement, [x, '=', 42]).
% true.

% ?- phrase(statement, [if, x, '==', 0, then, y, '=', 1]).
% true.
```

---

## DCG with Actions

### Example 6: Building Parse Trees

```prolog
% DCG that builds abstract syntax trees
expr(plus(L,R)) --> term(L), [+], expr(R).
expr(minus(L,R)) --> term(L), [-], expr(R).
expr(X) --> term(X).

term(mult(L,R)) --> factor(L), [*], term(R).
term(div(L,R)) --> factor(L), [/], term(R).
term(X) --> factor(X).

factor(num(N)) --> [N], { number(N) }.
factor(var(V)) --> [V], { atom(V) }.
factor(X) --> ['('], expr(X), [')'].

% Usage:
% ?- phrase(expr(Tree), [2, +, 3, *, x]).
% Tree = plus(num(2), mult(num(3), var(x))).
```

### Example 7: List Processing

```prolog
% DCG for processing comma-separated lists
list([]) --> [].
list([H|T]) --> element(H), list_tail(T).

list_tail([]) --> [].
list_tail([H|T]) --> [','], element(H), list_tail(T).

element(N) --> [N], { number(N) }.
element(A) --> [A], { atom(A) }.

% Usage:
% ?- phrase(list(L), [1, ',', 2, ',', hello, ',', 3]).
% L = [1, 2, hello, 3].
```

---

## Complex Grammar Examples

### Example 8: JSON Parser

```prolog
% Simple JSON parser
json_value(Value) --> json_string(Value).
json_value(Value) --> json_number(Value).
json_value(Value) --> json_array(Value).
json_value(Value) --> json_object(Value).
json_value(true) --> [true].
json_value(false) --> [false].
json_value(null) --> [null].

json_string(String) --> ['"'], string_chars(Chars), ['"'], 
    { atom_chars(String, Chars) }.

string_chars([]) --> [].
string_chars([C|Cs]) --> [C], { C \= '"' }, string_chars(Cs).

json_number(N) --> [N], { number(N) }.

json_array([]) --> ['['], [']'].
json_array([H|T]) --> ['['], json_value(H), array_tail(T), [']'].

array_tail([]) --> [].
array_tail([H|T]) --> [','], json_value(H), array_tail(T).

json_object(Object) --> ['{'], ['}'], { Object = {} }.
json_object(Object) --> ['{'], key_value_pairs(Pairs), ['}'],
    { Object = json_object(Pairs) }.

key_value_pairs([K-V]) --> json_string(K), [':'], json_value(V).
key_value_pairs([K-V|Rest]) --> 
    json_string(K), [':'], json_value(V), [','], key_value_pairs(Rest).

% Usage:
% ?- phrase(json_value(V), ['{', '"', name, '"', ':', '"', john, '"', '}']).
% V = json_object([name-john]).
```

### Example 9: Simple SQL Parser

```prolog
% Basic SQL SELECT statement parser
sql_query(select(Columns, Table, Condition)) -->
    [select], column_list(Columns),
    [from], table_name(Table),
    where_clause(Condition).

sql_query(select(Columns, Table, true)) -->
    [select], column_list(Columns),
    [from], table_name(Table).

column_list([*]) --> [*].
column_list([C]) --> column_name(C).
column_list([C|Cs]) --> column_name(C), [','], column_list(Cs).

where_clause(Condition) --> [where], condition(Condition).

condition(eq(Col, Val)) --> column_name(Col), ['='], value(Val).
condition(gt(Col, Val)) --> column_name(Col), ['>'], value(Val).
condition(lt(Col, Val)) --> column_name(Col), ['<'], value(Val).

table_name(T) --> [T], { atom(T) }.
column_name(C) --> [C], { atom(C) }.
value(V) --> [V].

% Usage:
% ?- phrase(sql_query(Q), [select, name, ',', age, from, users, where, age, '>', 18]).
% Q = select([name, age], users, gt(age, 18)).
```

---

## Best Practices

### 1. Structure Your Grammar Hierarchically

```prolog
% Good: Clear hierarchical structure
sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, adjectives, noun.
verb_phrase --> verb, noun_phrase.

% Avoid: Flat, unclear structure
sentence --> [the], [big], [cat], [chases], [a], [mouse].
```

### 2. Use Meaningful Rule Names

```prolog
% Good: Descriptive names
optional_adjectives --> adjectives.
optional_adjectives --> [].

mandatory_noun --> noun.

% Avoid: Cryptic names
opt_adj --> adj.
opt_adj --> [].
```

### 3. Handle Edge Cases

```prolog
% Good: Handle empty inputs
word_list([]) --> [].
word_list([W|Ws]) --> word(W), word_list(Ws).

% Good: Handle optional elements
sentence --> noun_phrase, verb_phrase, optional_punctuation.

optional_punctuation --> ['.'].
optional_punctuation --> ['!'].
optional_punctuation --> ['?'].
optional_punctuation --> [].
```

### 4. Use Actions Appropriately

```prolog
% Good: Use actions for validation and computation
integer(N) --> [N], { integer(N), N >= 0 }.
sum_expr(Sum) --> number(A), [+], number(B), { Sum is A + B }.

% Avoid: Complex logic in actions
% Keep actions simple and focused
```

### 5. Test Incrementally

```prolog
% Start with simple rules
noun --> [cat].
verb --> [runs].

% Build complexity gradually
noun_phrase --> article, noun.
verb_phrase --> verb, adverb.

% Test each level
% ?- phrase(noun, [cat]).
% ?- phrase(noun_phrase, [the, cat]).
```

---

## Troubleshooting

### Common Issues and Solutions

#### **DCG Rule Not Recognized**
```prolog
% Problem: DCG syntax not working
% Solution: Ensure DCG translation is supported

% Check if this works:
test --> [hello].

% Test with phrase/2:
% ?- phrase(test, [hello]).
```

#### **Infinite Loops in DCG**
```prolog
% Problem: Left-recursive rules cause infinite loops
% Bad:
expr --> expr, [+], term.  % Left-recursive!

% Good: Right-recursive alternative
expr --> term, expr_rest.
expr_rest --> [+], term, expr_rest.
expr_rest --> [].
```

#### **Variables Not Unifying**
```prolog
% Problem: Variables not passing correctly
% Solution: Check variable naming and scoping

% Good:
number_pair(N1, N2) --> number(N1), [','], number(N2).
number(N) --> [N], { number(N) }.

% Test:
% ?- phrase(number_pair(A, B), [1, ',', 2]).
```

#### **Actions Not Executing**
```prolog
% Problem: Actions in {} not working as expected
% Solution: Ensure proper Prolog syntax in actions

% Good:
validated_number(N) --> [N], { number(N), N > 0 }.

% Bad:
% validated_number(N) --> [N], { N > 0, number(N) }.  % Order matters!
```

### Debugging DCG Rules

#### **Step-by-Step Testing**
```prolog
% Test individual components
% ?- phrase(article, [the]).
% ?- phrase(noun, [cat]).
% ?- phrase(noun_phrase, [the, cat]).

% Use trace to see execution
% ?- trace, phrase(sentence, [the, cat, chases, a, mouse]).
```

#### **Visualizing Parse Trees**
```prolog
% Add structure to see parsing steps
sentence(s(NP, VP)) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase(np(Art, N)) --> article(Art), noun(N).
verb_phrase(vp(V, NP)) --> verb(V), noun_phrase(NP).

article(art(the)) --> [the].
noun(n(cat)) --> [cat].
verb(v(chases)) --> [chases].

% Usage:
% ?- phrase(sentence(Tree), [the, cat, chases, the, mouse]).
% Tree = s(np(art(the), n(cat)), vp(v(chases), np(art(the), n(mouse)))).
```

---

## DCG Status and Limitations in JProlog v2.0.6

### ✅ **Working DCG Features** (85% Success Rate)

JProlog v2.0.6 provides **comprehensive DCG support** with 85% success rate on complex parsing tasks:

#### **Core Features (100% Working)**
- ✅ **Basic DCG rule translation**: `rule --> body.` syntax
- ✅ **Terminal symbols**: `[word]`, `[hello, world]` lists
- ✅ **Non-terminal symbols**: Rule references without brackets
- ✅ **Variables and information passing**: `rule(X) --> body(X)`
- ✅ **Simple actions**: `{ number(X) }`, `{ X > 0 }` constraints
- ✅ **phrase/2 predicate**: Standard parsing interface
- ✅ **Recursive rules**: `list --> []; [H], list.`
- ✅ **Alternative rules**: Semicolon and multiple clause support
- ✅ **Empty productions**: `optional --> []; word.`

#### **Advanced Features (85% Working)**
- ✅ **Complex grammar constructions**: Nested rules, multiple variables
- ✅ **List processing**: Head/tail patterns `[H|T]`
- ✅ **Character code lists**: `[104,116,116,112]` for "http"
- ✅ **Basic arithmetic constraints**: `{ X is Y + 1 }`
- ✅ **Simple comparison operations**: `{ X >= 48, X =< 57 }`
- ✅ **Variable binding propagation**: Variables pass through DCG rules
- ✅ **Backtracking in parsing**: Multiple solution exploration

#### **Examples of Working Patterns**

```prolog
% ✅ Basic patterns work perfectly:
digits([]) --> [].
digits([D|Ds]) --> digit(D), digits(Ds).
digit(D) --> [C], { C >= 48, C =< 57, D is C - 48 }.

% ✅ Complex nested structures:
expression(plus(L,R)) --> term(L), [43], expression(R).  % 43 = '+'
expression(X) --> term(X).
term(mult(L,R)) --> factor(L), [42], factor(R).  % 42 = '*'

% ✅ Variable binding and constraints:
balanced([]) --> [].
balanced(L) --> [40], balanced(L1), [41], balanced(L2),  % 40='(', 41=')'
                { append(L1, L2, L) }.

% ✅ Character validation:
letter --> [C], { C >= 97, C =< 122 }.  % lowercase letters
uppercase --> [C], { C >= 65, C =< 90 }.  % uppercase letters
```

### ❌ **Current DCG Limitations** (15% Failure Rate)

Three specific parser limitations affect advanced DCG patterns:

#### **ISS-2025-0040: Complex Operator Terms in DCG Heads**
```prolog
% ❌ FAILS: Compound operator terms in list heads
json_object([K-V|Pairs]) --> [123], ws, json_pair(K-V), json_object_rest(Pairs), ws, [125].
% Error: Expected ')' at line 1, column 12

% ✅ WORKAROUND: Use separate structures
json_object([Pair|Pairs]) --> [123], ws, json_pair(Pair), json_object_rest(Pairs), ws, [125].
json_pair(pair(K,V)) --> json_string(K), ws, [58], ws, json_value(V).  % 58=':'
```

#### **ISS-2025-0041: Special Characters as Tokenizer Delimiters**  
```prolog
% ❌ FAILS: Special characters in terminal lists
question --> [does], noun_phrase, verb, noun_phrase, [?].
% Error: Expected atom name at line 1, column 2

% ✅ WORKAROUND: Use character codes
question --> [does], noun_phrase, verb, noun_phrase, [63].  % 63='?'
exclamation --> sentence, [33].  % 33='!'
semicolon_sep --> item, [59], item_list.  % 59=';'
```

#### **ISS-2025-0042: Complex Arithmetic in DCG Constraints**
```prolog
% ❌ FAILS: Complex function calls in constraints
depth(D) --> [40], depth(D1), [41], depth(D2), { D is max(D1+1, D2) }.
% Error: Expected ')' at line 1, column 14

% ✅ WORKAROUND: Use auxiliary predicates
depth(D) --> [40], depth(D1), [41], depth(D2), { max_depth(D1, D2, D) }.
max_depth(D1, D2, D) :- D1 >= D2, D is D1 + 1.
max_depth(D1, D2, D) :- D1 < D2, D is D2 + 1.
```

### **Impact Assessment**

- **85% Success Rate**: Covers all standard DCG usage patterns
- **Core Parsing**: Fully functional for practical language processing
- **ISO DCG Compliance**: Excellent compliance with DCG standard
- **Remaining Issues**: Affect only advanced/specialized parsing scenarios
- **Workarounds Available**: All limitations can be circumvented

### **Comprehensive Test Results**

**✅ WORKING (17/20 programs - 85%)**:
- Basic parsing, variables, arithmetic expressions
- List processing, calculator, XML parsing  
- State machine simulation, recursive structures
- Character validation (with character codes)
- Complex nested grammars

**❌ LIMITED (3/20 programs - 15%)**:
- JSON parsing (operator terms in heads)
- Grammar with punctuation (tokenizer conflicts)
- Mathematical validation (complex constraints)

### Testing Your DCG Rules

```prolog
% Always test your DCG rules with phrase/2:
?- phrase(your_rule, [input, list]).

% Use phrase/3 to get remaining input:
?- phrase(your_rule, [input, list], Remaining).

% For debugging, test components individually:
?- phrase(simple_part, [test]).
?- phrase(complex_rule, [full, test, input]).
```

---

**JProlog DCG Guide** - Master Definite Clause Grammars for parsing and language processing

*Version 2.0.6 | DenzoSOFT | https://denzosoft.it*

*This guide covers DCG implementation in JProlog v2.0.6. For additional parsing examples, see the comprehensive test programs in the `examples/` directory.*