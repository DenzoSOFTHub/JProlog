# JProlog Current Limitations

This document describes current limitations in JProlog implementation. Each limitation corresponds to an open issue in `issues.md`. When an issue is resolved, the corresponding limitation should be removed from this file.

---

## ISS-2025-0008: Variable Unification Fails After TermCopier Renaming in DCG

**Description**: Variables in DCG queries are not correctly unified with parsing results (partially resolved).

**Failing Examples** (updated status):
```prolog
% PARTIALLY FIXED: DCG rule parsing now works but variable binding incomplete
?- phrase(digits(Ds), [49,50,51]), number_codes(N, Ds).
% Expected: N = 123
% Actual: phrase succeeds but Ds is unbound → number_codes fails

% Progress made (2025-08-20):
% ✅ phrase(digits(Ds), [49,50,51]) now finds 1 solution (was 0)
% ❌ Ds variable not bound in solution (should be [49,50,51])
```

**Partial Fix Applied**: DCG transformation in Parser.java now works correctly

**Remaining Issue**: Variable binding propagation from DCG parsing to query result

---

## ISS-2025-0013: Critical QuerySolver StackOverflowError During Complex DCG Parsing

**Description**: StackOverflowError in QuerySolver with complex DCG rules that use built-in predicates.

**Failing Examples**:
```prolog
% Complex DCG with built-in predicates
?- consult("digit(D) --> [D], { D \\= [], between(48, 57, D) }.").
?- expr(N, [49], []).
% Expected: Successful parsing
% Actual: StackOverflowError
```

---

## ISS-2025-0014: Parser Limitations - Advanced ISO Prolog Syntax Not Supported

**Description**: Parser does not support various advanced ISO Prolog syntax constructions.

**Failing Examples**:
```prolog
% Braces syntax for compound terms
?- complex_match(data(X, [H|T], {key: Value}), Result).
% Expected: Parses correctly
% Actual: Parse error at '{'

% Mathematical functions
?- X is sqrt(16).
% Expected: X = 4.0
% Actual: Parse error - sqrt not recognized

% Directive syntax
?- :- dynamic(test/1).
% Expected: Directive processed
% Actual: Parse error

% Univ operator
?- f(a,b) =.. L.
% Expected: L = [f,a,b]
% Actual: Parse error at '=..'

% Existential quantification
?- bagof(Grade, Student^student(Student, math, Grade), Grades).
% Expected: Collects grades
% Actual: Parse error at '^'
```

---

## ISS-2025-0015: Missing Advanced Built-in Predicates for Mathematical Operations

**Description**: Missing built-in predicates for advanced mathematical operations.

**Failing Examples**:
```prolog
% Square root
?- X is sqrt(16).
% Expected: X = 4.0
% Actual: Unknown arithmetic function

% Keysort
?- keysort([3-a, 1-b, 2-c], Sorted).
% Expected: Sorted = [1-b, 2-c, 3-a]
% Actual: Predicate not found
```

---

## ISS-2025-0016: Meta-Programming Features Missing

**Description**: Meta-programming features missing including existential quantification.

**Failing Examples**:
```prolog
% Existential quantification with bagof
?- bagof(Grade, Student^student(Student, math, Grade), Grades).
% Expected: Collects all math grades
% Actual: Parse error at '^'

% Univ operator for term decomposition
?- f(a,b) =.. [f,a,b].
% Expected: true
% Actual: Parse error at '=..'

% Complex call/1
?- call((X = 5, Y is X + 1)).
% Expected: X = 5, Y = 6
% Actual: May fail depending on goal complexity
```

---

## ISS-2025-0019: ISO List Representation Format Issues

**Description**: Lists represented with dot notation instead of ISO standard format.

**Failing Examples**:
```prolog
% List append result format
?- append([a,b], [c,d], X).
% Expected: X = [a,b,c,d]
% Actual: X = .(a, .(b, .(c, .(d, []))))

% Findall result format
?- findall(X, member(X, [1,2,3]), L).
% Expected: L = [1,2,3]
% Actual: L = .(1, .(2, .(3, [])))
```

**Workaround**: Lists work correctly, only the output format is not ISO-compliant. The system correctly interprets both input and processing.

---

## ISS-2025-0040: DCG Parser Cannot Handle Compound Operator Terms in List Heads

**Description**: DCG rules with compound terms containing operators (`K-V`) inside list structures in rule heads cannot be parsed correctly.

**Failing Examples**:
```prolog
% DCG rule with compound operator term in list head fails:
json_object([K-V|Pairs]) --> [123], ws, json_pair(K-V), json_object_rest(Pairs), ws, [125].
% Expected: DCG rule loaded and functional
% Actual: Error: Expected ')' at line 1, column 12

% Other failing examples:
key_value_list([Name-Value|Rest]) --> identifier(Name), [61], value(Value), key_value_rest(Rest).
% Expected: Parse key-value pairs
% Actual: Parser error on compound term in list head
```

**Workaround**: Use separate structures instead of compound terms inline:
```prolog
% Workaround: Define separate structure
json_object([Pair|Pairs]) --> [123], ws, json_pair(Pair), json_object_rest(Pairs), ws, [125].
json_pair(pair(K,V)) --> json_string(K), ws, [58], ws, json_value(V).
```

---

## ISS-2025-0041: DCG Parser Fails on Special Characters Due to Tokenizer Delimiters

**Description**: DCG rules containing special characters like `?` in terminal lists fail because these characters are defined as tokenizer delimiters.

**Failing Examples**:
```prolog
% DCG rule with ? character fails:
question --> [does], noun_phrase, verb, noun_phrase, [?].
% Expected: DCG rule loaded and functional
% Actual: Error: Expected atom name at line 1, column 2

% Other special characters that fail:
exclamation --> sentence, [!].  % Fails due to ! in tokenizer
semicolon_sep --> item, [;], item_list.  % Fails due to ; in tokenizer
```

**Workaround**: Use character codes instead of character literals:
```prolog
% Workaround: Use character codes
question --> [does], noun_phrase, verb, noun_phrase, [63].  % 63 is ASCII for '?'
exclamation --> sentence, [33].  % 33 is ASCII for '!'
semicolon_sep --> item, [59], item_list.  % 59 is ASCII for ';'
```

---

## ISS-2025-0042: DCG Constraint Goals Cannot Handle Complex Arithmetic Functions

**Description**: DCG rules with complex arithmetic function calls (`max()`) inside constraint goals `{ }` cannot be parsed correctly.

**Failing Examples**:
```prolog
% DCG rule with max() function call fails:
depth(D) --> [40], depth(D1), [41], depth(D2), { D is max(D1+1, D2) }.
% Expected: DCG rule loaded and functional  
% Actual: Error: Expected ')' at line 1, column 14

% Other complex function calls that fail:
range_check(N) --> digits(Ds), { length(Ds, L), N is min(L, 10) }.
% Expected: Parse with length constraint
% Actual: Parser error on complex constraint
```

**Workaround**: Simplify constraints or use auxiliary predicates:
```prolog
% Workaround: Use auxiliary predicates
depth(D) --> [40], depth(D1), [41], depth(D2), { max_depth(D1, D2, D) }.
max_depth(D1, D2, D) :- D1 >= D2, D is D1 + 1.
max_depth(D1, D2, D) :- D1 < D2, D is D2 + 1.

% Or use simpler arithmetic:
simple_depth(D) --> [40], simple_depth(D1), [41], { D is D1 + 1 }.
```

---

## Notes

- This file is automatically updated when new issues are identified
- When an issue is resolved (status RESOLVED in `issues.md`), the corresponding limitation must be removed from this file
- Limitations are ordered by issue number (ISS-YYYY-NNNN)
- Each limitation includes concrete code examples that fail to facilitate testing and verification