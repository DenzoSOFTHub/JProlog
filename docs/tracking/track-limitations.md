# JProlog Current Limitations

This document describes current limitations in JProlog implementation. Each limitation corresponds to an open issue in `issues.md`. When an issue is resolved, the corresponding limitation should be removed from this file.

---

## ISS-2025-0008: Variable Unification Fails After TermCopier Renaming in DCG

**Descrizione**: Le variabili nelle query DCG non vengono unificate correttamente con i risultati del parsing (parzialmente risolto).

**Esempi che falliscono** (updated status):
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

## ~~ISS-2025-0011: CLI Input Processing Issues~~ ✓ RESOLVED

**Descrizione**: ~~Il CLI non riconosce alcuni comandi standard e ha problemi con input da file/pipe.~~ **FIXED 2025-08-20**

**Status**: ✅ **RESOLVED** - CLI command parsing now handles trailing periods correctly

**Examples now working**:
```prolog
% Commands with periods now work correctly
?- :listing.
% Result: ✓ Shows loaded predicates correctly

% Input from file/pipe works correctly  
$ echo ":help" | java -cp target/classes it.denzosoft.jprolog.PrologCLI
% Result: ✓ Shows help and processes input correctly
```

**Fix**: Modified `handleCommand()` in `PrologCLI.java` to strip trailing periods from commands

---

## ISS-2025-0013: Critical QuerySolver StackOverflowError During Complex DCG Parsing

**Descrizione**: StackOverflowError nel QuerySolver con regole DCG complesse che usano built-in predicates.

**Esempi che falliscono**:
```prolog
% Complex DCG with built-in predicates
?- consult("digit(D) --> [D], { D \\= [], between(48, 57, D) }.").
?- expr(N, [49], []).
% Expected: Successful parsing
% Actual: StackOverflowError
```

---

## ISS-2025-0014: Parser Limitations - Advanced ISO Prolog Syntax Not Supported

**Descrizione**: Il parser non supporta diverse costruzioni sintattiche avanzate ISO Prolog.

**Esempi che falliscono**:
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

**Descrizione**: Predicati built-in mancanti per operazioni matematiche avanzate.

**Esempi che falliscono**:
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

**Descrizione**: Funzionalità meta-programmazione mancanti inclusa quantificazione esistenziale.

**Esempi che falliscono**:
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

## ISS-2025-0017: Critical ISO Arithmetic Compliance Failures

**Descrizione**: Operatori aritmetici ISO standard non funzionanti.

**Esempi che falliscono** (partially fixed):
```prolog
% FIXED: Arithmetic equality - now works correctly
% ?- 5 =:= 5.
% Expected: true
% Actual: ✓ true

% FIXED: Arithmetic inequality - now works correctly  
% ?- 5 =\= 3.
% Expected: true
% Actual: ✓ true

% Remainder operator - still failing
?- X is 17 rem 5.
% Expected: X = 2
% Actual: Parse error - 'rem' not recognized

% Bitwise AND - still failing
?- X is 5 /\ 3.
% Expected: X = 1
% Actual: Parse error

% Bitwise OR - still failing
?- X is 5 \/ 3.
% Expected: X = 7
% Actual: Parse error

% Bitwise NOT
?- X is \\ 5.
% Expected: X = -6
% Actual: Parse error

% Left shift
?- X is 5 << 1.
% Expected: X = 10
% Actual: Evaluation error

% Right shift
?- X is 10 >> 1.
% Expected: X = 5
% Actual: Evaluation error
```

---

## ~~ISS-2025-0018: ISO Term Manipulation Predicates~~ ✓ RESOLVED

**Descrizione**: ~~Tutti i predicati standard ISO per manipolazione termini non funzionano.~~ **FIXED 2025-08-20**

**Status**: ✅ **RESOLVED** - All term manipulation predicates now work correctly

**Examples now working**:
```prolog
% functor/3 - now works correctly
?- functor(f(a,b), F, A).
% Result: F = f, A = 2

% arg/3 - now works correctly
?- arg(1, f(a,b,c), X).
% Result: X = a  

% Univ operator - now works correctly
?- f(a,b) =.. L.
% Result: L = [f,a,b] (internal dot notation format)

% copy_term/2 - now works correctly
?- copy_term(f(X,X), f(Y,Y)).
% Result: true (with Y as new variable)
```

**Fix**: Added missing entries in BuiltInRegistry.isBuiltIn() for functor/3, arg/3, =../2

---

## ISS-2025-0019: ISO List Representation Format Issues

**Descrizione**: Liste rappresentate con dot notation invece di formato ISO standard.

**Esempi che falliscono**:
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

**Workaround**: Le liste funzionano correttamente, solo il formato di output non è ISO-compliant. Il sistema interpreta correttamente sia input che processing.

---

## ~~ISS-2025-0020: Control Structures~~ ✓ RESOLVED

**Descrizione**: ~~Operatori di controllo fondamentali come disgiunzione e if-then-else non funzionano.~~ **FIXED 2025-08-20**

**Status**: ✅ **RESOLVED** - All control structure operators now work correctly

**Examples now working**:
```prolog
% Disjunction (OR) - now works correctly
?- (true ; false).
% Result: ✓ true

% Disjunction with false first - now works correctly
?- (false ; true).
% Result: ✓ true

% If-then-else - now works correctly
?- (5 > 3 -> true ; false).
% Result: ✓ true

% Cut - now works correctly
?- !.
% Result: ✓ true
```

**Fix**: Added missing entries in BuiltInRegistry.isBuiltIn() for ->/2, ;/2, !/0

---

## ISS-2025-0021: Atom Operations Predicates Missing or Non-Functional

**Descrizione**: La maggioranza dei predicati ISO per manipolazione atomi non funziona.

**Esempi che falliscono**:
```prolog
% atom_length/2
?- atom_length(hello, N).
% Expected: N = 5
% Actual: No solutions found

% atom_concat/3
?- atom_concat(hello, world, X).
% Expected: X = helloworld
% Actual: No solutions found

% sub_atom/5
?- sub_atom(hello, 1, 3, 1, X).
% Expected: X = ell
% Actual: No solutions found

% atom_chars/2
?- atom_chars(hello, L).
% Expected: L = [h,e,l,l,o]
% Actual: No solutions found
```

---

## ISS-2025-0022: Meta-Predicates bagof/3 and setof/3 Non-Functional

**Descrizione**: I meta-predicati bagof/3 e setof/3 non funzionano mentre findall/3 funziona.

**Esempi che falliscono**:
```prolog
% bagof/3
?- bagof(X, likes(mary, X), L).
% Expected: L = [food, wine] (with possible duplicates)
% Actual: No solutions found

% setof/3
?- setof(X, likes(mary, X), L).
% Expected: L = [food, wine] (sorted, unique)
% Actual: No solutions found

% forall/2
?- forall(member(X, [1,2,3]), number(X)).
% Expected: true
% Actual: false
```

**Workaround**: Usare `findall/3` che funziona correttamente:
```prolog
?- findall(X, likes(mary, X), L).
% Works correctly
```

---

## ISS-2025-0040: DCG Parser Cannot Handle Compound Operator Terms in List Heads

**Descrizione**: DCG rules con compound terms contenenti operatori (`K-V`) dentro list structures nei rule heads non possono essere parsate correttamente.

**Esempi che falliscono**:
```prolog
% DCG rule con compound operator term in list head fallisce:
json_object([K-V|Pairs]) --> [123], ws, json_pair(K-V), json_object_rest(Pairs), ws, [125].
% Expected: DCG rule loaded and functional
% Actual: Error: Expected ')' at line 1, column 12

% Altri esempi che falliscono:
key_value_list([Name-Value|Rest]) --> identifier(Name), [61], value(Value), key_value_rest(Rest).
% Expected: Parse key-value pairs
% Actual: Parser error on compound term in list head
```

**Workaround**: Usare strutture separate invece di compound terms inline:
```prolog
% Workaround: Define separate structure
json_object([Pair|Pairs]) --> [123], ws, json_pair(Pair), json_object_rest(Pairs), ws, [125].
json_pair(pair(K,V)) --> json_string(K), ws, [58], ws, json_value(V).
```

---

## ISS-2025-0041: DCG Parser Fails on Special Characters Due to Tokenizer Delimiters

**Descrizione**: DCG rules contenenti caratteri speciali come `?` in terminal lists falliscono perché questi caratteri sono definiti come delimitatori del tokenizer.

**Esempi che falliscono**:
```prolog
% DCG rule con carattere ? fallisce:
question --> [does], noun_phrase, verb, noun_phrase, [?].
% Expected: DCG rule loaded and functional
% Actual: Error: Expected atom name at line 1, column 2

% Altri caratteri speciali che falliscono:
exclamation --> sentence, [!].  % Fails due to ! in tokenizer
semicolon_sep --> item, [;], item_list.  % Fails due to ; in tokenizer
```

**Workaround**: Usare character codes invece di character literals:
```prolog
% Workaround: Use character codes
question --> [does], noun_phrase, verb, noun_phrase, [63].  % 63 is ASCII for '?'
exclamation --> sentence, [33].  % 33 is ASCII for '!'
semicolon_sep --> item, [59], item_list.  % 59 is ASCII for ';'
```

---

## ISS-2025-0042: DCG Constraint Goals Cannot Handle Complex Arithmetic Functions

**Descrizione**: DCG rules con complex arithmetic function calls (`max()`) dentro constraint goals `{ }` non possono essere parsate correttamente.

**Esempi che falliscono**:
```prolog
% DCG rule con max() function call fallisce:
depth(D) --> [40], depth(D1), [41], depth(D2), { D is max(D1+1, D2) }.
% Expected: DCG rule loaded and functional  
% Actual: Error: Expected ')' at line 1, column 14

% Altri function calls complessi che falliscono:
range_check(N) --> digits(Ds), { length(Ds, L), N is min(L, 10) }.
% Expected: Parse with length constraint
% Actual: Parser error on complex constraint
```

**Workaround**: Semplificare le constraints o usare predicati ausiliari:
```prolog
% Workaround: Use auxiliary predicates
depth(D) --> [40], depth(D1), [41], depth(D2), { max_depth(D1, D2, D) }.
max_depth(D1, D2, D) :- D1 >= D2, D is D1 + 1.
max_depth(D1, D2, D) :- D1 < D2, D is D2 + 1.

% Or use simpler arithmetic:
simple_depth(D) --> [40], simple_depth(D1), [41], { D is D1 + 1 }.
```

---

## Note

- Questo file viene aggiornato automaticamente quando vengono identificate nuove issue
- Quando una issue viene risolta (status RESOLVED in `issues.md`), la corrispondente limitazione deve essere rimossa da questo file
- Le limitazioni sono ordinate per numero di issue (ISS-YYYY-NNNN)
- Ogni limitazione include esempi concreti di codice che fallisce per facilitare testing e verifica