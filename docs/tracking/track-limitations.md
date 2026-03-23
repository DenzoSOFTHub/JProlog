# JProlog Current Limitations

This document describes current limitations in JProlog implementation.
When an issue is resolved, the corresponding limitation should be removed from this file.

**Last updated**: 2026-03-23 (v2.5.5)

---

## Critical — Missing Core Features

### LIM-001: Coroutining (`freeze/2`, `when/2`, `dif/2`)

**Severity**: Critical
**Category**: ISO Extension / Constraint Programming

Coroutining predicates are not implemented. These are essential for delayed goals and constraint-based programming.

```prolog
% These all fail with existence_error
?- freeze(X, write(hello)), X = a.
?- dif(X, Y), X = a, Y = b.
?- when(nonvar(X), write(X)), X = hello.
```

### LIM-002: Attributed Variables

**Severity**: Critical
**Category**: ISO Extension / Constraint Infrastructure

Attributed variables (`put_attr/3`, `get_attr/3`, `attr_unify_hook/2`) are not supported. These are the foundation for `freeze/2`, `dif/2`, CLP(R), and advanced constraint systems.

```prolog
% Not supported
?- put_attr(X, my_module, value).
?- get_attr(X, my_module, Value).
```

### LIM-003: Global Non-Backtrackable Variables

**Severity**: Critical
**Category**: ISO Extension / State Management

`nb_setval/2`, `nb_getval/2`, `nb_current/2`, `b_setval/2`, `b_getval/2` are not implemented. Cannot maintain persistent state across backtracking.

```prolog
% Not supported
?- nb_setval(counter, 0), nb_getval(counter, X).
?- b_setval(temp, hello), b_getval(temp, X).
```

### LIM-004: Module-Qualified Calls (Partial)

**Severity**: Critical
**Category**: Module System

Basic module framework exists (`Module.java`, `ModuleManager.java`) but module-qualified calls (`Module:Goal`) are not fully wired into the query resolver.

```prolog
% Framework exists but runtime resolution incomplete
?- lists:append([1],[2],X).
```

---

## High — ISO Compliance Gaps

### LIM-005: `predicate_property/2`

**Severity**: High
**Category**: ISO Introspection

Cannot query predicate properties (static, dynamic, built_in, defined, etc.).

```prolog
% Not supported
?- predicate_property(append(_,_,_), built_in).
?- predicate_property(foo/2, dynamic).
```

### LIM-006: `code_type/2`

**Severity**: High
**Category**: ISO Character Classification

Only `char_type/2` is implemented. `code_type/2` (character code classification) is missing.

```prolog
% Works
?- char_type(a, alpha).
% Not supported
?- code_type(97, alpha).
```

### LIM-007: Stream Repositioning

**Severity**: High
**Category**: ISO I/O

`set_stream_position/2` and `stream_position/3` are not implemented. Cannot seek or reposition in file streams.

```prolog
% Not supported
?- open('file.txt', read, S, [reposition(true)]),
   set_stream_position(S, 0).
```

### LIM-008: Arbitrary Precision Integers

**Severity**: High
**Category**: ISO Arithmetic

Arithmetic uses Java `double` throughout. No BigInteger support for arbitrary precision integers. Integer overflow is not properly handled for large numbers.

```prolog
% Precision loss for large integers
?- X is 2^100.  % Returns float approximation, not exact integer
```

### LIM-009: `read_term/2` and `write_term/2` Incomplete Options

**Severity**: High
**Category**: ISO I/O

Some ISO-required options for `read_term/2` and `write_term/2` are missing:
- `read_term`: `variable_names(Names)`, `singletons(Vars)` options
- `write_term`: full `numbervars(true)`, `quoted(true)` handling

```prolog
% Partial support — some options ignored
?- read_term(T, [variable_names(Vs), singletons(Ss)]).
?- write_term(f(X), [quoted(true), numbervars(true)]).
```

---

## Medium — Extended Features

### LIM-010: Constraint Handling Rules (CHR)

**Severity**: Medium
**Category**: Constraint Programming

CHR is not implemented. Only CLP(FD) is available for constraint programming.

### LIM-011: DCG Advanced Features

**Severity**: Medium
**Category**: Grammar Processing

Basic DCG (`-->` rules) works. Missing advanced features:
- Pushback notation
- `call//N` (meta-call in DCG context)
- Semicolon (`;`) choice in DCG rules
- Proper cut (`!`) semantics in DCG context

### LIM-012: Rational Numbers

**Severity**: Medium
**Category**: Arithmetic

No rational number support. Cannot represent exact fractions.

```prolog
% Not supported
?- X is 1 rdiv 3.  % Rational division
```

### LIM-013: Character Code and Number Literal Notation

**Severity**: Medium
**Category**: Parser

The parser does not support:
- `0'a` notation for character codes
- `0xFF` hexadecimal literals
- `0o77` octal literals
- `0b1010` binary literals

```prolog
% Not supported in parser
?- X is 0'A.      % Should be 65
?- X is 0xFF.     % Should be 255
?- X is 0b1010.   % Should be 10
```

### LIM-014: Multi-Argument Indexing

**Severity**: Medium
**Category**: Performance

Only first-argument indexing is implemented. Multi-argument indexing would significantly improve performance on large clause sets.

### LIM-015: WAM-Style Compilation

**Severity**: Medium
**Category**: Performance

Query resolution is fully interpreted (SLD resolution). No Warren Abstract Machine (WAM) bytecode compilation. This limits performance compared to compiled Prolog systems.

### LIM-016: Atom Garbage Collection

**Severity**: Medium
**Category**: Memory Management

Atoms accumulated in the symbol table are never reclaimed. Memory usage grows in long-running programs that generate many unique atoms.

---

## Notes

- This file is automatically updated when new issues are identified
- When an issue is resolved (status RESOLVED in `issues.md`), the corresponding limitation must be removed from this file
- Limitations are identified by `LIM-NNN` codes for easy reference
- Each limitation includes concrete code examples that fail to facilitate testing and verification

Previously resolved:
- DCG parser limitations (ISS-2025-0040, ISS-2025-0041, ISS-2025-0042) resolved by ISS-2025-0085 Pratt parser rewrite
