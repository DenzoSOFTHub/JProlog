# JProlog Current Limitations

This document describes current limitations in JProlog implementation.
When an issue is resolved, the corresponding limitation should be removed from this file.

**Last updated**: 2026-03-23 (v2.6.0)

---

*All Critical and High limitations have been resolved in v2.6.0.*

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

## Resolved Limitations

| ID | Feature | Resolved In |
|----|---------|-------------|
| LIM-001 | Coroutining (`freeze/2`, `when/2`, `dif/2`) | v2.6.0 |
| LIM-002 | Attributed variables (`put_attr/3`, `get_attr/3`) | v2.6.0 |
| LIM-003 | Global variables (`nb_setval/2`, `nb_getval/2`) | v2.6.0 |
| LIM-004 | Module-qualified calls (`Module:Goal`) | v2.6.0 |
| LIM-005 | `predicate_property/2` | v2.6.0 |
| LIM-006 | `code_type/2` | v2.6.0 |
| LIM-007 | Stream repositioning (`set_stream_position/2`) | v2.6.0 |
| LIM-008 | Arbitrary precision integers (BigInteger) | v2.6.0 |
| LIM-009 | `read_term/2` / `write_term/2` full options | v2.6.0 |

Previously resolved:
- DCG parser limitations (ISS-2025-0040, ISS-2025-0041, ISS-2025-0042) resolved by ISS-2025-0085 Pratt parser rewrite

## Notes

- This file is automatically updated when new issues are identified
- When an issue is resolved, move it to the Resolved table
- Limitations are identified by `LIM-NNN` codes for easy reference
