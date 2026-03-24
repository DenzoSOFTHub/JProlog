# JProlog Current Limitations

This document describes current limitations in JProlog implementation.
When an issue is resolved, the corresponding limitation should be removed from this file.

**Last updated**: 2026-03-23 (v2.6.1)

---

*All known limitations (LIM-001 through LIM-016) have been resolved as of v2.6.1.*

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
| LIM-010 | Constraint Handling Rules (CHR) | v2.6.1 |
| LIM-011 | DCG advanced features (pushback, `call//N`, if-then) | v2.6.1 |
| LIM-012 | Rational numbers (`rdiv`) | v2.6.1 |
| LIM-013 | Number literal notation (`0'a`, `0xFF`, `0o77`, `0b1010`) | v2.6.1 (already implemented) |
| LIM-014 | Multi-argument indexing | v2.6.1 |
| LIM-015 | Compiled clause cache | v2.6.1 |
| LIM-016 | Atom garbage collection | v2.6.1 |

Previously resolved:
- DCG parser limitations (ISS-2025-0040, ISS-2025-0041, ISS-2025-0042) resolved by ISS-2025-0085 Pratt parser rewrite

## Notes

- This file is automatically updated when new issues are identified
- When an issue is resolved, move it to the Resolved table
- Limitations are identified by `LIM-NNN` codes for easy reference
