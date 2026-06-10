# JProlog Current Limitations

This document describes current limitations in JProlog implementation.
When an issue is resolved, the corresponding limitation should be removed from this file.

**Last updated**: 2026-06-10 (v3.5.0)

---

## LIM-026: retract/1 on the LEGACY engine retracts eagerly

RESOLVED for the default v2 engine in v3.6.0 (ISS-2025-0396): retract/1 is re-executable, one
clause per redo. Remaining gap: under `-Djprolog.engine=legacy` the eager built-in protocol
materializes all retract solutions in one call, so all matching clauses are retracted up front
even if the query commits early (enumeration itself is ISO-correct).

## LIM-027: open-tail generative list modes are bounded

Fully-open `append(X, Y, Z)` and open-tail `last([a|T], X)` / `maplist(G, [a|T])` produce only the
FIRST standard solution (open tails are closed with `[]`) instead of enumerating infinitely.
Sound but incomplete; consequence of the eager built-in protocol (ISS-2025-0379/0380).

## LIM-028: legacy-engine gaps vs the default v2 engine

Under `-Djprolog.engine=legacy`: CLP(FD) constraint posts are not undone on backtracking (the
legacy solver never rolls the Trail back at choice points), the four-port `trace/0` output is not
emitted, and the inference budget (`Prolog.setInferenceBudget`) is not enforced.

## LIM-030: maplist on very long lists

`maplist/2..5` translates to one right-nested conjunction; lists of ~10k+ elements may hit
recursion depth in the legacy sub-solver used for built-in bodies.

---

## Known Limitations from 2026-06-07 Implementation Audit

The audit (`docs/reports/report-implementation-audit-2026-06-07.md`) confirmed 101
findings. The eight highest-confidence, low-risk items were fixed in v3.0.0
(ISS-2025-0245..0252). The following higher-effort items remain open:

| ID | Area | Limitation |
|----|------|------------|
| ~~LIM-017~~ | Parser | **RESOLVED v3.0.0** — clean-room v2 parser (default): single-pass `Lexer` is quote/escape/char-code aware; clause splitting is token-based. |
| ~~LIM-019~~ | Parser | **RESOLVED v3.0.0** — v2 parser (default): canonical functor `-(1,2)` and operator-as-atom (`X = -`, `foo(-,+)`) handled. `-Djprolog.parser=legacy` to fall back. |
| ~~LIM-021~~ | DCG | **RESOLVED v3.0.0** — clean-room v2 DCG translator (`core.dcg.v2.DCGTranslator`, now default): single recursive pass handling ISO head push-back, `[]`/terminal lists, strings, `{}`, `!`, `\+`, `(A,B)`/`(A;B)`/`(A\|B)`/`(A->B)`, `call//N`, and variable bodies. `-Djprolog.dcg=legacy` to fall back. |
| ~~LIM-022~~ | CLP(FD) | **RESOLVED v3.0.0** — clean-room v2 CLP(FD) (now default): interval domains (no OOM, no `TreeSet`), per-query identity store (no singleton leak), trail-backtracked **sound** labeling, real `#\=` propagation, `all_different` pigeonhole. Remaining = future *features* (`global_cardinality`, Hall-interval pruning, lazy labeling), not correctness gaps. `-Djprolog.clpfd=legacy` to fall back. |
| LIM-023 | Engine | **Rescoped 2026-06-10 (v3.6.0)**: largely superseded by the default v2 engine — `core.engine.v2.MachineSolver` (default since v3.1.0) is iterative (no Java-stack recursion), and ISS-2025-0341 converts residual deep-structure `StackOverflowError`s into `resource_error` terms. Remaining: the LEGACY engine (`-Djprolog.engine=legacy`) still recurses in Java and can overflow; first-argument indexing is NOT enabled in the v2 engine (attempted and reverted, ISS-2025-0340; index maintenance has since been fixed by ISS-2025-0344, so re-landing is feasible — perf opportunity). |
| LIM-024 | Concurrency | `threading/ConcurrentPredicates` run parallel goals on a single shared `QuerySolver` whose mutable fields race; no per-task solver isolation. |
| LIM-025 | Resource | Narrowed again 2026-06-10: `with_output_to/2` still swaps JVM-wide `System.out`. Since v3.2.0 the output built-ins write through the thread-local `StreamManager.out()` (`setThreadLocalOutput`), so a cheap correct fix is now available: route `with_output_to` through the thread-local stream instead of `System.setOut`. (RESOLVED in v3.0.0: the `open/4` dangling-alias handle — ISS-0305; `StreamManager` map thread-safety, HTTP disconnect, JDBC statement leaks — ISS-0257..0260, 0265.) |

> Resolved in v3.0.0: LIM-018 (negative radix/char-code literals — ISS-2025-0256);
> LIM-020 (int/float distinguished as terms — ISS-2025-0261).

---

*Limitations LIM-001 through LIM-016 were all resolved as of v2.6.1.*

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
