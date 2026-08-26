# JProlog Current Limitations

This document describes current limitations in JProlog implementation.
When an issue is resolved, the corresponding limitation should be removed from this file.

**Last updated**: 2026-08-26 (v4.0.0, wave W9)

---

## LIM-037: a large part of the library still runs on the eager built-in bridge

**Wave W9 is done** (v4.0.0, ISS-2025-0484..0488): the recursive `QuerySolver` engine is deleted,
`BuiltInWithContext` is typed against the `SolverContext` interface, `CollectionBuiltInAdapter`,
`CutStatus`, `MutableCutStatus`, `LayeredMap` and the seven control-construct built-ins are gone,
and the v4 engine has no name-keyed hop left. The v2 `MachineSolver` stays selectable for this
release with `-Djprolog.engine=v2` (design decision 1, B.17) and is deleted in 4.1; there is no
`legacy` value any more.

What is left of this limitation is limit **L-08** of the design. Of the 416 registered predicate
names, 63 are v4 natives and about 40 more are control constructs or inline built-ins the machine
handles itself and never dispatches (`,/2`, `;/2`, `->/2`, `\\+/1`, `call/N`, `catch/3`, `!/0`,
`=/2`, `is/2`, the arithmetic and term comparisons, the type checks, `once/1`, `ignore/1`,
`forall/2`, `between/3`, `repeat/0`); the remaining **~310 still run through
`core.engine.v4.LegacyBuiltinAdapter`** with the eager
`(goal, Map<String,Term>, List<Map<String,Term>>)` contract. Nothing they do reaches a recursive
solver (there is none), and a deterministic one costs only two extra term walks per call — the
adapter dereferences the goal and indexes its unbound cells by name — but a *nondeterministic* one
still materialises every solution before the first is delivered.

Native on v4 (65 indicators, so the adapter is never involved): the list library (`member/2`,
`memberchk/2`, `append/3`, `select/3`, `selectchk/3`, `nth0/3`, `nth1/3`, `last/2`, `reverse/2`,
`length/2`, `msort/2`, `sort/2`, `sort/4`, `predsort/3`, `sum_list/2`, `sumlist/2`, `numlist/3`,
`max_list/2`, `min_list/2`), the control and collection predicates (`phrase/2,3`, `bagof/3`,
`setof/3`, `aggregate_all/3`, `with_output_to/2`), the term predicates (`copy_term/2,3`,
`term_variables/2`, `ground/1`, `numbervars/3`, `subsumes_term/2`, `compare/3`, `setarg/3`,
`nb_setarg/3`, `cyclic_term/1`, `acyclic_term/1`, `unifiable/3`, `term_attvars/2`), the database
and module predicates (`clause/2`, `predicate_property/2`, `current_module/1`), the atom/string
slicers (`sub_atom/5`, `sub_string/5`), coroutining (`put_attr/3`, `get_attr/3`, `del_attr/2`,
`attvar/1`), tabling (`abolish_all_tables/0`, `abolish_table/1`, `current_table/2`), CLP(FD)
(`in/2`, `#=`, `#\=`, `#<`, `#>`, `#=<`, `#>=`, `all_different/1`, `all_distinct/1`, `label/1`,
`labeling/2`), the operator store (`current_op/3`) and the global variables (`nb_getval/2`,
`b_getval/2`). `maplist/2..7`, `foldl/4..7`, `include/3`, `exclude/3`, `partition/4,5`,
`member/2`, `append/3`, `freeze/2`, `frozen/2`, `when/2`, `dif/2` and `?=/2` are Prolog clauses in
`src/main/resources/prelude/`.

Still on the adapter, and why: the atom/string/character library, the arithmetic comparisons that
are inlined by the machine anyway, the type checks (likewise inlined), `keysort/2`, the I/O family,
`format/2,3`, `read_term/2,3`, `write_term/2,3`, the assert/retract/abolish/listing family (shared
with the `KnowledgeBase`, the IDE and the v2 engine), `op/3`, `statistics/2`, the debug and
profiler predicates, and the whole extended library (CSV, JSON, XML, HTTP, JDBC, crypto, datetime,
filesystem, graph, logging, network, os, persistence, regex, threading, FFI). None of them is on a
measured hot path and each would be a separate benchmark-backed change; migrating them is an
open-ended 4.1 item, not a correctness gap.

Two behaviour differences on v4 are deliberate and approved (design B.17): rational trees are
supported (queries that raised `representation_error(cyclic_term)` now succeed;
`set_prolog_flag(occurs_check, error)` restores the ISO behaviour), and
`setup_call_cleanup/3`/`call_cleanup/2` run `Cleanup` after the goal's LAST solution rather than
eagerly after the first.

Since v4.0.0 these are simply how JProlog behaves; they are listed because `-Djprolog.engine=v2`
still gets the old behaviour: `library(yall)` lambdas work (`existence_error(>>/4)` on v2),
`partition/4` exists (unregistered on v2), `sub_atom(A, B, L, Af, '')` terminates (it spins forever
on v2), the bindings of a `when/2`-woken goal propagate (ISS-2025-0336), `frozen/2`,
`term_attvars/2`, `copy_term/3`, `unifiable/3`, `?=/2`, `current_table/2`, `current_module/1` and
`memberchk/2` exist only on v4, **tabling is complete** rather than a bounded 100-round
re-evaluation, `append(X, Y, Z)` fully open enumerates and `member(X, PartialList)` extends the
open tail, cyclic terms are supported, query memory is O(live data) rather than O(bindings), the
fast paths are not disabled while debugging, and **module-qualified built-in calls, autoloaded
library modules and `meta_predicate/1` work**.

## LIM-039: RESOLVED in 4.0.0 (wave W9) — a tabled evaluation is claimed by one thread

Since v4.0.0 `thread_create/2,3` and the `concurrent_*` family run on their own machines over the
same engine (LIM-024), and the engine's tabling store is shared with them. It was not thread-safe:
one variant table, one producing stack and one answer list, all plain `HashMap`/`ArrayList`, so two
threads *producing* interleaved their bookkeeping and a consumer could read a half-produced answer
set as if it were complete.

**ISS-2025-0488** claims an evaluation for one thread, on **both** engines:

- a tabled CALL runs inside `Tabling.enterCall`/`exitCall` (v4) or `TableStore.enterCall`/`exitCall`
  (v2), so "does this variant exist, is it complete, do I produce it" and the frame it installs are
  atomic;
- the claim is **held for the whole evaluation** — until the SCC completes, is abandoned, or the
  query ends — so a second thread never sees an EVALUATING table that is not its own;
- a worker machine hands the claim back when it finishes (`Tabling.endWorker`), abandoning whatever
  it left EVALUATING, because a worker owns no query boundary;
- reading a COMPLETE table stays effectively parallel: a consumer holds the claim only for the
  duration of the call decision.

Residual, deliberate: the wait is bounded at 60 s and then raises
`resource_error(tabling_busy)` rather than hanging, and a tabled goal that fans out into workers
which are *themselves* tabled serialises them (and, if the parent's evaluation cannot finish
without them, hits that timeout). Running several top-level `Prolog.solve` calls concurrently on
ONE `Prolog` instance is still outside the contract on both engines — a top-level query owns the
engine-wide query boundary and abandons every EVALUATING table when it ends; use
`concurrent_maplist/2,3,4`, `concurrent/3` or `thread_create/2,3` from one query instead.

## LIM-036: .jpc compilation stores source lines, but only for the legacy-parser path

`.jpc` format 0x03 (ISS-2025-0447) carries `Rule.sourceLine`, and `Prolog.compile` stamps it from
the clause start lines `Parser.extractClauses` now records. That is the LEGACY parser's clause
splitter; a clause whose head sits on a different line from the first token of the clause text
(after a block comment, say) gets the line of the first token. `consultWithDiagnostics` on the v2
parser path remains the accurate source of line information for the IDE.

## LIM-034: RESOLVED in v3.14.0 — engine state is per engine

v3.8.0 (ISS-2025-0437) made the ISO flag store, `occurs_check`, `trace/0` and the current
input/output stream per engine (or per thread). **v3.14.0 (wave W7, ISS-2025-0472/0474/0477)
finishes the job**: the stream table and its aliases, the operator store (`op/3`, `current_op/3`,
the parser table, the `.jpc` writer and the IDE formatter all read the one store), the spy points
and the profiler counters now belong to the `Prolog` instance. Each engine owns a
`core.engine.v4.EngineState` and installs it as the thread-current state around every
solve/consult; `StreamManager`, `OperatorDefinition`, `Spy` and `Profiler` keep their static APIs as
facades over it, so the legacy and v2 engines and any embedder code are unaffected. Reach a
specific engine's state with `Prolog.getEngineState()`, `Prolog.getStreams()`, `Prolog.getOps()`.

What is still shared by every `Prolog` in the JVM:

- the legacy `ConstraintStore.getInstance()` singleton (only reachable under
  `-Djprolog.clpfd=legacy`);
- the stream-handle counter (deliberately: `stream_1001` must denote at most one stream in the JVM);
- `StreamManager.setThreadLocalOutput` — a per-**thread** override by design, because the IDE
  installs it before any engine is current on the worker thread.

Consequence for embedders: `Prolog.enableSafeMode()` plus a fresh instance is now a real boundary
for streams, operators, spy points and the profiler as well as the flags. A separate JVM is still
the answer for a hard security boundary (`builtin.io` is deliberately not in the safe-mode deny
list, so `open/3,4` can still reach the host filesystem).

## LIM-027: open-tail generative list modes are bounded on the v2 FALLBACK engine

Under `-Djprolog.engine=v2`, fully-open `append(X, Y, Z)` and open-tail `last([a|T], X)` /
`maplist(G, [a|T])` produce only the FIRST standard solution (open tails are closed with `[]`)
instead of enumerating infinitely. Sound but incomplete; a consequence of the eager built-in
protocol (ISS-2025-0379/0380).

**RESOLVED on the default v4 engine** in v3.13.0 (wave W6, ISS-2025-0468): `member/2` and
`append/3` are the two-clause definitions of `src/main/resources/prelude/lists.pl`, so
`append(X, Y, Z)` enumerates and `member(X, PartialList)` extends the open tail. That is a
deliberate divergence from v2 — a program that relied on the bounded behaviour to terminate now
loops (`testISS0379_AppendFullyOpenDoesNotThrow` has an engine-aware branch).

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
| ~~LIM-023~~ | Engine | **RESOLVED in 4.0.0** (waves W1-W9). The recursive engine that recursed in Java is deleted (ISS-2025-0484); the default `core.engine.v4.Machine` is iterative over mutable cells with compiled clause skeletons and first-argument indexing, every term walker is iterative and cycle-safe, and residual deep-structure `StackOverflowError`s in bridged built-ins still convert to `resource_error` terms (ISS-2025-0341). The v2 `MachineSolver` fallback (`-Djprolog.engine=v2`) keeps its own first-argument index (ISS-2025-0433). |
| ~~LIM-024~~ | Concurrency | **RESOLVED in v4.0.0** (wave W8, ISS-2025-0479/0480). `thread_create/2,3` and the `concurrent_*` family run every goal on a **fresh `core.engine.v4.Machine` over the same `Engine`** (`core.engine.v4.Workers`): shared clause store (thread-safe by birth/death generations), shared flags/operators/modules, per-thread current streams, one `ResourceGuard` per worker carrying the parent's budget, and a `copy_term`'d goal so no `Variable` cell is shared between machines. Interrupting the parent cancels the workers. On `-Djprolog.engine=v2`/`legacy` the old shared-solver behaviour remains (the recursive engine is deleted in 4.0.0; the v2 fallback keeps the shared-context behaviour until it is deleted in 4.1). |
| LIM-025 | Resource | **RESOLVED in v3.14.0** (wave W7, ISS-2025-0472): `with_output_to/2` on **both** engines, `format/3` with `atom/string/codes/chars`, `format ~@` and the `~p` portray path capture through the per-thread `StreamManager` output override alone — no `System.setOut`, no `user_output` stream swap — so two threads can capture concurrently and a capture never garbles unrelated output. The last built-ins that printed to `System.out` directly (`spy/1`, `nospy/1`, `leash/1`, `debugging/0`) were routed through `StreamManager.out()`. (Earlier: RESOLVED in v3.0.0 the `open/4` dangling-alias handle — ISS-0305; `StreamManager` map thread-safety, HTTP disconnect, JDBC statement leaks — ISS-0257..0260, 0265.) |

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
