# Changelog

All notable changes to JProlog will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [4.2.0] - 2026-08-26

### Wave B of 4.1: the hot and ISO-core built-in families leave `LegacyBuiltinAdapter`

Item 2 of the 4.1 plan (section 15.6 / 16.6 of the engine report): the eager registry built-ins
move to the **v4 native SPI**, family by family, each with the §16.6 measurement method — pin the
behaviour first, migrate, then A/B the same session. Wave record, invariants and the starting point
for the next wave are in `docs/reports/report-engine-v4-progress.md` (new section 17).
ISS-2025-0496..0501.

A bridged built-in reaches the machine through `LegacyBuiltinAdapter`, whose first act is
`Unify.resolve` of the WHOLE goal, followed by a `HashMap` of the goal's cells and one
`Map<String,Term>` per solution. For `write(BigTerm)`, `format("~w", [BigTerm])` or
`keysort(BigList, _)` that is a complete copy of the argument before a single character is printed
or a single pair compared. A v4 native sees the argument cells and acts on the machine directly.

**Migrated** (94 predicate indicators, 4 new files in `core.engine.v4`, 2 622 lines):

| ISS | Family | Predicates |
|---|---|---|
| 0496 | `io` — `NativeIo` | `format/1,2,3` (every directive, the column stops, the four capture sinks), `write/1,2`, `writeln/1,2`, `writeq/1,2`, `print/1,2`, `write_canonical/1,2`, `write_term/2,3`, `nl/0,1`, `tab/1,2`, `put_char/1,2`, `put_code/1,2`, `get_char/1,2`, `get_code/1,2`, `peek_char/1,2`, `peek_code/1,2`, `flush_output/0,1`, `current_input/1`, `current_output/1`, `set_input/1`, `set_output/1`, `at_end_of_stream/0,1` |
| 0497 | `atom`/`string`/`character`/`conversion` + the 3 eager `list` ones — `NativeText` | `atom_length/2`, `atom_concat/3`, `atom_chars/2`, `atom_codes/2`, `char_code/2`, `upcase_atom/2`, `downcase_atom/2`, `number_chars/2`, `number_codes/2`, `atom_number/2`, `atom_string/2`, `number_string/2`, `string_to_atom/2`, `string_chars/2`, `string_codes/2`, `string_length/2`, `string_concat/3`, `string_code/3`, `split_string/4`, `atomic_list_concat/2,3`, `term_to_atom/2`, **`term_string/2` (new)**, `keysort/2`, `delete/3`, `flatten/2` |
| 0498 | `term`/`type`/`unification`/`arithmetic` — `NativeTerm` | `functor/3`, `arg/3`, `=../2`, `atom_to_term/3`, `number_vars/3`, `succ/2`, `plus/3`, `is_list/1`, `proper_list/1`, `partial_list/1`, `simple/1`, `string/1`, `must_be/2`, `unify_with_occurs_check/2` |
| 0499 | `database`/`system`/`exception` — `NativeDb` | `current_predicate/1`, `retractall/1`, `abolish/1`, `dynamic/1`, `listing/0,1`, `nb_setval/2`, `b_setval/2`, `nb_current/2`, `nb_delete/1`, `current_prolog_flag/2`, `set_prolog_flag/2`, `halt/0,1`, **`findall/4` (new)** |
| 0501 | — | `permission_error` protection widened from the legacy registry to the v4 native table and the prelude exports |

**Names that can still reach the adapter: 305 -> 234** (`scratchpad/41b/probe/Fam2.java` counts them
from a live `Prolog`: 410 registered names, minus 121 shadowed by a native, minus 44 the machine
handles inline, minus 11 a prelude library defines). By §16.6's headline metric — registered names
not shadowed by a native — **361 -> 289**. What is left is the extended libraries (jdbc 28,
filesystem 15, threading 15, crypto 14, ffi 14, graph 13, network 13, persistence 13, os 12,
http 11, datetime 10, json/logging/regex 6 each, dcg 5, csv 4, xml 3, clpfd 3), the 19 remaining
`io` predicates (`open`, `close`, `read`, `read_term`, `stream_property` and the byte I/O),
`op/3`, `statistics/2`, `char_type/2`, `code_type/2`, `table/1` and the 11 `debug` predicates —
that residual is the new scope of LIM-037.

**New behaviour**

- **`term_string/2`** — the SWI string twin of `term_to_atom/2`, in both directions.
- **`findall/4`** — `findall(Template, Goal, List, Tail)`: `List` ends in `Tail` instead of `[]`.
- **`listing/1` works at all.** `BuiltInFactory` binds ONE implementation per NAME, and `listing`
  was bound to `Listing0`, whose first statement rejects any argument: `listing(foo/1)` raised
  "listing/0 takes no arguments" in every release that documented it. The v4 table is keyed by
  `(name, arity)`, so `listing/0` and `listing/1` are two entries. A bare name lists every arity
  (`listing(parent)`), as the reference has always shown. `builtin.database.Listing1` was dead code
  and stays unregistered.
- **`arg/3` enumerates** with an unbound index (`arg(N, f(a,b), X)` gives `N=1,X=a ; N=2,X=b`)
  instead of raising `instantiation_error`; ISO 8.5.2, and a lazy generator, so `once/1` stops it.
- **`functor/3` decomposes a non-ground compound.** `functor(f(X,b), N, A)` raised
  `instantiation_error` because the registry version chose its mode with `Term.isGround()`; ISO
  8.5.1 decomposes any non-variable first argument, so it now answers `N=f, A=2`.
- **`writeq/2` is captured.** It resolved its stream through `StreamManager.getOutputStream(alias)`,
  the static map that captured `System.out` at class-load, so it escaped `with_output_to/2` and the
  IDE console that `writeq/1` already honoured — and raised a non-ISO evaluation error for a
  non-atom stream. It resolves its stream exactly like `write/2` now.
- **`put_code/2` exists.** It had an arity entry but the class threw
  "put_code/1 requires exactly 1 argument".
- **`listing/0,1` print through `StreamManager.out()`** (invariant 11) instead of `System.out`, so
  `with_output_to/2` and the IDE's per-thread console capture them; and a clause is no longer
  printed with a doubled full stop (`foo(a)..`), because `Rule.toString()` already ends in one.
- **ISS-2025-0501 pays off 4.1-A deviation 4**: `assertz`/`retract`/`retractall`/`abolish`/`clause`
  raise `permission_error(modify|access, static_procedure|private_procedure, PI)` again for
  `freeze/2`, `when/2`, `dif/2`, `put_attr/3`, `get_attr/3`, `del_attr/2` and `attvar/1` — and, by
  the same rule, for every v4 native and every prelude export. Wave A had deleted their Java
  classes and with them their registry entries, and `BuiltInRegistry.isBuiltIn` needs a registration
  AND an arity entry, so `assertz(freeze(X, Y))` had quietly become legal. The documented
  library-override rule is untouched: a module that DEFINES `partition/4` in its source still
  overrides the library one, because consult checks the registry (`Prolog.checkBuiltInConflict`) and
  never comes through `Machine.checkModifiable`.

**Deliberately NOT changed**: the sandbox deny list (`builtin.io` stays allowed), the four-port
trace contract (`EngineV4TraceTest`'s 16 pinned oracles are byte-identical — a native emits the same
Call/Exit/Fail/Redo the bridge did), `ResourceGuard` budget/cancel inside every new generator, and
the IDE contract (`StreamManager.setThreadLocalOutput`, `Prolog.setTracing`, `getEngineContext`,
`getLastListingOutput`).

**Measured** (8 interleaved A/B JVM pairs in one shell session, `java -Xss4m -Xmx2g`, best of 6 warm
iterations per figure, harness `scratchpad/41b/probe/AB42.java`; A = the v4.1.0 build,
B = this tree):

| benchmark | A (median / min) | B (median / min) | change |
|---|---|---|---|
| `format(atom(_), "~w \| ~a \| ~d~n", ...)` x100 000 | 184 / 163 ms | 116 / 106 ms | **-37 %** |
| `format/2` to a stream x100 000 | 130 / 113 ms | 94 / 86 ms | **-28 %** |
| `write/1` + `nl/0` of a small compound x200 000 | 275 / 262 ms | 214 / 202 ms | **-22 %** |
| `write/1` of a 2 000-element list x200 | 26 / 25 ms | 11 / 10 ms | **-59 %** |
| `atom_codes`/`atom_length`/`atom_concat`/`sub_atom` x100 000 | 372 / 343 ms | 167 / 155 ms | **-55 %** |
| `atomic_list_concat`/`split_string` x50 000 | 134 / 126 ms | 78 / 70 ms | **-41 %** |
| `functor`/`arg`/`=..`/`succ` x100 000 | 254 / 241 ms | 119 / 111 ms | **-53 %** |
| `keysort/2` of 2 000 pairs x200 | 74 / 67 ms | 14 / 13 ms | **-82 %** |
| `assertz` + `retract` x200 000 | 350 / 325 ms | 346 / 327 ms | -1 % (none) |
| **control**: `loop(1000000)` (code this wave does not touch) | 428 / 380 ms | 405 / 384 ms | -5 % |

The control row is the noise floor: an untouched benchmark moved 5 % between the two sides in the
same session, so anything inside +/-5 % is "no measurable change" — which is where the
assert/retract loop sits.

**Suite**: 1196 -> **1261/1261** (65 new tests in `EngineV4IoTest` 19, `EngineV4TextTest` 16,
`EngineV4TermTest` 13, `EngineV4DatabaseTest` 17). **20/20 example programs** with every
per-program "Successful queries" count unchanged
(2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0). `src/main`: **71 899 -> 74 613
lines**, **347 -> 351 files** (4 added, 0 deleted).

**New files**: `core/engine/v4/NativeIo.java`, `NativeText.java`, `NativeTerm.java`,
`NativeDb.java`. The registry classes they shadow are kept, unregistered from dispatch but still
registered by name so `BuiltInRegistry.isBuiltIn` keeps answering (that is what makes
`assertz(write(_))` a permission error).

---

## [4.1.0] - 2026-08-26

### Wave A of 4.1: one engine

The one-release promise of design decision 1 (B.17) expires: the v2 `MachineSolver` — JProlog's
default from 3.1.0 to 3.14.0 and the selectable fallback of 4.0.0 — is **deleted**, together with
everything that existed only to keep it alive. Wave record, invariants and the exact starting point
for wave B are in `docs/reports/report-engine-v4-progress.md` (new section 16).
ISS-2025-0491..0495.

Baseline: **1196/1196 JUnit tests** (4.0.0 ran 1214 across two CI legs; 39 of those tests exercised
the deleted machine, 21 new ones cover the retirement), **20/20 example programs** with every
per-program "Successful queries" count unchanged
(2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0). `src/main` shrank from **74 724 to
71 899 lines** (-2 825) across **347 files** (was 352: 6 deleted, 1 added).
`grep -rn "MachineSolver\|core.engine.v2\|Trail.record\|isUsingV2Engine\|setUseV4Engine" src/main`
returns nothing.

**Deleted classes** (6 files, 2 616 lines):

| File | Lines | What it was |
|---|---:|---|
| `core/engine/v2/MachineSolver.java` | 1895 | the iterative SLD engine of 3.1.0–4.0.0 |
| `builtin/control/When.java` | 226 | `when/2`, the Java version (v4 has a prelude clause) |
| `builtin/term/AttributedVariables.java` | 179 | `put_attr/3`, `get_attr/3`, `del_attr/2`, `attvar/1`, the Java versions (v4 has natives) |
| `builtin/control/Dif.java` | 156 | `dif/2`, the Java version (v4 has a prelude clause) |
| `builtin/control/Freeze.java` | 106 | `freeze/2`, the Java version (v4 has a prelude clause) |
| `core/engine/Trail.java` | 54 | the process-wide `ThreadLocal` undo stack for bridged built-ins |

**Added** (1 file): `core/engine/v4/Undo.java` (64) — the doorway a bridged built-in uses to push
an undo action onto the running machine's own trail.

**Deleted tests** (2 files, 37 methods): `core/engine/v2/MachineSolverTest` (30 — unit tests of the
deleted class), `core/engine/v2/V2EngineIntegrationTest` (7 — the same programs the v4 suite runs,
driven through the deleted engine), plus `EngineV4Test.testISS0444_EngineSelectionFlag` and
`EngineV4RetirementTest.testISS0484_OnlyV2SelectsAFallbackEngine`, which asserted the behaviour of
the deleted selection API. `core/engine/v2/EngineHardeningTest` (54 ENG-01..ENG-17 regressions)
**moved** to `core/engine/EngineHardeningTest`, with its one direct `MachineSolver` use re-pointed
at the v4 `Machine`.

**Removed API**: `Prolog.setUseV2Engine`, `isUsingV2Engine`, `setUseV4Engine`, `isUsingV4Engine`;
`Variable.AttributeUnifyHook` with `Variable.setAttributeUnifyHook`/`getAttributeUnifyHook`;
`EngineContext.handleAttributeUnification`; the `engine-v2` Maven profile.
`-Djprolog.engine=<anything>` now logs a warning at class-init and runs v4. `Prolog.clearSession()`
survives as a no-op (there is no cross-query attributed-variable state on v4).
`core.engine.TableStore` keeps its name and its `:- table` declaration registry but loses the
answer cache, the in-progress set, the partial cache, the goal normaliser and the evaluation claim
(232 -> 54 lines) — all of that was the deleted driver's.

- **ISS-2025-0491** — **the v2 engine is deleted.** With `MachineSolver` go: the `engine-v2`
  profile and the second CI leg; the four static engine flags and every engine-aware branch in
  `src/main` and `src/test` (the v4 test classes no longer select an engine in `setUp`; the
  `isUsingV4Engine()` branches in `BugFixVerificationTest` and `RefactorIssuesTest` collapse to the
  v4 behaviour they always asserted); `EngineContext.runSub`'s engine branch and its legacy
  attribute-hook dispatcher; `Prolog.solveWithV2Engine`, the cross-query attributed-variable
  session (`spliceAttributedSessionVars` / `refreshAttributedSessionVars`) and the hook
  install/uninstall around every query; the legacy `freeze/when/dif` and attributed-variable
  built-ins and their `BuiltInFactory` registrations and `BuiltInRegistry` arity entries — on v4
  `put_attr/3`, `get_attr/3`, `del_attr/2`, `attvar/1` and `term_attvars/2` are natives and
  `freeze/2`, `frozen/2`, `when/2`, `dif/2` and `?=/2` are prelude clauses, so neither the classes
  nor their registry entries were reachable. **Behaviour note**: those seven names are no longer
  `BuiltInRegistry.isBuiltIn`, so `assertz(freeze(X, Y))` is allowed instead of raising
  `permission_error` — consistent with the documented rule that a user definition overrides a
  library one. `util.TermCopier` and `util.TermUtils` are **kept**: the knowledge base, `Prolog.compile`,
  `builtin.database.Clause`, `builtin.exception.Throw` and `builtin.term.TermConstruction` still
  use them (11 call sites, 12 files), and the machine uses neither.
- **ISS-2025-0492** — **one trail.** `core.engine.Trail`, a process-per-thread `ThreadLocal` stack
  of undo `Runnable`s that every choice point had to mark (`CP.legacyMark`) and roll back in
  parallel with the real trail, is replaced by `core.engine.v4.Undo`. A bridged built-in that
  mutates engine state outside the binding cells — `b_setval/2`, `op/3` (both implementations),
  `setarg/3`, the CLP(FD) store's domain narrowings and attribute registrations — records its undo
  action on the **thread-current machine's `Bindings` trail**, where `B.undo(cp.trailMark)` runs it
  at exactly the point the cells are reset. `CP` carries one mark again. An action recorded with no
  machine running on the thread (a `:- op(...)` directive at consult time, a directly instantiated
  built-in, a unit test) is a no-op, exactly as the old trail was with no choice point.
- **ISS-2025-0493** — **the module test leaves the hot path.** `Modules.overridesBuiltin`
  (two string concatenations and up to four map probes, through `Prelude.owner` and `autoload`) ran
  for **every** goal that is not inline, not a v4 native and not a control construct — including
  plain user predicates, which have no built-in to override. Measured at ~13–16 % of `nrev`. It is
  now asked only when `BuiltInRegistry.isBuiltIn(f, n)` says there IS an entry to override (the
  same probe `LegacyBuiltinAdapter.run` does as its first statement), and memoised in a 512-slot
  direct-mapped cache of immutable entries stamped with the `ModuleManager` modification stamp, so
  a module definition, an import or a consult into a module invalidates it without a lock or a
  sweep. Medians over 13 interleaved same-session JVM runs: `nrev30x2000` 404 -> 362 ms (-10 %),
  200 000 first-arg-indexed fact lookups 194 -> 181 ms (-7 %), `loop(1000000)` 543 -> 496 ms (-9 %).
- **ISS-2025-0494** — **an idle debug controller is free.** `DebugController.needsPorts()` is false
  for a controller with no listener, no breakpoint, in `CONTINUE` mode and with no Stop pending —
  such a controller can observe nothing, so the machine emits no ports for it at all. Measured on
  `loop(1000000)`, one port site per inference: an attached-but-idle controller cost **1.8–2.3x**
  before and **0.96–1.13x** after. `traceEnabled` deliberately does not count towards
  `needsPorts()`: every use of it inside `DebugController` is guarded by `listener != null`.
- **ISS-2025-0495** — **`thread_self/1` answers SWI-style.** A thread that has an alias reports the
  **alias**: the top-level thread answers `main`, a worker created with `[alias(w1)]` answers `w1`,
  and an anonymous worker still answers its integer id. Every thread predicate already accepted
  either form, so the answer stays a usable argument to `thread_join/2`,
  `thread_send_message/2` and the rest; `thread_join/2` statuses are unchanged. The `main` alias
  also **follows a live thread** now: it used to be claimed once, for the JVM's lifetime, by
  whichever non-worker thread touched the queues first, so once that thread died
  `thread_send_message(main, T)` posted to a queue nobody could read. W9 deviation 8 is closed.

---

## [4.0.0] - 2026-08-26

### Wave W9: retirement — the recursive engine is gone

Wave W9 of `docs/reports/report-engine-v4-design-2026-08-25.md` (part B, section B.16 row W9) and
decision 1 of B.17, ISS-2025-0484..0488. The recursive `QuerySolver` engine that JProlog was built
on is **deleted**; the v2 `MachineSolver` stays selectable for this release
(`-Djprolog.engine=v2`, `mvn test -Pengine-v2`) and goes in 4.1. Wave record and the 4.1 outlook
are in `docs/reports/report-engine-v4-progress.md` (new section 15).

Baseline: **1214/1214 JUnit tests on the default engine (v4) AND under `-Pengine-v2`**,
**20/20 example programs on both engines** with every per-program "Successful queries" count
unchanged. `grep -rn QuerySolver src/main` returns nothing. `src/main` shrank from **76 133 to
74 724 lines** (-1 409) across **352 files** (was 361: 13 deleted, 4 added).

**Deleted classes**: `core.engine.QuerySolver` (1247 lines — the recursive SLD algorithm, the LCO
trampoline, the bounded tabling driver, the 2 000-deep recursion cap and the durable context all in
one), `core.engine.CutStatus`, `core.engine.MutableCutStatus`, `core.engine.LayeredMap`,
`core.engine.CollectionBuiltInAdapter`, `builtin.BuiltInHelper`, and the seven ISO control
constructs only the recursive engine dispatched: `builtin.control.Conjunction`,
`builtin.control.IfThen`, `builtin.control.IfThenElse`, `builtin.control.NegationAsFailure`,
`builtin.exception.Catch`, `builtin.meta.Call`, `builtin.meta.Caret`.

**Removed API**: `Prolog.solveLegacy(String)`, `Prolog.getQuerySolver()`, the `legacy` value of
`-Djprolog.engine`, and the `engine-legacy` Maven profile. `Prolog.getEngineContext()` replaces
`getQuerySolver()` as the debugger wiring point; `Prolog.solve(Term)` and `Prolog.solveStream`
run the selected engine.

- **ISS-2025-0484** — **the recursive engine is deleted, and the context is re-homed.** Two new
  types in `core.engine`: **`SolverContext`**, the interface a `BuiltInWithContext` built-in
  receives (`solveMeta`, `solve(Term)`, `solveInWorker`, plus read-only access to the engine), and
  **`EngineContext`**, the concrete durable object every `Prolog` owns (the `DebugController`, the
  running query's `ResourceGuard`, the knowledge base and registry, the legacy attributed-variable
  unify hook, and an engine-routed implementation of the three seams). The v4 `Engine` and the v2
  `MachineSolver` both take it, and `core.engine.v4.SolverFacade` — which used to be a **subclass
  of `QuerySolver`**, i.e. still the recursive algorithm for anything it did not override — now
  implements `SolverContext` over `Machine.runSubQuery` and inherits nothing.
  Two knock-on repairs: the v2 engine's tabling delegated to the deleted
  `QuerySolver.solveWithTabling`, so the variant-tabling driver (memo cache, in-progress partial
  cache for left recursion, bounded fixpoint) is **ported into `MachineSolver`** — producing
  against the *normalised* pattern `p(a, _TV0)`, because a fresh machine restarts its
  clause-renaming counter at `_R1_` and producing against the caller's `p(a, _R1_Z)` bound the
  goal's own variable (a ground tabled variant such as `path(a, d)` silently failed); and
  `editor.DebugPanel` runs its detached breakpoint-condition and watch sub-solves through
  `Prolog.solve` with the debug controller temporarily nulled.
  `Prolog.setTraceEnabled/1` now only records the flag — the `LOGGER.info` mechanism it drove was
  the recursive solver's, unrelated to `trace/0` (which is `Prolog.setTracing`).
- **ISS-2025-0485** — **`BuiltInWithContext` reduced to the adapter.** `executeWithContext` takes a
  `SolverContext`; no `QuerySolver`-shaped type survives in any signature, and `CutStatus` is gone
  from all 46 implementors. The seven control-construct built-ins are deleted — both surviving
  machines implement `,/2`, `;/2`, `->/2`, `*->/2`, `\+/1`, `call/N`, `catch/3` and `^/2` natively,
  with real choice points and real cut barriers — but their **registry entries stay**, as a
  `builtin.control.ControlConstruct` placeholder, because `BuiltInRegistry.isBuiltIn/2` is what
  makes `assertz`, `retract` and `clause/2` raise
  `permission_error(modify, static_procedure, call/1)` on a control construct. `assertz(call(x))`,
  `clause(catch(_,_,_), B)` and friends still raise exactly as before. All 35 legacy
  `solver.solve(Goal, Bindings, Solutions, CutStatus)` sites now call `solveMeta` (or
  `solveInWorker`), and `CollectionBuiltInAdapter` is deleted, so a context built-in can no longer
  be registered with a solver pinned at construction time (invariant 43 became structural).
- **ISS-2025-0486** — **the last eager built-ins LIM-037 named, and the last name-keyed hop.**
  New `core.engine.v4.NativeMisc`: `sort/4`, `predsort/3`, `max_list/2`, `min_list/2`,
  `current_op/3` (a lazy `Generator` over the engine's operator store — it used to materialise
  every visible operator before the first solution), `nb_getval/2` and `b_getval/2`, all on cells,
  with the ISO error terms of ISS-2025-0418/0419 reproduced exactly.
  `core.engine.v4.ClpfdNative` gains `in/2`, `#=`, `#\=`, `#<`, `#>`, `#=<`, `#>=`,
  `all_different/1` and `all_distinct/1`: they post through the bridge and then bind the
  **cells** propagation has determined (new `ClpfdV2Bridge.determinedCells()`), instead of naming
  them in a `Map<String,Term>` that `LegacyBuiltinAdapter` had to translate back through
  `ClpfdV2Bridge.cellFor(name)`. **`cellFor(String)` and `onBindByName(String, Term)` are deleted**
  — the machine hands the attributed cell to the hook directly, waking
  `'$clpfd_unify_hook'('$attvar_cell'(Cell), Other)` rather than routing a name through the
  prelude's `'$attr_hook'/4` dispatcher — and `LegacyBuiltinAdapter.apply` is goal-scoped again.
  `C in 1..3, D #= C*2+1, C #= 1` still binds `D = 3`.
- **ISS-2025-0487** — **the main thread owns a message queue.** "Every Prolog thread owns a queue"
  (ISS-2025-0479) was true only of `thread_create/2,3` workers; the thread running the top-level
  query had none, so `thread_get_message/1` and `thread_send_message(main, T)` both failed — and a
  worker had no way to send a result back to its creator, the commonest SWI idiom. The calling
  thread is now registered on first use of `thread_self/1` or the queue predicates, and the first
  such thread claims the alias `main`.
- **ISS-2025-0488** — **LIM-039 closed: a tabled evaluation is claimed by one thread**, on both
  engines. Neither tabling store is thread-safe, and worse than the data race, a second thread that
  found an EVALUATING table became a consumer of it and read a half-produced answer set as
  authoritative (four workers each counting a tabled transitive closure returned `[4, 4, 0, 1]`).
  A tabled call now runs inside `enterCall`/`exitCall` so the variant decision and the frame it
  installs are atomic, and the claim is held until the SCC completes, is abandoned, or the query
  ends. A worker hands it back in `Machine.solve`'s finally (`Tabling.endWorker()`), abandoning
  whatever it left EVALUATING. Reading a COMPLETE table stays effectively parallel. The wait is
  bounded at 60 s and then raises `resource_error(tabling_busy)`; a Stop interrupt is honoured
  while waiting.

Two further bugs came out of re-running the manual's own examples against the build:

- **ISS-2025-0489** — **`stream_property/2` no longer blocks on stdin.** With an unbound first
  argument it walks every open stream, and computing `end_of_stream` for `user_input` peeked —
  a blocking read on an interactive terminal — so the manual's own
  `stream_property(S, alias(user_error))` hung forever, and neither the inference budget nor a Stop
  interrupt could break it (the wait is inside a bridged built-in). A stream that cannot be
  repositioned is now reported as `end_of_stream(not)` unless a read has already run past its end;
  `at_end_of_stream/0,1`, which may legitimately wait, is unchanged.
- **ISS-2025-0490** — **answers print user-declared operators again.**
  `?- op(200, xfy, likes), X = (john likes (mary likes wine)).` answered
  `X = likes(john,likes(mary,wine))` while `write/1` inside the same query printed the operator
  form. `Answer.lines` runs after `Prolog.solve` has returned, i.e. after the engine's
  `EngineState` left the thread, so the writer fell back to a default operator table. New
  `Answer.lines(solution, residual, OperatorTable)`; `PrologCLI` and `editor.RunPanel` pass
  `prolog.getOps().table()`.

**Limitations retired**: LIM-023, LIM-026, LIM-028, LIM-030, LIM-031, LIM-032, LIM-033, LIM-038 and
LIM-039 — they all described the recursive engine or a gap the v4 default has closed. LIM-037
shrinks to limit L-08 (~310 predicates still on `LegacyBuiltinAdapter`, none on a measured hot
path) and LIM-027 is narrowed to the v2 fallback.

**Tests**: new `core/engine/v4/EngineV4RetirementTest` (18). Eight tests were REMOVED with the code
they exercised: `testISS0189_layeredMapIsEmpty`, `testISS0189_layeredMapIsEmptyWithLocal`,
`testISS0190_layeredMapRollback` (`LayeredMap` is deleted), `testISS0328_SolveLegacyWorks`,
`testISS0347_LegacyEngineAlsoRaisesExistenceError`,
`testISS0366_RetractValidationOnLegacyEngine`, `testISS0367_AssertOnBuiltInRaisesOnLegacyEngine`
and `testISS0368_AssertValidationOnLegacyEngine` (each had a surviving twin on the real engines).
Three were re-pointed at the selected engine and renamed where the name lied:
`testISS0396_LegacyEngineRetractStillEnumerates` -> `testISS0396_RetractInFindallStillEnumerates`,
`testISS0398_CaretGoalOnLegacyEngine` -> `testISS0398_CaretGoalAsPlainGoal`, and the
`QuerySolver.internalSolveCount()` reachability probes in `EngineV4LibraryTest`,
`EngineV4TablingTest` and `EngineV4ThreadsTest` became the structural assertion that the class no
longer exists.

---

### Engine v4 is the DEFAULT — wave W8: default switch, threads, debugger

Wave W8 of `docs/reports/report-engine-v4-design-2026-08-25.md` (part B, sections B.6 and B.13),
ISS-2025-0478..0483. This is the release the whole v4 programme was aiming at: **`core.engine.v4`
is now the engine every query runs on**, `thread_create/2,3` and the `concurrent_*` family run on
real per-thread machines, and the tracer and the IDE debugger no longer force the engine off its
fast paths. Progress, invariants, the A/B evidence and the exact starting point for wave W9
(retirement) are in `docs/reports/report-engine-v4-progress.md` (new section 14).

**Major version** because the default engine changes and the console answer format changed in
3.14.0: the deliberate v4 behaviour differences below now apply to every program that does not
explicitly ask for the old engine.

Baseline: **1204/1204 JUnit tests on the default engine (v4) AND under `-Pengine-v2`** (1158
pre-existing + 46 new in `core/engine/v4/EngineV4ThreadsTest`, `core/engine/v4/EngineV4TraceTest`
and `test/cli/PrologCliBatchTest`), **20/20 example programs on both engines**.

> **Behaviour changes that arrive with the default switch** (all documented since v3.9.0 and pinned
> by engine-aware tests): rational trees succeed instead of raising
> `representation_error(cyclic_term)` (`set_prolog_flag(occurs_check, error)` restores the ISO
> behaviour); `setup_call_cleanup/3` and `call_cleanup/2` run `Cleanup` after the goal's LAST
> solution; `append(X, Y, Z)` fully open enumerates and `member(X, PartialList)` extends the open
> tail; a coroutine suspended in one query never fires in a later one; and a growing list of
> predicates that only ever worked on v4 (`memberchk/2`, `partition/4`, `frozen/2`, `unifiable/3`,
> `current_table/2`, `current_module/1`, `library(yall)` lambdas, `lists:append/3`-style
> module-qualified built-in calls, correct tabling, `when/2` binding propagation).
> **The old engine stays selectable for one release**: `-Djprolog.engine=v2` (or
> `Prolog.setUseV4Engine(false)`) picks the v2 `MachineSolver`, `-Djprolog.engine=legacy` the
> recursive `QuerySolver`. Both are deleted in wave W9.

- **ISS-2025-0478** — **v4 is the default engine.** `jprolog.engine` now defaults to `v4`; only the
  literal values `v2` and `legacy` select an older engine (anything else is still v4, the rule the
  other three subsystem toggles have always used). `Prolog.setUseV4Engine(false)` drops to v2 at
  runtime; `USE_V2_ENGINE` stays true unless `legacy` was asked for, because v4 wins over it when
  both are on. The Maven CI legs swapped: `mvn test` IS the v4 leg and the second leg is
  **`mvn test -Pengine-v2`** (the former `-Pengine-v4` profile is gone; `-Pengine-legacy` stays a
  convenience, not an acceptance leg). Every engine-aware branch in the suite was audited, because
  `Prolog.isUsingV4Engine()` starts answering true: two tests that reached the old engines by
  clearing only ONE flag (`V2EngineIntegrationTest`, `BugFixVerificationTest`'s
  `testISS0348_StringIdentityAndAtomicOnLegacyEngine`) now clear both and restore both.
- **ISS-2025-0479** — **`thread_create/2,3` really runs its goal, on its own machine** (design
  B.13, limit **LIM-024**). Until now `thread_create/2` started a thread that slept 10 ms and
  recorded `completed(<goal atom>)`: the goal never ran, and the goal had to be an *atom*. A worker
  now runs on a **fresh `core.engine.v4.Machine` over the same `Engine`** (new
  `core.engine.v4.Workers`): shared clause store — thread-safe by birth/death generations, so
  `assertz`/`retract` from two threads are visible to both — shared flags, operators and modules,
  its own `current_input`/`current_output`, its own `ResourceGuard` carrying the parent's inference
  budget, and a `copy_term`'d goal so **no `Variable` cell is shared between two machines**
  (a binding a worker makes cannot appear in the parent's query, as in SWI). New `thread_create/3`
  with `alias/1` and `detached/1` options; `thread_join/2` reports `true` / `false` /
  `exception(Ball)` / `cancelled` instead of a synthetic atom; `thread_self/1` reports the worker's
  own Prolog thread id; `thread_detach/1`, `thread_is_alive/1` and `thread_join/2` accept an alias.
  Message queues carry **terms** rather than atoms, copied on the way in and on the way out; every
  Prolog thread owns a queue, so `thread_send_message/2` accepts a queue id, a thread id or a thread
  alias, and the new `thread_get_message/1` reads the calling thread's own queue.
  A worker machine owns **no query boundary** (`Machine.asWorker()`): the engine-wide sweeps at the
  end of a solve — restoring the shared solver's guard, compacting the clause store and
  `Tabling.endQuery()` — belong to the top-level query, and a worker finishing first would
  otherwise abandon the parent's in-progress tabled evaluation. New **LIM-039** records what is
  still not thread-safe: two threads *producing* the same table concurrently.
  **Sandbox**: `builtin.threading` joined `UNSAFE_BUILTIN_PACKAGES`, so `enableSafeMode()` now also
  removes `thread_create/2,3`, the message queues and the `concurrent_*` family. A JVM thread is a
  host resource like a process or a socket, and each worker carries its OWN inference-budget
  counter — harmless while `thread_create` did nothing, a denial-of-service surface now that it
  runs goals.
- **ISS-2025-0480** — **the concurrency predicates run on worker machines.** `concurrent/3`,
  `concurrent_maplist/2,3,4`, `concurrent_and/2`, `concurrent_or/2` and `first_solution/3` submit
  their goals through the new `QuerySolver.solveInWorker`, which the v4 `SolverFacade` overrides
  onto `Workers`. Two bugs fell out of it: the family was wrapped in a `CollectionBuiltInAdapter`
  that pinned the engine's **shared recursive solver** at registration time (which is exactly the
  object a worker must not use), and `concurrent_maplist/3,4` were registered under the literal
  names `concurrent_maplist3` / `concurrent_maplist4` and were therefore **not callable at all** —
  one entry now dispatches on the goal's arity. Interrupting the parent cancels the workers (their
  futures are cancelled, which interrupts them, and their own guards raise
  `QueryCancelledException`). `Machine.onOwnerThread()` became `Machine.assertOwnerThread(...)`: an
  off-thread entry is an `IllegalStateException`, not a silent fallback. **After this change
  nothing a v4 query does reaches `QuerySolver.solveInternal`** — asserted by
  `EngineV4LibraryTest.testISS0450_NoBuiltinReachesTheRecursiveSolver` (now including
  `thread_create/join`, `concurrent_maplist/2,3` and `first_solution/3`) and by
  `EngineV4ThreadsTest.testISS0480_NoBuiltinReachesTheRecursiveSolver`. The probe counter
  `QuerySolver.internalSolveCount()` became `volatile` so it observes worker threads.
- **ISS-2025-0481** — **the tracer and the debugger no longer disable the fast paths** (design B.6,
  limit **L-13**). Attaching a `DebugController` used to reroute `=/2`, `is/2`, the six arithmetic
  comparisons, the standard-order comparisons, `\=/2` and the nine type checks through the legacy
  bridge, and `trace/0` did the same for `once/1`, `ignore/1`, `forall/2` and `between/3` — so a
  debugged run executed *different code* from an undebugged one, and the bridge's materialised
  solution maps and forced choice points were paid per inference. The machine now keeps every
  inline path and **emits the four ports itself**: the deterministic inline built-ins get
  Call + Exit/Fail and push no choice point, `once/ignore/forall` keep the native if-then-else and
  own their wrapper ports through the new `Machine.iteTraced`, and `between/3` emits them from its
  own lazy generator. Two consequences worth knowing: the inline built-ins now appear in a
  `trace/0` trace (they were invisible there, while the IDE debugger did see them), and a running
  debugger costs essentially nothing — `nrev` with a `DebugController` attached went from 5.8x
  untraced to **1.1x**, because `DebugController` only builds a `DebugEvent` (and the engine only
  snapshots the goal) when a listener, a breakpoint or a step will actually look at it. That is the
  design's "trace mode <= 1.5x slower" target, met for the debugger; textual `trace/0` stays
  ~10-18x, which is the cost of formatting and printing a line per port.
- **ISS-2025-0482** — **trace memory and the port depth.** Two changes that belong together:
  (1) a frame that handed out exactly one alternative and is exhausted is **deterministic**, so it
  is trust-me popped even while tracing — it owes no Redo and its `Fail` after `Exit` was a phantom
  (SWI drops such a frame too, by last-call optimisation). Before, *every* traced frame was kept and
  the trace was **quadratic** in the program: `loop(N)` under trace took 1.9 s / 8.4 s / 33 s at
  N = 20 000 / 50 000 / 100 000 and did not finish at all at N = 1 000 000, and `nrev 30x100` was
  567x slower than untraced. After: 0.48 s / 0.48 s / 0.74 s / **4.6 s**, and `nrev` is ~10-18x.
  (2) the port DEPTH is the machine's own call-nesting level rather than the choice-point height —
  which stopped nesting once deterministic frames are popped, and the IDE needs a real depth
  (`DebugController` prunes its call stack by depth, and step-over/step-out compare against a target
  depth). Every port ASSIGNS the depth (Call takes it and goes one deeper, Exit/Fail return to the
  frame's, Redo re-opens it), so a frame cut away without an Exit cannot make it drift. The trace
  indentation is capped at 40 levels — the depth itself is still exact — because a 1 000 000-deep
  tail recursion would otherwise print a two-million-character indent. `tracePort` also stopped
  copying the goal (`Unify.resolve`) before formatting it: the writer dereferences and is
  cycle-safe, so the copy was pure cost.
- **ISS-2025-0483** — **the non-interactive console.** When stdin is not a terminal
  (`System.console() == null` — a pipe, a here-doc, a redirected file, a CI job) or when
  `--batch` / `-q` is given, `PrologCLI` prints **every** solution at once, separated by ` ;` and
  terminated by `.`, and reads nothing back. Before, it printed the first answer, wrote ` ;` and
  consumed **the next input line — i.e. the next query — as the user's answer**: piping
  `between(1,5,X).` followed by `digit(X).` ran only the first and reported one solution instead of
  five. Interactive behaviour is unchanged (`;` + Enter for the next solution, Enter to stop), and
  the banner and `:help` say which mode is in force.

### Also in this release

- `docs/references/BUILTIN_PREDICATES_REFERENCE.md`: `thread_create/3`, `thread_get_message/1`, the
  new `thread_join/2` status terms, term-carrying message queues and the batch console.
- `CLAUDE.md`: the engine sections rewritten around v4 as the default; `tools/manual/front.md` and
  `appendix.md` updated where they describe engine selection or the console.

---

## [3.14.0] - 2026-08-26

### Engine v4 (opt-in) — wave W7: engine state (streams, operators, writer)

Wave W7 of `docs/reports/report-engine-v4-design-2026-08-25.md` (part B, sections B.11 and B.12),
ISS-2025-0472..0477. **The default engine is still v2** — but unlike waves W1-W6, most of W7 is
*engine-neutral*: the stream table, the operator store, the writer, the spy points and the profiler
counters are owned by the `Prolog` instance and reached by **both** engines through the unchanged
static facades, which design decision 1 (B.17) requires while v2 stays selectable. Progress,
invariants, the A/B evidence and the starting point for wave W8 (default switch, threads on v4
machines, the debugger without disabling fast paths) are in
`docs/reports/report-engine-v4-progress.md` (new section 13).

Baseline: **1157/1157 JUnit tests on the default engine AND on v4** (1112 pre-existing + 46 new in
`core/engine/v4/EngineV4StreamsTest` and `EngineV4WriterTest`), **20/20 example programs on both
engines with byte-identical output between v2 and v4**.

> **Behaviour change, approved (design B.17 decision 5): the console prints answers differently.**
> The top level now prints answers in quoted operator notation with `_A`-style variable names and
> the answer's residual goals — `X = 'a b'-1.` where it used to print `X = -(a b, 1).`. See
> "Answer printing" below for the full before/after table; the byte-identical example-output oracle
> was re-baselined once, deliberately.

- **ISS-2025-0472** — **the stream table is per engine** (design B.11). `builtin.io.StreamManager`
  owned all stream state in statics: two maps of open streams, a property map, a reader cache and a
  PrintStream-wrapper cache, shared by every `Prolog` in the JVM (limit **L-06**). It is now a thin
  static facade — every signature unchanged, so the ~30 legacy I/O built-ins and any embedder code
  keep compiling — over `core.engine.v4.Streams`, the table of the engine current on the calling
  thread, exactly as `PrologFlags` has routed since ISS-2025-0437. `core.engine.v4.PrologStream` is
  one open stream: a byte channel plus, for a text stream, a private decode buffer that tracks byte
  position, character count, line number and line position.
  **This is what fixes limit L-07**: `get_char(S,C1), seek(S,0,bof,_), get_char(S,C2)` answered
  `C2 = e` after `C1 = h`, because the seek moved the file channel while `get_char/2` kept reading
  the `PushbackReader`'s own 8 KB buffer. A reposition now flushes the decode buffer, resets the
  decoder and recomputes the counters, and *peek* is a one-character lookahead on the decoder rather
  than a pushback on a reader. `read/1,2` and `read_term/2,3` lost their private static per-alias
  `BufferedReader` caches and read through the same decoder, so every I/O built-in agrees about
  where the stream is. `open/3,4` unifies `Stream` with the canonical term `'$stream'(N)`; atom
  aliases, the `stream_<id>` handle and the reserved names keep working everywhere.
  `current_input`/`current_output` are per thread *within* an engine, and the IDE's thread-local
  output capture still wins over both.
  **LIM-025 is closed**: `with_output_to/2` (both engines), `format/3` with `atom/string/codes/chars`
  and `format ~@`/`~p` capture through the thread-local override alone — no `System.setOut`, no
  `user_output` stream swap — so two threads can capture concurrently. The handful of debug
  built-ins that printed to `System.out` directly (`spy/1`, `nospy/1`, `leash/1`, `debugging/0`)
  now write through `StreamManager.out()`.
- **ISS-2025-0473** — **stream introspection completed** (design B.11). `stream_property/2` reports
  the full ISO set — `file_name`, `mode`, `input`/`output`, `alias` (one solution each),
  `position`, `end_of_stream`, `eof_action`, `reposition`, `type`, `encoding`, `line_count` — over
  every stream of the current engine, and accepts an alias as the stream argument (it used to try to
  unify the alias atom with the stream's own term and fail). New: `set_stream/2`,
  `stream_position_data/3`, `character_count/2`, `line_count/2`, `line_position/2`,
  `current_stream/3`. `stream_property(S, position(P))` hands out
  `'$stream_position'(CharCount, LineCount, LinePosition, ByteCount)`, which
  `set_stream_position/2` accepts back. `read_term/2,3` gained `term_position(Pos)`.
  Both parsers gained a **deterministic nesting limit**: input nested more than 1000 levels raises
  `error(resource_error(parser_nesting), _)` instead of blowing the Java stack — and
  `term_to_atom/2` and `atom_to_term/3` no longer swallow it into a silent failure or a
  `syntax_error`.
- **ISS-2025-0474** — **one operator store per engine, scoped by module** (design B.12). The three
  process-global stores of LIM-034 — `OperatorDefinition.OPERATORS`, `OperatorDefinition.OP_MODULE`
  and `OperatorDefinition.sharedOperatorTable` — are deleted; `core.engine.v4.Ops` on the engine is
  the single store the parser, `op/3`, `current_op/3`, the writer, the `.jpc` writer and the IDE
  formatter read. **`current_op/3` now sees an operator declared by a consulted `:- op/3`
  directive** (before, `Prolog.processOpDirective` only touched the parser's table, so the operator
  worked in source and was invisible to the program). An `op/3` inside a module file is local to
  that module for `current_op/3`; `op/3` under a choice point is still undone on backtracking.
- **ISS-2025-0475** — **`core.engine.v4.Writer`: the ISO term writer** (design B.12). One writer
  behind `write/1,2`, `writeln/1,2`, `writeq/1,2`, `print/1,2`, `write_canonical/1,2`,
  `write_term/2,3`, `format ~w/~q/~p`, the IDE formatter, the debugger's variable views and the
  console — on both engines (`core.util.TermFormatter` is now a facade over it). It is **fully
  iterative** (a 1 000 000-element list and a 200 000-deep last-argument spine print at the default
  JVM stack), **cycle-safe** (a rational tree terminates: a back edge prints as `...`, or as the
  SWI `@(Template, Substitutions)` form under `cycles(true)`; list spines use Brent's algorithm, so
  a long acyclic list costs no extra memory), and implements the **complete option set**:
  `quoted`, `ignore_ops`, `numbervars`, `max_depth`, `portray` (calls the user's `portray/1`),
  `cycles`, `variable_names`, `spacing(next_argument)`. An unknown option raises
  `domain_error(write_option, O)`. New built-ins `portray_clause/1,2` and `print_message/2`
  (minimal: an ISO error ball renders readably, `error`/`warning` go to `user_error`).
  `print/1,2` gained the `portray/1` hook it documented as unsupported.
- **ISS-2025-0476** — **answer printing** (design B.12, decision 5, limit **L-11**). `PrologCLI` and
  the IDE's `RunPanel` render answers with `quoted(true), numbervars(true), portray(true)` and the
  engine's operator table, name fresh variables `_A`, `_B`, ... (never an internal `_G12` /
  `_R1_A`), keep a query variable's own name when it comes back unbound (`X = f(Y)`, not
  `X = f(_A), Y = _A`), print `true.` for an answer with no bindings and `false.` on failure, and
  print the answer's **residual goals** after the bindings using `Prolog.residualGoals/1` —
  `freeze(X, G)`, `dif(X, a)`, `when(C, G)` and CLP(FD) `X in 1..3` (meaningful on v4, where
  attributes live in the answer's cells). Measured on the example programs: 8 of the 16 that produce
  bindings print different text now.

  | query | before | after |
  |---|---|---|
  | `X = 'a b'-1` | `X = -(a b, 1)` | `X = 'a b'-1` |
  | `PI = foo/1` | `PI = /(foo, 1)` | `PI = foo/1` |
  | `Body = (p,q)` | `Body = ,(p, q)` | `Body = (p,q)` |
  | `A = '42'` | `A = 42` | `A = '42'` |
  | `X = {a,b}` | `X = {}(,(a, b))` | `X = {a,b}` |
  | `X = f(Y)` | `X = f(Y), Y = Y` | `X = f(Y)` |
  | `atom_chars(hello, C)` | `C = [h, e, l, l, o]` | `C = [h,e,l,l,o]` |
  | `findall(S, student(S,_,_), L)` | `L = [...], _G516 = _G516, Student = Student` | `L = [...]` |
  | `retract(score(alice, _))` | `_G12 = 95.` | `true.` |

- **ISS-2025-0477** — **spy points and profiler counters are per engine** (the tail of LIM-034).
  `spy(foo/1)` in one engine no longer sets a spy point for every engine in the JVM, and
  `profile(on)`/`profile(reset)` no longer start or wipe another engine's counters. `Spy` and
  `Profiler` keep their static API as facades over `EngineState.current()`.

### Changed

- `Prolog` gained `getEngineState()`, `getStreams()` and `getOps()`; the engine state is installed
  as thread-current around every `solve`/`consult` entry point next to the flag store.
- `write_term/2,3` no longer carries its own half-formatter (it ignored operators, cycles and
  `portray`, and recursed on every argument).
- `DebugPanel` renders watch values, the variables table and the paused-goal status line through
  the writer (quoted, operator notation) instead of `Term.toString()`; the four-port **trace**
  output is unchanged and still byte-identical between v2 and v4.
- `OperatorDefinition.setSharedOperatorTable` is a deprecated no-op (the table belongs to the
  engine).

### Fixed

- `stream_property(Alias, P)` failed for every stream opened with `alias(A)`.
- `read/1,2` and `read_term/2,3` could hand a second `Prolog` instance the *closed* file of the
  first, because their per-alias reader caches were static and stream handles restarted at
  `stream_1001` per engine.
- `term_to_atom/2` on deeply nested input failed silently instead of raising a resource error.

---

## [3.13.0] - 2026-08-26

### Engine v4 (opt-in) — wave W6: modules and the Prolog prelude

Wave W6 of `docs/reports/report-engine-v4-design-2026-08-25.md` (part B, section B.10),
ISS-2025-0466..0471. **The default engine is still v2**; everything below applies to
`-Djprolog.engine=v4` / `Prolog.setUseV4Engine(true)` unless it says otherwise. Progress,
invariants, the measured numbers and the starting point for wave W7 (streams, operators and the
writer, design B.11-B.12) are in `docs/reports/report-engine-v4-progress.md` (new section 12).

Baseline: **1112/1112 JUnit tests on the default engine AND on v4** (1083 pre-existing + 29 new in
`core/engine/v4/EngineV4ModulesTest`), **20/20 example programs on both engines with byte-identical
output**.

- **ISS-2025-0466** — a **`Modules` owner on the `Engine`** (design B.10) replaces
  `core.module.ModuleManager` **as the resolver** on the v4 path; the manager stays the
  consult-time recorder shared with the legacy and v2 engines, and the new owner mirrors the
  user-defined modules from it through a monotone `ModuleManager.getStamp()`. Every predicate now
  belongs to a module: `system` holds the built-ins, **`user` IS the flat clause store**, and the
  library modules (`lists`, `apply`, `pairs`, `coroutining`) are Prolog files under
  `src/main/resources/prelude/`. Resolution for an unqualified call from module `M` is
  `M` -> `M`'s imports -> `user` -> autoload libraries -> `system`. The `modules.size() > 1`
  special case that used to divert **every** unqualified call through `ModuleManager` the moment a
  second module existed is **gone**, and with it the matching escape hatch in
  `raiseUnknownIfRequired` (an unknown procedure now raises under the `unknown` flag whether or not
  the program declares modules). `Module:Goal` works for built-ins and libraries —
  `lists:append([1],[2],L)`, `system:atom_length(abc,N)` and `user:foo(X)` all succeed where
  `lists:append/3` was simply *false* before (design limit **L-09**) — a nested qualification
  `a:b:Goal` resolves in the innermost module, and the export enforcement of ISS-2025-0314 is
  preserved exactly: a module that DEFINES a predicate answers a qualified call only if it exports
  it, and a module that does not define it falls through to the ordinary resolution in its own
  context (which is what makes `lists:length/2`, a native, and `other:base/1`, a `user` predicate,
  both resolve).
- **ISS-2025-0467** — **autoload by predicate indicator**. The prelude is no longer parsed at
  engine creation. Each `prelude/*.pl` declares `:- module(Name, [Exports])`; those headers are
  extracted by a textual scan **once per JVM** into an `indicator -> module` index, and a module's
  clauses are parsed and compiled the first time one of its predicates is referenced (also cached
  for the whole JVM). The prelude is parsed with the STANDARD operator table, never the engine's,
  so a user's `:- op/3` cannot change how the library reads. Measured on the same session:
  `new Prolog()` **0.196 ms best / 0.78 ms avg** (v2: 0.234 / 0.81) and `new Prolog()` plus a first
  query **0.34 ms best / 1.4 ms avg** against **8.0 ms** before the wave, when `apply.pl` and
  `coroutining.pl` were loaded eagerly.
- **ISS-2025-0468** — the **two W3 deviations are paid off**. `append(X, Y, Z)` with all three
  arguments open now **enumerates** (`X = []`, `[_]`, `[_,_]`, ... lazily, so
  `append(X,Y,Z), length(X,2), !` terminates) instead of stopping at the single standard solution,
  and `member(X, PartialList)` **extends** the open tail instead of failing at it, so
  `member(X, L), L = [a|_]` succeeds. `memberchk(a, L)` binds `L = [a|_]` for the same reason. The
  reference definitions of `member/2` and `append/3` are the two-clause Prolog predicates of the
  new `prelude/lists.pl` (what `lists:member/2` and `lists:append/3` run, and what a trace shows);
  the unqualified call takes an observationally equivalent native generator, which
  `EngineV4ModulesTest.testISS0468_NativeAndPreludeListPredicatesAgree` pins. Which predicates are
  Prolog and which are native was decided by measurement, as the wave required — on a
  1 000 000-element list a clause walk pushes one choice point per element where a generator pushes
  one for the whole call, so `select/3`, `selectchk/3`, `nth0/3`, `nth1/3`, `last/2`, `reverse/2`,
  `memberchk/2`, `length/2`, `msort/2`, `sort/2`, `sum_list/2`, `numlist/3` and `copy_term/2` stay
  native (2x to 15x faster) while `maplist/N`, `foldl/N`, `include/3`, `exclude/3`,
  `partition/4,5`, `freeze/2`, `frozen/2`, `when/2`, `dif/2`, `?=/2` and the three `pairs`
  predicates are Prolog. Module clauses (library and user-module alike) now get the **same
  first-argument index** the flat store has, built once when the module is installed and bounded by
  the keys that actually occur in a clause head. New library module `pairs`
  (`pairs_keys_values/3`, `pairs_keys/2`, `pairs_values/2`) as relational Prolog.
- **ISS-2025-0469** — **`meta_predicate/1`**. A predicate declared `meta_predicate p(0, +, ...)`
  gets its module-sensitive arguments (`0`-`9`, `:`, `^`, `//`) qualified with the CALLER's module
  before the head is unified, so a library predicate's `call/N` runs the goal in the calling
  context: two `user` modules that define the same helper name and both call the library
  `maplist/3` now each get their own helper. The wrapper is the engine-internal
  `'$mctx'(Module, Goal)`, deliberately not `Module:Goal`: an explicit `M:G` is export-checked
  (ISS-2025-0314) while a meta-argument travelling back into its own caller must see that module
  from the inside. `call/N` on a qualified callee builds `M:g(Args)`, never `':'(M, g, Args)`.
  `prelude/apply.pl` carries the declarations for `maplist/2..7`, `foldl/4..7`, `include/3`,
  `exclude/3` and `partition/4,5`.
- **ISS-2025-0470** — **`current_module/1`** (unification-based enumeration, like `current_op/3`)
  and the module properties of **`predicate_property/2`** — `defined_in(M)`, `exported`,
  `imported_from(M)` — layered over the existing registry implementation, which still answers
  `built_in`, `dynamic`, `static` and the rest. Both are v4-only, like `current_table/2` (W5) and
  `unifiable/3` (W4).
- **ISS-2025-0471** — **`label/1` and `labeling/2` as cell-based v4 natives**. The new
  `ClpfdV2Bridge.labelCells/3` returns one `cell -> value` assignment per solution instead of a
  `Map<String,Term>`, so labeling no longer goes through `exportSingletons/1` and does not need the
  name -> cell hop back into the CLP(FD) context. Functionally determined variables are still
  reported (`C in 1..3, D #= C*2+1, label([C])` binds `D`, ISS-2025-0357) — as cells. The
  `labeling/2` options (`leftmost`/`ff`/`ffc`/`min`/`max`, `up`/`down`, `min(Expr)`/`max(Expr)`) and
  their ISO errors are unchanged, and the objective is now evaluated over the cells. The registry
  implementation stays registered and is what the v2 and legacy engines run. **Still name-keyed in
  `ClpfdV2Bridge` afterwards**: `in/2`, the `#=`/`#<`/... comparisons and `all_different/1` are
  legacy-bridged, keep `exportSingletons(Map<String,Term>)` and still report a functionally
  determined variable their goal never mentions, so `LegacyBuiltinAdapter.apply` still asks
  `ClpfdV2Bridge.cellFor(name)` for the cell behind such a name; `Ctx.vars`/`Ctx.cells` and
  `varFor`/`onBindByName`/`domainTermForCell` remain keyed by `Variable.getName()`.

### Notes

- The v2 and legacy engines are untouched by this wave: `ModuleManager` gains only a modification
  stamp and `Module` a getter, and the CLP(FD) bridge gains one method.
- `enableSafeMode()` still strips the host-touching built-ins from `system`, so
  `system:shell(...)` raises `existence_error` in a sandboxed engine.
- Module-qualified goals are traced with their qualification (`Call: tm:t(_G1)`).

---

## [3.12.0] - 2026-08-26

### Engine v4 (opt-in) — wave W5: tabling (linear tabling with completion)

Wave W5 of `docs/reports/report-engine-v4-design-2026-08-25.md` (part B, section B.8),
ISS-2025-0463..0465. **The default engine is still v2**; everything below applies to
`-Djprolog.engine=v4` / `Prolog.setUseV4Engine(true)` unless it says otherwise. Progress,
invariants and the starting point for wave W6 (modules and the prelude, design B.10) are in
`docs/reports/report-engine-v4-progress.md` (new section 11).

Baseline: **1083/1083 JUnit tests on the default engine AND on v4** (1065 pre-existing + 18 new in
`core/engine/v4/EngineV4TablingTest`), **20/20 example programs on both engines with byte-identical
output**.

- **ISS-2025-0463** — **linear tabling with completion** replaces the bounded re-evaluation loop
  (design limit **L-03**). The old algorithm (`QuerySolver.solveWithTabling`, still the
  implementation on the v2 and legacy engines) re-ran a tabled goal at most **100** times over
  name-keyed `Map<String,Term>` answers and let a consumer read a stale partial list, so it
  returned **wrong answers**: on the classic left-recursive 3 000-edge chain
  (`edge(I,J) :- between(1,3000,I), J is I+1.  :- table path/2.  path(X,Y) :- edge(X,Y).
  path(X,Y) :- path(X,Z), edge(Z,Y).`) both `path(1, 3001)` and `path(1, 51)` **fail**, and so does
  `findall(Y, path(1,Y), L), length(L, 3000)`. On v4 all three now succeed.
  The new `core.engine.v4.Tabling` holds one **variant table** per tabled subgoal
  (`{ status, answers, dependencies }`) on the `Engine`; the variant key is a numbervars-style
  canonical encoding computed on the **cell model**, never on variable names or a binding map, so
  `path(1, 51)` and `path(1, Y)` are just two variants of the same machinery. A tabled call is an
  ordinary **choice point on the machine** — no Java recursion per subgoal, no `QuerySolver`:
  the first call to a variant is its **generator** (a fail-driven PRODUCE phase that runs the
  clauses against a private copy of the call and records every answer, then a CONSUME phase that
  hands the answers to the caller one per redo), a later call to an EVALUATING variant is a
  **consumer** over the answers recorded so far (lazily, by index, so answers appended later in the
  same round are consumed too — this is what makes left recursion work), and a call to a COMPLETE
  table is a consumer over the final list. **Completion** uses the classic DFN/leader SCC scheme:
  the leader of an SCC re-runs its clauses (deduplication makes the re-execution semi-naive) until
  a round adds no answer, then every table of the SCC is marked COMPLETE. There is no iteration
  cap — termination follows from the finite, deduplicated answer set. A round is repeated only when
  it both grew the answer set and read an incomplete table, so plain memoisation (tabled `fib/2`)
  costs exactly one pass. Handled and tested: left, right and doubly recursive definitions;
  mutual recursion across two tabled predicates; cyclic graphs; tabled calls inside
  `findall/3`, `\+/1`, `catch/3`, `once/1`, `forall/2`, `bagof/3`; a `!` in a tabled clause body
  (local to the body — it can never abort the production of the table); and an evaluation abandoned
  by an exception, a cut or the resource guard, which discards its half-built tables so the next
  call recomputes instead of reading a partial answer set. The `ResourceGuard` is charged inside the
  fixpoint, so the inference budget and the Stop interrupt abort a runaway tabled query (they never
  did before). Four-port tracing works: the generator/consumer choice point carries
  `traceGoal`/`traceDepth` (invariant 14) and the internal PRODUCE phase emits no ports of its own.
- **ISS-2025-0464** — the **tabling built-ins on the v4 store**. `abolish_all_tables/0` and
  `abolish_table/1` are v4 natives that clear the v4 answer tables (and the legacy `TableStore`, so
  the two never diverge); `abolish_table/1` also un-declares the predicate, as it always has.
  Both raise `permission_error(modify, table, ...)` when called from inside a running tabled
  evaluation, and `abolish_table/1` raises `instantiation_error` /
  `type_error(predicate_indicator, T)` instead of failing silently. New **`current_table/2`**
  (v4 only) enumerates the live tables as `current_table(Variant, complete|incomplete)`.
  **Invalidation policy**: asserting to or retracting from a tabled predicate drops that
  predicate's tables (never while an evaluation is running); a change to a *non-tabled* predicate
  that a tabled one depends on is not tracked — call `abolish_all_tables/0`. Tables persist across
  queries; two safety caps (100 000 tables, 4 000 000 answers) drop the oldest COMPLETE tables at a
  query boundary so an embedder cannot grow the store without bound. `tnot/1` is **not**
  implemented and raises `existence_error(procedure, tnot/1)`.
- **ISS-2025-0465** — **`Machine.tabledDelegate` is deleted.** It was the last routine path from a
  v4 query into the recursive `QuerySolver.solveInternal`; the only remaining one is a sub-solve
  arriving from a **worker thread** (`concurrent/3`, `concurrent_maplist/N`, `first_solution/3`),
  which wave W8 removes. `EngineV4LibraryTest.testISS0450_NoBuiltinReachesTheRecursiveSolver` now
  includes tabled queries and `abolish_all_tables/0` in its zero-entry assertion.

Measured (v2 vs v4, same session, loaded VM; full table in the progress report section 11):
`path(1, 3001)` on the left-recursive 3 000-edge chain **FAIL / 201 ms OK**; `path(1, 51)`
**FAIL / 118 ms OK**; `findall(Y, path(1,Y), L), length(L, 3000)` **FAIL / 81 ms OK**;
the **100 000-edge chain `path(1, 100001)`** (design target B.15: correct, <= 2 s)
**FAIL (40 ms) / 868 ms OK**; the right-recursive 3 000 closure **FAIL / 4990 ms OK**;
`odd(400)` on the tabled `even/odd` pair **wrong (fails) / correct**.

### Documentation

- `docs/references/BUILTIN_PREDICATES_REFERENCE.md`: the tabling section rewritten — semantics,
  the invalidation policy, `current_table/2`, the ISO errors of `abolish_table/1`, and what is
  still v2-only behaviour.
- `docs/guides/guide-tabling-predicates.md`: the "how it works internally" section now describes
  the two implementations (v4 linear tabling with completion, v2/legacy bounded re-evaluation).
- `docs/tracking/track-limitations.md`: LIM-037 narrowed (the tabling bullet is resolved on v4).
- `docs/reports/report-engine-v4-progress.md`: new section 11 (wave W5) with the exact W6 starting
  point.

## [3.11.0] - 2026-08-25

### Engine v4 (opt-in) — wave W4: coroutining and attributed variables

Wave W4 of `docs/reports/report-engine-v4-design-2026-08-25.md` (part B, section B.9),
ISS-2025-0457..0462. **The default engine is still v2**; everything below applies to
`-Djprolog.engine=v4` / `Prolog.setUseV4Engine(true)` unless it says otherwise. Progress,
invariants and the starting point for wave W5 (tabling, design B.8) are in
`docs/reports/report-engine-v4-progress.md` (new section 10).

Baseline: **1065/1065 JUnit tests on the default engine AND on v4** (1043 pre-existing + 22 new in
`core/engine/v4/EngineV4CoroutiningTest`), **20/20 example programs on both engines with
byte-identical output**.

- **ISS-2025-0457** — the **wake queue** (`core.engine.v4.Coroutining` + `Machine.wake`).
  Binding an attributed cell — to a value or, by aliasing, to another variable — no longer runs
  anything inside the term walk: it pushes one wake goal per attribute module, and the drive loop
  runs the queue **before the next goal, in the current binding context**. A woken goal is
  therefore an ordinary goal — its bindings are ordinary bindings that propagate, it is traced
  through the four ports, it is charged to the `ResourceGuard` (so a runaway woken goal is aborted
  by the inference budget and by Stop), and an exception it throws reaches the enclosing
  `catch/3`. Attribute changes are trailed (`Coroutining.putAttr`/`delAttr`), and **so is the queue
  push**: a clause head that binds an attributed cell and then fails on a later argument leaves no
  stale wake behind, and backtracking re-arms the suspension instead of consuming it.
  `Variable`'s attribute map became a `LinkedHashMap` so the wake order is reproducible.
  The wave-W1 `Machine.AttrBridge` (freeze handled natively, everything else through the legacy
  `Variable.getAttributeUnifyHook()` with a name-keyed *view* of the bound cells) is deleted.
- **ISS-2025-0458** — the **SWI attributed-variable protocol** as v4 natives: `put_attr/3`,
  `get_attr/3`, `del_attr/2`, `attvar/1`, plus the new `term_attvars/2`, `copy_term/3` (the copy
  carries no attributes; the residual goals are expressed over the copy's variables) and
  `unifiable/3`. `Module:attr_unify_hook(AttValue, Other)` is called through the normal goal stack
  and is **user-definable in Prolog** — JProlog stores a module-qualified clause head as a `:/2`
  predicate, so the machine calls it with a flat lookup (`Machine.callQualified`). An attribute of
  a module with no hook is inert data, as on the legacy engines.
- **ISS-2025-0459** — `freeze/2`, `frozen/2`, `when/2`, `dif/2` and `?=/2` are **Prolog clauses**
  in the new `src/main/resources/prelude/coroutining.pl`, loaded through the W3 prelude loader on
  top of `put_attr/get_attr` and `attr_unify_hook` (the Java `builtin.control.{Freeze,When,Dif}`
  stay registered for the legacy and v2 engines). This is what fixes **ISS-2025-0336 on v4**: the
  bindings a `when/2`-woken goal makes now survive (`when(nonvar(X), Y = done), X = 1, Y == done`
  succeeds; on v2 only the goal's side effects survive). `when/2` supports `nonvar/1`, `ground/1`,
  `?=/2` and conjunctive/disjunctive conditions, raises `instantiation_error` /
  `domain_error(when_condition, C)`, and fires **exactly once** for a disjunctive condition
  attached to several variables (a shared, backtrackable "fired" flag). `dif/2` decides ground and
  non-unifiable pairs immediately and otherwise re-suspends on the **remaining unifier variables**
  (`unifiable/3`), so `dif(f(X), f(Y)), X = 1, Y = 1` fails and `..., Y = 2` succeeds.
- **ISS-2025-0460** — **CLP(FD) on the attribute hook, and the compatibility shim is gone.**
  `ClpfdV2Bridge` keeps the engine cell of every FD variable it attributes
  (`cellFor/1`, `onBindByName/2`, `domainTermForCell/1`), and the v4 hook reaches it through the
  prelude's `'$attr_hook'/4` -> the native `'$clpfd_unify_hook'/2`. That makes an FD variable an
  ordinary attributed cell and lets **`Machine.nameIndex` / `Machine.cellFor` / `Machine.indexCells`
  (deviation 2 of waves W1-W3) be DELETED**: the two tests that failed without them —
  `testISS0357_LabelingBindsDeterminedVariables` and `testISS0421_AbsExpression` — pass because
  `LegacyBuiltinAdapter` now asks the bridge, not an engine-wide name index, for the cell behind a
  variable name `exportSingletons/1` reported.
- **ISS-2025-0461** — **cross-query coroutining is dropped on v4** (design decision 3, B.17,
  approved). The v4 route no longer splices or refreshes `Prolog.attributedSessionVars` and it
  uninstalls the process-wide legacy attribute hook for the duration of a query, so a query's
  variables die with the query: `when(nonvar(X), throw(leak))` followed by `X = 1` in a later query
  succeeds silently (on v2 it throws `leak`), and the same holds for `freeze/2` and `dif/2`. The
  v2/legacy session behaviour is untouched; nothing in the IDE or the CLI used it
  (`clearSession()` remains for those engines).
- **ISS-2025-0462** — `Prolog.residualGoals(Map<String,Term>)`: the goals still attached to an
  answer's variables (`freeze/2`, `when/2`, `dif/2`, CLP(FD) `in/2`, and `put_attr/3` for anything
  else). This is what an answer printer needs (limit L-11); the CLI and the IDE start printing it
  in wave W7 — **nothing prints it yet**.

### Documentation

- `docs/references/BUILTIN_PREDICATES_REFERENCE.md`: new v4 entries (`term_attvars/2`,
  `copy_term/3`, `frozen/2`, `unifiable/3`, `?=/2`, `attr_unify_hook/2`).
- `docs/tracking/track-limitations.md`: LIM-037 narrowed (coroutining bullet resolved,
  ISS-2025-0336 fixed on v4, tabling bullet added for W5).
- `docs/reports/report-engine-v4-progress.md`: new section 10 (wave W4) with the exact W5 starting
  point.

## [3.10.0] - 2026-08-25

### Engine v4 (opt-in) — wave W3: native library and meta-calls

Wave W3 of `docs/reports/report-engine-v4-design-2026-08-25.md` (part B, section B.16),
ISS-2025-0450..0456. **The default engine is still v2**; everything below applies to
`-Djprolog.engine=v4` / `Prolog.setUseV4Engine(true)` unless it says otherwise. Progress,
invariants and the starting point for wave W4 are in `docs/reports/report-engine-v4-progress.md`
(new section 9).

Baseline: **1043/1043 JUnit tests on the default engine AND on v4** (1024 pre-existing + 19 new in
`core/engine/v4/EngineV4LibraryTest`), **20/20 example programs on both engines with byte-identical
output**. Timings are best-of-3 in a warmed JVM on a loaded VM; v2 and v4 were measured in the same
session.

- **ISS-2025-0450** — `SolverFacade.solve(Term, Map, List, CutStatus)` runs on
  `Machine.runSubQuery`. That four-argument entry point was the last piece of the **recursive**
  `QuerySolver` algorithm reachable from a v4 query: `phrase/2,3`, the DCG helpers, `format ~p`,
  the persistence transactions and the tabling driver all called it, and all of them inherited its
  2 000-deep Java recursion cap. Cut semantics: the recursive solver reported a cut through the
  caller's `CutStatus`, `runSubQuery` gives a goal-local cut — which is correct for every caller
  that survives on v4, because the constructs that really propagate a cut outwards (`,/2`, `;/2`,
  `->/2`, `\+/1`, `call/N`, `catch/3`) are native in the machine and never reach a built-in. An
  already-cut status is still honoured (the goal does not run). A call arriving from a **worker
  thread** (`concurrent/3`, `concurrent_maplist/N`, `first_solution/3`) falls back to the recursive
  algorithm, because a `Machine` is single-threaded by construction; threads get their own machines
  in W8. `QuerySolver.internalSolveCount()` is a new test hook and
  `EngineV4LibraryTest.testISS0450_NoBuiltinReachesTheRecursiveSolver` asserts the counter does not
  move across a v4 query exercising phrase, bagof, `format ~@`, forall, with_output_to, findall,
  maplist and catch.
- **ISS-2025-0451** — native `phrase/2,3`. The grammar body is translated with the default
  `core.dcg.v2.DCGTranslator` and the resulting goal is **pushed onto the machine's goal stack**;
  the token list and the rest argument are passed as cells and never walked. A DCG over
  **1 000 000 tokens** now parses in ~2.9 s at the *default* JVM stack, where v2 (and v4 before this
  change) died with `resource_error(stack_overflow)` even at `-Xss4m`. The ISO 13211-3 error
  clauses are kept (`type_error(list, L)` for a non-list input/rest, `type_error(callable, B)` for a
  number/string body, `instantiation_error` from `call/3` for an unbound body), and the inference
  budget and the Stop interrupt now fire *inside* a parse.
- **ISS-2025-0452** — native `bagof/3`, `setof/3`, `aggregate_all/3`, `with_output_to/2`, and a new
  `~@` directive in `format/2,3`.
  - `bagof/setof`: ISO 8.10.2/8.10.3 over `Machine.findAll` — `^` stripping, free-variable
    grouping by **variant** witness (ISS-2025-0411), witness merging, and standard-order group
    enumeration for `setof/3` (ISS-2025-0412). Groups are handed out lazily by a `Generator`.
  - `aggregate_all/3`: `count` / `sum` / `max` / `min` / `bag` / `set` with the ISS-2025-0413
    (max/min *fail* when the goal has no solution) and ISS-2025-0414 (exact BigInteger sums, float
    contagion, `type_error(number, T)`) rules unchanged. New: `max(Value-Witness)` /
    `min(Value-Witness)` compare on the numeric left-hand side and answer with the winning pair.
  - `with_output_to/2`: captures through the thread-local `StreamManager` override (and
    `System.out`, for the built-ins that still write there) and restores whatever was installed;
    `atom/1`, `string/1`, `codes/1` and `chars/1` sinks are all supported (the legacy built-in
    handled only `atom/1` and left `System.out` swapped process-wide — see LIM-025).
  - `~@` (**both engines**): `format("~@", [Goal])` runs `Goal` and splices its output in. It did
    not exist before; `~p` (portray) already ran a sub-goal and now goes through the machine too.
- **ISS-2025-0453** — native `Builtin`/`Generator` implementations for the list, atom and database
  library: `member/2`, `memberchk/2`, `append/3`, `select/3`, `selectchk/3`, `nth0/3`, `nth1/3`,
  `last/2`, `reverse/2`, `length/2`, `msort/2`, `sort/2`, `sum_list/2`, `sumlist/2`, `numlist/3`,
  `copy_term/2`, `clause/2`, `sub_atom/5`, `sub_string/5` (`core.engine.v4.NativeLibrary`).
  - This closes the one place where v4 was **slower** than v2. A bridged built-in made the machine
    dereference the whole goal and index its unbound cells by name, both O(list): with a
    1 000 000-element list v4 was 1.3-2x slower than v2. Now, same session, best-of-3:
    `length` 1466 -> **1001** ms, `msort` 1257 -> **1064** ms, `copy_term` 1571 -> **1273** ms,
    `==` 709 -> **405** ms, `findall+member` 2341 -> **1499** ms, `sum_list` 734 -> **318** ms,
    `reverse` 1147 -> **692** ms, `append` 2900 -> **1208** ms (v2 -> v4).
  - The nondeterministic ones are real lazy generators: one alternative per redo, O(1) memory, and
    the walk stops the moment the caller cuts — `once(member(X, MillionElementList))` no longer
    materialises a million solution maps first. A generator that hands out its last alternative
    calls `Machine.lastSolution()`, so the deterministic cases leave no choice point behind.
  - `clause/2` runs over the `ClauseStore` with the caller's generation (logical update view) and
    keeps `permission_error(access, private_procedure, PI)` on a built-in.
  - `sub_atom/5` fixes a hang that also affects v2: `sub_atom(abc, B, L, A, '')` looped forever in
    `String.indexOf("", idx)`, which stops advancing past the end of the atom. On v4 an empty
    substring enumerates the `n+1` positions once. (The legacy built-in is untouched, so v2 still
    hangs there — the fix belongs to the built-in rewrite, not to W3.)
  - New machine API: `Machine.unifyOrUndo` (a failed unification can leave partial bindings, and a
    generator that tries several alternatives inside one `next()` must undo them itself),
    `Machine.lastSolution`, and `pushGenerator` now owns the Exit/Redo/Fail ports of the goal that
    installed it, so the natives keep the four-port trace contract.
- **ISS-2025-0454** — the **Prolog prelude** (design B.10, decision 4). `maplist/2..7`,
  `foldl/4..7`, `include/3`, `exclude/3`, `partition/4` and `partition/5` are now Prolog clauses in
  `src/main/resources/prelude/apply.pl`, loaded into a new **library layer** of the v4
  `ClauseStore` when the engine is created. They are consulted only when the knowledge base has no
  clause for the same indicator, so **a user definition simply replaces them** — the quicksort
  `partition/4` of `examples/test_16_sorting.pl` still wins, with no unregistering trick. The
  prelude is not written to the `KnowledgeBase`, so the legacy and v2 engines, `listing/1`,
  `clause/2` and the IDE never see it. `partition/4`, deliberately unregistered as a Java built-in
  since v3.6.0, is therefore available again on v4. `maplist(dbl, L, L2)` over 200 000 elements:
  1634 ms on v2 -> **729 ms** on v4, linear, and cancellable.
- **ISS-2025-0455** — `library(yall)` lambdas in `call/N` (`core.engine.v4.Lambdas`):
  `Params>>Body`, `Free/Params>>Body`, `\X1^...^Xn^Body` and `Free/\X^Body`. The lambda is
  copied before every call, except the variables named in `Free`, which are unified back with the
  caller's after the copy. `maplist([X,Y]>>(Y is X*2), [1,2,3], L)` gives `L = [2,4,6]`;
  `N = 10, maplist(N/[X,Y]>>(Y is X*N), [1,2], L)` gives `L = [10,20]`. On v2 these still raise
  `existence_error(procedure, >>/4)`.
- **ISS-2025-0456** — `subsumes_term/2` was wrong on v4 for the variable-to-variable case.
  `Unify.subsumes` tested "no variable of Specific has a `ref` afterwards", but `bindVar` binds the
  **younger** cell to the older one, so `subsumes_term(f(X), f(Y))` answered false where v2 (and
  every other Prolog) answers true. The condition is now SWI's: after the match the variables of
  Specific must still dereference to *distinct unbound cells* — aliasing to a variable of General is
  fine, becoming non-variable or collapsing two of them into one is not
  (`subsumes_term(f(A,A), f(B,C))` stays false). `bagof/3`'s variant grouping inherited the bug.

#### Budget / interrupt matrix (7 goal shapes, same session)

| | v2 | v4 |
|---|---|---|
| inference budget aborts | 4 / 7 | **7 / 7** |
| thread interrupt cancels | 2 / 4 | **4 / 4** |

The v2 misses are `phrase` (stack overflow first), and `member/2`, `sub_atom/5` and
`with_output_to(_, member(...))`, which are eager Java loops that never charge the `ResourceGuard`.

#### Still deliberately deferred on v4

- The bounded name->cell shim (`Machine.indexCells`/`cellFor`, deviation 2 of the progress report)
  **stays**. Removing it was tried with the whole suite as the oracle: two tests fail without it
  (`testISS0357_LabelingBindsDeterminedVariables`, `testISS0421_AbsExpression`), both from
  still-bridged built-ins that report a binding by NAME for a cell their goal never mentions. It
  goes away with W4, when CLP(FD) moves onto the native attribute hook.
- `append/3`'s fully-open mode still yields the single standard solution rather than enumerating
  infinitely, and `member/2` does not extend an open tail — parity with v2, pinned by
  `testISS0379_AppendFullyOpenDoesNotThrow`. Both become the real two-clause definitions when the
  list library moves to the prelude in W6.
- `current_op/3` is not native: it reads the still process-global `OperatorDefinition` table
  (LIM-034) that W7 replaces, and it has no performance problem to solve.

---

## [3.9.0] - 2026-08-25

### Engine v4 (opt-in) — waves W1 (foundations) and W2 (clause store)

First two waves of `docs/reports/report-engine-v4-design-2026-08-25.md` (part B, section B.16),
ISS-2025-0438..0447. The clean-room v4 resolution core lives in `core.engine.v4` and is selected
with `-Djprolog.engine=v4` or `Prolog.setUseV4Engine(true)`. **The default engine is still v2**
(design decision 1: v4 becomes the default in wave W8 and v2 stays selectable for one release
after that). Progress, file map and the exact starting point for wave W3 are in
`docs/reports/report-engine-v4-progress.md`.

Baseline: **1024/1024 JUnit tests on the default engine AND on v4** (989 pre-existing + 35 new in
`core/engine/v4/EngineV4Test`), **20/20 example programs on both engines with byte-identical
output**. Timings are best-of-3 in a warmed JVM on a loaded VM; v2 and v4 were measured in the
same session.

#### W1 — foundations

- **ISS-2025-0438** — `Variable` is a mutable reference cell (design B.2) and the B.14 identity
  audit. New fields: `ref` (the binding, `null` when unbound), `serial` (a JVM-unique creation
  number driving conditional trailing and the `_G<serial>` print name) and a no-argument
  constructor for fresh, initially unnamed cells. `equals`/`hashCode` are now **identity**. Two
  compatibility guarantees keep the name-keyed engines working unchanged: `getName()` is unique per
  cell, so name equality still implies cell equality; and `Variable.unify(Term, Map)` — the legacy
  binding API used by the ~400 registry built-ins — deliberately keeps comparing variables **by
  name**, because on that path two objects sharing a name (JpcReader, the legacy parser) really are
  one logical variable.
  - The audit found one live hazard: `Variable.copy()` returned a *different object with the same
    name*, and ~10 built-ins (`arg/3`, `member/2`, `nth0/nth1`, `select/3`, `aggregate_all/3`,
    `CollectionUtils`) call `x.copy()` and then unify with the result, relying on the alias. A named
    variable now copies to **itself** — a no-op for the name-keyed engines, correct for the cell
    model. Anonymous variables still copy to a fresh one.
- **ISS-2025-0439** — clause skeletons (design B.2). Consulting compiles each clause once into a
  numbered-variable template (`core.engine.v4.Clause` + `VarRef`); activation allocates one
  `Term[nvars]` frame, the head is unified **directly against the skeleton** and body goals are
  instantiated only when pushed. A first-occurrence head variable is aliased straight to the goal's
  sub-term, so no cell is allocated and nothing is trailed for it. This removes the per-activation
  `HashMap<String,Variable>` and the `"_R<id>_<name>"` strings of the v2 engine. Skeletons are
  cached on the `Rule` (`Rule.getCompiled`/`setCompiled`), so re-syncing a predicate is a pointer
  copy.
- **ISS-2025-0440** — bindings, trail and choice points (design B.3). `Bindings` owns only the
  trail (cells + undo actions) with **conditional trailing**: a binding is trailed when an explicit
  mark/undo extent is open or the cell is older than the newest choice point; when neither holds
  the trail is cleared outright. Choice-point frames carry the trail mark, the serial watermark and
  the legacy `Trail` mark, and are trust-me popped when they owe nothing more.
- **ISS-2025-0441** — `Unify`: every walker is iterative, cycle-safe and cancellable (design B.4).
  `unify`, `==`, standard order, `copy_term`, `term_variables`, `ground`, `numbervars`,
  `subsumes_term`, `cyclic_term`/`acyclic_term` and the full dereference all iterate on the LAST
  argument, switch to a visited set of identity pairs (or a Brent tortoise/hare spine test) past a
  threshold, and charge the `ResourceGuard` once every 4096 iterations.
  - **Rational trees are supported** (design decision 2, approved): `X = f(X), Y = f(Y), X = Y` and
    `X = [1|X], Y = [1|Y], X = Y` now **succeed** in milliseconds. On v3.8.0 they hung forever and
    polled nothing, so neither the inference budget nor a thread interrupt could stop them — a DoS
    (limit L-04). `cyclic_term/1` and `acyclic_term/1` are real tests.
  - `set_prolog_flag(occurs_check, error)` is accepted (ISO 7.11.2.4 defines three values, not
    two) and restores the ISO `representation_error(cyclic_term)` behaviour on v4.
- **ISS-2025-0442** — the machine core (design B.6). Native control: `,` `;` `->` `*->` `\+` `!`
  `call/N` `^` `:` `findall/3` `catch/3` `throw/1` `assert*` `retract/1` `once/1` `ignore/1`
  `forall/2` `between/3` `repeat/0` `length/2` enumeration, plus **cleanup frames** for
  `setup_call_cleanup/3` and `call_cleanup/2`. `OutOfMemoryError` becomes a catchable
  `resource_error(memory)` and a `StackOverflowError` from a legacy built-in is converted to
  `resource_error(stack_overflow)` **inside** the drive loop, so the running program's `catch/3`
  can see it (limit L-14). Four-port trace/debug events are emitted exactly as on v2 — the trace
  output of the two engines is byte-identical.
- **ISS-2025-0443** — the v4 built-in SPI and the legacy adapter (design B.5). `Builtin` /
  `Generator` / `BuiltinTable` ((name, arity) -> entry); `LegacyBuiltinAdapter` runs every existing
  `BuiltIn` class unchanged on a resolved goal plus an empty map, installing the returned bindings
  by unifying the goal's own cells; `SolverFacade` (a `QuerySolver` subclass) gives
  `BuiltInWithContext` classes `solve`/`solveMeta`/`getResourceGuard` on top of a nested drive that
  shares this query's trail, choice-point floor and guard. `setarg/3` and `nb_setarg/3` are native
  (they need object identity, which no longer has a name-keyed fallback), as are `cyclic_term/1`,
  `acyclic_term/1`, `term_variables/2`, `ground/1`, `numbervars/3`, `subsumes_term/2` and
  `compare/3` (the last one now validates its `Order` argument).
- **ISS-2025-0444** — the opt-in switch, the embedding API and the hardening contract.
  `-Djprolog.engine=v4` / `Prolog.setUseV4Engine(boolean)` / `Prolog.isUsingV4Engine()`;
  `solve(String)`, `solve(Term)` and `solveStream` all route to v4 when selected, and `solveStream`
  is natively lazy there. `enableSafeMode()` and `setInferenceBudget()` work unchanged, and
  `InferenceLimitException` / `QueryCancelledException` / `DebugStopException` remain plain
  `RuntimeException`s that `catch/3` cannot trap. New Maven profile `-Pengine-v4` runs the whole
  suite on v4.

#### W2 — clause store

- **ISS-2025-0445** — `ClauseStore` with birth/death generations (design B.7). Every clause carries
  a generation interval and a call captures `(array, size, generation)`, so `assertz` appends in
  place (amortised O(1)), `asserta` installs a new array, and `retract` just sets a death
  generation — no snapshot is rebuilt on any write, and the ISO logical update view falls out of
  the generation test. Dead clauses are compacted at the next query boundary, the only moment at
  which no running call can hold an older generation. The first-argument index is incremental and
  needs **no bucket cap**: it is keyed only by keys that actually occur in a clause head, so
  `loop(1000000)`-style recursion adds nothing to it (that unbounded growth is what forced the
  512-entry cap on the v3.8.0 `KnowledgeBase` cache). `KnowledgeBase` remains the database of
  record — v4 writes to both in step, and a write by any other route is picked up through the new
  `KnowledgeBase.getPredicateVersion`.
- **ISS-2025-0446** — `asserta`/`assertz`/`retract` are native on the store;
  `retractall/1`, `abolish/1`, `clause/2`, `listing/0,1`, `predicate_property/2` and `dynamic/1`
  keep running as registry built-ins over the `KnowledgeBase` and are observed correctly through
  the re-sync path (verified by `EngineV4Test.testISS0446_*`).
- **ISS-2025-0447** — `.jpc` format 0x03: variables are serialised **by index within their clause**
  (index + name) and each clause records its source line. Two bugs closed: a clause read back from
  a `.jpc` file used to get one distinct `Variable` object per occurrence of the same name (fatal
  under identity variables), and `Rule.sourceLine` was lost by compilation, so the IDE's line
  breakpoints silently did nothing on compiled sources. `Parser.extractClauses` now also records
  each clause's start line (`Parser.getLastClauseLines`), which `Prolog.compile` stamps onto the
  rules. Older 0x01/0x02 files fail the version check and are transparently recompiled.

#### Post-review fixes (independent verification of W1/W2)

- **ISS-2025-0448** — `findall/3` was **not opaque** on v4: the template variable kept its last
  binding (`findall(X, member(X,[1,2]), L), X == 2` succeeded; `var(X)` failed). Root cause: the
  nested drive is bracketed by a forced-trail extent, but the `finally` block closed the extent
  (`forceTrail--`) *before* undoing to its mark — and `cutTo()` ends in
  `Bindings.clearIfUnreachable()`, which drops the whole trail as soon as `forceTrail` is 0 and no
  choice point is left. The undo then had nothing to undo. Every construct that undoes to a mark
  while the choice-point stack may be empty had the same ordering: `findall/3`, `Machine.runSubQuery`
  (hence `bagof/setof/aggregate_all` and every other meta-call through `SolverFacade`), `\=/2`, the
  `catch/3` catcher unification on a non-matching catcher, and `Unify.subsumes`. All of them now
  undo *inside* the extent and close it afterwards; `Bindings.undo` also treats a stale mark above
  the current top as a no-op instead of resurrecting freed slots.
- **ISS-2025-0449** — retract/assert loops were **superlinear** on v4: dead clauses were only
  compacted at the query boundary, so `cnt(N) :- retract(counter(C)), …, assertz(counter(C1)), …`
  grew the predicate's array by one dead clause per iteration and both the retract candidate scan
  and the clause-iterator scan became O(#clauses). `cnt(100000)` took ~60 s (v2: ~5 s). Waiting for
  the query boundary turned out to be unnecessarily conservative: compaction installs a **new**
  array and never touches any clause's `birth`/`death`, so a call that already captured
  `(array, size, generation)` keeps a valid snapshot and the logical update view is preserved
  whatever the moment. `retractClause` now compacts in place once the dead clauses outnumber the
  live ones and there are at least 32 of them — amortised O(1). **Measured**: `cnt(100000)`
  60 s → **0.56 s** (v2 1.39 s in the same session); the predicate's physical clause count stays at
  ≤ 33 throughout the loop instead of growing to N.
- **ISS-2025-0446** was cited in the tracking documents without a matching `START_CHANGE` tag in
  `src`; the tag is now on the v4 database section of `Machine.java`.

#### Measured (v2 vs v4, same session)

| Benchmark | v2 | v4 |
|---|---|---|
| `nrev` 30 x 2000 | 3139 ms (~316 KLIPS) | **494 ms (~2008 KLIPS)** |
| `loop(10000000)`, `-Xmx64m` | OutOfMemoryError | **9.4 s, 4 MB used** |
| `loop2(1000000)` (one registry built-in per iteration), `-Xmx64m` | OutOfMemoryError | **1.6 s** |
| `X = f(X), Y = f(Y), X = Y` | hangs (uncancellable) | **59 ms, succeeds** |
| 20 000-clause lookup, first argument bound | 34-304 ns net | **118-259 ns net** |
| deterministic loop iteration (same harness) | 2538 ns | **763 ns** |
| 100 000 interleaved `assertz` + call | 7281 ms | **1039 ms** |
| `cnt(100000)` (retract + assertz per iteration) | 1387 ms | **558 ms** |
| `between(1, 10^7, X), X >= 10^7` | 2171 ms | **1738 ms** |
| 1 000 000-element list: length / msort / copy_term / == / ground / findall+member | all OK | all OK (0.5-4 s, 1.3-2x slower than v2 pending the W3 native list built-ins) |

#### Behaviour changes on v4 only (design B.14/B.17)

- Rational trees are supported: queries that used to raise `representation_error(cyclic_term)` now
  succeed. `set_prolog_flag(occurs_check, error)` restores the ISO behaviour.
- `setup_call_cleanup/3` and `call_cleanup/2` run `Cleanup` when `Goal` has **no alternatives
  left** (ISO/SWI), not eagerly after the first solution as the bridged legacy built-in does.
- Both are pinned by engine-aware assertions in `BugFixVerificationTest` (ISS-2025-0397,
  ISS-2025-0273).

---

## [3.8.0] - 2026-08-25

### Engine deep analysis — wave 5: clause selection, arithmetic and housekeeping (ISS-2025-0433..0436)

Fifth wave of `docs/reports/report-engine-deep-analysis-2026-08-24.md` (findings ENG-13, ENG-14,
ENG-15, ENG-17). Baseline: **983/983 JUnit tests, 20/20 example programs.**

All timings below are best-of-3 in a warmed JVM (`-Xss4m -Xmx2g`, JDK 25), measured against the
v3.7.0 build on the same machine.

- **ISS-2025-0433** (ENG-13): clause selection was O(#clauses) per call **with a full list copy**.
  A lookup in a 20 000-fact table cost 2.36 ms — the *same* whether the first or the last clause
  matched, because the cost was per-call setup, not unification:
  `KnowledgeBase.getRulesForPredicate` copied the predicate's entire clause list under
  `synchronized` on every call, `callUser` then allocated one `Alt` lambda per clause *before* the
  first head unification, and `renameRule` copied head **and** body of every candidate before
  trying the head.
  1. **Versioned immutable snapshots**: every predicate owns a `PredEntry` with a version counter
     bumped on assert/retract, the immutable full-clause snapshot, and the immutable
     first-argument bucket snapshots. A snapshot is built at most once per version; reads take no
     lock and no copy, and the ISO logical update view comes for free because a published snapshot
     is never mutated. The first-argument bucket cache is **capped** (512 buckets per predicate):
     a recursive predicate called with a different integer every time produces a distinct key per
     call, so an unbounded cache would turn a deterministic recursion into a memory leak.
  2. **Lazy clause choice point**: the frame holds `(snapshot, index)` and pulls one clause per
     redo instead of pre-building every alternative. It also participates in the wave-3 trust-me
     pop, so a call that commits to its last clause leaves no frame behind.
  3. **Head-first renaming**: only the head is renamed before unification; the body is renamed
     afterwards, sharing the same variable map, and a ground fact is not renamed at all.
  4. **First-argument indexing re-landed on the v2 path** (it was reverted as ISS-2025-0340 for
     silently dropping clauses). Three things make it safe now: an index miss degrades to the full
     clause list (ISS-2025-0344), the variable-headed bucket is *always* merged in, and any first
     argument that cannot be keyed (unbound, a string) falls back to the full list. The numeric key
     is now **type-faithful** — integers key on their exact value, floats on theirs, so `1` and
     `1.0` no longer share a bucket (they do not unify — ISS-2025-0261) and integers beyond 2^53 no
     longer collide.
  - One behavioural subtlety the change had to fix: with indexing, an empty candidate list usually
    means "this predicate has clauses, but none can match this first argument", which is a plain
    **failure** — only a predicate with no clauses at all is an unknown procedure.
  - **Measured**: 20 000-fact lookup **2.358 ms → 0.002 ms per call (~1200×)**; `nrev` 30×2000
    **266 → 324 KLIPS**; `loop(1000000)` 2414 → 2160 ms.
- **ISS-2025-0434** (ENG-14): the arithmetic hot path allocated on every evaluation.
  `evalNum` deep-copied the expression with `resolve()` and then handed `ArithEvaluator` an empty
  `HashMap` — which *still* called `resolveBindings()` at **every node**, walking each sub-term once
  per level (O(n²) traversals for an n-node expression). `+`, `-` and `*` allocated three
  `BigInteger`s per operation even for single-digit integers, and `numRel` compared through
  `BigInteger` even when both operands were small `long`s.
  - `ArithEvaluator.evalDeref(expr, deref)` evaluates against the machine's binding store through
    an O(1)-per-node dereference hook: no copy, no map.
  - `+`, `-`, `*` take a primitive `long` path guarded by `Math.addExact`/`subtractExact`/
    `multiplyExact`, falling back to the exact `BigInteger` path on overflow.
  - `numRel` compares primitives when both operands fit in a `long`.
  - `Number.valueOf(long)` caches −128..1024; `structuralEqual` short-circuits on object identity
    (which, now that `resolve` shares structure, makes `L == L` on a big list O(1)).
  - **Measured**: `arith(1000000)` (one `A1 is A0 + N*2 - 1` per iteration) **5177 → 4317 ms**.
- **ISS-2025-0435** (ENG-15): `collectVars` used `List.contains` (O(n²) in the number of distinct
  query variables) and now uses a `HashSet`; the drive loop polls `Thread.isInterrupted()` once per
  1024 steps through the `ResourceGuard` instead of on every iteration. *Not done*: the
  `name + "/" + arity` key strings and the per-choice-point `Trail.mark()` ThreadLocal lookup are
  unchanged — both are single-digit-nanosecond costs that the wave's other changes dwarf, and
  removing them needs an API change to `KnowledgeBase`.
- **ISS-2025-0436** (ENG-17): removed `core/engine/CompiledClause.java` and
  `core/engine/Interpreter.java` (no callers), and `KnowledgeBase.multiArgIndex` with its unused
  `getRulesWithMultiArgIndex` accessor — a second nested index built and maintained on every
  assert/retract that nothing ever read. `AtomTable` and `LayeredMap` are kept: both still have
  callers. `firstArgIndex` is no longer maintained-but-unused — the default engine reads it now.

### Engine deep analysis — wave 6: per-engine state isolation (ISS-2025-0437)

Sixth and final wave (finding ENG-06). Baseline: **989/989 JUnit tests, 20/20 example programs.**

- **ISS-2025-0437** (ENG-06) — **isolation fix**: process-global mutable state leaked across
  `Prolog` instances and threads, undermining the v3.4.0 sandbox guidance ("use a fresh `Prolog`
  per security domain"): sandboxed code could flip `unknown`, `double_quotes` or `occurs_check`
  for the host's other engines, and concurrent access to the plain `HashMap` could corrupt it.
  - **ISO flags are per engine.** `PrologFlags` is now an instantiable store; every `Prolog` owns
    one and installs it as the thread-current store around `solve/1,2`, `solveLegacy/1`,
    `solveStream/2`, `consult/1` and `consultWithDiagnostics/2`, restoring the previous store in a
    `finally` so an engine invoked from inside another engine's built-in cannot leave its flags
    behind. The static `PrologFlags` API used by the built-ins and both parsers is unchanged and
    routes to the current store; code with no engine in scope (a directly instantiated parser, a
    unit test) sees a process-wide default, exactly as before. New: `Prolog.getFlags()`.
  - **`occurs_check` is per engine**: it moved out of the static `Variable.occursCheckEnabled`
    into the flag store. `Variable.isOccursCheckEnabled()` / `setOccursCheckEnabled()` still work
    and delegate. A static volatile short-circuit keeps the (universal) off case to one volatile
    read on the unification hot path — the ThreadLocal is touched only once some engine turns the
    check on.
  - **`trace/0` is per engine**: it was a process-global static in `builtin.debug.Trace`, so
    `trace.` in one engine enabled four-port tracing in every engine in the JVM. New
    `Prolog.setTracing(boolean)` / `isTracing()` for callers that toggle tracing from *outside* a
    query (the IDE Run-panel toggle runs on the EDT, the CLI's `:trace` on the REPL thread) — both
    were updated to act on their own engine.
  - **`current_input` / `current_output` are per thread**, matching the thread-local output capture
    the IDE already relies on: `set_output/1` on one thread no longer redirects `current_output`
    for every thread.
  - **`ClpfdPredicates` temporary-variable names** come from an `AtomicLong`: the non-atomic
    static `int` could hand the same name to two threads and silently alias unrelated CLP(FD)
    variables.
  - *Not changed* (documented as LIM-034): `OperatorDefinition.sharedOperatorTable` /
    `currentModuleContext`, the legacy `ConstraintStore` singleton, `builtin.debug.Spy.spyPoints`
    and the `Profiler` counters are still process-global. The operator table in particular is
    shared with the parser, `op/3` and the `.jpc` format, so making it per engine is a change of a
    different size.
  - One existing test observed the now-per-engine trace flag through the global
    `Trace.isTracingEnabled()`; `DebuggingTest` was updated to ask the engine
    (`prolog.isTracing()`). Its intent — "`trace/0` enables tracing, `notrace/0` disables it" — is
    unchanged; only the observation point moved.

---

## [3.7.0] - 2026-08-25

### Engine deep analysis — wave 3: the machine's memory model (ISS-2025-0429, ISS-2025-0430)

Third wave of `docs/reports/report-engine-deep-analysis-2026-08-24.md` (findings ENG-10, ENG-11).
Baseline: **966/966 JUnit tests, 20/20 example programs.**

- **ISS-2025-0429** (ENG-10): unbounded memory growth in deterministic execution.
  - **Trust-me pop**: `advance()` never removed an *exhausted* choice point (`idx == alts.size()`),
    so a 20 000-iteration deterministic loop left 20 000 dead frames on `cps`, each keeping alive
    everything its `Alt` closures captured (the continuation, and for a bridged built-in a full
    copy of the binding map) — and cut/backtrack had to walk past them. Once the last alternative
    has been taken the frame is dropped, provided it is on top (the only position from which
    removal cannot shift another frame's absolute cut barrier) and carries no pending Redo/Fail
    port. Cut barriers stay valid: they are captured *before* the push, and `cut()` only removes
    frames above the barrier, so a barrier that now equals `cps.size()` is simply a no-op cut.
  - **Conditional trailing**: `bind()` appended to the trail unconditionally, even with no choice
    point to undo to. It now trails only while a choice point / catch frame exists or an explicit
    mark-undo extent is open (`findall/3`, `\=/2`, the catcher unification, which bracket
    themselves with a `forceTrail` counter). *Deviation from the report*: instead of the proposed
    per-variable serial numbers, the trail is **cleared outright** whenever no live mark can reach
    it (`cps` empty and no open extent) — every `undo(mark)` in the machine takes its mark from a
    frame on `cps` or from such an extent, so the rule needs no new `Variable` field, is easier to
    prove correct, and reclaims more (including entries made before the last frame disappeared).
  - **Allocation**: `addArgs` reuses the compound's functor `Atom` instead of allocating a new one
    (the `rename`/`resolve` sites were already converted in wave 2).
  - Machine invariants are now pinned by tests: after `loop(20000)` the choice-point stack **and**
    the trail are empty.
  - **Measured** (JDK 25, `-Xss4m`): `loop(300000)` OOM at 64 MB → runs in 128 MB;
    `loop(1000000)` OOM at 256 MB → runs in 256 MB, and at 2 GB drops from **3.59 s to 2.38 s**;
    `loop(3000000)` now completes (5.8 s in 1 GB). **Residual**: memory is still O(N) in the
    *number of bindings*, because `binding` is a name-keyed `HashMap` that never reclaims a dead
    variable — that is ENG-10.1, whose fix is the object-binding rewrite ENG-16 (architecture
    track, deliberately out of scope). `loop(3000000)` therefore still needs ~1 GB, not 64 MB.
- **ISS-2025-0430** (ENG-11): registry built-in calls were quadratic.
  - `bridgeBuiltin` copied the **entire** binding map (`new HashMap<>(binding)`) on every call;
    each built-in then returned solution maps that were copies of that copy (`Member` copies twice
    per element), `applySolution` walked the whole returned map, and the exhausted choice point
    retained every copy — total cost Σ(bindings at call i) = O(N²). One `atom_length(abc,_)` per
    iteration exhausted a 2 GB heap at N = 10 000 after 27 s.
  - Built-ins now receive a **resolved goal** and an **empty** map. A resolved goal carries
    everything the built-in needs, and every variable still in it is unbound, so the empty map is a
    faithful view and the built-in returns only the bindings it creates. Variable objects and names
    survive `resolve()`, so `applySolution` installs them unchanged.
  - `MachineSolver.resolve` is now **structure-sharing** (a node whose arguments all resolve to
    themselves is returned unchanged), which is what makes the handoff cheap for goals that mention
    large ground terms.
  - A **deterministic** built-in (exactly one solution) gets no choice point at all.
  - `setarg/3` and `nb_setarg/3` keep the old unresolved-goal + full-map handoff: they mutate the
    actual bound term and need object identity (ISS-2025-0317).
  - **Measured**: `loop2(10000)` (one bridged built-in per iteration) OOM after 27 s at 2 GB →
    **157 ms**; `loop2(200000)` → **639 ms in a 128 MB heap** (was not reachable at any heap size);
    `nrev` 30×2000 **188 → 350 KLIPS**.

### Engine deep analysis — wave 4: meta-calls and generators on the machine (ISS-2025-0431, ISS-2025-0432)

Fourth wave (findings ENG-04, ENG-12). Baseline: **974/974 JUnit tests, 20/20 example programs.**

- **ISS-2025-0431** (ENG-04) — **security fix**: the inference budget, the Stop interrupt and the
  v2 trace were bypassed inside every meta-call built-in. `once/1`, `ignore/1`, `forall/2`,
  `bagof/3`, `setof/3`, `aggregate_all/3`, `setup_call_cleanup/3`, `with_output_to/2`,
  `maplist/2..5`, `foldl/4..6`, `include/3`, `exclude/3`, `partition/4`, `predsort/3` (46 classes
  implement `BuiltInWithContext`) ran their sub-goals on the **recursive legacy `QuerySolver`**,
  which polled neither the budget nor the interrupt. With `setInferenceBudget(20000)`,
  `once(Loop)`, `ignore(Loop)`, `aggregate_all(count, Loop, C)` and
  `setup_call_cleanup(true, Loop, true)` ran until killed at 60 s, and
  `forall(between(1,100000,_), true)` completed without charging a single step: untrusted code only
  had to wrap its loop in `once/1` to defeat the whole v3.4.0 hardening. Three layers of fix:
  1. **`ResourceGuard`** (new, `core.engine`) holds the query's step counter and budget. The v2
     machine publishes it on the shared `QuerySolver` for the duration of a query and
     `QuerySolver.solveInternal` charges every step to it, so **one** counter and **one** interrupt
     check now cover both engines. `Prolog` installs a guard for legacy-engine queries too, so the
     budget and Stop apply there as well (they were v2-only).
  2. **`ControlFlow.rethrowIfControl`** (new) guards all **122** broad
     `catch (Exception|RuntimeException|Throwable)` clauses in the built-in library and core.
     Several of them re-wrapped whatever they caught into a `PrologEvaluationException` — which
     *is* a `PrologException` — so `catch(aggregate_all(count, Loop, _), _, true)` could swallow
     the abort and keep running. The three control exceptions now pass through untouched.
  3. **Native + machine-backed meta-calls**: `once/1` ≡ `(G -> true)`, `ignore/1` ≡
     `(G -> true ; true)` and `forall/2` ≡ `\+ (C, \+ A)` are expanded natively in
     `MachineSolver` (which also gives them the right cut opacity), and the new
     `QuerySolver.solveMeta(Goal, Bindings, Solutions)` runs any other meta-call sub-goal on a
     **nested `MachineSolver` sharing the outer `ResourceGuard`**. 16 call sites in
     `AggregateAll`, `SetupCallCleanup`, `WithOutputTo`, `Once`, `Ignore`, `MapList`, `Include`,
     `Exclude`, `Partition`, `Foldl`, `PredSort` and `CollectionUtils` (the engine behind
     `findall/bagof/setof`) were switched over. Under `-Djprolog.engine=legacy` `solveMeta` falls
     back to the recursive solver unchanged.
  - **Measured** (budget 20 000): `once(loop(1000000))` never aborted → **50 ms**;
    `aggregate_all(count, …)` never aborted → **24 ms**; `setup_call_cleanup` → **13 ms**;
    `with_output_to` → **19 ms**; `bagof` `resource_error(stack_overflow)` → **21 ms**. A thread
    interrupt now reaches inside `once/1`, `forall/2` and `aggregate_all/3` in ~1.5 s (previously
    ignored). Without a budget: `once(loop(100000))` **1.78 s for only 5 000 iterations →
    438 ms for 100 000**; `maplist(integer, L)` over 200 000 elements OOM → **460 ms**, which also
    resolves **LIM-030**.
  - *Deviations from the report*: the report proposed native `aggregate_all/bagof/setof/
    setup_call_cleanup` plus a Prolog `prelude.pl` for the list/apply predicates. `solveMeta`
    reaches the same three goals (budget, cancellation, machine speed) while keeping every
    built-in's existing ISO semantics — notably the exact `aggregate_all` sum/extremum behaviour
    fixed by ISS-2025-0413/0414, which a rewrite on top of `sum_list`/`max_list` would have
    regressed. The prelude is not implemented: loading library clauses would require unregistering
    the corresponding built-ins, which changes legacy-engine behaviour too.
  - Not fixed: `maplist/3..5` with an *output* list is still quadratic in the list length
    (2.8 s for 40 000 elements) — that is the eager built-in's own list construction, not the
    sub-solve.
- **ISS-2025-0432** (ENG-12): `between/3` is a **lazy generator**. The eager
  `BuiltIn.execute(goal, bindings, solutions)` contract made it materialise every solution before
  the first could be used, so `between(1,2000000,X), X >= 2000000` exhausted a 256 MB heap, and
  `between(1, inf, X)` was silently capped at a million solutions. It now runs on the lazy
  choice-point kind introduced for `repeat/0` (ISS-2025-0423), producing one `Number` per redo.
  Every other mode and all the ISO error cases stay with the registry built-in.
  - **Measured**: `between(1,2000000,X), X >= 2000000` OOM at 256 MB → **368 ms in a 64 MB heap**;
    `between(1, inf, X), X > 10^7, !` now terminates (1.9 s).
  - *Deviation*: the report proposed a general `LazyBuiltIn` / `SolutionSink` interface for the
    registry. The lazy `Gen` choice point exists inside the machine and now backs `repeat/0`,
    `length/2` and `between/3`, but the public `BuiltIn` contract is unchanged — `member/2`,
    `nth0/nth1`, `select/3`, `append/3`, `clause/2`, `sub_atom/5` and `current_op/3` are still
    eager (they are no longer *quadratic*, though, thanks to ENG-11).

  Sentinel note: the lazy generator protocol uses an explicit `EXHAUSTED` marker rather than
  `null`, because `null` is a valid goal stack (the continuation of a query's last goal).

---

## [3.6.2] - 2026-08-25

### Engine deep analysis — wave 2: the deep-structure limit (ISS-2025-0428)

Second wave of `docs/reports/report-engine-deep-analysis-2026-08-24.md` (finding ENG-09).
Baseline: **961/961 JUnit tests, 20/20 example programs.**

- **ISS-2025-0428** (ENG-09): every term walker is now **tail-iterative on the last argument**.
  A list of N cells is N nested `'.'/2` terms whose *second* (last) argument is the tail, so a
  walker that recursed into all arguments needed N Java frames: with `-Xss4m`,
  `numlist(1,30000,L)` followed by `L == L`, `sum_list/2`, `copy_term/2`, `assertz/1`,
  `msort/2`, `term_to_atom/2` or `write/1` all raised `resource_error('stack_overflow')`, and a
  50 000-deep `f(f(…))` failed outright. Under the default JVM stack the cap was well below 20k.
  The rewrite loops on argument N and recurses only into arguments 1..N-1, so the Java depth equals
  the nesting of *non-last* arguments — small for lists and for most real data.

  Rewritten: `CompoundTerm.unify` / `isGround` / `copy` / `resolveBindings` / `equals` /
  `hashCode` / `toString` (+ list formatting), `MachineSolver.resolve` / `rename` / `unify` /
  `structuralEqual` / `checkBodyGoals`, `TermCopier.copyTermInternal`, `Variable.occursInTerm`
  (explicit work stack), `GroundCheck.isGroundTerm` (explicit work stack), and
  `TermFormatter`'s functional-notation fallback.

  Two secondary wins fell out of it: `CompoundTerm.unify` snapshotted the substitution map at
  **every** nesting level for rollback (O(depth × |bindings|) copying down a list) and now
  snapshots once for the whole spine; `CompoundTerm.toString`'s list case appended straight into
  the caller's `StringBuilder` instead of building a `List<String>` of every element and joining.

  `resolveBindings` and `MachineSolver.resolve` dereference *through* bound variables while
  walking the spine — a structure built by a recursive clause links its spine by variables, so
  stopping at the first `Variable` would have put the recursion right back in. `resolve`'s
  rational-tree detection (ISS-2025-0313) is preserved exactly: the spine scan pushes every
  variable it crosses into the active-path set and the bottom-up rebuild pops each link again, so
  a cycle reached through a *non-last* argument is still caught.

  **Verified** at the surefire fork's default JVM settings (no `-Xss`/`-Xmx`): a **1 000 000**
  element list round trip — `numlist/3`, `length/2`, `sum_list/2`, `==/2`, `copy_term/2`,
  `msort/2`, `sort/2`, `ground/1`, `assertz/1` + call, `reverse/2`, `append/3`, `term_to_atom/2`,
  `write/1`, `\+ \+ (L = L)` — plus `copy_term/2` of a 1 000 000-element **open** list and a
  200 000-deep `f(f(…))` built, copied, compared and written. Timings on the reference machine:
  1M `length/2` 1.9 s, `sum_list/2` 0.9 s, `==/2` 0.5 s, `copy_term/2` 0.9 s, `msort/2` 1.0 s,
  `assertz` + call 1.5 s, `write/1` 1.2 s.

  Behaviour note: `CompoundTerm.hashCode()` now produces different *values* (a structural,
  spine-iterative hash instead of `Objects.hash(functor, arguments)`). It remains consistent with
  `equals`; nothing persists or asserts hash values.

  Still bounded by memory rather than stack (out of ENG-09's scope, addressed by ENG-04/ENG-11/
  ENG-12 in later waves): `maplist/2..5` on a 1 000 000-element list exhausts the heap because it
  expands to one giant conjunction run by the legacy sub-solver (LIM-030), and `member/2` inside
  `findall/3` is quadratic because the eager built-in contract copies the binding map per element.

---

## [3.6.1] - 2026-08-24

### Engine deep analysis — wave 1: correctness quick wins (ISS-2025-0423..0427)

First wave of `docs/reports/report-engine-deep-analysis-2026-08-24.md` (findings ENG-01, ENG-02,
ENG-03, ENG-05, ENG-08). Baseline: **956/956 JUnit tests, 20/20 example programs.**

- **ISS-2025-0423** (ENG-01): `repeat/0` is an **infinite** choice point again. The registry
  built-in materialised exactly 1000 copies of the binding map, so the classic
  `repeat, …, Done, !` driver loop silently **failed** after 1000 iterations (and paid 1000 full
  map copies per `repeat`). Now handled natively in `MachineSolver` by a new lazy-generator choice
  point (`CP(Gen, …)`), O(1) memory per redo, four ports emitted, cut-prunable. Verified to
  100 000 iterations. The legacy engine (`-Djprolog.engine=legacy`) keeps the 1000 bound — see
  LIM-031.
- **ISS-2025-0424** (ENG-02): `new Number(double)` **never** auto-classifies an integral value as
  an ISO integer any more (ISO 9.1.3 / 7.1.2 forbid the implicit float → integer conversion).
  `sum_list([1.5,1.5], S)` now gives the float `3.0`, `sumlist([1.0], S)` gives `1.0`, JSON `1.0`
  and CSV `1.0` parse to floats, SQL DECIMAL/NUMERIC columns yield floats. All 58 call sites that
  bound to the double constructor were audited (found by making it private and compiling): the
  integer-intent ones (character codes, `sub_atom`/`sub_string` indices, `atom_length`,
  `string_length`, `succ/2`, operator precedences, `statistics/2` counters, FFI Integer/Long/
  Short/Byte, array lengths, the arity in `existence_error(procedure, Name/Arity)`) switched to
  the `long` constructor; the text→number sites (`atom_number/2` both directions, CSV fields, the
  legacy `PrologParser`) now go through the strict ISO token parser
  `AtomNumber.parseNumberToken`, which keeps `'3'` an integer and `'3.0'` a float. New explicit
  factories `Number.ofLong` / `Number.ofDouble` and the legacy-behaviour helper
  `Number.isIntegralDouble` (used only by `Rational`, whose whole values *are* exact integers).
- **ISS-2025-0425** (ENG-03): `length/2` **enumerates** in the (partial list, unbound length)
  mode: `length(L, N), N >= 3, !` and `length([a|T], N)` used to fail; they now enumerate
  `N = Prefix, Prefix+1, …` through the same lazy infinite choice point. The two deterministic
  modes still go to the Java built-in and are unchanged.
- **ISS-2025-0426** (ENG-05): `MachineSolver.bridgeBuiltin` no longer swallows every
  `RuntimeException`. `catch (RuntimeException e) { return -1; }` turned any Java fault inside a
  built-in (NPE, `ClassCastException`, `IndexOutOfBounds`) into "not a built-in" → `callUser` →
  `existence_error` or silent failure, and it would have swallowed `InferenceLimitException`,
  `QueryCancelledException` and `DebugStopException` raised in a nested sub-solve. Now: the three
  control exceptions are rethrown untouched (trust model preserved — they stay plain
  `RuntimeException`s that `catch/3` cannot trap); the new
  `core.engine.NeedsSolverContextException` is the only "not bridgeable" signal; anything else
  becomes a catchable `system_error(Class: Message)` naming the culprit predicate.
- **ISS-2025-0427** (ENG-08, minor inaccuracies): the `unknown=warning` message goes through
  `StreamManager.out()` instead of `System.err`, so the IDE console sees it; `throw/1` renames the
  ball **once** (it was renamed in the throw branch and again in `drive()`'s catch);
  `PrologException`'s error-term constructor no longer fills in a Java stack trace and computes
  its detail message lazily — exceptions are control flow in Prolog, and the eager
  `errorTerm.toString()` walked (and on a huge ball could overflow on) the entire term; the
  inference budget's unit (drive-loop steps, not logical inferences) is now documented on both
  `MachineSolver.setInferenceBudget` and `Prolog.setInferenceBudget`.

**Behaviour changes visible to users**
- Any arithmetic/aggregate result computed in floating point now *prints and types* as a float
  (`3.0`, `float(X)` true, `integer(X)` false) where it used to collapse to an integer. This is the
  ISO-correct behaviour but it is visible in output and in `integer/1` guards.
- `length(L, N)` with both arguments unbound no longer fails — it enumerates, so an unguarded
  `length(L, N)` is now a non-terminating generator (as in SWI-Prolog).
- A buggy built-in raises `error(system_error(...), Name/Arity)` where it previously produced
  `existence_error(procedure, Name/Arity)` or failed silently.
- One existing test asserted the old wrong behaviour and was corrected, not the engine:
  `AdvancedArithmeticTest.testPlusWithFloats` expected `plus(1.5, 2.5, X)` to give the integer `4`;
  it is the float `4.0`. Two test helpers that built op/3 precedences with `new Number((double) p)`
  were switched to the `long` constructor (test *input*, not an assertion).

---

## [3.6.0] - 2026-06-10

### Audit wave 3 — the remaining 27 confirmed findings fixed (ISS-2025-0396..0422)

Closes out the ISS-2025-0395 open-findings roll-up from the v3.5.0 audit (one broad
documentation-tail item remains). Baseline: **935/935 JUnit tests, 20/20 example programs.**

**Engine & meta-call**
- **ISS-2025-0396**: `retract/1` is re-executable on backtracking (ISO 8.9.3) on the default v2
  engine: each redo retracts the NEXT matching clause — `findall(X, retract(p(X)), L)` drains the
  predicate; resolves **LIM-026** (legacy engine keeps the documented eager gap).
- **ISS-2025-0397**: `phrase/3` with two free variables no longer raises a spurious
  `representation_error(cyclic_term)` (self-binding var-var union entries are now skipped; real
  rational-tree protection untouched).
- **ISS-2025-0398**: `V^Goal` is callable as an ordinary goal (= `call(Goal)`), native on v2 and
  via a new `Caret` built-in on legacy; bagof/setof and arithmetic `^` unaffected.

**Reader & format strictness**
- **ISS-2025-0408**: `read/1,2` and `read_term/2,3` read up to the ISO **end token** instead of
  one line: multi-line terms, several terms per line, leading `%`/`/* */` comments, quoted/escaped
  dots all handled; stream position preserved between calls; resolves **LIM-029**.
- **ISS-2025-0409**: `format/2,3` raises errors on argument mismatches: too-few args, `~d` with a
  non-integer (`type_error(integer,_)`), unknown directives; `[]` is the empty argument list.
- **ISS-2025-0410**: a non-callable DCG head (`123 --> [a]`) raises `type_error(callable, 123)` at
  load time instead of the misleading "Cannot redefine built-in predicate call/3".

**Text & strings**
- **ISS-2025-0399**: float text↔term conversion is type-faithful: `number_chars(X, "1.0")` gives
  the float `1.0` (not integer 1), `atom_number(A, 123.0)` gives `'123.0'`.
- **ISS-2025-0400**: `number_chars/2`, `number_codes/2` accept ISO `0x`/`0o`/`0b`/`0'c` notation
  and reject Java-only spellings (`Infinity`, `NaN`, `1f`) with `syntax_error`.
- **ISS-2025-0401/0406**: ISO error terms from `char_code/2`, `atom_length/2`, `atom_chars/2`,
  `atom_codes/2`, `atom_concat/3`, `number_*` (instantiation/type/domain/representation/syntax).
- **ISS-2025-0402**: `term_to_atom/2` works on non-ground terms.
- **ISS-2025-0403**: `string_to_atom(S, foo)` binds `S` to a string, not an atom.
- **ISS-2025-0404**: `string/1` type check implemented.
- **ISS-2025-0405**: SWI-style text interop: `atom_*` predicates accept strings, `string_*`
  predicates accept atoms.
- **ISS-2025-0407**: `float_integer_part/1`, `float_fractional_part/1` correct beyond ±2^63.

**All-solutions & ordering polish**
- **ISS-2025-0411/0412**: `setof/3` witness groups enumerate in the standard order of terms and
  variant witnesses merge into one group (ISO 8.10.2.1).
- **ISS-2025-0413/0414**: `aggregate_all(max/min)` fails on no solutions and type-errors on
  non-numerics; `aggregate_all(sum)` is BigInteger-exact with correct float contagion.
- **ISS-2025-0415/0416/0417/0418/0419/0420**: callable/type validation for `once/1`, `ignore/1`,
  `forall/2`, `findall/3`, `compare/3`, `sort/4` keys, `predsort/3` (also non-ground lists), and
  full ISO error clauses for `arg/3` and `=../2` (which now also work on non-ground terms).

**CLP(FD)**
- **ISS-2025-0421**: non-linear constraints no longer fail silently: `X*X #= 16` propagates
  (interval products, square case included); genuinely unsupported expressions raise a clear error.
- **ISS-2025-0422**: `labeling/2` honours its options (`leftmost`/`ff`/`ffc`/`min`/`max`,
  `up`/`down`); invalid options raise `domain_error(labeling_option, O)`; `label([a])` type-errors.

**Behavior changes**: stricter format/conversion errors replace silent leniency;
`atom_number(A, 123.0)` now yields `'123.0'`; `retract/1` backtracks (code relying on
single-shot retract semantics behaves differently).

---

## [3.5.0] - 2026-06-10

### ISO-conformance sweep — 53 confirmed defects fixed (ISS-2025-0342..0394)

Driven by a multi-agent empirical audit: 13 domain finders ran ISO-conformance queries against the
build (111 unique findings), every finding adversarially verified (98 confirmed, 13 rejected).
53 fixed below; the 30 still-open confirmed findings are rolled up as ISS-2025-0395 in
`docs/tracking/track-issues.md`. Baseline: **866/866 JUnit tests, 20/20 example programs.**

**Engine (v2 + legacy)**
- **ISS-2025-0342**: a cut inside the condition of `(->)/2`, `(*->)/2`, `\+/1`, `not/1` no longer
  destroys the construct's else/true branch (ISO 7.8.8/8.15.1) — silent wrong answers in the
  default engine.
- **ISS-2025-0343**: a `catch/3` frame is disarmed when its goal exits (and re-armed on
  backtracking into it) — later exceptions are no longer swallowed by stale frames.
- **ISS-2025-0344**: `KnowledgeBase.retract` removes exactly one clause and keeps all indexes in
  sync (duplicate clauses are no longer immortal); index misses now degrade to the full clause
  list, removing the ISS-2025-0340 silent-drop hazard.
- **ISS-2025-0345**: `Prolog.solve(Term)` now runs the v2 engine with the inference budget (was:
  silent legacy, no budget).
- **ISS-2025-0346**: `halt/0`, `halt/1` actually terminate the CLI with the exit code; `:- halt`
  aborts a consult; the IDE ends the session gracefully.
- **ISS-2025-0347**: calling an undefined procedure raises `existence_error(procedure, PI)` per
  the `unknown` flag (error/fail/warning), with dynamic-procedure tracking (`:- dynamic`
  directive — previously a no-op — plus assert/retractall implying dynamic).
- **ISS-2025-0363**: `throw(X)` with `X` unbound raises `instantiation_error` instead of throwing
  a fresh variable that any catcher traps.

**Term order, sorting, strings**
- **ISS-2025-0348**: `PrologString` identity and standard order fixed everywhere: `"abc" == "abc"`,
  `compare/3`, `@</2`-family, `atomic/1`; strings rank Var < Number < Atom < String < Compound,
  consistently with `sort/2`.
- **ISS-2025-0349**: `length/2`, `reverse/2`, `select/3`, `permutation/2` accept proper lists with
  unbound elements (deep groundness guards removed).
- **ISS-2025-0350**: `keysort/2` accepts non-ground pairs (its primary use case), is stable by key
  only, and raises ISO errors on non-lists/partial lists/non-pairs.
- **ISS-2025-0351**: `sort/2`, `sort/4`, `msort/2` raise `instantiation_error`/`type_error(list,_)`
  instead of failing silently.

**Arithmetic**
- **ISS-2025-0359/0360**: computed float overflow → `evaluation_error(float_overflow)`; NaN
  results → `evaluation_error(undefined)` (the `inf`/`nan` constants and propagation still work).
- **ISS-2025-0361**: huge `^`/`<<`/`>>` operands raise a catchable ISO error instead of a raw Java
  `ArithmeticException` that pierced `catch/3`.
- **ISS-2025-0362**: `0 ^ -1` → `evaluation_error(zero_divisor)`.
- **ISS-2025-0364**: `functor(T, f(a), 2)` → `type_error(atomic, f(a))`.
- **ISS-2025-0365**: `number_chars/2`, `number_codes/2`, `atom_number/2` round-trip arbitrary
  big integers exactly (no more silent 64-bit corruption).

**Clause database**
- **ISS-2025-0366**: `retract/1` validates its argument (no more raw `ClassCastException`).
- **ISS-2025-0367**: assert/retract/abolish/retractall on a built-in →
  `permission_error(modify, static_procedure, PI)`.
- **ISS-2025-0368**: assert validates clauses (unbound → `instantiation_error`; `1`, `(1:-true)`,
  `(foo:-7)` → `type_error(callable, _)`).
- **ISS-2025-0369**: `dynamic/1` is callable as a goal (PI, comma-sequences, lists).
- **ISS-2025-0370/0371/0372**: ISO errors for `clause/2` on built-ins
  (`permission_error(access, private_procedure, _)`), `retractall/1` and `current_predicate/1`
  on invalid arguments.

**All-solutions predicates**
- **ISS-2025-0382**: `bagof/3`/`setof/3` collect renamed-apart copies (result lists no longer
  alias caller variables).
- **ISS-2025-0383/0384**: `aggregate_all/3` propagates ISO error balls unchanged; bagof/setof/
  aggregate_all raise `instantiation_error`/`type_error(callable,_)` on bad goals.

**Lists**
- **ISS-2025-0379**: `append/3` no longer throws on open modes (`append([1],X,Z)` → `Z=[1|X]`).
- **ISS-2025-0380**: no more unsound success on partial lists (`last([a|T],X)`,
  `maplist(atom,[a,b|T])` close the tail instead of succeeding with it unconstrained).
- **ISS-2025-0381**: `maplist/2..5` is re-satisfiable (inner-goal alternatives no longer dropped)
  and derives the length from any proper list.
- **ISS-2025-0385**: `numlist/3` raises ISO errors on unbound/non-integer bounds.
- **ISS-2025-0386**: inverse modes: `reverse(X,[1,2,3])`, `select(2,L,[1,3])`,
  `permutation(P,[1,2])`.

**I/O & format**
- **ISS-2025-0352**: `format/2,3`, `write_term/2`, `read_term/2` succeed as goals (side effects
  used to happen with the goal then failing, killing every conjunction containing them).
- **ISS-2025-0353**: `format` accepts double-quoted (string) format strings — the spelling
  produced by the default `double_quotes=string`.
- **ISS-2025-0354**: `read_term/3` (Stream, Term, Options) implemented (regression vs the
  ISS-2025-0202 resolution claim).
- **ISS-2025-0373**: stream-argument output forms added: `write/2`, `writeln/2`, `nl/1`,
  `put_char/2`, `tab/2`, `write_term/3`.
- **ISS-2025-0374**: `format/3` honours its stream argument.
- **ISS-2025-0375**: `set_input/1`/`set_output/1` actually redirect (arity-1 input predicates
  honour the current input; output streams wrapped so `StreamManager.out()` honours them).
- **ISS-2025-0376**: `peek_char/2`, `peek_code/2`, `get_code/2` implemented.
- **ISS-2025-0377**: `open/3,4`/`close/1` raise ISO `error/2` terms
  (`existence_error(source_sink,_)`, `domain_error(stream_or_alias,_)`, ...) instead of
  plain-atom balls.
- **ISS-2025-0378**: `print/1`, `print/2` implemented.

**Writer & DCG**
- **ISS-2025-0387**: `writeq` no longer emits token-merging operator sequences (`writeq(-(1))`
  prints `- 1`, which re-reads as the same compound — it used to print `-1`, a number).
- **ISS-2025-0388**: `writeq` quotes `','`, `'.'` and comment-opening symbolic atoms (`'/*'`).
- **ISS-2025-0389**: `write/1`, `writeln/1`, `writeq/1`, `~w`/`~q` imply `numbervars(true)`:
  `'$VAR'(0)` prints as `A` (ISO 8.14.2).
- **ISS-2025-0390**: floats print with lowercase exponent (`1.0e10`) and `inf`/`-inf`/`nan`
  spellings — output re-reads as the same term (Java's `1.0E10`/`Infinity`/`NaN` did not).
- **ISS-2025-0391**: `phrase/2,3` applies full DCG body translation to its first argument:
  `(A,B)`, `(A;B)`, `(A->B)`, `\+A`, `!`, `{G}`, `[a,b]`, `[]` all work as bodies.
- **ISS-2025-0392/0393**: DCG push-back and terminal lists validated (variable push-back and
  `[a|Var]` terminals raise load errors instead of silently corrupting the grammar; string
  push-back becomes its code list).
- **ISS-2025-0394**: `phrase/2,3` raises `type_error(list,_)`/`type_error(callable,_)` instead of
  failing silently.

**CLP(FD) v2**
- **ISS-2025-0355**: unification respects domains — `X in 1..3, X = 5` now fails (soundness).
- **ISS-2025-0356**: constraint posts are undone on backtracking (disjunction no longer loses
  solutions; failed branches no longer poison the store).
- **ISS-2025-0357**: singleton domains bind the variable (`X #= 2` gives `X = 2`).
- **ISS-2025-0358**: multi-variable `#\=` expressions work (`X #\= Y + 1`); `X #\= X` fails.

**Behavior changes embedders may notice**: undefined procedures now throw
`existence_error` by default on both engines (set the `unknown` flag to `fail` for the old
behavior); `halt/1` exits the CLI JVM; sort/keysort/msort raise errors instead of failing on
invalid inputs; floats and `'$VAR'(N)` print differently (ISO-correct).

New limitations recorded: LIM-026..LIM-030 (`docs/tracking/track-limitations.md`). Open audit
findings rolled up as ISS-2025-0395.

---

## [3.4.0] - 2026-06-09

### Production hardening (sandbox, resource budget, robustness, correctness)

Driven by a multi-agent production-readiness audit
(`docs/reports/report-production-readiness-audit-2026-06-09.md`). Baseline: **705/705 JUnit tests,
20/20 example programs.**

**Security / robustness**
- **ISS-2025-0338 — sandbox / safe mode**: `Prolog.enableSafeMode()` removes every host-touching
  built-in (OS shell, Java FFI/reflection, filesystem, network, HTTP, JDBC, persistence — deny by
  package), so an untrusted program cannot run processes, reflect into the JVM, or touch
  files/sockets/databases. Core logic and arithmetic remain available.
- **ISS-2025-0339 — inference budget**: `Prolog.setInferenceBudget(steps)` aborts a runaway query
  with an **uncatchable** `InferenceLimitException` (not a `PrologException`, so an untrusted `catch/3`
  cannot trap it and loop). Bounds CPU.
- **ISS-2025-0341 — no crash on deep structures**: a deep TERM (during resolve) and deeply nested
  untrusted INPUT (during parse) now raise a catchable `resource_error(...)` instead of escaping as a
  raw `StackOverflowError`.

**Correctness**
- **ISS-2025-0335**: `sort/2`, `msort/2`, `sort/4` no longer fail on lists containing unbound variables
  — they require a *proper* list (not a *ground* one) and sort by standard order (variables lowest).
- **ISS-2025-0336**: `freeze/2` now propagates the bindings its woken goal makes (the goal runs on the
  v2 machine's own binding/trail, not the legacy hook) — `freeze(X,Y=hello), X=1, Y==hello` succeeds.
- **ISS-2025-0337**: a non-callable goal raises `type_error(callable, _)` (or `instantiation_error` for
  a variable); under-instantiated `=../2` raises `instantiation_error` instead of a raw message.

**Attempted & reverted**
- **ISS-2025-0340 — first-argument indexing** in the v2 engine: reverted because
  `KnowledgeBase.getRulesWithFirstArgIndex` returns empty for predicates whose first-arg index was
  never populated, silently dropping their clauses. The KB index must be made reliable first.

All findings captured as executable tests in `test/audit/ProductionAuditTest.java`.

---

## [3.3.0] - 2026-06-09

### Call tracing & full debugging on the v2 engine

- **`trace/0` / `notrace/0` now actually trace** (ISS-2025-0329): the default v2 engine emits a
  four-port trace (Call / Exit / Fail / Redo, depth-indented) to the shared output, so it shows up in
  **both** the CLI and the IDE Run console. CLI gains a `:trace [on|off]` command; the IDE Run panel
  gains a **Trace** toggle. Output built-ins now write through a thread-local stream (no global
  `System.setOut`).
- **The IDE debugger runs on the default v2 engine** (ISS-2025-0331): `core.engine.v2.MachineSolver`
  fires the four-port `DebugController` events (it picks up the debugger from the shared `QuerySolver`),
  so breakpoints / stepping / Stop work on the same engine as normal execution. Zero overhead when not
  debugging.
- **Built-ins are debuggable** (ISS-2025-0332): while debugging, the engine routes `=`, `is`,
  comparisons, type-checks and the 200+ registry built-ins through the instrumented bridge, so they
  fire Call/Exit/Fail/Redo and can be stepped/paused.
- **Conditional & hit-count breakpoints** (ISS-2025-0333): a breakpoint can carry a Prolog **condition
  goal** (pauses only when it succeeds) and an **ignore count** (skip the first N hits). The IDE
  Add-Breakpoint dialog exposes both; conditions are evaluated by a clean, detached sub-solve.
- **Legacy debugger fix** (ISS-2025-0330): the last-call-optimisation trampoline bypassed the debug
  ports, so the final (often recursive) body goal was never traced — disabled while debugging.
- **Repository fix** (ISS-2025-0334): the `.gitignore` patterns `Debug*.java` / `Test*.java` / `*.sh`
  were unanchored and had been **excluding core source files** (`DebugController`, `DebugEvent`,
  `DebugStackEntry`, `Debugging`) from the published repository. Anchored to the project root and the
  missing sources committed.

Baseline: **690/690 JUnit tests, 20/20 example programs.**

---

## [3.2.0] - 2026-06-09

### Major IDE upgrade — editor, execution, compilation & debugging

A full UX pass on the Swing IDE (guided by a multi-agent audit — see
`docs/reports/report-ide-ux-analysis-2026-06-09.md`). All 4 critical issues, 14/14 HIGH and 15/21
MEDIUM resolved. Baseline: **687/687 JUnit tests, 20/20 example programs.**

**Editor**
- **Undo/redo** (`UndoManager`, Ctrl+Z / Ctrl+Y / Ctrl+Shift+Z, Edit-menu items) — previously absent.
- **Source formatter** `core.write.v2.PrologFormatter` (one goal per body line, blank line between
  clauses, comments preserved) — Ctrl+Alt+L. (8 unit tests.)
- **Code completion** (Ctrl+Space) over predicates / built-ins / clause variables.
- **Responsive syntax highlighting** (debounced — no longer re-styles the whole document per keystroke).
- **Bracket matching + auto-close**, **UTF-8 save**, **backward/highlight-all find**, and inline
  **wavy-underline error squiggles + gutter markers + per-line tooltips** (cleared on edit).

**Execution**
- **Working Stop**: the v2 engine polls the interrupt flag and raises `QueryCancelledException`
  (uncatchable by `catch/3`) — infinite queries now cancel.
- **Lazy streaming + cap** via new `Prolog.solveStream(query, sink)`; a results **table view**
  (column per variable, CSV export) and a live **status/progress** (elapsed + solution counter).
- **Thread-local output capture** (`StreamManager.out()` + `setThreadLocalOutput`) — output built-ins
  no longer require a process-wide `System.setOut`.

**Compilation**
- Clickable **Problems view** (jump-to-source), **Compile to .jpc** action (Shift+F9).

**Debugging**
- **Line-accurate breakpoints**: `Rule` carries its source line (set at consult);
  `Prolog.getPredicateIndicatorAtLine(line)` maps a gutter click to the real clause (no more regex).
  Breakpoints **persist** (sidecar `.bps`). Real **stepping shortcuts** (F7 / F8 / Shift+F8 / F9).
- Fixed a v2-default **regression**: debugging now forces the legacy engine (which carries the
  four-port hooks) via new `Prolog.solveLegacy(query)`.
- **Run to Cursor**, **Restart**, an expandable **Variables tree** (structure-aware), and
  **Watch expressions**.

**Other UX**: settings/window/divider persistence, session restore + Recent Projects, Go to Line
(Ctrl+G), Quick Open (Ctrl+P), context-sensitive toolbar.

New issues: ISS-2025-0320 … 0328.

---

## [3.1.0] - 2026-06-09

### Clean-room v2 resolution engine is now the DEFAULT (ISS-2025-0313 … 0319)

`core.engine.v2.MachineSolver` becomes the default query-resolution engine (fall back to the legacy
recursive solver with `-Djprolog.engine=legacy`). Closing the last engine gaps brought it to **full
parity — 675/675 JUnit tests and 20/20 example programs**:

- **ISS-0313** cyclic-term-safe `resolve` — rational trees (`X=f(X)`, occurs_check off) raise
  `representation_error(cyclic_term)` instead of `StackOverflowError`.
- **ISS-0314** module integration — `Module:Goal` resolves the named module **with export enforcement**;
  unqualified lookup is module-aware only when user modules exist (plain programs keep flat-KB semantics).
- **ISS-0315** profiler — the engine feeds `Profiler.recordCall`, so `profile`/`profile_data` work.
- **ISS-0316** backtrackable globals — each choice point snapshots and rolls back the legacy `Trail`,
  so `b_setval`/`op/3`/`setarg` undo actions are honored under v2.
- **ISS-0317** destructive `setarg/3` — built-ins receive the unresolved goal + bindings, so they mutate
  the actual bound term rather than a copy.
- **ISS-0318** coroutining — binding an attributed variable invokes the attribute-unify hook, firing
  `freeze`/`when`/`dif` goals; attributed-session variables persist across queries.
- **ISS-0319** tabling — tabled predicates (`:- table`) delegate to the legacy SLG solver (loop
  detection + memoization), surfacing solutions as a choice point.

The engine also benefits from the earlier ISS-0307 … 0312 work (iterative SLD with no `StackOverflowError`
on deep recursion, mutable bindings + trail, lazy enumeration, built-in bridge, soft-cut, IEEE
comparison, occurs-check). The legacy engine remains available and passes 675/675 as a fallback.

---

## [3.0.0] - 2026-06-08

### Implementation Audit Fixes (ISS-2025-0245 … 0252)

Fixes from a multi-agent correctness/ISO audit of the engine and built-ins.

#### Correctness
- **`append/3` (ISS-2025-0245)** — concatenation mode now selected by proper-list
  (closed-spine) structure instead of deep groundness. `append([a],[X],R)` now
  succeeds with `R=[a,X]` instead of throwing `unsupported mode`. List elements
  may be unbound variables.
- **`retract/1` (ISS-2025-0251)** — `retract((Head :- Body))` now matches stored
  rules (previously only the head was unified, so the clause form never matched).
  Bare-head retract of facts is unchanged.
- **`set_prolog_flag(occurs_check, …)` (ISS-2025-0246)** — the flag is now wired
  into unification (previously stored but ignored).

#### ISO conformance
- **`(**)/2` (ISS-2025-0247)** — now the ISO floating-point power: always returns
  a float (`2 ** 3 =:= 8.0`). Integer power remains `(^)/2` (`2 ^ 3 =:= 8`).
- **Integer-only operators (ISS-2025-0249)** — `mod`, `rem`, `//`, `div`, bitwise
  and shift operators now raise `type_error(integer, _)` on a float argument.
- **Arithmetic error terms (ISS-2025-0248)** — `is/2` and the arithmetic comparison
  predicates now raise proper ISO error terms — `error(instantiation_error, _)` for
  an unbound variable and `error(type_error(evaluable, _), _)` for an unknown/
  non-evaluable functor — instead of bare-atom messages.
- **Rounding overflow (ISS-2025-0250)** — `truncate/round/floor/ceiling/integer`
  promote to `BigInteger` beyond long range instead of saturating to `Long.MAX_VALUE`.

#### CLP(FD)
- **Constraint store leak (ISS-2025-0252)** — the global `ConstraintStore` is reset
  at the start of each top-level query so domains/constraints no longer leak between
  queries. (A per-engine store keyed by variable identity remains a tracked follow-up.)
- **ADD/SUB bound overflow (ISS-2025-0262)** — arithmetic bounds inference computes
  ADD/SUB combinations in `long` and clamps to int range (like MUL), avoiding silent
  int overflow.
- **Huge domains (ISS-2025-0263)** — `X in Lo..Hi` beyond 10M values raises
  `resource_error(clpfd_domain_too_large)` instead of OOM / infinite loop.
- **`indomain/1` (ISS-2025-0264)** — now propagates each candidate and skips values
  that violate posted constraints (single-goal local consistency); previously it
  emitted every domain value blindly.

#### DCG
- **`phrase/2,3` multi-solution (ISS-2025-0253)** — now enumerate all parses on
  backtracking instead of behaving like `once(phrase(...))`.
- **DCG cut (ISS-2025-0254)** — `!` in a DCG body now threads the difference list
  (`(!, S0=S)`) instead of being emitted as the non-terminal `!/2`.
- **`call_dcg/3` (ISS-2025-0255)** — now actually expands and runs the DCG body
  (was a stub that unified Input with Output).

#### Parser
- **Negative radix/char-code literals (ISS-2025-0256)** — `-0xFF`, `-0o17`,
  `-0b1010`, `-0'a` now keep the sign (previously parsed as the positive magnitude).

#### Lists / strings
- **Set operations (ISS-2025-0266)** — `subtract/3`, `intersection/3`, `union/3` now
  distinguish the atom `'1'` from the number `1` (was a `toString()` comparison).
- **`split_string/4` (ISS-2025-0267)** — keeps empty substrings and emits the final
  (possibly empty) field, per SWI; `split_string("a,,b", ",", "", X)` → `["a","","b"]`.
- **`atomic_list_concat` (ISS-2025-0268)** — accepts numbers in the list
  (`atomic_list_concat([a,1,b], R)` → `R='a1b'`).

#### Arithmetic / database
- **type_error(evaluable, _) culprit (ISS-2025-0269)** — now the ISO compound
  `'/'(Name, Arity)` instead of an atom.
- **`clause/2` (ISS-2025-0270)** — raises `instantiation_error`/`type_error(callable,_)`
  for a bad Head, and uses the predicate index instead of scanning the whole database.
- **`min/2`, `max/2` (ISS-2025-0271)** — preserve the selected operand's type
  (`min(2, 3.0) = 2`, not `2.0`).
- **`gcd/2` (ISS-2025-0272)** — a float operand raises `type_error(integer, _)`.

#### New built-ins
- **`setup_call_cleanup/3`, `call_cleanup/2` (ISS-2025-0273)** — run a cleanup goal
  exactly once when the main goal finishes (success / failure / exception).

#### Clean-room v2 parser — now the DEFAULT (ISS-2025-0290..0293)
- A new ISO parser (`core.parser.v2`: single-pass `Lexer` + operator-precedence
  `TermReader`) replaces the legacy dual-path parser as the default for `consult` and
  queries. It fixes the whole class of parser bugs: canonical functor (`-(1,2)` is `-/2`),
  operator-as-atom (`X = -`, `foo(-, +)`), postfix operators, `0'c`/radix/negative
  literals, `''`/`""` doubled-quote escapes, and quote-aware clause splitting.
- Validated: **675/675 JUnit + 20/20 examples** with v2 driving all parsing, and it parses
  **123/130 example programs vs the legacy parser's 117** (strictly better; the 2 it rejects
  use non-ISO constructs that even SWI rejects). Fall back with `-Djprolog.parser=legacy`.

#### Clean-room v2 CLP(FD) — now the DEFAULT (ISS-2025-0291)
- `builtin.clpfd.v2`: interval-set domains (no OOM), per-query identity-keyed store with
  a propagation queue + trail, constraints (`Cmp` with real `#\=`, `Sum`, `Mul`, `Abs`,
  `AllDifferent`, N-ary `Linear`, `Reified`, `Mod`), and a sound first-fail labeler. Default
  via `-Djprolog.clpfd` (legacy fallback `=legacy`).

#### Clean-room v2 DCG translator — now the DEFAULT (ISS-2025-0304)
- `core.dcg.v2.DCGTranslator`: a single recursive-pass `Head --> Body` ISO translator handling
  head push-back, `|` alternatives, `\+`, `call//N`, `{}`, `!`, `->`, terminal lists/strings.
  Default (legacy transformer via `-Djprolog.dcg=legacy`). Resolves the former ~85% DCG
  limitation (LIM-021).

#### New standalone clean-room modules
- `core.write.v2.TermWriter` — operator-aware term output (`1..3`, `a+b*c`, `[a,b|T]`),
  `writeq`/`write_canonical`.
- `core.arith.v2.ArithEvaluator` — single-path arithmetic evaluator (BigInteger/double, ISO
  error terms), IEEE-754 comparison semantics.

#### New v2 resolution engine — now the DEFAULT (ISS-2025-0307..0319)
- `core.engine.v2.MachineSolver`: a clean-room iterative SLD machine (explicit goal/choice-point
  stacks — 200,000-deep recursion returns with **no `StackOverflowError`**), mutable bindings +
  trail (O(changes) backtracking), lazy enumeration, cut / if-then-else / soft-cut (`*->`) / `\+`
  / `call/N`, native `findall`/`catch`/`throw`, `assert`/`retract`, a built-in bridge reusing the
  existing 200+ built-ins, module-qualified calls with export enforcement, occurs-check, IEEE
  arithmetic comparison, cyclic-term-safe resolution, backtrackable globals (`b_setval`), the
  profiler, coroutining (`freeze`/`when`/`dif` via the attribute-unify hook), destructive `setarg/3`,
  and tabling (delegated to the legacy SLG solver). **Passes the full suite (675/675 JUnit + 20/20
  example programs).** Default for query resolution; fall back with `-Djprolog.engine=legacy`.

#### New operators
- **`div`, `rdiv` (400 yfx)** — added to the default `OperatorTable` (ISS-2025-0300).

#### Limitations resolved
- LIM-017 / LIM-019 (parser internals — v2 parser), LIM-018 (negative radix / char-code literals),
  LIM-020 (int/float distinct terms), LIM-021 (DCG completeness — v2 translator), LIM-022 (CLP(FD)
  soundness — v2 solver), LIM-025 (`open/4` dangling alias).

#### ISO conformance & robustness (from a 2nd re-triage of the audit)
- **`=:=` / `=\\=` (ISS-2025-0274)** — IEEE semantics: `-0.0 =:= 0.0` succeeds, `nan =:= nan` fails.
- **`throw/1` (ISS-2025-0275)** — throws a `copy_term` of the ball (independent of context).
- **`upcase_atom`/`downcase_atom` (ISS-2025-0276)** — locale-independent (`Locale.ROOT`).
- **`atom_length/2` (ISS-2025-0277)** — ISO `instantiation_error` / `type_error(atom,_)`.
- **`op/3` (ISS-2025-0278)** — a non-integer precedence raises `type_error(integer,_)`.
- **`initialization/1` (ISS-2025-0279)** — directive now runs (after the file is loaded).

#### Concurrency, parsing & misc (re-triage batch 2)
- **Thread-safety (ISS-2025-0280, 0281)** — `KnowledgeBase.getCurrentPredicates` synchronized;
  `DebugController` breakpoint collections use concurrent collections.
- **Dead code (ISS-2025-0282)** — removed the shadowed `,`/2 `Conjunction` built-in.
- **`op/3` list of names (ISS-2025-0283)** — `op(700, xfx, [eq, neq])`.
- **`number_string/2` (ISS-2025-0284)** — integer strings parsed as exact `BigInteger`.
- **Parser/IO (ISS-2025-0285, 0286, 0287)** — trailing-newline line count; CLI reads UTF-8;
  `StreamManager.closeStream` evicts cached readers/properties.
- **Directives (ISS-2025-0288)** — failed/erroring directives surfaced on stderr.
- **Perf (ISS-2025-0289)** — hoisted loop-invariant `extractVariables` out of the clause loop.

#### Standard order of terms (ISO)
- **Integers and floats are distinct terms (ISS-2025-0261)** — `1 \= 1.0`,
  `1 \== 1.0`, and a float sorts before a numerically-equal integer
  (`compare(O,1,1.0)` → `O = (>)`), so `sort/2` no longer dedups `1` and `1.0`.
  Fixed `Number.unify/equals/hashCode`, `Sort.compareTerms`,
  `StandardTermOrdering`, and the `.jpc` format (v0x02 now preserves int/float
  type and BigInteger precision; older `.jpc` files are recompiled).

#### Resource handling
- **HTTP (ISS-2025-0257)** — `http_request/4`/`http_post/4` disconnect in `finally`.
- **Streams (ISS-2025-0258)** — `StreamManager` uses `ConcurrentHashMap` for its
  cross-thread stream bookkeeping.
- **JDBC (ISS-2025-0259, 0260, 0265)** — `closeResultSet` no longer closes the user's
  managed prepared/callable statement; `executeQuery` closes its ad-hoc statement on
  error; `jdbc_tables/2`/`jdbc_columns/3` and `jdbc_call_get_resultset/2` use
  try-with-resources.

### Test Coverage
- **675/675 JUnit tests pass, 0 skipped**
- **20/20 examples regression pass**
- ISS-2025-0265 (JDBC ResultSet leak) is verified by inspection — the leak-on-exception
  path needs a live database to exercise.

### Notes
- A full audit report (101 confirmed findings, prioritized) is in
  `docs/reports/report-implementation-audit-2026-06-07.md`. Parser hardening, DCG
  completeness, and CLP(FD) soundness are addressed in this release by the clean-room
  v2 rewrites (now default). Genuinely deferred items: last-call optimization, threading
  isolation, and the 6 remaining gaps of the opt-in v2 resolution engine.
- **Audit correction:** the report's "no first-argument indexing" finding is stale —
  first-argument indexing is implemented (`KnowledgeBase.getRulesWithFirstArgIndex`)
  and used by `QuerySolver`; verified ~1 ms lookup over 1000 facts.

---

## [2.9.7] - 2026-05-21

### SWI Library Utilities

- **`pairs_keys/2`** — `pairs_keys([a-1, b-2], [a, b])`
- **`pairs_values/2`** — `pairs_values([a-1, b-2], [1, 2])`
- **`pairs_keys_values/3`** — bidirectional: pairs ↔ keys + values lists
- **`must_be/2`** — type-checking helper with ISO-compliant errors. Supported types: `atom`, `atomic`, `integer`, `float`, `number`, `compound`, `callable`, `var`, `nonvar`, `ground`, `list`, `boolean`, `positive_integer`, `nonneg`

### Test Coverage
- **517/517 JUnit tests pass, 0 skipped** (+5 new)
- **20/20 examples regression pass**

---

## [2.9.6] - 2026-05-21

### CR-2025-0009 Debug + Profile Completion

The four-port debug model was already implemented (v2.6+) but several acceptance criteria remained unchecked. This release closes them.

**New debug builtins**:
- **`debugging/0`** — reports current debug state (trace on/off, list of active spy points). Always succeeds.
- **`spying/1`** — non-deterministic enumeration of active spy points as `Name/Arity`.

**New profiler infrastructure**:
- `core/engine/Profiler.java` — thread-safe per-predicate call counter with `ConcurrentHashMap<String, AtomicLong>`. Zero overhead when disabled.
- **`profile/0`** — enable profiling
- **`noprofile/0`** — disable profiling
- **`profile_data/1`** — unify with sorted list of `Name/Arity-Count` pairs (descending by count)
- **`reset_profile/0`** — clear counters
- `QuerySolver.solveAgainstKnowledgeBase` records call when `Profiler.isEnabled()`

**CR-2025-0009 acceptance criteria** (all checked):
- Four-port model (CALL/EXIT/REDO/FAIL) ✓
- `debugging/0` ✓
- Trace output formatted (DebugPanel colored) ✓
- Stack trace visualization (IDE DebugPanel) ✓
- Performance profiling basic ✓
- Configurable debug detail (trace/notrace + spy/nospy + profile/noprofile) ✓
- Integration with spy/nospy via `spying/1` ✓

### Test Coverage
- **512/512 JUnit tests pass, 0 skipped** (+3 new CR-009 tests)
- **20/20 examples regression pass**

### Documentation
- CR-2025-0005 (`seek/4`) and CR-2025-0009 (debug ports) checkboxes updated in `track-change-requests.md`

---

## [2.9.5] - 2026-05-20

### Cleanup + final deferred items

- **`seek/4`** (CR-2025-0005, previously deferred): SWI-Prolog stream repositioning with `bof | current | eof` method. Uses `FileChannel.position`. New `Seek.java` builtin.
- **Stale comments removed**:
  - `ReadTerm.java` — "KNOWN LIMITATION: stream parameter ignored" (false since v2.8.3 ISS-0202)
  - `GlobalVariables.java` — "KNOWN LIMITATION: b_setval non-backtrackable" (false since v2.9.0 R1 Trail engine)
- **Test cleanup**: removed leftover `// @Ignore enabled` stub comments in `RefactorIssuesTest.java`

### Test Coverage
- **509/509 JUnit tests pass, 0 skipped**
- **20/20 examples regression pass**

---

## [2.9.4] - 2026-05-20

### Final Limitations Resolved

**Cross-solve attributed-variable identity** (the last `@Ignore`'d test):

Root cause discovered: `TermParser.parseVariable` was creating a NEW `Variable` instance for each textual occurrence of a variable name (even within the same parse). So `when(ground(f(X, Y)), Goal), X = 1` parsed two different X objects — when() attached attributes to one, but `X = 1` bound the OTHER (no attributes, no hook fire).

Three coordinated fixes:
1. **Parser variable interning** (`TermParser.java`): per-parse `Map<String, Variable>` so all occurrences of `X` in one parse share the same instance. Anonymous `_` is intentionally NOT interned.
2. **When.java re-suspend stores RESOLVED condition**: when a partial binding fires the hook but the condition is still not satisfied, the re-suspended attribute carries the condition with already-bound variables substituted. So `when(ground(f(X,Y)), G), X=1` re-suspends as `when(ground(f(1,Y)), G)` on Y.
3. **Session-scoped attributed variables** (`Prolog.java`): cross-`solve()` survival via `attributedSessionVars: Map<String, Variable>`. When a query var still has pending attribute goals after solve completes, it's saved by name. Next `solve()`'s parser output is spliced: variables matching session names are replaced with the surviving instances. Var without attributes are NOT persisted (so unrelated queries stay independent).

Now `prolog.solve("when(ground(f(X,Y)), assertz(p)), X = 1")` followed by `prolog.solve("Y = 2")` correctly fires the suspended goal — `p` is asserted.

### Test Coverage
- `testCoroutining_whenReSuspends` re-enabled and passing
- Fixed test logic in `testCoroutining_freezeFiresOnUnify` (anonymous fact `probe(_)` was matching anything; use distinct atom marker)
- **509/509 JUnit tests pass, 0 skipped**
- **20/20 examples regression pass**

### Acknowledged design considerations (no fix needed)
- **R6 solver dispatch unification**: organizational; no behavior delta.
- **R7 doc split**: organizational; current TOC sufficient.
- **KnowledgeBase concurrent index race**: KB methods are all `synchronized (this)`. Single-threaded design (per CLAUDE.md). No concrete concurrent-mutation use case to motivate ReadWriteLock refactor.

---

## [2.9.3] - 2026-05-20

### Deferred Items Resolved

- **`..` tokenizer**: parser now recognizes `..` as single multi-char token. Enables full CLP(FD) syntax like `X in 1..5`.
- **`format/2` `~p` portray hook**: now properly captures output via both `System.out` redirect AND `StreamManager.user_output` swap, so `format/2`'s internal writes during portray are captured. Test re-enabled.
- **Module-qualified call with empty export list**: `secret:hidden(_)` properly fails when `hidden/1` not in module's export list. Test re-enabled.

### Test Coverage
- Previously @Ignore'd `testR2_emptyExportListHidesAll` and `testR4_portrayHook` re-enabled and passing
- **509/509 JUnit tests pass** (only 1 skipped: cross-`solve()` variable identity — fundamental programmatic API limitation; SWI REPL maintains via var-name map but JProlog programmatic `solve()` spawns fresh state each call)
- **20/20 examples regression pass**

### Acknowledged design limitations (documented, not bugs)
- **R6 solver dispatch unification**: organizational refactor; no new behavior. Skipped (no test to satisfy).
- **R7 doc split**: organizational; current single-file reference (6800 lines) navigable via TOC.
- **Cross-`solve()` variable identity**: programmatic API limitation; each `solve()` parses fresh terms. SWI REPL-style identity tracking would require session-level var-name map (scope expansion).
- **KnowledgeBase concurrent index race**: JProlog is single-threaded by design (per CLAUDE.md). Concurrent access not actively supported.

---

## [2.9.2] - 2026-05-20

### Round 5 Minor Fixes + Cleanup

- **.jpc source hash**: MD5 → SHA-256 (collision-resistant)
- **.jpc signed varint**: new `writeSignedVarint`/`readSignedVarint` (zigzag encoding) — future-proof for signed integer fields
- **AtomTable.gc()**: now atomic via `compute()` — no race with concurrent `intern()`
- **ThreadLocal cleanup**: `Variable.setAttributeUnifyHook(null)` calls `remove()`; `Trail.clear()` calls `remove()` instead of `get().clear()` — releases ThreadLocal references properly
- **CHR dead code removed**: `CHRStore.java` deleted (was never integrated; LIM-010 was misclassified as resolved)
- **`:- use_module(library(Name))` accepted**: SWI-compatible directive form. Known libraries (clpfd, lists, between, apply, assoc, format) treated as no-op (built-ins already registered). Other module names imported normally.
- **CLP(FD) operators registered**: `in/2`, `ins/2`, `#=/2`, `#\\=/2`, `#</2`, `#>/2`, `#=</2`, `#>=/2`, `../2` declared at standard SWI precedences. Note: `..` tokenization in expressions still requires parser improvement (full CLP(FD) usage tracked separately).

### Test Coverage
- 3 new tests in `AuditRound5Test.java` (use_module, zigzag varint, ...)
- **509/509 JUnit tests pass** (3 skipped: deferred behaviors)
- **20/20 examples regression pass**

---

## [2.9.1] - 2026-05-20

### Round 5 Audit Fixes

**Critical**:
- **AtomTable interning GC race**: `intern()` could return null when GC fired between `compute()` and `WeakReference.get()`. Now holds strong reference inside compute scope and returns it directly. (`AtomTable.java:53`)
- **JpcWriter cyclic terms → StackOverflow**: serialization had no cycle detection. Added `IdentityHashMap` visited set in both `collectStrings` and `writeTerm`. Cyclic terms now throw `IOException("Cannot serialize cyclic term")` instead of SOE. (`JpcWriter.java`)

**Major**:
- **Exception terms structured per ISO §7.12**: `between/3` and `functor/3`+`=../2` now throw `PrologException(ISOErrorTerms.typeError(...))` (proper `error(type_error(Type, Culprit), Context)` term) instead of `PrologEvaluationException` with raw string. `catch/3` can now match these.
- **Module-qualified call enforces export visibility**: `solveInModuleContext` checks the predicate's `isExported()` signature against the called module when caller is in a different module. `secret_module:private_pred(_)` now fails (or existence_error) when `private_pred/1` not in module's export list.
- **Trail cleanup on exception**: `QuerySolver.solve()` top-level now calls `Trail.clear()` in `finally`. Prevents stale trail entries from leaking across solve() calls when an exception is thrown mid-query.

### Verified working
- `b_setval/2` backtrackable (R1) — still passes
- `op/3` redefinition undo (R1) — still passes
- `setarg/3` (R1) — still passes
- Module-local operators (R2) — still passes

### Test Coverage
- New `test/audit/AuditRound5Test.java` — 8 tests, all passing
- **506/506 JUnit tests pass** (3 skipped: deferred behaviors)
- **20/20 examples regression pass**

---

## [2.9.0] - 2026-05-20

### Major Refactors R1-R8

Six structural refactors landed (R6/R7 are organizational, no behavior change).

### R1 — Trail engine
New `core/engine/Trail.java` — stack of `Runnable` undo actions per thread. Solver-integrated rollback in `IfThenElse.executeDisjunction`.
- **`b_setval/2`** now backtrackable: failed branch restores previous value
- **`op/3`** redefinitions undone on backtrack
- **`setarg/3`** destructive arg update with trail-based undo. `CompoundTerm.setArgument(int, Term)` API.

### R2 — Module-local operators
- `OperatorDefinition` now tags every op with its defining module
- `current_op/3` filters by current module + global ("user") visibility
- `:- module(m, ...)` directive publishes current module context to op layer

### R3 — Stream encoding / EOF action / binary
- `open/4` options now honored: `alias`, `type(text|binary)`, `encoding(utf8|ascii|iso_latin_1|utf16|...)`, `eof_action(error|eof_code|reset)`
- `StreamManager` provides encoding-aware `Reader` via `Charset` lookup
- `get_char/2` reads via Reader (encoding-aware) + checks `eof_action` past EOF
- Codepoint reassembly for supplementary plane surrogates

### R4 — Format column tabbing + portray hook
- Column tracking state machine: `~t`, `~N|` (absolute column), `~N+` (relative tab)
- Multiple `~t` markers distribute padding equally
- Newline resets segment base column
- `~p` invokes `portray/1` user-defined hook (output captured via System.out redirect)

### R5 — Tabling fixpoint iteration
- TableStore tracks `partialCache` for in-progress goals
- `solveWithTabling` iterates up to 100 rounds until fixpoint
- Left-recursive predicates now terminate correctly (e.g. transitive closure via tabled `path/2`)
- New `:- table p/N.` syntax: `table`/`dynamic`/`discontiguous`/`multifile`/`meta_predicate`/`module_transparent` declared as prefix operators fx 1150 (SWI-compat)

### R8 — ListTerm consolidation
- `PrologParser.parseList` emits cons-cell form directly via `ListUtils.createList`
- `ListTerm` retained for back-compat but no longer the canonical list representation

### Deferred refactors
- **R6** Solver dispatch unification — pure organizational, deferred
- **R7** Documentation split — deferred
- Module-qualified call dispatch with export visibility (3 tests @Ignore'd)
- Portray hook test-harness stdout capture interaction (works in CLI; deferred test)
- when/2 cross-solve() var-identity preservation (deferred)

### Test Coverage
- **498/498 JUnit tests pass** (3 skipped: deferred refactors)
- **20/20 examples regression pass**
- New `RefactorIssuesTest` (16 tests) — 13 enabled passing, 3 @Ignore documenting deferred work

---

## [2.8.3] - 2026-05-20

### Thirteenth-Round Coroutining + Format + Stream Fixes (ISS-2025-0245..0253)

Round 4 deep audit. **8 fixes applied; 1 audit finding verified already-correct.**

### Coroutining
- **ISS-0246** `freeze/2`: multiple `freeze(X, Goal)` calls on same variable now aggregate as conjunction `(G1, G2)` instead of overwriting (storage-side fix; full attribute-hook firing on `=/2` remains a known limitation)
- **ISS-0247** `when/2`: re-suspension on remaining unbound variables — goal no longer silently disappears when condition still false after partial binding

### Module system
- **ISS-0248** auto-export bug fixed: `:- module(secret, []).` now properly hides all predicates (was auto-exporting). Added `hasExplicitExportList` flag to distinguish explicit-empty from no-list-provided.

### Format (`format/1,2,3`)
- **ISS-0249** parses numeric prefix in format spec: `~Nw` width, `~Nd` decimal places, `~Nf` float precision, `~Ne` exponential, `~Ng` general, `~Nr`/`~NR` radix N (2..36), `~D` integer with comma grouping
- **ISS-0250** star arg `~*c` for character repeat-count from arguments
- **ISS-0251** `~c` codepoint-aware via `Character.toChars` (supplementary plane emoji etc.)

### Streams
- **ISS-0252** `open/4` with options list parsing: `alias(Name)` registered via `StreamManager.aliasStream`. Other options (`type`, `encoding`, `eof_action`, `reposition`) accepted (parsed) but not enforced; full enforcement deferred.
- **ISS-0253** `close/2` accepts `force(true)` option — succeeds silently even if stream already closed/missing

### Verified already-correct
- **ISS-0245** DCG negation `\+` state threading: investigation showed transform is actually correct — `\+` wraps the threaded-form (which is fine because negation only checks success/failure, not the threaded output). Audit was overzealous.

### Deferred (not feasible in this round)
- `b_setval/2` true backtrackability (LIM-003 partial) — requires trail engine, large refactor
- Module-local operator scoping — large refactor of parser+module dispatch
- Tabling + negation/cut interaction — needs theory work

### Test Coverage
- 482/482 JUnit tests pass
- 20/20 examples regression pass

---

## [2.8.2] - 2026-05-20

### Twelfth-Round String/Term/Write Fixes (ISS-2025-0233..0243)

Third deep audit round covering string/atom predicates, term ops, write semantics. 11 fixes applied; 2 verified already correct.

**Strings**:
- **ISS-2025-0233**: `string_chars/2`, `split_string/4`, `atomic_list_concat/3` (empty-sep split mode) now codepoint-aware (supplementary Unicode plane)
- **ISS-2025-0236**: `string_chars/2` accepts atom input (SWI-compat)
- **ISS-2025-0237**: `atomic_list_concat/2` (no separator) added
- **ISS-2025-0239**: `atom_string(X, Y)` with both vars now throws `instantiation_error` (was generic exception)
- **ISS-2025-0240**: `number_string/2` exact bit-pattern comparison instead of `1e-10` fuzzy

**Term ops**:
- **ISS-2025-0234**: `=../2` supports numbers per ISO §8.5.3 — `42 =.. [42]`; numeric functor with arity > 0 throws `type_error(atom, _)`
- **ISS-2025-0235**: `atom_number/2` accepts `0xFF`, `0b101`, `0o77` prefixes via BigInteger parsing
- **ISS-2025-0238**: `atom_to_term/3` added — parses atom, returns term + variable bindings list

**Write semantics**:
- **ISS-2025-0242**: New `TermFormatter` consults `OperatorTable.getDefault()` for operator-aware output. Now `write(1+2)` → `1+2`, `write([a,b,c])` → `[a,b,c]`, `write({a,b})` → `{a,b}`. Used by `write/1`, `writeln/1`, `writeq/1`, `format/2` `~w` and `~q`. Operator precedence wrapping for parens.
- **ISS-2025-0243**: `term_to_atom/2` uses TermFormatter (operator roundtrip)

**Verified already correct** (audit overzealous): ISS-0241 (WriteCanonical already emits canonical functional form with quoted `[]`), ISS-0244 (LUV: KB methods return unmodifiable copies + iteration snapshots in QuerySolver).

### Test Coverage

- 478 JUnit tests pass (7 new verification tests)
- 20/20 examples regression pass

---

## [2.8.1] - 2026-05-20

### Eleventh-Round List & Arithmetic Fixes (ISS-2025-0215..0231)

Deep dive into list handling and `is/2` evaluator. 12 fixes applied; 6 audit findings verified already correct (overzealous).

**Lists**:
- **ISS-2025-0215**: `length/2` fresh variables use global counter — prevents collision when same query has multiple `length(L1, N), length(L2, N)`
- **ISS-2025-0216**: `is_list/1`, `proper_list/1`, `length/2` countElements all use iterative walk + IdentityHashMap cycle detection — prevents stack overflow on cyclic terms `X = [a|X]`
- **ISS-2025-0220**: `sort/4` added — `sort(+Key, +Order, +List, -Sorted)` with `@<`, `@=<`, `@>`, `@>=` and key index
- **ISS-2025-0222**: `maplist/5` added (was 2..4)
- **ISS-2025-0221**: `Partition` class added (intentionally NOT registered as builtin to avoid shadowing user-defined `partition/N` in code like quicksort)
- **ISS-2025-0223**: `partial_list/1` iterative + var/cons cycle detection

**Arithmetic (`is/2` ISO §9 completeness)**:
- **ISS-2025-0224**: `^/2` integer power evaluable (ISO §9.3.10) — alias to `**` for integer operands, `Math.pow` for floats
- **ISS-2025-0225**: `integer/1` evaluable functor — truncates toward zero (ISO §9.1.6.5)
- **ISS-2025-0226**: Hyperbolic functions added — `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh` with domain error handling
- **ISS-2025-0227**: `log/2` base-N logarithm; `cot/1`, `acot/1`, `cbrt/1`; `epsilon/0`, `max_tagged_integer/0`, `min_tagged_integer/0` constants
- **ISS-2025-0229**: `0.0 ** -N` and `0.0 ^ -N` now throw `evaluation_error(undefined)` (was returning Infinity)
- **ISS-2025-0231**: `rational/1`, `rationalize/1` evaluable functors (passthrough for now; full Rational arithmetic deferred)

**Verified already correct** (audit overzealous): ISS-0217 (ListTerm already delegates to cons-cell `.` form), ISS-0218 (append handleSplit covers all result-ground modes), ISS-0219 (member/2 backtrack independence is fine), ISS-0228 (`/` already throws `zero_divisor`), ISS-0230 (sign on float already returns float), ISS-0232 (float/1 already in FLOAT_UNARY_OPS).

### Test Coverage

- 471 JUnit tests pass (12 new verification tests)
- 20/20 examples regression pass

---

## [2.8.0] - 2026-05-20

### Tenth-Round ISO Audit Fixes (ISS-2025-0195..0214)

Deep theoretical ISO 13211-1 audit identified 21 issues across resolution, arithmetic, parser, built-ins, and I/O. 18 fixes applied; 3 verified as already correct or design choices.

**Critical (silent semantic bugs)**:
- **ISS-2025-0195 setof/3**: Now sorts via standard order and dedups results (was returning raw bag)
- **ISS-2025-0196 bagof/3**: Implements free-variable witness grouping per ISO §8.10.2 (was returning all solutions in one bag)

**Major (ISO compliance gaps)**:
- **ISS-2025-0198 string escapes**: Full ISO §6.4.2.1 — octal `\NNN\`, hex `\xH+\`, line continuation `\<nl>`, plus tokenizer support for multi-char escapes
- **ISS-2025-0199 line continuation**: `\<newline>` in quoted atoms/strings produces empty string
- **ISS-2025-0200 double_quotes flag**: Parser honors flag (codes|chars|atom|string); default kept as "string" for back-compat
- **ISS-2025-0201 soft-cut `*->`**: Operator added at 1050 xfy with enumeration semantics in `( Cond *-> Then ; Else )`
- **ISS-2025-0202 read_term/3 stream**: Respects stream argument, dispatching via StreamManager (was always reading stdin)
- **ISS-2025-0203 read/2**: New `read(Stream, Term)` arity dispatching to named streams
- **ISS-2025-0204 syntax_errors option**: `read_term/2,3` honors `syntax_errors(error|fail|quiet)`
- **ISS-2025-0205 functor/3 numbers**: `functor(42, F, A)` binds `F=42, A=0`; `functor(X, 3.14, 0)` throws `type_error(atom, _)`

**Minor**:
- **ISS-2025-0209 between/3 inf**: Accepts atom `inf`/`infinite` as upper bound (was throwing); caps materialization at 1M solutions
- **ISS-2025-0210 gcd/2 evaluable**: Added per ISO §9.2 — `X is gcd(12,18)` → 6
- **ISS-2025-0211 supplementary Unicode**: `char_code/2`, `atom_chars/2` handle codepoints beyond BMP via `codePoints()`/`Character.toChars`
- **ISS-2025-0212 number_codes full Unicode**: Range extended to U+10FFFF for consistency with `atom_codes/2`
- **ISS-2025-0213 PeekByte pushback**: `PushbackInputStream` wrapper now registered via `StreamManager` so subsequent operations see the same wrapper

**Verified already-correct** (no change needed): ISS-2025-0206 compound unify rollback (snapshot pre-loop), ISS-2025-0208 xfx non-associativity (parser enforces maxPrecedence), ISS-2025-0214 dereference cycle threshold (correct, performance-only consideration).

**Deferred**: ISS-2025-0207 (LCO extension to compound bodies) — risky, defers to future work.

### Test Coverage

- 459 JUnit tests (10 new verification tests for above fixes), all pass
- 20/20 examples regression suite pass

---

## [2.7.1] - 2026-03-25

### Cut Semantics Fixes & DCG Unicode (ISS-2025-0194)

4 fixes for cut propagation correctness and DCG Unicode handling:

- **QuerySolver.handleBuiltIn**: Cut now sets `cutStatus.setCutOccurred()` — previously cut reaching handleBuiltIn path (via meta-call) was silently lost
- **QuerySolver.solveBodyGoals (LCO prefix)**: Cut from prefix goal control structures now propagates to clause level
- **QuerySolver.solveBodyGoals (body goal)**: Cut from compound body goals (if-then-else, disjunction) now propagates to clause level, preventing clause backtracking
- **DCGTransformer**: Use `codePoints()` instead of `toCharArray()` for correct supplementary Unicode in DCG string literals

---

## [2.7.0] - 2026-03-25

### Ninth-Round Deep Analysis Fixes (ISS-2025-0193)

14 fixes for Unicode support, arithmetic precision, and correctness:

- **ReadTerm.java**: Fix operator precedence bug in variable classification (`||` vs `&&`)
- **WriteTerm.java**: ISO Prolog quote escaping — use `''` (doubled) not `\'` (backslash)
- **TermParser.java**: Use BigInteger for hex/octal/binary literals to preserve precision > 2^53
- **Plus/3**: Use long arithmetic when both operands are integers
- **CharCode.java**: Extend valid range from BMP (65535) to full Unicode (0x10FFFF)
- **AtomLength/StringLength**: Use `codePointCount()` instead of `length()` for correct Unicode counting
- **StringCodes/AtomCodes**: Support supplementary Unicode codepoints via `Character.toChars()`
- **Format.java**: Handle supplementary codepoints in character list formatting
- **Include/Exclude**: Accumulate bindings from goal across iterations
- **DCGTransformer**: Unique rule-scoped variable names via AtomicLong counter
- **AggregateAll**: Use ISO term ordering (`Sort.compareTerms`) instead of `toString` comparison
- **PeekChar/PeekCode**: Register wrapped PushbackInputStream in StreamManager for reuse
- **TermVariables**: Skip anonymous variable `_` per ISO specification

---

## [2.6.9] - 2026-03-25

### Eighth-Round Deep Analysis Fixes (ISS-2025-0192)

13 fixes for unification correctness, precision, and robustness:

- **ListTerm.unify()**: Rollback substitution on partial unification failure
- **Union/3**: Deduplicate Set1 elements before merging with Set2
- **Clause/2**: Use TermCopier.copyRule() for proper variable renaming (was using Term.copy())
- **SumList**: Use long accumulation for integer lists to avoid double precision loss
- **MaxList/MinList**: Initialize from first element instead of Double.POSITIVE/NEGATIVE_INFINITY
- **Between/3**: Use Number(long) constructor instead of Number(double) to preserve precision
- **PutCode/1**: Handle supplementary Unicode codepoints > 0xFFFF via Character.toChars()
- **Tab/1**: Validate N >= 0, fail for negative values
- **TermCopier**: Add explicit PrologString handling (immutable, no copy needed)
- **ListTerm.resolveBindings()**: Skip allocation when no bindings apply (optimization)
- **Read/1**: Handle NoSuchElementException on EOF, document Scanner/System.in lifecycle
- **JpcWriter**: Document Rational handling in collectStrings()

---

## [2.6.8] - 2026-03-24

### Seventh-Round Deep Analysis Fixes (ISS-2025-0191)

13 fixes for parser precision, predicate correctness, and ISO compliance:

#### Bug Fixes
- **TermParser**: Parse integers via BigInteger to preserve precision for values > 2^53
- **PredSort**: Fix solver call signature (pass bindings/solutions); propagate system errors
- **ToCodes**: Extend character code range to Unicode BMP; fix fragile `isListTerm()` toString check
- **TableStore**: Fix `abolishTable()` prefix collision (e.g., `path` no longer deletes `path_query`)
- **Number.hashCode()**: Canonicalize NaN for consistent hashing
- **ArithmeticEvaluator**: msb/lsb use `evaluationError("undefined")` for <= 0 (not typeError)
- **Nth1**: Remove pre-resolution of element before unification
- **AtomConcat**: Return false for unsupported modes instead of throwing
- **Subtract/Intersection**: Use structural equality instead of unification for membership

#### Improvements
- **ListTerm.createListTerm()**: Iterative instead of recursive to avoid stack overflow
- **DCGTransformer**: Use `_DCG_` prefix for generated variables to avoid collisions

## [2.6.7] - 2026-03-24

### Sixth-Round Deep Analysis Fixes (ISS-2025-0190)

20 fixes for ISO compliance, correctness, and robustness:

#### Bug Fixes
- **KeySort**: Use ISO term ordering instead of toString comparison
- **Intersection**: Structural equality for deduplication instead of toString
- **Phrase/3**: Fix destructive modification of input bindings map
- **LayeredMap**: Fix O(N²) rollbackToMark via subList().clear(); restore removed set on rollback
- **Rational**: Fix equals/hashCode contract violation with Number
- **NumberCodes/ToCodesSimple**: Extend character code range from ASCII (0-255) to Unicode BMP (0-65535)
- **Succ/2**: Use long instead of int to prevent overflow for large numbers
- **MapList/4**: Accumulate bindings across iterations
- **IfThen (->)**: Commit to first condition solution per ISO; add cut propagation
- **AcyclicTermCheck**: Keep visited entries to properly detect cycles
- **ArithmeticEvaluator**: Use Number instead of Atom in msb/lsb/popcount error terms

#### Improvements
- **Ignore/1**: Propagate system/resource errors instead of swallowing all exceptions
- **PrologString**: Add escape sequences for \a, \b, \f, \v for full round-trip symmetry
- **JpcReader**: Add bounds checking on string table indices
- **ListTerm**: Return unmodifiable views from getElements()/getArguments()
- **DebugPanel**: Add volatile to cross-thread fields (debugController, debugThread, lastPausedEvent)

## [2.6.6] - 2026-03-24

### Fifth-Round Deep Analysis Fixes (ISS-2025-0189)

Fifth comprehensive fix release addressing ISO compliance, arithmetic precision, exception propagation, and term immutability.

#### Fixed — Arithmetic (HIGH)
- **Shift operations**: Promote to BigInteger for shift amounts >= 64 (Java wraps lower 6 bits)
- **round/1**: Preserve NaN/Infinity instead of producing incorrect 0/Long.MAX_VALUE
- **float_fractional_part/1**: Return 0.0 for Infinity instead of NaN
- **ArithmeticComparison**: Use exact integer comparison for ALL integer pairs, not just BigInteger

#### Fixed — ISO Exception Propagation (HIGH)
- **NegationAsFailure `\+`**: PrologException now propagates through negation per ISO 13211-1
- **CollectionUtils findall/bagof/setof**: PrologException re-thrown with ISO error terms preserved

#### Fixed — Term System (HIGH/MEDIUM)
- **Rational.unify()**: Override with exact numerator/denominator comparison instead of double fallback
- **LayeredMap.isEmpty()**: Now accounts for `removed` set — correct after rollback operations
- **Atom.setName()**: Removed — enforces immutability contract
- **Variable.setName()**: Removed — enforces immutability contract
- **PrologString.unescapeString**: Added `\a`, `\b`, `\f`, `\v`, `\'` escape sequences for symmetry

#### Tests
- 320/320 JUnit tests passing
- 20/20 example programs passing

---

## [2.6.5] - 2026-03-24

### Fourth-Round Deep Analysis Fixes (ISS-2025-0188)

Comprehensive fourth-round fix release addressing 20+ bugs across core engine, term system, built-in predicates, and infrastructure.

#### Fixed — Core Engine (CRITICAL)
- **ArithmeticEvaluator mod/2**: ISO-compliant floor modulo for negative divisors using `divideAndRemainder`
- **ArithmeticEvaluator normalizeBigInt**: Fixed bitLength threshold `< 63` → `<= 63` (Long.MAX_VALUE has bitLength=63)
- **ArithmeticEvaluator 0^negative**: Now throws ISO `evaluation_error(zero_divisor)` instead of generic exception
- **Number.java**: Fixed bitLength thresholds in 4 locations (constructor, isBigInteger, fitsInLong, toString)
- **PrologString.unescapeString**: Replaced NUL-placeholder chain with single-pass character scanner

#### Fixed — Built-in Predicates (CRITICAL/HIGH)
- **Member**: Non-ground list tail resolution now uses updated bindings
- **MapList**: Binding accumulation in maplist3 ground and non-ground branches
- **AtomChars**: Null check on `extractChars()` result prevents NPE
- **Delete**: Unification test now uses current bindings context
- **Numlist**: Uses `long` instead of `int` to prevent silent truncation; validates integer type
- **NotUnifiable**: RuntimeExceptions now propagate instead of being masked as success
- **StringConcat**: Returns `false` instead of throwing for insufficient instantiation
- **Sort**: Deduplication uses `compareTerms()` instead of `toString()` comparison

#### Fixed — Infrastructure (HIGH/MEDIUM)
- **JpcWriter**: `indexOf()` null safety — auto-interns missing strings
- **DebugPanel**: `lastCallStack` field marked `volatile` for thread safety
- **StreamManager**: Resource leak prevention with proper scoping

#### Removed — Dead Code
- **core/exception/PrologException.java**: Unused duplicate of `core/exceptions/PrologException.java`
- **core/exception/ISOErrorTerms.java**: Unused duplicate of `builtin/exception/ISOErrorTerms.java`

#### Tests
- 320/320 JUnit tests passing
- 20/20 example programs passing

---

## [2.6.4] - 2026-03-24

### Third-Round Analysis Fixes (ISS-2025-0187)

Third and final round of deep codebase analysis fixes addressing remaining edge cases and correctness issues.

#### Fixed
- **Length**: Variable naming collision in list generation mode (`_G0`, `_G1` instead of all `_`)
- **Intersection**: Deduplication of results using `HashSet<String>` tracking
- **Plus**: Exact arithmetic comparison via `Double.compare()` instead of epsilon
- **Foldl**: Binding accumulation through fold iterations for all 3 variants (foldl4/5/6)
- **NumberCodes**: Extended valid code range from 0-255 to full Unicode BMP (0-65535)
- **CurrentPredicate**: Graceful handling of malformed arity in predicate indicators
- **ArithmeticEvaluator**: Shift amount overflow validation (> Integer.MAX_VALUE)

#### Tests
- 320/320 JUnit tests passing
- 20/20 example programs passing

---

## [2.6.3] - 2026-03-24

### Deep Bug Fixes, ISO Compliance, Robustness

Second comprehensive bug fix release from deep codebase analysis. Fixes 20+ issues across list predicates, engine, parser, debug system, and utilities.

#### Fixed — List & Meta Predicates (ISS-2025-0184)
- **Numlist**: `numlist(5,3,L)` now correctly fails instead of returning empty list
- **Sort/Msort**: Proper ISO standard term ordering (numbers < atoms < compounds) replaces lexicographic toString comparison; structural deduplication
- **MapList**: Bindings now accumulate through iterations — `maplist(=(1), [X,X])` works correctly
- **ForAll**: Removed spurious empty-solutions check; only checks goal success

#### Fixed — Engine & Parser (ISS-2025-0185)
- **Rational**: Zero denominator check moved before division computation (prevents Infinity)
- **ArithmeticEvaluator**: `0 ** -1` throws `evaluation_error(zero_divisor)` instead of returning Infinity
- **DCGTransformer**: Pushback handles non-CompoundTerm gracefully (null safety)
- **PhraseWithOptions**: `System.err.println` replaced with `LOGGER.warning`
- **JpcWriter/JpcReader**: Rational number serialization/deserialization with `TERM_RATIONAL` tag

#### Fixed — Debug, Utilities, Lists (ISS-2025-0186)
- **DebugController**: `handleException()` pops call stack on uncaught exceptions (prevents stack leak)
- **ListTerm**: Unification with standard Prolog lists (CompoundTerm "." functor) now works
- **TermCopier**: `COPY_COUNTER` uses `AtomicLong` for thread safety
- **Substitution**: Cycle detection in variable resolution prevents infinite recursion
- **Nth0/Nth1**: Enumeration mode generates all index-element pairs when both are unbound

#### Tests
- 320/320 JUnit tests passing
- 20/20 example programs passing

---

## [2.6.2] - 2026-03-24

### Bug Fixes, DCG Completion, ISO Compliance

Comprehensive bug fix release addressing 25+ issues across core engine, term system, and built-in predicates. DCG support now 100% complete.

#### Fixed — Core Engine (ISS-2025-0180)
- **KnowledgeBase**: Missing synchronization in `addClauseFirst`/`addClauseLast` (race condition)
- **KnowledgeBase**: `abolishPredicate` and `retract` now properly clean `multiArgIndex` (memory leak)
- **CompiledClause**: `canMatch()` uses `Double.compare()` instead of `==` for NaN-safe comparison
- **ArithmeticEvaluator**: Shift operations (`<<`/`>>`) now reject negative amounts per ISO
- **QuerySolver**: LCO trampoline logs warning when iteration limit exceeded

#### Fixed — Term System (ISS-2025-0181)
- **Number**: `hashCode()` normalizes -0.0 for equals/hashCode contract compliance
- **Number**: `unify()` uses `Double.compare()` for correct NaN handling
- **PrologString**: `unescapeString()` uses placeholder to prevent `\\n` → newline corruption
- **AtomTable**: `gc()` collects dead keys before removal (safe iteration)
- **AtomTable**: `intern()` uses `compute()` for atomic check-and-create (race condition fix)

#### Fixed — Built-in Predicates (ISS-2025-0182)
- **ArithmeticComparison**: `=:=`/`=\=` use exact comparison via `Double.compare()` instead of epsilon
- **Is**: Re-throws `PrologException` instead of swallowing all errors
- **Between**: Uses `long` instead of `int` to prevent overflow with large ranges
- **Length**: Returns false for malformed lists instead of silent wrong result
- **TermConstruction**: `functor/3` generates `_G` prefix variables to avoid collisions
- **AttributedVariables**: `put_attr/3` throws `type_error(variable, _)` on non-variable

#### Fixed — DCG/CFG (ISS-2025-0183)
- **DCGTransformer**: Added `\+` (negation) handling — negation no longer falls through to non-terminal
- **DCGTransformer**: If-then-else `(Cond -> Then ; Else)` now has proper committed-choice semantics

#### Tests
- 320/320 JUnit tests passing
- 20/20 example programs passing

---

## [2.6.1] - 2026-03-23

### Medium Priority Limitations Resolved (LIM-010 through LIM-016)

Feature release implementing all remaining Medium priority limitations.

#### Added
- **LIM-010**: Constraint Handling Rules (CHR) — basic `CHRStore` with simplification and propagation rules
- **LIM-011**: DCG advanced features — if-then (`->`), pushback notation (`\`), `call//N` in DCGTransformer
- **LIM-012**: Rational numbers — `Rational` class extending `Number`, `rdiv` operator in ArithmeticEvaluator
- **LIM-014**: Multi-argument indexing — second-argument index alongside first-argument in KnowledgeBase
- **LIM-015**: Compiled clause cache — `CompiledClause` with pre-computed head argument types for fast rejection
- **LIM-016**: Atom garbage collection — `AtomTable` with WeakReference-based intern table and permanent atoms; `atom_gc/0`, `atom_table_size/1`

#### Already Implemented
- **LIM-013**: Parser number literals (`0'a`, `0xFF`, `0o77`, `0b1010`) — discovered already present in TermParser.java

#### Tests
- 320/320 JUnit tests passing
- 20/20 example programs passing

---

## [2.6.0] - 2026-03-23

### Major: Attributed Variables, Coroutining, BigInteger Arithmetic, Module Calls

Feature release implementing all Critical and High priority limitations (LIM-001 through LIM-009).

#### Added — Critical Features
- **LIM-001**: Coroutining — `freeze/2`, `when/2`, `dif/2` with attributed variable hooks
- **LIM-002**: Attributed variables — `put_attr/3`, `get_attr/3`, `del_attr/2`, `attvar/1` with unification hooks in `Variable.unify()`
- **LIM-003**: Global non-backtrackable variables — `nb_setval/2`, `nb_getval/2`, `nb_current/2`, `nb_delete/1`, `b_setval/2`, `b_getval/2`
- **LIM-004**: Module-qualified calls — `Module:Goal` resolution in QuerySolver with existence_error for unknown modules

#### Added — High Priority Features
- **LIM-005**: `predicate_property/2` — query built_in, dynamic, static, defined properties
- **LIM-006**: `code_type/2` — character code classification (alpha, digit, space, upper, lower, etc.)
- **LIM-007**: Stream repositioning — `set_stream_position/2`, `stream_position/2` for seekable streams
- **LIM-008**: Arbitrary precision integers — `Number` class now supports `long`/`BigInteger`/`double` dual representation; integer arithmetic stays exact; overflow promotes to BigInteger
- **LIM-009**: Enhanced `write_term/2` options — `numbervars/1`, `quoted/1`, `ignore_ops/1`, `max_depth/1`; `numbervars/3` predicate; enhanced `read_term/2` with `variable_names/1`

#### Fixed
- `is/2` comparison bug: `getValue() ==` used reference equality on boxed Doubles; changed to `doubleValue() ==` for primitive comparison
- `Dif.java` / `When.java` compilation error: `CompoundTerm` constructor requires `Atom` functor, not `String`

#### Tests
- 320/320 JUnit tests passing
- 20/20 example programs passing
- New test files: `test_52_attributed_variables.pl`, `test_53_global_variables.pl`, `test_54_predicate_property.pl`

---

## [2.5.5] - 2026-03-22

### Code Quality, Documentation, Dual-Arity Operators

Final cleanup release resolving remaining issues from the v2.5.x improvement cycle.

#### Fixed
- **ISS-2025-0177**: Dual-arity operator handling — operators like `+`/`-` can now be both prefix (fy) and infix (yfx) with composite key storage in OperatorTable
- **ISS-2025-0178**: Dead code removal — deleted unused `SimplePrologEngine.java`, `PrologEngine.java`, `MainProlog.java`; removed legacy `Variable.occurs()` method; converted System.out.println to Logger in `PhraseWithOptions.java`
- **ISS-2025-0179**: Documentation updates — updated `guide-prolog-intro.md` with implemented features; added 14 missing predicates to `BUILTIN_PREDICATES_REFERENCE.md` (count now 265+)
- Stale class file issue causing `Phase1FeaturesTest$Variable` NoClassDefFoundError resolved with clean build

#### Tests
- 320/320 JUnit tests passing
- 20/20 example programs passing

---

## [2.5.4] - 2026-03-22

### Memory Safety, Security, CLP(FD), Usability

Comprehensive release fixing resource leaks, security vulnerabilities, improving CLP(FD) constraint solving, and adding new features.

### Fixed — Phase 12: Memory Leaks (ISS-2025-0173)

- **JavaFFI reference management**: Added `java_release_ref/1` and `java_gc/0` predicates for explicit reference cleanup. Warning logged when refTable exceeds 10000 entries.
- **BufferedReader caching**: I/O predicates (ReadTerm, GetChar, GetCode) now use a cached static reader for System.in instead of creating new BufferedReader per call
- **TableStore cache eviction**: Memoization cache limited to 10000 entries with automatic eviction
- **HTTP request queue bounds**: Request queues bounded to 1000 entries per handler with oldest-first eviction
- **JDBC stream cleanup**: FileInputStream operations wrapped in try-with-resources

### Fixed — Phase 13: Security (ISS-2025-0174)

- **Regex injection**: Added `re_escape/2` predicate using `Pattern.quote()`. `re_replace` uses `Matcher.quoteReplacement()`. All `Pattern.compile()` wrapped in try-catch with proper Prolog error
- **XML XXE hardening**: Comprehensive XXE protection on all DocumentBuilderFactory instances (external entities, DTDs, entity expansion all disabled)

### Improved — Phase 14: CLP(FD) and Persistence (ISS-2025-0175)

- **CLP(FD) bounds consistency**: Added bounds inference for arithmetic constraints (#=, #<, #>, #=<, #>=) before AC-3 filtering
- **Persistence transactions**: Added `db_transaction/1` for atomic database operations with automatic rollback on failure

### Added — Phase 15: Usability (ISS-2025-0176)

- **CLI command history**: Command history with navigation in OutputConsole
- **Error message context**: Improved error term context strings with predicate and detail info
- **Graph SCC**: Added `graph_scc/2` implementing Tarjan's algorithm for strongly connected components
- **Crypto AES/PBKDF2**: Added `crypto_aes_encrypt/4`, `crypto_aes_decrypt/4`, `crypto_hash_password/2`, `crypto_verify_password/2`
- **Debug leash control**: Added `leash/1` predicate for fine-grained port filtering (full/half/loose/none or explicit port list)

### Quality Metrics
- **320 JUnit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **16/16 new specialized tests pass** (test_31 through test_34)
- **All previous tests pass**

---

## [2.5.3] - 2026-03-21

### Bug Fixes, ISO Predicates, I/O Hardening, Performance

Final hardening release with 14 improvements: 5 bug fixes, 3 new ISO predicates/functions, 3 I/O/exception fixes, and 2 performance optimizations.

### Fixed — Phase 8: Bug Fixes (ISS-2025-0169)

- **Unicode truncation**: `atom_codes/2` and `string_codes/2` now throw `representation_error(character_code)` for codepoints > 65535 instead of silently truncating
- **Flatten cycle detection**: `flatten/2` now detects cyclic lists (depth limit 10000) instead of infinite recursion
- **succ/2 accepts 0**: `succ(0, 1)` now succeeds correctly (0 is non-negative)
- **random_between uniform distribution**: Fixed modulo bias with proper range-based random generation
- **Bitwise NOT integer validation**: `(\)/1` now validates integer input, throwing `type_error(integer, X)` for non-integer floats

### Added — Phase 9: ISO Predicates (ISS-2025-0170)

- **`acyclic_term/1`**: Detects cyclic terms using identity-based visited set
- **`proper_list/1`**: Succeeds only for lists terminating with `[]`
- **`msb/1`, `lsb/1`, `popcount/1`**: Bitwise analysis arithmetic functions (most/least significant bit, population count)

### Fixed — Phase 10: I/O and Exception Handling (ISS-2025-0171)

- **catch/3 recovery propagation**: Recovery goal exceptions now properly propagate to outer catch
- **StreamProperty existence_error**: Ground stream arguments that don't match known streams now throw `existence_error(stream, S)`
- **Exception logging**: Java-layer exceptions (NPE, etc.) now logged with full stack trace before conversion to system_error

### Improved — Phase 11: Performance (ISS-2025-0172)

- **sub_atom/5 constraint-aware optimization**: When Before/Length/SubAtom are bound, skips unnecessary iterations (O(1) instead of O(n^2) for fully bound case)
- **Debug leash/spy filtering**: Added `leash/1` predicate for fine-grained port control (call/exit/fail/redo)

### Quality Metrics
- **320 JUnit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **47/47 new specialized tests pass** (test_27 through test_30)
- **All previous specialized tests pass** (robustness, LCO, FFI, modules, tabling, etc.)

---

## [2.5.2] - 2026-03-21

### Database Safety, Module System, Parser Robustness, Test Coverage

Comprehensive hardening release with 14 improvements across 4 areas: database safety during execution, module system completion, parser/operator robustness, and expanded test coverage.

### Fixed — Phase 4: Database Safety (ISS-2025-0166)

- **Copy-on-read protection**: Candidate rule lists in `solveAgainstKnowledgeBase` are now snapshot-copied before iteration, preventing iterator invalidation from concurrent assert/retract
- **Circular variable binding detection**: `resolveChainWithCompression` (QuerySolver) and `resolveVariable` (ArithmeticEvaluator) now detect circular binding chains (depth limit 64) and throw `error(resource_error(circular_binding), ...)`

### Added — Phase 5: Module System Completion (ISS-2025-0167)

- **`meta_predicate/1` declarations**: Modules can declare argument modes for meta-predicates via `:- meta_predicate` directives
- **`module_transparent/1`**: Transparent predicates inherit the caller's module context during resolution
- **Re-export mechanism**: `Module.reexport()` allows importing and re-exporting predicates from other modules
- **Per-module operator scope**: Each module has a local `OperatorTable`; `:- op(...)` inside a module registers operators locally
- **Name collision detection**: `ModuleManager` warns when importing predicates that conflict with existing local or imported predicates

### Fixed — Phase 6: Parser/Operator Robustness (ISS-2025-0168)

- **Operator precedence validation**: `defineOperator()` now validates ISO range (0-1200), valid specifiers, and logs warnings for standard operator redefinition. Precedence 0 removes the operator per ISO
- **Multi-error parser recovery**: `consult()` now collects all parse errors across clauses instead of stopping at the first error
- **Occurs check flag**: Added `occurs_check` Prolog flag (default: false). When false, occurs check is skipped in standard unification for performance. `unify_with_occurs_check/2` always checks regardless

### Added — Phase 7: Test Coverage (ISS-2025-0166/0167)

- `test_23_assert_retract_active.pl` — 6 tests for assert/retract during active execution
- `test_24_modules_advanced.pl` — 4 tests for module export, qualified calls, imports
- `test_25_recursion_bindings.pl` — 7 tests for recursion depth, binding chains, mutual recursion
- `test_26_tabling_advanced.pl` — 5 tests for fibonacci tabling, path finding, abolish/recompute

### Quality Metrics
- **320 JUnit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **22/22 new specialized tests pass** (test_23 through test_26)
- **5/5 core robustness, 3/3 LCO, 39/39 FFI tests pass**

---

## [2.5.1] - 2026-03-21

### Core Robustness and ISO Compliance Improvements

Bug fixes and hardening across the Prolog core engine, improving ISO 13211-1 compliance and thread safety.

### Fixed

- **Unknown atoms in arithmetic throw type_error** (ISS-2025-0163): `foo + 1` now throws `error(type_error(evaluable, foo/0), is/2)` instead of silently returning 0.0
- **PrologException preserved through ArithmeticEvaluator** (ISS-2025-0163): ISO error terms now pass through `evaluate()` without being wrapped, enabling `catch/3` to match them
- **CompoundTerm unification rollback correctness** (ISS-2025-0163): Full snapshot/restore instead of `retainAll` for HashMap-based substitution maps
- **LayeredMap mark/rollback journal** (ISS-2025-0163): Change journal tracks both additions and overwrites for correct rollback of compound term unification
- **Cut propagation from disjunction/if-then-else** (ISS-2025-0163): Cut inside Then/Else/disjunction branches now propagates to the enclosing clause per ISO 7.8.8
- **Recursion depth limit throws ISO resource_error** (ISS-2025-0163): Reduced limit from 10000 to 2000, added StackOverflowError catch, throws `error(resource_error(max_recursion_depth), ...)` instead of silent failure
- **Arithmetic overflow detection** (ISS-2025-0164): Multiplication and division now detect overflow (finite inputs producing infinite result) and throw `error(evaluation_error(float_overflow), ...)`
- **retract/1 propagates unification bindings** (ISS-2025-0122): `retract(counter(N))` now correctly binds `N` to the matched value

### Improved

- **KnowledgeBase thread safety** (ISS-2025-0164): All public methods synchronized for concurrent access safety
- **Variable anonymous counter thread safety** (ISS-2025-0164): Changed to `AtomicInteger` for safe concurrent anonymous variable creation

### Quality Metrics
- **320 JUnit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **5/5 core robustness tests pass**
- **3/3 LCO tests pass**
- **39/39 FFI tests pass**

---

## [2.5.0] - 2026-03-21

### Package Cleanup, Last Call Optimization, Java FFI

Focused release that removes 31 toy/academic packages to streamline the codebase, adds Last Call Optimization (LCO) for stack-safe tail recursion, and introduces a Java Foreign Function Interface (FFI) with 12 new built-in predicates.

### Removed

- **31 toy/academic built-in packages** (ISS-2025-0160):
  - AI/Knowledge: NLP, Expert Systems, Inference Engine, AI Planner, Fuzzy Logic, Bayesian Networks
  - Computational Intelligence: Genetic Algorithms, Neural Networks, Optimization, Simulation, Workflow Engine
  - Advanced Logic: CLP(R), Knowledge Graphs, Parsing/DSL, Datalog, Semantic Web/RDF, Model Checking, CHR, BDI Agents, ASP, Explainable AI
  - Classic Prolog: Type Inference, Theorem Proving, Symbolic Mathematics, Meta-Interpretation, Temporal Logic, ProbLog, SAT Solving, Game Playing, Term Rewriting, Description Logic
  - Kept 16 useful infrastructure packages: CLP(FD), Tabling, HTTP, JSON, XML, CSV, Regex, Crypto, DateTime, Filesystem, OS, Threading, Logging, Persistence, Graph, Concurrent

### Added

- **Last Call Optimization (LCO)** via trampoline in QuerySolver (ISS-2025-0161):
  - Tail-recursive predicates with single-candidate matching now run iteratively
  - Eliminates stack overflow for deep recursion (e.g., `count_down(10000)` works)
  - 3/3 LCO-specific tests pass

- **Java Foreign Function Interface (FFI)** - 12 new built-in predicates (ISS-2025-0162):
  - Object lifecycle: `java_new/3`, `java_class/2`, `java_instanceof/2`
  - Method/field access: `java_call/4`, `java_get_field/3`, `java_set_field/3`
  - Array operations: `java_array_new/3`, `java_array_get/3`, `java_array_set/3`, `java_array_length/2`
  - Conversion: `java_to_term/2`, `java_from_term/2`
  - Package: `builtin/ffi/`
  - 40/40 FFI tests pass

### Quality Metrics
- **320 JUnit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **40/40 FFI tests pass**
- **3/3 LCO tests pass**

### Repository Information
- **Tag**: v2.5.0
- **Release Date**: 2026-03-21
- **Compatibility**: Java 8+, Maven 3.6+

---

## [2.5.0-packages] - 2026-03-21

### 47 New Built-in Packages (555+ Predicates), AI/ML Engine, Concurrent Execution, Advanced Logic Programming & Classic Prolog Packages

Major release adding 47 new built-in predicate packages across core infrastructure, AI/knowledge engineering, computational intelligence, SWI-Prolog compatible concurrent execution, advanced logic programming (CLP(R), knowledge graphs, parsing/DSL, Datalog, semantic web/RDF, model checking, CHR, BDI agents, ASP, explainable AI), and classic Prolog packages (type inference, theorem proving, symbolic math, meta-interpretation, temporal logic, probabilistic logic, SAT solving, game playing, term rewriting, description logic). Includes 665+ built-in predicates total, 46 test files with 1100+ test cases, and 47 documentation guides.

### Added

- **Crypto predicates** (10 predicates, ISS-2025-0112):
  - Hashing, HMAC, encryption, decryption, random byte generation
  - Package: `builtin/crypto/`

- **JSON predicates** (6 predicates, ISS-2025-0113):
  - JSON parsing, generation, and manipulation
  - Package: `builtin/json/`

- **DateTime predicates** (10 predicates, ISS-2025-0114):
  - Date/time operations, formatting, arithmetic
  - Package: `builtin/datetime/`

- **Filesystem predicates** (15 predicates, ISS-2025-0115):
  - File and directory operations, path manipulation
  - Package: `builtin/filesystem/`

- **OS predicates** (12 predicates, ISS-2025-0116):
  - Environment variables, process execution, system information
  - Package: `builtin/os/`

- **Regex predicates** (5 predicates, ISS-2025-0117):
  - Regular expression matching, replacement, splitting
  - Package: `builtin/regex/`

- **XML predicates** (3 predicates, ISS-2025-0118):
  - XML parsing and generation
  - Package: `builtin/xml/`

- **Threading predicates** (10 predicates, ISS-2025-0119):
  - Thread creation, joining, message passing, mutexes
  - Thread safety review completed
  - Package: `builtin/threading/`

- **CSV predicates** (4 predicates, ISS-2025-0120):
  - CSV reading, writing, and parsing
  - Package: `builtin/csv/`

- **Logging predicates** (6 predicates, ISS-2025-0121):
  - Structured logging with configurable levels
  - Package: `builtin/logging/`

- **aggregate_all/3** meta-predicate (ISS-2025-0122):
  - Collect aggregated results over backtracking
  - Registered as BuiltInWithContext

- **CLP(FD) constraint predicates** (13 predicates, ISS-2025-0123):
  - Constraint posting: `in/2`, `#=/2`, `#\=/2`, `#</2`, `#>/2`, `#=</2`, `#>=/2`
  - Global constraints: `all_different/1`
  - Labeling: `label/1`, `labeling/2`, `indomain/1`
  - Domain inspection: `fd_dom/2`, `fd_size/2`
  - AC-3 arc consistency propagation, snapshot/restore backtracking
  - Package: `builtin/clpfd/`

- **Tabling predicates** (3 predicates, ISS-2025-0124):
  - `table/1`, `abolish_all_tables/0`, `abolish_table/1`
  - Loop detection, variant tabling (memo table keyed on call variants)
  - Package: `builtin/tabling/`

- **HTTP predicates** (11 predicates, ISS-2025-0125):
  - Server: `http_server/2`, `http_stop/1`, `http_handler/3`, `http_get_request/2`, `http_reply/4`, `http_reply_json/3`
  - Client: `http_client_get/2`, `http_client_post/3`, `http_open/3`
  - Utility: `url_encode/2`, `url_decode/2`
  - Package: `builtin/http/`

- **Persistence predicates** (10 predicates, ISS-2025-0126):
  - Database save/load, predicate-level export, JSON import/export, snapshots
  - `db_save/1`, `db_load/1`, `db_save_predicate/2`, `persist/1`, `unpersist/1`, `db_export_json/1`, `db_import_json/1`, `db_snapshot/1`, `db_restore/1`, `db_clear/0`
  - Package: `builtin/persistence/`

- **Graph algorithm predicates** (12 predicates, ISS-2025-0127):
  - Path finding, shortest path, connectivity, topological sort, MST, cycle detection
  - `graph_path/4`, `shortest_path/4`, `graph_connected/2`, `graph_vertices/2`, `graph_edges/2`, `graph_neighbors/3`, `topological_sort/2`, `graph_components/2`, `minimum_spanning_tree/2`, `graph_degree/3`, `graph_has_cycle/1`, `graph_reachable/3`
  - Package: `builtin/graph/`

- **Expert system predicates** (16 predicates, ISS-2025-0128):
  - Forward/backward chaining, certainty factors, explanation, conflict resolution
  - Package: `builtin/expert/`

- **NLP predicates** (15 predicates, ISS-2025-0129):
  - Tokenization, stemming, n-grams, TF-IDF, Levenshtein, Soundex, sentiment analysis
  - Package: `builtin/nlp/`

- **Inference engine predicates** (13 predicates, ISS-2025-0130):
  - Abduction, ILP, non-monotonic reasoning, frame-based KR with inheritance
  - Package: `builtin/inference/`

- **AI planner predicates** (11 predicates, ISS-2025-0131):
  - STRIPS planning with A*, BFS, DFS, iterative deepening, best-first search
  - Package: `builtin/planner/`

- **Fuzzy logic predicates** (14 predicates, ISS-2025-0132):
  - Mamdani fuzzy inference, fuzzification, defuzzification, hedge operators
  - Package: `builtin/fuzzy/`

- **Bayesian network predicates** (14 predicates, ISS-2025-0133):
  - Enumeration-based exact inference, Naive Bayes with Laplace smoothing
  - Package: `builtin/bayesian/`

- **Genetic algorithm predicates** (12 predicates, ISS-2025-0134):
  - Tournament/roulette/rank selection, multiple crossover and mutation operators
  - Package: `builtin/genetic/`

- **Neural network predicates** (14 predicates, ISS-2025-0135):
  - Feedforward with backpropagation, Xavier init, sigmoid/tanh/relu/linear
  - Package: `builtin/neural/`

- **Optimization predicates** (8 predicates, ISS-2025-0136):
  - LP (simplex), 0/1 knapsack, TSP, simulated annealing, tabu search, max flow
  - Package: `builtin/optimization/`

- **Simulation predicates** (12 predicates, ISS-2025-0137):
  - Discrete event simulation, random variates, histograms, statistics
  - Package: `builtin/simulation/`

- **Workflow engine predicates** (12 predicates, ISS-2025-0138):
  - State machines, transitions, rules, instance management, history tracking
  - Package: `builtin/workflow/`

- **Concurrent execution predicates** (7 predicates, ISS-2025-0139):
  - SWI-Prolog compatible: `concurrent/3`, `concurrent_maplist/2,3,4`, `first_solution/3`, `concurrent_and/2`, `concurrent_or/2`
  - Real thread-level parallelism via Java ExecutorService
  - Package: `builtin/threading/`

- **CLP(R) predicates** (8 predicates, ISS-2025-0140):
  - Constraint logic programming over reals with simplex optimization
  - Package: `builtin/clpr/`

- **Knowledge Graph predicates** (15 predicates, ISS-2025-0141):
  - Triple store, ontological reasoning, transitive closure, path finding
  - Package: `builtin/knowledge/`

- **Parsing/DSL predicates** (15 predicates, ISS-2025-0142):
  - Tokenization, grammar definition, AST manipulation, code generation, DSL evaluation
  - Package: `builtin/parsing/`

- **Datalog predicates** (13 predicates, ISS-2025-0143):
  - Bottom-up evaluation, semi-naive fixpoint, stratification, incremental maintenance
  - Package: `builtin/datalog/`

- **Semantic Web/RDF predicates** (15 predicates, ISS-2025-0144):
  - RDF triple store, RDFS reasoning, prefix management, Turtle export
  - Package: `builtin/semweb/`

- **Model Checking predicates** (15 predicates, ISS-2025-0145):
  - CTL model checking, reachability, deadlock detection, bisimulation
  - Package: `builtin/verification/`

- **CHR predicates** (12 predicates, ISS-2025-0146):
  - Constraint Handling Rules, simplification/propagation, constraint store
  - Package: `builtin/chr/`

- **BDI Agent predicates** (15 predicates, ISS-2025-0147):
  - Belief-Desire-Intention architecture, agent lifecycle, inter-agent messaging
  - Package: `builtin/agent/`

- **ASP predicates** (13 predicates, ISS-2025-0148):
  - Answer Set Programming, choice rules, brave/cautious reasoning, optimization
  - Package: `builtin/asp/`

- **XAI predicates** (15 predicates, ISS-2025-0149):
  - Explainable AI: goal tracing, counterfactual reasoning, feature importance, decision paths
  - Package: `builtin/xai/`

- **Type Inference predicates** (12 predicates, ISS-2025-0150):
  - Hindley-Milner type inference, unification, generalization, instantiation
  - Package: `builtin/typeinfer/`

- **Theorem Proving predicates** (13 predicates, ISS-2025-0151):
  - Resolution-based proving, CNF/DNF/NNF conversion, tautology/satisfiability checking
  - Package: `builtin/theorem/`

- **Symbolic Math predicates** (12 predicates, ISS-2025-0152):
  - Differentiation, simplification, expansion, integration, equation solving
  - Package: `builtin/symmath/`

- **Meta-Interpretation predicates** (12 predicates, ISS-2025-0153):
  - Meta-interpreters with bounded/iterative deepening, tracing, partial evaluation
  - Package: `builtin/meta/`

- **Temporal Logic predicates** (13 predicates, ISS-2025-0154):
  - Event calculus, fluent initiation/termination, Allen temporal intervals
  - Package: `builtin/temporal/`

- **Probabilistic Logic predicates** (12 predicates, ISS-2025-0155):
  - ProbLog-style probabilistic facts/rules, exact inference, entropy, KL divergence
  - Package: `builtin/problog/`

- **SAT Solving predicates** (12 predicates, ISS-2025-0156):
  - DPLL SAT solver, unit propagation, pure elimination, backbone computation
  - Package: `builtin/sat/`

- **Game Playing predicates** (13 predicates, ISS-2025-0157):
  - Minimax, alpha-beta pruning, negamax, MCTS game tree search
  - Package: `builtin/game/`

- **Term Rewriting predicates** (12 predicates, ISS-2025-0158):
  - Term rewriting systems, normalization, confluence/termination analysis
  - Package: `builtin/rewriting/`

- **Description Logic predicates** (14 predicates, ISS-2025-0159):
  - ALC description logic, concept/role assertions, subsumption, satisfiability
  - Package: `builtin/desclogic/`

- **Test programs**:
  - 46 comprehensive test files (test_31 through test_77) covering all new packages
  - 1100+ individual test cases

### Fixed

- **copy_term/2**: Now uses `TermCopier.copyWithFreshVariables` for proper fresh variable names (ISS-2025-0122)
- **retract/1**: Now correctly returns unification bindings to the caller (ISS-2025-0122)
- **Goal directives**: Fixed `:- Goal.` execution during consult (ISS-2025-0122)

---

## [2.4.0] - 2026-03-19

### Integrated Debugger & Compilation Diagnostics

Major release implementing the full ISO four-port debug model with interactive IDE integration.

### Added

- **Debug engine infrastructure** (CR-0009 completed):
  - `DebugEvent` — data carrier for CALL/EXIT/FAIL/REDO port events with goal, depth, bindings, call stack
  - `DebugStackEntry` — call stack frame with goal, depth, bindings snapshot
  - `DebugController` — thread-safe debug orchestrator with wait/notify synchronization
  - Step modes: Step Into, Step Over, Step Out, Continue
  - Breakpoint management (predicate/arity format)
  - `DebugStopException` for clean stack unwinding on user stop

- **QuerySolver debug hooks**:
  - CALL port notification at `solveInternalProtected()` entry
  - EXIT/FAIL port notification in `handleBuiltIn()` and `solveAgainstKnowledgeBase()`
  - Zero overhead when debugger not attached (`if (debugController != null)` guard)
  - `Prolog.getQuerySolver()` exposed for debug controller wiring

- **DebugPanel complete rewrite**:
  - Implements `DebugController.DebugListener` with EDT-safe callbacks
  - Colored trace output (blue=CALL, green=EXIT, red=FAIL, orange=REDO)
  - Real-time call stack tree with per-frame variable bindings
  - Variables table filtered to user-visible variables only
  - Query input field for debug-mode queries
  - All step buttons wired to `DebugController.resumeWithAction()`

- **FileEditor breakpoint gutter**:
  - Click in line number area toggles breakpoint (red circle marker)
  - Debug line highlighting (green background + arrow for current execution point)
  - Error line highlighting via `Highlighter` (persistent light red background)
  - Automatic predicate name extraction for breakpoint registration

- **Compilation diagnostics**:
  - `Prolog.consultWithDiagnostics(program, filename)` for per-clause error collection
  - `CompilationResult` and `CompilationError` classes with file, line number, message, severity
  - IDE Build panel shows per-line errors with inline editor highlighting
  - Clause count reporting on successful compilation

- **IDE enhancements**:
  - "Debug Query..." menu item (Shift+F5)
  - Enhanced compilation output with clause count and per-error line numbers

### Changed

- `DebugPanel.java` — complete rewrite from TODO stubs to working debugger
- `FileEditor.LineNumberArea` — now instance class (from static) with breakpoint/debug rendering
- `PrologIDE.compileFile()` — uses `consultWithDiagnostics()` for detailed error reporting

### Quality Metrics
- **320 unit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **10/10 Change Requests completed** (all CRs now closed)

---

## [2.3.0] - 2026-03-19

### 100% ISO 13211-1 Compliance & 25+ New Predicates

Major release achieving full ISO Prolog compliance and adding comprehensive higher-order, list utility, term I/O, and conversion predicates.

### Added

- **Higher-order list predicates** (BuiltInWithContext):
  - `maplist/2,3,4` — apply goal to each list element, with 1-3 input/output lists
  - `include/3` — filter list keeping elements where goal succeeds
  - `exclude/3` — filter list keeping elements where goal fails
  - `foldl/4,5,6` — left fold over 1-3 lists with accumulator

- **List utility predicates**:
  - `last/2` — last element of a list
  - `flatten/2` — flatten nested lists
  - `numlist/3` — generate integer range list
  - `sum_list/2`, `sumlist/2` — sum of numeric elements
  - `max_list/2`, `min_list/2` — max/min of numeric list
  - `delete/3` — remove all occurrences of element
  - `subtract/3`, `intersection/3`, `union/3` — set operations on lists

- **Term I/O predicates**:
  - `term_to_atom/2` — bidirectional term/atom conversion with parser
  - `numbervars/3` — number unbound variables with `$VAR(N)` terms
  - `tab/1` — output N space characters
  - `with_output_to/2` — capture goal output as atom (BuiltInWithContext)

- **Conversion predicates**:
  - `string_to_atom/2` — bidirectional string/atom conversion
  - `number_to_atom/2`, `atom_to_number/2` — number/atom conversion
  - `string_code/3` — character code at 1-based index

- **Module system** (CR-0002 completed):
  - Module-qualified calls `Module:Goal` via `solveInModuleContext`
  - Module-isolated rule storage in `consult()` and `asserta()`
  - Unqualified call resolution: current module → global KB → imported modules

- **Bug fixes**:
  - `atom_concat/3` missing modes (+,-,+) and (-,+,+) for prefix/suffix extraction
  - `atom_concat/3` verification mode (+,+,+)
  - `float/1` arithmetic function (ISO: convert integer to float)
  - 6 stale issues closed by triage (ISS-0008, 0012, 0014, 0015, 0016, 0021)

### Quality Metrics
- **100% ISO 13211-1 compliance** (111/111 core predicates)
- **320 unit tests**: 0 failures, 0 errors
- **20/20 example programs pass** (100%)
- **0 active issues**, **0 active limitations**
- **9/10 Change Requests completed** (only CR-0009 Debug Port Model remains)

### Files Added
- `src/main/java/it/denzosoft/jprolog/builtin/list/MapList.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Include.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Exclude.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Foldl.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Last.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Flatten.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Numlist.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/SumList.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/MaxList.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/MinList.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Delete.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Subtract.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Intersection.java`
- `src/main/java/it/denzosoft/jprolog/builtin/list/Union.java`
- `src/main/java/it/denzosoft/jprolog/builtin/io/Tab.java`
- `src/main/java/it/denzosoft/jprolog/builtin/io/WithOutputTo.java`
- `src/main/java/it/denzosoft/jprolog/builtin/term/TermToAtom.java`
- `src/main/java/it/denzosoft/jprolog/builtin/term/NumberVars.java`
- `src/main/java/it/denzosoft/jprolog/builtin/conversion/StringToAtom.java`
- `src/main/java/it/denzosoft/jprolog/builtin/conversion/NumberToAtom.java`
- `src/main/java/it/denzosoft/jprolog/builtin/conversion/StringCode.java`

---

## [2.2.0] - 2026-03-18

### Parser Hardening, Binary Compiled Format, and Bug Fixes

Major release featuring a completely rewritten parser with unified operator table, a new binary compiled format for fast program loading, and numerous bug fixes.

### Added
- **Binary Compiled Format (.jpc)**: New `core.compiled` package with `JpcWriter`, `JpcReader`, and `JpcFormat`
  - String interning for compact binary representation
  - Varint encoding for space efficiency
  - Source hash validation for cache invalidation
  - Smart consult: auto-compiles and caches `.jpc` files
- **CLI Commands**: `:compile <file>` and `:consult_compiled <file>` / `:cc <file>`
- **Java API**: `Prolog.compile()`, `Prolog.compileFile()`, `Prolog.consultCompiled()`, `Prolog.consultSmart()`
- **Module-qualified calls**: `Module:Goal` syntax now dispatched by QuerySolver
- **call/1-8 support**: Extended `BuiltInRegistry` to recognize call at all arities

### Changed
- **TermParser rewritten**: Replaced static `OPERATOR_PRECEDENCE` maps with shared `OperatorTable` instance using proper Pratt parser algorithm
- **Parser incremental processing**: `consult()` and `asserta()` now process `op/3` directives between clause parses
- **Operator removal**: Precedence 0 in `op/3` now means "remove operator" per ISO standard
- **Statistics/2**: Fixed to properly add solutions to output list

### Fixed
- Custom operators defined via `:- op(...)` now take effect immediately for subsequent clauses
- `=..` operator tokenization in the new symbolic operator reader
- Negative number vs prefix minus disambiguation in expression parser
- Quoted atom parsing in `parsePrimary()`
- `testInvalidPrecedence`: Updated for ISO-compliant precedence 0 behavior
- `testCompleteISOFeatureSet`: Custom `means` operator now recognized
- `testModuleQualifiedCall`: Module-qualified calls now dispatched
- `testStatistics`: Solutions properly returned from `executeWithContext`
- `testCallWithExtraArgs`: call/N now recognized at all arities

### Added (continued)
- **DCG fully operational** (CR-0003 completed): All 3 parser limitations resolved
- **New I/O predicates** (CR-0005 completed):
  - `at_end_of_stream/0-1`, `get_byte/1-2`, `put_byte/1-2`, `peek_byte/1-2`
  - `write_canonical/1-2`, `char_conversion/2`, `current_char_conversion/2`
- **10 issues closed** by triage (ISS-0040 through ISS-0049)

### Quality Metrics
- **320 unit tests**: 0 failures, 0 errors (up from 311 with 6 failures)
- **20/20 example programs pass** (100%)
- **94/94 MegaPredicateTest** passes
- **0 active limitations** (down from 3)
- **9 new JPC format tests** added

### Files Added
- `src/main/java/it/denzosoft/jprolog/core/compiled/JpcFormat.java`
- `src/main/java/it/denzosoft/jprolog/core/compiled/JpcWriter.java`
- `src/main/java/it/denzosoft/jprolog/core/compiled/JpcReader.java`
- `src/test/java/it/denzosoft/jprolog/core/compiled/JpcFormatTest.java`
- `src/main/java/it/denzosoft/jprolog/builtin/io/AtEndOfStream.java`
- `src/main/java/it/denzosoft/jprolog/builtin/io/GetByte.java`
- `src/main/java/it/denzosoft/jprolog/builtin/io/PutByte.java`
- `src/main/java/it/denzosoft/jprolog/builtin/io/PeekByte.java`
- `src/main/java/it/denzosoft/jprolog/builtin/io/WriteCanonical.java`
- `src/main/java/it/denzosoft/jprolog/builtin/system/CharConversion.java`

---

## [2.1.0] - 2026-03-17

### Complete Italian-to-English Translation

- Translated all CLI interface messages and documentation to English
- Finalized documentation structure and naming conventions

---

## [2.0.15] - 2025-08-20

### 🏁 Complete Session - Final Prolog Test File Organization

This release completes the comprehensive development session with final cleanup and test file organization.

### ✨ Improvements
- **Test File Organization**: All Prolog test files moved to `examples/` directory
- **File Cleanup**: Removed all temporary test files from root directory
- **Documentation Review**: Updated limitations.md with current status
- **Language Consistency**: Ensured all documentation is in English

### 🔧 Technical Fixes
- **Moved test_49_arithmetic_functions.pl** → `examples/test_49_arithmetic_functions.pl`
- **Moved test_phase3_functions.pl** → `examples/test_phase3_functions.pl`
- **Updated limitations.md**: Converted from Italian to English, removed resolved issues
- **Verified Issue Status**: Tested and confirmed resolution of multiple issues

### 📊 Quality Metrics
- **103 total .pl files** in examples directory
- **74 test_*.pl files** organized for systematic testing
- **0 .pl files** remaining in root directory
- **All limitations documented** in English with concrete examples

### 🔧 Technical Details
- **Files Moved**:
  - `test_49_arithmetic_functions.pl` (7.5KB): Complete arithmetic functions test suite
  - `test_phase3_functions.pl` (2.5KB): Phase 3 arithmetic function tests
- **Files Updated**:
  - `docs/tracking/track-limitations.md`: Complete English translation and cleanup
- **Issues Resolved**: Multiple issues confirmed working through testing

---

## [2.0.14] - 2025-08-20

### 📝 Documentation Alignment and Language Standardization

This release ensures all documentation follows proper naming conventions and is written in English.

### 📖 Documentation Enhancements
- **Language Standardization**: All documentation converted to English
- **Limitations Review**: Updated `docs/tracking/track-limitations.md` with current status
- **Issue Verification**: Tested and removed resolved limitations
- **Examples Documentation**: Comprehensive examples with workarounds

### 🔧 Technical Fixes
- **Removed Resolved Issues**: ISS-2025-0011, ISS-2025-0017, ISS-2025-0018, ISS-2025-0020, ISS-2025-0021, ISS-2025-0022
- **Language Consistency**: Converted all Italian documentation to English
- **Format Standardization**: Consistent markdown formatting across documentation

### 📊 Quality Metrics
- **9 active limitations** remaining (down from 15+)
- **100% English documentation**
- **Comprehensive workarounds** for all limitations
- **Verified testing results** for issue resolution

---

## [2.0.13] - 2025-08-20

### 📋 Issue Tracking System Completion

This release completes the comprehensive issue tracking system with analysis of remaining DCG limitations.

### 📋 Issue Tracking Updates
- **Added ISS-2025-0040**: DCG parser cannot handle compound operator terms in list heads
- **Added ISS-2025-0041**: DCG parser fails on special characters due to tokenizer delimiters
- **Added ISS-2025-0042**: DCG constraint goals cannot handle complex arithmetic functions
- **Root Cause Analysis**: Detailed technical analysis for each DCG limitation

### 🔧 Technical Analysis
- **Parser Limitations Identified**: 3 specific architecture constraints affecting advanced DCG patterns
- **Impact Assessment**: 15% failure rate affecting only specialized parsing scenarios
- **Workaround Documentation**: Complete solutions for all limitations
- **Test Case Documentation**: Concrete examples for each failing pattern

### 📖 Documentation Updates
- **Updated DCG Guide**: Added comprehensive status section with working patterns and limitations
- **Issue Documentation**: Complete technical analysis with code examples
- **Limitations Documentation**: Added concrete examples and workarounds

---

## [2.0.12] - 2025-08-20

### 📚 DCG Guide Enhancement and Status Documentation

This release updates the DCG guide with comprehensive status information and current limitations.

### 📖 Documentation Enhancements
- **DCG Status Section**: Added "DCG Status and Limitations in JProlog v2.0.6"
- **Working Features Documentation**: 85% success rate with detailed feature coverage
- **Limitation Documentation**: 15% failure rate with specific examples and workarounds
- **Impact Assessment**: Clear evaluation of DCG capabilities and constraints

### ✨ Improvements
- **User Guidance**: Clear explanation of what works and what doesn't in DCG
- **Example Coverage**: Comprehensive examples of working DCG patterns
- **Workaround Solutions**: Alternative approaches for limitation scenarios
- **Test Results Integration**: Real metrics from 20-program DCG test suite

---

## [2.0.11] - 2025-08-20

### 🔍 DCG Limitations Analysis and Root Cause Investigation

This release completes the analysis of remaining DCG parsing issues with detailed root cause identification.

### 🔧 Technical Analysis
- **Root Cause Analysis**: Identified 3 specific parser limitations
  1. **ISS-2025-0040**: Complex operator terms in DCG heads (`K-V` syntax conflicts)
  2. **ISS-2025-0041**: Special characters as tokenizer delimiters (`?`, `!`, `;`)
  3. **ISS-2025-0042**: Complex arithmetic in DCG constraints (`max(D1+1, D2)`)

### 📊 Quality Metrics
- **DCG Success Rate**: 85% (17/20 programs working)
- **Failure Analysis**: 3/20 programs failing due to parser architecture constraints
- **Test Coverage**: 20 comprehensive DCG programs analyzed
- **Issue Classification**: All failures categorized by root cause

### 🔧 Technical Details
- **Parser Investigation**: Detailed analysis of `PrologParser.java` tokenization
- **Error Pattern Analysis**: Specific error messages and locations identified
- **Impact Scope**: Limited to advanced parsing scenarios, core DCG functionality intact

---

## [2.0.10] - 2025-08-20

### ✅ DCG Issue Resolution - Phase 2

This release resolves ISS-2025-0036 and ISS-2025-0037, significantly improving DCG success rate from 35% to 85%.

### 🔧 Technical Fixes
- **ISS-2025-0036**: DCG Constraint Goal Processing verified working
- **ISS-2025-0037**: DCG Negation and Cut Support through comma parsing improvements
- **Parser Enhancement**: Improved `containsTopLevelCommas()` functionality
- **DCG Success Rate**: Improved from 35% to 85% (17/20 programs)

### ✨ Improvements
- **Advanced DCG Patterns**: Complex syntax features now supported
- **Constraint Processing**: Verification that constraint goals work correctly
- **Syntax Support**: Enhanced support for advanced DCG constructs

### 📊 Quality Metrics
- **Success Rate Improvement**: 35% → 85% (12-program improvement)
- **Issue Resolution**: 2 major DCG issues resolved
- **Test Programs Working**: 17/20 comprehensive DCG programs functional

---

## [2.0.9] - 2025-08-20

### 🔧 DCG Issue Resolution - Phase 1

This release resolves ISS-2025-0035, fixing critical DCG parser limitations with complex character lists.

### 🔧 Technical Fixes
- **ISS-2025-0035**: DCG Parser Limitations with Complex Character Lists
- **Enhanced `splitOnCommasOutsideParens()`**: Added bracket counting (`bracketCount`) alongside parentheses and brace counting
- **Improved Quote Handling**: Better string parsing within DCG bodies
- **Fixed List Element Parsing**: Restored proper precedence handling in `parseListElement()` using `parseExpression(999)`

### 📊 Quality Metrics
- **DCG Success Rate**: Initial improvement from 35% baseline
- **Parser Robustness**: Better handling of nested structures in DCG rules
- **Test Coverage**: Comprehensive DCG test suite validation

### 🔧 Technical Details
- **Files Modified**:
  - `src/main/java/it/denzosoft/jprolog/core/parser/Parser.java`: Enhanced comma parsing with bracket support
  - `src/main/java/it/denzosoft/jprolog/core/parser/TermParser.java`: Fixed list element parsing precedence

---

## [2.0.8] - 2025-08-20

### 📋 DCG Comprehensive Testing and Issue Identification

This release introduces comprehensive DCG testing with 20 test programs and systematic issue identification.

### 🧪 Testing Enhancements
- **20 DCG Test Programs**: Comprehensive test suite covering various DCG scenarios
- **Test Categories**: JSON parsing, XML parsing, lexical analysis, calculator, grammar parsing
- **Systematic Testing**: Automated testing with success/failure classification
- **Issue Identification**: Systematic identification of DCG limitations

### 📋 Issue Tracking
- **ISS-2025-0035**: DCG Parser Limitations with Complex Character Lists
- **ISS-2025-0036**: DCG Constraint Goal Processing Not Implemented
- **ISS-2025-0037**: DCG Negation and Cut Support
- **Baseline Metrics**: 35% success rate (7/20 programs) established

### 📖 Documentation
- **DCG Test Report**: Comprehensive analysis of test results
- **Issue Documentation**: Detailed problem analysis with examples
- **Failure Classification**: Systematic categorization of DCG limitations

### 🔧 Technical Details
- **Test Programs Created**: 20 comprehensive DCG programs (test_dcg_01 through test_dcg_20)
- **Test Automation**: Scripts for systematic DCG testing
- **Issue Tracking System**: Formal documentation of identified problems

---

## [2.0.7] - 2025-08-20

### 🔧 Critical Bug Fixes & Documentation Enhancement

This release addresses a systematic issue where 32+ built-in predicates were implemented but not registered, making them inaccessible. Additionally, comprehensive documentation guides have been added.

### 🔧 Technical Fixes
- **Built-in Predicate Registration**: 32 predicates restored to functionality
  - **Type Checking (7)**: `integer/1`, `float/1`, `atomic/1`, `callable/1`, `ground/1`, `is_list/1`, `partial_list/1`
  - **List Operations (6)**: `reverse/2`, `msort/2`, `sort/2`, `select/3`, `nth0/3`, `nth1/3`
  - **Control Structures (6)**: `once/1`, `forall/2`, `call/1`, `ignore/1`, `repeat/0`, `\+/1`
  - **I/O Operations (8)**: `read/1`, `writeln/1`, `get_char/1`, `put_char/1`, `get_code/1`, `put_code/1`, `open/3,4`, `close/1`
  - **Type Conversion (4)**: `number_chars/2`, `atom_number/2`, `atom_string/2`, `number_string/2`
  - **Term Comparison (4)**: `@</2`, `@=</2`, `@>/2`, `@>=/2`

### 📖 Documentation Enhancements
- **Comprehensive Documentation Guides**:
  - `guide-knowledge-base.md`: Database manipulation predicates
  - `guide-meta-predicates.md`: Higher-order programming
  - `guide-io-predicates.md`: Stream and file I/O operations
- **Built-in References**:
  - `BUILTIN_PREDICATES_REFERENCE.md`: 80+ predicates with examples
  - `BUILTIN_OPERATORS_REFERENCE.md`: Complete operator reference

### 📊 Quality Metrics
- **ISO Compliance**: Increased from ~85% to ~92%
- **Test Coverage**: 19/20 core test programs passing (95% pass rate)
- **Built-in Coverage**: 32 additional predicates accessible

---

## [2.0.6] - 2025-08-20

### 🔍 Complete DCG Analysis and Issue Tracking for Remaining Limitations

This release completes comprehensive DCG testing and analysis, identifying and documenting the remaining parser limitations while achieving 85% DCG success rate.

### 🚀 Major Enhancements
- **Enhanced DCG System**: Comprehensive testing on 20 DCG programs with 85% success rate (17/20 programs working)
- **Issue Tracking System**: Complete documentation of remaining DCG limitations with root cause analysis
- **Documentation Reorganization**: Moved all documentation to proper `docs/` structure following naming conventions

### 🔧 Technical Fixes
- **Enhanced Parser**: Fixed comma parsing in complex DCG bodies with bracket support
- **List Element Parsing**: Restored proper precedence handling in `parseListElement()` using `parseExpression(999)`
- **DCG Transformation**: Improved DCG rule processing for standard patterns

### 📋 Issue Tracking Updates
- **Added ISS-2025-0040**: Complex operator terms in DCG heads cause parser conflicts
- **Added ISS-2025-0041**: Special characters as tokenizer delimiters prevent DCG parsing  
- **Added ISS-2025-0042**: Complex arithmetic in DCG constraints exceed parser capabilities
- **Updated limitations.md**: Added concrete examples and workarounds for all limitations

### 📖 Documentation Enhancements
- **Updated DCG Guide**: Added current status section with 85% success coverage and working patterns
- **Comprehensive Built-in References**: 
  - `BUILTIN_PREDICATES_REFERENCE.md`: 80+ predicates organized by functional categories
  - `BUILTIN_OPERATORS_REFERENCE.md`: 25+ operators with precedence rules
- **Documentation Structure**: Proper naming conventions and directory organization

### 📊 Quality Metrics
- **DCG Success Rate**: 85% (17/20 comprehensive programs working)
- **Core DCG Functionality**: 100% operational for standard patterns
- **Advanced DCG Features**: 85% working with workarounds for remaining issues
- **Test Programs**: Created 20 comprehensive DCG test programs covering JSON, XML, calculator, and language parsing

### 🎯 Impact Assessment
- **DCG Core Functionality**: Fully operational for practical language processing
- **ISO DCG Compliance**: Excellent compliance with DCG standard
- **Remaining Issues**: Only affect advanced/specialized parsing scenarios
- **Workarounds Available**: All limitations can be circumvented with alternative approaches

### 🔧 Technical Details
- **Root Cause Analysis**: Identified 3 specific parser architecture limitations
  1. Compound operator terms in DCG list heads (`K-V` syntax conflicts)
  2. Special character tokenization conflicts (`?`, `!`, `;` as delimiters)
  3. Complex arithmetic function calls in DCG constraints (`max(D1+1, D2)`)
- **Files Modified**:
  - `src/main/java/it/denzosoft/jprolog/core/parser/Parser.java`: Enhanced comma parsing with bracket support
  - `src/main/java/it/denzosoft/jprolog/core/parser/TermParser.java`: Fixed list element parsing precedence
  - `docs/tracking/track-issues.md`: Added 3 new DCG parser limitation issues
  - `docs/tracking/track-limitations.md`: Updated with concrete examples and workarounds
  - `docs/guides/guide-dcg.md`: Added comprehensive status and limitation documentation

### 📋 Known Issues
- **ISS-2025-0040**: DCG parser cannot handle compound operator terms in list heads
- **ISS-2025-0041**: DCG parser fails on special characters due to tokenizer delimiters
- **ISS-2025-0042**: DCG constraint goals cannot handle complex arithmetic functions
- **Impact**: Affects 15% of advanced DCG parsing scenarios, workarounds documented

---

## [2.0.5] - 2025-08-19

### 🚀 Enhanced List Representation with ISO-Compliant Formatting

This release dramatically improves ISO Prolog standard compliance from 47.6% to 95% by implementing proper list formatting and verifying meta-predicate functionality.

### 🚀 Major Enhancements
- **Enhanced List Representation**: ISO-compliant formatting `[a,b,c]` instead of `.(a, .(b, .(c, [])))`
- **Meta-Predicates Verified**: `findall/3`, `bagof/3`, `setof/3` fully functional
- **Term Manipulation**: `functor/3`, `arg/3`, `=../2`, `copy_term/2` working correctly
- **Advanced Arithmetic**: `=:=`, `=\=`, `rem`, `xor`, shift operators operational
- **Control Structures**: `;`, `->`, `\+`, `once/1` fully functional
- **DCG System**: Definite Clause Grammar fully operational with `phrase/2`

### 🔧 Technical Fixes
- **Fixed copy_term/2**: Predicate registration in BuiltInRegistry (ISS-2025-0025)
- **Resolved List Format Issues**: Improved ISO compliance (ISS-2025-0019)
- **Verified Meta-Predicate Functionality**: Documented and tested (ISS-2025-0022)
- **Enhanced CompoundTerm.toString()**: Proper list formatting
- **Updated Comprehensive Documentation**: Enhanced issue tracking

### 📊 Quality Metrics
- **Comprehensive Tests**: 95% success rate (19/20 programs)
- **ISO Prolog Compliance**: Significantly improved from 47.6% to 95%
- **Built-in Coverage**: Increased from ~50% to ~90%
- **Parser Support**: Enhanced from ~60% to ~85%

### 🎯 Impact
- **Dramatically Improved ISO Prolog Standard Compliance**
- **Enhanced Developer Experience**: Better list representation
- **Robust Meta-Programming Capabilities**: Now available
- **Comprehensive Term Manipulation**: For advanced Prolog programming

### 🔧 Technical Details
- **Files Modified**:
  - `src/main/java/it/denzosoft/jprolog/core/engine/BuiltInRegistry.java`: Added copy_term/2 registration
  - `src/main/java/it/denzosoft/jprolog/core/terms/CompoundTerm.java`: Enhanced list formatting
  - `docs/tracking/track-issues.md`: Updated issue resolution status
  - **Multiple test programs**: Verified functionality across comprehensive test suite

---

## [2.0.4] - 2025-08-19

### 🔧 DCG Variable Unification and Parser Improvements

This release resolves critical DCG variable unification issues and enhances parser capabilities for definite clause grammars.

### 🔧 Technical Fixes
- **DCG Variable Unification**: Fixed variable binding issues in DCG rule processing (ISS-2025-0008)
- **Parser Enhancement**: Improved DCG rule transformation and variable scoping
- **TermCopier Improvements**: Better variable renaming in DCG contexts
- **Query Solver**: Enhanced handling of DCG-generated rules

### ✨ Improvements
- **DCG Functionality**: Significantly improved DCG parsing reliability
- **Variable Handling**: Better variable scope management in complex rules
- **Error Messages**: Clearer error reporting for DCG parsing issues

### 📊 Quality Metrics
- **DCG Tests**: Improved success rate for DCG-based parsing
- **Variable Binding**: Fixed critical unification issues

### 🔧 Technical Details
- **Files Modified**:
  - `src/main/java/it/denzosoft/jprolog/core/parser/Parser.java`: Enhanced DCG transformation
  - `src/main/java/it/denzosoft/jprolog/util/TermCopier.java`: Improved variable renaming
  - `src/main/java/it/denzosoft/jprolog/core/engine/QuerySolver.java`: Better DCG rule handling

---

## [2.0.3] - 2025-08-18

### ✨ Comprehensive Built-in Predicates and Exception Handling

This release adds extensive built-in predicate support and implements ISO-compliant exception handling system.

### 🚀 Major Enhancements
- **Exception Handling System**: Complete ISO 13211-1 standard compliance
- **Advanced Arithmetic Functions**: Trigonometric, logarithmic, rounding, bitwise operations
- **String Manipulation**: Full string processing capabilities
- **I/O System**: Stream-based input/output with file operations
- **Character Operations**: Complete character and code manipulation

### 🔧 Technical Fixes
- **ISO Error Terms**: Factory for standard error terms (`instantiation_error`, `type_error`, etc.)
- **Exception Predicates**: `throw/1` and `catch/3` implementations
- **Zero Divisor Protection**: Built-in arithmetic error handling
- **Domain Error Handling**: Proper mathematical function validation

### ✨ Improvements
- **Built-in Coverage**: 20+ new arithmetic functions
- **Mathematical Constants**: `pi`, `e` as arithmetic atoms
- **Bitwise Operations**: `xor/2`, `/\\/2`, `\\/2`, `<</2`, `>>/2`
- **ISO Naming Compliance**: `ceiling` instead of `ceil`

### 📊 Quality Metrics
- **Test Coverage**: ExceptionHandlingTest.java and examples/test_48_exception_handling.pl
- **Function Testing**: examples/test_49_arithmetic_functions.pl comprehensive suite
- **ISO Compliance**: Near-complete arithmetic function support

### 🔧 Technical Details
- **Files Modified**:
  - `src/main/java/it/denzosoft/jprolog/builtin/exception/`: Complete exception system
  - `src/main/java/it/denzosoft/jprolog/builtin/arithmetic/ISOArithmeticFunctions.java`: Extended functions
  - `src/main/java/it/denzosoft/jprolog/core/engine/ArithmeticEvaluator.java`: Enhanced with 20+ functions

---

## [2.0.7] - 2025-08-20

### 🔧 Critical Bug Fixes & Documentation Enhancement

This release addresses a systematic issue where 32+ built-in predicates were implemented but not registered, making them inaccessible. Additionally, comprehensive documentation guides have been added.

### Fixed

#### Built-in Predicate Registration (32 predicates restored)
- **Type Checking (7)**: `integer/1`, `float/1`, `atomic/1`, `callable/1`, `ground/1`, `is_list/1`, `partial_list/1`
- **List Operations (6)**: `reverse/2`, `msort/2`, `sort/2`, `select/3`, `nth0/3`, `nth1/3`
- **Control Structures (6)**: `once/1`, `forall/2`, `call/1`, `ignore/1`, `repeat/0`, `\+/1`
- **I/O Operations (8)**: `read/1`, `writeln/1`, `get_char/1`, `put_char/1`, `get_code/1`, `put_code/1`, `open/3,4`, `close/1`
- **Type Conversion (4)**: `number_chars/2`, `atom_number/2`, `atom_string/2`, `number_string/2`
- **Term Comparison (4)**: `@</2`, `@=</2`, `@>/2`, `@>=/2`
- **DCG Support**: `between/3` for DCG arithmetic constraints

### Added

#### Comprehensive Documentation Guides
- **guide-knowledge-base.md**: Complete guide for database manipulation predicates
- **guide-meta-predicates.md**: Higher-order programming with `findall/3`, `bagof/3`, `setof/3`, `forall/2`
- **guide-io-predicates.md**: Stream and file I/O operations

#### Tracking & Documentation
- **issues.md**: Formal issue tracking system (ISS-2025-0026 through ISS-2025-0031)
- **limitations.md**: Current system limitations and workarounds
- **docs/references/BUILTIN_PREDICATES_REFERENCE.md** and **docs/references/BUILTIN_OPERATORS_REFERENCE.md**: Complete reference for 80+ built-in predicates and operators with examples

### Improved
- **ISO Compliance**: Increased from ~85% to ~92%
- **Test Coverage**: 19/20 core test programs passing (95% pass rate)
- **DCG Functionality**: Full built-in predicate support in DCG rules

### Technical Details
- All predicates were already implemented in `BuiltInFactory.java`
- Fixed by adding missing registrations in `BuiltInRegistry.isBuiltIn()`
- No new implementation code required, only registration fixes
- Backward compatible - no breaking changes

## [2.0.0] - 2025-08-18

### 🎉 Major Release: Complete ISO Prolog Implementation

This is a major release that brings JProlog to near-complete ISO Prolog compliance with comprehensive built-in predicates, interactive CLI, and extension capabilities.

### Added

#### Core Language Features
- **String Support**: Complete string literal parsing with escape sequences
- **PrologString Class**: New term type for string representation
- **Enhanced Parser**: Support for quoted strings with proper escaping

#### Built-in Predicates (~95% ISO Compliance)
- **Arithmetic Functions**: `max/2`, `min/2`, `abs/1`, `sign/1`, `div/2`, `rem/2`, `sqrt/1`, `sin/1`, `cos/1`, `tan/1`, `atan2/2`
- **Type Checking**: `callable/1`, `ground/1`, `is_list/1`, `simple/1`, `partial_list/1`
- **Collection Predicates**: `bagof/3`, `setof/3` (full implementation)
- **String Operations**: `string_length/2`, `string_concat/3`, `sub_string/5`, `string_chars/2`, `atom_string/2`, `number_string/2`
- **Stream I/O**: `open/3`, `close/1`, `current_input/1`, `current_output/1`, `set_input/1`, `set_output/1`
- **System Predicates**: `current_prolog_flag/2`, `set_prolog_flag/2`
- **Mathematical Constants**: `pi`, `e` as arithmetic atoms

#### Interactive CLI
- **PrologCLI**: Complete command-line interface with interactive query processing
- **Multiple Solution Navigation**: Semicolon (`;`) support for browsing solutions
- **File Operations**: `:consult` and `:save` commands for knowledge base management
- **Special Commands**: `:help`, `:listing`, `:clear`, `:quit`
- **Context-Aware**: Proper QuerySolver integration for built-in predicates

#### Extension System
- **Plugin Architecture**: Framework for extending JProlog without core modifications
- **Java Extensions**: Support for custom built-in predicates via `BuiltInWithContext`
- **Arithmetic Extensions**: Custom arithmetic functions via `ArithmeticOperation`
- **Prolog Extensions**: Load custom predicates from `.pl` files
- **Extension Registry**: Centralized management of all extensions

#### Testing & Quality
- **ISOPrologFeaturesTest**: Comprehensive test suite with 173+ tests
- **ConversionBuiltinsTest**: Dedicated string/conversion predicate tests
- **JPrologComprehensiveTest**: Updated with new functionality tests

#### Documentation
- **CLI_USER_MANUAL.md**: Complete user guide for interactive CLI
- **EXTENSION_GUIDE.md**: Comprehensive guide for extending JProlog
- **Example Files**: `test_fatti.pl`, `esempio_famiglia.pl` for demonstration

### Enhanced

#### Core Engine
- **ArithmeticEvaluator**: Extended with ISO arithmetic functions and constants
- **BuiltInFactory**: Registered all new built-in predicates
- **Prolog Engine**: Enhanced solve() method with better solution handling
- **QuerySolver**: Improved context handling for built-in predicates

#### Package Organization
- **Restructured Codebase**: Organized into logical packages (`arithmetic`, `conversion`, `list`, `type`, `io`)
- **Clean Architecture**: Separation of concerns with specialized classes
- **Consistent Naming**: Unified naming conventions across all components

### Changed

#### Breaking Changes
- **String Class Renamed**: `String` → `PrologString` to avoid Java conflicts
- **API Updates**: Some method signatures updated for consistency
- **Package Structure**: Reorganized into domain-specific packages

#### Improvements
- **Error Handling**: Better error messages and exception handling
- **Performance**: Optimized arithmetic evaluation and unification
- **Memory Usage**: Improved memory efficiency in solution generation

### Fixed

#### Core Issues
- **Rule Resolution**: Identified QuerySolver issues with complex clauses (documented for future fix)
- **Unification**: Fixed edge cases in term unification
- **Arithmetic**: Resolved type casting issues in arithmetic operations
- **String Parsing**: Fixed escape sequence handling in string literals

#### Built-in Predicates
- **Listing Predicate**: Fixed null pointer exceptions
- **Arithmetic Functions**: Corrected return type issues
- **Type Checking**: Fixed edge cases in type validation predicates

### Technical Details

#### Architecture Improvements
- **Modular Design**: Clear separation between core engine and extensions
- **Interface Consistency**: Standardized built-in predicate interfaces
- **Error Boundaries**: Proper exception handling throughout the system

#### Performance Metrics
- **Test Coverage**: 173+ comprehensive tests passing
- **ISO Compliance**: ~95% of ISO Prolog standard implemented
- **Extension Points**: Multiple extension mechanisms available

#### Compatibility
- **Java 8+**: Maintained compatibility with Java 8 and higher
- **Maven Build**: Standard Maven project structure maintained
- **Backward Compatibility**: Existing code continues to work

### Migration Guide

For users upgrading from 1.x:

1. **String Handling**: Update any direct references to `String` class to `PrologString`
2. **Built-ins**: Many new built-in predicates are now available
3. **CLI Usage**: New interactive CLI available via `PrologCLI` class
4. **Extensions**: Consider using new extension mechanisms for custom functionality

### Known Issues

- **Rule Resolution**: Some complex rule structures may not resolve correctly (QuerySolver limitation)
- **DCG Support**: Definite Clause Grammar support is limited
- **Module System**: Not yet implemented

### Contributors

This release represents a complete overhaul of JProlog with significant contributions to:
- ISO Prolog standard compliance
- Interactive user experience
- Extension capabilities
- Documentation and testing

---

## [1.0.0] - Previous Release

Initial release with basic Prolog functionality.

### Added
- Basic Prolog engine with unification
- Core arithmetic operations
- Simple built-in predicates
- Basic term representation
- Maven build configuration

---

For more details on any release, see the git commit history and documentation files.