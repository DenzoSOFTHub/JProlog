# Engine v4 — implementation progress and handoff

**Date**: 2026-08-26 · **Version**: 4.1.0 · **Design**:
`docs/reports/report-engine-v4-design-2026-08-25.md` (part B) ·
**Background**: `docs/reports/report-engine-deep-analysis-2026-08-24.md`

**Status**: **ALL NINE WAVES ARE DONE.** W1 (foundations), W2 (clause store), W3 (native library &
meta-calls), W4 (coroutining), W5 (tabling), W6 (modules & prelude), W7 (engine state: streams,
operators, writer), W8 (default switch, threads, debugger) and **W9 (retirement)** are implemented.
**v4 is the DEFAULT engine since v4.0.0 and the recursive `QuerySolver` is deleted.**
**Section 16 is the 4.1 wave A record** (the v2 machine is deleted) and the starting point for
wave B; section 15 is W9; section 14 is W8; section 13 is W7; section 12 is W6; section 11 is W5;
section 10 is W4; section 9 is W3; sections 1–8 describe W1/W2 and are still accurate except where
sections 9 to 16 say otherwise.

**Since 4.1.0 there is ONE engine.** The v2 `MachineSolver` stayed selectable for one release
(`-Djprolog.engine=v2`), as design decision 1 (B.17) required; wave A of 4.1 deletes it, with the
`engine-v2` profile, the engine-selection API and `core.engine.Trail`. Anywhere below that says
"on both engines" or "the v2 fallback", read it as history.

**Suite**: 1196/1196 JUnit tests, one engine, one leg; 20/20 example programs.

**Post-review**: an independent verification of W1/W2 found two v4-only regressions, both fixed
before W3 — `findall/3` was not opaque (**ISS-2025-0448**) and retract/assert loops were superlinear
(**ISS-2025-0449**). See section 8.

---

## 1. How to run it

```bash
# one query / one program — v4 is what you get
java -cp target/classes it.denzosoft.jprolog.PrologCLI
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE

# the whole suite on the DEFAULT engine (v4)
mvn -o test

# the whole suite on the v2 fallback (the second CI leg while v2 is still selectable)
mvn -o test -Pengine-v2
mvn -o test -DargLine="-Djprolog.engine=v2"          # equivalent one-off form

# just the v4 acceptance tests (they select v4 themselves, so the engine flag is irrelevant)
mvn -o test -Dtest=EngineV4Test

# the 20 example programs on the v2 fallback
JAVA_TOOL_OPTIONS=-Djprolog.engine=v2 ./test_all_examples.sh
```

From Java:

```java
Prolog p = new Prolog();          // one v4 Engine per Prolog, created on the first query
p.solve("...");                   // solve(String), solve(Term) and solveStream all route to v4
Prolog.setUseV4Engine(false);     // ... and this drops to the v2 MachineSolver, at runtime
```

Only the literal property value `v2` selects an older engine — `-Djprolog.engine=legacy`, `=v1`,
`=off` or anything else is still v4 (ISS-2025-0484). There is ONE flag now:
`Prolog.setUseV2Engine(b)` is the inverse of `setUseV4Engine(b)`, and `isUsingV2Engine()` is
"not v4".

---

## 2. File map

### New — `src/main/java/it/denzosoft/jprolog/core/engine/v4/`

| File | Lines | Design | What it is |
|---|---:|---|---|
| `Machine.java` | 1455 | B.6 | the drive loop: goal stack, choice points, cut, catch/throw, findall, cleanup frames, native control constructs, ports, the database operations, query normalisation and the answer snapshot |
| `Unify.java` | 598 | B.4 | every term walker: `unify`, `==`, standard order, `resolve`, `copy_term`, `term_variables`, `ground`, `numbervars`, `subsumes_term`, `cyclic_term`, `occurs` — iterative, cycle-safe, guard-polling |
| `ClauseStore.java` | 400 | B.7 | per-predicate `Predicate` entries, birth/death generations, incremental first-argument index, compaction, sync with `KnowledgeBase` |
| `Clause.java` | 316 | B.2 | the compiled clause skeleton: `compile`, `instantiate`, `unifyHead`, `toTerm`, index keys |
| `LegacyBuiltinAdapter.java` | 211 | B.5 | runs the ~400 existing `BuiltIn` classes on v4 |
| `NativeBuiltins.java` | 143 | B.4/B.5 | the built-ins that must be (or clearly should be) native: `setarg/3`, `nb_setarg/3`, `cyclic_term/1`, `acyclic_term/1`, `term_variables/2`, `ground/1`, `numbervars/3`, `subsumes_term/2`, `compare/3` |
| `Bindings.java` | 100 | B.3 | the trail + conditional trailing (there is no binding store) |
| `SolverFacade.java` | 88 | B.5 | the `QuerySolver`-shaped view `BuiltInWithContext` classes receive |
| `Engine.java` | 56 | B.13 | the per-`Prolog` v4 context |
| `Errors.java` | 52 | B.6 | ISO error construction with the machine's context |
| `VarRef.java` | 40 | B.2 | the numbered-variable placeholder inside a skeleton |
| `Builtin.java` | 39 | B.5 | the v4 built-in SPI (`Outcome` = SUCCESS / FAILURE / SUSPENDED) |
| `BuiltinTable.java` | 38 | B.5 | `(name, arity)` -> entry |
| `Generator.java` | 21 | B.5 | the lazy nondeterministic SPI |

Added by the later waves (the table above is the W1/W2 snapshot): `NativeControl.java`,
`NativeLibrary.java`, `Prelude.java`, `Lambdas.java` (W3, section 9); `Coroutining.java`
(W4, section 10); **`Tabling.java`** (W5, section 11 — the variant answer store plus
`TableFrame`, the generator/consumer choice point of a tabled call).

### New — tests

- `src/test/java/it/denzosoft/jprolog/core/engine/v4/EngineV4Test.java` (531 lines, 35 tests).
  Every test selects v4 in `setUp` and restores the previous selection in `tearDown`, so the class
  behaves identically whether the suite runs on the default engine or under `-Pengine-v4`. Sizes
  are chosen to pass under the surefire fork's default JVM settings (no `-Xmx`, no `-Xss`).

### Changed — outside the v4 package

| File | ISS | Change |
|---|---|---|
| `core/terms/Variable.java` | 0438 | the cell model: `ref`, `serial`, `currentSerial()`, no-arg constructor, lazy `_G<serial>` name, **identity** `equals`/`hashCode`, name-comparison guard inside the legacy `unify(Term, Map)`, `copy()` of a named variable returns `this` |
| `core/engine/Rule.java` | 0439 | `getCompiled()`/`setCompiled()` — the compiled-skeleton cache |
| `core/engine/KnowledgeBase.java` | 0445 | `getPredicateVersion(functor, arity)` — the re-sync signal |
| `core/engine/Prolog.java` | 0444, 0447 | `USE_V4_ENGINE`, `setUseV4Engine`, `isUsingV4Engine`, `getV4Engine()`, `solveWithV4Engine`, `solveStreamWithV4Engine`, v4 routing in `solve(String)`/`solve(Term)`/`solveStream`; `compile()` stamps `Rule.sourceLine` |
| `core/system/PrologFlags.java` | 0441 | `occurs_check` accepts `error` (ISO 7.11.2.4) and the stored atom is no longer overwritten by the boolean |
| `core/parser/Parser.java` | 0447 | `extractClauses` records each clause's start line; new `getLastClauseLines()` |
| `core/compiled/JpcFormat/JpcWriter/JpcReader.java` | 0447 | format 0x03: variables by clause index (+ name), per-clause source line, one `Variable` per index on read |
| `pom.xml` | 0444 | version 3.9.0; `engine-v4` and `engine-legacy` profiles |
| `test/builtin/BugFixVerificationTest.java` | 0441, 0442 | engine-aware branches for the two deliberate v4 behaviour changes (`testISS0397_RealCyclicTermProtectionUntouched`, `testISS0273_setupCallCleanup`) |

---

## 3. Invariants a later wave must not break

1. **A cell is bound in place, and the trail is conditional.** `Bindings.bind` trails a cell only
   when `forceTrail > 0` (an explicit mark/undo extent: `findall/3`, `\=`/2, the catcher
   unification, `\+`) or when the cell is older than the newest choice point
   (`serial <= barrierSerial`). Any new code that takes a mark and later undoes to it **must**
   bracket itself with `forceTrail++` / `forceTrail--`, or its bindings will silently not be undone
   when no choice point happens to exist. `pushCP`/`popCP` are the only places that may touch
   `barrierSerial`. **And it must run its `undo` BEFORE decrementing `forceTrail`**: `cut()` and the
   trust-me pop both end in `Bindings.clearIfUnreachable()`, which drops the whole trail the moment
   `forceTrail` is 0 and no choice point is left — closing the extent first therefore throws away
   exactly the entries the undo needs. That is ISS-2025-0448, and it is the single easiest way to
   break opacity in this engine.
2. **Trust-me pop + `clearIfUnreachable`** are what keep a deterministic recursion at O(1) memory.
   A generator that has handed out its last alternative must set `cp.genExhausted`; a frame that
   still owes a Redo/Fail port (`cp.traceGoal != null`) must **not** be popped.
3. **`EXHAUSTED` is the "no more alternatives" sentinel, never `null`.** `null` is a perfectly good
   goal stack — it is what an empty continuation looks like when the generator is the query's last
   goal. (This exact bug was ISS-2025-0432 on the v2 engine.)
4. **A `VarRef` never escapes the `core.engine.v4` package.** It exists only inside skeletons;
   `Clause.instantiate` replaces it before the term reaches the machine, a built-in or the user.
5. **Clause skeleton variables are numbered by NAME, not by object.** On the paths that build a
   `Rule` (the legacy parser, `JpcReader`, `assert` of a term read from a stream) two occurrences of
   `X` can be two objects, and the name is the identity there.
6. **The query term is normalised** (`Machine.normalise`) so all occurrences of a variable name
   share one cell. Without it, identity variables would make `X = 1, X = 2` succeed. Any new entry
   point that hands a term to the machine must normalise it (`runSubQuery` already does).
7. **`ClauseStore.Predicate.rawArray()` is valid only up to the `size()` captured with it.**
   `assertz` appends in place; every operation that would disturb the first `size` entries
   (`asserta`, compaction, a re-sync) installs a *new* array instead. That pair is the logical
   update view, at zero copying cost. **Compaction is safe at any time** for exactly that reason —
   it installs a new array and never touches a clause's `birth`/`death`, so an existing capture is
   unaffected (ISS-2025-0449; the earlier "query boundary only" rule was over-conservative and made
   retract/assert loops quadratic).
8. **`KnowledgeBase` stays the database of record.** v4 writes to both stores in the same step and
   records the KB's per-predicate version so its own writes trigger no rebuild; a write by any other
   route bumps that version and the store re-syncs that predicate on its next lookup. Never let the
   two diverge — `listing/1`, the IDE and `Prolog.getPredicateIndicatorAtLine` all read the KB.
9. **The trust model.** `InferenceLimitException`, `QueryCancelledException` and
   `DebugStopException` are plain `RuntimeException`s and must never become a `PrologException`.
   Every broad catch in the v4 package starts with `ControlFlow.rethrowIfControl`.
10. **The IDE debugger contract.** Four ports for user predicates, bridged built-ins and v4 natives;
    Redo/Fail are emitted from the choice point via `traceGoal`/`traceDepth`, so a new choice-point
    kind representing a traced goal must carry them. The v2 and v4 trace outputs are currently
    byte-identical — that is a useful regression oracle, keep it.
11. **Output discipline**: everything the machine prints goes through `StreamManager.out()`.

---

## 4. Measured (v2 vs v4, same session, loaded VM, best-of-3 warm)

Harness: `scratchpad/bench/V4Bench.java` (modes `loop`, `loop2`, `nrev`, `cyclic`, `lookup`,
`asserts`, `bigterm`, `misc`); compile with `javac -cp target/classes -d <dir> V4Bench.java`, run
with `java -Xmx... -cp target/classes:<dir> V4Bench <mode> <v2|v4> [n]`.

| Benchmark | v2 | v4 | design target |
|---|---|---|---|
| `nrev` 30 x 2000 | 3139 ms (~316 KLIPS) | **494 ms (~2008 KLIPS)** | >= 2 MLIPS OK |
| `loop(10000000)`, `-Xmx64m` | OutOfMemoryError after 5.2 s | **9.4 s, 4 MB used** | <= 64 MB OK (<= 8 s missed, loaded VM) |
| `loop2(1000000)`, `-Xmx64m` | OutOfMemoryError after 4.4 s | **1.6 s** | <= 64 MB, <= 3 s OK |
| `X = f(X), Y = f(Y), X = Y` | hangs (uncancellable) | **59 ms, succeeds** | < 1 ms (first-call JIT) |
| `X = [1\|X], Y = [1\|Y], X = Y` | hangs | **1 ms, succeeds** | OK |
| 20 000-clause lookup, 1st arg bound, last clause | 34 ns net | **259 ns net** | <= 1 us OK |
| 20 000-clause lookup, 1st arg bound, first clause | 304 ns net | **118 ns net** | <= 1 us OK |
| deterministic loop iteration (same harness) | 2538 ns | **763 ns** | — |
| 100 000 interleaved `assertz` + call | 7281 ms | **1039 ms** | <= 1 s OK |
| `cnt(10k/25k/50k/100k)` (retract + assertz per iteration) | 1257 / 978 / 1089 / 1387 ms | **448 / 341 / 343 / 558 ms** | linear OK |
| `between(1, 10^7, X), X >= 10^7` | 2171 ms | **1738 ms** | <= 1 s missed |
| 1 M-element list: `length` | 1450 ms | 2063 ms | <= 0.5 s missed |
| 1 M-element list: `msort` | 563 ms | 1208 ms | <= 0.5 s missed |
| 1 M-element list: `copy_term` | 1397 ms | 2244 ms | <= 0.5 s missed |
| 1 M-element list: `==` | 238 ms | 508 ms | <= 0.5 s OK |
| 1 M-element list: `findall` + `member` | 2076 ms | 2538 ms | — |

The 1 M-element list rows are the one place v4 is currently **slower** than v2 (1.3-2x). The cause
is the legacy bridge, not the core: for every bridged built-in call v4 dereferences the goal
(`Unify.resolve`) *and* indexes its unbound cells by name (`LegacyBuiltinAdapter.collectCells`), and
on a goal holding a million-element list both walks are O(list). The second walk is already skipped
when the built-in returns only empty maps. The real fix is wave W3, which makes `length/2`,
`member/2`, `msort/2`, `copy_term/2` and friends native `Generator`s that never see a
`Map<String,Term>` at all.

---

## 5. Deviations from the design, and why

1. **`SolverFacade` overrides only `solveMeta`, `solve(Term)` and `getResourceGuard`.** The design
   (B.5) wants the whole `QuerySolver` surface on top of `runSubQuery`. The four-argument
   `solve(goal, bindings, solutions, cutStatus)` — the one 15 built-ins call directly for `phrase`,
   tabling, threads and persistence — is deliberately left inherited, i.e. still the recursive
   algorithm, so that path stays byte-for-byte identical to the v2 engine during W1/W2. Replacing it
   is the core of W3.
2. **[REMOVED IN W4 — see section 10.]** ~~A bounded name->cell index~~ (`Machine.indexCells`/`cellFor`) existed as a compatibility shim.
   A few legacy built-ins report a binding for a variable their goal never mentions — the CLP(FD) v2
   bridge's `exportSingletons` is the live example (`C in 1..3, D #= C*2+1, label([C])` reports `D`).
   The v3.8.0 engine installed such entries because its whole binding store was name-keyed. The index
   holds the query's own variables permanently and bridged goal cells up to a 4096-entry cap (dropped
   wholesale past it), so it can never become the unbounded map v4 exists to remove. It disappears
   with W3 (native built-ins) and W4 (CLP(FD) on the native attribute hook).
3. **[SUPERSEDED BY W4 — see section 10.]** ~~Coroutining still goes through the legacy attribute hook~~ (`Variable.getAttributeUnifyHook`),
   with `freeze/2` handled natively on the machine's wake queue as on v2. The hook is given a
   name-keyed **view** of the currently bound cells reachable from the attribute terms, so `dif/2`
   and `when/2` behave exactly as on v2 — bugs included (ISS-2025-0336). B.9's real wake queue is W4.
4. **Fast paths are disabled while debugging**, as on v2: `=/2`, `is/2`, the comparisons and the type
   checks are routed through the registry bridge so they trace. B.6 wants them kept and the ports
   emitted natively; that is a W8 item, and the current arrangement is what makes the v2 and v4 trace
   output identical (a valuable oracle for W3-W7).
5. **`retractall/1`, `abolish/1,2`, `clause/2`, `listing/0,1`, `predicate_property/2`, `dynamic/1`**
   remain registry built-ins over the `KnowledgeBase` rather than being rewritten on `ClauseStore`.
   They are shared with the legacy engines and the IDE, and the store observes them correctly through
   the version re-sync path. Rewriting them belongs to W3.
6. **`Unify.resolve`/`copy` cap NON-last-argument recursion at 2000 levels** and return the sub-term
   unflattened past that. The last-argument spine (where lists and right-nested structures grow) is
   iterative and cycle-checked with Brent's algorithm, so this cap is only reachable by a term nested
   2000 deep through a non-last argument. It is a StackOverflow backstop, not a semantic limit.
7. **`Prolog.compile()` stamps source lines from the LEGACY parser's clause splitter.** The `.jpc`
   format carries the line; the accuracy of the value is only as good as `Parser.extractClauses`
   (recorded as LIM-036). `consultWithDiagnostics` on the v2-parser path remains the accurate source.

---

## 6. Known gaps on v4 (all recorded as LIM-037)

> **Superseded by wave W3 (section 9)**: every bullet below except the coroutining and
> modules/streams ones is now fixed. Read section 9.4 for what deliberately still differs.


- `phrase/2,3`, tabling and threads still run on the recursive legacy solver via the inherited
  `SolverFacade.solve(...)`, so they keep the 2 000-deep recursion cap (design limits L-02, L-03).
- The library built-ins are still eager (`member/2`, `append/3`, `nth0/nth1`, `sub_atom/5`,
  `clause/2`, `current_op/3`, `maplist/3..5` with an output list — L-08, LIM-030).
- Coroutining semantics are v2's, including the lost bindings of a `when/2`-woken goal (L-05).
- Modules, streams, the operator store and answer printing are v2's (L-06, L-07, L-09, L-11).
- `library(yall)` lambdas are still unsupported (`maplist([X,Y]>>(Y is X*2), ...)` ->
  `existence_error(>>/4)`), as on v2.
- 1 M-element list operations are 1.3-2x slower than v2 (section 4).

---

## 7. Where wave W3 starts

> **W3 is done (v3.10.0)** — see section 9 for what was implemented, how it deviates from the plan
> below, and where **W4** starts. This section is kept as the historical brief.


W3 is "native library & meta-calls" (design B.16). In dependency order:

1. **`SolverFacade.solve(Term, Map, List, CutStatus)`** — override it to run on
   `Machine.runSubQuery` instead of inheriting the recursive algorithm. That single override moves
   `phrase/2,3`, the tabling driver, `with_output_to/2`, `format/2,3` sub-goals and the persistence
   transactions off the 2 000-deep Java recursion. Do it first and re-run the suite on
   `-Pengine-v4`: it is the highest-value, lowest-code change of the wave, and the suite plus the
   byte-identical example output are a good oracle.
   *Watch out for*: cut semantics. The recursive solver reports cut through the `CutStatus` object;
   `runSubQuery` gives a goal-local cut. Built-ins that pass a caller `CutStatus` through
   (`IfThenElse`, `Disjunction`) must keep working — check `QuerySolver.getCurrentCutStatus()`
   callers before switching them.
2. **Native `Generator`s** for `member/2`, `append/3`, `select/3`, `nth0/3`, `nth1/3`, `length/2`
   (the deterministic modes; the enumeration mode is already native), `clause/2`, `sub_atom/5`,
   `current_op/3`. Register them in `NativeBuiltins.register`; the SPI (`Builtin`, `Generator`,
   `Machine.pushGenerator`) is already in place and used by `setarg/3` and friends. This also fixes
   the 1 M-element list slowdown of section 4, because those goals stop being resolved and indexed
   by the adapter.
3. **Native control built-ins**: `bagof/3`, `setof/3`, `aggregate_all/3`, `with_output_to/2`,
   `format/2,3` with `~@`. `Machine.findAll`, `runOnce` and `runSubQuery` are the primitives; the
   CATCH and CLEANUP frame kinds are already there.
4. **`maplist/2..5`, `foldl/4..6`, `include/3`, `exclude/3`, `partition/4` as prelude Prolog**
   (design decision 4, approved) plus **yall lambdas** in `call/N`. Note the v3.7.0 finding recorded
   under ISS-2025-0431: loading library clauses requires unregistering the corresponding built-ins,
   which changes legacy-engine behaviour — on v4 that is now safe to do per engine, because
   `BuiltinTable` is keyed by `(name, arity)` and the v4 machine consults it before the registry.
5. **Remove the compatibility shim** of deviation 2 once the CLP(FD) built-ins no longer report
   bindings by name (that part lands with W4).

Acceptance for W3 (design B.16): DCG over 1 M tokens; the budget/interrupt matrix at 100 %;
`maplist(p, L, L2)` linear; `DebuggingTest` and `DCGTranslatorTest` green on v4; and, as always,
the full suite green on **both** engines plus 20/20 example programs.

---

## 8. Post-review fixes (ISS-2025-0448, ISS-2025-0449)

Two v4-only regressions found by the independent verification of W1/W2, both fixed in the same
working tree. The v2 engine was correct in both cases, which made it the oracle.

### ISS-2025-0448 — `findall/3` was not opaque

`findall(X, member(X,[1,2]), L), var(X)` failed and `..., X == 2` succeeded: the template variable
kept the binding of the last solution.

The nested drive *was* correctly bracketed by a forced-trail extent. The bug was the order in the
`finally`: `B.forceTrail--` ran before `cutTo(floor); B.undo(m)`, and `cut()` ends in
`Bindings.clearIfUnreachable()`, which drops the entire trail as soon as `forceTrail` is 0 and no
choice point is left — exactly the state at the end of a findall whose goal is exhausted. The undo
then found an empty trail.

Every construct that undoes to a mark while the choice-point stack may be empty had the same
ordering. Fixed at all six: `Machine.findAll`, `Machine.runSubQuery` (and therefore
`bagof`/`setof`/`aggregate_all` and every meta-call routed through `SolverFacade`), the `\=/2`
inline built-in, the `catch/3` catcher unification on a non-matching catcher, and `Unify.subsumes`.
`Bindings.undo` now also treats a mark at or above the current top as a no-op — after a clear the
old code would set `top = m` and resurrect freed slots.

The rule is now invariant 1 in section 3, and it is stated as a contract on
`Bindings.clearIfUnreachable`.

### ISS-2025-0449 — retract/assert loops were superlinear

`cnt(N) :- retract(counter(C)), C1 is C+1, assertz(counter(C1)), N1 is N-1, cnt(N1).` cost 60 s at
N = 100000 on v4 against ~5 s on v2, because dead clauses were only compacted at the query boundary:
the predicate's array grew by one dead clause per iteration and both the retract candidate scan and
the clause-iterator scan are O(#clauses).

The query-boundary restriction was over-conservative. Compaction installs a **new** array and never
touches a clause's `birth`/`death`, so a call that already captured `(array, size, generation)`
keeps walking its own array with the clause objects' own generation interval — nothing it can
observe changes, and the logical update view holds whenever we compact. `ClauseStore.retractClause`
now compacts in place once the dead clauses outnumber the live ones and there are at least 32 of
them (`COMPACT_MIN_DEAD`), i.e. amortised O(1); the query-boundary sweep stays as a catch-all.

Measured on the same loaded VM in the same session: `cnt(10k/25k/50k/100k)` = 448 / 341 / 343 /
558 ms on v4 (linear) against 1257 / 978 / 1089 / 1387 ms on v2. The physical clause count of
`counter/1` peaks at 33 during the loop (5001 without the fix) and is 1 afterwards.

### Tests added

| Test | Guards |
|---|---|
| `testISS0448_FindallIsOpaque` | the three reported repros, nested findall, a side-effect goal, the collected list itself. Fails on its first assertion without the fix. |
| `testISS0448_EveryMarkUndoExtentIsOpaque` | `\+/1`, `\=/2`, a non-matching *and* a matching catcher, `subsumes_term/2`, `aggregate_all/3`, `forall/2`, `bagof/3` |
| `testISS0449_RetractAssertLoopStaysLinear` | `cnt(100000)` under a 30 s JUnit timeout (~15x the fixed cost, well under the ~40 s the leak cost), the counter value, and one physical clause afterwards |
| `testISS0449_PhysicalClauseCountStaysBoundedDuringTheLoop` | store-level retract/assert loop with no query boundary to hide the leak; peaks at 5001 clauses without the fix, <= 33 with it |

### Follow-up done at the same time

`ISS-2025-0446` was cited in `CHANGELOG.md` and `track-issues.md` with no matching `START_CHANGE`
tag in `src`. The tag now brackets the v4 database section of `Machine.java` — the native
`asserta`/`assertz`/`assert`/`retract` path and its ISO validation — which is where the change lives.


---

## 9. Wave W3 — native library and meta-calls (v3.10.0, ISS-2025-0450..0456)

**Status**: done. Suite **1043/1043 on the default engine and on v4**; **20/20 example programs on
both**, output byte-identical apart from the `-Djprolog.engine=v4` the runner script echoes.

### 9.1 What changed

| ISS | Change | Files |
|---|---|---|
| 0450 | `SolverFacade.solve(Term, Map, List, CutStatus)` runs on `Machine.runSubQuery`; `QuerySolver.internalSolveCount()` test hook | `v4/SolverFacade.java`, `v4/Machine.java` (`onOwnerThread`), `core/engine/QuerySolver.java` |
| 0451 | native `phrase/2,3` | `v4/NativeControl.java` |
| 0452 | native `bagof/3`, `setof/3`, `aggregate_all/3`, `with_output_to/2`; new `~@` in `format/2,3` (both engines); `StreamManager.threadLocalOutput()` | `v4/NativeControl.java`, `builtin/io/Format.java`, `builtin/io/StreamManager.java` |
| 0453 | native lazy library: `member/2`, `memberchk/2`, `append/3`, `select/3`, `selectchk/3`, `nth0/3`, `nth1/3`, `last/2`, `reverse/2`, `length/2`, `msort/2`, `sort/2`, `sum_list/2`, `sumlist/2`, `numlist/3`, `copy_term/2`, `clause/2`, `sub_atom/5`, `sub_string/5`; `Machine.unifyOrUndo`, `Machine.lastSolution`, ports on `pushGenerator` | `v4/NativeLibrary.java`, `v4/Machine.java` |
| 0454 | Prolog prelude + `ClauseStore` library layer | `src/main/resources/prelude/apply.pl`, `v4/Prelude.java`, `v4/ClauseStore.java`, `v4/Machine.java`, `v4/Engine.java` |
| 0455 | `library(yall)` lambdas in `call/N` and in goal position | `v4/Lambdas.java`, `v4/Machine.java` |
| 0456 | `subsumes_term/2` fixed for the variable-to-variable case (v4-only bug from W1) | `v4/Unify.java` |

New tests: `src/test/java/it/denzosoft/jprolog/core/engine/v4/EngineV4LibraryTest.java`
(19 tests, same v4-select/restore pattern as `EngineV4Test`).

### 9.2 New invariants (add to section 3)

12. **A `Generator` that tries several alternatives inside one `next()` must undo its own failed
    unifications.** `Unify.unify` binds as it walks and stops at the first mismatch, and the
    machine only undoes at the *next* redo — so the second attempt would see the first attempt's
    bindings. Use `Machine.unifyOrUndo`, or the explicit
    `mark / forceTrail++ / … / undo / forceTrail--` bracket (that order — invariant 1).
13. **A generator announces its last alternative with `Machine.lastSolution()`.** Without it the
    choice point survives, defeating the trust-me pop, `Bindings.clearIfUnreachable` and the
    deterministic-exit detection of `setup_call_cleanup/3`. `GeneratorGen` reads the flag right
    after `next()` returns and sets `cp.genExhausted`.
14. **`Machine.pushGenerator` owns the Exit/Redo/Fail ports of the goal that installed it.**
    `callNative` stashes the traced goal in `nativeTraceGoal`/`nativeTraceDepth` (saved and
    restored, because a native may run a sub-query that calls other natives) and skips its own Exit
    when `pushGenerator` reports it has taken them over. A new nondeterministic native that does
    not go through `pushGenerator` must emit them itself or Redo/Fail vanish from the trace.
15. **The prelude is the LAST resort, never the first.** `Machine.selectClauses` looks at
    `ClauseStore.libraryClauses` only when the knowledge-base-backed `Predicate` is empty, and
    `Machine.stepN` routes a library indicator to `callUser` *before* `LegacyBuiltinAdapter` so the
    registry entry of the same name cannot shadow it. Both halves are needed: drop the first and a
    user `partition/4` would be shadowed; drop the second and the Java `maplist/3` would win.
16. **The prelude never touches the `KnowledgeBase`.** It lives in a side map on the `ClauseStore`,
    so `listing/1`, `clause/2`, the IDE and the legacy/v2 engines do not see it, and invariant 8
    (the KB is the database of record) still holds — a `sync` cannot wipe the library because the
    library is not part of a `Predicate` at all.

### 9.3 Measured (v2 vs v4, same session, loaded VM, best-of-3 warm)

Harness: `scratchpad/bench/W3Bench.java` (modes `bigterm`, `maplist`, `phrase`, `budget`,
`interrupt`) and `scratchpad/bench/Ph.java`; compile with `javac -cp target/classes -d <dir> X.java`,
run with `java -Xss4m -Xmx2g -cp target/classes:<dir> W3Bench <mode> <v2|v4>`.

**1 M-element list operations** — this is the row that was 1.3–2x *slower* on v4 in section 4:

| Benchmark (1 000 000 elements) | v2 | v4 (W1/W2) | v4 (W3) |
|---|---|---|---|
| `length/2` | 1466 ms | 2063 ms | **1001 ms** |
| `msort/2` | 1257 ms | 1208 ms | **1064 ms** |
| `copy_term/2` | 1571 ms | 2244 ms | **1273 ms** |
| `==/2` | 709 ms | 508 ms | **405 ms** |
| `findall` + `member` | 2341 ms | 2538 ms | **1499 ms** |
| `sum_list/2` | 734 ms | — | **318 ms** |
| `reverse/2` | 1147 ms | — | **692 ms** |
| `append/3` | 2900 ms | — | **1208 ms** |

(The v2 and v4-W3 columns are one session; the middle column is the section-4 session, so compare
it only for the direction of travel.)

**DCG over 1 000 000 tokens** (`digits([]) --> []. digits([D|T]) --> [D], digits(T).`), default JVM
stack, query `numlist(1,1000000,L), phrase(digits(D), L)`:

| | v2 | v4 |
|---|---|---|
| default stack | `resource_error(stack_overflow)` | **ok, 2886 ms, ~1.1 GB** |
| `-Xss4m` | `resource_error(stack_overflow)` | ok, 3513 ms |

The design's ≤ 2 s target is missed by ~0.9 s on this loaded VM (of which ~0.3 s is the `numlist`
that builds the token list); the qualitative acceptance — "a 1 M-token DCG runs at the default
stack" — is met, and v2 cannot run it at all.

**`maplist/3` (prelude Prolog on v4, Java built-in on v2)**:

| n | v2 | v4 |
|---|---|---|
| 50 000 | 493 ms | **265 ms** |
| 200 000 | 1634 ms (609 MB) | **729 ms** (618 MB) |

**Budget / interrupt matrix.** Seven long-running goal shapes with
`setInferenceBudget(200000)`, and four with a thread interrupt after 400 ms:

| Goal | budget v2 | budget v4 | interrupt v2 | interrupt v4 |
|---|---|---|---|---|
| `phrase(digits(_), 1M list)` | stack_overflow | **aborted** | stack_overflow | **cancelled** |
| `maplist(dbl, 1M list, _)` | aborted | **aborted** | cancelled | **cancelled** |
| `member(zzz, 1M list)` | *ran to completion* | **aborted** | *ran to completion* | **cancelled** |
| `bagof(X, member(X, 1M list), _)` | aborted | **aborted** | cancelled | **cancelled** |
| `aggregate_all(count, member(_, 1M list), _)` | aborted | **aborted** | — | — |
| `with_output_to(atom(_), member(zzz, 1M list))` | *ran to completion* | **aborted** | — | — |
| `sub_atom(...), member(zzz, 1M list)` | *ran to completion* | **aborted** | — | — |
| **total** | **4 / 7** | **7 / 7** | **2 / 4** | **4 / 4** |

**`QuerySolver.solveInternal` reachability**: 0 entries across a v4 query exercising `phrase/2`,
`bagof/3`, `setof/3`, `aggregate_all/3`, `forall/2`, `with_output_to/2`, `format ~@`, `findall/3`,
`maplist/2` and `catch/3` (asserted by `testISS0450_NoBuiltinReachesTheRecursiveSolver`). Before
W3 the same query entered it thousands of times.

### 9.4 Deviations from the W3 brief, and why

1. **[NO LONGER TRUE — the shim was deleted in W4, section 10.1/10.4.]** ~~The name→cell shim (deviation 2 of section 5) STAYS.~~ It was removed experimentally and the
   whole suite run on v4: two tests fail without it — `testISS0357_LabelingBindsDeterminedVariables`
   (the CLP(FD) `exportSingletons` case the shim was written for: `C in 1..3, D #= C*2+1,
   label([C])` reports `D`, which `label/1`'s goal never mentions) and `testISS0421_AbsExpression`.
   Both come from built-ins that are still legacy-bridged and report a binding **by name** for a
   cell their goal never mentions, which no amount of native list/collection built-ins can change.
   It goes away with W4, when CLP(FD) moves onto the native attribute hook.
2. **`append/3`'s fully-open mode and `member/2`'s open-tail mode keep the v2 answer.**
   `append(X, Y, Z)` with all three arguments open yields the single standard solution
   (`X = [], Z = Y`) instead of enumerating infinitely, and `member(X, PartialList)` stops at the
   open tail instead of generating longer and longer lists. Both are what the eager built-ins do,
   both are pinned by the suite (`testISS0379_AppendFullyOpenDoesNotThrow`), and a v4-only infinite
   generator would make programs that rely on the failure loop. They become the real two-clause
   definitions when the list library moves to the prelude in **W6**.
3. **`current_op/3` is not native.** It reads the process-global `OperatorDefinition` table
   (LIM-034), which **W7** replaces with a per-engine operator store; the table holds ~50 entries,
   so there is no laziness or performance argument for touching it now.
4. **`sort/4` and `predsort/3` stay bridged.** `sort/2` and `msort/2` are native (they are on the
   1 M-element path); `sort/4`'s key/order handling and its ISS-2025-0418 up-front key validation
   have no performance problem to solve. `BuiltinTable` is keyed by `(name, arity)`, so `sort/2`
   native and `sort/4` bridged coexist with no special case.
5. **`with_output_to/2` still swaps `System.out` as well as the thread-local stream.** A handful of
   built-ins (and the `format ~p` portray path) still write to `System.out` directly; until the
   stream rework of **W7** the capture has to cover both. LIM-025 is narrowed, not closed.
6. **`aggregate_all(max(Expr), ...)` remains numbers-only** (`type_error(number, T)` otherwise),
   because ISS-2025-0413's tests pin that. SWI compares in the standard order. The
   `max(Value-Witness)` / `min(Value-Witness)` pair form was added as a strict extension: it
   compares the numeric left-hand side, so no previously-erroring query changes behaviour.
7. **Off-thread sub-solves still use the recursive solver.** `concurrent/3`,
   `concurrent_maplist/N` and `first_solution/3` submit `solver.solve(...)` to a worker thread; a
   `Machine` is single-threaded by construction (one goal stack, one choice-point list, and
   bindings that live in shared `Variable` cells), so `SolverFacade` checks
   `Machine.onOwnerThread()` and falls back. This is the only remaining path from a v4 query into
   `QuerySolver.solveInternal`, and it is also LIM-024 (the concurrency predicates share one
   solver). **W8** gives each thread its own machine.

### 9.5 Where wave W4 starts

> **W4 is done (v3.11.0)** — see section 10. All five items below were implemented as written,
> with one shape change (item 3: the CLP(FD) hook is reached through the prelude dispatcher rather
> than being called from the binder). This section is kept as the historical brief.

W4 is **coroutining** (design B.9, B.16 row W4). In dependency order:

1. **`Coroutining` wake queue** — replace `Machine.AttrBridge`. Today `freeze/2` is handled
   natively on the machine's `woken` list, but `when/2`, `dif/2` and the CLP(FD) v2 bridge still go
   through the legacy `Variable.getAttributeUnifyHook()`, which is handed a name-keyed **view** of
   the currently bound cells (`LegacyBuiltinAdapter.bindingView`). That view is why bindings made
   by a `when/2`-woken goal are lost (ISS-2025-0336) — the goal binds names in a map nobody reads
   back. B.9 wants a real wake queue: `attr_unify_hook(Module, AttrValue, Value)` called with
   cells, the woken goals pushed onto the goal stack in the machine's own order, and their bindings
   landing in the real cells.
2. **`attr_unify_hook` protocol + `put_attr/3`, `get_attr/3`, `del_attr/2`** as v4 natives over the
   `Variable` attribute map, with the attribute change trailed (`Bindings.pushUndo`, as
   `AttrBridge` already does for the freeze goal).
3. **CLP(FD) v2 on the hook** — `builtin.clpfd.v2.ClpfdV2Bridge.onAlias` is already called from
   `AttrBridge`; the rest of the bridge reports bindings by name. **This is what lets deviation 1
   (the `Machine.nameIndex` shim) finally be deleted** — check with
   `testISS0357_LabelingBindsDeterminedVariables` and `testISS0421_AbsExpression`, the two tests
   that fail today when `cellFor` returns null.
4. **`freeze/when/dif` as prelude modules** — the prelude loader (`v4/Prelude.java`) and the
   `ClauseStore` library layer built in W3 are the mechanism; add `prelude/coroutining.pl` to
   `Prelude.RESOURCES`.
5. **Drop cross-query coroutining** (design decision 3, approved): session-scoped attributed
   variables become an explicit opt-in `Prolog` API for the IDE.

Acceptance for W4 (design B.16): `when/2` bindings propagate; no cross-query wake-ups;
`ClpfdV2Test`, `ClpfdV2EngineTest` green on v4; and, as always, the full suite green on **both**
engines plus 20/20 example programs.


---

## 10. Wave W4 — coroutining and attributed variables (v3.11.0, ISS-2025-0457..0462)

**Status**: done. Suite **1065/1065 on the default engine and on v4** (1043 pre-existing + 22 new);
**20/20 example programs on both**, output byte-identical apart from the `-Djprolog.engine=v4` the
runner script echoes.

### 10.1 What changed

| ISS | Change | Files |
|---|---|---|
| 0457 | the wake queue: `Coroutining.onBind` + `Machine.wake(Term, Bindings)` (trailed), trailed attribute changes, `LinkedHashMap` attributes, `Machine.hasQualifiedHook`/`callQualified`, `flat` lookup in `callUser`/`selectClauses`; `Machine.AttrBridge` and `LegacyBuiltinAdapter.bindingView` deleted | `v4/Coroutining.java` (new), `v4/Machine.java`, `v4/LegacyBuiltinAdapter.java`, `core/terms/Variable.java` |
| 0458 | the SWI protocol as natives: `put_attr/3`, `get_attr/3`, `del_attr/2`, `attvar/1`, `term_attvars/2`, `copy_term/3`, `unifiable/3`, the `'$attr_unify'/4` dispatcher and `Module:attr_unify_hook/2`; `Bindings.boundSince(mark)` | `v4/Coroutining.java`, `v4/Bindings.java`, `v4/NativeBuiltins.java` |
| 0459 | `freeze/2`, `frozen/2`, `when/2`, `dif/2`, `?=/2` and `'$attr_hook'/4` as prelude Prolog; fixes ISS-2025-0336 on v4 | `src/main/resources/prelude/coroutining.pl` (new), `v4/Prelude.java` |
| 0460 | CLP(FD) on the attribute hook (`cellFor`, `onBindByName`, `domainTermForCell`, `Ctx.cells`), the native `'$clpfd_unify_hook'/2`; **`Machine.nameIndex`/`cellFor`/`indexCells` deleted** | `builtin/clpfd/v2/ClpfdV2Bridge.java`, `v4/Coroutining.java`, `v4/Machine.java`, `v4/LegacyBuiltinAdapter.java` |
| 0461 | no cross-query coroutining on v4: no session splicing/refresh on the v4 route, and the legacy `Variable` attribute hook is uninstalled for the duration of a v4 query | `core/engine/Prolog.java`, `test/refactor/RefactorIssuesTest.java` |
| 0462 | `Prolog.residualGoals(Map)` over `Coroutining.residualGoals(List<Term>)` (shared with `copy_term/3`) | `core/engine/Prolog.java`, `v4/Coroutining.java` |

New tests: `src/test/java/it/denzosoft/jprolog/core/engine/v4/EngineV4CoroutiningTest.java`
(22 tests, same v4-select/restore pattern as `EngineV4Test`).

### 10.2 How it works, in one page

1. `Unify.bindVar` binds the cell and then calls the machine's `Unify.AttrHandler`
   (`Machine.WakeHandler` -> `Coroutining.onBind`). **Nothing runs there** — we are inside a term
   walk. `onBind` pushes one goal per attribute module onto `Machine.woken`:
   `'$attr_unify'(Module, AttValue, Other, VarName)`.
2. `Machine.drive` drains the queue at the top of its loop, **before the next goal**, pushing the
   wake goals in order onto the goal stack with `cps.size()` as the cut barrier. From there they are
   completely ordinary goals.
3. `'$attr_unify'/4` (a native) dispatches: a user-defined `Module:attr_unify_hook(AttValue, Other)`
   clause if one exists, else the prelude's `'$attr_hook'/4`. JProlog stores a module-qualified
   clause head as a `:/2` predicate (`listing` shows `:(foo, hook(X, Y)) :- ...`), which
   `Machine.stepN`'s `:` branch resolves through the *module manager* — and there is no `freeze`
   module — so the hook is called with `Machine.callQualified`, a **flat** `:/2` lookup.
   `Machine.hasQualifiedHook(module)` scans the `:/2` clause heads; the predicate has no clauses at
   all in every program that does not define one, which is the fast path.
4. The prelude implements `freeze` (run or re-suspend on `Other`), `dif` (re-check each
   `'$dif'(X,Y)`), `when` (re-check each `'$when'(Fired, Cond, Goal)`), `clpfd` (the native
   `'$clpfd_unify_hook'/2`), and a **catch-all last clause** so an attribute of a module with no
   hook is inert data — the behaviour the legacy engines had for an unrecognised module.
5. Everything is trailed: `Coroutining.putAttr`/`delAttr` record an undo action, and
   `Machine.wake` records one that truncates the queue back to its previous height. That is what
   makes a failed head unification leave no stale wake behind, and backtracking **re-arm** a
   suspension instead of consuming it.

### 10.3 New invariants (add to section 3)

17. **The attribute hook may only QUEUE.** `Coroutining.onBind` runs inside `Unify.unify`; running a
    goal there would re-enter the drive loop in the middle of a term walk. Everything a hook wants
    to do becomes a wake goal. A hook that must *fail* the unification does so by having its woken
    goal fail — the machine then backtracks the binding, which is the same observable result and
    keeps the failure inside the normal control flow.
18. **A wake push is trailed, exactly like a binding.** `Machine.wake` takes the queue height and
    pushes an undo that truncates back to it. Without that, `p(1, b)` matched against
    `p(FrozenX, a)` would bind `FrozenX`, queue its goal, fail on the second argument, and then run
    the goal anyway on the next drive iteration.
19. **Attribute changes go through `Coroutining.putAttr`/`delAttr`, never through
    `Variable.putAttribute` directly.** The raw setters are untrailed; the CLP(FD) bridge is the
    one legitimate exception, and it records its own undo on the legacy `Trail` (which the v4 choice
    points roll back through `cp.legacyMark`).
20. **A probe unification must undo inside its own forced-trail extent.** `unifiable/3` and
    `\=/2` bind, inspect and undo; because the wake pushes are trail entries, the undo removes them
    too — a probe therefore wakes nothing. Keep the ISS-2025-0448 order (undo, *then* close the
    extent).
21. **The prelude's coroutining predicates are the v4 implementation; the Java ones are the
    v2/legacy implementation.** They store *different* attribute values under the same module names
    (v4 `when` holds a list of `'$when'(Fired, Cond, Goal)`, the Java one a single
    `when(Cond, Goal)`). They never meet, because attributes do not survive a query on v4 and the
    v4 route uninstalls the legacy hook. Do not "unify" the two.

### 10.4 The shim is gone

`Machine.nameIndex`, `Machine.cellFor` and `Machine.indexCells` — deviation 2 of section 5, kept
through W3 — **no longer exist** (asserted by reflection in
`testISS0460_TheNameIndexShimIsGone`). The two tests that needed it,
`testISS0357_LabelingBindsDeterminedVariables` (`C in 1..3, D #= C*2+1, label([C])` must report `D`)
and `testISS0421_AbsExpression`, pass because `LegacyBuiltinAdapter.apply` now asks
`ClpfdV2Bridge.cellFor(name)` for the cell behind a name the built-in reported but the goal never
mentioned. That is not the old shim under a new name: the bridge *created* those cells (`varFor`,
`onAlias`), the map is bounded by the number of FD variables in the query, it is reset with the rest
of the CLP(FD) context at every top-level query, and no other subsystem can put anything in it.

`exportSingletons/1` still reports by name because `label/1` is still an eager legacy built-in that
returns `Map<String,Term>` solutions; making `label/1` a lazy v4 generator is a W6 item, and it
would remove the last name-keyed hop.

### 10.5 A/B evidence (v2 vs v4, same session)

Probe: 33 acceptance queries, one fresh `Prolog` per query except where the query sequence is the
point (`scratchpad/probe/Cor.java`, `Seq.java`). **v4: 33/33. v2: 17/33.**

| Query | v2 | v4 |
|---|---|---|
| `findall(Y, (freeze(X, Y = got(X)), member(X, [1,2])), L), L == [got(1), got(2)]` | fail | **ok** |
| `freeze(X, Y = hello), X = 1, Y == hello` | ok | ok |
| `when(nonvar(X), Y = done), X = 1, Y == done` | fail | **ok** |
| `when(ground(X-Y), Z is X+Y), X = 1, Y = 2, Z == 3` | fail | **ok** |
| `dif(X, Y), X = a, Y = b` | ok | ok |
| `dif(X, a), X = a` | fail (correct) | fail (correct) |
| `dif(f(X), f(Y)), X = 1, Y = 1` | fail (correct) | fail (correct) |
| `freeze(X, G = fired), X = Y, Y = 1, G == fired` | fail | **ok** |
| `when((nonvar(A) ; nonvar(B)), Counter), A = 1, B = 2` fires once | fires twice | **once** |
| `catch((freeze(X, throw(boom)), X = 1), boom, true)` | ok | ok |
| query 1 `when(nonvar(X), throw(leak))`, query 2 `X = 1` | **throws `leak`** | **ok, silent** |
| query 1 `freeze(X, throw(leak))`, query 2 `X = 1` | **throws `leak`** | **ok, silent** |
| query 1 `dif(X, a)`, query 2 `X = a` | fail (leak) | **ok** |
| `term_attvars/2`, `copy_term/3`, `frozen/2`, `unifiable/3` | `existence_error` | **ok** |
| user `mymod:attr_unify_hook/2` honoured (`put_attr(X, mymod, 7), X = 8` must fail) | not called | **ok** |
| `put_attr(X, nomod, 7), X = 8` (inert module) | ok | ok |
| `C in 1..3, D #= C*2+1, label([C])` -> 3 answers with `D` bound | ok | ok |
| `Y in 1..5, Z #= abs(Y-3), label([Y])` -> 5 answers | ok | ok |
| `X in 1..3, X = 5` / `X = 2` | fail / ok | fail / ok |
| `A in 1..4, B in 1..4, C in 1..6, A*A+B*B #= C*C, label([A,B,C])` | ok | ok |

The four coroutining rows of `scratchpad/bench/Limits.java` — which share **one** `Prolog` instance
and were therefore polluted by the cross-query leak — now all pass in sequence on v4
(`when/2 binding propagation` 31 ms, `dif/2 suspension then success` 1 ms, `freeze inside findall`
2 ms, `exception through when-woken goal` 13 ms). On v2 the same sequence still fails the first row
and returns unbound `_R0_Y` elements for the third.

Core performance is unaffected (the hook is behind `Variable.hasAttributes()`, and W4 *removed*
`indexCells` from every bridged call): `nrev 30 x 2000`, same loaded session, v2 5593 ms vs
v4 **829 ms**.

### 10.6 Deviations from the W4 brief, and why

1. **The CLP(FD) hook is native and reached through the prelude dispatcher**, not called from the
   binder. `'$attr_hook'(clpfd, _, Other, VarName) :- '$clpfd_unify_hook'(VarName, Other).` The
   CLP(FD) store is a Java constraint store keyed by variable name, so its hook cannot usefully be
   Prolog; routing it through the same dispatcher keeps one wake path for all attribute kinds. The
   wake goal carries the bound cell's **name** as a fourth argument precisely because the cell
   itself would dereference to its value by the time the hook runs.
2. **`unifiable/3` was added** (it is not in the B.9 list). `dif/2`'s "re-suspend on the remaining
   unifier variables" needs exactly it, and the alternative — suspending on every variable of both
   terms — over-suspends. It is a v4-only predicate, like `partition/4` and the yall lambdas.
3. **`when/2`'s "fired" flag is a shared logic variable**, not a mutable cell: a disjunctive
   condition attached to N variables must run its goal once, and an ordinary binding gives
   backtracking for free (re-arm on undo).
4. **The Java `Freeze`/`When`/`Dif` built-ins were not touched.** They are the v2/legacy
   implementation and the suite pins their behaviour on those engines, ISS-2025-0336 included.
5. **No opt-in cross-query API was added.** B.9 allows one "if the IDE needs it"; nothing in
   `editor/` or `PrologCLI` references `attributedSessionVars` or `clearSession` (only `Prolog`
   itself does), so there is no consumer to serve. Adding it later is a `Prolog` method that keeps
   the previous answer's cells alive — no engine change.
6. **The internal `'$attr_unify'/4` and `'$attr_hook'/4` goals appear in a four-port trace** of a
   program that uses coroutining, between the port pair of the goal that made the binding and the
   port pair of the woken goal. The woken goal itself is traced normally (asserted by
   `testISS0457_WokenGoalIsTraced`), which is what invariant 10 asks for; v2 traces woken goals not
   at all, so the "byte-identical v2/v4 trace" oracle now holds only for programs without
   attributed variables.

### 10.7 Where wave W5 starts

> **W5 is done (v3.12.0)** — see section 11 for what was implemented, how it deviates from the
> plan below, and where **W6** starts. This section is kept as the historical brief.

W5 is **tabling** (design **B.8**, B.16 row W5). Today `Machine.callUser` detects a tabled
predicate (`engine.tables().isTabled(...)`) and calls `tabledDelegate`, which hands the *resolved*
goal to the recursive legacy `QuerySolver` (`engine.contextSolver().solve(...)`) and installs the
resulting solution maps through `LegacyBuiltinAdapter.installSolutions`. That is the **last**
routine path from a v4 query into `QuerySolver.solveInternal` other than the off-thread sub-solve of
deviation 7 (section 9.4), and it drags the whole legacy package along: the 2 000-deep recursion
cap, name-keyed answers, and no interaction with the budget inside the fixpoint.

In dependency order:

1. **An answer trie keyed by variant** on the `Engine` (`Engine.tables()` today is the legacy
   `TableStore` — give v4 its own store, or re-shape that one). Answers are stored as
   variant-normalised terms, **not** `Map<String,Term>`, so `path(1, 51)` and `path(1, Y)` share the
   machinery (limit L-03). `Unify.copy` + `Unify.numberVars` already give the normal form, and
   `Unify.compareTerms` gives the ordering for deduplication.
2. **Generator / consumer choice points in the machine.** The first call to a variant becomes the
   GENERATOR: it runs the clauses normally and records every answer in the trie as it is found
   (answers are returned to the caller as they arrive). A later call to a variant that is
   EVALUATING becomes a CONSUMER choice point that iterates the answers recorded *so far* — a new
   `CP` kind, or a `Generator` over the trie. Note invariant 14: whichever it is, it must carry
   `traceGoal`/`traceDepth` or Redo/Fail vanish from traces.
3. **Completion by re-execution to a fixpoint** (semi-naive: a consumer only returns answers it has
   not yet seen). When a generator exhausts its clauses and some consumer of its SCC read answers
   that were later extended, re-run the clauses until no table in the SCC grows; then mark them all
   COMPLETE. Termination comes from the finite, deduplicated answer set, so there is no iteration
   cap.
4. **`abolish_all_tables/0`, `abolish_table/1`, the `:- table` directive** on the new store, and
   `tnot/1` (well-founded negation) if it fits; the directive and the two abolish predicates are
   registry built-ins today and must observe the v4 store.
5. **Delete `Machine.tabledDelegate`** and re-check `EngineV4LibraryTest`'s
   `QuerySolver.internalSolveCount()` assertion with a tabled query added to it — that is the
   acceptance oracle for "no built-in reaches the recursive solver".

Acceptance for W5 (design B.16): left-recursive `path/2` over a 3 000-edge chain answers at the
default JVM stack and under the inference budget (today `path(1, 3001)` FAILS on both engines in
`scratchpad/bench/Limits.java`, and `budget inside tabled call` fails too); `path(1, Y)` and
`path(1, 51)` give the same answers; the existing tabling tests green on both engines; and, as
always, the full suite green on **both** engines plus 20/20 example programs.


---

## 11. Wave W5 — tabling (v3.12.0, ISS-2025-0463..0465)

**Status**: done. Suite **1083/1083 on the default engine and on v4** (1065 pre-existing + 18 new);
**20/20 example programs on both**, output byte-identical apart from the `-Djprolog.engine=v4` the
runner script echoes.

### 11.1 What changed

| ISS | Change | Files |
|---|---|---|
| 0463 | linear tabling with completion: `Tabling` (variant store + `TableFrame`, the generator/consumer choice point), `Machine.callTabled`, `Machine.buildClauseBody`, `CP.tframe` + the discard hook in `cut`/`handleBall`, `Tabling.endQuery` in `Machine.solve`'s finally, `Engine.tabling()`; `Machine.FAILED`/`EXHAUSTED` made package-visible | `v4/Tabling.java` (new), `v4/Machine.java`, `v4/Engine.java` |
| 0464 | `abolish_all_tables/0`, `abolish_table/1` and the new `current_table/2` as v4 natives on that store; table invalidation from `assertClause`/`retractClause`; the two store caps | `v4/Tabling.java`, `v4/NativeBuiltins.java`, `v4/Machine.java` |
| 0465 | `Machine.tabledDelegate` **deleted**; `testISS0450_NoBuiltinReachesTheRecursiveSolver` extended with tabled queries | `v4/Machine.java`, `test/.../EngineV4LibraryTest.java` |

New tests: `src/test/java/it/denzosoft/jprolog/core/engine/v4/EngineV4TablingTest.java`
(18 tests, same v4-select/restore pattern as the other `EngineV4*Test` classes, plus one
engine-independent oracle that runs on both engines).

### 11.2 How it works, in one page

1. `Machine.callUser` sees `engine.tables().isTabled(...)` and calls `Machine.callTabled`, which
   computes the **variant key** (`Tabling.variantKey`: a numbervars-style canonical encoding over
   the variable **cells**, iterative, guard-polling, length-prefixed so it is injective) and looks
   the table up.
2. **No table** -> create one and install a **generator** frame. **COMPLETE** -> install a
   **consumer** frame over the final answer list. **EVALUATING** -> record the dependency
   (`noteIncompleteRead`) and install a generator if the table has not produced yet in the current
   round and is not already producing, otherwise a consumer.
3. A generator's PRODUCE phase is a `Gen` that returns, per clause,
   `clause-body -> record-answer -> fail`, run against `prod`, a private copy of the call. It is
   fail-driven, so it hands **nothing** to the caller and the machine's own backtracking drives it;
   `record-answer` deduplicates by variant key and appends `Unify.copy(prod)`.
4. When the clauses are exhausted, `Tabling.endProduction` pops the frame from the producing stack
   and answers "am I the leader of my SCC?" using the classic DFN scheme: each table has a creation
   sequence number, a producing frame keeps `minOuterSeq` (the smallest sequence number it read
   while incomplete), a child frame propagates its `minOuterSeq` to its parent when it is not a
   leader, and a frame is a leader when `minOuterSeq >= table.seq`.
5. A leader that both **grew** the answer set and **read an incomplete table** during the round
   (two global counters, snapshotted at the start of the round) starts a new round: `round++`,
   `beginProduction` again, clause index back to 0. Every SCC table whose `producedRound < round`
   re-produces when it is next called, so the whole component is recomputed; deduplication is what
   makes that semi-naive. When a round changes nothing, `completeScc` marks the leader and every
   still-EVALUATING table created at or after it COMPLETE.
6. The CONSUME phase unifies the caller's goal with `Unify.copy(answer)` per redo, reading
   `answers.size()` freshly each time so a table that is still growing is followed.

### 11.3 New invariants (add to section 3)

22. **A tabled call is a choice point, never a nested drive.** The PRODUCE phase runs in the main
    drive loop with the continuation `record, fail`; that is what keeps a 100 000-subgoal
    evaluation (`fib(1000, F)`, a 3 000-node right-recursive closure) inside the default JVM stack.
    A future "just run it with `findAll`" simplification reintroduces one Java frame per subgoal.
23. **A `!` in a tabled clause body must stay local.** `TableFrame.bodyBarrier` is `cps.size()`
    taken **after** the generator CP is pushed, so a cut prunes only the body's own choice points.
    With the ordinary `callUser` barrier (taken before the push) a cut would discard the generator
    and abandon the table half-built.
24. **An abandoned evaluation must discard its tables.** A table left EVALUATING would be read as
    authoritative by the next call. Three hooks: `CP.tframe.discard()` from `Machine.cut` and
    `Machine.handleBall`, and the `Tabling.endQuery()` sweep in `Machine.solve`'s `finally` (which
    is what catches an `InferenceLimitException`/`QueryCancelledException` unwinding out of the
    machine). Any new code path that destroys a choice point without driving it to exhaustion needs
    the same call.
25. **The production template is created before `pushCP`.** Its cells are then older than the
    choice point's `barrierSerial`, so every binding to them is trailed and `advance`'s
    `B.undo(cp.trailMark)` really does clean it between clauses (invariant 1). For the same reason
    one template is reused across all rounds of a frame — a fresh one made inside `next()` would be
    younger than the barrier and its bindings would not be undone.
26. **Only a COMPLETE table may trust-me pop.** `cp.genExhausted` is set at the last answer only
    when `table.status == COMPLETE`; an EVALUATING table can still grow inside the same round, and
    a popped consumer cannot pick the new answers up.
27. **The PRODUCE phase emits no ports of its own.** `TableFrame.next` clears `cp.traceGoal` on
    entry and re-arms it only after the first answer has been delivered (so the second answer
    onwards gets a Redo) and just before returning `EXHAUSTED` (so the Fail port fires). Otherwise
    every clause retry of the production would print a spurious `Redo` for the tabled goal.

### 11.4 A/B evidence (v2 = default engine vs v4, same session, loaded VM)

Harness: `scratchpad/bench/AB5.java` — `javac -cp target/classes -d scratchpad/bench AB5.java`,
then `java -Xss4m -Xmx2g -cp target/classes:scratchpad/bench AB5 v2|v4`. One fresh `Prolog` per
row. Chain programs are
`edge(I,J) :- between(1,N,I), J is I+1.  :- table path/2.  path(X,Y) :- edge(X,Y).` plus the
recursive clause named in the row.

| Query | v2 (default) | v4 |
|---|---|---|
| LEFT 3000 `path(1, 3001)` | **FAIL** 124 ms | **OK** 201 ms |
| LEFT 3000 `path(1, 51)` | **FAIL** 120 ms | **OK** 118 ms |
| LEFT 3000 `findall(Y, path(1,Y), L), length(L, 3000)` | **FAIL** 65 ms | **OK** 81 ms |
| RIGHT 3000 `path(1, 3001)` | **FAIL** 6534 ms | **OK** 152 ms |
| RIGHT 3000 `path(1, 51)` | **FAIL** 6627 ms | **OK** 46 ms |
| RIGHT 3000 `findall ... length(L, 3000)` | **FAIL** 9903 ms | **OK** 4990 ms |
| DOUBLE 120 `path(1, 121)` | OK 9346 ms | **OK** 342 ms |
| DOUBLE 120 `path(1, 51)` | OK 7715 ms | **OK** 343 ms |
| DOUBLE 120 `findall ... length(L, 120)` | **FAIL** 7622 ms | **OK** 364 ms |
| DOUBLE 400 `findall ... length(L, 400)` | **TIMEOUT** (180 s) | **OK** 10363 ms |
| **LEFT 100000 `path(1, 100001)`** (design B.15 row) | **FAIL** 40 ms | **OK 868 ms** |
| LEFT 100000 `findall ... length(L, 100000)` | **FAIL** 96 ms | **OK** 953 ms |
| tabled `fib(30, 832040)` | OK 8 ms | OK 35 ms |
| tabled `fib(1000, F)`, 209 digits | OK 2723 ms | **OK 99 ms** |
| mutual `p/1`-`q/1` -> `[1,2]` | OK 21 ms | OK 13 ms |
| tabled `even(400)` | OK 162 ms | OK 13 ms |
| tabled `odd(400)` (must fail) | fails (correct) | fails (correct) |
| cyclic graph, closure of `a` == `[a,b,c,d]` | OK 4 ms | OK 11 ms |
| `abolish_all_tables` forces recomputation (side-effect counter) | **FAIL** | **OK** |
| `assertz` invalidates the table | **FAIL** | **OK** |
| `current_table(path(a,c), complete)` | `existence_error` | **OK** |
| `tnot/1` -> `existence_error(procedure, tnot/1)` | OK | OK |
| cut in a tabled body is local (`findall(X, t(X), [1,9])`) | **FAIL** | **OK** |
| exception leaves no partial table | **FAIL** | **OK** |
| tabled call inside `findall`/`\+`/`catch`/`once` | **FAIL** | **OK** |
| budget 50 000, 100 000-edge chain | **FAIL** (no budget inside tabling) | **InferenceLimitException** 106 ms |
| thread interrupt after 150 ms, 100 000-edge chain | *n/a* (the query fails in 40 ms) | **QueryCancelledException** 162 ms |

Notes on three rows:

- **The design B.15 target is met**: the 100 000-edge chain answers `path(1, 100001)` correctly in
  **868 ms** (target: correct, <= 2 s; the task's acceptance was <= 5 s). v2 answers *wrong* in
  40 ms.
- **`scratchpad/bench/Limits.java`'s "budget inside tabled call" row now needs a bigger chain.**
  It sets `setInferenceBudget(50000)` on the **3 000**-edge chain, and the W5 evaluation of
  `path(1, 3001)` costs between 20 000 and 50 000 inferences, so it now *completes* instead of
  aborting. Enforcement is real and was verified by bisecting the budget on that exact query —
  50 000: OK, 20 000: `InferenceLimitException`, 5 000: `InferenceLimitException` — and by the
  100 000-edge row above, which is what `EngineV4TablingTest` asserts.
- **The doubly recursive definition is cubic, in any tabling system.** `path(X,Y) :- path(X,Z),
  path(Z,Y)` over an n-edge chain builds n tables, and the join for each one pairs O(n) answers
  with O(n) answers, so the full closure is Θ(n³): 364 ms at n = 120 and 10 363 ms at n = 400 on
  this VM (≈ n^2.8 measured), which extrapolates to hours at n = 3 000. The acceptance query set
  is therefore run at n = 120 in the JUnit test and n = 400 in the harness; correctness — not the
  size — is what that shape demonstrates. Use the left- or right-recursive formulation for long
  chains.

`QuerySolver.solveInternal` reachability: **0** entries across a v4 session running a tabled
left-recursive query, `findall/3` over it, a tabled `fib/2`, `\+ path(...)` and
`abolish_all_tables/0` (`EngineV4LibraryTest.testISS0450_NoBuiltinReachesTheRecursiveSolver` and
`EngineV4TablingTest.testISS0450_NoBuiltinReachesTheRecursiveSolverForTabledQueries`).

### 11.5 Deviations from the W5 brief, and why

1. **Answers are returned after the table's SCC completes, not streamed as they are found.**
   Section 10.7 item 2 says "answers are returned to the caller as they arrive". A generator that
   streamed would have to re-run the caller's continuation for the same answer in the next
   completion round, which needs suspension/resumption of continuations — the SLG-WAM machinery the
   design explicitly avoided ("keeps the machine's SLD structure"). The PRODUCE phase is therefore
   fail-driven and the CONSUME phase follows it. Observable difference: a tabled call is
   *semi-deterministic in cost* — the first answer costs the whole fixpoint. `once(path(1, Y))`
   still computes the table. Consumers of an EVALUATING table (the recursive calls, which is where
   laziness actually matters) **are** lazy and index-based, exactly as the brief asks.
2. **A real answer trie was not built** (the brief calls it optional). Answers are a
   `List<Term>` in insertion order plus a `HashSet<String>` of variant keys. The key string is
   built into a per-frame reusable `StringBuilder` and computed on the template *before* the copy,
   so a duplicate answer costs one string build and one hash lookup and no term copy — which is
   what took the right-recursive 3 000 closure (4.5 M answers) from 6.5 s to 5.0 s. A trie would
   save the ~60 bytes/answer the key strings cost; it is a memory optimisation, not a correctness
   one.
3. **`tnot/1` is not implemented.** Well-founded negation needs delay lists and a three-valued
   completion, which is a wave of its own. It raises `existence_error(procedure, tnot/1)`, as the
   brief requires. Ordinary `\+/1` inside a tabled predicate stays negation-as-failure over the
   answers available at that instant, which means a program whose meaning is *undefined* under the
   well-founded semantics gets an engine-dependent answer: for `:- table p/1. p(X) :- \+ p(X).`,
   `p(a)` **succeeds** on v4 (round 1: the inner call consumes the empty in-progress table, `\+`
   succeeds, `p(a)` is recorded; round 2 adds nothing, so the table completes with that answer) and
   **fails** on v2. Neither is WFS. `RefactorIssuesTest.testR5_tabledNegation` only requires
   termination inside one second, and passes on both (661 ms cold, ~90 ms warm on v4).
4. **`table/1` and the `:- table` directive stay legacy built-ins.** They only write the
   *declaration*, which lives in the shared `TableStore` because the v2 and legacy engines and
   `Prolog.processTableDirective` all read it. Only the *answers* moved to the v4 store. The two
   abolish predicates had to become natives because they touch answers.
5. **`abolish_table/1` still un-declares the predicate.** That is what the v2 implementation and
   `docs/guides/guide-tabling-predicates.md` have always documented, and `examples/test_42_tabling.pl`
   relies on it. It is surprising (SWI/XSB keep the declaration) but changing it is a separate,
   cross-engine decision.
6. **Invalidation is per-predicate, not dependency-tracked.** A table is dropped when the tabled
   predicate *itself* is asserted to or retracted from. Tracking that `path/2`'s table depends on
   `edge/2` needs a call-graph the engine does not build (SWI calls this "incremental tabling" and
   makes it opt-in). Documented in the reference, the guide and LIM-037.
7. **Two store caps were added** (100 000 tables, 4 000 000 answers, evicting the oldest COMPLETE
   tables at a query boundary only). Not in the brief, but the legacy `TableStore` had a
   10 000-entry cap and the sandbox story ("one engine per security domain, long-lived") makes an
   unbounded memo a denial-of-service surface. Eviction can never touch a running evaluation.
8. **`current_table/2` was added** (the brief lists it as optional). It is unification-based
   enumeration, like `current_op/3`, rather than SWI's variant lookup — so
   `current_table(path(a,c), S)` also matches the more general table `path(a,_)`. It is v4-only.
9. **The "repeat this round?" test is deliberately conservative.** It compares two *global*
   counters (answers added, incomplete reads) against the snapshot the frame took at the start of
   its round, so activity inside a **nested SCC that completed during the round** also counts. A
   leader whose body created one independent inner variant therefore runs one extra, cheap round:
   `path(1, 100001)` re-consumes the (by then COMPLETE) `path(1, Z)` table once. Crediting each
   completed inner SCC back to its parent frame would remove it (roughly 200 k of the ~500 k
   inferences that query costs), but it can only ever *reduce* the number of rounds, i.e. an error
   there under-iterates and silently loses answers — not a trade worth making while the design
   target is met with a 6x margin. If it is done later, the oracle test in `EngineV4TablingTest` is
   the thing to run first.

### 11.6 Where wave W6 starts

> **W6 is done (v3.13.0)** — see section 12 for what was implemented, how it deviates from the
> plan below, and where **W7** starts. This section is kept as the historical brief.

W6 is **modules and the Prolog prelude** (design **B.10**, B.16 row W6). Nothing in W5 blocks it;
the pieces it needs — the `ClauseStore` library layer and the `Prelude` loader (W3), the flat `:/2`
lookup `Machine.callQualified` (W4) — are already in place.

In dependency order:

1. **A `Modules` owner object on the `Engine`** (design B.10) replacing the shared
   `core.module.ModuleManager` on the v4 path: every predicate belongs to a module, `system` holds
   the built-ins, `user` is the default and *is* the flat KB (so the `modules.size() > 1` special
   case in `Machine.selectClauses` disappears). Resolution order for an unqualified call from
   module `M`: `M` -> `M`'s imports -> `user` -> autoload libraries -> `system`.
2. **`Module:Goal` for built-ins** — `lists:append/3` and `system:atom_length/2` must work
   (`lists:append([1],[2],L)` is false today; it is a row in `scratchpad/bench/Limits.java`).
   `Machine.stepN`'s `:` branch currently resolves through the module manager and falls back to a
   plain call; it needs the new resolution order, and `Machine.callQualified` (the flat `:/2`
   lookup W4 added for `attr_unify_hook/2`) must keep working for module-qualified *clause heads*.
3. **`meta_predicate/1`** declarations so `call/N` arguments are module-transparent (the context
   module travels on the goal).
4. **Move the redundant library predicates to `prelude/*.pl`** (lists, pairs, strings, apply
   already there): this is where the two W3 deviations of section 9.4 item 2 are paid off —
   `append(X, Y, Z)` fully open and `member(X, PartialList)` become the real two-clause definitions
   and start enumerating, and `label/1` should become a lazy v4 generator so the last name-keyed
   hop (`exportSingletons/1`, section 10.4) disappears.
5. **Autoload by predicate indicator** so a library module is consulted on first use rather than at
   `Engine` construction (`Prelude.load` is eager today: `apply.pl` + `coroutining.pl` on every
   `new Prolog()`).

Acceptance for W6 (design B.16): `lists:append/3` works; a user `partition/4` overrides the library
one; the module tests (the ISS-2025-0314 family) green on v4; and, as always, the full suite green
on **both** engines plus 20/20 example programs.

---

## 12. Wave W6 — modules and the Prolog prelude (v3.13.0, ISS-2025-0466..0471)

**Status**: done. Suite **1112/1112 on the default engine and on v4** (1083 pre-existing + 29 new);
**20/20 example programs on both**, output byte-identical apart from the `-Djprolog.engine=v4` the
runner script echoes.

### 12.1 What changed

| ISS | Change | Files |
|---|---|---|
| 0466 | the `Modules` owner (system / user / library, resolution order, imports, export enforcement); `Machine.Goal.module` + `ctxModule`; `Machine.qualifiedCall`/`callInModule`/`activate`/`callSystem`/`flatQualified`; `selectClauses` rewritten; the `modules.size() > 1` special case and its `raiseUnknownIfRequired` escape hatch **deleted**; `ModuleManager.getStamp()`/`touch()` | `v4/Modules.java` (new), `v4/Machine.java`, `v4/Engine.java`, `core/module/ModuleManager.java` |
| 0467 | autoload by predicate indicator: the `:- module/2` header index (textual scan, once per JVM), per-module lazy parse and per-JVM cache, standard operator table; `ClauseStore`'s flat library layer removed | `v4/Prelude.java`, `v4/ClauseStore.java`, `v4/Modules.java` |
| 0468 | `prelude/lists.pl` and `prelude/pairs.pl` (new); `append/3` fully open enumerates and `member/2`/`memberchk/2` extend an open tail (native, matching the clauses); first-argument indexing for module clauses (`Modules.Pred`) | `src/main/resources/prelude/lists.pl`, `pairs.pl`, `v4/NativeLibrary.java`, `v4/Modules.java` |
| 0469 | `meta_predicate/1`: mirroring, prelude directives, `'$mctx'/2`, `Machine.withMetaContext`, `addArgs` on a qualified callee | `v4/Modules.java`, `v4/Prelude.java`, `v4/Machine.java`, `core/module/Module.java`, `src/main/resources/prelude/apply.pl` |
| 0470 | `current_module/1` and the `defined_in`/`exported`/`imported_from` properties of `predicate_property/2` | `v4/ModuleBuiltins.java` (new), `v4/NativeBuiltins.java` |
| 0471 | `label/1`/`labeling/2` as cell-based v4 generators; `ClpfdV2Bridge.labelCells/3` | `v4/ClpfdNative.java` (new), `builtin/clpfd/v2/ClpfdV2Bridge.java`, `v4/NativeBuiltins.java` |

New tests: `src/test/java/it/denzosoft/jprolog/core/engine/v4/EngineV4ModulesTest.java` (29 tests,
same v4-select/restore pattern as the other `EngineV4*Test` classes).

Two existing tests gained an engine-aware branch for the deliberate `append(X,Y,Z)` change:
`BugFixVerificationTest.testISS0379_AppendFullyOpenDoesNotThrow` and
`EngineV4LibraryTest.testISS0453_ListPredicatesAllModes`.

### 12.2 How it works, in one page

1. **`Modules` on the `Engine`.** `system` (built-ins, no clauses), `user` (**the flat
   `ClauseStore`**) and the library modules, whose descriptors come from `Prelude.libraries()`.
   User-defined modules are **mirrored** from `core.module.ModuleManager`, which is still what
   `Prolog.consult` writes to (it is shared with the legacy and v2 engines); a new monotone
   `ModuleManager.getStamp()` tells the mirror when it is stale, and only non-`user` modules are
   copied, because `user` is the flat store by definition.
2. **The context module travels on the goal.** `Machine.Goal.module` (`null` == `user`) is set by
   `Machine.mg(...)` from the machine's `ctxModule`; the drive loop sets `ctxModule` from the goal
   it pops; a clause body carries its **defining** module; `findAll`, `runSubQuery`, `runOnce` and
   the `catch/3` frame save and restore it. A single-module program pays one null test.
3. **`selectClauses`** applies the order `M` -> `M`'s imports -> `user` -> autoload, and returns
   the defining module in `selModule` so `activate` can stamp the clause bodies with it. For the
   `user` context the first two steps are skipped entirely and the code is exactly what it was
   before the wave, plus the autoload step at the end.
4. **`stepN`** routes an indicator to `callUser` *before* the legacy registry when
   `Modules.overridesBuiltin(ctx, f, n)` says the calling context (or an autoloadable library) has
   clauses for it — the W3 rule, now module-aware.
5. **`Module:Goal`** is `Machine.qualifiedCall`: unwrap to the innermost qualification;
   `system:G` is a built-in dispatch (`callSystem`); `user:G` and an unknown module push `G` with
   that context; a known module that *defines* `f/n` answers only if it exports it; a known module
   that does not define it falls through to ordinary resolution in its own context. The flat `:/2`
   lookup of W4 (`Machine.callQualified`) is tried as a fallback and is unchanged for
   `Module:attr_unify_hook/2`.
6. **`meta_predicate/1`.** `Machine.withMetaContext` wraps the module-sensitive arguments of a
   declared predicate in `'$mctx'(CallerModule, Arg)` just before the head is unified;
   `stepN`'s `'$mctx'/2` branch pushes the inner goal with that module as its context, i.e. full
   internal resolution. `addArgs` keeps a qualification outermost, so `call(M:g, X)` is `M:g(X)`.
7. **Autoload.** `Prelude` reads each resource's `:- module(Name, [Exports])` header with a
   textual scan (no parser) into a JVM-wide `indicator -> module` index, and parses a module's
   clauses only on first reference, caching the result for the JVM. `Prelude.owner(f, n)` is the
   cheap negative on the hot path.

### 12.3 New invariants (add to section 3)

28. **`Goal.module` is the context module, and it is derived, never guessed.** `null` means `user`.
    The drive loop sets `ctxModule` from the goal it pops; every construct that pushes a *program*
    goal must stamp it (`Machine.mg`), a clause body must carry its **defining** module, and every
    nested drive must save and restore `ctxModule` exactly as it saves the goal stack. A construct
    that forgets makes a module predicate resolve in the wrong place — silently, and only when a
    second module exists.
29. **`user` is the flat clause store.** There is no separate storage for module `user`, and no
    "more than one module exists" mode switch anywhere. `Modules.localClauses` returns null for
    `user` on purpose.
30. **A library module is still the LAST resort, and `overridesBuiltin` is what enforces it.**
    `Machine.stepN` consults `Modules.overridesBuiltin` before `LegacyBuiltinAdapter` so a prelude
    or context-module definition is not shadowed by the registry entry of the same name; the
    resolution order inside `selectClauses` puts the library after `user`. Both halves are needed
    (invariant 15, now module-aware).
31. **A meta-argument travels in `'$mctx'/2`, never in `:/2`.** `M:G` is export-checked
    (ISS-2025-0314); a meta-argument going back into its own caller must see that module from the
    inside. Using `:` for both would make a library `maplist/3` unable to call a private helper of
    the module that called it.
32. **`member/2` and `append/3` exist twice and must stay observationally identical** — as the
    two-clause definitions of `prelude/lists.pl` (what `lists:member/2` runs and what the module
    layer would use if the native were stripped) and as native generators (what the unqualified
    call runs). `EngineV4ModulesTest.testISS0468_NativeAndPreludeListPredicatesAgree` is the guard.
33. **A library module's export list is its autoload key.** Two library modules must never export
    the same indicator: `Prelude.owner` keeps the first, and the second one's implementation
    becomes unreachable through autoload (this really happened — `lists` exporting `exclude/3`
    silently disabled `apply`'s). `testISS0467_LibraryModulesDoNotClaimTheSameIndicator` guards it,
    and `testISS0467_PreludeHeadersMatchTheClauses` checks that every header matches its file.

### 12.4 Measured (v2 = default engine vs v4, same session, loaded VM, best of 3)

Harnesses: `scratchpad/bench/AB6.java` (the 27 acceptance queries plus engine creation),
`scratchpad/bench/W6Bench.java` (1 M-element list operations) and `scratchpad/bench/Base3.java` /
`Base4.java` (clause-walk vs generator). Compile with
`javac -cp target/classes -d scratchpad/bench X.java`, run with
`java -Xss4m -Xmx3g -cp target/classes:scratchpad/bench X v2|v4`.

**Engine creation** — the number the wave had to protect (`new Prolog()` must not get slower):

| | v2 | v4 before W6 | v4 after W6 |
|---|---|---|---|
| `new Prolog()` (best / avg of 200) | 0.234 / 0.81 ms | 0.28 / 0.52 ms | **0.196 / 0.78 ms** |
| `new Prolog()` + first query | 0.24 / 0.85 ms | **8.04 ms** | **0.34 / 1.45 ms** |

`new Prolog()` itself is unchanged, because the v4 `Engine` is still created lazily on the first v4
query; what moved is the prelude, which used to be parsed in full at `Engine` construction and is
now indexed by a textual header scan (once per JVM) and parsed per module on first reference.

**1 000 000-element list operations** (each row includes the `numlist/3` that builds the list:
262 ms on v2, 287 ms on v4):

| Benchmark | v2 | v4 |
|---|---|---|
| `numlist/3` only (baseline) | 262 ms | 287 ms |
| `length/2` | 1084 ms | **693 ms** |
| `append/3` (+,+,-) | 1171 ms | **881 ms** |
| `reverse/2` | 890 ms | 1087 ms |
| `member/2` (last element) | 566 ms | 690 ms |
| `memberchk/2` (last element) | `existence_error` | **787 ms** |
| `nth1/3` (last element) | 463 ms | **355 ms** |
| `last/2` | 511 ms | 527 ms |
| `msort/2` | 2362 ms | **1325 ms** |
| `copy_term/2` | 1842 ms | **1246 ms** |
| `findall/3` + `member/2` | 2143 ms | **1945 ms** |

**Clause walk vs native generator** — this is the measurement that decided which library predicates
are Prolog and which stay native (design B.17 decision 4 versus the wave's "keep native generators
only where they are measurably faster"):

| 1 M elements, v4 | time |
|---|---|
| `append/3` (native generator) | 426 ms |
| `lists:append/3` (the prelude clauses) | 429 ms |
| user-written `myappend/3` (same two clauses, in the KB) | 1162 ms |
| `member/2` (native generator) | 110 ms |
| `lists:member/2` (the prelude clauses) | 276 ms |

and the same user-written `myappend/3` costs **7205 ms on v2** — so the v4 machine is 6x faster at
running a user's own list predicate, but a clause walk still costs 2-15x a Java spine walk because
it pushes one choice point per element. That is why `select/3`, `selectchk/3`, `nth0/3`, `nth1/3`,
`last/2`, `reverse/2`, `memberchk/2`, `length/2`, `msort/2`, `sort/2`, `sum_list/2`, `numlist/3`
and `copy_term/2` stayed native, and why `member/2` and `append/3` are native *and* Prolog: the
generators were extended to the relational modes rather than replaced.

Adding the first-argument index to module clauses (`Modules.Pred`) is part of the same story: it
took `lists:append/3` over a 1 M-element list from 3057 ms to 429 ms, because
`append([H|T], L, [H|R])` stops pushing a two-clause choice point per element.

**Acceptance queries** (`scratchpad/bench/AB6.java`, one fresh `Prolog` per row): **v4 27/27,
v2 11/27**.

| Query | v2 | v4 |
|---|---|---|
| `lists:append([1],[2],L), L == [1,2]` | **FAIL** | **OK** |
| `system:atom_length(abc, N), N == 3` | **FAIL** | **OK** |
| `user:foo(X), X == 7` | ok | ok |
| `user:user:foo(X), X == 7` (nested) | **FAIL** | **OK** |
| `apply:partition([X]>>(X>2), [1,2,3,4], I, E)` | **FAIL** | **OK** |
| user `partition/4` overrides the library | ok | ok |
| user `partition/4` override **and** `apply:partition/4` still callable | **FAIL** | **OK** |
| two modules exporting `p/1`, imported into two clients | **FAIL** | **OK** |
| `meta_predicate` context passing (two modules, same helper name) | **FAIL** | **OK** |
| export enforcement: `\+ sec:priv(_)` | **FAIL** | **OK** |
| export enforcement: `sec:pub(1)` | ok | ok |
| `current_module(user)` / `current_module(lists)` | `existence_error` | **OK** |
| `predicate_property(append(_,_,_), imported_from(lists))` | **FAIL** | **OK** |
| `findall(X-Y, append(X,Y,[1,2]), L), length(L, 3)` | ok | ok |
| `append(X,Y,Z), length(X,2), !` | **FAIL** | **OK** |
| `once((member(X, L), L = [a|_])), X == a` | **FAIL** | **OK** |
| `once((L = [1|T], member(2, L))), T = [H|_], H == 2` | **FAIL** | **OK** |
| `memberchk(a, L), L = [a|_]` | `existence_error` | **OK** |
| `C in 1..3, D #= C*2+1, label([C]), C == 1, D == 3` | ok | ok |
| `X in 1..5, labeling([max(X)], [X]), X == 5` | ok | ok |

### 12.5 Deviations from the W6 brief, and why

1. **`member/2` and `append/3` are native AND Prolog.** The brief asks for them (and for
   `select/3`, `memberchk/2`, `reverse/2`, `nth0/3`, `nth1/3`, `last/2`) as prelude clauses, and in
   the same sentence asks to "keep native Generators only where they are measurably faster on
   1 M-element lists". The measurement (12.4) says the generators are 2-15x faster on every one of
   them, because a clause walk pushes one choice point per element. Replacing them wholesale would
   have shipped a 2-25x regression into W8, when v4 becomes the default. So: the **semantics** the
   brief asks for were implemented (fully open `append/3` enumerates, `member/2` and
   `memberchk/2` extend an open tail), the generators were **extended** rather than deleted, and
   `member/2`/`append/3` also exist as the two-clause definitions of module `lists` — which is
   what `lists:member/2` runs, what a trace shows, and what documents the reference semantics.
   The remaining seven stayed native and are exported by `lists` without clauses, so
   `lists:last/2` still resolves. Invariant 32 is the maintenance obligation this creates.
2. **The built-in tables are still consulted before the clause layers.** Design B.10 puts `system`
   last in the resolution order, which would make a user definition of `atom_length/2` win. That is
   not JProlog's behaviour on any engine (`Prolog.consult` refuses such a clause outright,
   `Cannot redefine built-in predicate f/n`), and changing it is a cross-engine decision, not a
   module one. What the wave *did* implement is the part that matters: a **library** predicate is
   the last resort, so a user or module definition of `partition/4`, `maplist/3`, `member/2`, ...
   wins over the library and over the registry entry of the same name.
3. **A `meta_predicate/1` declaration in module `user` has no effect.** Qualification is applied
   only when the *defining* module is not `user`, because in a single-module program every
   meta-argument would be wrapped with `user` and then resolved in `user` anyway — pure overhead on
   the hot path. A declaration in a real module works normally.
4. **`coroutining.pl` carries no `meta_predicate` declarations.** `freeze(X, Goal)` therefore stores
   `Goal` exactly as written and `frozen/2` reports it unqualified, as it did in W4. Adding them
   would change the term `frozen/2` returns, which several tests compare literally; the goal still
   runs correctly because it falls through to `user`.
5. **`label/1` is lazy in delivery, eager in search.** The generator hands out one assignment per
   redo and trust-me pops on the last, but the DFS runs to completion first. A resumable DFS is
   impossible while the CLP(FD) store rolls back through the process-global legacy `Trail`: the
   machine calls `Trail.rollbackTo(cp.legacyMark)` before every redo, which would undo the
   half-finished search's own narrowing. A per-engine constraint store is W7 (design B.12, L-06).
6. **`exportSingletons/1` did not disappear — only labeling stopped using it.** `in/2`, the six
   `#`-comparisons and `all_different/1` are still legacy-bridged `BuiltIn` classes returning
   `Map<String,Term>`, and they still report a functionally determined variable their goal never
   mentions (`C in 1..3, D #= C*2+1, C #= 1` binds `D`). So `LegacyBuiltinAdapter.apply`'s
   `ClpfdV2Bridge.cellFor(name)` fallback is still needed and still exercised, and `Ctx.vars`,
   `Ctx.cells`, `varFor`, `onBindByName` and `domainTermForCell` remain keyed by
   `Variable.getName()`. Making the constraint-posting built-ins native is a W7/W8 item: they are
   arithmetic-expression compilers, not list walks, and nothing about the module system needs them.
7. **`ModuleManager` was not deleted.** It is the consult-time recorder for all three engines and
   the IDE reads it; `Modules` replaces it as the *resolver* on the v4 path only, and mirrors it.
   Deleting it belongs to W9, with the rest of the legacy package.
8. **`strings` and `aggregate` were not added as library modules.** The brief lists them
   "where Java versions are redundant"; they are not — `sub_atom/5`, `sub_string/5` and
   `aggregate_all/3` are already native v4 generators (W3) and a Prolog version would be strictly
   worse by the same measurement as 12.4. `yall` stays native as the brief says.

### 12.6 Where wave W7 starts

> **W7 is done (v3.14.0)** — see section 13 for what was implemented, how it deviates from the plan
> below, and where **W8** starts. This section is kept as the historical brief.

W7 is **engine state: streams, operators, writer** (design **B.11** and **B.12**, B.16 row W7).
Nothing in W6 blocks it; the piece it needs — a per-`Engine` owner object hanging off
`core.engine.v4.Engine`, mirrored from or replacing a process-global — is exactly the shape
`Modules` established in this wave, and `Engine` already has the constructor slot for it.

In dependency order:

1. **`Streams` / `PrologStream` per engine (B.11)** — `Engine.streams()`, with
   `user_input`/`user_output`/`user_error` bound to the thread-local overrides the IDE already
   uses and `current_input`/`current_output` per machine. A text stream decodes through a private
   buffer that tracks byte position, character count, line number and line position, so `seek/4`,
   `set_stream_position/2` and `stream_property(S, position(P))` are correct on text streams
   (limit **L-07**: today `get_char(S,C1), seek(S,0,bof,_), get_char(S,C2)` gives `C2 = e` after
   `C1 = h`). Full `stream_property/2`, `set_stream/2`, `stream_position_data/3`,
   `character_count/2`, `line_count/2`, `line_position/2`. Delete the `StreamManager` statics
   (limit **L-06**); this is also what finally closes **LIM-025**, because `with_output_to/2`
   currently has to swap `System.out` as well as the thread-local stream (deviation 5 of section
   9.4).
2. **`Ops` per engine and per module (B.12)** — one operator store on the `Engine`, scoped by
   module (`op/3` inside a module file is module-local, `system` operators are global), read by
   the parser, `current_op/3`, `write_term/2,3` and the `.jpc` writer. `OperatorDefinition`'s
   static tables go away (LIM-034), which is what makes `current_op/3` see the operators a
   consulted file declared, and lets `current_op/3` become a native generator (deviation 3 of
   section 9.4). Note that `Prelude` deliberately parses the library with a **standard**
   `OperatorTable`; keep that when the store moves.
3. **`Writer` (B.12)** — ISO `write_term/2,3` with the complete option set (`quoted`,
   `ignore_ops`, `numbervars`, `max_depth`, `portray`, `cycles`, `variable_names`, `spacing`) over
   the engine's operator store, plus `print_message/2` and a `portray_clause/1` built on it. The
   CLI and the IDE then print answers with `quoted(true), numbervars(true), portray(true)` and
   print residual goals after the bindings (limit **L-11**; `Prolog.residualGoals(Map)` and
   `ClpfdV2Bridge.domainTermForCell` are already there from W4, with no consumer). Design decision
   5 (B.17) is approved: this **changes the text of every CLI answer** (`X = 'a b'-1` instead of
   `X = -(a b, 1)`, `_A` instead of `_R1_A`), so budget for the example-output diff — the
   "byte-identical v2/v4 example output" oracle this wave still relies on will no longer hold, and
   `test_all_examples.sh` output must be re-baselined once, deliberately.
4. **Per-engine `Spy` and `Profiler`** (the rest of LIM-034), which are one-line moves once the
   `Engine`-owner pattern is in place.

Acceptance for W7 (design B.16): seek on text streams correct; `stream_property/2` complete;
`current_op/3` sees operators declared by a consulted `:- op/3`; two `Prolog` instances fully
isolated in streams, operators, spy points and profiler counters; and, as always, the full suite
green on **both** engines plus 20/20 example programs.

---

## 13. Wave W7 — engine state: streams, operators, writer (v3.14.0, ISS-2025-0472..0477)

**Status**: done. Suite **1157/1157 on the default engine and on v4** (1112 pre-existing + 46 new);
**20/20 example programs on both engines**, byte-identical *between the two engines*. The example
**text** changed once, deliberately (design decision 5 — see 13.5).

**W7 is the first wave that is mostly NOT v4-only.** Design decision 1 (B.17) keeps the v2
`MachineSolver` selectable for one release after v4 becomes the default, and the ~400 legacy
built-ins reach streams and operators through static facades — so the per-engine state and the new
writer had to work for **both** engines. They live in `core.engine.v4` as the design says, and the
old statics (`StreamManager`, `OperatorDefinition`, `Spy`, `Profiler`) became facades that delegate
to the engine current on the calling thread, exactly as `PrologFlags` has since ISS-2025-0437.

### 13.1 What changed

| ISS | Change | Files |
|---|---|---|
| 0472 | `EngineState` (the thread-current per-engine state) + `Streams` + `PrologStream`; `StreamManager` becomes a facade; the text decoder with byte/char/line/column tracking (**L-07**); `'$stream'(N)`; per-thread `current_input`/`current_output`; capture without `System.out` (**LIM-025**) | `v4/EngineState.java`, `v4/Streams.java`, `v4/PrologStream.java` (all new), `builtin/io/StreamManager.java` (rewritten as a facade), `builtin/io/{Open,Close,GetChar,GetCode,PeekChar,PeekCode,GetByte,PeekByte,PutByte,AtEndOfStream,FlushOutput,SetInput,SetOutput,CurrentInput,CurrentOutput,Seek,StreamPosition,SetStreamPosition,Read,ReadTerm,IOStreamUtils,WithOutputTo,Format}.java`, `v4/NativeControl.java`, `builtin/debug/{Spy,NoSpy,Leash,Debugging}.java`, `core/engine/Prolog.java` |
| 0473 | complete `stream_property/2`; `set_stream/2`, `stream_position_data/3`, `character_count/2`, `line_count/2`, `line_position/2`, `current_stream/3`; `read_term` `term_position`; the deterministic parser nesting limit | `builtin/io/StreamProperty.java` (rewritten), `builtin/io/StreamInfo.java` (new), `builtin/io/ReadTerm.java`, `core/parser/v2/TermReader.java`, `core/parser/TermParser.java`, `builtin/term/{TermToAtom,AtomToTerm}.java`, `core/engine/BuiltInFactory.java` |
| 0474 | `Ops`: one operator store per engine, module-scoped for `current_op/3`; `OperatorDefinition`'s three static stores deleted; `current_op/3` sees a consulted `:- op/3` | `v4/Ops.java` (new), `builtin/system/OperatorDefinition.java` (rewritten as a facade), `core/engine/Prolog.java`, `core/write/v2/TermWriter.java` |
| 0475 | `Writer`: the iterative, cycle-safe ISO term writer with the complete option set; `portray_clause/1,2`; `print_message/2`; `print/1,2` gains `portray` | `v4/Writer.java` (new), `builtin/io/{WriteTerm,WriteOptions,Print,PortrayClause,PrintMessage,WriteCanonical}.java`, `core/util/TermFormatter.java` (now a facade), `core/engine/BuiltInFactory.java` |
| 0476 | `Answer`: console answer rendering (quoted, operators, `_A` names, residual goals) — **L-11** | `v4/Answer.java` (new), `PrologCLI.java`, `editor/RunPanel.java`, `editor/DebugPanel.java` |
| 0477 | spy points and profiler counters per engine (the tail of **LIM-034**) | `v4/EngineState.java`, `builtin/debug/Spy.java`, `core/engine/Profiler.java`, `test/builtin/DebuggingTest.java` |

New tests: `core/engine/v4/EngineV4StreamsTest.java` (25) and `EngineV4WriterTest.java` (20).
Both are mostly **engine-neutral** and only select v4 where the feature is v4-only (residual goals,
rational trees), restoring the previous selection in `tearDown`.

### 13.2 How it works, in one page

1. **`EngineState` is the owner.** Each `Prolog` constructs one (`getEngineState()`) holding
   `Streams`, `Ops`, the spy-point set and the profiler counters, and installs it as the
   thread-current state in `enterState()` / `exitState()` around every `solve`/`consult` entry
   point, next to `PrologFlags`. Code with no engine in scope (a directly instantiated built-in, a
   unit test) sees a process-wide default, exactly as the flag store does.
2. **A text stream owns its decoder.** `PrologStream` keeps a `ByteBuffer` plus a `CharsetDecoder`
   and decodes **one code point at a time** — the one-character `CharBuffer` is the point: a bigger
   one lets the decoder consume as many bytes as fit and the extra characters are dropped while the
   byte position runs ahead (that is exactly how the first cut of this class turned
   `get_char, get_char` on `"foo(bar)"` into `f` then `b`). A supplementary character does not fit
   in one char, so the decoder answers OVERFLOW consuming nothing and the second attempt uses a
   two-char buffer. The reported position is `basePos + buffer.position()` minus a peeked
   character's byte width, so it is exact; `reposition(pos)` clears the buffer, resets the decoder,
   drops the peek and re-scans the prefix (bounded at 8 MB) to recompute the counters.
3. **Peek is a decoder lookahead, not a reader pushback.** `pendingCp` + `pendingBytes`. A
   reposition simply discards them, which is why the L-07 repro is fixed for
   `peek_char, seek, get_char` as well as `get_char, seek, get_char`.
4. **`Ops` is one `OperatorTable` plus an ownership map.** The table is the object
   `Prolog.getOperatorTable()` returns, so every existing consumer (both parsers, `.jpc`, the IDE
   formatter) is unchanged; the map records which module declared each non-standard operator and is
   what makes `current_op/3` module-scoped. `Prolog.processOpDirective` now goes through
   `Ops.define`, which is the whole fix for "`current_op/3` does not see a consulted `:- op/3`".
5. **`Writer` is a work-stack machine.** Items are: a term with its context priority and depth, a
   literal string, a list continuation, an operator token, and a pop-from-path marker. No Java
   recursion anywhere. Cycles: an `IdentityHashMap` of the current path for general compounds, and
   **Brent's algorithm** on list spines so a 1 M-element list costs O(1) extra memory. Operator
   spacing is decided against the characters already in the `StringBuilder` on the left (exact) and
   a bounded leftmost-spine walk (`firstChar`) on the right.
6. **`TermFormatter` is a facade over `Writer`.** That single change puts the new writer behind
   `write/1,2`, `writeln`, `writeq`, `print`, `format ~w/~q/~p`, `term_to_atom/2` and both engines'
   four-port trace output at once — and the whole suite is the oracle that the rendering is
   unchanged for the options those callers use.
7. **`Answer` renders a solution.** Bindings at priority 699 (the right argument of `=/2`, so a
   conjunction prints as `(p,q)`) with `quoted(true), numbervars(true), portray(true)`, fresh
   variables named `_A`, `_B`, ... **keyed by the engine's variable NAME rather than by object
   identity** — the legacy and v2 engines hand back a renamed copy per binding, so `X = f(Y)` really
   does contain two distinct `Variable` objects both called `Y` — then the residual goals from
   `Prolog.residualGoals(solution)`.

### 13.3 New invariants (add to section 3)

34. **The writer must stay iterative and cycle-safe.** It is what prints a rational tree, which the
    v4 engine creates happily (design decision 2). A new special case that recurses on an argument
    reintroduces `resource_error(stack_overflow)` on a long list; one that forgets to put a compound
    on the path (or to pop it) either loops forever on a cycle or prints `...` for a shared subterm.
    List spines use Brent instead of the path map **on purpose** — the map would cost 32 MB on a
    1 M-element list.
35. **A stream argument is a TERM.** Resolve it with `IOStreamUtils.inputStream` /
    `outputStream` / `StreamManager.stream(Term)`, which accept `'$stream'(N)`, the legacy
    `stream(A)` wrapper, an atom alias, the `stream_<id>` handle and the reserved names. A built-in
    that only recognises an `Atom` silently treats `open/3`'s result as "not a stream" — that is how
    `read_term(S, T)` briefly became `read_term(-Term, +Options)` and blocked on stdin.
36. **Everything a built-in prints goes through `StreamManager.out()` (or `resolveOutput`).** The
    capture used by `with_output_to/2`, `format/3 atom(A)` and `format ~@` is now *only* the
    thread-local override; a `System.out.print` escapes it and re-opens LIM-025.
37. **Only BIND a stream argument that was unbound.** `stream_property/2` and `current_stream/3`
    take a stream that may be named by an alias atom, which of course does not unify with the
    canonical `'$stream'(N)`. Unifying unconditionally made `stream_property(myin, alias(A))` false
    for every stream opened with `alias(myin)`.
38. **The stream-handle counter is process-global on purpose.** `stream_1001` must denote at most
    one stream in the JVM, or a stale per-alias cache in an older built-in hands engine B the closed
    file of engine A. (This really happened while the handle counter was per engine.)

### 13.4 A/B evidence

Harnesses in `scratchpad/bench/`: `Probe.java` / `Probe2.java` / `Probe3.java` (behaviour),
`AnsAB.java` (the answer-format diff), `Nest.java` (the parser nesting limit), `W7Bench.java`
(timings). Compile with `javac -cp target/classes -d scratchpad/bench X.java`, run with
`java -Xss4m -Xmx2g -cp target/classes:scratchpad/bench X [v2|v4]`.

**Acceptance items, before and after** (all on the DEFAULT engine unless marked):

| item | before W7 | after W7 |
|---|---|---|
| `get_char(S,C1), seek(S,0,bof,_), get_char(S,C2)` | `C1 = h, C2 = e` | `C1 = h, C2 = h` |
| `peek_char(S,_), seek(S,0,bof,_), get_char(S,C)` | stale buffer | `C = h` |
| `stream_property/2` properties | 6, standard streams only | 11 + one per alias, every open stream |
| `stream_property(myin, alias(A))` after `open(..,[alias(myin)])` | **false** | `A = myin` |
| `line_count/2`, `line_position/2`, `character_count/2` | did not exist | `L = 2, LP = 1, CC = 7` after 7 chars of `"hello\nworld\n..."` |
| `set_stream/2`, `stream_position_data/3`, `current_stream/3` | did not exist | implemented |
| `read_term(S,T,[term_position(P)])` | did not exist | `P = '$stream_position'(14,2,0,14)` for the second term |
| engine B sees engine A's stream alias | **yes** | no |
| engine B sees engine A's `op/3` | **yes** | no |
| engine B sees engine A's spy point / profiler counters | **yes** | no |
| two threads with different `current_output` | one process-wide table | each writes its own file |
| `current_op(P,T,X)` after `:- op(777,xfx,X)` in a consulted file | **false** | `P = 777, T = xfx` |
| `term_to_atom(T, <5000 nested parens>)` | `resource_error(stack_overflow)` from the solve wrapper (and a silent **failure** on the legacy-parser path) | `error(resource_error(parser_nesting), read)` |
| `X = f(X), write(X)` (v4) | loops forever | `f(...)` |
| `X = f(X), write_term(X,[cycles(true)])` (v4) | loops forever | `@(_S1,[_S1=f(_S1)])` |
| `write` of a 1 000 000-element list | iterative already, but not cycle-safe | 6 888 897 chars, 322 ms (v4) / 550 ms (v2) |
| `write_term` options | `quoted`, `ignore_ops`, `numbervars`, `max_depth` (in a second, operator-blind formatter) | the complete set, one writer |
| `portray_clause/1,2`, `print_message/2` | did not exist | implemented |
| `print/1,2` portray hook | documented as unsupported | works |
| `with_output_to/2` capture | swapped `System.out` | thread-local override only |

**Timings** (same session, loaded VM, best of 3):

| | v2 | v4 |
|---|---|---|
| `new Prolog()` + first query (best / avg of 200) | 0.277 / 19.3 ms | 0.273 / 6.8 ms |
| `write` of a 1 000 000-element list | 550 ms | 322 ms |
| `writeq` of a 1 000 000-element list | 332 ms | 291 ms |
| `get_char/2` over a 220 000-character file | 482 ms | 287 ms |
| `findall(current_op/3)` over the whole table | < 1 ms | < 1 ms |

(The averages include JIT warm-up and GC; the "best" column is the number to compare. Nothing here
regressed: the writer is the same code on both engines and the stream decoder replaced a
`PushbackReader` with a `ByteBuffer` + `CharsetDecoder` at the same or better cost.)

### 13.5 The example-output re-baseline

Design decision 5 changes the text of every CLI answer, so the "byte-identical example output"
oracle had to be re-baselined once. It was, deliberately, and the *cross-engine* half of the oracle
still holds: **v2 and v4 produce byte-identical example output**, and both are 20/20.

`scratchpad/bench/AnsAB.java` re-runs each example program's queries and prints the OLD rendering
(`Term.toString()`, what the console used) next to the NEW one. **8 of the 16 example programs that
produce bindings print different text**:

| example | query | before | after |
|---|---|---|---|
| `test_02_unification.pl` | `person(john, Age, Address)` | `Address = address(street(Main St),city(boston))` | `Address = address(street('Main St'),city(boston))` |
| `test_04_lists.pl` | `my_append([1,2],[3,4],L)` | `L = [1, 2, 3, 4]` | `L = [1,2,3,4]` |
| `test_08_term_manipulation.pl` | `analyze_term(person(john,25), A)` | `A = analysis(person, 2, [john, 25])` | `A = analysis(person,2,[john,25])` |
| `test_09_meta_predicates.pl` | `findall(S, student(S,_,_), L)` | `L = [...], _G516 = _G516, Student = Student` | `L = [...]` |
| `test_10_string_atom.pl` | `atom_chars(hello, Chars)` | `Chars = [h, e, l, l, o]` | `Chars = [h,e,l,l,o]` |
| `test_14_dcg_simple.pl` | `number_manual(N,[49,50,51],[])` | `N = [49, 50, 51]` | `N = [49,50,51]` |
| `test_16_sorting.pl` | `sort([3,1,4,1,5], S)` | `S = [1, 3, 4, 5]` | `S = [1,3,4,5]` |
| `test_11_database.pl` | `retract(score(alice, _))` | `_G12 = 95.` | `true.` |

(The `test_11` row is why the runner script's "Successful queries" count for that program went from
1 to 2: the answer is now the `true.` it should always have been.)

Three kinds of change: the writer does not put a space after an argument separator unless
`spacing(next_argument)` is asked for (ISO, and what `write/1` has always produced inside a term —
only the *top level* used `Term.toString()`); a spurious `Var = Var` line for a query variable that
came back unbound is gone; and an engine-internal `_`-prefixed key (an anonymous `_` that the
solution map happened to carry) is no longer reported as a binding. The eight other programs are
unchanged. The change is much more
visible on terms the examples do not use: `X = 'a b'-1` printed `X = -(a b, 1)` and now prints
`X = 'a b'-1`; `Body = (p,q)` printed `Body = ,(p, q)`.

### 13.6 Deviations from the W7 brief, and why

1. **A module-local operator is still installed in the shared parser table.** Design B.12 wants the
   parser to read a module-scoped store. What is module-scoped here is the *visibility* to
   `current_op/3` (which is the observable ISO/SWI describe, and what
   `RefactorIssuesTest.testR2_operatorLocalToModule` pins). A JProlog session consults everything
   into one operator space and every already-read clause depends on it; narrowing *parsing* to the
   declaring module would change how existing programs read and is a cross-engine decision, not a
   W7 one. The `Ops` API already carries the module (`tableFor(module)`), so the change is a small
   one when it is wanted.
2. **`current_op/3` is still not a lazy native generator.** Deviation 3 of section 9.4 said W7 would
   make it one once the store moved. The store moved, but the table holds ~50 entries and
   `findall(current_op/3)` over all of them is under a millisecond, so there is nothing to make
   lazy. It stays a bridged built-in (the last item of limit L-08 that is not a real cost).
3. **`Engine.streams()` / `Engine.ops()` delegate to the `Prolog`'s `EngineState`.** The design puts
   them on the v4 `Engine`. They *are* reachable there, but the owner has to be the `Prolog`
   instance because the legacy and v2 engines need the same state and the v4 `Engine` is created
   lazily on the first v4 query. This is the direct consequence of design decision 1.
4. **The counters of an OUTPUT stream count UTF-8 lead bytes, not decoded characters**, and
   `user_output`/`user_error` are not counted at all (they are `System.out`/`System.err`, which the
   engine does not own and must not wrap — tests and the IDE redirect them). `line_position/2` on
   `user_output` therefore reports 0. Input streams, where the counters matter for `stream_property`
   and `read_term(term_position)`, are exact.
5. **`recountTo` after a reposition is bounded at 8 MB.** Seeking into a larger file leaves the
   character/line counters at a byte-derived approximation rather than re-scanning gigabytes. The
   byte position is always exact.
6. **`print_message/2` does not consult `message_hook/3` or a message catalogue.** The design asks
   for "minimal"; JProlog has no `message/1` DCG layer to hang one on.
7. **`write_canonical/1,2` does not number variables.** ISO says `write_canonical` implies
   `quoted(true), ignore_ops(true)`; SWI also emits `A`, `B`, ... for variables. JProlog prints the
   variable's own name, as it did before this wave, so no existing output changes.
8. **`quoteAtom` doubles the quote (`'don''t'`) rather than escaping it (`'don\'t'`).** Both are
   ISO; this is what `TermFormatter` produced and what the suite pins.

### 13.7 Where wave W8 started

> **W8 is done (v4.0.0)** — see section 14 for what was implemented, how it deviates from the plan
> below, and where **W9** starts. This section is kept as the historical brief.

W8 is **the default switch and threads** (design B.13, B.16 row W8). Nothing in W7 blocks it, and
W7 removed two of its obstacles: the per-engine state a second `Machine` on another thread will
need already exists, and the console/IDE answer rendering is now shared code (`Answer`), so
switching the default cannot change it again.

In dependency order:

1. **Make v4 the default.** Flip the default in `Prolog` (`USE_V4_ENGINE` when
   `jprolog.engine` is unset), keep `-Djprolog.engine=v2` / `=legacy` selectable for one release
   (design decision 1), and run the whole suite plus the examples on **all three** settings. Budget
   for the deliberate v4 divergences already documented in LIM-037 and pinned by engine-aware
   branches in `BugFixVerificationTest` and `EngineV4LibraryTest`: rational trees succeed,
   `setup_call_cleanup/3` runs `Cleanup` after the LAST solution, `append(X,Y,Z)` fully open
   enumerates, `member/2` extends an open tail, and coroutining does not cross queries. The
   *engine-aware branches themselves* are what needs auditing — several read
   `Prolog.isUsingV4Engine()`, which will start answering true by default.
2. **Threads on v4 machines** (limit **LIM-024**, and deviation 7 of section 9.4 — the last path
   from a v4 query into `QuerySolver.solveInternal`). `thread_create/2,3`, `thread_join/2`, the
   message queues, `concurrent/3`, `concurrent_maplist/N` and `first_solution/3` must each get a
   **new `Machine` over the same `Engine`**: the clause store is already thread-safe by generations,
   `Modules`/`Tabling`/`Ops` are concurrent maps, and `Streams` already gives each thread its own
   `current_input`/`current_output`. Two things to watch: `Bindings` lives in shared `Variable`
   cells, so a term must not be passed between machines without copying, and
   `Machine.onOwnerThread()` (the current fallback) should become an assertion once every path has
   its own machine.
3. **The debugger without disabling fast paths** (limit **L-13**, deviation 4 of section 5). B.6
   wants `=/2`, `is/2`, the comparisons and the type checks to keep their inline paths and emit the
   four ports natively. The current arrangement is what makes the v2 and v4 trace output
   byte-identical, which has been the regression oracle for W3-W7 — so the port emission has to be
   proved byte-identical *before* the fast paths are re-enabled, not after. `DebuggingTest`,
   `test-debug.sh` and the IDE debugger checklist in `docs/reports/report-debug-features.md` are the
   acceptance set, plus the design's "trace mode <= 1.5x slower".
4. **Optional, cheap now**: re-land first-argument indexing on the v2 engine is NOT wanted (v2 is
   being retired), but `Modules.overridesBuiltin` and `Prelude.owner` are on the v4 hot path and
   worth a profile once v4 is the default.

Acceptance for W8 (design B.16): full suite + `test_all_examples.sh` + the IDE debugger manual
checklist green on v4 **as the default**; trace mode <= 1.5x slower; and the suite still green under
`-Djprolog.engine=v2` and `=legacy`.


---

## 14. Wave W8 — the default switch, threads and the debugger (v4.0.0, ISS-2025-0478..0483)

**Status**: done. Suite **1204/1204 on the default engine (v4) and under `-Pengine-v2`** (1158
pre-existing + 46 new); **20/20 example programs on both engines**, with every per-program
"Successful queries" count unchanged.

### 14.1 What changed

| ISS | Change | Files |
|---|---|---|
| 0478 | **v4 is the default engine**; `jprolog.engine` defaults to `v4` and only the literal `v2`/`legacy` select an older one; the `engine-v4` Maven profile is replaced by `engine-v2`; every engine-aware branch audited | `core/engine/Prolog.java`, `pom.xml`, `test/.../V2EngineIntegrationTest.java`, `test/builtin/BugFixVerificationTest.java` |
| 0479 | threads on v4 machines: `core.engine.v4.Workers`, `QuerySolver.solveInWorker`, `thread_create/2,3` really runs its goal, `thread_join/2` status terms, aliases, per-thread queues carrying TERMS, `thread_get_message/1` | `v4/Workers.java` (new), `v4/SolverFacade.java`, `core/engine/QuerySolver.java`, `builtin/threading/ThreadPredicates.java` |
| 0480 | the `concurrent_*` family + `first_solution/3` on worker machines; the `CollectionBuiltInAdapter` wrapper removed for them; `concurrent_maplist/2,3,4` one entry dispatching on arity; parent interrupt cancels workers; `Machine.assertOwnerThread`; `internalSolveCount()` volatile | `builtin/threading/ConcurrentPredicates.java`, `core/engine/Prolog.java`, `core/engine/BuiltInFactory.java`, `v4/Machine.java`, `v4/SolverFacade.java`, `core/engine/QuerySolver.java` |
| 0481 | the tracer/debugger without disabling fast paths (L-13): `isInlineBuiltin`, native ports for the inline table, `Machine.iteTraced` for `once/ignore/forall`, ports inside `betweenNative`, lazy `DebugEvent`, `DebugController.needsGoalSnapshot()` | `v4/Machine.java`, `core/engine/DebugController.java` |
| 0482 | trace memory and the port depth: `CP.altsTaken` + the deterministic trust-me pop while tracing, `Machine.portDepth`, capped indentation, `tracePort` without `Unify.resolve` | `v4/Machine.java`, `v4/LegacyBuiltinAdapter.java` |
| 0483 | the non-interactive console (`System.console() == null`, `--batch`, `-q`) | `PrologCLI.java` |

The sandbox was widened in the same wave (tagged ISS-2025-0479): `builtin.threading` joined
`UNSAFE_BUILTIN_PACKAGES`, so `Prolog.enableSafeMode()` now also removes `thread_create/2,3`, the
message queues and the whole `concurrent_*` family. A JVM thread is a host resource like a process
or a socket, and each worker carries its OWN inference-budget counter — harmless while
`thread_create` did nothing, a denial-of-service surface now that it runs goals.

New tests: `core/engine/v4/EngineV4ThreadsTest.java` (15), `core/engine/v4/EngineV4TraceTest.java`
(25 — 16 pinned trace oracles, the debugger port stream, the two cost regressions and five
programmatic IDE-debugger contract tests) and `test/cli/PrologCliBatchTest.java` (6).
`EngineV4LibraryTest.testISS0450_NoBuiltinReachesTheRecursiveSolver` gained five concurrency queries.

### 14.2 How it works, in one page

1. **The default switch.** `Prolog.ENGINE_PROPERTY` is read once; `USE_V4_ENGINE` is true unless it
   is literally `v2` or `legacy`, `USE_V2_ENGINE` unless it is literally `legacy`. v4 wins over v2,
   so `setUseV4Engine(false)` drops to v2 and `=legacy` drops all the way down. That is why the two
   tests that reached an old engine by clearing ONE flag had to be fixed: they were silently going
   to run on v4 and stop testing what they name.
2. **A worker is a `Machine`, not a solver call.** `Workers.run(engine, budget, goal, sink)`
   `copy_term`s the goal (recording original cell -> copy), installs the engine's `PrologFlags` and
   `EngineState` as the worker thread's current state, builds a `Machine` with a **new**
   `ResourceGuard(budget)`, drives it, and for each solution resolves each copy cell and copies the
   value back out under the ORIGINAL cell's name. Nothing but the `Engine` is shared.
3. **`solveInWorker` is the seam.** `QuerySolver.solveInWorker(goal, bindings, solutions,
   cutStatus, maxSolutions)` defaults to the pre-W8 behaviour (`solve/4`, i.e. the recursive
   algorithm), so v2 and legacy are untouched; `SolverFacade` overrides it onto `Workers`. Every
   concurrency built-in calls it instead of `solve/4`, and `SolverFacade.solve/4` now asserts it is
   on the owner thread.
4. **The ports come from the machine.** `stepN` no longer skips its inline table when a
   `DebugController` is attached. `isInlineBuiltin(f, n)` answers, before the work, whether
   `solveBuiltin` will handle the goal (the Call port must precede execution); the built-in then
   gets Call + Exit/Fail and no choice point. `once/ignore/forall` go through `iteTraced`, which
   emits Call, pushes a **port-only frame** underneath the construct's own choice point (so the
   Fail port survives the commit that cuts it), and puts an Exit marker on the continuation.
   `between/3` emits the four ports from its own generator, like `lengthEnumerate`.
5. **Depth and memory.** `advance` now trust-me pops an exhausted frame that handed out exactly one
   alternative even while tracing (`cp.altsTaken == 1`), which is what makes traced memory linear
   in the number of OPEN calls. That breaks `cps.size()` as a depth, so the depth is
   `Machine.portDepth`, assigned by every port. The indentation is capped at 40 levels.
6. **The console.** `PrologCLI` computes `batch` once (`System.console() == null || --batch || -q`)
   and, in batch mode, prints ` ;` and continues instead of calling `reader.readLine()`.

### 14.3 New invariants (add to section 3)

39. **A term must be COPIED before it crosses a machine boundary.** Bindings live in the
    `Variable` cells, so a cell reachable from two machines would be bound with neither machine's
    trail knowing. `Workers` copies the goal in and every answer out; the message queues copy on
    send and on receive. A new cross-thread hand-off must do the same.
40. **A worker gets its OWN `ResourceGuard`**, constructed with the parent's limit — never the
    parent's object. Sharing it would make two workers race on one step counter and would poll the
    wrong thread's interrupt flag.
41. **A new port site takes its depth from `enterPort()` and returns it by ASSIGNING it** in
    `portExit`/`portFail` (or, for a Redo, `cp.traceDepth + 1`). Incrementing and decrementing
    drifts the moment a frame is cut away or a ball is thrown past it; assignment is self-healing.
42. **Never re-introduce a "skip this when `debugController != null`" branch.** L-13 is exactly
    that bug: the debugged run must execute the same code as the undebugged one. If a fast path has
    no ports, give it ports.
43. **A built-in that must reach the per-query `SolverFacade` may not be wrapped in a
    `CollectionBuiltInAdapter`** in the `Prolog` constructor. The wrapper pins the engine's shared
    recursive `QuerySolver` at registration time; every dispatcher already handles a
    `BuiltInWithContext` and passes the right solver.
44. **A built-in that spawns a thread, a process, a socket or a file handle belongs in a
    safe-mode-denied package.** That rule pre-dates W8; what W8 showed is that it applies the
    moment a predicate *starts* touching the host — `builtin.threading` had been harmless for four
    releases because `thread_create/2` did not run its goal.
45. **A deterministic frame emits no `Fail` after its `Exit`.** It handed out one alternative, it
    is exhausted, it is popped. A future change that keeps such a frame "so the trace looks
    complete" reintroduces the quadratic trace.

### 14.4 The trace oracle: what changed and why

The oracle was recorded on the pre-W8 build for 16 programs (user predicates, inline built-ins,
backtracking with Redo/Fail, cut, negation, catch/throw, findall, `once`/`between`,
`forall`/`ignore`, the native list library, coroutining, tabling, if-then-else, maplist, the
database) and is now pinned line for line in `EngineV4TraceTest`. The output is **not** identical
to the pre-W8 output. Every difference is deliberate:

| # | Difference | Why it is right |
|---|---|---|
| 1 | The inline built-ins (`X = 1`, `Y is 1+2`, `3>2`, `integer(3)`, `atom(foo)`, `var(X)`, …) now have Call/Exit/Fail ports under `trace/0` | They had **none** there, while the IDE debugger saw them (it forced them through the bridge). The two modes now show the same ports, which is what invariant 10 asks for. |
| 2 | The depth is the call depth, so the trace nests (`app/3` recursion is 0,1,2 instead of everything at 0 after the pop) | `cps.size()` stopped being a nesting measure the moment deterministic frames are popped, and the IDE compares depths for step-over/step-out. |
| 3 | No `Fail` after a deterministic `Exit` (the trailing `Fail: app([],[3],_G8)` block is gone) | The frame has no alternatives; a `Redo` can never happen, so the `Fail` was a phantom. SWI drops such a frame too (last-call optimisation). This is what makes tracing usable. |
| 4 | `once(G)` no longer shows `Fail: G` between `Exit: G` and `Exit: once(G)` | That was an artifact of the bridged sub-solve exhausting eagerly inside `once/1`. The native `once` commits with a cut, so the inner goal never fails. |
| 5 | The indentation stops growing after 40 levels (the depth in parentheses is still exact) | A 1 000 000-deep tail recursion would otherwise print a two-million-character indent — quadratic output. |

Nothing else moved: ports, order, goal text and the Redo/Fail of genuinely nondeterministic goals
are unchanged, which is what the 16 pinned oracles assert.

### 14.5 Measured (same session, loaded VM, best of 3 warm)

Harness: `scratchpad/w8/TB.java` (`nrev` 30x100, `loop`), `scratchpad/w8/L2.java` (`loop(N)` under
trace at several N), `scratchpad/w8/TraceOracle.java` (the port streams) and
`scratchpad/w8/TH.java` / `RS.java` (the thread matrix).

The **"before W8" column is a faithful reconstruction of the v3.14.0 `Machine`**: a copy of the
current file with the four W8 trace changes reverted — the deterministic trust-me pop, the
`portDepth` call depth, the inline fast paths under trace/debug, and `tracePort`'s goal copy plus
the indentation cap — compiled into `scratchpad/w8/pre/out` and put in front of `target/classes`.
It is verified: `TraceOracle` run against it reproduces the pre-W8 trace oracle **byte for byte**,
so the numbers below isolate exactly this wave. Same session, loaded VM, best of 3 warm.

| | before W8 | after W8 |
|---|---|---|
| `nrev 30x100` untraced | 16 ms | 17 ms (unchanged) |
| `nrev 30x100` under `trace/0` (output to a null stream) | 10779 ms (**567x**) | **255 ms (~10-18x)** |
| `nrev 30x100` with a `DebugController` attached, no listener | 93 ms (**5.8x**) | **21 ms (~1.1x)** |
| `loop(20000)` under `trace/0`, `-Xmx1g` | 1850 ms | **479 ms** |
| `loop(50000)` under `trace/0`, `-Xmx1g` | 8364 ms | **478 ms** |
| `loop(100000)` under `trace/0`, `-Xmx1g` | 33293 ms | **738 ms** |
| `loop(1000000)` under `trace/0`, `-Xmx1g` | **did not complete in 110 s** | **4597 ms** |
| `loop(1000000)` under `trace/0`, `-Xmx384m` | OOM | **4.9 s, 4 MB retained afterwards** |
| `loop(1000000)` untraced | 0.7 s | 0.7 s (unchanged) |

The `loop` rows are the shape of the bug: before W8, doubling N **quadrupled** the time (a retained
choice point per inference, and a depth that grew with them, so the indentation grew too — quadratic
output). After, it is linear.

Reading the numbers: the design's "trace mode <= 1.5x slower" target is met for the **debugger**
(port events with no renderer: 1.1x, from 5.8x — that is the L-13 result, since the fast paths are
no longer disabled) and is **not** met for textual `trace/0`, which is ~10-18x. That is the cost of
formatting and printing one line per port — six lines per iteration for a clause with two inline
built-ins in its body — and it is not reducible without changing what a trace *is*. What was
actually broken (a 567x slowdown that grew quadratically with the program, i.e. tracing being
unusable on anything but a toy) is fixed by a factor of ~42 at `nrev` and by turning "does not
complete in 110 s" into 4.6 s at `loop(1000000)`.

Memory under trace is **linear in the number of OPEN calls**, not in the number of inferences: an
open frame has to remember to emit its Exit port, which is one continuation node per frame, so a
1 000 000-deep tail recursion needs ~300 MB (it runs at `-Xmx384m`, not at `-Xmx256m`). Every Prolog
tracer has this property — tracing disables last-call optimisation — and before W8 it was linear in
the number of *inferences*, which is the part that made it unusable.

**Threads** (`scratchpad/w8/TH.java`, 16 acceptance queries, one shared `Prolog`): 16/16 on the
default engine and 16/16 under `-Djprolog.engine=v2`.
**Recursive-solver reachability** (`scratchpad/w8/RS.java`, `QuerySolver.internalSolveCount()`
around each query): before the `CollectionBuiltInAdapter` fix, `concurrent_maplist/3` cost 6
entries, `concurrent/3` 6, `concurrent_and/2` 2, `first_solution/3` 1; after it, **0 for every
one**, and 0 for `thread_create/join` from the start.

### 14.6 Deviations from the W8 brief, and why

1. **The trace oracle is not byte-identical, by design.** The brief says "make the pinned output
   identical (differences only if the old output was wrong — document each)". Five differences
   remain and are the table in 14.4. Three of them (1, 2, 3) are forced: you cannot both keep a
   choice point per traced inference and have bounded trace memory, and once the frames are popped
   `cps.size()` is not a depth any more. The other two are corrections.
2. **"trace mode ≤ 1.5x slower" is met for the debugger, not for textual tracing.** See 14.5. No
   further optimisation was attempted: the remaining cost is `Writer` + `println` per port, and
   making *that* 1.5x would mean not printing.
3. **`length/2`'s enumeration mode was left exactly as it was.** It is not in the brief's list, it
   was already native under trace, and it already emits its ports; touching it would have changed
   the oracle for no gain.
4. **`thread_create/2` keeps its 60-second join timeout and the queues their 30-second get
   timeout.** They are pre-existing deadlock guards, not part of this wave; SWI blocks forever.
5. **A worker's control exceptions become a join STATUS.** `InferenceLimitException` ->
   `exception(inference_limit_exceeded)`, `QueryCancelledException` -> `cancelled`. That is the
   worker's top level, i.e. the analogue of the embedder catching them; untrusted `catch/3` inside
   the goal still cannot see them, because they are not `PrologException`s (invariant 9).
6. **The legacy `Variable` attribute hook is not touched by a worker.** The parent's v4 query
   already uninstalled it process-wide for its duration (ISS-2025-0461); a worker saving and
   restoring the same static would race with the parent. A detached thread outliving its creator's
   query therefore runs with whatever hook the creator restored — an accepted edge case.
7. **`thread_send_message/2` to a thread whose queue has been cleaned up raises**
   `existence_error`-style evaluation error rather than creating the queue. SWI raises
   `existence_error(message_queue, Id)`; the message text differs, the failure mode does not.
8. **A worker owns no query boundary, and the tabling store is not thread-safe.** The sweeps at the
   end of `Machine.solve` (restoring the shared `QuerySolver`'s guard, `ClauseStore.compact()` and
   `Tabling.endQuery()`) are engine-wide, so a worker machine skips them (`Machine.asWorker()`) —
   without that, a worker finishing first ABANDONS the parent's in-progress tabled evaluation.
   What is NOT fixed: two tabled evaluations PRODUCING concurrently on one engine share a variant
   table, a producing stack and an answer list that are plain `HashMap`/`ArrayList`. Consuming a
   COMPLETE table from several threads is fine; producing from two is undefined. Giving each worker
   its own `Tabling` would lose answer sharing, which is the whole point of a table, so the honest
   fix is a lock or a concurrent store — a W9 item, recorded here and in the `Workers` javadoc.
9. **A worker does not attach the IDE debug controller.** `DebugController` keeps one call stack
   and one pause lock for the whole session; a worker firing ports into them from another thread
   would corrupt the stack the IDE renders and could park a worker inside `waitForUserAction` with
   nothing in the UI to release it. `trace/0` still works in a worker — it only prints. Debugging
   *into* a thread needs a controller per thread, which is a UI question as much as an engine one.
10. **`-Pengine-v4` was removed rather than kept as an alias.** `mvn test` is that leg now; keeping
   a profile that sets the default would only hide which leg a CI job actually ran.

### 14.7 Where wave W9 starts

W9 is **retirement** (design B.16 row W9). It is the last wave, and nothing in W8 blocks it: no v4
query reaches `QuerySolver.solveInternal` any more (asserted by two tests), and the only remaining
consumers of the old engines are the two fallback flags.

In dependency order:

1. **Delete the recursive `QuerySolver`.** It is still the durable home of the `DebugController`
   and the `ResourceGuard` (`Prolog.querySolver`, `Engine.contextSolver()`,
   `SolverFacade extends QuerySolver`), so the deletion is really three moves: give `Prolog` a
   small `DebugContext` object for the controller and the guard; change
   `BuiltInWithContext.executeWithContext` to take that instead of a `QuerySolver` (which is what
   "reduce `BuiltInWithContext` to the adapter" means — `SolverFacade` stops being a subclass and
   becomes the interface the ~40 context built-ins see); then delete `QuerySolver`,
   `solveInternal`, `solveBodyGoals`, `CutStatus`/`MutableCutStatus`, `LayeredMap`, the static
   `Trail`, the name-based `TermCopier`/`TermUtils` paths and the 35 legacy sub-solve sites.
   `Prolog.solveLegacy` goes with them — check the IDE first: `DebugPanel` uses it for detached
   breakpoint-condition sub-solves and must move to `Prolog.solve` with the controller nulled.
2. **Delete the v2 `MachineSolver`** (design decision 1 allows it after one release) and with it
   `-Djprolog.engine=v2`, `Prolog.setUseV2Engine`/`isUsingV2Engine`, `solveWithV2Engine`, the
   `engine-v2` Maven profile and `QuerySolver.solveMeta`'s v2 branch. Then the engine-aware test
   branches collapse: `BugFixVerificationTest` (4 sites), `EngineV4LibraryTest`,
   `RefactorIssuesTest`, `V2EngineIntegrationTest` (delete), `MachineSolverTest` (delete),
   `EngineHardeningTest` (re-point at v4 or delete the v2-specific rows).
   **Keep** `core.arith.v2`, `core.parser.v2`, `core.dcg.v2`, `builtin.clpfd.v2` and
   `core.write.v2` — those are unrelated subsystems that merely share the "v2" name.
3. **Retire the limitations the fallbacks carried**: LIM-026, LIM-028, LIM-030, LIM-031, LIM-032,
   LIM-033, LIM-038 and the L-03/L-09 notes in LIM-037 all describe v2/legacy behaviour and are
   deleted with the code. LIM-037 itself shrinks to the eager-built-in list (L-08).
4. **Migrate the remaining eager built-ins** off `LegacyBuiltinAdapter` where it pays: the
   string/atom library, `sort/4`, `predsort/3`, the extended libraries. This is the open-ended part
   — do it predicate by predicate, each with a benchmark.
5. **Regenerate the manual** (`tools/build-manual.sh`) and sweep the docs for "v2 engine",
   "default v2", `-Djprolog.engine=legacy` and `QuerySolver`: `CLAUDE.md`, `README.md`, the
   references, `docs/guides/*`, `docs/ISO132111/*` and this report.

Acceptance for W9 (design B.16): no reference to `QuerySolver` in `src/main`; test count >= 1204;
`./test_all_examples.sh` 20/20; the manual regenerated.

Two things worth doing in W9 that are not in the brief:

- **Profile `Modules.overridesBuiltin` and `Prelude.owner`** — they are on the v4 hot path (every
  goal that is not an inline built-in or a v4 native) and were never measured (W8 item 4 of the
  brief, deliberately skipped).
- **Consider a `DebugController.needsPorts()` gate** so the machine can skip port emission entirely
  when a controller is attached but nothing will consume the events. W8 made the events cheap; not
  emitting them at all would make an attached-but-idle debugger free.

---

## 15. Wave W9 — retirement (v4.0.0, ISS-2025-0484..0488)

**Status**: done. This is the last wave. Suite **1214/1214 on the default engine (v4) and under
`-Pengine-v2`**; **20/20 example programs on both engines**, every per-program "Successful queries"
count unchanged (2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0).
`grep -rn QuerySolver src/main` returns **nothing**.

### 15.1 What changed

| ISS | Change | Files |
|---|---|---|
| 0484 | the recursive engine is DELETED; the context is re-homed on `SolverContext` + `EngineContext`; the v2 tabling driver is ported into `MachineSolver`; `solveLegacy`/`getQuerySolver`/`=legacy`/`-Pengine-legacy` go away | `core/engine/SolverContext.java` (new), `core/engine/EngineContext.java` (new), `core/engine/Prolog.java`, `core/engine/v4/{Engine,Machine,SolverFacade}.java`, `core/engine/v2/MachineSolver.java`, `core/terms/CompoundTerm.java`, `editor/DebugPanel.java`, `pom.xml` |
| 0485 | `BuiltInWithContext` takes a `SolverContext`; the seven control-construct built-ins and `CollectionBuiltInAdapter` are deleted; the 35 legacy sub-solve sites route through `solveMeta`/`solveInWorker` | `core/engine/BuiltInWithContext.java`, `builtin/control/ControlConstruct.java` (new), `core/engine/BuiltInFactory.java`, `core/engine/Prolog.java`, 46 `BuiltInWithContext` implementors |
| 0486 | `sort/4`, `predsort/3`, `max_list/2`, `min_list/2`, `current_op/3`, `nb_getval/2`, `b_getval/2` as v4 natives; the CLP(FD) posting predicates on cells; `ClpfdV2Bridge.cellFor`/`onBindByName` deleted | `core/engine/v4/NativeMisc.java` (new), `core/engine/v4/{ClpfdNative,Coroutining,LegacyBuiltinAdapter,NativeBuiltins}.java`, `builtin/clpfd/v2/ClpfdV2Bridge.java`, `src/main/resources/prelude/coroutining.pl` |
| 0487 | the main thread owns a message queue and the alias `main` | `builtin/threading/ThreadPredicates.java` |
| 0488 | LIM-039: a tabled evaluation is claimed by one thread, on both engines | `core/engine/v4/{Tabling,Machine}.java`, `core/engine/TableStore.java`, `core/engine/v2/MachineSolver.java` |
| 0489 | `stream_property/2` must not BLOCK: computing `end_of_stream` for `user_input` peeked stdin, so the manual's own `stream_property(S, alias(user_error))` hung forever and no budget or interrupt could break it | `builtin/io/StreamProperty.java` |
| 0490 | an answer printed a user-declared operator canonically: `Answer.lines` runs after the engine's `EngineState` has left the thread, so the writer fell back to a default operator table | `core/engine/v4/Answer.java`, `PrologCLI.java`, `editor/RunPanel.java` |

**Deleted** (13 files, `src/main` 76 133 -> 74 724 lines, 361 -> 352 files):

| File | Lines | What it was |
|---|---:|---|
| `core/engine/QuerySolver.java` | 1247 | the recursive SLD algorithm + the durable context, in one class |
| `core/engine/CutStatus.java` | 46 | the cut flag threaded through the recursion |
| `core/engine/MutableCutStatus.java` | 42 | an older, unused variant of the same |
| `core/engine/LayeredMap.java` | 199 | the recursive solver's mark/rollback binding map |
| `core/engine/CollectionBuiltInAdapter.java` | 22 | wrapped a `BuiltInWithContext` around ONE pinned solver |
| `builtin/BuiltInHelper.java` | 27 | reflective `Prolog`-finder; no callers left |
| `builtin/control/Conjunction.java` | 77 | `,/2` — native in both machines |
| `builtin/control/IfThen.java` | 81 | `->/2` |
| `builtin/control/IfThenElse.java` | 193 | `;/2`, `*->/2` |
| `builtin/control/NegationAsFailure.java` | 72 | `\+/1` |
| `builtin/exception/Catch.java` | 122 | `catch/3` |
| `builtin/meta/Call.java` | 131 | `call/N` |
| `builtin/meta/Caret.java` | 58 | `^/2` as a goal |

**Added** (4 files): `core/engine/SolverContext.java` (66), `core/engine/EngineContext.java` (201),
`builtin/control/ControlConstruct.java` (40), `core/engine/v4/NativeMisc.java` (274).

### 15.2 How it works, in one page

1. **Two types replace one.** `SolverContext` is the interface a `BuiltInWithContext` receives:
   `solveMeta(Goal, Bindings, Solutions)`, `solve(Term)`, `solveInWorker(Goal, Bindings, Solutions,
   Max)`, and read-only `getPrologContext/getKnowledgeBase/getBuiltInRegistry/getDebugController/
   getResourceGuard`. `EngineContext` is the concrete object a `Prolog` owns for its whole life: it
   implements `SolverContext` by routing to a fresh machine of the **selected** engine, and it adds
   the two mutators the engine needs (`setDebugController`, `setResourceGuard`) plus the legacy
   `handleAttributeUnification` hook the v2 engine installs on `Variable`.
2. **Nothing inherits any more.** `SolverFacade implements SolverContext` over
   `Machine.runSubQuery`; every entry point is overridden by construction, so the class of bug that
   made W1/W2's `solve/4` silently recursive cannot recur.
3. **The control constructs keep their registry entry, not their code.** `BuiltInRegistry.isBuiltIn`
   is what raises `permission_error` on `assertz(call(x))`, so a 40-line `ControlConstruct`
   placeholder holds the entry; executing it is an `IllegalStateException`.
4. **v2 tabling moved house.** `MachineSolver.tabledDelegate` used to call
   `contextSolver.solve(Goal)`, which entered `QuerySolver.solveWithTabling`. The driver is now
   `MachineSolver.tabledAnswers` + `produceTabled` + `replayTabled`, with the same memo cache, the
   same in-progress partial cache (that is what makes left recursion terminate) and the same
   100-round fixpoint cap.
5. **CLP(FD) posts through cells.** A v4 native posts through the bridge and then binds
   `ClpfdV2Bridge.determinedCells()`; the wake goal is `'$clpfd_unify_hook'('$attvar_cell'(Cell),
   Other)`, which carries the attributed cell itself past the prelude dispatcher (a Prolog clause
   could only carry a name).
6. **Tabling is claimed.** `Tabling.enterCall`/`exitCall` (v4) and `TableStore.enterCall`/`exitCall`
   (v2) make the variant decision atomic, and the claim is held for the whole evaluation.

### 15.3 New invariants (add to section 3)

46. **`SolverContext` is the ONLY type a built-in may take as its context.** No engine class, no
    machine, no `Prolog` internals. If a built-in needs something the interface does not expose,
    add it to the interface and implement it on BOTH `EngineContext` and `SolverFacade` — a
    downcast would pin the built-in to one engine.
47. **A sub-solve is `solveMeta`.** There is no cut-propagating sub-solve any more: the constructs
    that propagated a cut outwards are native in both machines. A new built-in that wants "cut
    escapes into my caller" is asking for something neither engine offers.
48. **A control construct's registry entry is ISO protection, not dispatch.** Unregistering `call`,
    `;`, `->`, `\+`, `catch` or `^` re-opens `assertz(call(X))`. Keep the placeholder.
49. **A generator that unifies several arguments needs ONE mark/undo extent for all of them.**
    `Unify.unify` binds as it walks, so `unifyOrUndo(a) && unifyOrUndo(b)` leaves `a` bound when
    `b` fails, and the next alternative then fails against the stale binding. That is what broke
    `current_op/3` the first time it was made native (invariant 12, restated because it bit again).
50. **A tabled evaluation belongs to one thread.** Claim it with `enterCall`, hold it until the SCC
    completes / is abandoned / the query ends, and hand it back from a worker in
    `Machine.solve`'s finally. A consumer that reads an EVALUATING table it did not produce is
    reading a half-built answer set.
51. **A production runs against the NORMALISED pattern, never the caller's goal** (v2 driver). Two
    machines allocate the same renamed variable names (`_R1_Z`), so a nested machine can bind the
    outer goal's own variable behind its back.

### 15.4 Deviations from the W9 brief, and why

1. **The v2 `MachineSolver` is NOT deleted.** The §14.7 brief listed it as item 2; design decision
   1 of B.17 and row W9 of B.16 keep it selectable for one release after v4 becomes the default,
   and the task brief made that binding. It goes in 4.1, and with it `-Djprolog.engine=v2`, the
   `engine-v2` profile, the ported v2 tabling driver, `EngineContext`'s engine-routed `runSub`,
   `V2EngineIntegrationTest`, `MachineSolverTest` and `EngineHardeningTest`'s v2-specific rows.
2. **`core.engine.Trail` is NOT deleted.** The brief listed the static `ThreadLocal` trail for
   removal, but the **v4 machine uses it**: `Machine.CP.legacyMark` / `Trail.rollbackTo` is how a
   v4 choice point rolls back the backtrackable state of bridged built-ins — the CLP(FD) store's
   posts, `setarg/3`, `b_setval/2`, `op/3`. Removing it means giving the ~310 bridged built-ins a
   `Bindings`-native undo protocol, which is the rest of L-08, not this wave.
3. **`util.TermCopier` and `util.TermUtils` are NOT deleted.** The brief called for removing the
   "name-based `TermCopier` paths". `TermCopier` is still the rule-copy renamer used by
   `core.engine.Rule`, `Prolog.compile`, `builtin.database.Clause`, `builtin.exception.Throw` and
   `builtin.term.TermConstruction`; `TermUtils.getArity`/`getFunctorName` are used in a dozen
   places on both engines. Neither is dead, and the v4 machine does not use either (it has
   `Clause.instantiate` and `Unify.copy`). Deleting them is a v2-removal item.
4. **The control constructs keep a registry placeholder** rather than being unregistered outright
   (see 15.2 item 3). Unregistering them was tried first and drops the ISO `permission_error` for
   `assertz`/`retract`/`clause` on `call/1`, `;/2`, `catch/3` and `^/2`.
5. **`length/2` is untouched, deliberately** (the W8 deviation 3 carry-over the brief asked to
   decide). Its enumeration mode is already a native lazy generator that emits its own four ports,
   exactly like `between/3`; its deterministic modes are native too (ISS-2025-0453). There is
   nothing left to migrate, and `EngineV4TraceTest` pins its port stream line for line, so touching
   it would change a pinned oracle for no gain. **Decision: closed, no work.**
6. **`Modules.overridesBuiltin` / `Prelude.owner` were not profiled** (the first of the two
   "worth doing" items at the end of §14.7). Both are `HashMap`/`HashSet` lookups on an index built
   once per JVM; W9 spent its budget on the deletions, the CLP(FD) cell migration and two
   thread-safety fixes. Carried to 4.1.
7. **`DebugController.needsPorts()` was not added** (the second one). Same reason; W8 already made
   an attached-but-idle controller cost ~1.1x.
8. **The main thread's `thread_self/1` still reports an integer, not the atom `main`.** SWI reports
   `main`. Changing the *type* of `thread_self/1`'s answer would break every program and test that
   does arithmetic on it; the queue alias `main` is what the brief asked for and what the idiom
   needs. Recorded here rather than silently diverging.
9. **`docs/ISO132111/` was annotated, not rewritten.** Six EN/IT chapter pairs narrate the *old*
   engine in detail (`QuerySolver.java - Implementazione centrale del backtracking`, extension
   snippets calling `context.getQuerySolver()`). Rewriting twelve bilingual walkthroughs is a
   documentation project of its own; each now carries a banner under its title saying that the
   language semantics still hold but the classes shown were deleted in 4.0.0, and pointing at
   `CLAUDE.md` and this report. A real rewrite is a 4.1 item.
10. **The two references were made consistent, not re-audited.** Every registered indicator has an
   entry in `BUILTIN_PREDICATES_REFERENCE.md` **or** in `tools/manual/supplement.md` (the manual
   merges them, and a script over `registerFactory(`/`t.register(`/the prelude exports now reports
   zero gaps except the internal `'$attr_hook'/4`); `BUILTIN_OPERATORS_REFERENCE.md` gained the 13
   default operators it never listed (the six CLP(FD) comparisons, `/\`, `\/`, `xor`, and the
   1150 prefix declaration operators) and its precedence overview was wrong (it claimed `;` at 1200
   and `->` at 1100) and is now the engine's real table. What was NOT done is a semantic re-read of
   all 8 000 lines of the predicate reference.

### 15.5 A/B evidence

Same session, loaded VM. The A side is the working tree before W9 (v4.0.0 as W8 left it), the B
side after.

**Concurrent tabled production** (`scratchpad/w9/T7.java`: 30 iterations, each a fresh `Prolog`
running `concurrent_maplist(count, [a,b,c,d], Ns)` where `count` does
`findall(Y, tabled_path(1, Y), L), length(L, N)`; the correct answer is `[4,4,4,4]`):

| | before W9 | after W9 |
|---|---|---|
| v4 default | 15 / 30 runs WRONG (`[4,4,0,0]`, `[4,2,4,4]`, …) | **30 / 30 correct** |
| `-Djprolog.engine=v2` | 29 / 30 runs WRONG | **30 / 30 correct** |

**`current_op/3`** is lazy now: the first solution no longer builds the whole visible list. The
table holds ~50 entries so there is no measurable wall-clock difference; the change is the
allocation profile and the fact that `once(current_op(P, T, is))` stops at the match.

**The v2 tabling port** is a correctness fix, not a performance one:
`:- table path/2` with `path(X,Y) :- edge(X,Y). path(X,Y) :- path(X,Z), edge(Z,Y).`

| query | before W9 (v2) | after W9 (v2) |
|---|---|---|
| `path(a, Y)` | `[b, c, d]` | `[b, c, d]` |
| `path(a, d)` (ground variant) | **fails** | **succeeds** |
| `RefactorIssuesTest.testR5_tabledLeftRecursion` | passes | passes |

(The "before" column here is the state during the wave, i.e. the naive port that produced against
the caller's goal — the pre-W9 build routed the whole thing through `QuerySolver.solveWithTabling`,
where `path(a, d)` also succeeded. The row exists because it is the bug the normalised-pattern rule
of invariant 51 prevents.)

**Suite and examples**: 1214/1214 on both legs; 20/20 example programs on both engines with
identical per-program "Successful queries" counts.

**The manual's own examples, re-run against the build** (`scratchpad/w9/extract.py` pulls every
`?-` query out of `tools/manual/{front,supplement,appendix}.md` into the block format
`scratchpad/manual/examples.txt` uses; `scratchpad/w9/ExCheck2.java` runs them, one `Prolog` per
source file, each block's clauses consulted as a reader would, 8 s per query; `scratchpad/w9/cmp.py`
diffs expected against actual):

| | count |
|---|---:|
| `?-` queries extracted | 134 |
| skipped (need a network peer, a terminal read, `halt`, an external file) | 13 |
| executed | 121 |
| matched the documented answer | 103 |
| differed | 18 |

Four real defects came out of it, all fixed:

1. **A documentation error**: `stream_property(S, alias(user_error))` answers `S = '$stream'(2)`,
   not `S = user_error` — an unbound stream argument is bound to the canonical stream term, never
   to an alias. The example was rewritten as `stream_property(user_error, alias(A))` and the rule
   spelled out.
2. **A documentation error**: the `when/2` example's answer gained `Z = 3`, because since wave W4 a
   woken goal's bindings propagate (the old text said only its side effects were visible).
3. **An engine bug** (ISS-2025-0489): `stream_property/2` HUNG on that very example — deciding
   `end_of_stream` for `user_input` peeks, which blocks on an interactive terminal.
4. **An engine bug** (ISS-2025-0490): an answer printed a user-declared operator canonically,
   because the CLI/IDE render it after the engine's `EngineState` has left the thread.

The remaining 18 differences are harness or environment artifacts: the manual omits the auxiliary
`S = '$stream'(N)` binding an `open/3` leaves behind (8), a timestamp / working directory / network
peer / generated variable name is environment-dependent (4), the harness accumulates clauses and
global variables within a source file so `nb_current/2` and one `existence_error` example see
earlier blocks' state (2), the printed side effects of `spy/1` and `trace/0` are reported on a
separate line (2), and two answers legitimately carry more bindings than the manual's excerpt shows
(`numbervars/3`, `clause/2`). One convention difference is now stated in the manual's own preface:
the manual writes a space after a comma inside a compound term (`[1, 2]`) where the console writes
none (`[1,2]`).

### 15.6 What remains for 4.1

**ENG-16 (the object-binding rewrite) is DONE by construction.** It was the last open item of
`docs/reports/report-engine-deep-analysis-2026-08-24.md` — "bindings live in a name-keyed HashMap
that never reclaims dead variables, ~1 GB for `loop(3000000)`" — and the v4 engine has no binding
store at all: a binding IS the `Variable.ref` field, the trail is conditional, and the JVM reclaims
a finished deterministic call's bindings. `loop(10000000)` runs in a 64 MB heap using 4 MB. With
v4 the default since 4.0.0, that is simply how JProlog behaves; nothing is left to do.

Residual limitations after W9:

| LIM | What |
|---|---|
| **LIM-037** (shrunk) | Of the 416 registered predicate names, 63 are v4 natives and ~40 more are control constructs or inline built-ins the machine never dispatches; the remaining **~310** run on `LegacyBuiltinAdapter` with the eager `(goal, Map, List<Map>)` contract — the atom/string/character library, the I/O family, `format/2,3`, `read_term`/`write_term`, the assert/retract/listing family, `op/3`, `statistics/2`, the debug and profiler predicates, and the whole extended library (CSV, JSON, XML, HTTP, JDBC, crypto, datetime, filesystem, graph, logging, network, os, persistence, regex, threading, FFI). None is on a measured hot path; a deterministic one costs two extra term walks, a nondeterministic one still materialises every solution. |
| **LIM-036** | `.jpc` source lines come from the legacy parser's clause splitter. |
| **LIM-027** (narrowed) | open-tail generative list modes are bounded on the **v2 fallback** only. |
| **LIM-039** (closed, with a residual) | tabled production is serialised; the 60-second wait then raises `resource_error(tabling_busy)`, and several concurrent top-level `Prolog.solve` calls on ONE instance remain outside the contract (a top-level query owns the engine-wide query boundary). |

Recommended 4.1 work, in order:

1. **Delete the v2 `MachineSolver`** (the one-release promise expires): with it go
   `-Djprolog.engine=v2`, the `engine-v2` profile, `Prolog.setUseV2Engine`/`isUsingV2Engine`,
   `EngineContext.runSub`'s v2 branch and the ported v2 tabling driver, `core.engine.TableStore`
   and `Prolog.getTableStore` (v4 has `Tabling`), `core.engine.Trail` and the legacy
   `Variable.AttributeUnifyHook` (once no built-in needs a non-`Bindings` undo), the legacy
   `Freeze`/`When`/`Dif` built-ins (the prelude owns them on v4), `util.TermCopier`/`TermUtils`,
   the second CI leg, and the engine-aware test branches in `BugFixVerificationTest`,
   `EngineV4LibraryTest`, `RefactorIssuesTest`, `V2EngineIntegrationTest` (delete),
   `MachineSolverTest` (delete) and `EngineHardeningTest` (re-point at v4).
2. **Re-land the eager-built-in migration (L-08) predicate by predicate**, each with a benchmark:
   the atom/string library first (it is the largest and the one users hit on long strings), then
   `format/2,3` and the I/O family, then the extended libraries. Every migration that removes a
   `Map<String,Term>` also removes a `Trail` user, which is what unblocks item 1's `Trail` deletion.
3. **Profile `Modules.overridesBuiltin` and `Prelude.owner`** (every non-inline, non-native goal
   pays them) and add `DebugController.needsPorts()` so an attached-but-idle debugger is free.
4. **First-argument indexing measurements on v4**: `ClauseStore` has the index; nobody has measured
   the bucket cap against a realistic large program since W2.
5. **A concurrent tabling store**, if anyone needs parallel production: the claim of ISS-2025-0488
   is correct but serial. The honest design is a per-variant lock plus a concurrent answer trie.

---

## 16. Release 4.1, wave A — one engine (v4.1.0, ISS-2025-0491..0495)

**Status**: done. Suite **1196/1196** on the one engine; **20/20 example programs** with every
per-program "Successful queries" count unchanged
(2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0).
`grep -rn "MachineSolver\|core.engine.v2\|Trail.record\|isUsingV2Engine\|setUseV4Engine" src/main`
returns **nothing** (the only hits in the tree are in `EngineV41RetirementTest`, which asserts those
names are gone, and one explanatory comment in `EngineV4Test`).
`src/main`: **74 724 -> 71 899 lines** (-2 825), **352 -> 347 files** (6 deleted, 1 added).

### 16.1 What changed

| ISS | Change | Files |
|---|---|---|
| 0491 | the v2 `MachineSolver` is DELETED, with the `engine-v2` profile, the second CI leg, the four static engine flags, every engine-aware branch in `src/main` and `src/test`, the legacy attribute-unify hook, the cross-query attributed-variable session, the legacy `freeze`/`when`/`dif`/attributed-variable built-ins and the v2 half of `TableStore` | `core/engine/v2/MachineSolver.java` (deleted), `builtin/control/{Freeze,When,Dif}.java` (deleted), `builtin/term/AttributedVariables.java` (deleted), `core/engine/{Prolog,EngineContext,TableStore,BuiltInFactory,BuiltInRegistry,ResourceGuard,NeedsSolverContextException}.java`, `core/terms/{Variable,CompoundTerm}.java`, `core/engine/v4/{Engine,Machine,EngineState,Tabling,ClpfdNative}.java`, `core/system/PrologFlags.java`, `core/utils/CollectionUtils.java`, `builtin/{control/Repeat,control/ControlConstruct,database/Retract}.java`, `pom.xml`, 12 test files |
| 0492 | `core.engine.Trail` (the second, process-wide undo stack) is replaced by `core.engine.v4.Undo` over the machine's own `Bindings` trail; `CP.legacyMark` is gone | `core/engine/v4/Undo.java` (new), `core/engine/v4/Machine.java`, `core/engine/Trail.java` (deleted), `builtin/system/{Op,OperatorDefinition,GlobalVariables}.java`, `builtin/term/SetArg.java`, `builtin/clpfd/v2/ClpfdV2Bridge.java` |
| 0493 | the module override test leaves the hot goal path: asked only when a registry entry exists, and memoised behind the `ModuleManager` stamp | `core/engine/v4/{Machine,Modules}.java` |
| 0494 | `DebugController.needsPorts()` — an attached controller that can observe nothing gets no ports | `core/engine/DebugController.java`, `core/engine/v4/Machine.java` |
| 0495 | `thread_self/1` reports the thread's alias (`main`, `w1`, …); the `main` alias follows a live thread | `builtin/threading/ThreadPredicates.java`, `docs/references/BUILTIN_PREDICATES_REFERENCE.md` |

**Deleted** (6 files, 2 616 lines):

| File | Lines | What it was |
|---|---:|---|
| `core/engine/v2/MachineSolver.java` | 1895 | the iterative SLD engine of 3.1.0–4.0.0 |
| `builtin/control/When.java` | 226 | `when/2` in Java |
| `builtin/term/AttributedVariables.java` | 179 | `put_attr/3`, `get_attr/3`, `del_attr/2`, `attvar/1` in Java |
| `builtin/control/Dif.java` | 156 | `dif/2` in Java |
| `builtin/control/Freeze.java` | 106 | `freeze/2` in Java |
| `core/engine/Trail.java` | 54 | the process-per-thread undo stack for bridged built-ins |

**Added** (1 file): `core/engine/v4/Undo.java` (64).

**Deleted tests** (39 methods). Every one either tested the deleted class directly or asserted the
behaviour of the deleted selection API:

| Test | Methods | Why it goes, and what covers the behaviour now |
|---|---:|---|
| `core/engine/v2/MachineSolverTest` | 30 | unit tests of `MachineSolver` itself (backtracking, cut, lazy enumeration, deep recursion), constructed directly. The same properties are pinned for the surviving engine by `EngineV4Test` (34) and `EngineHardeningTest` (54). |
| `core/engine/v2/V2EngineIntegrationTest` | 7 | end-to-end programs driven through `MachineSolver` with the engine flag forced. The same programs run in `EngineV4Test` / `BugFixVerificationTest` on the one engine. |
| `EngineV4Test.testISS0444_EngineSelectionFlag` | 1 | asserted `setUseV4Engine(false)` selects another engine. There is no other engine; `EngineV41RetirementTest.testISS0491_TheEngineSelectionApiIsGone` asserts the API is gone. |
| `EngineV4RetirementTest.testISS0484_OnlyV2SelectsAFallbackEngine` | 1 | same, for the `v2` literal. Replaced by the same new test. |

`core/engine/v2/EngineHardeningTest` (54 methods) was **moved** to `core/engine/EngineHardeningTest`
— it is engine-neutral apart from one method that built a `MachineSolver` to read
`choicePointCount()`/`trailSize()`, now built as a v4 `Machine`.

**Added tests**: `core/engine/v4/EngineV41RetirementTest` (21).

### 16.2 How it works, in one page

1. **One undo trail.** `Bindings` always accepted undo actions next to its cell resets
   (`pushUndo`). What was missing was a way for a *bridged* built-in — which sees a goal, a map and
   a solution list, and no machine — to reach it. `core.engine.v4.Undo` is that doorway: the
   machine installs itself as the thread's undo target for the duration of `solve` (saved and
   restored, so a nested machine hands the role back), and `Undo.record(Runnable)` pushes onto its
   trail. `B.undo(cp.trailMark)` then runs the action at exactly the point the cells are reset, and
   the trail's own economy applies — an action nothing can backtrack over is dropped by
   `clearIfUnreachable`, the same condition under which a binding is not trailed at all. An action
   recorded with **no** machine on the thread (a `:- op(...)` directive at consult time, a
   directly-instantiated built-in, a unit test) is a no-op: nothing can undo it, exactly as before.
2. **The module test is a decision about built-ins, so it is asked only about built-ins.**
   `stepN`'s tail used to be: `overridesBuiltin(ctx, f, n)` -> `LegacyBuiltinAdapter.run` ->
   `callUser`. The first of those cost two string concatenations and up to four map probes for
   *every* goal, including `app/3` in nrev, which has no registry entry to override. It is now
   inside `if (registry.isBuiltIn(f, n))` — the probe the adapter does as its own first statement —
   so a plain user predicate pays one `HashMap` miss and goes straight to its clauses. The answer
   is also memoised: a 512-slot direct-mapped cache of **immutable** `Dispatch` entries carrying the
   `ModuleManager` stamp. A stamp bump (a module defined, an import added, a clause consulted into
   a module) makes every stale entry miss; no clearing, no lock, and a racing worker thread can at
   worst recompute an entry. A library `load()` cannot flip an entry either — `autoload` loads the
   module and only then reads its clauses.
3. **An attached controller is not necessarily a listening one.** `DebugController.notifyPort`
   maintains a call stack, decides whether to pause, and only then builds an event. With no
   listener, no breakpoint, `CONTINUE` mode and no Stop pending, all three are no-ops, so
   `needsPorts()` is false and the machine emits nothing — `debugPortsTarget()` replaces every
   `debugController != null` port decision. `traceEnabled` deliberately does not count: every use
   of it inside the controller is guarded by `listener != null`, and it defaults to `true`.
4. **`thread_self/1` answers an identity, not an index.** A thread with an alias reports it; the
   top-level thread's alias is `main`. The alias is now kept pointing at a **live** thread: it used
   to be claimed once for the JVM's lifetime by the first non-worker thread to touch the queues, so
   a JUnit `@Test(timeout=)` body, an IDE background solve or any one-shot embedder thread could
   take it and then die, leaving `thread_send_message(main, T)` posting into a queue nobody could
   read. `ensureLiveMainAlias()` hands the alias to the current thread when the owner is gone, and a
   `WORKER_IDS` set makes sure a `thread_create/2,3` worker never becomes `main`.

### 16.3 New invariants (add to section 3)

52. **There is ONE trail.** A backtrackable side effect outside the binding cells is recorded with
    `core.engine.v4.Undo.record`, never with a private stack and never with a second choice-point
    mark. If a construct needs an undo that must survive `clearIfUnreachable`, it needs a choice
    point or a `forceTrail` extent — the same rule as a binding (invariant 1).
53. **A thread-current facade must be saved and restored, not set.** `Undo.enter/exit` mirrors
    `EngineState.setCurrent` and `PrologFlags`: a nested machine (or an engine invoked from inside
    another engine's built-in) hands the role back in a `finally`.
54. **The module override test is a question about a registry entry.** Never ask
    `Modules.overridesBuiltin` for a predicate that `BuiltInRegistry.isBuiltIn` does not claim —
    the answer cannot change the dispatch, and it is pure cost on the hottest path in the engine.
55. **The dispatch memo is stamped, never cleared.** Any new module-visibility state that could
    change `overridesBuiltin`'s answer must bump `ModuleManager`'s stamp (`touch()`), or the memo
    will keep answering the old question. Entries are immutable so the cache can race harmlessly;
    do not make `Dispatch` mutable.
56. **A port site asks `debugPortsActive()`, not `debugController != null`.** An attached
    controller that can observe nothing must cost nothing — and, symmetrically, a controller that
    can observe something must get every port, including the ones the inline fast paths emit
    (limit L-13, invariant from W8: never skip the work, skip only the *reporting* when there is
    provably no observer).

### 16.4 Deviations from the 4.1-A brief, and why

1. **`core.engine.TableStore` is not deleted, it is reduced.** The brief listed it as v2 tabling.
   Its answer cache, in-progress set, partial cache, goal normaliser and one-thread claim were
   indeed the v2 driver's and are gone (232 -> 54 lines), but the class also holds the `:- table`
   **declarations**, which the v4 machine reads (`Machine.isTabled` via `Engine.tables()`) and
   which three registered built-ins write (`table/1`, `abolish_table/1`, `abolish_all_tables/0`,
   through `Prolog.getTableStore()`). Moving the declaration set into `core.engine.v4.Tabling`
   would rename a public API for no behavioural gain and is not worth doing in a wave whose point
   is deletion; it is a candidate for wave B, when the three built-ins migrate anyway.
2. **`util.TermCopier` and `util.TermUtils` are not deleted** — re-verified, not assumed:
   `TermCopier` has 11 call sites (`core.engine.Rule`, `Prolog.compile`, `builtin.database.Clause`,
   `builtin.exception.Throw`, `builtin.term.TermConstruction`, …) and `TermUtils` is used in 12
   files. Neither is reachable from the machine (it has `Clause.instantiate` and `Unify.copy`).
   They go when the built-ins that use them migrate — wave B, as W9 deviation 3 already said.
3. **`-Djprolog.engine` warns, it does not throw.** The brief allowed either. A build script that
   still passes `-Djprolog.engine=v2` in CI should be told, loudly, that it is not getting what it
   asked for — but failing every `new Prolog()` in a deployed embedder because of a stale JVM flag
   is a worse trade. The warning is logged once, from a static initialiser.
4. **Seven predicates lost their ISO `permission_error` on assert/retract.** Deleting the legacy
   `Freeze`/`When`/`Dif`/`AttributedVariables` classes means deleting their `BuiltInFactory`
   registrations, and `BuiltInRegistry.isBuiltIn` needs a registration *and* an arity entry — so
   `assertz(freeze(X, Y))`, and the same for `when/2`, `dif/2`, `put_attr/3`, `get_attr/3`,
   `del_attr/2` and `attvar/1`, is now allowed instead of raising
   `permission_error(modify, static_procedure, …)`. Calling them still runs the native or the
   prelude clause. This is consistent with the documented library rule (a user definition of a
   library predicate overrides it — that is how `partition/4` works), and the alternative — teaching
   `Machine.checkModifiable` about the native table — would newly protect ~63 indicators that were
   never protected before, a larger behaviour change than the one it prevents. Recorded here rather
   than done silently.
5. **The measurement is a median of interleaved JVMs, not a single A/B pair.** The VM is a
   VirtualBox guest on a shared folder and its noise floor is brutal: the *same* build measured
   324 ms and 844 ms for `nrev30x2000` in two JVMs five seconds apart (2.6x). Cross-run comparison
   of single numbers is meaningless here, so every figure in 16.5 is the median of alternating
   A/B/A/B JVM runs in one shell session, each run reporting the best of 6 warm iterations.
6. **`DebugController.needsPorts()` excludes `traceEnabled`**, although the brief's phrasing
   ("no listener/breakpoints") could be read either way. The field defaults to `true` and every use
   of it in the class is `traceEnabled && listener != null`, so including it would make
   `needsPorts()` true for every controller ever constructed and the optimisation would be dead.
7. **`thread_self/1` needed a second fix to be shippable.** Reporting the alias exposed a latent
   bug in the alias itself (item 4 of 16.2): with `main` pinned to a dead thread, two test classes
   that each ran on their own JUnit timeout thread could not exchange a message. The liveness
   takeover is part of ISS-2025-0495 rather than a separate issue because the feature is not
   correct without it.

### 16.5 A/B evidence

Same shell session, alternating JVMs, `java -Xss4m -Xmx2g`, harness
`scratchpad/41a/probe/AB41.java` (best of 6 warm iterations per figure, after a warm-up round).
**A** = the working tree after the deletions of ISS-2025-0491/0492, before the hot-path work;
**B** = the same tree with ISS-2025-0493 and ISS-2025-0494.

| benchmark | A (median / min) | B (median / min) | change (median) |
|---|---|---|---|
| `nrev30x2000` (496 LI per iteration) | 404 / 344 ms | 362 / 316 ms | **-10 %** |
| `lk`: 200 000 x `f(19999,_)` (indexed, 20 000 facts) | 194 / 162 ms | 181 / 161 ms | **-7 %** |
| `lk0`: 200 000 x `f(0,_)` (first clause) | 192 / 146 ms | 173 / 159 ms | -10 % |
| `loop(1000000)` (deterministic recursion) | 543 / 456 ms | 496 / 451 ms | **-9 %** |

13 A runs and 13 B runs; the medians are over runs, the minima over all iterations of all runs.

**And against the release this wave started from** (same method, 8 interleaved pairs, run later in
the session on a busier machine — hence the higher absolute numbers; what matters is that the wave
costs nothing anywhere): **A** = the v4.0.0 build, **B** = the finished 4.1.0 tree.

| benchmark | v4.0.0 (median / min) | 4.1.0 (median / min) | change (median) |
|---|---|---|---|
| `nrev30x2000` | 835 / 658 ms | 790 / 752 ms | -5.3 % |
| `lk` 200 000 x `f(19999,_)` | 366 / 317 ms | 356 / 335 ms | -2.7 % |
| `lk0` 200 000 x `f(0,_)` | 335 / 301 ms | 314 / 283 ms | -6.3 % |
| `loop(1000000)` | 1054 / 834 ms | 927 / 862 ms | -12.1 % |

**The probe that justified the work** (same method, a build with the `overridesBuiltin` call
short-circuited to `false` — semantically wrong, but it isolates the cost): `nrev30x2000` 386 ms ->
323 ms, i.e. the module test was **~16 %** of nrev and ~5 % of the fact-lookup loops. The reorder
(ask it only for a registered name) removes it entirely for a plain user predicate; the memo
removes most of what remains for the library predicates that DO have a registry entry
(`member/2`, `append/3`, `maplist/N`, `include/3`, …).

**`DebugController.needsPorts()`** needs its own harness (`scratchpad/41a/probe/Idle.java`),
because none of the benchmarks above attaches a controller: it times the same goal with no
controller, then with an **idle** one (no listener, no breakpoint, `CONTINUE`), then with none
again, and reports the ratio.

| goal | attached-idle cost, A | attached-idle cost, B |
|---|---|---|
| `loop(1000000)` (one port site per inference) | **2.27x / 1.77x / 2.08x** | **1.13x / 1.12x / 0.96x** |
| `nrev30x1000` | 1.31x / 1.18x / 0.94x | 1.18x / 1.13x / 1.12x |

The deterministic loop is the honest measurement — every inference hits a port site, so the
overhead is not diluted by unification work: an attached-but-idle controller went from roughly
**doubling** the run to costing nothing measurable. (On `nrev` the effect is inside the noise, which
is why the loop is the one to quote.) `EngineV41RetirementTest.testISS0494_AnIdleControllerDoesNotSeeThePorts`
pins the structural guarantee: the call stack of such a controller stays empty over a 200-step
recursion, which is the observable form of "no port was emitted".

**Suite and examples**: 1196/1196; 20/20 example programs, per-program counts unchanged.

### 16.6 Where 4.1 wave B starts — the L-08 migration

Wave B is item 2 of the 4.1 list in section 15.6: **move the eager registry built-ins to the v4
native SPI, family by family, each with a benchmark**. Everything below is measured on the 4.1.0
tree (`scratchpad/41a/probe/Fam.java` enumerates it from a live `Prolog`).

**The shape of the problem.** 410 names are registered in `BuiltInRegistry`; 63 of them (66
indicators) are shadowed by a v4 native and are never dispatched through the adapter. Of the
remaining 361, about 44 are control constructs or inline built-ins the machine handles itself and
never dispatches either (`!`, `,/2`, `;/2`, `->/2`, `\+/1`, `call/N`, `catch/3`, `throw/1`, `^/2`,
`=/2`, `is/2`, the six arithmetic comparisons, the six term comparisons, `\=/2`, the nine type
checks, `findall/3`, `assert*`/`retract/1`, `once/1`, `ignore/1`, `forall/2`, `between/3`,
`repeat/0`). **~317 predicates really reach `LegacyBuiltinAdapter`.**

**The families, by count** (bridged names only; the count is names, not indicators):

| Family (`builtin.*`) | Bridged | Hot-path relevance | Notes for the migration |
|---|---:|---|---|
| `io` | 40 | **high** — `format/2,3`, `write/1,2`, `nl/0,1`, `read_term/2,3` are in every program's inner loop when it prints | the biggest single win; `format/2,3` alone is worth a wave step. Output must keep going through `StreamManager.out()` (invariant 11) and a stream argument is a term, not a string. |
| `jdbc` | 28 | none | pure I/O against a database; migrate last, or never |
| `type` | 16 | **low** — the nine hot type checks are already inline; what is left is `code_type/2`, `is_list/1`, `must_be/2`, `partial_list/1`, `proper_list/1`, `simple/1`, `ground/1` | cheap, deterministic, mechanical |
| `filesystem` | 15 | none | host I/O |
| `threading` | 15 | low | already runs goals on `Workers`; the adapter hop is not the cost |
| `crypto` | 14 | none | |
| `ffi` | 14 | none | |
| `list` | 14 | **high** — `maplist/N`, `foldl/N`, `include/3`, `exclude/3` are prelude clauses already, but `keysort/2`, `permutation/2`, `flatten/2`, `subtract/3`, `intersection/3`, `union/3`, `delete/3`, `pairs_*` are bridged and eager | second-highest value; several are natural lazy generators |
| `graph` | 13 | none | |
| `network`, `persistence` | 13 + 13 | none | |
| `os` | 12 | none | |
| `term` | 12 | **medium** — `functor/3`, `arg/3`, `=../2`, `term_to_atom/2`, `atom_to_term/3`; `arg/3` in a loop is common | `arg/3` should be a generator (it is nondeterministic with an unbound N) |
| `debug` | 11 | none | |
| `http` | 11 | none | |
| `conversion` | 10 | **medium** — `atom_codes/2`, `atom_chars/2`, `number_codes/2`, `atom_number/2` are hot in parsing-shaped programs | deterministic, easy |
| `datetime` | 10 | none | |
| `system` | 10 | low, but two are special: `op/3` and `b_setval/2` are the last `Undo.record` users outside CLP(FD) | migrating them lets `Undo` become an internal detail again |
| `arithmetic` | 9 | low — the comparisons are inline; `succ/2`, `plus/3`, `between/3` (native) are what is left | |
| `database` | 9 | **medium** — `listing/1`, `current_predicate/1`, `dynamic/1`, `abolish/1`, `retractall/1` (assert/retract are already machine-native) | `current_predicate/1` is a generator |
| `string` | 8 | **high** on string-heavy programs — `split_string/4`, `atomic_list_concat/2,3`, `string_concat/3`, `string_chars/2` | `atomic_list_concat/3` in split mode is a generator |
| `json`, `logging`, `regex`, `meta`, `dcg`, `character`, `csv`, `clpfd`, `xml`, `atom`, `exception` | 6,6,6,6,5,4,4,3,3,2,2 | mixed | `meta`'s five are machine-native already except `table/1`; `atom_concat/3` (in `atom`) is a **generator** in the split mode and is hot |

**The order the evidence supports**: (1) `format/2,3` and the `io` write family; (2) the `string`
and `conversion` families (they are what users hit on long inputs, and they are the ones LIM-037
names); (3) `list` — the eager ones that should be lazy (`keysort`, `permutation`, `subtract`,
`intersection`, `union`); (4) `term` (`arg/3`, `functor/3`, `=../2`); (5) `database`
(`current_predicate/1`, `listing/1`); (6) `system` (`op/3`, `b_setval/2`, which retires the last
external `Undo` users); the extended libraries (jdbc, http, network, crypto, graph, os, datetime,
persistence, ffi, filesystem — 148 names between them) have no hot-path claim and can stay bridged
indefinitely.

**How to measure each step** (the method used in 16.5, which is the only one that survives this
VM's noise):

1. Write the microbenchmark FIRST, as a `Prolog` program driven from a Java `main` that runs the
   target predicate in a tight recursive loop 100 000+ times, plus one realistic composite
   (e.g. `format/3` into an atom inside a `numlist` fold). Add it to
   `scratchpad/41a/probe/AB41.java` or a sibling.
2. Snapshot `target/classes` to `scratchpad/<wave>/classes-A` **before** the change.
3. After the change, run **alternating** JVMs `A, B, A, B, …` at least 5 pairs, in one shell
   session, each JVM reporting the **best of 6** warm iterations (`java -Xss4m -Xmx2g -cp
   <classes>:<probe> AB41 6`).
4. Report the **median over runs** and the **min over all iterations**, both sides. A change under
   ~5 % of the median is not distinguishable from the noise on this machine and should be reported
   as "no measurable change", not as a win.
5. Guard the behaviour with a test in `EngineV4*Test` **before** the migration, so the native and
   the registry version are pinned to the same answers — including the error terms, which is where
   an eager built-in and a native most often disagree.

**The two structural rules a migrated built-in must follow** (invariants 3, 12, 49): a generator
that tries several alternatives inside one `next()` needs ONE mark/undo extent around the whole
group, and it must announce its last alternative with `Machine.lastSolution()` or the choice point
survives the trust-me pop.

**What wave B also unblocks**: every migration that removes a `Map<String,Term>` removes a
potential `Undo.record` caller and a `TermCopier`/`TermUtils` user; when `op/3`, `b_setval/2` and
`setarg/3` are native, `core.engine.v4.Undo` can lose its public `record` entry point and become
`Machine`-internal, and `util.TermCopier` / `util.TermUtils` can be deleted (W9 deviation 3).
