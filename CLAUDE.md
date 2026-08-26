# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

JProlog is a Prolog interpreter written in Java (source/target 1.8; it builds and runs fine on a
modern JDK — JDK 25 / Maven 3.9 in the dev environment). It has a clean-room resolution core, ~415
built-in predicates, a Swing IDE and a CLI. No external dependencies beyond JUnit 4.

**The engine is `core.engine.v4`, and it is the only one** — waves W1–W9 of
`docs/reports/report-engine-v4-design-2026-08-25.md`, finished in 4.0.0. The recursive
`QuerySolver` that JProlog ran on until 3.x was deleted in 4.0.0; the v2 `MachineSolver` (the
default from 3.1.0 to 3.14.0, kept as a one-release fallback in 4.0.0) is **deleted in 4.1.0**, and
with it `-Djprolog.engine=v2`, the `engine-v2` Maven profile and the four static engine-selection
accessors. **"v2" elsewhere in the tree does not mean an engine**: `core.parser.v2`,
`core.dcg.v2`, `builtin.clpfd.v2`, `core.arith.v2` and `core.write.v2` are second-generation
clean-room rewrites of the parser, the DCG translator, the CLP(FD) solver, the arithmetic evaluator
and the IDE source formatter, and they are all current.

**Repository**: https://github.com/DenzoSOFTHub/JProlog
**Current version**: `<version>` in pom.xml (4.2.0). pom.xml and CHANGELOG.md are the source of
truth; README.md is refreshed at release time and may lag between releases.

## Build & Run

```bash
mvn compile                  # Build
mvn test                     # the whole suite — ONE engine, one leg (4.2.0 baseline: 1261/1261)
mvn test -Dtest=BugFixVerificationTest                                  # one test class
mvn test -Dtest=BugFixVerificationTest#testISS0188_ModNegativeDivisor   # one method
mvn clean compile            # Clean rebuild

# Run CLI
java -cp target/classes it.denzosoft.jprolog.PrologCLI

# Run IDE (or: mvn exec:java@run-ide)
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE

# Test core example programs test_01..test_20 (REQUIRED after any code change)
./test_all_examples.sh
# Success rate >= 75% for maintenance, >= 85% for new features

# Regenerate the user manual (pure Python 3, no external tools)
tools/build-manual.sh

# Other test scripts
./comprehensive_test.sh         # Built-in predicates
./test_phase1_features.sh       # ISO Phase 1
./test_phase2_features.sh       # Exception handling
./test_phase3_features.sh       # Arithmetic functions
./test-debug.sh                 # Debug features
./test_iso_examples.sh          # ISO conformance examples
./test_all_dcg_examples.sh      # DCG examples
./test_dcg_comprehensive.sh     # DCG comprehensive scenarios
./test_all_40_examples.sh       # 40-example regression set
```

Build/test gotchas:
- **None of the root `*.sh` scripts are in git** — `.gitignore` has `/*.sh`, so `test_all_examples.sh`
  and friends exist only in local working copies (the sole tracked script is
  `examples/demo_semicolon.sh`; `tools/build-manual.sh` is tracked because it is not at the root).
  A fresh clone has none of them; if a script is missing, drive `PrologCLI` directly with
  `java -cp target/classes ...` and the `examples/test_XX_*.pl` files.
- The checkout lives on a VirtualBox shared folder (`/media/sf_Cloud`): plain `git` reports
  "dubious ownership" and `git status` lists ~585 files as modified purely because of mode changes
  (644→755). Use `git -c safe.directory=$PWD -c core.fileMode=false ...` and never commit the mode
  churn.
- Bare `mvn exec:java` fails — the pom's default mainClass `it.denzosoft.jprolog.Main` does not
  exist (the only `Main` is the demo `core.engine.Main`). Use `mvn exec:java@run-ide` or the
  `java -cp` commands above. `run_ide.sh` is broken; `start-ide.sh` works but skips compilation.
- `test_all_examples.sh` enforces nothing: "PASSED" only means the CLI exited 0 within a 30 s
  timeout, and the script always exits 0. The 75%/85% thresholds are manual policy — eyeball the
  per-program output and the "Successful queries" counts (the baseline, unchanged since 4.0.0, is
  2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0).
- No surefire plugin is pinned in pom.xml, so the Maven-default surefire (2.12.4) runs the tests:
  new test classes must match `Test*` / `*Test` / `*TestCase` or they are silently skipped — note
  `*Tests` (plural) is NOT matched. That is why the tree holds 10 more `@Test` methods than the
  baseline runs: the 10 in `BuiltInTests.java` never do. `-Dtest=Class#method` also does not work
  with that surefire; run the whole class.
- Every `new Prolog()` logs ~13 `WARNING: Overriding existing built-in predicate` lines (the v2
  CLP(FD) *solver* re-registering `#=`, `label`, …). Expected noise, not a bug.

## Architecture

### Query Execution Flow (v4, the default)

1. **Parser** — the clean-room single-pass v2 parser (`core.parser.v2.Lexer` +
   `core.parser.v2.TermReader`, operator-precedence/Pratt) converts Prolog text to `Term` objects,
   reading the engine's own operator table (`core.engine.v4.Ops`). The legacy
   `core.parser.Parser` is the fallback (`-Djprolog.parser=legacy`).
2. **KnowledgeBase** (`core.engine.KnowledgeBase`) is the database of record: facts and rules as
   `Rule` objects, each carrying a `sourceLine` for line-accurate breakpoints — populated only by
   `consultWithDiagnostics` on the v2-parser path, so plain `consult()` leaves it -1.
3. **ClauseStore** (`core.engine.v4.ClauseStore`) mirrors it as compiled `Clause` skeletons with
   birth/death generations and an incremental first-argument index, re-syncing a predicate when the
   KB's version for it changes.
4. **Machine** (`core.engine.v4.Machine`) resolves: an iterative SLD drive loop over a goal stack
   and an explicit choice-point list, binding directly in mutable `Variable` cells with a
   conditional trail (`core.engine.v4.Bindings`). One `core.engine.v4.Engine` per `Prolog`, one
   `Machine` per query, one more per worker thread.
5. For each goal the machine tries, in order: its **inline** table (`=/2`, `is/2`, the comparisons,
   the type checks, `once/ignore/forall`, `between/3`), the **v4 native** table
   (`core.engine.v4.BuiltinTable`), a **user or prelude clause**, then the legacy
   `BuiltInRegistry` through `core.engine.v4.LegacyBuiltinAdapter`.
6. **Unification** is `core.engine.v4.Unify` — iterative, cycle-safe, guard-polling. (The immutable
   `Term.unify(Term, Map<String,Term>)` Robinson implementation still exists and is what the
   bridged built-ins use.)
7. **Cut** is a choice-point barrier (`Machine.cut`), and `catch/3` a CATCH frame on the same stack.

Supporting services, all per `Prolog`: `Modules` (module resolution and the prelude library layer),
`Streams`/`PrologStream` (the stream table), `Ops` (the one operator store), `Writer` (the ISO term
writer behind the whole `write/1` family), `Answer` (console answer rendering), `Tabling` (linear
tabling with completion), `Coroutining` (the wake queue and the attributed-variable protocol) and
`Workers` (one machine per thread).

### Subsystem toggles

**Note on the name "v2"**: in the four rows below it means the *second-generation clean-room
rewrite* of that subsystem (parser, CLP(FD) solver, DCG translator, arithmetic evaluator, source
formatter) — **not** an engine. The v2 *engine* (`core.engine.v2.MachineSolver`) is gone since
4.1.0; these subsystems are current and are the defaults.

| Subsystem | Default | Fallback |
|---|---|---|
| Resolution engine | **`core.engine.v4.Machine`** — the only one | — (the v2 machine and `-Djprolog.engine=v2` were deleted in 4.1.0) |
| Parser | `core.parser.v2` (`Lexer` + `TermReader`) | `-Djprolog.parser=legacy` |
| CLP(FD) | `builtin.clpfd.v2` (`ClpStore`/`IntervalDomain`/`Constraint`/`Labeler`) | `-Djprolog.clpfd=legacy` |
| DCG | `core.dcg.v2.DCGTranslator` | `-Djprolog.dcg=legacy` |
| Arithmetic | `core.arith.v2.ArithEvaluator` | — (`core.engine.ArithmeticEvaluator` survives only as a helper) |
| Term writer | `core.engine.v4.Writer` behind the whole `write/1` family (via the `core.util.TermFormatter` facade) | `core.write.v2.TermWriter` backs only the IDE source formatter |

Toggle semantics (all three properties live in `Prolog.java`):
- **Only the literal value `legacy` selects a fallback**, for the parser, DCG and CLP(FD) flags.
- `jprolog.engine` is **obsolete**: any value logs a warning at class-init and runs v4. There is no
  engine flag and no `setUseV4Engine`/`isUsingV4Engine`/`setUseV2Engine`/`isUsingV2Engine` any more.
- Parser/DCG flags are re-read on every call, so the static setters work at runtime. The **CLP(FD)
  flag is read only in the `Prolog` constructor** — set it before constructing, or upgrade an
  instance with `prolog.enableV2Clpfd()`.
- There is **no** `jprolog.sandbox`/`jprolog.budget` property — those are Java APIs (below).

Pick the right file when fixing bugs: arithmetic is `core.arith.v2.ArithEvaluator`;
output of `write/1`/`writeln/1`/`writeq/1`/`format/2` is **`core.engine.v4.Writer`**
(`core.util.TermFormatter` is a facade over it, and `core.write.v2` only backs the IDE's Ctrl+Alt+L
source formatter). Console answers are `core.engine.v4.Answer`.

### Embedding API (`core.engine.Prolog`)

- `solve(String)` / `solve(Term)` — the configured engine.
- `solveStream(query, sink)` — lazy, cancellable solution streaming; the sink
  (`Predicate<Map<String,Term>>`) returns `false` to stop.
- `consultWithDiagnostics(program, filename)` — per-clause compile collecting `CompilationError`s
  with line numbers (returns `CompilationResult`) instead of throwing on the first error.
- `compileFile("x.pl")` / `consultSmart("x.pl")` — compile to `.jpc` / auto-cached consult.
- `getPredicateIndicatorAtLine(line)` — maps a source line to the clause's predicate indicator
  (IDE gutter breakpoints).
- `getEngineContext()` — the durable `core.engine.EngineContext`: where the IDE installs its
  `DebugController` and where the running machine publishes the query's `ResourceGuard`.
  (It replaced `getQuerySolver()` in 4.0.0; `solveLegacy` is gone.)
- `getEngineState()`, `getStreams()`, `getOps()`, `getFlags()`, `residualGoals(solution)`.
- `enableSafeMode()` / `setInferenceBudget(steps)` — see hardening below.

### Sandbox & Resource Limits

- **`Prolog.enableSafeMode()`** — per-instance, **irreversible**, deny-by-package sandbox:
  unregisters every built-in whose class lives in
  `builtin.{os,ffi,filesystem,network,http,jdbc,persistence,threading}`
  (`UNSAFE_BUILTIN_PACKAGES` in `Prolog.java`; ~110 removed). Java API only. **Gotcha**:
  `builtin.io` is *not* denied, so `open/3,4` can still read/write host files — file isolation
  needs OS-level sandboxing (or extending the deny list). When adding a host-touching built-in, put
  it in one of the denied packages or safe mode will not strip it.
- **`Prolog.setInferenceBudget(steps)`** (0 = unlimited) — aborts a runaway query with
  `InferenceLimitException`, enforced through `core.engine.ResourceGuard` inside meta-call
  sub-solves and worker threads too. (The javadoc claims it raises
  `resource_error(inference_limit_exceeded)` — wrong; trust the implementation and
  `ProductionAuditTest`.)
- **Error-trust model** — deliberate asymmetry: deep-term/deep-input `StackOverflowError`s from a
  bridged built-in are converted to a `PrologException` carrying ISO
  `resource_error('stack_overflow')` / `resource_error('parser_nesting')` so the Java embedder sees
  a controlled error, while `InferenceLimitException` (budget), `QueryCancelledException`
  (IDE/embedder Stop, via thread interrupt) and `DebugStopException` (debugger Stop) are plain
  `RuntimeException`s — **not** `PrologException` — so untrusted `catch/3` cannot swallow them.
  `core.engine.ControlFlow.rethrowIfControl(t)` must be the first statement of any broad
  `catch (Exception/RuntimeException)`. Preserve this distinction when adding limits.

### Engine v4

The clean-room core designed in `docs/reports/report-engine-v4-design-2026-08-25.md`. All nine
waves are done (v3.9.0: ISS-2025-0438..0449; v3.10.0: 0450..0456; v3.11.0: 0457..0462;
v3.12.0: 0463..0465; v3.13.0: 0466..0471; v3.14.0: 0472..0477; **v4.0.0: 0478..0488**), and so are
the two 4.1 waves (**v4.1.0**, one engine: 0491..0495; **v4.2.0**, the L-08 built-in migration:
0496..0501). Progress, the 60 invariants, the benchmarks and what remains live in
`docs/reports/report-engine-v4-progress.md` — **read it before touching `core.engine.v4`**;
sections 9–17 are the wave records.

**Package `core.engine.v4`**:
- `Machine` — the drive loop: goal stack, choice points, cut, catch/throw, findall, cleanup frames,
  native control constructs, the four ports, the database operations, query normalisation, answers.
- `Unify` — every term walker: `unify`, `==`, standard order, `resolve`, `copy_term`,
  `term_variables`, `ground`, `numbervars`, `subsumes_term`, `cyclic_term`, `occurs`.
- `Bindings` — the trail and conditional trailing (there is no binding store).
- `ClauseStore` + `Clause` + `VarRef` — compiled skeletons, generations, first-arg index, the
  prelude library layer.
- `Builtin` / `Generator` / `BuiltinTable` / `NativeBuiltins` — the v4 built-in SPI.
- `NativeControl` (phrase, bagof, setof, aggregate_all, with_output_to), `NativeLibrary` (the lazy
  list/atom/database generators), `NativeMisc` (sort/4, predsort/3, max_list, min_list, current_op,
  nb_getval, b_getval), `ClpfdNative` (the cell-based CLP(FD) posting and labeling),
  `Lambdas` (yall), `ModuleBuiltins`, `Tabling`, `Coroutining`; and, since 4.2.0,
  **`NativeIo`** (format/1,2,3 + the whole write family + the character I/O),
  **`NativeText`** (atom_*/string_*/number_*/char_code/atomic_list_concat/split_string/
  term_to_atom/term_string + keysort/delete/flatten),
  **`NativeTerm`** (functor/arg/=../atom_to_term, the remaining type checks, succ/plus,
  unify_with_occurs_check) and **`NativeDb`** (current_predicate/retractall/abolish/dynamic/
  listing, the global variables, the ISO flags, halt, findall/4).
- `Modules` — the module owner (system/user/library, resolution order, imports, meta_predicate);
  `Prelude` — indexes and autoloads `prelude/*.pl`.
- `Workers` — one `Machine` per thread over the same `Engine`.
- `LegacyBuiltinAdapter` + `SolverFacade` — the **234** remaining registry built-ins, unchanged
  (4.1 wave B took 94 indicators off it). What is left is essentially the extended libraries plus
  the stream/parser half of `io`; see LIM-037 and the residual list below.
- `Engine` — the per-`Prolog` context; `Errors` — ISO error construction.
- `EngineState` (the thread-current per-engine state), `Streams` + `PrologStream`, `Ops`, `Writer`,
  `Answer` — the W7 services the bridged built-ins reach through their static facades.
- `Undo` — the doorway a bridged built-in uses to push a backtrackable undo action onto the running
  machine's trail (`b_setval/2`, `op/3`, `setarg/3`, the CLP(FD) store). It replaced the static
  `core.engine.Trail` in 4.1.0; there is ONE trail now.

**What the core does differently**: `Variable` is a mutable cell (`ref`, `serial`, identity
`equals`/`hashCode`, lazy `_G<serial>` name), so bindings are reclaimed by the JVM and
`loop(10000000)` runs in 64 MB; clauses are compiled once into numbered-variable skeletons and the
head is unified directly against them (nrev ~2 MLIPS vs ~316 KLIPS on the v2 machine); every walker is
iterative, cycle-safe (rational trees **succeed**) and polls the `ResourceGuard`; clauses carry
birth/death generations so `assertz`/`retract` are O(1).

**Deliberate behaviour differences** (design B.17, approved): rational trees are supported
(`set_prolog_flag(occurs_check, error)` restores the ISO error); `setup_call_cleanup/3` and
`call_cleanup/2` run `Cleanup` after the goal's LAST solution; `append(X,Y,Z)` fully open
**enumerates** and `member(X, PartialList)` **extends** the open tail (a program that relied on the
old termination loops must be rewritten). All three are pinned in `BugFixVerificationTest`.

**Unchanged on v4**: the embedding API and `Map<String,Term>` results, `enableSafeMode()`, the
inference budget, the trust model, the IDE debugger contract, and the four-port trace.

### Rules that bite in `core.engine.v4`

- **The trail rule** (ISS-2025-0448, invariant 1): any construct that takes a `Bindings` mark and
  undoes to it must keep `forceTrail` raised until *after* the undo — `cut()` and the trust-me pop
  end in `clearIfUnreachable()`, which drops the whole trail as soon as `forceTrail` is 0 and no
  choice point is left. Getting this backwards is what made `findall/3` non-opaque.
- **`EXHAUSTED` is the "no more alternatives" sentinel, never `null`** — `null` is a perfectly good
  goal stack (an empty continuation).
- **A generator that tries several alternatives inside one `next()` must undo its own failed
  unifications** (`Machine.unifyOrUndo`, or one explicit mark/undo extent around a whole group of
  unifications — a partially-bound argument makes every later alternative fail; that is what broke
  `current_op/3` in W9, invariant 49).
- **A generator announces its last alternative with `Machine.lastSolution()`**, or the choice point
  survives and defeats the trust-me pop.
- **The prelude is the LAST resort, never the first**: `selectClauses` looks at
  `ClauseStore.libraryClauses` only when the KB-backed `Predicate` is empty, and `stepN` routes a
  library indicator to `callUser` *before* the legacy adapter. Both halves are needed.
- **A term must be COPIED before it crosses a machine boundary** — bindings live in the cells.
  `Workers` copies the goal in and every answer out; the message queues copy on send and receive.
- **A worker gets its OWN `ResourceGuard`**, constructed with the parent's limit.
- **A new port site takes its depth from `enterPort()` and returns it by ASSIGNING it** in
  `portExit`/`portFail`; incrementing/decrementing drifts.
- **Never re-introduce a "skip this when `debugController != null`" branch** (limit L-13). If a
  fast path has no ports, give it ports.
- **`Machine.Goal.module` is the context module** (`null` == `user`); every nested drive and the
  `catch/3` recovery frame must save/restore it, and a new construct that pushes a goal must stamp
  it (`Machine.mg`).
- **Three tabling rules**: a `!` in a tabled clause body stays local (`bodyBarrier` is `cps.size()`
  AFTER the generator CP is pushed); an evaluation abandoned by an exception, a cut or the guard
  must **discard** its tables; the production template is created **before** `pushCP` and reused
  across rounds. And since W9: **a tabled evaluation belongs to one thread** — claim it with
  `Tabling.enterCall`, hold it until the SCC completes/aborts/the query ends, and hand it back from
  a worker in `Machine.solve`'s finally (`Tabling.endWorker`).
- **Two identity rules**: a named `Variable` `copy()`s to **itself** (ten built-ins rely on the
  alias), and a clause skeleton numbers its variables **by name** (JpcReader and the legacy parser
  allocate one object per occurrence). Query terms are normalised to one cell per name.
- **The writer must stay iterative and cycle-safe** — it is what prints a rational tree.
- **A stream argument is a term, not a string** — resolve it with `IOStreamUtils.inputStream` /
  `outputStream` / `StreamManager.stream(Term)`.
- **Anything a built-in prints goes through `StreamManager.out()`** — `with_output_to/2` captures
  through the thread-local override alone, so a `System.out.print` escapes it. That includes
  `Prolog.listing()` (ISS-2025-0499).
- **A native must NOT `resolve` the term it is about to print, walk or unify.** `Writer`, `Unify`
  and every `NativeLibrary` walker deref as they go; `m.resolve(t)` is a full copy and is what made
  the bridge slow. Use it only for the small things — a stream argument, an option list, an error
  culprit.
- **Protection is a question about three stores, not one.** `Machine.isProtectedProcedure` asks
  `BuiltInRegistry.isBuiltIn`, `BuiltinTable.isNativeKey` and `Modules.isLibraryIndicatorKey`;
  `checkModifiable` (assert/retract), `NativeLibrary.ClauseB` (`clause/2`) and
  `NativeDb.checkModifiable` (`retractall`/`abolish`) all go through it. Consult does NOT — it
  checks the registry only (`Prolog.checkBuiltInConflict`), which is what keeps the documented
  library-override rule (a module may define its own `partition/4`) working.

### Term Hierarchy

All terms are immutable **except `Variable`**, which since v3.9.0 is a mutable reference cell.
Base class: `core.terms.Term`
- `Atom` — symbols like `hello`, `'John'`
- `Number` — integers and floats. `new Number(double)` is *always* a float; use
  `new Number(long)` / `Number.valueOf(long)` for integers (ISS-2025-0424).
- `Variable` — logic variables (`X`, `_`); carries `ref` (the binding, `null` when unbound) and a
  JVM-unique `serial`. `equals`/`hashCode` are **identity**; `getName()` is unique per cell;
  `copy()` of a named variable returns `this`.
- `CompoundTerm` — compound terms like `f(a, b)`
- `PrologString` — string literals

### Built-in Predicate System

There are **two** SPIs, and new work should use the first:

**1. The v4 native SPI** (`core.engine.v4`): implement `Builtin` (deterministic — return
`Outcome.SUCCESS` / `FAILURE`) or push a `Generator` (nondeterministic — return
`Outcome.SUSPENDED`) and register it in `NativeBuiltins.register` / `NativeControl.register` /
`NativeLibrary.register` / `NativeMisc.register` / `NativeIo.register` / `NativeText.register` /
`NativeTerm.register` / `NativeDb.register`. A native sees dereferenced `Term[] args` and the
`Machine`; it never builds a `Map<String,Term>`. **160 indicators (136 names) are native**, and
~44 more are handled inline by the machine.

**How the io natives are laid out** (`NativeIo`, 4.1 wave B): one `Builtin` class per *shape*, not
per predicate — `WriteB(Kind, streamArg)` covers `write`/`writeln`/`writeq`/`print`/
`write_canonical` at both arities by choosing a `Writer.Options` preset, `GetB(asChar, peek,
streamArg)` covers the eight `get_*`/`peek_*` entries, and the format-directive engine is the
private `Fmt` class, a faithful port of `builtin.io.Format.processFormat` that reads dereferenced
cells instead of a `Map<String,Term>` and calls back into the running machine
(`Machine.runSubQuery` with the output captured through `StreamManager.setThreadLocalOutput`) for
`~@` and the `portray/1` hook. A stream argument is resolved with
`IOStreamUtils.resolveOutputStream(m.resolve(arg), emptyMap, ctx)` — a TERM, never a string — and
the term to print is handed to `core.engine.v4.Writer` **unresolved**, because the writer derefs as
it walks. That last point is the whole performance story: the bridge had to copy the term first.

**2. The legacy registry SPI** (`core.engine`), which the **234** remaining built-ins use — the
extended libraries (jdbc, filesystem, threading, crypto, ffi, graph, network, persistence, os,
http, datetime, json, logging, regex, dcg, csv, xml, clpfd), the stream/parser half of `io`
(`open`, `close`, `read`, `read_term`, `stream_property`, `seek`, the byte I/O, `print_message`,
`portray_clause`), `op/3`, `statistics/2`, `char_type/2`, `code_type/2`, `table/1` and the debug
and profiler predicates (LIM-037):
- `BuiltIn.execute(Term query, Map bindings, List solutions)` — the eager contract: a *resolved*
  goal, an empty bindings map, one solution map appended per answer.
- `BuiltInWithContext.executeWithContext(SolverContext solver, Term query, Map bindings,
  List solutions)` — for a built-in that runs a sub-goal. Since 4.0.0 the context is the
  **`core.engine.SolverContext` interface**, not a solver class: `solveMeta(Goal, Bindings,
  Solutions)` for a sub-goal, `solve(Term)` for its solution list, `solveInWorker(...)` for a goal
  that must run on another thread, plus read-only `getPrologContext`/`getKnowledgeBase`/
  `getBuiltInRegistry`/`getDebugController`/`getResourceGuard`. Inside a query the object is the
  per-query `core.engine.v4.SolverFacade` (running on `Machine.runSubQuery`); a caller that holds
  only the durable `core.engine.EngineContext` gets a fresh machine. There is no `CutStatus` and no
  cut-propagating sub-solve: the control constructs that propagated a cut outwards are native.

**Registration**: `BuiltInFactory` has a static `FACTORY_MAP` of predicate names to
`Supplier<BuiltIn>`; the `Prolog` constructor iterates it into `BuiltInRegistry`. A
`BuiltInWithContext` is registered **unwrapped** — the wrapper that used to pin one solver at
registration time is deleted.

`BuiltInRegistry.isBuiltIn(name, arity)` needs BOTH a registration and an arity entry, and it is
what makes `assertz`/`retract`/`clause/2` raise `permission_error` on a built-in. That is why the
ISO control constructs (`;`, `->`, `\+`, `call`, `catch`, `^`) keep a registry entry — the 40-line
`builtin.control.ControlConstruct` placeholder — even though both machines implement them natively
and never dispatch them.

**Adding a native built-in (preferred)**:
1. Implement `Builtin`/`Generator` in the right `core.engine.v4.Native*` file and register it.
   Registering it is also what makes `assertz`/`retract`/`clause` raise `permission_error` for it
   (`Machine.isProtectedProcedure` asks the registry, the `BuiltinTable` and the prelude owner
   index) — there is no separate arity table to maintain, unlike the legacy registry.
2. Follow the three rules: `Machine.unifyOrUndo` (or one explicit extent) when you try several
   alternatives inside one `next()`; `Machine.lastSolution()` on the last alternative; never take a
   `Bindings` mark without the `forceTrail++` / undo / `forceTrail--` ordering of invariant 1.
3. Update `docs/references/BUILTIN_PREDICATES_REFERENCE.md` and, if it has no entry there,
   `tools/manual/supplement.md`; run `tools/build-manual.sh`.
4. Run `mvn test` and `./test_all_examples.sh`.

**Adding a legacy built-in** (only when the eager registry contract is genuinely the right fit):
1. Create the class in `it.denzosoft.jprolog.builtin.<category>/` implementing `BuiltIn` or
   `BuiltInWithContext`. If it touches the host (processes, files, network, JVM, threads) it must
   live in one of the safe-mode denied packages.
2. Register it in `BuiltInFactory.FACTORY_MAP` (and add an arity entry to `BuiltInRegistry` if the
   name needs one).
3. Same documentation and test steps as above.

Output discipline: built-ins must write through `builtin.io.StreamManager.out()` — never
`System.out`/`System.setOut`. `StreamManager` is a static facade over `core.engine.v4.Streams`, the
stream table of the engine current on the calling thread; `StreamManager.setThreadLocalOutput(ps)`
is a process-wide **per-thread** override that wins over every engine's `current_output` — that is
how the IDE captures a background solve's output (reset in `finally`) and the only mechanism
`with_output_to/2` and `format/3 atom(A)` use. Term output must go through `core.engine.v4.Writer`
(via `core.util.TermFormatter`), never `Term.toString()`.

### Package Layout

- `core.engine` — `Prolog`, **`EngineContext`** (the durable per-engine context: debug controller,
  query `ResourceGuard`, the legacy attribute hook), **`SolverContext`** (the interface a context
  built-in receives), `KnowledgeBase`, `BuiltInRegistry`, `BuiltInFactory`, `TableStore` (the
  `:- table` declarations ONLY — the answer tables are `core.engine.v4.Tabling`),
  `ArithmeticEvaluator`, the debug classes (`DebugController`, `DebugEvent`, `DebugStackEntry`),
  `ControlFlow`, `ResourceGuard`, `InferenceLimitException`, `QueryCancelledException`,
  `NeedsSolverContextException`
- `core.engine.v4` — the engine (see above), plus the W7 services
- `core.terms` — `Term`, `Atom`, `Number`, `Variable`, `CompoundTerm`, `PrologString`
- `core.parser` — legacy `Parser`, `PrologParser`, `TermParser` (fallback); `core.parser.v2` —
  `Lexer` + `TermReader`, the default
- `core.operator` — `Operator`, `OperatorTable` (the table `core.engine.v4.Ops` owns)
- `core.module` — `Module`, `ModuleManager`, `PredicateSignature` (the consult-time recorder; on
  v4 `core.engine.v4.Modules` is the resolver)
- `core.dcg` — `DCGTransformer` (fallback); `core.dcg.v2.DCGTranslator` is the default
- `core.write.v2` — `TermWriter` + `PrologFormatter` (IDE source formatter only);
  `core.arith.v2.ArithEvaluator` — the engine's arithmetic
- `core.compiled` — `JpcReader`, `JpcWriter`, `JpcFormat` (`.jpc` binary format)
- `core.exceptions` — `PrologException` and ISO error-term helpers
- `core.system` — `PrologFlags` (the per-engine ISO flag store)
- `core.util` — `TermFormatter` (the entry point of the `write/1` family; a facade over
  `core.engine.v4.Writer`), `ListUtils`; distinct from `core.utils` (`ListTerm`, `Substitution`,
  `CollectionUtils`) and from top-level `util` (`TermCopier`, `TermUtils` — rule-copy renaming,
  used by the KB and the bridged built-ins, not by the v4 machine)
- `builtin/` — by category. ISO-core: `arithmetic/`, `atom/`, `character/`, `control/`,
  `conversion/`, `database/`, `dcg/`, `debug/`, `exception/`, `io/`, `list/`, `meta/`, `string/`,
  `system/`, `term/`, `type/`, `unification/`. Extended: `clpfd/` (with `clpfd/v2/`), `crypto/`,
  `csv/`, `datetime/`, `ffi/`, `filesystem/`, `graph/`, `http/`, `jdbc/`, `json/`, `logging/`,
  `network/`, `os/`, `persistence/`, `regex/`, `threading/`, `xml/`, `extension/`
- `extension/` (top-level) — pluggable extension examples (`example/`, `math/`)
- `editor/` — Swing IDE: `PrologIDE` (owns the single shared `Prolog` — panels fetch it via
  `ide.getPrologEngine()`, never construct their own), `FileEditor`, `EditorTabbedPane`, and the
  bottom tabs `OutputConsole`, `BuildPanel`, `RunPanel`, `SearchResultsPanel`, `DebugPanel`
- `src/main/resources/prelude/` — the engine's Prolog **library modules**, each headed by
  `:- module(Name, [Exports])` and autoloaded by predicate indicator into `core.engine.v4.Modules`:
  `lists.pl` (member/2, append/3), `apply.pl` (maplist/foldl/include/exclude/partition plus their
  `meta_predicate` declarations), `pairs.pl`, `coroutining.pl` (freeze/frozen/when/dif/?= and the
  `'$attr_hook'/4` dispatcher). The `KnowledgeBase` never sees them, so neither `listing/1` nor
  the IDE does. Maven picks the directory up by default.
- `PrologCLI` — CLI with `:consult`/`:c`, `:compile`/`:cc` (to `.jpc`), `:listing`/`:l`, `:save`/`:s`,
  `:clear`, `:trace [on|off]`, `:help`/`:h`, `:quit`/`:q`. It detects a **non-interactive** stdin
  (`System.console() == null`, or `--batch` / `-q`) and prints every solution at once, separated by
  ` ;` and terminated by `.`, instead of reading the next input line as the answer to the "more
  solutions?" prompt.

### Debug & Tracing Architecture

The debugger uses a **two-thread model** with blocking synchronization:

1. **Solver Thread**: runs the query in a background thread
2. **Swing EDT**: handles UI updates and button clicks
3. **Synchronization**: `DebugController` uses `wait()/notify()` on a `pauseLock` object;
   `DebugPanel` implements `DebugController.DebugListener` and marshals to the EDT via
   `SwingUtilities.invokeLater()`

**Wiring contract**: install the controller with
`prolog.getEngineContext().setDebugController(...)` — the `EngineContext` is the durable home.
Each per-query machine picks it up at solve start and fires the four-port events
(Call/Exit/Fail/Redo), so the IDE debugger runs via plain `engine.solve()`.
Since 4.1.0 the machine asks `DebugController.needsPorts()`, not just `!= null`: a controller with
no listener, no breakpoint, in CONTINUE mode and with no Stop pending can observe nothing, so no
port is emitted for it at all (ISS-2025-0494). Attaching an idle controller is free.

Engine specifics:
- **The fast paths are NOT disabled while debugging** (limit L-13). The machine keeps its
  inline `=/2`, `is/2`, comparisons, type checks, `once/ignore/forall` and `between/3` and emits
  their four ports itself, so a debugged run executes exactly the same code as an undebugged one.
  Never re-introduce a "skip this when a controller is attached" branch: add the ports instead
  (`isInlineBuiltin` gates the deterministic ones, `Machine.iteTraced` owns the control constructs'
  wrapper ports, a lazy generator carries `cp.traceGoal`).
- **Depth is `Machine.portDepth`** — the call-nesting level, assigned by every port. It is
  NOT `cps.size()`, because a deterministic frame is trust-me popped even while tracing.
- **A deterministic frame emits no phantom `Fail` after its `Exit`**, which is what keeps
  trace memory linear in the number of OPEN calls.
- Redo/Fail ports are emitted by stashing `traceGoal`/`traceDepth` on choice points; a new
  choice-point kind representing a traced goal must carry these fields.
- Breakpoints: line-accurate via `Rule.sourceLine` + `Prolog.getPredicateIndicatorAtLine()`;
  persisted in hidden sidecar files `.<source>.bps` next to `.pl` files. Conditional/hit-count
  breakpoints and the watch panel evaluate their goal via a **detached sub-solve**
  (`engine.solve()` with the controller temporarily nulled on the `EngineContext`); a throwing
  condition never pauses.
- `trace/0`/`notrace/0` (and the CLI `:trace`, the IDE Run-panel Trace toggle) set the **per-engine**
  trace flag (`Prolog.setTracing`, stored in the engine's `PrologFlags`); the machine checks it and
  writes depth-indented four-port lines to `StreamManager.out()` (indentation capped at 40 levels,
  the depth number itself exact). `EngineV4TraceTest` pins the output line for line: change it
  only deliberately. (`Prolog.setTraceEnabled/1` is a *different*, vestigial flag — it drove the
  recursive solver's `LOGGER.info` tracing and now only records a boolean.)

### Compilation Diagnostics

`Prolog.consultWithDiagnostics(program, filename)` compiles per-clause, collecting errors with line
numbers instead of throwing on the first error. Returns `CompilationResult` with
`List<CompilationError>`. The IDE renders these in the Build tab plus in-editor squiggles and gutter
markers.

### Binary Compiled Format (.jpc)

JProlog compiles `.pl` source to `.jpc` for fast loading (`JpcWriter` / `JpcReader` /`JpcFormat` in
`core.compiled`): string interning, varint encoding, per-clause source lines, variables by clause
index, and source-hash validation to detect stale compilations. The one `OperatorTable` is shared
with the parser, `op/3` and the writer, so dynamic operators round-trip.

### Key Design Decisions

- **One `Machine` per thread** (a machine is single-threaded by construction). The IDE runs queries
  on background threads with thread-local output capture and cancels via thread interrupt →
  `QueryCancelledException`; `thread_create/2,3` and the `concurrent_*` family get a fresh machine
  over the same `Engine` through `core.engine.v4.Workers`, with the goal copied in and every answer
  copied out.
- **Immutable terms** except `Variable`; the engine binds in the cell with a trail, and the
  bridged built-ins push their own undo actions onto the same trail through `core.engine.v4.Undo`.
- ISO 13211-1 compliance where possible (exception handling, arithmetic functions, error terms).
- `.gitignore` scratch patterns must stay **anchored to the root** (`/Debug*.java`, `/Test*.java`,
  `/*.sh`) — the unanchored forms once silently excluded core sources from the repo (ISS-2025-0334).

## Development Workflow

### Issue Tracking (MANDATORY)

Every bug or feature request must be documented before implementation:

- **Issues**: `docs/tracking/track-issues.md` — format `ISS-YYYY-NNNN`; terminal status in practice
  is `RESOLVED`/`VERIFIED`
- **Change Requests**: `docs/tracking/track-change-requests.md` — format `CR-YYYY-NNNN`
- **Limitations**: `docs/tracking/track-limitations.md` — add when found, remove when resolved
- **Release Notes**: `docs/tracking/track-release-notes.md`

Before allocating a new ISS number, grep **CHANGELOG.md** and `src/` (`START_CHANGE` tags) for the
highest used one (**ISS-2025-0501** as of 4.2.0) — track-issues.md lags behind recent releases.
Its internal ordering and header levels are inconsistent; grep for an ID rather than assuming
position. Some tracking content is in Italian — match surrounding style rather than rewriting.

### Code Change Tagging

All code modifications must be tagged with the ID that drove the change (ISS, LIM, or CR):
```java
// START_CHANGE: ISS-2025-0001 - Description
// ... modified code ...
// END_CHANGE: ISS-2025-0001
```
For a **deletion**, tag the surviving call site and record the deleted class in CHANGELOG.md.

### Test Verification (MANDATORY per bug/limitation fix)

Every resolved bug or limitation must have a JUnit test that fails without the fix. Tests go in
`src/test/java/it/denzosoft/jprolog/test/builtin/BugFixVerificationTest.java` (~420 tests, organized
by ISS number); `test/audit/` holds the earlier audit suites (`ProductionAuditTest`,
`AuditRound5Test`); the clean-room subsystems each have their own `*Test` next to the package
(`core/parser/v2/NewParserTest`, `builtin/clpfd/v2/ClpfdV2Test`, …);
`core/engine/EngineHardeningTest` holds the ENG-01..ENG-17 regressions; and each engine wave has
its own `core/engine/v4/EngineV4*Test`:
`EngineV4Test` (34, W1/W2), `EngineV4LibraryTest` (19, W3), `EngineV4CoroutiningTest` (22, W4),
`EngineV4TablingTest` (18, W5), `EngineV4ModulesTest` (29, W6), `EngineV4StreamsTest` (25, W7),
`EngineV4WriterTest` (21, W7), `EngineV4ThreadsTest` (15, W8), `EngineV4TraceTest` (25, W8 — 16
line-for-line pinned trace oracles), `EngineV4RetirementTest` (17, W9),
`EngineV41RetirementTest` (21, 4.1 wave A) and **`EngineV4IoTest` (19), `EngineV4TextTest` (16),
`EngineV4TermTest` (13), `EngineV4DatabaseTest` (17) — 4.1 wave B**, plus
`test/cli/PrologCliBatchTest` (6). A wave-B class opens with a `test*IsNative` method that fails
the moment one of the migrated indicators is not in the `BuiltinTable`; the rest pin the modes and
the ISO error terms so a migration cannot quietly change one.
- Add a `@Test` method named after the issue and fix (e.g. `testISS0188_ModNegativeDivisor`)
- The test must fail without the fix and pass with it
- Use `prolog.solve()` for query-level assertions, direct Java assertions for internal fixes
- Every wave must keep the full suite green (4.2.0 baseline: **1261/1261**). There is one engine
  and one leg, so a test never selects an engine (ISS-2025-0491 removed the `setUp`/`tearDown`
  toggles the v4 classes used to carry).

### Documentation Updates (MANDATORY per release)

When adding/modifying built-ins or operators, update:
- `docs/references/BUILTIN_PREDICATES_REFERENCE.md`
- `docs/references/BUILTIN_OPERATORS_REFERENCE.md`

(These two uppercase filenames are deliberate exceptions — do not rename them.)

The user-facing **Reference Manual** (`docs/guides/guide-builtin-manual.md` + `.pdf`, every default
operator and built-in with examples) is *generated*: edit `tools/manual/{front,supplement,appendix}.md`
(the supplement holds the ~110 predicates that have no entry in
`BUILTIN_PREDICATES_REFERENCE.md`) or the reference itself, then run `tools/build-manual.sh`
(pure Python 3, no external tools; `tools/manual/md2pdf.py` is the renderer). Only reference
sections 1–28, 29 (Java FFI) and 40 (concurrency) are included — the other sections describe
libraries that are not registered by default.

All documentation must be in English. Naming convention: `[category-]descriptive-name.md`
(lowercase, hyphens, no underscores).

Doc directories: `docs/guides/guide-*.md`, `docs/references/ref-*.md`, `docs/reports/report-*.md`,
`docs/tracking/track-*.md`. `docs/ISO132111/` holds bilingual ISO-chapter walkthroughs
(`ChapterN_Topic_EN.md` / `_IT.md`) — CamelCase names and Italian content are a deliberate
exception; keep the EN/IT pairs in sync.

### Release Process

1. `mvn clean compile` (must succeed)
2. `mvn test` (must pass — one engine, one leg)
3. `./test_all_examples.sh` (must be >= 75% — checked manually)
4. `tools/build-manual.sh` if any predicate or operator changed
5. Increment the version in `pom.xml`
6. Update: `CHANGELOG.md`, `docs/tracking/track-issues.md`, `docs/tracking/track-limitations.md`,
   `docs/tracking/track-release-notes.md`
7. Tag format: `v{major}.{minor}.{patch}`

Versioning: major = breaking changes, minor = new features, patch = bug fixes.

### Session Cleanup

Before concluding any session, remove temporary files from the project root:
```bash
rm -f *.class *Test.java Debug*.java temp_*.txt *_debug.* test_input.txt
mv test_*.pl examples/ 2>/dev/null || true
rm -f temp_*.pl debug_*.pl
```

Test `.pl` files belong in `examples/` with pattern `test_XX_description.pl`.
