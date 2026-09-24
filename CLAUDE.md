# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

JProlog is a Prolog interpreter written in Java (source/target 1.8; it builds and runs fine on a
modern JDK — JDK 25 / Maven 3.9 in the dev environment). It has a clean-room resolution core, 443
registered built-in names plus 264 v4-native indicators, a Swing IDE and a CLI (also a runnable
jar). No external dependencies
beyond JUnit 4.

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
**Current version**: `<version>` in pom.xml (4.6.0 — the completeness release, waves Q1..Q7 of
`docs/reports/report-completeness-4.6-2026-09-23.md`; 4.5.0 was the production-readiness release,
`report-production-readiness-2026-09-23.md`). pom.xml and CHANGELOG.md are the source of truth;
README.md is refreshed at release time and may lag between releases. The version flags
(`version` = 40600, `version_data`, `prolog_version` in `core.system.PrologFlags`) and
`EngineV45ReleaseTest.testISS0675_VersionFlags` are bumped by hand at each release.
**Deliberate deviations** from ISO/SWI live in ONE place: `docs/references/ref-deviations.md`
(reference semantics: ISO 13211-1 first, SWI-Prolog 9 where ISO is silent).

## Build & Run

```bash
mvn compile                  # Build
mvn test                     # the whole suite — ONE engine, one leg (4.6.0 baseline: 1547/1547)
mvn package -DskipTests      # + target/jprolog.jar (runnable, Main-Class PrologCLI)
mvn test -Dtest=BugFixVerificationTest                                  # one test class
mvn test -Dtest=BugFixVerificationTest#testISS0188_ModNegativeDivisor   # one method
mvn clean compile            # Clean rebuild

# Run CLI (or: java -jar target/jprolog.jar [options] [file.pl ...]; or: mvn exec:java)
java -cp target/classes it.denzosoft.jprolog.PrologCLI
java -jar target/jprolog.jar prog.pl -g main -t halt      # -g/-t/--safe/--budget/--demo/-q ...

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
- `mvn exec:java` runs the CLI (4.5.0 fixed the pom's default mainClass, which named a class that
  did not exist); `mvn exec:java@run-ide` runs the IDE. `run_ide.sh` is broken; `start-ide.sh`
  works but skips compilation.
- `test_all_examples.sh` enforces nothing: "PASSED" only means the CLI exited 0 within a 30 s
  timeout, and the script always exits 0. The 75%/85% thresholds are manual policy — eyeball the
  per-program output and the "Successful queries" counts (the baseline, unchanged since 4.0.0, is
  2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0). Since 4.5.0 the CLI loads its demo
  facts (`father/2`, `likes/2`, …) only with `--demo`, and test_01's two successes depend on them:
  the (untracked) script must run `PrologCLI --demo` — a copy that does not gets 0 for test_01.
- No surefire plugin is pinned in pom.xml, so the Maven-default surefire runs the tests (2.17 with
  this Maven, per the build log): new test classes must match `Test*` / `*Test` / `*TestCase` or
  they are silently skipped — note `*Tests` (plural) is NOT matched (the one such class,
  `BuiltInTests.java`, was deleted in 4.5.0). `-Dtest=Class#method` does not work here; run the
  whole class (`-Dtest=A,B` works).
- The dev machine is shared and memory-tight: `export MAVEN_OPTS="-Xmx512m"`, tests with
  `-DargLine="-Xmx1g"`, `mvn -o`, one JVM at a time. The full suite takes 1–7 min (load-dependent).
- `ExtendedLibraryErrorsTest` writes `target/extended-library-errors.txt`: per-family counts of what
  every registered name does with unbound / `f(x)` arguments, the offending goals, and (since 4.6
  Q7) the review list of goals that fail or succeed — read it after touching a library.
- The CLP(FD) *solver* re-registers `#=`, `label`, … on every `new Prolog()`; since 4.5.0 that is
  logged at FINE (it printed 13 WARNING lines per engine, ISS-2025-0674).

## Architecture

### Query Execution Flow (v4, the default)

1. **Parser** — the clean-room single-pass v2 parser (`core.parser.v2.Lexer` +
   `core.parser.v2.TermReader`, operator-precedence/Pratt) converts Prolog text to `Term` objects,
   reading the engine's own operator table (`core.engine.v4.Ops`). The legacy
   `core.parser.Parser` is the fallback (`-Djprolog.parser=legacy`).
2. **KnowledgeBase** (`core.engine.KnowledgeBase`) is the database of record: facts and rules as
   `Rule` objects, each carrying a `sourceLine` for line-accurate breakpoints — populated only by
   `consultWithDiagnostics` on the v2-parser path, so plain `consult()` leaves it -1 — and a
   `sourceFile` (4.6 Q3, ISS-2025-0730): the file that owns a consulted clause (null = asserted).
   A reconsult removes only the file's own clauses (plus, for a non-multifile predicate, the
   asserted ones); a multifile predicate's reloaded clauses keep their place (ISS-2025-0794,
   `rulesAfterFirstOwnedBy`/`moveToEnd`). Since 4.5.0
   (P2) each predicate is ONE `PredEntry` (created once, never replaced) holding a gap-buffer
   `RuleSeq`: O(1) `asserta`/`assertz`, O(1) retract of a stored `Rule` (slot hint), one-pass
   `retractAllClauses`; there is no global rule list (`getRules()` rebuilds the order from
   per-rule sequence numbers) and no KB-side first-argument index any more.
3. **ClauseStore** (`core.engine.v4.ClauseStore`) mirrors it as compiled `Clause` skeletons with
   birth/death generations and an incremental first-argument index, syncing a predicate through
   its `PredEntry` handle (a volatile version read, no map lookup). Clause sequences are gap
   buffers handed out as `ClauseStore.View` windows (no copies); a bucket and the variable-headed
   clauses are merged by clause ordinal, cached per bucket. **Every** clause-selection path goes
   through that index — calls, `retract/1`, `retractall/1` (4.5.0) and `clause/2`; see
   "First-argument indexing" below.
4. **Machine** (`core.engine.v4.Machine`) resolves: an iterative SLD drive loop over a goal stack
   and an explicit choice-point list, binding directly in mutable `Variable` cells with a
   conditional trail (`core.engine.v4.Bindings`). One `core.engine.v4.Engine` per `Prolog`, one
   `Machine` per query, one more per worker thread. Since 4.5.0 (P2) a compound body goal of a
   compiled clause is a `Clause.Skel` carrying a `Machine.CallSite` (the resolved
   `ClauseStore.Predicate` or module predicate), valid while `Engine.dispatchStamp()` is unchanged;
   `,`/`;`/`->`/`*->` in clause bodies are expanded lazily over the frame (`stepControlSkel`), and
   body-only variables get their cells at clause activation (ISS-2025-0551). Since 4.6 (Q6,
   ISS-2025-0777) RUN-TIME goals (`call/N`, findall/forall/`\+`/once bodies, the top level) have
   call sites too: a per-machine direct-mapped (name, arity) cache (`rtLookup`/`rtStore`) validated
   by context module + `dispatchStamp()`. Answers handed to Java are COPIES (`Unify.copyAnswer`,
   ISS-2025-0514) and omit `_`-variables.
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
tabling with completion; one table SPACE per worker thread), `Coroutining` (the wake queue and the
attributed-variable protocol), `Workers` (one machine per thread), the recorded database and
`flag/3` store (`NativeRecords.Store`, 4.6 Q7) and the working directory (`EngineState`, 4.6 Q4).

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
  (`Predicate<Map<String,Term>>`) returns `false` to stop. `solveStream(String, AnswerSink)` (4.5.0)
  also tells the sink whether alternatives remain (`Machine.hasAlternatives()`) — the CLI uses it.
- A query that does not parse raises `error(syntax_error(Msg), query)` (4.5.0, ISS-2025-0671).
- `consultWithDiagnostics(program, filename)` — per-clause compile collecting `CompilationError`s
  with line numbers (returns `CompilationResult`) instead of throwing on the first error.
- `compileFile("x.pl")` / `consultSmart("x.pl")` — compile to `.jpc` / auto-cached consult.
- `getPredicateIndicatorAtLine(line)` — maps a source line to the clause's predicate indicator
  (IDE gutter breakpoints).
- `getEngineContext()` — the durable `core.engine.EngineContext`: where the IDE installs its
  `DebugController` and where the running machine publishes the query's `ResourceGuard`.
  (It replaced `getQuerySolver()` in 4.0.0; `solveLegacy` is gone.)
- `getEngineState()`, `getStreams()`, `getOps()`, `getFlags()`, `residualGoals(solution)` (CLP(FD)
  residual constraints in SWI's printed forms since 4.6 Q5), `currentAnswerDelays()` (the WFS
  delay list of the current answer, 4.6 Q4), `runMain()` (`initialization(G, main)` for embedders:
  returns the exit code, never halts, 4.6 Q3).
- `enableSafeMode()` / `enableSafeMode(SafeModeOptions)` / `setInferenceBudget(steps)` — see
  hardening below.

### Sandbox & Resource Limits

- **`Prolog.enableSafeMode()`** / **`enableSafeMode(SafeModeOptions)`** — per-instance,
  **irreversible** deny-list sandbox over BOTH the registry and the native `BuiltinTable`: every
  registry built-in whose class lives in
  `builtin.{os,ffi,filesystem,network,http,jdbc,persistence,threading}` (`UNSAFE_BUILTIN_PACKAGES`)
  plus every name in `UNSAFE_PREDICATE_NAMES` (`open`, `see`/`tell`, `csv_*_file`, `log_to_file`,
  the loaders `consult`/`ensure_loaded`/`load_files`/`make`, `absolute_file_name`, `exists_file`,
  `shell`, `getenv`, …), and `halt/0,1` become `permission_error(call, sandboxed, halt)`
  (ISS-2025-0672). `SafeModeOptions.allowFileRead(dir)` keeps `open/3,4` (read mode) and the
  loaders for whitelisted directories; `allowHalt()` keeps `halt` (the CLI's `--safe` uses it).
  Logging is per engine. `src/test/resources/safe-mode-allowlist.txt` is a snapshot of what
  survives (`registry` names, `native` indicators and `library` prelude exports): **a new built-in
  or prelude export fails `EngineV45HardeningTest` until it is classified** — put a host-touching
  one in a denied package or the name list, a pure one in the allowlist.
- **`Prolog.setInferenceBudget(steps)`** (0 = unlimited) — aborts a runaway query with
  `InferenceLimitException`, enforced through `core.engine.ResourceGuard` inside meta-call
  sub-solves and worker threads too. Since 4.5.0 (ISS-2025-0624) it is ONE shared pool per query
  (an `AtomicLong`; each guard draws credit in chunks of 1 024), a worker runs on
  `parentGuard.child()`, and natives that do O(N) work charge the guard per element
  (`ResourceGuard.charge(n)`: `length`, `append`, `member`, `nth`, `msort`, `copy_term`, `findall`
  copying, `atom_codes`, …). Since 4.6 Q6 (ISS-2025-0786) a BRIDGED call is charged too
  (`LegacyBuiltinAdapter`: one step per 64 characters of text input and one per solution;
  `ResourceGuard.guarded(CharSequence)` meters `java.util.regex`). (The javadoc claims it raises
  `resource_error(inference_limit_exceeded)` — wrong; trust the implementation and
  `ProductionAuditTest`.)
- **Error-trust model** — deliberate asymmetry: deep-term/deep-input `StackOverflowError`s from a
  bridged built-in are converted to a `PrologException` carrying ISO
  `resource_error('stack_overflow')` / `resource_error('parser_nesting')` so the Java embedder sees
  a controlled error, while `InferenceLimitException` (budget), `QueryCancelledException`
  (IDE/embedder Stop, via thread interrupt), `DebugStopException` (debugger Stop) and
  `core.engine.ThreadExitException` (`thread_exit/1`, 4.5.0) are plain `RuntimeException`s —
  (a worker's join status for a budget stop is the TERM `exception(error(resource_error(
  inference_limit), _))` since 4.6 Q1: the join is outside the stopped query) —
  **not** `PrologException` — so untrusted `catch/3` cannot swallow them. `halt/0,1` reach the
  embedder as a `PrologException` with `isHalt()`; the engine never calls `System.exit`.
  `core.engine.ControlFlow.rethrowIfControl(t)` must be the first statement of any broad
  `catch (Exception/RuntimeException)`. Preserve this distinction when adding limits.

### Engine v4

The clean-room core designed in `docs/reports/report-engine-v4-design-2026-08-25.md`. (4.5.0 —
the production-readiness waves P1..P7, ISS-2025-0514..0675 — is recorded in
`docs/reports/report-production-readiness-2026-09-23.md` §10–§16, and 4.6.0 — the completeness
waves Q1..Q7, ISS-2025-0680..0799 — in `docs/reports/report-completeness-4.6-2026-09-23.md`
§10–§16; neither is in the progress report except its invariants.)
All nine waves are done (v3.9.0: ISS-2025-0438..0449; v3.10.0: 0450..0456; v3.11.0: 0457..0462;
v3.12.0: 0463..0465; v3.13.0: 0466..0471; v3.14.0: 0472..0477; **v4.0.0: 0478..0488**), and so are
the two 4.1 waves (**v4.1.0**, one engine: 0491..0495; **v4.2.0**, the L-08 built-in migration:
0496..0501), **v4.3.0** (4.2 wave C — indexing on every selection path, `op/3` and
`char_conversion/2` native, `char_type/2`/`code_type/2` generators: 0500, 0502, 0503) and
**v4.4.0** (4.3 wave D — ISO error conformance across the natives, `retractall/1` through the
index, `bounded = false`, the cleanup catch escape: 0504..0513). Progress, the 68 invariants, the benchmarks and what remains
live in `docs/reports/report-engine-v4-progress.md` — **read it before touching `core.engine.v4`**;
sections 9–19 are the wave records; §3 invariants 12–15 were added by 4.5.0 (activation-time body
cells, call sites and the dispatch stamp, the loader/reader) and 16–18 by 4.6 Q6 (trail tidying,
run-time call sites, native iterations on the goal stack — see the rules below).

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
  listing, the global variables, the ISO flags, halt, findall/4); and, since 4.3.0,
  **`NativeChars`** (`char_type/2` and `code_type/2` as generators, with the SWI parametric forms)
  plus `op/3`, `char_conversion/2` and `current_char_conversion/2` in `NativeMisc`; and, since
  4.5.0, **`NativeRead`** (`read/1,2`, `read_term/2,3`, `read_term_from_atom/3`, `open_string/2`,
  `with_input_from/2`, `read_line_to_string/2`, `read_line_to_codes/2,3`, `read_string/3,5` — the
  clause collector + v2 reader with fresh variable cells) and **`NativeExpand`**
  (`dcg_translate_rule/2`, `expand_term/2`, the consult-time `term_expansion/2`), plus
  `print_message/2`, `statistics/0,2`, `predicate_property/2`, `string_upper/lower`, the SWI
  `aggregate_all/3` forms and every CLP(FD) predicate (`ClpfdNative`, lazy labeling generators).
  And, since 4.6.0: **`NativeSequences`** (`limit/2`, `offset/2`, `order_by/2`, `distinct/1,2`,
  `call_nth/2` over `Machine.pushFiltered`, Q2), **`NativeApply`** (maplist/2..7, foldl/4..7,
  include/3, exclude/3, partition/4,5 run natively over the tagged first clause of each apply
  library predicate, Q6), **`NativeStreams`** (22 stream indicators: `open/3,4`, `close/1,2`,
  `stream_property/2`, `current_stream/3`, `set_stream/2`, `seek/4`, the positions, the byte I/O,
  `portray_clause/1,2`, Q6), **`NativeRecords`** (`recorda/z`, `recorded/2,3`, `erase/1`,
  `current_key/1`, `flag/3` over the per-engine `Engine.records()` store, Q7), `ColumnPrintStream`
  (the column tracker behind `format/2,3` column stops and `line_position/2`, Q2), plus `tnot/1`,
  `undefined/0`, `call_delays/2` (in `Tabling`, Q4), `multifile/1`, `discontiguous/1` (in
  `NativeDb`, Q3) and `enhanced_phrase/2,3` (= `phrase/2,3`, Q7).
  The loaders (`consult/1`, `[F]`, `ensure_loaded/1`, `load_files/1,2`, `make/0`, `use_module/1,2`)
  are the registry built-in `builtin.filesystem.LoadFiles` (safe-mode denied); since 4.6 Q3 a load
  holds a PER-FILE `core.engine.LoadLock` (re-entrant for its owner; text loads share one key),
  with a per-thread load stack and a waits-for walk (`core.engine.ThreadWaits`: file locks,
  `thread_join`, `concurrent_*` awaits, message and mutex waits) that turns a wait that can never
  end into `permission_error(load, source_sink, File)`. File specs (`library(X)`, `Alias(Path)`)
  resolve through `core.engine.FileSearch` (`file_search_path/2`, then the `library`/`swi`/
  `foreign` defaults) and `absolute_file_name/2,3` is `builtin.filesystem.AbsoluteFileName`.
- `Modules` — the module owner (system/user/library, resolution order, imports, meta_predicate);
  `Prelude` — indexes and autoloads `prelude/*.pl`.
- `Workers` — one `Machine` per thread over the same `Engine`.
- `LegacyBuiltinAdapter` + `SolverFacade` — the remaining registry built-ins: **237** names can
  still reach the adapter (4.6.0; of the registry's 443: 151 are shadowed by a native of the same
  name, 37 are control constructs/inline built-ins, 11 are prelude exports, 7 the machine handles
  itself — `assert*`, `retract`, `repeat`, the cleanup constructs, `'.'`). What is left is the
  extended libraries (incl. the loaders and the thread API), `table/1` and the debug/profiler
  predicates; see LIM-037.
- `Engine` — the per-`Prolog` context; `Errors` — ISO error construction (since 4.6 Q1 also the
  LIBRARY builders `Errors.type(type, culprit, name, arity, message)` & co. that produce
  `error(Formal, context(Name/Arity, Message))`, and `Errors.host(e, op, kind, culprit, name,
  arity)` mapping a host failure to `existence_error`/`permission_error`/`io_error`/
  `system_error`; argument checks for registry code are `builtin.LibArgs`).
- `EngineState` (the thread-current per-engine state), `Streams` + `PrologStream`, `Ops`, `Writer`,
  `Answer` — the W7 services the bridged built-ins reach through their static facades.
- `Undo` — **package-private since 4.3.0** (ISS-2025-0500): the doorway that finds the machine
  running on the calling thread and pushes a backtrackable undo action onto its trail. It replaced
  the static `core.engine.Trail` in 4.1.0; there is ONE trail now. Every built-in that used it is
  native and pushes with `Machine.pushUndo` directly; the one client outside the engine,
  `builtin.clpfd.v2.ClpfdV2Bridge`, declares its own `UndoSink` interface, which
  `core.engine.v4.ClpfdNative`'s static initialiser points at `Undo.record`. **Do not make it
  public again** — a construct that needs the trail belongs inside the machine, or behind a sink
  the engine installs.

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
The 4.5.0 program added its own decisions (SWI as the reference): `op/3` is permanent on
backtracking, `integer/1` rounds, negative shifts shift the other way, `intersection/union` keep
duplicates, `call((fail,1))` and `string_concat(-,-,-)` raise, `M:G` runs non-exported `G`,
`print/1` quotes, globals are per thread, directives run once, tabled non-stratified negation
raises (LIM-046), consulted predicates stay modifiable. The 4.6 program added (ref-deviations
§4a–§4e): `'|'` at 1105 and `as` at 700; a second file ADDS clauses to a non-multifile predicate;
tabling across threads evaluates instead of waiting; a per-engine working directory; minimal WFS;
residual forms from SWI's `clpfd.pl`; every non-worker thread is `main`. **All of them — and every other deliberate
deviation — are listed in `docs/references/ref-deviations.md`; a change of pinned behaviour
updates that file and the pinning test, it never adds a second test.**

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
  survives and defeats the trust-me pop. Decide it BEFORE the unification that binds the caller's
  pattern (a look-ahead over the remaining candidates reads the pattern unbound — `NativeRecords`
  and `current_op/3` pre-filter the candidates by the bound arguments, ISS-2025-0775/0791).
- **The trail is tidied on every deterministic frame pop** (invariant 16, ISS-2025-0787):
  `Bindings.tidy` drops the entries of variables newer than the new top choice point. So a
  `Bindings` mark held ACROSS the execution of goals is only stable if the holder either keeps
  `forceTrail` raised for the whole extent (findall, `runOnce`, `runSubQuery`, `\=`, the catcher
  match) or owns a choice point below the mark that outlives the extent (predsort's fail frame).
- **A native that runs user goals pushes them on the goal stack** (invariant 18): the user goal
  followed by a `Machine.Step` (a goal action that may fail), never a nested drive; a Step that
  emits ports claims them (`claimNativePorts`). `NativeApply` reproduces the apply library's
  two-clause CLAUSES frame whenever something is traced or both alternatives remain, so traces are
  byte-identical to the Prolog version's. **Run-time goals have call sites** (invariant 17): a new
  dispatch table must feed `Engine.dispatchStamp()` or the `rtLookup` cache goes stale.
- **A filter over a goal's solutions runs in the caller's continuation** (4.6 Q2):
  `Machine.pushFiltered(goal, filter)` — the filter sees each solution with its bindings live and
  answers ACCEPT / REJECT (backtrack into the goal) / ACCEPT_LAST, which cuts the goal's choice
  points exactly as `!` would (cleanups run now, their errors propagate). `cutQuietly` is for
  ABANDONED runs only. And the clause look-ahead (`nextMayMatch`/`argClash`, a per-argument
  principal-functor clash) runs on every activation, traced or not (ISS-2025-0715): it is what
  makes `foldl(_, [], A, A)` vs `foldl(G, [X|Xs], ...)` deterministic; keep it allocation-free.
- **The prelude is the LAST resort, never the first**: `selectClauses` looks at
  `ClauseStore.libraryClauses` only when the KB-backed `Predicate` is empty, and `stepN` routes a
  library indicator to `callUser` *before* the legacy adapter. Both halves are needed. An EMPTY
  first-argument selection of a module predicate means "no clause matches" (fail), never
  "undefined" (existence_error) — ISS-2025-0770. `Prelude.header` reads a module's export list as
  TEXT (comments are blanked first since ISS-2025-0792): keep exports plain `Name/Arity` terms and
  add a new prelude file to `Prelude.RESOURCES`.
- **A term must be COPIED before it crosses a machine boundary** — bindings live in the cells.
  `Workers` copies the goal in and every answer out; the message queues copy on send and receive.
- **A worker gets its OWN `ResourceGuard`, a `child()` of the parent's** — it draws from the
  query's ONE budget pool (ISS-2025-0624); never construct a worker guard with the parent's limit
  (that multiplied the budget per thread).
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
- **Table SPACES** (4.6 Q4, ISS-2025-0752): `engine.tabling()` is the CALLING THREAD's space —
  the main space for every non-worker thread, a private one per worker machine
  (`Tabling.enterWorker`); only a COMPLETE table of an `as shared` predicate crosses threads, an
  incomplete one never does, and a thread never waits for another's evaluation (it evaluates
  privately). Look the space up once per machine (`Machine.tablingHere()`), not per call. Moded
  arguments are evaluated FREE (`Machine.freeModedArguments`) and aggregated per SWI's
  `boot/tabling.pl`; a mode-directed answer list has holes (invariant 14).
- **The WFS delay list is trailed machine state** (4.6 Q4, ISS-2025-0755): `Machine.setDelays`
  (undone on backtracking), reset at each PRODUCE clause; `tnot/1` of an incomplete table succeeds
  with `tnot(G)` delayed; conditional answers keep their alternative delay lists and
  `Tabling.simplify` resolves them in `completeScc`. `\+` over an incomplete table still raises.
- **A thread signal runs on the target's own goal stack, never asynchronously** (4.6 Q4,
  ISS-2025-0749): the drive loop polls `ThreadSignals.PENDING` (one volatile read per step) and
  pushes every queued signal as `ignore(\+ \+ G)`; no new signal is taken while signal goals run
  (`sigEnd`); a blocking thread built-in waits in 100 ms slices and runs signals in place
  (`ThreadSignals.enterHandler`). **Every non-worker thread is `main`** (the CLI, IDE background
  solves, embedder threads): one signal box and one table space shared by all of them (a
  documented deviation, ref-deviations §4e).
- **Library modules are copy-on-write** (4.6 Q4, ISS-2025-0747): `Modules.Mod` collections are
  published through volatile fields, `loaded` written last under the module's monitor, `mods` a
  `ConcurrentHashMap`; the user-module mirror is incremental (per-module structural version,
  append-only compile, ISS-2025-0784) — never rebuild every module on a stamp change.
- **Two identity rules**: a named `Variable` `copy()`s to **itself** (ten built-ins rely on the
  alias), and a clause skeleton numbers its variables **by name** (JpcReader and the legacy parser
  allocate one object per occurrence). Query terms are normalised to one cell per name.
- **The writer must stay iterative and cycle-safe** — it is what prints a rational tree.
- **A stream argument is a term, not a string** — resolve it with `IOStreamUtils.inputStream` /
  `outputStream` / `StreamManager.stream(Term)`.
- **A relative file name resolves against the ENGINE's working directory** (4.6 Q4,
  ISS-2025-0745): `EngineState.file(name)` / `EngineState.path(name)`; never `new File(rel)`, never
  read or write `user.dir` (several engines share one JVM).
- **A consulted clause has an owner** (4.6 Q3): `Rule.sourceFile` is set from the load context;
  anything that adds clauses during a load must set it, or a reconsult cannot remove them. A clause
  `M:H` for another module is stored as an `M:H` clause of the flat store (module created on
  demand); `user:H` goes to `user`.
- **Anything a built-in prints goes through `StreamManager.out()`** — `with_output_to/2` captures
  through the thread-local override alone, so a `System.out.print` escapes it. That includes
  `Prolog.listing()` (ISS-2025-0499).
- **A native must NOT `resolve` the term it is about to print, walk or unify.** `Writer`, `Unify`
  and every `NativeLibrary` walker deref as they go; `m.resolve(t)` is a full copy and is what made
  the bridge slow. Use it only for the small things — a stream argument, an option list, an error
  culprit.
- **First-argument indexing is on EVERY clause-selection path, and a miss must never drop a
  clause.** Design B.7: one lazy hash per predicate from the first argument's key to its clauses,
  merged with the variable-headed clauses **in source order**, maintained incrementally by
  `assert`/`retract` and rebuilt on compaction and on a re-sync from the KB. Three rules:
  - `Clause.argKey(goalArg)` returns **null** for a variable or anything unindexable, and
    `ClauseStore.Predicate.select(null)` is the FULL clause list. That is what makes an index miss
    degrade instead of dropping clauses — the ISS-2025-0340 hazard that made the old engine revert
    indexing. Any new key kind must keep it.
  - The key is **type-faithful and allocation-light**: an atom's own name `String`, a boxed
    `Long`/`Double`/`BigInteger`, or a small non-escaping `Clause.FunctorKey`/`Clause.StringKey`.
    `1`, `1.0`, `'1'` and `"1"` are four buckets. Never go back to building a `String` per call
    (`"i" + n.bigIntegerValue()` cost `loop(1000000)` about 16%, ISS-2025-0502), and never let two
    kinds of key compare equal unless you mean the over-approximation.
  - A **new selection site** must call `p.view(Clause.argKey1(goal), view)` (4.5.0: a
    `ClauseStore.View` window over the gap buffer — `a`, `from`, `to` — instead of the copied array
    `p.select(...)` returned; a module predicate still uses `select`), not `p.all()`. The sites are
    `Machine.callSite`/`selectClauses` (calls), `Machine.retractClause`, the store-side
    `retractall/1` (ISS-2025-0545) and `NativeLibrary.ClauseB` (`clause/2`); `p.all()` survives
    only where the whole predicate really is wanted (`Machine.hasQualifiedHook`). On the KB side
    `KnowledgeBase.retractAllClauses` (the Java-API path) is one pass over the predicate's own
    `RuleSeq` plus one compaction (ISS-2025-0544); `getRulesWithFirstArgIndex` is an O(n) filter
    with the same over-approximating key rule (no v4 path uses it).
    `EngineV4IndexingTest` pins the property with a randomised equivalence against an independent
    "could the first arguments unify?" oracle, plus the partial-structure key (`f(g(X), _)` keys on
    `g/1`) on all four paths.
- **A built-in raises `error(Formal, Context)`, never a message atom** (invariant 64,
  ISS-2025-0504..0510). Build it with `core.engine.v4.Errors` —
  `instantiation`/`type`/`domain`/`existence`/`permission`/`representation`/`resource`/
  `evaluation`/`syntax`, plus `Errors.pi(name, arity)` for the `Name/Arity` culprit ISO asks for.
  **Never** `new PrologEvaluationException("some sentence")` and never a formal built as a Java
  string: both produce an error term that is a bare ATOM, which `catch(G, error(E, _), R)` cannot
  match, so only a bare-variable catcher sees it. And an argument fault **raises**, it does not
  fail silently (invariant 65). `EngineV4IsoErrorsTest` is the net — a 249-row
  `Goal -> expected error term` table asserted on the whole `error(Formal, _)` shape, with the
  deliberate deviations listed in its class comment; a new built-in with an argument contract
  belongs in it (298 rows in 4.6.0). Since 4.6 Q1 the bridged extended libraries follow the rule
  too (LIM-038 closed): `Errors.type(type, culprit, name, arity, message)` & co. give
  `error(Formal, context(Name/Arity, Message))`, `builtin.LibArgs` checks the arguments, and
  `ExtendedLibraryErrorsTest` fails on any message atom, arity-guard message or timeout over
  every registered name (and, since Q7, on any FFI goal that fails on a bad argument).
- **A goal argument is checked before anything runs** (invariant 66). `setup_call_cleanup/3`
  validates Setup, Goal and Cleanup up front, so an argument fault is raised inside the enclosing
  catch scope (ISS-2025-0509).
- **A cleanup runs at the moment its frame is popped, never after the search that popped it**
  (invariant 68, ISS-2025-0513). `Machine.handleBall` used to collect the CLEANUP frames it unwound
  past and run them after the matching CATCH frame had been popped and its recovery installed — and
  `handleBall` is called from inside `drive`'s `catch` clause, so a `PrologException` the cleanup
  threw was outside the loop that routes exceptions and escaped to the Java embedder. It now runs
  each cleanup at pop time (as `backtrack` always has) and lets the cleanup's ball REPLACE the one
  being unwound, continuing the search from the same position; when nothing catches the replacement
  it throws it itself, because `drive` rethrows the ORIGINAL exception object when `handleBall`
  answers false. Any new construct that runs user code while a ball is unwinding must do the same.
- **Protection is a question about three stores, not one.** `Machine.isProtectedProcedure` asks
  `BuiltInRegistry.isBuiltIn`, `BuiltinTable.isNativeKey` and `Modules.isLibraryIndicatorKey`;
  `checkModifiable` (assert/retract), `NativeLibrary.ClauseB` (`clause/2`) and
  `NativeDb.checkModifiable` (`retractall`/`abolish`) all go through it. Consult does NOT — it
  checks the registry only (`Prolog.checkBuiltInConflict`), which is what keeps the documented
  library-override rule (a module may define its own `partition/4`) working.
- **A CLP(FD) `Constraint` subclass must render itself** (4.6 Q5, `builtin.clpfd.v2.Residuals`):
  implement `render(Residuals, out)` (its residual goal in SWI's printed form) and, when it can
  become entailed before all its variables are fixed, `alive(ClpStore)` (a dead constraint is not
  printed); a constraint that stands for another source form sets `form = FORM_NOT`, and one that
  is part of another constraint's residual (circuit/1's inner all_distinct) `FORM_HIDDEN`.
  `Coroutining.residualGoals` and `copy_term/3` share one rendering state per call. The CLP(FD)
  bridge tracks changed variables (`ClpStore` change marks, ISS-2025-0781): never go back to
  rescanning every FD cell after a post or a labeling step.

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
`NativeTerm.register` / `NativeDb.register` (and `NativeRead`, `NativeExpand`, `NativeChars`,
`NativeSequences`, `NativeStreams`, `NativeRecords`, … — `NativeBuiltins.register` calls them all).
A native sees dereferenced `Term[] args` and the `Machine`; it never builds a `Map<String,Term>`.
**264 indicators (220 names) are native** (4.6.0), and ~40 more are handled inline by the machine.
A native that needs per-engine state keeps it on `Engine` (e.g. `Engine.records()`), shared by the
engine's threads and synchronised.

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

**2. The legacy registry SPI** (`core.engine`), which the 237 remaining built-ins use — the
extended libraries (jdbc, filesystem incl. the loaders and `absolute_file_name`, threading incl.
signals and pools, crypto, ffi, graph, network, persistence, os, http, datetime, json, logging,
regex, dcg, csv, xml), `table/1` and the debug and profiler predicates (LIM-037; the stream half of
`io` is native since 4.6 Q6):
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
what makes `assertz`/`retract`/`clause/2` raise `permission_error` on a built-in. Since 4.6 Q1
(ISS-2025-0685) the arity sets are EXACT: every name `new Prolog()` registers declares its arities
(`putArity`; pinned by `testISS0685_EveryRegisteredNameDeclaresItsArities`), and a call at another
arity falls through to `existence_error(procedure, Name/Arity)`. An embedder's
`registerBuiltIn(name, b)` without arities keeps the historical any-arity answer;
`registerBuiltIn(name, b, arities...)` is exact. That is why the
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
3. Update `docs/references/BUILTIN_PREDICATES_REFERENCE.md` (a `###` heading naming the
   indicator — `DocumentedPredicatesTest` checks every heading against the live engine) and, if it
   has no entry there, `tools/manual/supplement.md`; run `tools/build-manual.sh` (it reads the
   version from pom.xml). Classify it in `safe-mode-allowlist.txt`.
4. Run `mvn test` and `./test_all_examples.sh`.

**Adding a legacy built-in** (only when the eager registry contract is genuinely the right fit):
1. Create the class in `it.denzosoft.jprolog.builtin.<category>/` implementing `BuiltIn` or
   `BuiltInWithContext`. If it touches the host (processes, files, network, JVM, threads) it must
   live in one of the safe-mode denied packages.
2. Register it in `BuiltInFactory.FACTORY_MAP` AND declare its exact arities in `BuiltInRegistry`
   (`putArity`) — an undeclared name fails `ExtendedLibraryErrorsTest`. Check arguments with
   `builtin.LibArgs` / the `Errors` library builders; map host failures with `Errors.host`.
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
  `NeedsSolverContextException`, and since 4.6 `LoadLock` (per-file load lock), `ThreadWaits` (the
  waits-for graph of the load-cycle check), `ThreadSignals` (per-thread signal boxes),
  `FileSearch` (`file_search_path/2` resolution)
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
  `lists.pl` (member/2, append/2,3, nextto/3, max/min_member/2,3, list_to_set/2, permutation/2,
  …), `apply.pl` (maplist/foldl/include/exclude/partition plus their `meta_predicate`
  declarations — run natively by `NativeApply`), `pairs.pl`, `coroutining.pl`
  (freeze/frozen/when/dif/?= and the `'$attr_hook'/4` dispatcher), `clpfd.pl` (the decomposed
  globals: chain/2, lex_chain/1, disjoint2/1, automaton/3,8, …) and `ordsets.pl` (4.6 Q7). The
  `KnowledgeBase` never sees them, so neither `listing/1` nor the IDE does. Maven picks the
  directory up by default.
- `PrologCLI` — `jprolog [options] [file.pl ...]` (also `java -jar target/jprolog.jar`): consults
  the files, `-g Goal` runs a goal once, `-t Goal` replaces the toplevel, `--safe`
  (`enableSafeMode(allowHalt)`), `--budget N`, `--max-solutions N`, `--demo` (the demo facts are
  NOT loaded otherwise), `--batch`, `--interactive`, `-q`, `-h`. Exit status: `halt(N)` → N, a
  failing/raising `-g` → 1, a bad option → 2; `initialization(G, main)` halts after `G`.
  `PrologCLI(args, in, out, err).run()` never calls `System.exit` (tests use it; only `main`
  exits). Answers are **streamed** through `solveStream(String, AnswerSink)`: interactively one
  answer is computed per `;`, and with a non-interactive stdin (`System.console() == null`, or
  `--batch` / `-q`) each answer is printed as it is found, separated by ` ;` and ended by `.` (or
  `;` + `false.` when a choice point was left); an answer prints its CLP(FD) residual constraints
  and, for a conditional WFS answer, `undefined` (4.6). Commands: `:consult`/`:c`, `:compile`/`:cc` (to
  `.jpc`), `:listing`/`:l`, `:save`/`:s` (writes the SWI-layout listing), `:clear`,
  `:trace [on|off]`, `:help`/`:h`, `:quit`/`:q`. Uncaught errors print through the ISO writer.

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
- **A deterministic frame emits no phantom `Fail` after its `Exit`**, and a failure inside a
  predicate's body prints that predicate's `Fail` (4.5.0, ISS-2025-0668): a traced CLAUSES frame
  (and the `iteTraced` port frame) is NOT trust-me popped when its last alternative is handed out
  — it stays until its Exit port, where `popIfDeterministicTop` drops it if nothing is above it;
  a traced frame also looks ahead (`anyMayMatch`, a per-argument key clash checked before the head
  binds) so its last matching clause makes it deterministic. The frames kept are exactly the OPEN
  calls, so trace memory stays linear. Untraced execution is untouched (all of it is behind
  `traceGoal != null`). Ports print `'$mctx'(user, G)` as `G` (`portView`). A kept traced frame
  must not keep its bindings alive: the trail is tidied at the traced Exit pop too (invariant 16,
  ISS-2025-0787 — a debugged 20 000-level recursion ran out of memory), and the debugger's call
  stack is an immutable linked stack (an O(1) snapshot per port, ~1.4 µs per port with a listener).
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
- **Immutable terms** except `Variable`; the engine binds in the cell with a trail. There is ONE
  trail: a native pushes an undo action with `Machine.pushUndo`, and the single non-built-in client
  outside the engine package (the CLP(FD) bridge) reaches it through a sink the engine installs —
  `core.engine.v4.Undo` itself is package-private since 4.3.0.
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
highest used one (**ISS-2025-0799** as of 4.6.0; the 4.5.0 and 4.6.0 programs allocated ranges
per wave, so 0530–0539, 0554–0559, 0580–0589, 0614–0619, 0653–0659, 0676–0679, 0700–0709,
0719–0729, 0740–0744, 0756–0759, 0771–0774 and 0788–0789 are unused) — track-issues.md lags
behind recent releases. The next free limitation is **LIM-048**.
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
`EngineV41RetirementTest` (21, 4.1 wave A), `EngineV4IoTest` (19), `EngineV4TextTest` (16),
`EngineV4TermTest` (13), `EngineV4DatabaseTest` (17) — 4.1 wave B — `EngineV4IndexingTest`
(14), `EngineV4OpsTest` (16), `EngineV4CharTypeTest` (14) — 4.2 wave C — and
**`EngineV4IsoErrorsTest` (1 method, now 298 table rows) and `EngineV4CleanupTest` (7) — 4.3 wave
D**, plus `test/cli/PrologCliBatchTest` (6). The 4.5.0 program added one class per wave:
`EngineV45SemanticsTest` (16, P1), `EngineV45PerformanceTest` (17, P2 — counters: call-site hits,
views, merged slots, allocation bounds), `EngineV45LoadReadWriteTest` (21, P3),
`EngineV45ConformanceTest` (23) + `EngineV4FormatTest` (85-row directive table) (P4),
`builtin/clpfd/v2/ClpfdV45Test` (19, P5), `EngineV45HardeningTest` (17, P6 — incl. the safe-mode
allowlist snapshot `src/test/resources/safe-mode-allowlist.txt`), `test/cli/PrologCliToplevelTest`
(9, P6/P7), `EngineV45ReleaseTest` (6, P7), `test/performance/PerformanceRegressionTest` (10, P7 —
N-vs-4N growth tests with warm-up and a 10x bound, never an absolute time) and the real programs
of `test/integration/FamousPrologProgramsTest` (9, 13 since 4.6 Q7 — solution_sequences paging,
a `file_search_path` library load, a tabled shortest path with `min`, the CLP(FD) 6×6 knight's
tour). The 4.6.0 program added: `ExtendedLibraryErrorsTest` (5, Q1 — the probe harness over every
registered name; it writes `target/extended-library-errors.txt`), `EngineV46StandardPredicatesTest`
(13) + `DocumentedPredicatesTest` (2, Q2 — every `###` heading of the reference and the supplement
must name an existing predicate or evaluable), `EngineV46LoaderTest` (17, Q3),
`EngineV46ThreadsTablingTest` (15, Q4), `builtin/clpfd/v2/ClpfdV46Test` (15, Q5),
`EngineV46PerformanceTest` (15, Q6 — counter hooks: `rtSiteHits`, `applyLevels`,
`examinedCells`, `mirrorCompiles`, …) and `EngineV46ResidueTest` (9, Q7). Interrupt/Stop tests synchronise on
`test/support/QueryStartLatch` (the query writes `go` into a thread-local stream), never on a
`sleep`. A wave-B class opens with a `test*IsNative` method that fails
the moment one of the migrated indicators is not in the `BuiltinTable`; the rest pin the modes and
the ISO error terms so a migration cannot quietly change one.
- Add a `@Test` method named after the issue and fix (e.g. `testISS0188_ModNegativeDivisor`)
- The test must fail without the fix and pass with it
- Use `prolog.solve()` for query-level assertions, direct Java assertions for internal fixes
- Assert VALUES, not existence: `==` inside the query (`X == [a,b]`), exact answer counts, and the
  whole `error(Formal, _)` term — never `!isEmpty()`, `>= N`, `toString().contains(...)` or a
  `catch (RuntimeException e) { /* acceptable */ }` (4.5.0 P7 removed ~80 such assertions).
- Every wave must keep the full suite green (4.6.0 baseline: **1547/1547**). There is one engine
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
