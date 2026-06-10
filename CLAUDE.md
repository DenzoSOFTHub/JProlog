# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

JProlog is a Prolog interpreter written in Java (1.8). It includes a core engine, 200+ built-in predicates, a Swing-based IDE, and a CLI. No external dependencies beyond JUnit 4 for tests. The clean-room **v2** rewrites are the defaults for parser, CLP(FD), DCG, and (since v3.1.0) the resolution engine — see "v2 Subsystems" below. v3.4.0 added embedder-facing hardening: a sandbox (`enableSafeMode`) and an inference budget.

**Repository**: https://github.com/DenzoSOFTHub/JProlog
**Current version**: check `<version>` in pom.xml. README.md version/metrics lag behind — trust pom.xml and CHANGELOG.md.

## Build & Run

```bash
mvn compile                  # Build
mvn test                     # JUnit tests (full suite; v3.4.0 baseline: 705/705)
mvn test -Dtest=BugFixVerificationTest                  # Single test class
mvn test -Dtest=BugFixVerificationTest#testISS0188_ModNegativeDivisor   # Single method
mvn clean compile            # Clean rebuild

# Run CLI
java -cp target/classes it.denzosoft.jprolog.PrologCLI

# Run IDE (or: mvn exec:java@run-ide)
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE

# Test core example programs test_01..test_20 (REQUIRED after any code change)
./test_all_examples.sh
# Success rate >= 75% for maintenance, >= 85% for new features

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
- Bare `mvn exec:java` fails — the pom's default mainClass `it.denzosoft.jprolog.Main` does not exist. Use `mvn exec:java@run-ide` or the `java -cp` commands above. `run_ide.sh` is broken (points at a nonexistent `gui.PrologIDE`); `start-ide.sh` works but skips compilation.
- `test_all_examples.sh` enforces nothing: "PASSED" only means the CLI exited 0 within a 30s timeout, and the script always exits 0. The 75%/85% thresholds are manual policy — eyeball the per-program output and "Successful queries" counts.
- No surefire plugin is pinned in pom.xml, so the Maven-default surefire (2.12.4) runs the tests: new test classes must match `Test*` / `*Test` / `*TestCase` or they are silently skipped — note `*Tests` (plural) is NOT matched by this old surefire.
- Every `new Prolog()` logs ~13 `WARNING: Overriding existing built-in predicate` lines (v2 CLP(FD) re-registering `#=`, `label`, …). Expected noise, not a bug.

## Architecture

### Query Execution Flow

1. **Parser** — by default the clean-room single-pass v2 parser (`core.parser.v2.Lexer` + `core.parser.v2.TermReader`, operator-precedence/Pratt) converts Prolog text to `Term` objects. The legacy `core.parser.Parser` is the fallback (`-Djprolog.parser=legacy`).
2. **KnowledgeBase** (`core.engine.KnowledgeBase`) stores facts/rules as `Rule` objects. Each `Rule` carries a `sourceLine` used for line-accurate breakpoints — but it is populated only by `consultWithDiagnostics` on the v2-parser path; plain `consult()` leaves it -1, so line breakpoints silently fail for code loaded that way.
3. **Resolution** — the default engine is `core.engine.v2.MachineSolver` (iterative SLD with explicit goal stack/choice points). `Prolog` constructs a **fresh MachineSolver per query** over the live KnowledgeBase/BuiltInRegistry, so the solver instance holds no cross-query state; durable state (DebugController, flags) lives on the shared legacy `QuerySolver`, which MachineSolver receives as context. The legacy recursive `QuerySolver` remains as fallback (`-Djprolog.engine=legacy`, or per-call via `Prolog.solveLegacy(query)`).
4. For each goal, the solver checks `BuiltInRegistry` first, then falls back to user-defined rules in the knowledge base.
5. **Unification** happens via `Term.unify(Term, Map<String, Term> substitution)` — Robinson algorithm (the v2 machine uses its own binding/trail internally).
6. **Cut** in the legacy engine is managed through `CutStatus`/`MutableCutStatus` flags passed through recursion; the v2 machine uses choice-point barriers.

### v2 Subsystems

Clean-room rewrites, selectable via system properties (and `Prolog.setUseV2*()` static toggles):

| Subsystem | v2 package | Default | Fallback |
|---|---|---|---|
| Parser | `core.parser.v2` (`Lexer` + `TermReader`) | **v2** | `-Djprolog.parser=legacy` |
| CLP(FD) | `builtin.clpfd.v2` (`ClpStore`/`IntervalDomain`/`Constraint`/`Labeler`) | **v2** | `-Djprolog.clpfd=legacy` |
| DCG | `core.dcg.v2.DCGTranslator` | **v2** | `-Djprolog.dcg=legacy` |
| Resolution engine | `core.engine.v2.MachineSolver` | **v2** (since 3.1.0) | `-Djprolog.engine=legacy` |
| Arithmetic | `core.arith.v2.ArithEvaluator` | used by the v2 engine | legacy engine uses `core.engine.ArithmeticEvaluator` |
| Term writer | `core.write.v2.TermWriter` | IDE source formatter only | `write/1` family uses `core.util.TermFormatter` |

Toggle semantics (all four properties live in `Prolog.java`):
- Only the literal value `legacy` (case-insensitive) selects the fallback — `-Djprolog.engine=v1` or `=off` still means v2.
- Parser/DCG/engine flags are re-read on every call, so `Prolog.setUseV2Parser/Dcg/Engine()` work at runtime. The **CLP(FD) flag is read only in the `Prolog` constructor** — set it before constructing, or upgrade an existing instance with `prolog.enableV2Clpfd()`.
- There is **no** `jprolog.sandbox`/`jprolog.budget` property — those are Java APIs (below).

Pick the right file when fixing bugs: arithmetic on the default engine is `core.arith.v2.ArithEvaluator` (MachineSolver inlines `is/2` and the arithmetic comparisons through it); output of `write/1`/`writeln/1`/`writeq/1`/`format/2` is `core.util.TermFormatter` — **not** `core.write.v2`, which only backs the IDE's Ctrl+Alt+L formatter (`core.write.v2.PrologFormatter`).

### Embedding API (`core.engine.Prolog`)

- `solve(String)` — default v2 engine. The `solve(Term)` overload always runs the legacy `QuerySolver` (no v2 engine, no inference budget). `solveLegacy(String)` forces the legacy solver explicitly regardless of `jprolog.engine`.
- `solveStream(query, sink)` — lazy solution streaming; the sink (`Predicate<Map<String,Term>>`) returns `false` to stop. Truly lazy/cancellable only on the v2 engine; on legacy it eagerly materializes all solutions and replays them.
- `consultWithDiagnostics(program, filename)` — per-clause compile collecting `CompilationError`s with line numbers (returns `CompilationResult`) instead of throwing on first error.
- `compileFile("x.pl")` / `consultSmart("x.pl")` — compile to `.jpc` / auto-cached consult.
- `getPredicateIndicatorAtLine(line)` — maps a source line to the clause's predicate indicator (IDE gutter breakpoints).
- `enableSafeMode()` / `setInferenceBudget(steps)` — see hardening below.

### Sandbox & Resource Limits (v3.4.0)

- **`Prolog.enableSafeMode()`** — per-instance, **irreversible**, deny-by-package sandbox: unregisters every built-in whose class lives in `builtin.{os,ffi,filesystem,network,http,jdbc,persistence}` (`UNSAFE_BUILTIN_PACKAGES` in `Prolog.java`; ~96 removed). Java API only — no system property or flag; use a fresh `Prolog` per security domain. **Gotcha**: `builtin.io` is *not* denied, so `open/3,4` can still read/write host files under safe mode — file isolation needs OS-level sandboxing (or extending the deny list). When adding a host-touching built-in, put it in one of the denied packages or safe mode won't strip it.
- **`Prolog.setInferenceBudget(steps)`** (0 = unlimited) — aborts a runaway query with `InferenceLimitException`. Enforced by the **v2 engine only**; legacy paths ignore it. (The javadoc claims it raises `resource_error(inference_limit_exceeded)` — wrong; trust the implementation and `ProductionAuditTest`.)
- **Error-trust model** — deliberate asymmetry: deep-term/deep-input `StackOverflowError`s are converted to a `PrologException` carrying ISO `resource_error('stack_overflow')` / `resource_error('parser_nesting')` so the Java embedder sees a controlled error instead of a raw `Error` (on the v2 engine the conversion happens after the query unwinds, so the running program's `catch/3` does not see it), while `InferenceLimitException` (budget), `QueryCancelledException` (IDE/embedder Stop, via thread interrupt polled by MachineSolver), and `DebugStopException` (debugger Stop) are plain `RuntimeException`s — **not** `PrologException` — so untrusted `catch/3` cannot swallow them; the Java embedder must catch them. Preserve this distinction when adding limits.
- **First-argument indexing is deliberately absent** from the v2 engine: attempted and reverted (ISS-2025-0340) because `KnowledgeBase.getRulesWithFirstArgIndex` returns an empty list for predicates whose index was never populated, silently dropping clauses. Fix the KB index before retrying.

### Term Hierarchy

All terms are immutable. Base class: `core.terms.Term`
- `Atom` — symbols like `hello`, `'John'`
- `Number` — integers and floats
- `Variable` — logic variables (`X`, `_`)
- `CompoundTerm` — compound terms like `f(a, b)`
- `PrologString` — string literals

### Built-in Predicate System

Two interfaces for built-ins:
- `BuiltIn.execute(Term query, Map bindings, List solutions)` — standard predicates
- `BuiltInWithContext.executeWithContext(QuerySolver, Term, Map, List)` — meta-predicates that need solver access (findall, bagof, setof, call, catch)

Context-dependent built-ins are wrapped in `CollectionBuiltInAdapter` to bridge the two interfaces.

**Registration**: `BuiltInFactory` has a static `FACTORY_MAP` of predicate names to `Supplier<BuiltIn>`. The `Prolog` constructor iterates this map and registers each into `BuiltInRegistry`.

**Adding a new built-in**:
1. Create class in `it.denzosoft.jprolog.builtin.<category>/` implementing `BuiltIn` or `BuiltInWithContext`. If it touches the host (processes, files, network, JVM), it must live in one of the safe-mode denied packages.
2. Register in `BuiltInFactory.FACTORY_MAP`
3. Update `docs/references/BUILTIN_PREDICATES_REFERENCE.md`
4. Run `./test_all_examples.sh` to verify no regressions

Output discipline: built-ins must write through the thread-local `StreamManager.out()` — never `System.out`/`System.setOut`. That is how the IDE captures per-thread output (`StreamManager.setThreadLocalOutput(ps)` around background solves, reset in `finally`).

### Package Layout

- `core.engine` — `Prolog`, `QuerySolver`, `KnowledgeBase`, `BuiltInRegistry`, `BuiltInFactory`, `ArithmeticEvaluator` (legacy arith), debug classes (`DebugController`, `DebugEvent`, `DebugStackEntry`), `InferenceLimitException`, `QueryCancelledException`; `core.engine.v2.MachineSolver` — the **default** iterative SLD engine (fallback `-Djprolog.engine=legacy`)
- `core.terms` — `Term`, `Atom`, `Number`, `Variable`, `CompoundTerm`, `PrologString`
- `core.parser` — legacy `Parser`, `PrologParser`, `TermParser` (recursive descent, fallback). `core.parser.v2` — `Lexer` + `TermReader` (single-pass, operator-precedence/Pratt), the **default** parser
- `core.operator` — `Operator`, `OperatorTable` (precedence management)
- `core.module` — `Module`, `ModuleManager`, `PredicateSignature`
- `core.dcg` — `DCGTransformer` (legacy `-->` translator, fallback via `-Djprolog.dcg=legacy`); `core.dcg.v2.DCGTranslator` is the default single-pass ISO translator (head push-back, `|`, `\+`, `call//N`, `{}`, `!`, `->`)
- `core.write.v2` — `TermWriter` + `PrologFormatter` (IDE source formatter); `core.arith.v2` — `ArithEvaluator` (default-engine arithmetic)
- `core.compiled` — `JpcReader`, `JpcWriter`, `JpcFormat` (`.jpc` binary format)
- `core.exceptions` — `PrologException` and ISO error-term helpers
- `core.system` — `PrologFlags` (ISO flag store)
- `core.util` — `TermFormatter` (output of the `write/1` family), `ListUtils`; distinct from `core.utils` (`ListTerm`, `Substitution`, `CollectionUtils`) and from top-level `util` (`TermCopier`, `TermUtils` — variable renaming on rule copy)
- `builtin/` — organized by category. ISO-core: `arithmetic/`, `atom/`, `character/`, `control/`, `conversion/`, `database/`, `dcg/`, `debug/`, `exception/`, `io/`, `list/`, `meta/`, `string/`, `system/`, `term/`, `type/`, `unification/`. Extended library: `clpfd/` (with `clpfd/v2/` — the default CLP(FD) engine), `crypto/`, `csv/`, `datetime/`, `ffi/`, `filesystem/`, `graph/`, `http/`, `jdbc/`, `json/`, `logging/`, `network/`, `os/`, `persistence/`, `regex/`, `threading/`, `xml/`, `extension/`
- `extension/` (top-level) — pluggable extension examples (`example/`, `math/`)
- `editor/` — Swing IDE: `PrologIDE` (owns the single shared `Prolog` instance — panels fetch it via `ide.getPrologEngine()`, never construct their own), `FileEditor`, `EditorTabbedPane`, and the bottom tabs `OutputConsole`, `BuildPanel`, `RunPanel`, `SearchResultsPanel`, `DebugPanel` (hosted by `BottomTabbedPane`)
- `PrologCLI` — CLI with `:consult`, `:trace [on|off]`, `:help`, `:quit` commands

### Debug & Tracing Architecture

The debugger uses a **two-thread model** with blocking synchronization:

1. **Solver Thread**: runs the query in a background thread
2. **Swing EDT**: handles UI updates and button clicks
3. **Synchronization**: `DebugController` uses `wait()/notify()` on a `pauseLock` object; `DebugPanel` implements `DebugController.DebugListener` and marshals to the EDT via `SwingUtilities.invokeLater()`

Wiring contract (works for **both** engines): install the controller with `querySolver.setDebugController(...)` — the shared `QuerySolver` is the durable home. Each per-query `MachineSolver` picks it up at solve start and fires the same four-port events (Call/Exit/Fail/Redo), so since v3.3.0 the IDE debugger runs on the default v2 engine via plain `engine.solve()`. All hooks are guarded by `debugController != null` — zero overhead when not debugging.

Engine specifics:
- While debugging, MachineSolver **disables its fast paths** (inline `=/2`, native built-ins) so the 200+ registry built-ins fire ports through an instrumented bridge. Don't "optimize" those branches back without checking `debugController`. Same rule on legacy: the LCO trampoline bypassed ports and is disabled while debugging (ISS-2025-0330) — any new fast path needs the same treatment.
- Redo/Fail ports are emitted by stashing `traceGoal`/`traceDepth` on choice points; a new choice-point kind representing a traced goal must carry these fields or Redo/Fail silently vanish from traces.
- Breakpoints: line-accurate via `Rule.sourceLine` + `Prolog.getPredicateIndicatorAtLine()`; persist in hidden sidecar files `.<source>.bps` next to `.pl` files. Conditional/hit-count breakpoints evaluate their condition goal via a detached sub-solve (`engine.solveLegacy()` with the controller temporarily nulled); a throwing condition never pauses.
- `trace/0`/`notrace/0` (and the CLI `:trace`, IDE Run-panel Trace toggle) use a process-global static flag in `builtin.debug.Trace`; only the **v2 engine** checks it, writing depth-indented four-port lines to `StreamManager.out()`. Under the legacy engine, `trace/0` sets the flag but no port lines are printed (the legacy `traceEnabled` field is an unrelated `LOGGER.info` mechanism).

### Compilation Diagnostics

`Prolog.consultWithDiagnostics(program, filename)` compiles per-clause, collecting errors with line numbers instead of throwing on first error. Returns `CompilationResult` with `List<CompilationError>`. The IDE renders these in the Build tab plus in-editor squiggles/gutter markers.

### Binary Compiled Format (.jpc)

JProlog compiles `.pl` source to `.jpc` (JProlog Compiled) for fast loading. Implemented by `JpcWriter` / `JpcReader` in `core.compiled` — uses string interning, varint encoding, and source-hash validation to detect stale compilations. The unified `OperatorTable` is shared between parser, `op/3`, and query resolution so dynamic operators round-trip through `.jpc`.

### Key Design Decisions

- **Single-threaded** execution (Prolog semantics); IDE runs queries on background threads with thread-local output capture (`StreamManager`) and cancels via thread interrupt → `QueryCancelledException`
- **Immutable terms** with external substitution maps for bindings
- Variable scoping handled by `util.TermCopier` which renames variables when copying rules
- ISO 13211-1 compliance where possible (exception handling, arithmetic functions, error terms)
- `.gitignore` scratch patterns must stay **anchored to the root** (`/Debug*.java`, `/Test*.java`, `/*.sh`) — the unanchored forms once silently excluded core sources like `DebugController` from the repo (ISS-2025-0334)

## Development Workflow

### Issue Tracking (MANDATORY)

Every bug or feature request must be documented before implementation:

- **Issues**: `docs/tracking/track-issues.md` — format `ISS-YYYY-NNNN`; terminal status in practice is `RESOLVED`/`VERIFIED`
- **Change Requests**: `docs/tracking/track-change-requests.md` — format `CR-YYYY-NNNN`
- **Limitations**: `docs/tracking/track-limitations.md` — add when issue found, remove when resolved
- **Release Notes**: `docs/tracking/track-release-notes.md`

Before allocating a new ISS number, grep **CHANGELOG.md** for the highest used one — track-issues.md lags behind recent releases (e.g. ISS-2025-0320..0341 exist only in CHANGELOG.md and source `START_CHANGE` tags). track-issues.md's internal ordering and header levels are inconsistent; grep for an ID rather than assuming position. Some tracking content is in Italian — match surrounding style rather than rewriting.

### Code Change Tagging

All code modifications must be tagged with the ID that drove the change (ISS, LIM, or CR):
```java
// START_CHANGE: ISS-2025-0001 - Description
// ... modified code ...
// END_CHANGE: ISS-2025-0001
```

### Test Verification (MANDATORY per bug/limitation fix)

Every resolved bug or limitation must have a corresponding JUnit test that verifies the fix. Tests go in `src/test/java/it/denzosoft/jprolog/test/builtin/BugFixVerificationTest.java`, organized by ISS number (audit-driven findings live in `test/audit/` instead, e.g. `ProductionAuditTest.java` for the v3.4.0 sandbox/budget fixes):
- Add a `@Test` method named after the issue and fix (e.g., `testISS0188_ModNegativeDivisor`)
- The test must fail without the fix and pass with the fix
- Use `prolog.solve()` for query-level assertions, direct Java assertions for internal class fixes
- Run `mvn test` to confirm the new tests pass alongside all existing tests

### Documentation Updates (MANDATORY per release)

When adding/modifying built-ins or operators, update:
- `docs/references/BUILTIN_PREDICATES_REFERENCE.md`
- `docs/references/BUILTIN_OPERATORS_REFERENCE.md`

(These two uppercase filenames are deliberate exceptions — do not rename them.)

All documentation must be in English. Naming convention: `[category-]descriptive-name.md` (lowercase, hyphens, no underscores).

Doc directories: `docs/guides/guide-*.md`, `docs/references/ref-*.md`, `docs/reports/report-*.md`, `docs/tracking/track-*.md`

### Release Process

1. `mvn clean compile` (must succeed)
2. `mvn test` (must pass)
3. `./test_all_examples.sh` (must be >= 75% — checked manually, the script doesn't compute it)
4. Increment patch version in `pom.xml`
5. Update: `CHANGELOG.md`, `docs/tracking/track-issues.md`, `docs/tracking/track-limitations.md`, `docs/tracking/track-release-notes.md`
6. Tag format: `v{major}.{minor}.{patch}`

Versioning: major = breaking changes, minor = new features, patch = bug fixes (auto-increment per session).

### Session Cleanup

Before concluding any session, remove temporary files from the project root:
```bash
rm -f *.class *Test.java Debug*.java temp_*.txt *_debug.* test_input.txt
mv test_*.pl examples/ 2>/dev/null || true
rm -f temp_*.pl debug_*.pl
```

Test `.pl` files belong in `examples/` with pattern `test_XX_description.pl`.
