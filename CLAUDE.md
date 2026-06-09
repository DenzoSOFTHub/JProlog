# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

JProlog is a Prolog interpreter written in Java (1.8). It includes a core engine, 200+ built-in predicates, a Swing-based IDE, and a CLI. No external dependencies beyond JUnit 4 for tests. As of v3.0.0, several subsystems have clean-room **v2** rewrites that are now the default (parser, CLP(FD), DCG) plus an opt-in v2 resolution engine — see "v2 Subsystems" below.

**Repository**: https://github.com/DenzoSOFTHub/JProlog
**Current version**: check `<version>` in pom.xml

## Build & Run

```bash
mvn compile                  # Build
mvn test                     # JUnit tests (full suite)
mvn test -Dtest=BugFixVerificationTest                  # Single test class
mvn test -Dtest=BugFixVerificationTest#testISS0188_ModNegativeDivisor   # Single method
mvn clean compile            # Clean rebuild

# Run CLI
java -cp target/classes it.denzosoft.jprolog.PrologCLI

# Run IDE
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

## Architecture

### Query Execution Flow

1. **Parser** — by default the clean-room single-pass v2 parser (`core.parser.v2.Lexer` + `core.parser.v2.TermReader`, operator-precedence/Pratt) converts Prolog text to `Term` objects. The legacy `core.parser.Parser` is the fallback (`-Djprolog.parser=legacy`).
2. **KnowledgeBase** (`core.engine.KnowledgeBase`) stores facts/rules as `Rule` objects
3. **QuerySolver** (`core.engine.QuerySolver`) implements SLD resolution with backtracking
4. For each goal, the solver checks `BuiltInRegistry` first, then falls back to user-defined rules in the knowledge base
5. **Unification** happens via `Term.unify(Term, Map<String, Term> substitution)` — Robinson algorithm
6. **Cut** is managed through `CutStatus`/`MutableCutStatus` flags passed through recursion

### v2 Subsystems (v3.0.0)

v3.0.0 ships clean-room rewrites of several subsystems, selectable via system properties (and `Prolog.setUseV2*()` toggles):

| Subsystem | v2 package | Default | Fallback |
|---|---|---|---|
| Parser | `core.parser.v2` (`Lexer` + `TermReader`) | **v2** | `-Djprolog.parser=legacy` |
| CLP(FD) | `builtin.clpfd.v2` (`ClpStore`/`IntervalDomain`/`Constraint`/`Labeler`) | **v2** | `-Djprolog.clpfd=legacy` |
| DCG | `core.dcg.v2.DCGTranslator` | **v2** | `-Djprolog.dcg=legacy` |
| Term writer | `core.write.v2.TermWriter` | standalone | — |
| Arithmetic | `core.arith.v2.ArithEvaluator` | standalone | — |
| Resolution engine | `core.engine.v2.MachineSolver` | legacy | `-Djprolog.engine=v2` (opt-in) |

Also new in v3.0.0: `div`/`rdiv` operators (400 yfx) in `core.operator.OperatorTable`; `setup_call_cleanup/3` and `call_cleanup/2` (`builtin.meta.SetupCallCleanup`). Baseline: 675/675 JUnit, 20/20 example programs; the default v2 parser parses 123/130 examples (legacy: 117); the opt-in v2 engine passes ~664/670.

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
1. Create class in `it.denzosoft.jprolog.builtin.<category>/` implementing `BuiltIn` or `BuiltInWithContext`
2. Register in `BuiltInFactory.FACTORY_MAP`
3. Update `docs/references/BUILTIN_PREDICATES_REFERENCE.md`
4. Run `./test_all_examples.sh` to verify no regressions

### Package Layout

- `core.engine` — `Prolog`, `QuerySolver`, `KnowledgeBase`, `BuiltInRegistry`, `BuiltInFactory`, `ArithmeticEvaluator`; `core.engine.v2.MachineSolver` — opt-in iterative SLD resolution engine (`-Djprolog.engine=v2`)
- `core.terms` — `Term`, `Atom`, `Number`, `Variable`, `CompoundTerm`, `PrologString`
- `core.parser` — legacy `Parser`, `PrologParser`, `TermParser` (recursive descent, fallback). `core.parser.v2` — `Lexer` + `TermReader` (single-pass, operator-precedence/Pratt) is the **default** parser in v3.0.0
- `core.operator` — `Operator`, `OperatorTable` (precedence management)
- `core.module` — `Module`, `ModuleManager`, `PredicateSignature`
- `core.dcg` — `DCGTransformer` (legacy `-->` translator, fallback via `-Djprolog.dcg=legacy`); `core.dcg.v2.DCGTranslator` is the default single-pass ISO translator (head push-back, `|`, `\+`, `call//N`, `{}`, `!`, `->`)
- `core.compiled` — `JpcReader`, `JpcWriter`, `JpcFormat` (`.jpc` binary format)
- `core.exceptions` — `PrologException` and ISO error-term helpers
- `core.system` — `PrologFlags` (ISO flag store)
- `core.util` — `TermCopier`, `TermUtils` (variable renaming on rule copy); distinct from `core.utils` (`ListTerm`, `Substitution`, `CollectionUtils`)
- `builtin/` — organized by category. ISO-core: `arithmetic/`, `atom/`, `character/`, `control/`, `conversion/`, `database/`, `dcg/`, `debug/`, `exception/`, `io/`, `list/`, `meta/`, `string/`, `system/`, `term/`, `type/`, `unification/`. Extended library: `clpfd/` (with `clpfd/v2/` — interval-domain solver: `ClpStore`, `IntervalDomain`, `Labeler`, `Constraint` — the **default** CLP(FD) engine in v3.0.0; `-Djprolog.clpfd=legacy` to fall back), `crypto/`, `csv/`, `datetime/`, `ffi/`, `filesystem/`, `graph/`, `http/`, `jdbc/`, `json/`, `logging/`, `network/`, `os/`, `persistence/`, `regex/`, `threading/`, `xml/`, `extension/`
- `extension/` (top-level) — pluggable extension examples (`example/`, `math/`)
- `editor/` — Swing IDE: `PrologIDE`, `FileEditor`, `DebugPanel`, `ConsolePanel`
- `PrologCLI` — CLI with `:consult`, `:trace`, `:help`, `:quit` commands

### Debug System Architecture

The debugger uses a **two-thread model** with blocking synchronization:

1. **Solver Thread**: Runs `QuerySolver.solve()` in a background `Thread`
2. **Swing EDT**: Handles UI updates and button clicks
3. **Synchronization**: `DebugController` uses `wait()/notify()` on a `pauseLock` object

Key classes:
- `DebugController` (`core.engine`) — orchestrator with breakpoint management, step mode logic, call stack tracking
- `DebugEvent` (`core.engine`) — data carrier for port events (CALL/EXIT/FAIL/REDO)
- `DebugStackEntry` (`core.engine`) — single call stack frame
- `DebugPanel` (`editor`) — implements `DebugController.DebugListener`, receives callbacks via `SwingUtilities.invokeLater()`

QuerySolver hooks are guarded by `if (debugController != null)` — zero overhead when not debugging.

### Compilation Diagnostics

`Prolog.consultWithDiagnostics(program, filename)` compiles per-clause, collecting errors with line numbers instead of throwing on first error. Returns `CompilationResult` with `List<CompilationError>`.

### Binary Compiled Format (.jpc)

JProlog compiles `.pl` source to `.jpc` (JProlog Compiled) for fast loading. Implemented by `JpcWriter` / `JpcReader` in `core.compiled` — uses string interning, varint encoding, and source-hash validation to detect stale compilations. The unified `OperatorTable` is shared between parser, `op/3`, and query resolution so dynamic operators round-trip through `.jpc`.

### Key Design Decisions

- **Single-threaded** execution (Prolog semantics), debug uses separate thread with blocking sync
- **Immutable terms** with external substitution maps for bindings
- Variable scoping handled by `util.TermCopier` which renames variables when copying rules
- ISO 13211-1 compliance where possible (exception handling, arithmetic functions, error terms)

## Development Workflow

### Issue Tracking (MANDATORY)

Every bug or feature request must be documented before implementation:

- **Issues**: `docs/tracking/track-issues.md` — format `ISS-YYYY-NNNN`, status flow: `TO_ANALYZE` -> `IN_ANALYSIS` -> `IN_PROGRESS` -> `RESOLVED` -> `CLOSED`
- **Change Requests**: `docs/tracking/track-change-requests.md` — format `CR-YYYY-NNNN`
- **Limitations**: `docs/tracking/track-limitations.md` — add when issue found, remove when resolved
- **Release Notes**: `docs/tracking/track-release-notes.md`

### Code Change Tagging

All code modifications must be tagged:
```java
// START_CHANGE: ISS-2025-0001 - Description
// ... modified code ...
// END_CHANGE: ISS-2025-0001
```

### Test Verification (MANDATORY per bug/limitation fix)

Every resolved bug or limitation must have a corresponding JUnit test that verifies the fix. Tests go in `src/test/java/it/denzosoft/jprolog/test/builtin/BugFixVerificationTest.java`, organized by ISS number:
- Add a `@Test` method named after the issue and fix (e.g., `testISS0188_ModNegativeDivisor`)
- The test must fail without the fix and pass with the fix
- Use `prolog.solve()` for query-level assertions, direct Java assertions for internal class fixes
- Run `mvn test` to confirm the new tests pass alongside all existing tests

### Documentation Updates (MANDATORY per release)

When adding/modifying built-ins or operators, update:
- `docs/references/BUILTIN_PREDICATES_REFERENCE.md`
- `docs/references/BUILTIN_OPERATORS_REFERENCE.md`

All documentation must be in English. Naming convention: `[category-]descriptive-name.md` (lowercase, hyphens, no underscores).

Doc directories: `docs/guides/guide-*.md`, `docs/references/ref-*.md`, `docs/reports/report-*.md`, `docs/tracking/track-*.md`

### Release Process

1. `mvn clean compile` (must succeed)
2. `mvn test` (must pass)
3. `./test_all_examples.sh` (must be >= 75%)
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
