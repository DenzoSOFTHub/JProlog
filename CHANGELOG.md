# Changelog

All notable changes to JProlog will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

## [3.0.0] - 2026-03-21

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