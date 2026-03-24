# JProlog - Release Notes

## Release 2.6.8 - 2026-03-24

### Seventh-Round Deep Analysis Fixes

13 fixes for parser precision, predicate correctness, and ISO compliance across 11 files.

- **ISS-2025-0191**: TermParser BigInteger precision, PredSort solver call + error propagation, ToCodes Unicode BMP + list check, TableStore abolish collision, Number NaN hashCode, msb/lsb evaluationError, Nth1 unification, AtomConcat mode, ListTerm iterative, DCG variable prefix, Subtract structural equality

---

## Release 2.6.7 - 2026-03-24

### Sixth-Round Deep Analysis Fixes

20 fixes for ISO compliance, correctness, and robustness across 17 files.

- **ISS-2025-0190**: KeySort ISO ordering, Intersection structural dedup, Phrase bindings fix, LayeredMap O(N²) rollback + removed set restoration, Rational equals/hashCode contract, Unicode BMP range in NumberCodes/ToCodesSimple, Succ long overflow, MapList/4 bindings, IfThen ISO first-solution commit + cut propagation, AcyclicTermCheck cycle detection, msb/lsb/popcount error terms, Ignore system error propagation, PrologString escape symmetry, JpcReader bounds checking, ListTerm unmodifiable views, DebugPanel volatile fields

---

## Release 2.6.6 - 2026-03-24

### Fifth-Round Deep Analysis Fixes

11 fixes for ISO compliance, arithmetic precision, exception propagation, and term immutability.

- **ISS-2025-0189**: Shift >= 64 BigInteger promotion, round/float_fractional_part NaN/Infinity, \+ exception propagation, findall/bagof PrologException passthrough, Rational.unify exact comparison, LayeredMap.isEmpty with removed set, ArithmeticComparison integer precision, Atom/Variable immutability, PrologString escape symmetry

**Tests**: 320/320 JUnit, 20/20 examples

---

## Release 2.6.5 - 2026-03-24

### Fourth-Round Deep Analysis Fixes

20+ fixes from comprehensive fourth-round codebase analysis.

- **ISS-2025-0188**: ISO mod/2 fix, Number bitLength thresholds, PrologString single-pass unescape, Member/MapList/Delete/Numlist/AtomChars/Sort/NotUnifiable/StringConcat bug fixes, JpcWriter null safety, DebugPanel thread safety, dead code removal (duplicate exception classes)

**Tests**: 320/320 JUnit, 20/20 examples

---

## Release 2.6.4 - 2026-03-24

### Third-Round Analysis Fixes

Final round of deep codebase analysis. 7 fixes for remaining edge cases.

- **ISS-2025-0187**: Length variable naming, Intersection deduplication, Plus exact comparison, Foldl binding accumulation, NumberCodes Unicode BMP range, CurrentPredicate parseInt safety, ArithmeticEvaluator shift overflow

**Tests**: 320/320 JUnit, 20/20 examples

---

## Release 2.6.3 - 2026-03-24

### Deep Bug Fixes, ISO Compliance, Robustness

Second comprehensive fix release from deep analysis. 20+ fixes across list predicates, engine, parser, debug, and utilities.

- **ISS-2025-0184**: Numlist range, Sort ISO ordering, MapList bindings, ForAll check
- **ISS-2025-0185**: Rational zero-div, power 0^-N, DCG pushback, JPC Rational support
- **ISS-2025-0186**: DebugController stack, ListTerm unification, TermCopier AtomicLong, Substitution cycles, Nth0/Nth1 enumeration

**Tests**: 320/320 JUnit, 20/20 examples

---

## Release 2.6.2 - 2026-03-24

### Bug Fixes, DCG Completion, ISO Compliance

Comprehensive bug fix release addressing 25+ issues found via deep codebase analysis.

- **ISS-2025-0180**: Core engine fixes — KnowledgeBase sync, multiArgIndex leaks, CompiledClause NaN, shift validation, LCO logging
- **ISS-2025-0181**: Term system fixes — Number equals/hashCode contract, NaN unification, PrologString escapes, AtomTable race conditions
- **ISS-2025-0182**: Built-in fixes — ArithmeticComparison exact comparison, Is error handling, Between overflow, Length malformed list, functor/3 variable naming, put_attr type error
- **ISS-2025-0183**: DCG completion — `\+` negation in DCG bodies, if-then-else committed-choice semantics

**DCG/CFG**: Now 100% feature complete (18/18 standard features)
**Tests**: 320/320 JUnit, 20/20 examples

---

## Release 2.6.1 - 2026-03-23

### Medium Priority Limitations Resolved (LIM-010 through LIM-016)

All remaining Medium priority limitations resolved. All 16 tracked limitations now closed.

- **LIM-010**: CHR — basic `CHRStore` with simplification/propagation rules
- **LIM-011**: DCG advanced features — if-then, pushback notation, `call//N`
- **LIM-012**: Rational numbers — `Rational` class, `rdiv` operator
- **LIM-013**: Number literal notation — already implemented in TermParser
- **LIM-014**: Multi-argument indexing in KnowledgeBase
- **LIM-015**: Compiled clause cache (`CompiledClause`) for fast rejection
- **LIM-016**: Atom garbage collection via `AtomTable` with WeakReferences

**Tests**: 320/320 JUnit, 20/20 examples

---

## Release 2.6.0 - 2026-03-23

### Attributed Variables, Coroutining, BigInteger Arithmetic, Module Calls

Major feature release implementing all Critical and High priority limitations (LIM-001 through LIM-009).

- **LIM-001**: `freeze/2`, `when/2`, `dif/2` — coroutining with attributed variable hooks
- **LIM-002**: `put_attr/3`, `get_attr/3`, `del_attr/2`, `attvar/1` — attributed variables with unification hooks
- **LIM-003**: `nb_setval/2`, `nb_getval/2`, `nb_current/2`, `nb_delete/1`, `b_setval/2`, `b_getval/2` — global variables
- **LIM-004**: `Module:Goal` — module-qualified calls wired into QuerySolver
- **LIM-005**: `predicate_property/2` — predicate introspection
- **LIM-006**: `code_type/2` — character code classification
- **LIM-007**: `set_stream_position/2`, `stream_position/2` — stream repositioning
- **LIM-008**: `Number` class with `long`/`BigInteger`/`double` dual representation; integer arithmetic stays exact
- **LIM-009**: Enhanced `write_term/2` (`numbervars`, `quoted`, `ignore_ops`, `max_depth`), `numbervars/3`, enhanced `read_term/2`

**Tests**: 320/320 JUnit, 20/20 examples

---

## Release 2.5.5 - 2026-03-22

### Code Quality, Documentation, Dual-Arity Operators

Final cleanup release resolving remaining issues from the v2.5.x improvement cycle.

- **ISS-2025-0177**: Fixed dual-arity operator bug — `+`/`-` can now be both prefix and infix via composite key storage in OperatorDefinition
- **ISS-2025-0178**: Dead code removal — deleted `SimplePrologEngine`, `PrologEngine`, `MainProlog`; removed legacy `Variable.occurs()`; converted `System.out.println` to Logger
- **ISS-2025-0179**: Documentation — updated intro guide, added 14 missing predicates to reference (265+ total)

**Tests**: 320/320 JUnit, 20/20 examples

---

## Release 2.5.0 - 2026-03-21

### Package Cleanup, Last Call Optimization, Java FFI

Focused release that removes 31 toy/academic packages to streamline the codebase, adds Last Call Optimization (LCO) for stack-safe tail recursion, and introduces a Java Foreign Function Interface (FFI) with 12 new built-in predicates.

### Package Cleanup (ISS-2025-0160)

Removed 31 toy/academic built-in packages that were impractical for real-world use:
- AI/Knowledge: NLP, Expert Systems, Inference Engine, AI Planner, Fuzzy Logic, Bayesian Networks
- Computational Intelligence: Genetic Algorithms, Neural Networks, Optimization, Simulation, Workflow Engine
- Advanced Logic: CLP(R), Knowledge Graphs, Parsing/DSL, Datalog, Semantic Web/RDF, Model Checking, CHR, BDI Agents, ASP, Explainable AI
- Classic Prolog: Type Inference, Theorem Proving, Symbolic Mathematics, Meta-Interpretation, Temporal Logic, ProbLog, SAT Solving, Game Playing, Term Rewriting, Description Logic

Kept 16 useful infrastructure packages: CLP(FD), Tabling, HTTP, JSON, XML, CSV, Regex, Crypto, DateTime, Filesystem, OS, Threading, Logging, Persistence, Graph, Concurrent.

### Last Call Optimization (ISS-2025-0161)

Implemented LCO via trampoline in QuerySolver.java. Tail-recursive predicates with single-candidate matching now run iteratively instead of recursively, eliminating stack overflow for deep recursion. `count_down(10000)` now works without stack overflow.

### Java Foreign Function Interface (ISS-2025-0162)

12 new built-in predicates for Java interoperability:
- **Object lifecycle**: `java_new/3`, `java_class/2`, `java_instanceof/2`
- **Method/field access**: `java_call/4`, `java_get_field/3`, `java_set_field/3`
- **Array operations**: `java_array_new/3`, `java_array_get/3`, `java_array_set/3`, `java_array_length/2`
- **Conversion**: `java_to_term/2`, `java_from_term/2`

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

## Release 3.0.0 - 2026-03-21

### 47 New Built-in Packages (555+ Predicates), AI/ML Engine, Concurrent Execution, Advanced Logic Programming & Classic Prolog Packages

Major release with 47 new built-in predicate packages covering core infrastructure (CLP(FD), tabling, HTTP, crypto, JSON, datetime, filesystem, OS, regex, XML, threading, CSV, logging, persistence, graph algorithms), AI/knowledge engineering (expert systems, NLP, inference, planning, fuzzy logic, Bayesian networks), computational intelligence (genetic algorithms, neural networks, optimization, simulation, workflow engines), SWI-Prolog compatible concurrent execution, advanced logic programming (CLP(R), knowledge graphs, parsing/DSL, Datalog, semantic web/RDF, model checking, CHR, BDI agents, ASP, explainable AI), and classic Prolog packages (type inference, theorem proving, symbolic math, meta-interpretation, temporal logic, probabilistic logic, SAT solving, game playing, term rewriting, description logic). 665+ total built-in predicates.

### New Built-in Packages

**Core Infrastructure (108 predicates)**:
- **CLP(FD)** (13 predicates): Constraint logic programming over finite domains with AC-3 propagation and snapshot/restore backtracking (ISS-2025-0123)
- **Tabling** (3 predicates): Memoization with loop detection and variant tabling (ISS-2025-0124)
- **HTTP** (11 predicates): HTTP server/client with handler routing, JSON replies, and URL encoding (ISS-2025-0125)
- **Crypto** (10 predicates): Hashing, HMAC, encryption, random byte generation (ISS-2025-0112)
- **JSON** (6 predicates): JSON parsing and generation (ISS-2025-0113)
- **DateTime** (10 predicates): Date/time operations and formatting (ISS-2025-0114)
- **Filesystem** (15 predicates): File and directory operations (ISS-2025-0115)
- **OS** (12 predicates): Operating system interaction (ISS-2025-0116)
- **Regex** (5 predicates): Regular expression matching and manipulation (ISS-2025-0117)
- **XML** (3 predicates): XML parsing and generation (ISS-2025-0118)
- **Threading** (10 predicates): Concurrent execution with thread safety review (ISS-2025-0119)
- **CSV** (4 predicates): CSV reading and writing (ISS-2025-0120)
- **Logging** (6 predicates): Structured logging capabilities (ISS-2025-0121)
- **Persistence** (10 predicates): Database save/load, JSON import/export, snapshots (ISS-2025-0126)
- **Graph Algorithms** (12 predicates): Path finding, shortest path, topological sort, MST, cycle detection (ISS-2025-0127)

**AI and Knowledge Engineering (147 predicates)**:
- **Expert Systems** (16 predicates): Forward/backward chaining, certainty factors, explanation facilities (ISS-2025-0128)
- **NLP** (15 predicates): Tokenization, stemming, n-grams, TF-IDF, Levenshtein, Soundex, sentiment (ISS-2025-0129)
- **Inference Engine** (13 predicates): Abduction, ILP, non-monotonic reasoning, frame-based KR (ISS-2025-0130)
- **AI Planner** (11 predicates): STRIPS planning with A*, BFS, DFS, IDS, best-first search (ISS-2025-0131)
- **Fuzzy Logic** (14 predicates): Mamdani fuzzy inference with centroid defuzzification (ISS-2025-0132)
- **Bayesian Networks** (14 predicates): Exact inference, Naive Bayes with Laplace smoothing (ISS-2025-0133)
- **Genetic Algorithms** (12 predicates): Configurable selection, crossover, mutation operators (ISS-2025-0134)
- **Neural Networks** (14 predicates): Feedforward with backpropagation, multiple activations (ISS-2025-0135)
- **Optimization** (8 predicates): LP (simplex), knapsack, TSP, simulated annealing, tabu search, max flow (ISS-2025-0136)
- **Simulation** (12 predicates): Discrete event simulation with statistical analysis (ISS-2025-0137)
- **Workflow Engine** (12 predicates): State machines, transitions, rules, instance management (ISS-2025-0138)

### New Meta-Predicate

- **aggregate_all/3**: Collect aggregated results over backtracking (ISS-2025-0122)

### Bug Fixes

- **copy_term/2**: Now uses fresh variable names via `TermCopier.copyWithFreshVariables` (ISS-2025-0122)
- **retract/1**: Now correctly returns unification bindings (ISS-2025-0122)
- **Goal directives**: Fixed `:- Goal.` execution during consult (ISS-2025-0122)

### Concurrent Execution (ISS-2025-0139)

- **concurrent/3** — execute N goals with thread pool
- **concurrent_maplist/2,3,4** — parallel map over lists
- **first_solution/3** — OR-parallelism, first goal wins
- **concurrent_and/2** — AND-parallelism
- **concurrent_or/2** — OR-parallelism with winner index

### Advanced Logic Programming (146 predicates)

- **CLP(R)** (8 predicates): Constraint logic programming over reals with simplex optimization (ISS-2025-0140)
- **Knowledge Graphs** (15 predicates): Triple store with ontological reasoning, transitive closure, path finding (ISS-2025-0141)
- **Parsing/DSL** (15 predicates): Tokenization, grammar definition, AST manipulation, code generation, DSL evaluation (ISS-2025-0142)
- **Datalog** (13 predicates): Bottom-up evaluation with semi-naive fixpoint, stratification, incremental maintenance (ISS-2025-0143)
- **Semantic Web/RDF** (15 predicates): RDF triple store with RDFS reasoning, prefix management, Turtle export (ISS-2025-0144)
- **Model Checking** (15 predicates): CTL model checking with reachability, deadlock detection, bisimulation (ISS-2025-0145)
- **CHR** (12 predicates): Constraint Handling Rules with simplification/propagation, store operations (ISS-2025-0146)
- **BDI Agents** (15 predicates): Belief-Desire-Intention architecture with agent lifecycle and messaging (ISS-2025-0147)
- **ASP** (13 predicates): Answer Set Programming with choice rules, brave/cautious reasoning (ISS-2025-0148)
- **Explainable AI** (15 predicates): Goal tracing, counterfactual reasoning, feature importance, decision paths (ISS-2025-0149)

### Classic Prolog Packages (125 predicates)

- **Type Inference** (12 predicates): Hindley-Milner type inference with unification, generalization, instantiation (ISS-2025-0150)
- **Theorem Proving** (13 predicates): Resolution-based theorem proving with CNF/DNF conversion, tautology/satisfiability checking (ISS-2025-0151)
- **Symbolic Mathematics** (12 predicates): Symbolic differentiation, simplification, expansion, integration, equation solving (ISS-2025-0152)
- **Meta-Interpretation** (12 predicates): Meta-interpreters with bounded/iterative deepening, tracing, partial evaluation, program transformation (ISS-2025-0153)
- **Temporal Logic / Event Calculus** (13 predicates): Event calculus with fluent initiation/termination, Allen temporal intervals (ISS-2025-0154)
- **Probabilistic Logic / ProbLog** (12 predicates): Probabilistic facts/rules, exact inference, evidence, entropy, KL divergence (ISS-2025-0155)
- **SAT Solving** (12 predicates): DPLL-based SAT solver with unit propagation, pure elimination, backbone computation (ISS-2025-0156)
- **Game Playing** (13 predicates): Minimax, alpha-beta pruning, negamax, MCTS game tree search (ISS-2025-0157)
- **Term Rewriting** (12 predicates): Term rewriting systems with normalization, confluence/termination analysis, critical pairs (ISS-2025-0158)
- **Description Logic** (14 predicates): ALC description logic with concept/role assertions, subsumption, satisfiability (ISS-2025-0159)

### Test Programs

- 46 comprehensive test files added: test_31 through test_77 covering all new packages
- 1100+ individual test cases across all new packages

### Repository Information
- **Tag**: v3.0.0
- **Release Date**: 2026-03-21
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 2.4.0 - 2026-03-19

### Integrated Debugger & Compilation Diagnostics

This release implements the full ISO four-port debug model with interactive IDE integration, completing all 10 Change Requests.

### Debug Engine (CR-0009 completed)
- **Four-port model**: CALL, EXIT, FAIL, REDO port events with full goal/depth/bindings data
- **DebugController**: Thread-safe orchestrator with wait/notify synchronization between solver thread and Swing EDT
- **Step execution**: Step Into, Step Over, Step Out, Continue modes
- **Breakpoints**: Predicate/arity-based breakpoint management
- **QuerySolver hooks**: Zero-overhead instrumentation (`if (debugController != null)` guard)

### DebugPanel (complete rewrite)
- Colored trace output (blue=CALL, green=EXIT, red=FAIL, orange=REDO)
- Real-time call stack tree with per-frame variable inspection
- Variables table filtered to user-visible variables only
- Integrated query input for debug-mode execution
- All step buttons wired to actual debug controller

### FileEditor Enhancements
- Breakpoint gutter: click line numbers to toggle breakpoints (red circle markers)
- Debug line highlighting: green background + arrow for current execution point
- Error line highlighting: persistent light red background via Highlighter

### Compilation Diagnostics
- `consultWithDiagnostics()` for per-clause error collection with file/line/message
- Build panel shows per-line errors with inline editor highlighting
- Clause count reporting on successful compilation

### IDE Enhancements
- "Debug Query..." menu item (Shift+F5)
- Enhanced compile output with clause counts and per-error line numbers

### Quality Metrics
- **320 unit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **10/10 Change Requests completed** (all CRs closed)

### Repository Information
- **Tag**: v2.4.0
- **Release Date**: 2026-03-19
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 2.3.0 - 2026-03-19

### 100% ISO 13211-1 Compliance & 25+ New Predicates

This release achieves full ISO Prolog compliance and adds comprehensive higher-order, list utility, term I/O, and conversion predicates.

### New Predicates (25+)

**Higher-order list predicates** (meta-predicates with QuerySolver access):
- `maplist/2,3,4` — apply goal to list elements
- `include/3`, `exclude/3` — filter lists by goal success/failure
- `foldl/4,5,6` — left fold with accumulator

**List utility predicates**:
- `last/2`, `flatten/2`, `numlist/3`
- `sum_list/2` / `sumlist/2`, `max_list/2`, `min_list/2`
- `delete/3`, `subtract/3`, `intersection/3`, `union/3`

**Term I/O predicates**:
- `term_to_atom/2` — bidirectional term/atom conversion
- `numbervars/3` — number variables with `$VAR(N)`
- `tab/1` — output N spaces
- `with_output_to/2` — capture output as atom

**Conversion predicates**:
- `string_to_atom/2`, `number_to_atom/2`, `atom_to_number/2`
- `string_code/3` — character code at index

### Module System (CR-0002 completed)
- Module-qualified calls `Module:Goal` via `solveInModuleContext`
- Module-isolated rule storage for non-user modules
- Import resolution: current module → global KB → imported modules

### Bug Fixes
- `atom_concat/3` modes (+,-,+) and (-,+,+) for suffix/prefix extraction
- `float/1` arithmetic function (ISO convert to float)
- 6 stale issues closed by triage

### Quality Metrics
- **100% ISO 13211-1 compliance** (111/111 core predicates)
- **320 unit tests, 0 failures**
- **20/20 example programs pass** (100%)
- **0 active issues, 0 active limitations**
- **9/10 Change Requests completed**

### Repository Information
- **Tag**: v2.3.0
- **Release Date**: 2026-03-19
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 2.2.0 - 2026-03-18

### Parser Hardening & Binary Compiled Format

This release delivers a completely rewritten parser, a new binary compiled format (.jpc) for faster program loading, and fixes for 6+ previously failing tests.

### New Features
- **Binary Compiled Format (.jpc)**: Serialize Prolog programs to compact binary with string interning, varint encoding, and source hash validation. Load programs 5-10x faster on subsequent consults.
- **Smart Consult**: `consultSmart(file)` auto-compiles to `.jpc` on first load and uses the cache on subsequent loads, with hash-based invalidation.
- **CLI Commands**: `:compile <file>` compiles `.pl` to `.jpc`; `:consult_compiled <file>` loads `.jpc` directly.
- **Module-qualified calls**: `Module:Goal` syntax now works (dispatched by QuerySolver).
- **call/1 through call/8**: Higher-order `call/N` now recognized at all arities.

### Parser Improvements
- **Unified Operator Table**: Replaced three disconnected operator registries with single shared `OperatorTable` instance, eliminating custom operator parsing failures.
- **Incremental Directive Processing**: `op/3` directives take effect immediately during `consult()` and `asserta()`, before subsequent clauses are parsed.
- **Pratt Parser**: Proper operator-precedence parsing with correct XFX/XFY/YFX associativity handling.
- **ISO Operator Removal**: Precedence 0 in `op/3` removes the operator per ISO standard.

### Bug Fixes
- Custom operators (e.g., `means`) now work correctly after `:- op(...)` directives
- `statistics/2` now returns solutions properly
- `=..` operator tokenization fixed
- Negative number vs prefix minus disambiguation
- Quoted atom parsing in new parser

### DCG Enhancements (CR-0003 completed)
- All 3 DCG parser limitations resolved (ISS-0040, 0041, 0042) — compound operators in list heads, special characters in terminals, complex arithmetic in constraints all now work
- `phrase/2`, `phrase/3` confirmed working with disjunction, constraint goals, and complex grammars

### New I/O Predicates (CR-0005 completed)
- `at_end_of_stream/0`, `at_end_of_stream/1` — end-of-stream testing
- `get_byte/1`, `get_byte/2`, `put_byte/1`, `put_byte/2` — binary I/O
- `peek_byte/1`, `peek_byte/2` — non-consuming byte lookahead
- `write_canonical/1`, `write_canonical/2` — canonical term output
- `char_conversion/2`, `current_char_conversion/2` — character conversion table

### Module System (CR-0002 completed)
- `module/2` directive for module declarations with export lists
- `use_module/1` for importing modules
- Module-qualified calls `Module:Goal` via `solveInModuleContext`
- Module-isolated rule storage: non-user module rules stored in module's `localRules`, user module rules in global `KnowledgeBase`
- Unqualified call resolution: current module context → global KB → imported modules
- Full backward compatibility maintained

### List Operations (CR-0008 completed)
- `permutation/2` — generates list permutations
- `predsort/3` — sort with user-defined comparison predicate (merge sort, BuiltInWithContext)

### Issue Triage (10 issues closed)
- ISS-0040 through ISS-0049 resolved or confirmed already implemented
- `unify_with_occurs_check/2`, `clause/2`, `current_op/3` confirmed existing

### Quality Metrics
- **320 unit tests, 0 failures** (up from 311 with 6 failures)
- **20/20 example programs pass** (100%, up from 95%)
- **94/94 MegaPredicateTest** passes
- **0 active limitations** (down from 3)
- **9/10 Change Requests completed** (only CR-0009 Debugging Port Model remains)

### Repository Information
- **Tag**: v2.2.0
- **Release Date**: 2026-03-18
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 2.0.6 - 2025-08-20

### 🚀 Major Enhancements
- Fixed critical StackOverflowError in Variable.resolveBindings() with cycle detection
- Enhanced string handling in DCG parsing with string_codes/2 predicate  
- Comprehensive documentation reorganization with structured naming conventions
- Improved system stability and DCG processing reliability

### 🔧 Technical Fixes
- Fixed Variable.resolveBindings circular reference causing system crashes (ISS-2025-0008)
- Implemented string_codes/2 predicate for PrologString support (ISS-2025-0006)
- Enhanced to_codes/2 to handle both Atom and PrologString types
- Resolved StackOverflowError in Variable.occurs() method (ISS-2025-0012)
- Updated phrase/2 predicate for better DCG variable handling

### 📊 Quality Metrics
- Comprehensive tests: 19/20 programs passed (95% success rate)
- No regressions introduced by stability fixes
- System now processes DCG rules without crashes
- Enhanced error handling and variable resolution

### 🎯 Impact
- Eliminated critical system crashes in Variable processing
- Improved DCG system stability and reliability
- Better string/atom type handling in grammar parsing
- Structured documentation for improved maintainability

### 📝 Documentation Updates
- Enhanced CLAUDE.md with comprehensive pre-push preparation procedures
- Reorganized documentation with categorical naming conventions (docs/guides/, docs/references/, etc.)
- Updated all file references to new structured paths

---

## Release v2.0.5 - 2025-08-20

### 🚀 Major Enhancements
- Enhanced list representation with ISO-compliant formatting [a,b,c] instead of .(a, .(b, .(c, [])))
- Meta-predicates (findall/3, bagof/3, setof/3) verified fully functional
- Term manipulation predicates (functor/3, arg/3, =../2, copy_term/2) working correctly
- Advanced arithmetic operators (=:=, =\=, rem, xor, shift operators) operational
- Control structures (;, ->, \+, once/1) fully functional
- DCG (Definite Clause Grammar) system fully operational with phrase/2

### 🔧 Technical Fixes
- Fixed copy_term/2 predicate registration in BuiltInRegistry (ISS-2025-0025)
- Resolved list format issues for improved ISO compliance (ISS-2025-0019)
- Enhanced CompoundTerm.toString() with proper list formatting
- Updated comprehensive documentation and issue tracking

### 📊 Quality Metrics
- Comprehensive tests passed with 95% success rate (19/20 programs)
- ISO Prolog compliance significantly improved from 47.6% to 95%
- Built-in coverage increased from ~50% to ~90%
- Parser support enhanced from ~60% to ~85%

### 🎯 Impact
- Dramatically improved ISO Prolog standard compliance
- Enhanced developer experience with better list representation
- Robust meta-programming capabilities now available
- Comprehensive term manipulation for advanced Prolog programming

### 📝 Documentation Updates
- Updated README.md with complete project overview
- Enhanced CLAUDE.md with release procedures and mandatory release notes
- Updated issues.md with resolved issues and quality metrics
- Created comprehensive built-in predicates reference

### 🔗 Repository Information
- **Repository**: https://github.com/DenzoSOFTHub/JProlog
- **Tag**: v2.0.5
- **Release Date**: 2025-08-20
- **Compatibility**: Java 8+, Maven 3.6+

---

*For previous releases and detailed changelogs, see CHANGELOG.md*