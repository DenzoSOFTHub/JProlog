# JProlog - Complete Prolog Implementation in Java

**A Full-Featured Prolog System with Engine, IDE, CLI, and Comprehensive Built-ins**

[![Version](https://img.shields.io/badge/version-4.6.0-blue.svg)](https://github.com/DenzoSOFTHub/JProlog/releases/tag/v4.6.0) [![Java](https://img.shields.io/badge/java-1.8%2B-orange.svg)]() [![ISO](https://img.shields.io/badge/ISO%2013211--1-100%25%20core-green.svg)]()

**Current version**: `4.6.0` — see [CHANGELOG](CHANGELOG.md) and [Releases](https://github.com/DenzoSOFTHub/JProlog/releases).

### 🆕 New in 4.6.0: **completeness**

Seven waves (Q1..Q7) closed what 4.5.0 left open — details and upgrade notes in the
[CHANGELOG](CHANGELOG.md), deliberate differences in
[`ref-deviations.md`](docs/references/ref-deviations.md) §4a–§4e.

- **ISO errors everywhere**: every extended library (jdbc, http, crypto, xml, …) and the Java FFI
  raise `error(Formal, context(Name/Arity, Message))` — a 3 942-goal probe finds no message atom.
- **SWI libraries**: `library(solution_sequences)` (`limit/2`, `offset/2`, `order_by/2`,
  `distinct/1,2`, `call_nth/2`), rationals (`1r3`, `rdiv`), `aggregate/3,4`, `library(ordsets)`,
  `append/2`, `nextto/3`, `max_member/2`, `list_to_set/2`, the recorded database, `flag/3`,
  `message_hook/3`.
- **Loading and modules**: `multifile/1` and per-file clause ownership, `goal_expansion/2`,
  `file_search_path/2` + `absolute_file_name/3`, `use_module/2` import lists, `(a|b)` and `as`
  as in SWI, `subterm_positions/1`, a per-file load lock with deadlock detection.
- **Threads and tabling**: `thread_signal/2`, thread pools, private table spaces per worker
  (`as shared` to share), `lattice`/`po` moded tabling, `tnot/1` with a minimal well-founded
  semantics; the working directory is per engine.
- **CLP(FD)**: SWI-style residual constraints in answers, `circuit/1`, `cumulative/1,2`,
  `automaton/3`, big coefficients, iterative branch and bound.
- **Faster**: `call/N` −43 %, native maplist/foldl/include/exclude/partition, native stream
  built-ins, CLP(FD) change queues (20 000-variable models in 0.1 s).

### 🆕 New in 4.5.0: **production readiness**

Seven waves driven by a 1 300-probe audit of 4.4.0 (ISO 13211-1 first, SWI-Prolog 9 where ISO is
silent). Every deliberate difference from ISO/SWI is listed in
[`docs/references/ref-deviations.md`](docs/references/ref-deviations.md); upgrade notes are in the
[CHANGELOG](CHANGELOG.md).

- **A runnable jar and a real command line**: `mvn package` builds `target/jprolog.jar`;
  `java -jar target/jprolog.jar prog.pl -g main -t halt`, with `--safe`, `--budget N`,
  `--max-solutions N`, `--demo`, exit codes from `halt/1`, and answers **streamed** one at a time
  (`between(1, inf, X).` no longer exhausts memory).
- **Faster**: nrev/loop −59..71 %, O(1) `asserta/assertz/retract`, linear `retractall/1`,
  O(n log n) `bagof/setof`, `.jpc` format 0x04.
- **Loading and I/O**: `consult/1`, `[F]`, `ensure_loaded/1`, `load_files/1,2`, `include/1`,
  `make/0`, string streams, `read_term/2,3` on the v2 reader, `term_expansion/2`, SWI `listing/1`
  layout and float printing.
- **CLP(FD)**: domains with holes, lazy labeling with the SWI options and branch and bound,
  `ins/2`, `sum/3`, `scalar_product/4`, reification, `element/3`, `tuples_in/2`,
  `global_cardinality/2`, domain-consistent `all_distinct/1`, big integers.
- **Hardening**: safe mode covers the native table, file sinks and `halt/0,1`
  (`SafeModeOptions.allowFileRead(dir)`, `allowHalt()`); one inference budget per query shared
  with its threads; SWI threads (mutexes, selective receive, `thread_property/2`); ISO error terms
  from the thread and concurrent families.
- **Semantics**: answers are copies; `bagof/setof`, `between/3`, `aggregate_all/3`, cleanup,
  catch/cut interactions fixed; `op/3` permanent; many argument faults now raise ISO errors.

### 🆕 New in 4.1.0: **one engine**

- **The v2 machine is deleted.** `core.engine.v2.MachineSolver` — the default from 3.1.0 to 3.14.0
  and the one-release fallback of 4.0.0 — is gone, and with it `-Djprolog.engine=v2`, the
  `engine-v2` Maven profile, the four static engine-selection accessors, the v2 tabling driver, the
  legacy `freeze/when/dif` and attributed-variable built-ins (v4 has natives and prelude clauses
  for all of them) and the legacy `Variable.AttributeUnifyHook`. **2 825 lines and 6 classes gone**;
  `mvn test` is now the whole story. `-Djprolog.engine=<anything>` logs a warning and runs v4.
  (`core.parser.v2`, `core.dcg.v2`, `builtin.clpfd.v2`, `core.arith.v2` and `core.write.v2` are
  unrelated — they are the current parser, DCG translator, CLP(FD) solver, arithmetic evaluator and
  IDE formatter, not engines.)
- **One trail.** The process-wide `core.engine.Trail`, a second undo stack every choice point had to
  mark and roll back in parallel with the real one, is replaced by `core.engine.v4.Undo`: a bridged
  built-in (`b_setval/2`, `op/3`, `setarg/3`, the CLP(FD) store) now pushes its undo action onto the
  running machine's own `Bindings` trail, where it is run at exactly the point the cells are reset.
- **A faster goal path.** The module test that decided "clauses or built-in?" ran on every goal,
  including plain user predicates that have no built-in to override. It is now asked only when there
  IS a registry entry, and memoised behind the module stamp: `nrev` ~10 % faster, a fact-table
  lookup loop ~7 %, `loop(1000000)` ~9 % (medians, same session).
- **An idle debugger is free.** `DebugController.needsPorts()` — a controller with no listener, no
  breakpoint, in CONTINUE mode and with no Stop pending observes nothing, so the machine emits no
  ports for it at all: on `loop(1000000)` attaching one cost 1.8-2.3x before and ~1x now.
- **`thread_self/1` answers SWI-style**: the top-level thread reports `main` and an aliased worker
  reports its alias (an anonymous worker still reports its integer id). The `main` alias also
  follows the live top-level thread instead of pointing at a dead one for the life of the JVM.

### 🆕 New in 3.9.0 – 4.0.0: **engine v4 is the default, and the old one is gone**

- **Engine v4 (`core.engine.v4`) replaces the v2 machine as the default resolution engine in 4.0.0**
  — nine months of design in `docs/reports/report-engine-v4-design-2026-08-25.md`, built over nine
  waves (ISS-2025-0438..0488) and shipped only once the whole suite was green on it. Variables are
  mutable cells with a trail, clauses are compiled skeletons, every term walker is iterative and
  cycle-safe, and the clause store keeps birth/death generations. What you get:
  **`nrev` ~2 MLIPS (v2: ~316 KLIPS)**, `loop(10000000)` in 64 MB where v2 runs out of memory,
  a user's own list predicate over 1 M elements 6x faster, rational trees (`X = f(X)`) that succeed
  and print, **correct tabling** (left recursion over a 100 000-edge chain in 868 ms — v2 answers
  *wrong* in 40 ms), a real coroutining wake queue (`when/2` bindings propagate), a proper module
  system with autoloaded Prolog library modules and `meta_predicate/1`, per-engine streams and
  operators, and an ISO term writer behind the whole write family.
  (The previous engine stayed selectable for one release, through 4.0.0; it is deleted in 4.1.0.)
- **The recursive engine is deleted (4.0.0, wave W9)** — the original `QuerySolver` that JProlog ran
  on until 3.0.0, together with `CutStatus`, the binding map, the built-in adapter and the seven
  Java control constructs the newer machines implement natively: 1 434 lines and 13 classes gone.
  `BuiltInWithContext` now takes a small `SolverContext` interface instead of a solver class, so an
  embedded built-in can no longer accidentally run on a recursive sub-solver.
  Removed with it: `-Djprolog.engine=legacy`, the `engine-legacy` Maven profile,
  `Prolog.solveLegacy` and `Prolog.getQuerySolver` (use `Prolog.getEngineContext()`).
- **Tabling is thread-safe, and a worker can answer its creator (4.0.0)**: several workers of one
  query may call the same tabled predicate — the first produces the table, the rest read it
  complete (they used to interleave and some saw half a table). `thread_send_message(main, T)` and
  `thread_get_message(T)` from the main thread work, as in SWI.
- **Threads run real goals on real machines (4.0.0)**: `thread_create/2,3` used to start a thread
  that slept 10 ms and never ran the goal. Each worker now gets its own machine over the same
  engine — shared clause store, shared flags, its own streams and inference budget, and a copied
  goal so no variable is shared across threads. `concurrent_maplist/3,4` are callable at all for
  the first time, message queues carry terms, and interrupting the parent cancels the workers.
- **Debugging and tracing that you can leave switched on (4.0.0)**: attaching a debugger no longer
  reroutes `=/2`, `is/2`, the comparisons and the type checks through a slower bridge — the engine
  keeps its fast paths and reports the four ports itself, so a running debugger costs nothing
  measurable (5.8x -> 1.1x on `nrev`). Textual tracing stopped being quadratic: `loop(1000000)`
  under `trace/0` finishes in 4.6 s instead of not finishing at all, and `nrev` went from 567x
  slower than untraced to ~10-18x.
- **A piped console no longer eats your queries (4.0.0)**: with stdin redirected the CLI used to
  read the *next query* as the answer to its "more solutions?" prompt. It now prints every solution
  at once when stdin is not a terminal, or with `--batch` / `-q`.

### 🆕 New in 3.0.0 – 3.8.0

- **Engine hardening waves (3.7.0 / 3.8.0)**: a deep engine analysis (`docs/reports/report-engine-deep-analysis-2026-08-24.md`) drove 15 fixes (ISS-2025-0423..0437): lists of **1,000,000 elements** work at the default JVM stack (all term walkers are tail-iterative), deterministic recursion runs in bounded memory (choice points popped when exhausted, trail released), built-in calls no longer copy the whole binding map, **`between/3`, `repeat/0` and `length/2` are lazy generators**, the **inference budget and Stop are enforced inside `once/ignore/forall/aggregate_all/bagof/setof/setup_call_cleanup`** and on the legacy engine (`ResourceGuard`), first-argument indexing is back (20 000-fact lookups ~2000× faster), arithmetic runs on primitive `long` fast paths, and ISO flags / `trace/0` / `current_output` are **isolated per engine and per thread**. Correctness: `repeat/0` past 1000 iterations, floats no longer collapse to integers (`sum_list([1.5,1.5],S)` gives `3.0`), Java failures inside built-ins surface as `system_error`. Baseline **989/989 tests, 20/20 example programs**; `nrev` 188 → ~440 KLIPS.
- **Reference Manual (PDF)**: `docs/guides/guide-builtin-manual.pdf` documents every default operator and built-in predicate with verified examples (regenerate with `tools/build-manual.sh`).

- **ISO-conformance audit waves (3.5.0 / 3.6.0)**: a multi-agent empirical audit ran ISO-conformance queries against the build; **80 confirmed defects fixed** (ISS-2025-0342..0422) — cut/catch semantics, ISO error terms for undefined procedures and bad arguments, ISO end-token `read/1`, strict `format/2,3`, ISO-correct `writeq`/float output, `setof/3` ordering, re-executable `retract/1`, list-predicate modes, `phrase/2,3` full body translation, CLP(FD) soundness, and more.
- **Production hardening (3.4.0)**: a **sandbox** — `Prolog.enableSafeMode()` removes all host-touching built-ins (OS shell, Java FFI, filesystem, network, HTTP, JDBC, persistence) — and a **CPU budget** — `Prolog.setInferenceBudget(steps)` aborts runaway queries with an uncatchable `InferenceLimitException`. Deep terms and deeply nested input now raise a catchable `resource_error` instead of crashing. Correctness fixes: `sort`/`msort`/`sort_4` on lists with variables, `freeze/2` binding propagation, ISO errors for `call/1` & `=../2`. (Driven by a multi-agent production-readiness audit — `docs/reports/report-production-readiness-audit-2026-06-09.md`.)
- **Tracing & full v2 debugging (3.3.0)**: `trace/0` / `notrace/0` now emit a real **four-port trace** (Call/Exit/Fail/Redo) through the default v2 engine — in both the CLI (`:trace`) and the IDE (Trace toggle). The **interactive debugger runs on the v2 engine** and traces **built-ins too** (`is`, `=`, …), with **conditional & hit-count breakpoints**. (Also fixed: core `Debug*.java`/`DebugController` were accidentally git-ignored and missing from the published source.)
- **Major IDE upgrade (3.2.0)**: undo/redo, **source formatter** (`Ctrl+Alt+L`), **code completion** (`Ctrl+Space`), debounced syntax highlighting, bracket matching/auto-close, **working Stop** (cancellable queries), lazy streaming + a results **table view**, clickable **Problems view**, **Compile to .jpc**, **line-accurate breakpoints** with persistence, real stepping shortcuts (F7/F8/Shift+F8/F9), an expandable **Variables tree**, **Watch expressions**, Run-to-Cursor/Restart, settings/session persistence, Go-to-Line/Quick-Open. (Driven by a multi-agent IDE audit — `docs/reports/report-ide-ux-analysis-2026-06-09.md`.)
- **Clean-room rewrites, now default**: the parser (`core.parser.v2`), CLP(FD) solver (`builtin.clpfd.v2`), DCG translator (`core.dcg.v2`) and — as of **3.1.0** — the **resolution engine** (`core.engine.v2.MachineSolver`, superseded by `core.engine.v4` in 4.0.0 and deleted in 4.1.0) were new from-scratch implementations and became the defaults. The parser/CLP(FD)/DCG legacy paths remain available via a system property (see *Engine/Parser Toggles* below).
- **Sound CLP(FD)**: interval-domain solver (no `OutOfMemoryError` on wide domains), per-query trail-backtracked store, sound first-fail labeling, real `#\=` propagation; constraints `Cmp`/`Sum`/`Mul`/`Abs`/`AllDifferent`/`Linear`/`Reified`/`Mod`.
- **Complete DCG**: single-pass ISO translator covering head push-back, `|`, `\+`, `call//N`, `{}`, `!`, `->` (resolves the former ~85% DCG limitation).
- **New standalone modules**: operator-aware term writer (`core.write.v2`) and a single-path arithmetic evaluator (`core.arith.v2`, BigInteger/double, ISO error terms).
- **Clean-room v2 resolution engine, the default in 3.1.0** (`core.engine.v2.MachineSolver`): iterative SLD — deep recursion with **no `StackOverflowError`** — mutable bindings + trail, lazy enumeration, coroutining (`freeze`/`when`/`dif`), tabling, `setarg/3`. Passed the full suite of its day (935/935 + 20/20 examples). *Superseded by engine v4 in 4.0.0 and deleted in 4.1.0.*
- **New built-ins/operators**: `setup_call_cleanup/3`, `call_cleanup/2`; `div`, `rdiv` (400 yfx) operators.
- **~50 ISO/correctness fixes** in v3.0.0 plus the 2026-06-10 ISO-conformance audit waves (**80 defects fixed** across v3.5.0/v3.6.0), all driven by adversarial multi-agent reviews. **935/935 JUnit tests, 20/20 example programs.**

## Overview

JProlog is a complete and robust Prolog implementation in Java that provides a comprehensive ecosystem for Prolog programming. It consists of multiple integrated components designed to offer both programmatic access and interactive development environments for Prolog applications.

### 🎯 Project Scope

JProlog aims to provide ISO-compliant Prolog functionality with modern development tools, offering:
- **Full ISO Compliance**: 100% ISO 13211-1 core predicate coverage (111/111 predicates)
- **Clean-room v2 Parser (default)**: single-pass `Lexer` + operator-precedence/Pratt `TermReader` (`core.parser.v2`) with shared OperatorTable and dynamic `op/3` support. Correctly handles canonical functor `-(1,2)`, operators-as-atoms (`X = -`, `foo(-,+)`), postfix operators, `0'c`/radix/negative numeric literals, `''`/`""` doubled-quote escapes, full ISO §6.4.2.1 escape sequences, and quote-aware clause splitting. Parses 123/130 example programs (legacy parser: 117). Fall back to the legacy parser with `-Djprolog.parser=legacy`.
- **Operator-Aware Output**: `write/1`, `writeq/1`, `format ~w/~q` use operator notation (e.g. `1+2`, `[a,b,c]`, `{a,b}`) via shared OperatorTable
- **Binary Compiled Format**: `.jpc` (JProlog Compiled) format with string interning for fast loading
- **Professional Development Environment**: Full-featured IDE with debugging capabilities
- **Command Line Interface**: Interactive Prolog console for quick testing and scripting
- **Comprehensive Built-in Library**: 280+ built-in predicates including higher-order list operations, soft-cut `*->`, `maplist/2..5`, `sort/4`, `read/2`, `atom_to_term/3`, `atomic_list_concat/2,3`, and `setup_call_cleanup/3`, `call_cleanup/2`
- **Full ISO §9 arithmetic**: `gcd`, `^/2` integer power, `integer/1` evaluable, hyperbolic functions (sinh/cosh/tanh + arc/h), `log/2` base-N, `cot`/`acot`/`cbrt`, `epsilon` constant, rational numbers, BigInteger auto-promotion
- **Complete DCG Support (default)**: clean-room single-pass ISO translator (`core.dcg.v2`) handling head push-back, `|`, `\+`, `call//N`, `{}`, `!`, and `->`. Fall back to the legacy transformer with `-Djprolog.dcg=legacy`.
- **Embedding safety (v3.4.0+)**: `Prolog.enableSafeMode()` sandbox (removes all host-touching built-ins) and `Prolog.setInferenceBudget(steps)` CPU budget for runaway queries
- **Java Integration**: Easy embedding of Prolog logic in Java applications

## 🏗️ System Architecture

JProlog consists of four main components that work together to provide a complete Prolog development and execution environment:

### 1. 🧠 Prolog Engine (`core/engine/`)

The heart of JProlog - a complete Prolog inference engine with:
- **ISO-compliant query resolution**: Robinson unification algorithm with proper backtracking
- **Knowledge base management**: Dynamic fact/rule storage and retrieval system
- **Unified operator system**: Shared `OperatorTable` between parser, `op/3`, and query resolution
- **Binary compiled format**: `.jpc` files with string interning, varint encoding, and source hash validation
- **DCG (Definite Clause Grammar) support**: clean-room single-pass ISO translator (`core.dcg.v2`, default) covering head push-back, `|`, `\+`, `call//N`, `{}`, `!`, `->`; legacy transformer available via `-Djprolog.dcg=legacy`
- **Built-in predicate registry**: Extensible system for registering new predicates
- **Exception handling**: Comprehensive error management with ISO-compliant error terms
- **Module system**: Namespace management with module-qualified calls (`Module:Goal`)

**Key Classes**: `Prolog.java`, `core/engine/v4/Machine.java`, `core/engine/v4/Unify.java`, `core/engine/v4/ClauseStore.java`, `KnowledgeBase.java`, `EngineContext.java`, `core/dcg/v2/DCGTranslator.java`, `JpcWriter.java`, `JpcReader.java`

### 2. 🖥️ Integrated Development Environment (`editor/`)

A professional IDE specifically designed for Prolog development:
- **Project Management**: Structured project organization with directory trees
- **Syntax Highlighting**: Full Prolog syntax highlighting with keywords, operators, and comments
- **Advanced Editor**: Multi-tab interface with auto-indentation, bracket matching, and line numbering
- **Interactive Debugger**: Full ISO four-port debug model (CALL/EXIT/FAIL/REDO) with Step Into/Over/Out, breakpoints, call stack inspection, and variable monitoring
- **Breakpoint Gutter**: Click line numbers to toggle breakpoints with visual red circle markers
- **Build System**: Per-clause compilation diagnostics with inline error highlighting and line-level reporting
- **Query Console**: Interactive Prolog execution environment within the IDE
- **Debug Query Mode**: Run queries with step-by-step execution and colored trace output
- **Search & Navigation**: Find/replace with regex support and project-wide search

**Key Classes**: `PrologIDE.java`, `FileEditor.java`, `DebugPanel.java`, `DebugController.java`, `ProjectTree.java`

### 3. 📟 Command Line Interface (`PrologCLI.java`)

An interactive Prolog console for quick testing and scripting (runnable as `java -jar
target/jprolog.jar` since 4.5.0):
- **Command line**: `jprolog [options] [file.pl ...]` — consults the files, then `-g Goal` runs a
  goal once, `-t Goal` replaces the interactive toplevel; `--safe` (sandbox), `--budget N`
  (inference budget per query), `--max-solutions N`, `--demo` (the demo facts `father/2`,
  `likes/2`, … — no longer loaded by default), `--batch`, `--interactive`, `-q`, `-h`.
  Exit status: `halt(N)` → N; a failing or raising `-g` goal → 1; a bad option → 2.
- **Streamed answers**: one answer is computed at a time (`;`/Enter interactively); with a piped
  stdin every answer is printed as it is found, separated by ` ;` and ended by `.` or `false.`
- **Interactive query execution**: Direct Prolog query input with immediate results
- **File consultation**: Load Prolog files with proper DCG transformation
- **Multiple solutions handling**: Backtracking through solutions with `;` operator
- **Built-in commands**: `:consult`, `:listing`, `:save`, `:clear`, `:compile`, `:consult_compiled`, etc.
- **Binary compilation**: Compile `.pl` files to `.jpc` for faster loading
- **History and shortcuts**: Navigate previous queries and use command shortcuts
- **ISO-compliant output**: List representation in standard `[a,b,c]` format

### 4. 🔧 Built-in Predicates Library (`builtin/`)

Comprehensive library of standard Prolog predicates organized by category:

#### 🧮 **Arithmetic Operations** (`builtin/arithmetic/`)
- **Comparison operators**: `=:=`, `=\\=`, `>`, `<`, `>=`, `=<`
- **Advanced arithmetic**: `rem`, `div`, `mod`, `abs`, `sign`, `min`, `max`
- **Bitwise operations**: `xor`, `<<`, `>>`, `/\\`, `\\/`
- **Mathematical functions**: `sqrt`, `sin`, `cos`, `exp`, `log`, etc.

#### 📝 **Term Manipulation** (`builtin/term/`)
- **Structure analysis**: `functor/3`, `arg/3`, `=../2` (univ), `copy_term/2`
- **Term comparison**: `@<`, `@>`, `@=<`, `@>=`, `==`, `\\==`
- **Type checking**: `var/1`, `nonvar/1`, `atom/1`, `compound/1`, `number/1`

#### 📋 **List Processing** (`builtin/list/`)
- **Core operations**: `append/3`, `member/2`, `length/2`, `reverse/2`
- **List manipulation**: `select/3`, `nth0/3`, `nth1/3`
- **Sorting**: `sort/2`, `msort/2`, `keysort/2`

#### 🔀 **Control Structures** (`builtin/control/`)
- **Conditional execution**: `->` (if-then), `;` (or), `\\+` (not)
- **Meta-predicates**: `findall/3`, `bagof/3`, `setof/3`, `once/1`
- **Cut and unification**: `!` (cut), `=/2` (unify), `unify_with_occurs_check/2`

#### 🔤 **Atom Operations** (`builtin/atom/`)
- **Atom manipulation**: `atom_length/2`, `atom_concat/3`, `sub_atom/5`
- **String operations**: `atom_chars/2`, `atom_codes/2`, `atom_string/2`

#### 💾 **Database Operations** (`builtin/database/`)
- **Dynamic predicates**: `assert/1`, `asserta/1`, `assertz/1`, `retract/1`, `retractall/1`
- **Meta-database**: `abolish/1`, `current_predicate/1`, `listing/0`, `listing/1`

#### 📂 **I/O Operations** (`builtin/io/`)
- **Basic I/O**: `read/1`, `write/1`, `writeln/1`, `nl/0`
- **Stream operations**: `open/3`, `close/1`, `current_input/1`, `current_output/1`
- **Character I/O**: `get_char/1`, `put_char/1`, `get_code/1`, `put_code/1`

#### 🧬 **DCG Support** (`builtin/dcg/`)
- **Core DCG Functionality**: Standard `phrase/2` predicate for grammar parsing
- **Automatic DCG Transformation**: `-->` rules converted to standard Prolog predicates
- **Complete DCG Translation**: clean-room single-pass ISO translator (`core.dcg.v2`) passes the comprehensive 20-program DCG suite
- **Working Features**: Terminal/non-terminal symbols, variables, recursion, basic constraints
- **Full Construct Coverage**: head push-back, `|`, `\+`, `call//N`, `{}`, `!`, `->`
- **Systematic Testing**: Complete test suite with `test_dcg_01` through `test_dcg_20` programs

## 🚀 Quick Start

### Prerequisites
- **Java**: Java 8 or higher (Java 11+ recommended)
- **Maven**: For building from source (3.6+ recommended)
- **Memory**: 512MB RAM minimum (1GB recommended for IDE)

### Installation & Launch

1. **Build the project**:
   ```bash
   mvn clean compile          # classes only
   mvn package -DskipTests    # also target/jprolog.jar (runnable)
   ```

2. **Launch options**:

   **🖥️ IDE (Recommended for development)**:
   ```bash
   java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
   # or use the script:
   ./start-ide.sh
   ```

   **📟 CLI (Quick testing and scripting)**:
   ```bash
   java -jar target/jprolog.jar                       # interactive toplevel
   java -jar target/jprolog.jar facts.pl -g "grandparent(tom, X), write(X), nl" -t halt
   echo "member(X, [a,b])." | java -jar target/jprolog.jar -q
   java -cp target/classes it.denzosoft.jprolog.PrologCLI   # without the jar
   ```

   **☕ Java API (Programmatic access)**:
   ```java
   import it.denzosoft.jprolog.core.engine.Prolog;

   Prolog prolog = new Prolog();
   List<Map<String, Term>> solutions = prolog.solve("factorial(5, X)");

   // Compile to binary for faster loading next time
   prolog.compileFile("my_program.pl");

   // Smart consult: auto-uses .jpc cache when available
   prolog.consultSmart("my_program.pl");
   ```

### Simple Example

```prolog
% facts.pl - Basic family relationships
father(tom, bob).
father(bob, ann).
mother(ann, sue).

% Rules for family relationships  
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% Queries to try:
% ?- parent(bob, X).        % Who is bob parent of?
% ?- grandparent(tom, X).   % Who is tom grandparent of?  
% ?- findall(X, parent(X, _), Parents). % Find all parents
```

**Test with CLI**:
```bash
$ java -jar target/jprolog.jar
?- consult('facts.pl').
?- parent(bob, X).
X = ann.
?- grandparent(tom, X).
X = sue.
```

**Test with IDE**:
1. Launch IDE, create new project
2. Create `facts.pl` with above content
3. Compile project (Ctrl+F9)  
4. In Run tab: `?- grandparent(tom, X).`
5. Result: `X = sue`

## 📚 Documentation

JProlog provides comprehensive documentation for all aspects of the system:

### 🚀 **User Guides**
- **[docs/guides/guide-quick-start.md](docs/guides/guide-quick-start.md)**: 5-minute setup guide for immediate productivity
- **[docs/guides/guide-user-manual.md](docs/guides/guide-user-manual.md)**: Complete user guide for all components
- [Reference Manual — built-in predicates and operators](docs/guides/guide-builtin-manual.md) — every default predicate and operator with verified examples; also as [PDF](docs/guides/guide-builtin-manual.pdf)
- **[docs/guides/guide-cli-usage.md](docs/guides/guide-cli-usage.md)**: Dedicated command-line interface guide
- **[docs/guides/guide-ide-usage.md](docs/guides/guide-ide-usage.md)**: Integrated Development Environment usage guide
- **[docs/guides/guide-prolog-intro.md](docs/guides/guide-prolog-intro.md)**: Introduction to Prolog programming

### 🔧 **Development & Technical**  
- **[CLAUDE.md](CLAUDE.md)**: Developer guide with build commands, architecture, and workflow procedures
- **[docs/guides/guide-java-integration.md](docs/guides/guide-java-integration.md)**: Guide for embedding JProlog in Java applications
- **[docs/guides/guide-extension.md](docs/guides/guide-extension.md)**: How to create custom built-in predicates
- **[docs/guides/guide-debugging.md](docs/guides/guide-debugging.md)**: Comprehensive debugging tutorial with examples

### 📖 **Reference Documentation**
- **[BUILTIN_PREDICATES_REFERENCE.md](docs/references/BUILTIN_PREDICATES_REFERENCE.md)**: Comprehensive guide to all built-in predicates organized by functional groups
- **[BUILTIN_OPERATORS_REFERENCE.md](docs/references/BUILTIN_OPERATORS_REFERENCE.md)**: Complete reference for operators with precedence rules and examples
- **[ref-builtins-legacy.md](docs/references/ref-builtins-legacy.md)**: Original alphabetical built-ins reference (legacy)
- **[docs/references/ref-iso-compliance.md](docs/references/ref-iso-compliance.md)**: ISO Prolog standard compliance analysis
- **[docs/references/ref-limitations.md](docs/references/ref-limitations.md)**: Known limitations and workarounds
- **[docs/references/ref-dcg-grammar.md](docs/references/ref-dcg-grammar.md)**: Definite Clause Grammars reference

### 📝 **Project Management & Tracking**
- **[docs/tracking/track-issues.md](docs/tracking/track-issues.md)**: Issue tracking and resolution documentation
- **[docs/tracking/track-change-requests.md](docs/tracking/track-change-requests.md)**: Change requests and enhancement tracking
- **[docs/tracking/track-release-notes.md](docs/tracking/track-release-notes.md)**: Latest release notes
- **[CHANGELOG.md](CHANGELOG.md)**: Complete version history

### 🧪 **Reports & Analysis**
- **[docs/reports/report-test-results.md](docs/reports/report-test-results.md)**: Comprehensive test results and coverage analysis
- **[docs/reports/report-debug-features.md](docs/reports/report-debug-features.md)**: Overview of debugging capabilities
- **[docs/reports/report-package-reorg.md](docs/reports/report-package-reorg.md)**: Package reorganization documentation
- **[docs/reports/report-resolution-summary.md](docs/reports/report-resolution-summary.md)**: Summary of resolved issues and improvements

### 💡 **Examples & Tutorials**
- **[examples/](examples/)**: 130 example Prolog programs covering all language features (the default v2 parser parses 123/130)
- **[examples/example-bug-workflow.md](examples/example-bug-workflow.md)**: Step-by-step bug fixing examples  
- **[examples/example-nqueens-compilation.md](examples/example-nqueens-compilation.md)**: N-Queens problem compilation guide

## 📊 **Quality Metrics & Compliance**

### 🎯 **Current Status (Version 4.6.0)**
- **Unit Tests**: 1547 tests, 0 failures, 0 errors — one engine, one CI leg (`mvn test`), including the growth tests of `test/performance/PerformanceRegressionTest` and real programs (92 queens, zebra, SEND+MORE, the 6×6 knight's tour, a tabled shortest path) in `FamousPrologProgramsTest`.
- **Deliberate deviations** from ISO/SWI: one list, [`docs/references/ref-deviations.md`](docs/references/ref-deviations.md).
- **Core Test Success Rate**: 100% (20/20 example programs pass)
- **ISO Prolog Compliance**: 100% ISO 13211-1 core predicate coverage (111/111); ~50 ISO/correctness fixes in v3.0.0 plus 80 audit-confirmed conformance defects fixed across v3.5.0/v3.6.0
- **Built-in Predicate Coverage**: 443 registered predicate names including I/O, CLP(FD), FFI, crypto, networking — 264 indicators (220 names) native to the v4 engine, more handled inline by the machine or by the prelude library modules (lists, apply, pairs, ordsets, coroutining, clpfd), 237 names (the extended libraries, the debug and profiler predicates) on the built-in bridge
- **Resolution Engine**: clean-room **v4** core (`core.engine.v4`, default since 4.0.0, **the only engine since 4.1.0**) — mutable variable cells + trail, compiled clause skeletons, first-argument indexing, iterative cycle-safe walkers, linear tabling with completion, native coroutining, per-engine modules/streams/operators
- **Parser**: Clean-room single-pass v2 parser (`core.parser.v2`, default) with unified operator table and dynamic `op/3` support; legacy parser via `-Djprolog.parser=legacy`
- **Binary Format**: `.jpc` compiled format with string interning for fast loading
- **DCG Support**: complete (clean-room v2 translator passes the comprehensive 20/20 DCG program suite)
- **Module Support**: Module framework with module-qualified calls (`Module:Goal`) wired into execution

### 🧪 **Testing Framework**
- **1547 Unit Tests**: JUnit test suite covering all engine components (incl. `EngineHardeningTest`, the `EngineV4*Test` wave suites, the `EngineV45*Test` / `ClpfdV45Test` / `PrologCliToplevelTest` classes of 4.5.0 and the `EngineV46*Test` / `ClpfdV46Test` / `ExtendedLibraryErrorsTest` / `DocumentedPredicatesTest` classes of 4.6.0)
- **130 Total Prolog Programs**: Comprehensive test suite in `examples/` directory
- **74 Systematic Test Programs**: `test_*.pl` programs covering all language features
- **20 DCG Test Programs**: Complete DCG testing from `test_dcg_01` to `test_dcg_20`
- **Automated Testing**: `./test_all_examples.sh` for continuous validation
- **JPC Format Tests**: Round-trip serialization/deserialization verification

### ✅ **Verified Features**
- ✅ **Meta-predicates**: `findall/3`, `bagof/3`, `setof/3` fully functional (confirmed v2.0.14)
- ✅ **Term manipulation**: `functor/3`, `arg/3`, `=../2`, `copy_term/2` (confirmed v2.0.14)
- ✅ **Arithmetic operations**: `=:=`, `=\=`, `rem`, `xor`, shift operators (confirmed v2.0.14)
- ✅ **Control structures**: `;`, `->`, `\+`, `once/1`, `!` (cut) (confirmed v2.0.14)
- ✅ **List processing**: ISO-compliant `[a,b,c]` representation
- ✅ **DCG Core Functionality**: complete (clean-room v2 ISO translator passes the comprehensive DCG suite)
- ✅ **DCG Working Patterns**: Terminal/non-terminal symbols, variables, recursion, constraints
- ✅ **Database operations**: Dynamic assert/retract predicates (confirmed v2.0.14)
- ✅ **I/O operations**: File and stream handling
- ✅ **Atom operations**: `atom_length/2`, `atom_concat/3` (confirmed v2.0.14)

## ⚠️ **Current Limitations**

The following features are not yet implemented or are partially supported compared to full ISO 13211-1 Prolog systems (e.g., SWI-Prolog).

The current list is [`docs/tracking/track-limitations.md`](docs/tracking/track-limitations.md)
(4.5.0 added LIM-040..046; 4.6.0 resolved LIM-038, shrank LIM-037 and LIM-041..046 and added
LIM-047: library goals that still fail on a bad input). Deliberate differences, as opposed to limitations, are in
[`docs/references/ref-deviations.md`](docs/references/ref-deviations.md).

### Resolved in v3.0.0

Clean-room rewrites (now default) and ~50 in-place ISO/correctness fixes resolved the remaining limitations:
- **DCG completeness** (LIM-021) — clean-room v2 translator covers head push-back, `|`, `\+`, `call//N`, `{}`, `!`, `->`
- **CLP(FD) soundness** — clean-room v2 interval-domain solver (no OOM), real `#=` propagation, sound first-fail labeling
- Arithmetic/ISO fixes: ISO `(**)/2` float power vs `(^)/2` integer power, integer-only operators raising `type_error(integer,_)` on floats, ISO arithmetic error terms, BigInteger rounding-overflow promotion, `retract((H:-B))`, `occurs_check` flag wiring (LIM-017/018/019/020/022/025)

### Resolved earlier (v2.x)
- `freeze/2`, `when/2`, `dif/2` (coroutining), attributed variables, global variables (`nb_setval/2` etc.)
- Module-qualified calls (`Module:Goal`), `predicate_property/2`, `code_type/2`
- Stream repositioning, arbitrary precision integers (BigInteger), enhanced `write_term/2` options
- CHR (basic simplification/propagation), rational numbers (`rdiv`), number literal notation (`0'a`, `0xFF`, `0o77`, `0b1010`)
- Multi-argument indexing, compiled clause cache, atom garbage collection

Tracked limitations are recorded in [docs/tracking/track-limitations.md](docs/tracking/track-limitations.md). After 4.1.0 the open list is two entries: **LIM-037** (~310 of the 416 registered predicates still run on the eager built-in bridge — none on a measured hot path) and **LIM-036** (`.jpc` source lines come from the legacy parser). LIM-027 closed with the v2 machine that carried it.

---

## 🔧 **IDE Keyboard Shortcuts**

| Action | Shortcut | Description |
|--------|----------|-------------|
| New Project | Ctrl+N | Create new Prolog project |
| Compile Project | Ctrl+F9 | Build current project |  
| Run Query | F5 | Execute query in console |
| Toggle Debug | F8 | Enable/disable debug mode |
| Find in File | Ctrl+F | Search current file |
| Find in Project | Ctrl+Shift+F | Search all project files |
| Save File | Ctrl+S | Save current file |
| Open File | Ctrl+O | Open existing file |

## 🗂️ **Project Structure**

```
JProlog/
├── src/main/java/it/denzosoft/jprolog/
│   ├── core/                         # 🧠 Core Prolog Engine
│   │   ├── engine/                   # Main engine components
│   │   │   ├── Prolog.java           # Primary Prolog engine
│   │   │   ├── EngineContext.java    # Durable per-engine context (debugger, budget)
│   │   │   ├── v4/                   # The resolution engine (Machine, Unify, ClauseStore, ...)
│   │   │   ├── KnowledgeBase.java    # Fact/rule storage system
│   │   │   └── BuiltInRegistry.java  # Built-in predicate registry
│   │   ├── parser/                   # Prolog syntax parsing
│   │   │   ├── Parser.java           # Main parser interface
│   │   │   ├── PrologParser.java     # Core Prolog parser
│   │   │   ├── TermParser.java       # Legacy Pratt parser with shared OperatorTable
│   │   │   └── v2/                    # Clean-room v2 parser (default): Lexer + TermReader
│   │   ├── compiled/                 # Binary compiled format (.jpc)
│   │   │   ├── JpcFormat.java        # Format constants and type tags
│   │   │   ├── JpcWriter.java        # Serializer with string interning
│   │   │   └── JpcReader.java        # Deserializer
│   │   ├── terms/                    # Prolog term representation
│   │   │   ├── Term.java             # Base term interface
│   │   │   ├── Atom.java             # Atomic terms
│   │   │   ├── CompoundTerm.java     # Complex terms
│   │   │   ├── Variable.java         # Prolog variables
│   │   │   └── Number.java           # Numeric terms
│   │   └── dcg/                      # DCG grammar support
│   │       └── DCGTransformer.java   # Grammar rule transformation
│   ├── builtin/                      # 🔧 Built-in Predicates Library
│   │   ├── arithmetic/               # Math operations and comparisons
│   │   ├── control/                  # Control flow and meta-predicates
│   │   ├── list/                     # List processing predicates
│   │   ├── term/                     # Term manipulation
│   │   ├── atom/                     # Atom operations
│   │   ├── database/                 # Dynamic database predicates
│   │   ├── io/                       # Input/output operations
│   │   └── dcg/                      # DCG-specific predicates
│   ├── editor/                       # 🖥️ IDE Components
│   │   ├── PrologIDE.java            # Main IDE application
│   │   ├── FileEditor.java           # Text editor with syntax highlighting
│   │   ├── DebugPanel.java           # Visual debugger interface
│   │   ├── ProjectTree.java          # Project file navigator
│   │   └── ...                       # Other IDE components
│   └── PrologCLI.java                # 📟 Command-line interface
├── examples/                         # 🧪 Example Prolog Programs  
│   ├── test_01_basic_facts.pl        # Basic facts and queries
│   ├── test_14_dcg_simple.pl         # DCG grammar examples
│   ├── family_tree.pl                # Family relationship examples
│   └── ...                           # 40+ comprehensive examples
├── 📚 Documentation Files
├── CLAUDE.md                         # Developer guide and procedures
├── USER_MANUAL.md                    # Complete user documentation
├── docs/references/BUILTIN_PREDICATES_REFERENCE.md  # Built-in predicates reference
├── issues.md                         # Issue tracking and resolutions
└── README.md                         # This file
```

## 🧪 **Testing & Validation**

### Comprehensive Testing Suite
```bash
# JUnit tests (1452 tests)
mvn test

# Run all 20 example programs (comprehensive testing)
./test_all_examples.sh

# Test individual components
./test-debug.sh        # Debug features
./simple_test.sh       # Basic functionality
./test-ide-console.sh  # IDE console integration
```

### Testing Categories
- **Basic Functionality**: Facts, rules, queries, unification
- **Advanced Features**: DCG grammars, meta-predicates, arithmetic  
- **Control Structures**: Cut, if-then-else, negation
- **Built-in Predicates**: I/O, database operations, list processing
- **Performance**: Complex algorithms (N-Queens, sorting, recursion)

## 🔧 **Configuration & Customization**

### Engine/Parser Toggles (system properties)

The clean-room implementations are the defaults. Select an older one per subsystem:

The clean-room implementations are the defaults. There is **one resolution engine** — the toggles
below are the parser, the DCG translator and the CLP(FD) solver, whose `v2` packages are
second-generation rewrites of those subsystems, not engines.

| Subsystem | default | fallback |
|---|---|---|
| Resolution engine | **`core.engine.v4`** | — (the v2 machine, `-Djprolog.engine=v2` and the `engine-v2` profile were deleted in 4.1.0; the recursive `QuerySolver` and `=legacy` in 4.0.0) |
| Parser | `core.parser.v2` | `-Djprolog.parser=legacy` |
| DCG translator | `core.dcg.v2` | `-Djprolog.dcg=legacy` |
| CLP(FD) solver | `builtin.clpfd.v2` | `-Djprolog.clpfd=legacy` |

```bash
-Djprolog.parser=legacy   # legacy parser instead of core.parser.v2
-Djprolog.dcg=legacy      # legacy DCG transformer instead of core.dcg.v2
-Djprolog.clpfd=legacy    # legacy CLP(FD) store instead of builtin.clpfd.v2
```

Only the literal value `legacy` selects a fallback. `-Djprolog.engine=<anything>` is obsolete: it
logs a warning and runs v4. Programmatic equivalents: `setUseV2Parser(false)`,
`setUseV2Dcg(false)`, `setUseV2Clpfd(false)` — `setUseV4Engine`/`isUsingV4Engine` and their v2
twins are gone.
The parser and DCG flags are re-read on every call; the CLP(FD) one is read in the `Prolog`
constructor. Maven: `mvn test` runs the whole suite, on the one engine.

### IDE Settings
Settings stored in `~/.jprolog-ide.properties`:
```properties
editor.font.size=14
editor.tab.width=4
build.auto.save=true
debug.trace.enabled=false
syntax.highlighting=true
```

### CLI Configuration
Environment variables for CLI customization:
```bash
export JPROLOG_STACK_SIZE=10000
export JPROLOG_TRACE_MODE=off  
export JPROLOG_LIST_FORMAT=iso  # Use [a,b,c] format
```

## 🤝 **Development & Contributing**

### Building from Source
```bash
git clone https://github.com/DenzoSOFTHub/JProlog.git
cd JProlog
mvn clean compile
```

### Architecture Highlights
- **Modular Design**: Clean separation between engine, IDE, CLI, and built-ins
- **Extensible Built-in System**: Easy addition of custom predicates
- **ISO Compliance Focus**: Adherence to Prolog standards where possible
- **Test-Driven Development**: Comprehensive test suite ensuring quality

### Key Design Principles
- **Performance**: Efficient unification and backtracking algorithms
- **Reliability**: Robust error handling and edge case management
- **Usability**: Intuitive interfaces for both beginners and experts
- **Maintainability**: Clean code structure with comprehensive documentation

## 📋 **System Requirements**

- **Operating System**: Windows 10+, macOS 10.14+, Linux (Ubuntu 18.04+)
- **Java Runtime**: Java 8 minimum (Java 11+ recommended)
- **Memory**: 512MB RAM minimum (1GB+ for IDE, 2GB+ for large projects)
- **Disk Space**: 200MB for installation, additional space for projects

## 🆘 **Support & Troubleshooting**

### Quick Fixes
- **Build Issues**: Run `mvn clean compile` 
- **CLI Problems**: Check Java classpath and version
- **IDE Won't Start**: Verify Java version with `java -version`
- **Slow Performance**: Increase JVM memory with `-Xmx1g`

### Getting Help
1. **Documentation**: Check relevant `.md` files in the repository
2. **Examples**: Review `examples/` directory for usage patterns
3. **Issues**: Check `issues.md` for known problems and solutions
4. **Testing**: Run test suite to verify installation

## 📄 **License & Copyright**

**JProlog - Complete Prolog Implementation**

Copyright © 2024-2026 DenzoSOFT. All rights reserved.

Version 4.6.0 - Released September 2026

## 🌐 **Project Information**

- **Repository**: https://github.com/DenzoSOFTHub/JProlog
- **Website**: https://denzosoft.it
- **Latest Release**: v4.1.0 — **one engine**: the v2 machine and its CI leg are deleted, the second undo trail is gone, the goal path skips the module test for plain user predicates, an idle debugger costs nothing and `thread_self/1` answers `main`; v4.0.0 made **engine v4 the default** (threads on per-thread machines, a debugger that no longer changes how your program runs, usable tracing, a non-interactive console); v3.9.0-v3.14.0 built engine v4 wave by wave; v3.7.0/v3.8.0 brought the engine hardening waves (15 fixes: bounded-memory execution, million-element lists, lazy generators, budget enforced in meta-calls, first-argument indexing, per-engine flags) and the new Reference Manual; v3.5.0/v3.6.0 brought the ISO-conformance audit waves (80 defects fixed), clean-room v2 engine/parser/CLP(FD)/DCG (default), binary compiled format, and 935 passing tests
- **License**: Proprietary (DenzoSOFT)

---

## 🎯 **Why Choose JProlog?**

✅ **Strong ISO Compliance**: 100% ISO 13211-1 core support (111/111) with 280+ built-in predicates
✅ **Clean-room v2 Parser**: single-pass Lexer + Pratt TermReader (default) with dynamic operator support; legacy fallback available
✅ **Fast Loading**: Binary `.jpc` compiled format with string interning and smart caching
✅ **Complete Ecosystem**: Engine + IDE + CLI + 280+ Built-ins in one package
✅ **Professional Tools**: Full-featured IDE with debugging capabilities
✅ **Complete Grammar Processing**: clean-room ISO DCG translator with full construct coverage
✅ **Easy Integration**: Simple Java API for embedding Prolog logic with `compile()` and `consultSmart()`
✅ **989 Passing Tests**: Comprehensive JUnit test suite with 0 failures  

**Start your Prolog journey with a robust, professional-grade implementation!**

*Powerful Prolog Programming Made Accessible*