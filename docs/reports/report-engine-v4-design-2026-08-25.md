# JProlog Engine v4 — Residual Limits and Clean-Room Redesign

**Date**: 2026-08-25 · **Baseline analysed**: v3.8.0 (working tree after the six engine-hardening
waves, 989/989 tests) · **Author**: engine analysis pass #2 (follows
`report-engine-deep-analysis-2026-08-24.md`)

This document has two halves. **Part A** is the inventory of the limits that remain in the v3.8.0
engine after waves 1–6 — every one reproduced on the current build, with its root cause located in
the code and an explanation of why it cannot be fixed cheaply inside the present architecture.
**Part B** is the design of a new resolution core (**engine v4**, package `core.engine.v4`) that
resolves all of them, followed by the implementation plan intended for Claude Opus 5.

---

## Part A — Residual limits of the v3.8.0 engine

### A.0 How the limits were found

Static review of `core/engine/v2/MachineSolver.java` (1787 lines after the waves),
`QuerySolver.java`, `KnowledgeBase.java`, `StreamManager.java`, `OperatorDefinition.java`,
`When.java`/`Dif.java`/`Freeze.java`, `TableStore.java`, `PrologCLI.java`; plus two probe
harnesses (`Limits.java`, `Limits2.java`, JDK 25, `-Xss4m -Xmx2g`, 20–60 s timeouts per query)
and the 117 verified examples of the reference manual. Timings below come from a loaded VM
(load ≈ 5 on 8 cores) and are only indicative; failures and hangs are not.

### A.1 Inventory

| ID | Limit | Severity | Evidence (v3.8.0) |
|---|---|---|---|
| L-01 | Bindings are a name-keyed `HashMap<String,Term>`: O(#bindings) memory for the whole query, string-typed variable identity, no reclamation | High | `loop(3000000)` needs ~1 GB (LIM-033); `nrev` ≈ 100–450 KLIPS |
| L-02 | The legacy recursive `QuerySolver` still executes `phrase/2,3`, tabled calls, threads, `format/2` sub-goals, DCG helpers, persistence transactions (35 direct `solver.solve(` call sites in 15 built-ins) | **Critical** | `phrase(digits(D), L)` fails at **2 000 tokens** with `resource_error(max_recursion_depth)`, at 200k with `stack_overflow` |
| L-03 | Tabling is a bounded re-evaluation loop (100 iterations, eager, name-keyed) and gives **wrong answers** | **Critical** | `:- table path/2` left-recursive chain: `path(1, 51)` **fails** while `findall(Y, path(1,Y), L)` finds 3 000 answers |
| L-04 | Unifying two cyclic terms **hangs**; neither the inference budget nor a thread interrupt stops it (the unify loop polls nothing) | **Critical** (DoS) | `X = f(X), Y = f(Y), X = Y` and `X = [1|X], Y = [1|Y], X = Y` never return; every other walker throws `representation_error(cyclic_term)` |
| L-05 | Coroutining: `when/2` and `dif/2` still run through the legacy attribute hook, so bindings made by a woken goal are lost; attributed variables are **session-scoped by variable name**, so a suspended goal fires in a *later, unrelated query* | **Critical** (correctness) | `when(nonvar(X), Y = done), X = 1, Y == done` fails; `when(nonvar(X), throw(leak))` in query 1 makes `X = 1` in query 2 throw `leak` |
| L-06 | Process-global state remains: stream tables (`StreamManager` statics shared by every engine and thread), three separate operator stores, `Spy`, `Profiler`, legacy `ConstraintStore`, `ClpfdV2Bridge` per thread rather than per engine | High | `current_op/3` does not see `:- op/3` from a consulted file; streams opened by engine A are visible to engine B |
| L-07 | Streams: text streams are `PushbackReader`s over the raw file, so `seek/4`/`set_stream_position/2` do not affect what is read next; no character/line position, `stream_property/2` supports 6 properties, aliases are global | Medium | `get_char(S,C1), seek(S,0,bof,_), get_char(S,C2)` gives `C2 = e` after `C1 = h` |
| L-08 | Built-in contract is eager: `execute(goal, Map, List<Map>)` materialises every solution; `member/2`, `append/3`, `select/3`, `nth0/nth1`, `clause/2`, `sub_atom/5`, `current_op/3`, `maplist/3..5` output lists remain eager or quadratic (LIM-030) | Medium | `maplist(p, L, L2)` 2.8 s at 40 000 elements |
| L-09 | Module system: flat KB unless a second module exists; built-ins cannot be module-qualified (`lists:append` fails); no library modules, so `partition/4` cannot be offered without shadowing user code; `meta_predicate` ignored | Medium | `lists:append([1],[2],L)` → false |
| L-10 | Missing language features: `library(yall)` lambdas, parametric `char_type/2` forms, `partition/4`, `term_string/2`, `thread_create/3`, `dcg_body//2` (stub), `ins/2` (operator only); `term_to_atom/2` fails silently on 5 000 nested parentheses | Low–Medium | `maplist([X,Y]>>(Y is X*2), …)` → `existence_error(>>/4)` |
| L-11 | Answer printing: the console prints canonical, unquoted terms (`-(a,1)`, `/(foo,1)`, `,(p,q)`) and leaks internal variable names (`_R1_A`, `_G12`); no residual-goal (dif/freeze/clpfd) display | Medium | `?- X = 'a b'-1.` prints `X = -(a b, 1)` |
| L-12 | Clause activation copies the whole clause (`rename`) with fresh `String` names per variable; goal-stack cells allocated per body goal; arithmetic still boxes every intermediate `Number` | Medium (perf) | ceiling ≈ 0.5 MLIPS |
| L-13 | Debugger coupling: tracing/debugging disables the fast paths and keeps exhausted choice points alive to emit `Fail` ports | Low | trace mode 3–5× slower and memory-unbounded |
| L-14 | Deep-structure errors (`stack_overflow`) are converted only after the query unwinds — the program's `catch/3` never sees them | Low | documented since v3.4.0 |

### A.2 Why the current architecture cannot absorb these fixes

1. **Two engines, one contract.** Every `BuiltInWithContext` receives a `QuerySolver`; 15 of them
   still call `solver.solve(...)` directly, and `phrase`, tabling and threads sit on that path. Each
   legacy sub-solve is recursive (2 000-deep cap), eager and copies binding maps. `solveMeta` (wave 4)
   bridges *some* of them onto a nested `MachineSolver`, but every bridge re-implements the same
   marshalling, and the nested machine cannot share choice points with the outer one (so it cannot
   be lazy). Retiring `QuerySolver` requires a built-in API that does not mention it.
2. **Name-keyed bindings** (`binding: Map<String,Term>`, `Variable.equals` by name) make three of the
   limits structural: memory cannot be reclaimed (L-01), variable identity is a string so attributed
   variables must be re-attached "by name" across queries (L-05), and every clause activation must
   invent new names (L-12). A cell-based variable is the only representation under which the JVM's
   own GC reclaims dead bindings and identity is a pointer.
3. **Cycle safety** was retro-fitted as a `Set<String>` of "active" names inside `resolve()`; the
   tail-iterative `unify` (wave 2) has no such guard and loops (L-04). Rational-tree support needs
   pointer identity on *terms* (a visited set keyed by object pairs), which again wants the cell
   model, plus a cancellation check inside the walkers.
4. **Tabling** is implemented as "re-run the goal until the answer set stops growing (max 100)".
   That is neither SLG nor linear tabling: there is no notion of subgoal completion, consumers of an
   in-progress table read a stale partial list, and bound-argument variants of a left-recursive
   predicate never converge (L-03). A correct algorithm must live in the machine's choice-point
   stack, not in a built-in.
5. **Global statics** (streams, operator stores, spy points, profiler) were fine for one engine per
   JVM; the IDE, the sandbox story ("one engine per security domain") and threads make them wrong.
   Each needs an owner object — which the machine must be able to reach through its context.

---

## Part B — Engine v4 design

### B.0 Goals and non-goals

Goals — in priority order:

1. One engine. `core.engine.v4` executes *everything*: control, meta-calls, DCG, tabling, threads,
   coroutining. `QuerySolver` and `MachineSolver` are deleted at the end of the plan.
2. Bounded memory for deterministic programs; million-element and million-step programs run with
   default JVM settings; ≥ 2 MLIPS on `nrev` (≈ 5× v3.8.0), 20 000-clause lookups in ≤ 1 µs.
3. Correct semantics where v3.8.0 is wrong: tabling, coroutining, rational trees, per-engine state.
4. A built-in API that is lazy, machine-aware and does not depend on the legacy solver, with an
   adapter that keeps the ~400 existing `BuiltIn` classes working unchanged until each is migrated.
5. Embedder API (`Prolog.solve/solveStream/consult/...`, `Map<String,Term>` results) unchanged;
   the sandbox and resource-budget trust model unchanged.

Non-goals: a bytecode/WAM compiler (the interpreter over pre-compiled clause skeletons is enough for
the targets), JIT, and changes to the parser (`core.parser.v2` stays).

### B.1 Package layout

```
core/engine/v4/
  Machine.java            drive loop, goal/choice-point stacks, cut, catch/throw, cleanup frames
  Bindings.java           Var cells, trail, conditional trailing, mark/undo, wake queue
  Unify.java              unification, ==/compare, copy_term, occurs check — iterative, cycle-safe
  ClauseStore.java        predicates, generations (logical update view), first-argument indexing
  Clause.java             compiled clause skeleton (numbered variables, ground flags, arg keys)
  Builtin.java            v4 built-in SPI (Det / NonDet / Control) + Generator protocol
  LegacyBuiltinAdapter.java  runs the existing BuiltIn/BuiltInWithContext classes on v4
  Table.java, Tabling.java   linear tabling (subgoal tables, answer tries, completion)
  Coroutining.java        attributed variables, attr_unify_hook dispatch, freeze/when/dif natively
  Modules.java            module table, resolution, library modules, meta_predicate expansion
  Streams.java, PrologStream.java   per-engine stream table, positions, properties
  Ops.java                per-module operator tables (single store for parser, writer, current_op)
  Writer.java             ISO write_term (quoted, ignore_ops, numbervars, max_depth, cycles, portray)
  Errors.java             ISO error construction with context, Java-exception mapping
  Engine.java             the per-engine context handed to built-ins (flags, streams, ops, KB, guard)
core/engine/Prolog.java   facade unchanged; holds an Engine; solve() builds a Machine per query
```

### B.2 Terms and variables

Atoms, numbers, strings and compounds stay immutable (`CompoundTerm` argument arrays are exposed as
`Term[]` for speed; the `List` view remains for compatibility). **`Variable` becomes a mutable
reference cell**:

```java
public final class Variable extends Term {
    Term ref;                 // null = unbound; otherwise the bound value (a chain is possible)
    final long serial;        // creation order (machine-wide counter): drives conditional trailing
    AttrList attrs;           // attributed-variable payload, null when absent
    String name;              // only for query variables and numbervars; null for fresh cells
}
```

- `equals`/`hashCode` are identity (`==`); `getName()` returns `name` or `_G<serial>` for printing.
  This is the single largest ripple of the redesign — see B.14 for the audit.
- `deref(t)`: follow `ref` until unbound or non-variable; O(chain). Chains are shortened by binding
  the younger variable to the older one (the WAM rule), which also keeps trailing cheap.
- Query variables keep their source names so `Prolog.solve` can still return `Map<String,Term>`.

**Clause skeletons.** Consulting compiles each clause once into a `Clause`: head and body terms
where every variable is replaced by a `VarRef(k)` placeholder (`k` = 0..n-1), `nvars`, a
`groundHead` flag, the first-argument index key, and a flat body goal array. Activating a clause
allocates one `Variable[nvars]` frame and instantiates head/body lazily: the head is unified
*directly against the skeleton* with the frame (no copy — `Unify.unifyHead(skeleton, frame, goal)`),
and body goals are built only when pushed. This removes the per-activation `HashMap<String,Variable>`,
the `"_R" + id + "_" + name` strings and most of the term copying of v3.8.0 (L-12).

### B.3 Bindings, trail, choice points (`Bindings`, `Machine`)

- **Trail**: an array of `Variable` cells plus an array of undo `Runnable`s (for attribute changes,
  `b_setval`, `setarg`, catch-frame disarm). `bind(v, t)` trails `v` only if `v.serial <
  topChoicePoint.serial` (conditional trailing); `undo(mark)` resets `ref = null`.
- **Choice point** (`Frame`): `{ kind, trailMark, serialAtCreation, goalContinuation, cutBarrier,
  alternative supplier }`. Kinds: clause-iterator (predicate + clause cursor + generation), generator
  (built-in `Generator`), disjunction, ITE/soft-cut, catch frame, cleanup frame (for
  `setup_call_cleanup/3`), tabling consumer/generator (B.8). Frames live in an array with an
  explicit top; exhausted frames are popped ("trust-me") exactly as in v3.8.0.
- **Goal stack**: a linked continuation of `{ goal, cutBarrier, next }` cells as today, plus
  *environment-free* execution (a body goal carries its frame reference). Because a `Variable`
  cell is reachable only from live terms, the JVM reclaims every binding of a finished, deterministic
  call — `loop(N)` runs in O(1) heap for any N (L-01).
- **Resource guard** is polled in the drive loop *and* inside every long-running loop of `Unify`
  (unify, compare, copy, occurs) every 4 096 iterations, so a cyclic-term loop is cancellable (L-04)
  even before rational-tree support makes it terminate.

### B.4 Unification and term walkers (`Unify`)

All walkers use an explicit work stack (no Java recursion) and are **cycle-safe**:

- `unify(a, b)`: iterative; when the work stack depth exceeds a threshold (say 1 024 pairs), switch
  to a `visited` identity set of `(compound, compound)` pairs — a pair already visited is assumed
  to unify (the standard rational-tree unification). Result: `X = f(X), Y = f(Y), X = Y` succeeds
  in finite time, `X = [1|X], Y = [1|Y], X = Y` too.
- `compare/3`, `==/2`, `copy_term/2`, `term_variables/2`, `ground/1`, `numbervars/3`,
  `subsumes_term/2`: same scheme; `copy_term` rebuilds cycles (a visited map from source node to copy).
- `cyclic_term/1` and `acyclic_term/1` become real; `write/1` of a cyclic term prints with
  depth limit and `@(Template, Substitutions)` notation when `cycles(true)` is requested.
- Occurs check: iterative, guarded, honours the per-engine flag and `unify_with_occurs_check/2`.
- Comparison of numbers keeps the v3.8.0 primitive fast paths; `Number` gains a `long` fast
  representation used by the evaluator to avoid boxing on `+ - * < is` when both operands are small.

### B.5 The v4 built-in SPI (`Builtin`, `Generator`) and the legacy adapter

```java
public interface Builtin {
    /** Called with dereferenced argument cells; may bind through `m.unify`, push goals, throw ISO errors. */
    Outcome call(Machine m, Term[] args);      // Outcome: SUCCESS, FAILURE, or SUSPENDED (pushed goals/CP)
}
public interface Generator {                   // for nondeterministic built-ins
    /** Produce the next solution by binding through `m`; return false when exhausted. */
    boolean next(Machine m);
    default void cut() {}                      // release resources when the choice point is cut
}
```

- Determinism is expressed by what the built-in does: bind and return `SUCCESS`, or
  `m.pushGenerator(generator)` and return `SUSPENDED`. Control built-ins (`call/N`, `findall/3`,
  `catch/3`, `forall/2`, `once/1`, `bagof/setof`, `aggregate_all/3`, `setup_call_cleanup/3`,
  `phrase/2,3`, `with_output_to/2`, `format/2,3` with `~@`) are written against `Machine`
  primitives: `pushGoal`, `pushChoice`, `pushCatch`, `pushCleanup`, `runSubQuery(goal, sink)`
  (a nested drive sharing the trail, choice-point floor and guard).
- `Machine` exposes the engine context: `flags()`, `streams()`, `ops()`, `modules()`,
  `kb()`, `guard()`, `err()` (ISO error factory with the built-in's indicator as context).
- **`LegacyBuiltinAdapter`** wraps every class implementing the old `BuiltIn` interface: it builds a
  resolved goal (structure-sharing, exactly as `bridgeBuiltin` does in v3.8.0), a `Map<String,Term>`
  view keyed by the names of the goal's remaining variables, calls `execute`, and turns the returned
  maps into bindings through `m.unify`, one solution per `Generator.next`. `BuiltInWithContext`
  classes receive a `SolverFacade` implementing the three `QuerySolver` methods they use
  (`solve`, `solveMeta`, `getResourceGuard`) on top of `runSubQuery` — so the legacy solver class
  can be deleted before all 46 context built-ins are rewritten. The identity-sensitive
  `setarg/3`/`nb_setarg/3` become native in wave 1 of the plan.
- Registration: `BuiltInFactory` gains `registerNative(name, arity, Builtin)`; the registry maps
  `(name, arity)` → entry (no more "any arity" wildcards); `isBuiltIn` answers per arity.

### B.6 Execution core (`Machine`)

Same shape as v3.8.0's `drive()` — it is the part of the current engine that is right — with these
changes:

- Dispatch on the predicate entry (`ClauseStore.lookup(name, arity)` → `Predicate` object holding
  clauses, flags, module, tabling state, native built-in) instead of a chain of string comparisons;
  the goal term's functor atom caches its `Predicate` per engine (`Atom` gets a small
  engine-keyed side table) so a call is one array index in the common case.
- Control constructs (`,`/`;`/`->`/`*->`/`\+`/`!`/`call/N`/`catch`/`throw`/`findall`) stay native.
- **Cleanup frames** implement `setup_call_cleanup/3` and `call_cleanup/2` correctly: the cleanup
  goal runs once on deterministic exit, failure, cut of the frame, or exception.
- **Exceptions** carry an ISO context (`context(Name/Arity, Message)`) filled by the machine from
  the current predicate; `StackOverflowError` cannot occur in the core (no recursion), and
  `OutOfMemoryError` in a walker is turned into `resource_error(memory)` *before* unwinding so
  `catch/3` can see it (L-14).
- Last-call: when a clause's last body goal is pushed and the clause frame owns no choice point,
  the continuation cell is reused (no growth of the continuation list in tail recursion).
- The debugger/tracer subscribes to port events emitted by the machine; fast paths are *not*
  disabled — instead the machine emits ports for native built-ins itself, and a traced choice
  point is kept only when the tracer asks for `Fail`/`Redo` ports (L-13).

### B.7 Clause store and indexing (`ClauseStore`, `Clause`)

- `Predicate { name, arity, module, dynamic, tabled, clauses: Clause[] (append-only), erased
  generations, firstArgIndex }`. **Generations** implement the logical update view: every clause has
  `birth` and `death` generation numbers; a call captures the current generation and iterates
  clauses with `birth ≤ g < death`. `assertz` appends (amortised O(1)); `asserta` prepends into a
  head chunk; `retract` sets `death` (O(1)) — no snapshot rebuild per write (fixes the O(n) rebuild
  of the v3.8.0 versioned snapshots for large dynamic predicates), and dead clauses are compacted
  when no running call holds an older generation.
- First-argument index: per predicate, a hash from key (atom / small int / functor+arity / string)
  to clause id list, merged with the variable-headed list in clause order; built lazily on first
  call with a bound first argument, maintained incrementally on assert/retract. No bucket cap is
  needed because clause ids are ints and the index holds no snapshots.
- `Clause` also records the source line (breakpoints) and the module.

### B.8 Tabling (`Tabling`, `Table`) — linear tabling with completion

Replace the bounded re-evaluation with **linear tabling (SLD + iterative completion, B-Prolog /
`DRA` style)**, which is correct and complete for definite programs with left recursion and keeps
the machine's SLD structure:

- A *variant table* per tabled subgoal: `{ status: EVALUATING | COMPLETE, answers: AnswerTrie,
  dependencies }`. The first call to a variant becomes its **generator**: it runs the clauses
  normally while recording every answer in the trie (answers are returned to the caller as they are
  found). A later call to the same variant while it is EVALUATING becomes a **consumer** choice
  point that iterates the answers recorded *so far*.
- **Completion**: when a generator exhausts its clauses, if any consumer of it (or of a table in its
  SCC) read answers that were later extended, the generator **re-runs** its clauses (semi-naive: only
  new answers are returned by consumers) until a fixpoint; then all tables of the SCC are marked
  COMPLETE. There is no iteration cap; termination is guaranteed by the finite answer set (variant
  tabling) and answer deduplication.
- Answers are stored as ground/variant-normalised terms (no name-keyed maps), so
  `path(1, 51)` and `path(1, Y)` share the same machinery and are both correct (L-03).
- `abolish_all_tables/0`, `abolish_table/1`, `table/1` directives, and `tnot/1` (well-founded
  negation) can be added later on the same structure.

### B.9 Coroutining and attributed variables (`Coroutining`)

- Attributes live on the `Variable` cell (`attrs`), trailed on change. Binding an attributed
  variable pushes a **wake goal** onto the machine's wake queue; the drive loop runs the queue before
  the next goal, inside the current binding context — bindings made by woken goals are ordinary
  bindings (L-05). This generalises the `woken` list that v3.8.0 already uses for `freeze/2` to all
  attribute kinds.
- The SWI protocol: `put_attr(V, Module, Value)`; when `V` is bound to `Other`, the machine calls
  `Module:attr_unify_hook(Value, Other)`. `freeze/2`, `when/2`, `dif/2` are library modules written
  in Prolog on top of `put_attr/get_attr` (the prelude, B.10), and the CLP(FD) v2 bridge registers
  a native hook. `copy_term/3` (attribute goals), `term_attvars/2`, `frozen/2`, `dif/2` residual
  goals in answers follow from the same data.
- **No session-scoped attributed variables.** A query's variables die with the query. The v2.9.4
  feature "suspended goals survive into the next query" is removed (it is what makes `when/2` fire
  in an unrelated query). If cross-query coroutining is wanted by the IDE, it becomes an explicit,
  opt-in API on `Prolog` (keeping the variable cells of the previous answer alive on request).

### B.10 Modules and the Prolog prelude (`Modules`)

- Every predicate belongs to a module; `system` holds the built-ins, `user` is the default,
  library modules (`lists`, `apply`, `yall`, `strings`, `pairs`, `aggregate`, `dcg_basics`, `freeze`,
  `when`, `dif`) are **written in Prolog** and loaded from classpath resources (`prelude/*.pl`) on
  first use (autoload by predicate indicator). Resolution order for an unqualified call from module
  `M`: `M` → `M`'s imports → `user` → autoload libraries → `system`. A user definition of
  `partition/4` therefore wins over `library(apply)` without any registration trick (L-09, L-10).
- `Module:Goal` resolves in `Module` with the same order, so `lists:append/3` and
  `system:atom_length/2` work; `meta_predicate/1` declarations make `call/N` arguments
  module-transparent (context module carried on the goal cell).
- The flat-KB special case of v3.8.0 (`modules.size() > 1`) disappears: `user` *is* the flat KB.
- `library(yall)` lambdas: `>>/N` and `/`/N implemented natively in `call/N` (copy the lambda
  term unless it is `\`-marked, bind parameters, run the body).

### B.11 Streams (`Streams`, `PrologStream`)

- `PrologStream { id, aliases, mode, type (text/binary), encoding, eofAction, reposition, channel }`
  where `channel` is a `SeekableByteChannel` (files) or a plain stream (console, sockets, strings).
  Text streams decode through a private buffer that tracks **byte position, character count, line
  number and line position**; `seek/4`, `set_stream_position/2` and `stream_property(S,
  position(P))` use the byte offset and flush/re-fill the decoder buffer, so repositioning is
  correct on text streams too (L-07). Peek is a one-character lookahead on the decoder.
- Stream table is **per engine** (`Engine.streams()`), with `user_input/user_output/user_error`
  bound to the thread-local overrides the IDE already uses; `current_input/output` are per machine.
- Full `stream_property/2` (`file_name`, `mode`, `input`, `output`, `alias`, `position`,
  `end_of_stream`, `eof_action`, `reposition`, `type`, `encoding`, `line_count`), `set_stream/2`,
  `stream_position_data/3`, `character_count/2`, `line_count/2`, `line_position/2`.
- `read_term/2,3` gains `variable_names`, `singletons`, `syntax_errors`, `term_position`; the
  parser's nesting limit raises `resource_error(parser_nesting)` (never a silent failure).

### B.12 Operators, flags, writer (`Ops`, `Writer`)

- One operator store per engine, scoped by module (`op/3` in a module file is module-local;
  `system` ops are global). The parser, `current_op/3`, `write_term/2` and the `.jpc` writer all
  read the same store (L-06, L-08). `OperatorDefinition`'s static tables are deleted.
- `Writer` implements ISO `write_term/2,3` options completely (`quoted`, `ignore_ops`,
  `numbervars`, `max_depth`, `portray`, `cycles`, `variable_names`, `spacing`) using the engine's
  operator store; `print_message/2` and a `portray_clause/1` built on it. The CLI and the IDE
  print answers with `quoted(true), numbervars(true), portray(true)` and print residual goals
  (`dif/2`, `freeze/2`, CLP(FD) domains) after the bindings (L-11). Fresh variables print as `_123`
  or `_A`.

### B.13 Threads, engines and the embedding facade

- `thread_create/2,3`, `thread_join/2`, message queues: a new thread gets a new `Machine` over the
  same `Engine` (shared clause store — already thread-safe by generations, shared flags with
  per-machine current streams). No legacy solver.
- `Prolog` (facade) keeps its public API. Internally: `Engine` (flags, streams, ops, modules,
  clause store, tables, guard settings) + one `Machine` per `solve`. `solveStream` is the native
  path; `solve` collects. Results remain `Map<String,Term>` keyed by query variable names (the
  values are resolved terms in which unbound variables are fresh `Variable` cells).
- Sandbox: unchanged (`enableSafeMode` unregisters host-touching predicates in the `system`
  module); budget/cancel: the single `ResourceGuard` per machine, polled by the drive loop and the
  walkers.

### B.14 Compatibility and the identity-variable audit

Changing `Variable.equals` to identity is the one change that touches code outside the engine.
Audit plan (grep-driven, done in wave 1 before any engine code):

- `Map<String, Term>` bindings passed to legacy built-ins are produced by the adapter from
  *resolved* goals, so built-ins that only read/unify through the map keep working.
- Built-ins that create `new Variable("Name")` and expect it to alias a query variable of the same
  name (a few legacy list/DCG helpers) must be fixed to use the variable cells from the goal.
- `Variable.copy()` returning "same name" is replaced by explicit `copy_term` semantics.
- `TermCopier`, `TableStore`, `Prolog.mapInternalVariablesToQueryVariables`, the IDE's variable
  views (`DebugPanel` filters names starting with `_R`/`_G`) and the `.jpc` writer are updated to
  the cell model (variables are serialised by index within a clause, as the skeletons already do).

Behaviour changes visible to users, to be listed in the release notes: rational trees are
supported (some previously "error" queries now succeed); coroutining no longer crosses queries;
`current_op/3` sees file-declared operators; answers print with operators and quotes; library
predicates can be overridden by user definitions.

### B.15 Performance targets and acceptance benchmarks

| Benchmark (default JVM flags, quiet machine) | v3.8.0 | v4 target |
|---|---|---|
| `nrev` 30 × 2 000 | ~0.45 MLIPS warm | **≥ 2 MLIPS** |
| `loop(10000000)` | needs > 2 GB | **≤ 64 MB**, ≤ 8 s |
| `loop2(1000000)` (one built-in per iteration) | 128 MB, ~3 s | ≤ 64 MB, ≤ 3 s |
| 1 000 000-element list: `length`, `msort`, `copy_term`, `==`, `write` | OK, 0.5–2 s each | ≤ 0.5 s each |
| `phrase/2` over 1 000 000 tokens | fails at 2 000 | **OK**, ≤ 2 s |
| `:- table path/2` chain of 100 000 edges, `path(1, 100001)` | wrong answer at 51 | **correct**, ≤ 2 s |
| `X = f(X), Y = f(Y), X = Y` | hangs | succeeds in < 1 ms |
| 20 000-clause lookup, first argument bound | ~2–6 µs | ≤ 1 µs |
| `between(1, 10^7, X), fail` | ~3 s | ≤ 1 s |
| budget/interrupt inside every meta-call, `phrase`, tabling, threads | partial | **100 %** |

### B.16 Implementation plan for Opus 5 (waves, each shippable)

Conventions as in `CLAUDE.md` (ISS numbers from ISS-2025-0438, `START_CHANGE` tags, one test per
fix, `mvn test` + `test_all_examples.sh` green after every wave, docs/tracking/CHANGELOG per wave).
The v4 engine is developed **alongside** v2 behind `-Djprolog.engine=v4` / `Prolog.setUseV4Engine`
until wave 8 makes it the default and wave 9 deletes the old engines; every wave keeps the full
suite green on the *default* engine.

| Wave | Scope | Acceptance |
|---|---|---|
| **W1 — foundations** | `Variable` cell model + identity audit (B.2/B.14); `Unify` iterative & cycle-safe with guard polling (B.4); clause skeletons (B.2); `Bindings` trail/CP (B.3); `Machine` core with control constructs, catch/throw, findall, cleanup frames (B.6); `LegacyBuiltinAdapter` + `SolverFacade` (B.5); v4 opt-in flag | v4 passes `MachineSolverTest`, `V2EngineIntegrationTest`, `EngineHardeningTest` when selected; cyclic unify terminates; `loop(10M)` ≤ 64 MB |
| **W2 — clause store** | `ClauseStore` with generations and incremental first-arg index (B.7); assert/retract/abolish/clause/listing on it; `.jpc` reader/writer on skeletons | logical-update-view tests (ISS-2025-0396 family); lookup ≤ 1 µs; 100 000 asserts + calls interleaved in ≤ 1 s |
| **W3 — native library & meta-calls** | v4 SPI for all control/meta built-ins incl. `phrase/2,3`, `bagof/setof/aggregate_all`, `setup_call_cleanup`, `format ~@`, `with_output_to`; lazy `Generator` versions of `member/append/select/nth0/nth1/clause/sub_atom/current_op/between/length/repeat`; `maplist`/`foldl`/`include`/`exclude`/`partition` as prelude Prolog; yall lambdas | DCG over 1 M tokens; budget/interrupt matrix 100 %; `maplist(p, L, L2)` linear; `DebuggingTest`/`DCGTranslatorTest` green on v4 |
| **W4 — coroutining** | `Coroutining` wake queue, `attr_unify_hook` protocol, `freeze/when/dif` as prelude modules, CLP(FD) v2 bridge on the hook, residual goals API; remove session-scoped attributed vars (explicit opt-in API for the IDE) | `when/2` bindings propagate; no cross-query wake-ups; `ClpfdV2*Test` green on v4 |
| **W5 — tabling** | Linear tabling with completion (B.8); `table/1`, `abolish_*_tables`, answer tries; delete `TableStore` + `QuerySolver.solveWithTabling` | left-recursive `path/2` chain 100 000 correct; `fib(1000)` tabled; mutual recursion SCC tests |
| **W6 — modules & prelude** | `Modules` (B.10): system/user/library modules, autoload, resolution order, `Module:Goal` for built-ins, `meta_predicate`; move list/apply/pairs/strings library predicates whose Java versions are redundant to `prelude/*.pl` | `lists:append/3` works; user `partition/4` overrides library; module tests (ISS-2025-0314 family) green |
| **W7 — engine state: streams, operators, writer** | `Streams`/`PrologStream` per engine (B.11); `Ops` per engine/module (B.12); `Writer` with full `write_term` options; CLI/IDE answer printing with operators, quotes, residual goals; delete `StreamManager` statics, `OperatorDefinition` tables, `Spy`/`Profiler` statics (per engine) | seek on text streams correct; `stream_property` full; `current_op` sees consulted ops; two engines fully isolated (streams, ops, spy, profile) |
| **W8 — default switch & threads** | v4 becomes the default; `thread_create/2,3`, `concurrent_*`, `first_solution` on v4 machines; debugger/tracer on machine port events without disabling fast paths (B.6) | full suite + `test_all_examples.sh` + IDE debugger manual checklist green on v4; trace mode ≤ 1.5× slower |
| **W9 — retirement** | Delete `QuerySolver`, `MachineSolver` (v2), `CutStatus`, `LayeredMap`, `TermCopier` name-based paths, `Trail` static, the 35 legacy sub-solve sites, `-Djprolog.engine=legacy`; reduce `BuiltInWithContext` to the adapter; update all docs (`CLAUDE.md`, manual, references) | no reference to `QuerySolver` in `src/main`; test count ≥ current; manual regenerated |

Effort signal for planning: W1 and W3 are the large ones (each roughly the size of the whole
v3.7.0–v3.8.0 effort); W2, W4, W5, W7 are medium; W6, W8, W9 are small-to-medium but touch many
files.

### B.17 Decisions required before starting

1. **Keep a fallback engine?** Recommended: keep v2 (`MachineSolver`) selectable for one release
   after v4 becomes the default, delete the recursive `QuerySolver` immediately in W9.
2. **Rational trees**: support them (recommended, SWI/YAP/SICStus behaviour) or keep the ISO
   `representation_error(cyclic_term)` policy but with guaranteed termination. The design supports
   both through one flag (`occurs_check = error` gives the ISO behaviour).
3. **Cross-query coroutining**: drop it (recommended) or keep it as an explicit `Prolog` API.
4. **Library-in-Prolog**: accept that `member/2`, `append/3`, … become Prolog clauses (lazy,
   traceable, overridable) — slightly slower than Java for tiny lists, much better everywhere else.
5. **Answer printing**: switch the console to quoted operator notation with `_A`-style variable
   names (this changes the text of every CLI answer and some tests that compare output).

**Decisions taken (project owner, 2026-08-25)**: all five recommended options are approved —
(1) v2 `MachineSolver` stays selectable for one release after v4 becomes the default and the
recursive `QuerySolver` is deleted in W9; (2) rational trees are supported (`occurs_check = error`
gives the ISO error behaviour); (3) cross-query coroutining is dropped; (4) `member/2`, `append/3`
and the other list/apply predicates become prelude Prolog clauses; (5) the console prints answers
in quoted operator notation with `_A`-style variable names.

### B.18 Risks and mitigations

- *Identity variables ripple through 400 built-ins.* Mitigated by the adapter (built-ins keep
  receiving name-keyed maps over resolved goals) and by doing the audit first (W1), with the full
  suite as the oracle.
- *Two engines during the transition.* Mitigated by the opt-in flag and by running the whole test
  suite twice in CI (default engine + `-Djprolog.engine=v4`) from W1 to W8.
- *Tabling completeness.* Linear tabling is well understood; the acceptance tests include the
  classic left-recursive, mutually recursive and bound-argument cases plus a randomised
  graph-reachability oracle against `findall`-based transitive closure.
- *Behaviour changes.* Each is listed in B.14/B.17 and must be called out in the release notes.
