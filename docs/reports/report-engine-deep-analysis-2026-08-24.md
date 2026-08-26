# JProlog Engine Deep Analysis — 2026-08-24

**Scope**: the default v2 resolution engine (`core.engine.v2.MachineSolver`), its supporting data
structures (`core.terms.*`, `core.engine.KnowledgeBase`, `Rule`, `Trail`, `BuiltInRegistry`), the
default arithmetic path (`core.arith.v2.ArithEvaluator`), the registry bridge to the 200+ built-ins and
the legacy `QuerySolver` sub-solve path used by `BuiltInWithContext` built-ins.
**Version analysed**: 3.6.0 (`pom.xml`), commit `18707ca`.
**Method**: static review of the engine sources plus an empirical harness (`Bench.java`, JDK 25,
`-Xss4m`, heap as noted) exercising memory, deep structures, budget/cancellation, numeric typing and
clause lookup. Every finding below carries the evidence that produced it.

Each finding has an ID `ENG-NN`, a **severity** (Critical / High / Medium / Low), a **category**
(correctness, limit, performance, memory, robustness), the **root cause** with file references, the
**proposed solution**, and the **type of intervention** (bugfix / refactor / feature / architecture)
with an effort estimate. An implementation plan grouped in waves closes the document.

---

## 0. Baseline measurements

| Test | Result |
|---|---|
| `nrev` 30 elements × 2000 iterations | 5.27 s → **~188 KLIPS** |
| `loop(N) :- N1 is N-1, loop(N1).` deterministic recursion, N = 40 000 | 198 ms |
| same loop, N = 300 000, `-Xmx64m` | **OutOfMemoryError** |
| same loop, N = 1 000 000, `-Xmx256m` | **OutOfMemoryError** (3.6 s at `-Xmx2g`) |
| `loop2/1`: same loop + one `atom_length(abc,_)` per iteration, N = 10 000, `-Xmx2g` | **OutOfMemoryError after 27 s** |
| `between(1,2000000,X), X >= 2000000`, `-Xmx256m` | **OutOfMemoryError** (2.0 s at `-Xmx2g`) |
| `numlist(1,20000,L), length(L,N)` | OK |
| `numlist(1,30000,L), L == L` / `sum_list(L,S)` / `copy_term(L,L2)` / `assertz(big(L))` | **resource_error(stack_overflow)** |
| `numlist(1,100000,L), length(L,N)` | **resource_error(stack_overflow)** |
| 20 000-fact table `f(I,I)`, 2000 calls of `f(19999,_)` | 8.2 s (**4.1 ms per call**) |
| same table, 2000 calls of `f(0,_)` (first clause matches) | 7.9 s (**3.9 ms per call**) |
| `catch(throw(x),_,true)` × 50 000 | 493 ms (~10 µs each) |
| `nb_setval(c,0), repeat, …, V1 >= 1500, !` | **fails** (`repeat` stops after 1000 redos) |
| `sum_list([1.5,1.5],S)` | **S = 3** (integer; must be 3.0) |
| budget 20 000 steps: `loop(100000)` | `InferenceLimitException` after 251 ms ✔ |
| budget 20 000 steps: `once(loop(100000))`, `ignore(...)`, `aggregate_all(count, ...)`, `setup_call_cleanup(true, loop(100000), true)` | **never aborted** (killed at 60 s) |
| budget 20 000 steps: `forall(between(1,100000,_), true)` | completes (**budget not counted**) |
| budget 20 000 steps: `bagof(X, (between(1,100000,X), loop(10)), L)` | `resource_error(stack_overflow)` after 3.2 s |
| thread interrupt at 1.5 s: `loop(30000000)` | `QueryCancelledException` at 1.57 s ✔ |
| thread interrupt at 1.5 s: `once(loop(3000000))` | **not cancelled** (killed at 60 s) |
| `once(loop(5000))` vs native `loop(100000)` | 1.78 s vs 0.25 s → legacy sub-solve is **~70× slower per inference** |

---

## 1. Correctness / inaccuracies

### ENG-01 — `repeat/0` yields at most 1000 solutions — **Critical**, correctness
- **Evidence**: `nb_setval(c,0), repeat, nb_getval(c,V), V1 is V+1, nb_setval(c,V1), V1 >= 1500, !` fails.
- **Root cause**: `builtin/control/Repeat.java` materialises 1000 copies of the binding map
  (`for (int i = 0; i < 1000; i++) solutions.add(new HashMap<>(bindings))`). The classic
  `repeat, read(X), …, X == end_of_file, !` loop silently fails after 1000 iterations, and each
  `repeat` costs 1000 full map copies.
- **Solution**: handle `repeat` natively in `MachineSolver.drive()` as an *infinite* choice point: a
  `CP` whose single alternative re-installs the continuation and never advances `idx` (or an `Alt`
  implementation that is re-added). Remove the eager built-in (or keep it for the legacy engine only).
- **Intervention**: bugfix, small (≈30 lines). Test: the counter loop above to 1500 and to 100 000.

### ENG-02 — float results collapse to integers (`Number(double)` auto-classification) — **High**, correctness
- **Evidence**: `sum_list([1.5,1.5],S)` → `S = 3`; `sumlist([1.0],S)` → `S = 1`;
  JSON `1.0` parses to integer `1` (`JsonPredicates.java:268`); CSV numeric fields likewise
  (`CsvPredicates.java:96`).
- **Root cause**: `core/terms/Number.java` constructor `Number(double)` decides "integer if the value
  has no fractional part", so any built-in that computes a `double` and wraps it with `new Number(d)`
  returns an ISO *integer* whenever the value happens to be integral. 146 call sites in
  `src/main` use `new Number(<non-long expression>)`; `SumList.java:40` is the visible one
  (`allIntegers ? new Number(longSum) : new Number(doubleSum)` — the float branch still yields an
  integer). ISO 9.1.3 / 7.1.2: a float never becomes an integer implicitly.
- **Solution**: make `Number(double)` always construct a **float** (remove the inference) and add
  explicit factories `Number.ofLong(long)` / `Number.ofDouble(double)`; audit the 146 call sites and
  switch those that intend an integer (counts, lengths, arities, codes) to the `long` constructor.
  Run the full suite — the parser and `ArithEvaluator` already use the explicit two-argument
  constructor, so the blast radius is the built-in library.
- **Intervention**: bugfix + audit, medium (mechanical, guided by the compiler if the double constructor
  is deprecated). Tests: `sum_list`/`sumlist`/`max_list`/`min_list` on float lists, JSON/CSV floats,
  `float(X)` after each arithmetic built-in.

### ENG-03 — `length/2` does not enumerate partial lists — **Medium**, correctness
- **Evidence**: `length(L, N), N >= 3, !` fails; `length([a|T], N)` fails.
- **Root cause**: `builtin/list/Length.java` handles (proper list, _) and (_, integer) and returns
  `false` for every other mode instead of enumerating `N = 0, 1, 2, …` (ISO 8.x / SWI semantics).
- **Solution**: native `length/2` in `MachineSolver` for the partial-list/unbound-length mode as a lazy
  infinite choice point (same mechanism as ENG-01); keep the Java built-in for the two deterministic
  modes.
- **Intervention**: bugfix, small. Depends on the lazy-generator mechanism of ENG-11.

### ENG-04 — inference budget, cancellation and tracing are bypassed inside meta-call built-ins — **Critical**, robustness/security
- **Evidence**: with `setInferenceBudget(20000)`: `once(loop(100000))`, `ignore(loop(100000))`,
  `aggregate_all(count, loop(100000), C)`, `setup_call_cleanup(true, loop(100000), true)` run until
  killed at 60 s; `forall(between(1,100000,_), true)` completes normally;
  `bagof(X, (between(1,100000,X), loop(10)), L)` dies with `resource_error(stack_overflow)`. A thread
  interrupt is ignored inside `once(loop(3000000))`. `once(loop(5000))` takes 1.78 s versus 0.25 s
  for a native `loop(100000)`.
- **Root cause**: `once/1`, `ignore/1`, `forall/2`, `bagof/3`, `setof/3`, `aggregate_all/3`,
  `maplist/2..5`, `foldl/4..6`, `include/3`, `exclude/3`, `partition/4`, `predsort/3`,
  `setup_call_cleanup/3`, `with_output_to/2`, `clause/2`, `freeze/when/dif` (46 classes implement
  `BuiltInWithContext`) are executed through `bridgeBuiltin()` → `executeWithContext(contextSolver, …)`
  on the **legacy recursive `QuerySolver`**, which polls neither the budget nor the interrupt flag
  (`grep isInterrupted|InferenceLimit QuerySolver.java` → nothing), recurses on the Java stack, is
  eager (materialises every solution) and does not emit the v2 trace/debug ports. This defeats the
  v3.4.0 hardening: untrusted code only needs `once(Loop)` to escape the budget.
- **Solution** (two layers):
  1. **Safety net (small)**: make `QuerySolver.solve(...)` poll `Thread.interrupted()` and a shared
     step counter/budget handed over by the v2 machine (e.g., a `ResourceGuard` object stored on the
     `QuerySolver` context) so the legacy path throws the same `InferenceLimitException` /
     `QueryCancelledException`.
  2. **Native meta-calls (the real fix)**: implement in `MachineSolver.drive()`:
     `once(G)` ≡ `(G -> true)`, `ignore(G)` ≡ `(G -> true ; true)`, `forall(C,A)` ≡ `\+ (C, \+ A)`,
     `not/1` (already native), `aggregate_all/3` (count/sum/max/min/bag/set) and `bagof/setof`
     on top of the native `findAll()` (collect `Witness-Template` pairs, then group/sort — port the
     grouping logic of `Bagof.java`/`Setof.java`), `setup_call_cleanup/3` with a cleanup frame
     (run cleanup on deterministic exit, failure and exception — reuse the catch-frame machinery),
     and express `maplist/2..5`, `foldl/4..6`, `include/3`, `exclude/3`, `partition/4`, `nth0/nth1`,
     `member/2`, `append/3`, `select/3`, `last/2` as **Prolog library clauses** loaded into the
     KnowledgeBase at construction (a `prelude.pl` resource) so they run on the machine with budget,
     cancellation, tracing and lazy backtracking for free. Keep the Java versions as the legacy-engine
     fallback and for `safe mode` parity.
- **Intervention**: feature/refactor, medium-large (staged). Tests: the budget/cancel matrix above
  must raise `InferenceLimitException`/`QueryCancelledException` for every meta-call; `ProductionAuditTest`
  additions.

### ENG-05 — `bridgeBuiltin` swallows every `RuntimeException` — **High**, robustness
- **Root cause**: `MachineSolver.bridgeBuiltin()` (`catch (RuntimeException e) { return -1; }`) turns
  any Java failure inside a built-in (NPE, `ClassCastException`, `IndexOutOfBounds`, …) into "not a
  built-in", which then falls through to `callUser` → `existence_error(procedure, name/arity)` or a
  silent failure. It would also swallow `DebugStopException`, `QueryCancelledException` and
  `InferenceLimitException` raised inside a nested sub-solve (all three extend `RuntimeException`).
- **Solution**: rethrow the three control exceptions explicitly; map any other `RuntimeException`
  to a catchable `PrologException(system_error(Message))` with the Java cause logged; keep `-1` only
  for a dedicated `NeedsSolverContextException` (the original intent of the comment "needs solver
  context / not bridgeable yet").
- **Intervention**: bugfix, small. Test: a built-in stub throwing NPE must surface as `system_error`,
  and a `DebugStopException` inside `forall/2` must abort the query.

### ENG-06 — process-global mutable state leaks across `Prolog` instances and threads — **High**, correctness/security
- **Root cause** (static, non-thread-safe fields):
  `core/system/PrologFlags.FLAGS` (static `HashMap` — `set_prolog_flag(unknown, fail)`,
  `double_quotes`, `occurs_check` affect **every** engine in the JVM; concurrent access can corrupt the
  map); `Variable.occursCheckEnabled` (static); `builtin/debug/Trace.tracingEnabled`;
  `builtin/debug/Spy.spyPoints`; `StreamManager.currentInputStream/currentOutputStream` (static
  Strings: `set_output/1` in one thread redirects `current_output` for all threads, while the
  captured output itself is thread-local); `OperatorDefinition.currentModuleContext` and
  `sharedOperatorTable`; `ClpfdPredicates.tempVarCounter` (non-atomic static int); legacy
  `ConstraintStore.getInstance()` singleton; `Profiler` counters. The CLAUDE.md sandbox guidance
  ("use a fresh `Prolog` per security domain") is undermined: sandboxed code can flip `occurs_check`
  or `unknown` for the host's other engines.
- **Solution**: turn `PrologFlags` into a per-`Prolog` instance (held by `Prolog`, reachable from
  `MachineSolver`/`QuerySolver` context and from built-ins via the solver); move `occurs_check` into
  it; make trace/spy state per engine (or per thread); make the current input/output stream names
  `ThreadLocal` like the output capture. Migrate call sites (`PrologFlags.` is referenced from 6
  files; `Variable.isOccursCheckEnabled()` from the two unifiers).
- **Intervention**: refactor, medium; risk medium (static call sites). Tests: two engines with
  different `unknown`/`double_quotes` flags; two threads with different `current_output`.

### ENG-07 — cyclic terms crash unification / copy — **Low**, limitation
- **Evidence**: `X = f(X), Y = f(Y), X = Y` and `X = f(X), copy_term(X, Y)` → `resource_error(stack_overflow)`
  (raised only after the whole query unwinds, so the program's `catch/3` cannot handle it).
- **Root cause**: `unify()`, `rename()`, `structuralEqual()` have no cycle detection; only `resolve()`
  does (`representation_error(cyclic_term)`).
- **Solution**: either document as LIM (no rational-tree support) or add a visited-pair set to
  `unify`/`structuralEqual`/`rename` when the recursion depth exceeds a threshold (cheap common path).
- **Intervention**: limitation entry now; optional robustness fix later, small-medium.

### ENG-08 — minor inaccuracies — **Low**
- `MachineSolver.raiseUnknownIfRequired` prints the `unknown=warning` message with
  `System.err.println` — violates the `StreamManager` output discipline (IDE never sees it).
- `inferenceBudget` counts drive-loop iterations (`true`, conjunction splits, internal action goals),
  not logical inferences; either document it or count only `callUser`/built-in calls.
- `throw/1` renames the ball twice (in the `throw` branch and again in `drive()`'s catch) and
  `PrologException` fills a Java stack trace on every `throw/1` (override `fillInStackTrace` for the
  error-term constructor — exceptions are control flow in Prolog).
- **Intervention**: bugfix, trivial each.

---

## 2. Structural limits

### ENG-09 — recursive term walkers cap list length at ~20–30k elements — **Critical**, limit
- **Evidence**: with `-Xss4m`, `numlist(1,30000,L)` followed by `L == L`, `sum_list`, `copy_term`,
  `assertz(big(L))`, `findall(X, member(X,L), _)`, `term_to_atom`, `msort` all raise
  `resource_error(stack_overflow)`; every 50k/100k-element operation fails; `deep(50000, T)` (a
  50 000-deep `f(f(…))`) fails. Default JVM stacks (512 KB–1 MB) fail well below 20k.
- **Root cause**: every term walker recurses into *all* arguments, so a list of N cells needs N Java
  frames (the recursion goes down the tail): `MachineSolver.resolve/rename/unify/structuralEqual/checkBodyGoals`,
  `CompoundTerm.unify/copy/resolveBindings/isGround/equals/hashCode` (`Objects.hash(functor, arguments)`
  recurses through `ArrayList.hashCode`), `TermCopier.copyTermInternal`, `Variable.occursInTerm`,
  `TermFormatter.format` (list case), `core.write.v2.TermWriter`, `KnowledgeBase.unifiable`,
  `Prolog.spliceAttributedSessionVars/extractVariablesRecursive`, `ListTerm`/`CollectionUtils` helpers.
  The `StackOverflowError → resource_error` conversion (ISS-2025-0341) only makes the crash polite.
- **Solution**: make all walkers **tail-iterative on the last argument** (loop on argument N,
  recurse only on arguments 1..N-1 — depth then equals the nesting of non-last arguments, which is
  small for lists and most data) or use an explicit stack (as `occurs()`/`collectVars()` already do).
  Give `CompoundTerm.hashCode` a cached, spine-iterative implementation. Add a regression test with a
  1 000 000-element list: `numlist/3`, `length/2`, `sum_list/2`, `msort/2`, `copy_term/2`, `==/2`,
  `assertz/1` + call, `findall/3`, `atom_to_term/term_to_atom`, `write/1` (must all succeed with the
  default JVM stack size).
- **Intervention**: refactor, medium, mechanical, high value. ~12 functions.

### ENG-10 — unbounded memory growth in deterministic execution — **Critical**, memory
- **Evidence**: `loop/1` retains O(N) memory (300 000 iterations OOM at 64 MB, 1 000 000 at 256 MB);
  any real Prolog runs it in constant space. 20 000 exhausted choice points are left behind by a
  20 000-iteration loop.
- **Root causes** (`MachineSolver`):
  1. `binding` is a name-keyed `HashMap<String,Term>` that keeps every binding for the query's
     lifetime — dead variables are never reclaimed (see ENG-16 for the architectural angle).
  2. `bind()` **always** appends to `trail`, even when no choice point exists to undo to. WAM trails
     only bindings of variables older than the newest choice point.
  3. `advance()` never pops an **exhausted** choice point: after the last alternative is taken the `CP`
     stays on `cps` (with `idx == alts.size()`), together with everything its `Alt` closures capture
     (the continuation, and for `bridgeBuiltin` the full `fsol` binding-map copy). Each `loop/1` call
     leaves one such CP (clause 1 `loop(0) :- !` fails on head unification, clause 2 is the last
     alternative), so `cps` grows linearly and cut/backtrack must walk through the garbage.
  4. Every renamed variable allocates a new `String` (`"_R" + id + "_" + name`) and a new `Variable`;
     every `rename`/`resolve` allocates a new `Atom` per compound (`new Atom(c.getName())` instead of
     reusing `c.getFunctor()`).
- **Solution** (staged):
  - **Trust-me pop**: in `advance()`, when the alternative just taken is the last one, remove the CP
    from `cps` if it is on top (keep it only while `traceGoal != null` so Redo/Fail ports are still
    emitted, or emit the Fail port eagerly). Cut barriers stay valid because they are absolute indices
    captured *before* the push and `cut()` only removes CPs above the barrier.
  - **Conditional trailing**: give `Variable` an `int serial` (assigned from `renameCounter` in
    `rename()`/`renameRule()`, 0 for query variables, from the anonymous counter for `_`), record the
    current `renameCounter` in each `CP`, and trail a binding only if `var.serial < topCP.serial`
    (and always inside `findAll`/`\=`/catch extents, which use explicit marks — keep a
    `forceTrail` depth counter for those).
  - Reuse `c.getFunctor()` in `rename()`/`resolve()`; intern renamed names lazily (or switch to
    ENG-16).
- **Intervention**: refactor, medium; risk medium (choice-point invariants, trace tests in
  `MachineSolverTest`/`DebuggingTest`). Test: `loop(3000000)` under `-Xmx64m` must succeed; `cps.size()`
  bounded in a deterministic recursion.

### ENG-11 — registry built-in calls copy the entire binding map (quadratic time, OOM) — **Critical**, performance/memory
- **Evidence**: `loop2/1` (one `atom_length(abc,_)` per iteration) → OOM with a 2 GB heap at N = 10 000
  after 27 s, while the same loop without the built-in runs N = 40 000 in 198 ms.
- **Root cause**: `bridgeBuiltin()` does `new HashMap<>(binding)` **per call** (O(total bindings)),
  every built-in returns solution maps that are again full copies (`new HashMap<>(bindings)` in
  `Member`, `Between`, `Length`, … — `Member` even copies twice per element), `applySolution()`
  iterates the whole returned map, and the exhausted CP (ENG-10.3) retains each `fsol` copy: memory
  is Σ(bindings at call i) = O(N²).
- **Solution**: pass built-ins a **resolved goal** (structure-sharing dereferenced copy — reuse
  unchanged sub-terms exactly like `CompoundTerm.resolveBindings` does) plus a **small map** containing
  no entries (all remaining variables in the resolved goal are unbound), so the built-in only returns
  the bindings it created; `applySolution` then binds a handful of entries. Keep the current
  "unresolved goal + full map" path only for the destructive built-ins that need object identity
  (ISS-2025-0317): `setarg/3` / `nb_setarg/3` (`SetArg.java` is the sole caller of
  `CompoundTerm.setArgument`) — or make them native. For deterministic built-ins (exactly one solution)
  do not push a CP at all. `LayeredMap` (already in `core.engine`) can serve as the overlay if a
  read-through view is needed.
- **Intervention**: refactor, small-medium, confined to `MachineSolver.bridgeBuiltin/applySolution`.
  Test: `loop2(200000)` under `-Xmx128m` in < 2 s; ISS-2025-0317 `setarg` test must keep passing.

### ENG-12 — nondeterministic built-ins are materialised eagerly — **High**, memory/performance
- **Evidence**: `between(1,2000000,X), X >= 2000000` → OOM at 256 MB (2 s at 2 GB); `member/2` on a
  list copies the binding map twice per element up front; `forall(member(X,L), …)` on 100k elements
  blows up.
- **Root cause**: the `BuiltIn.execute(goal, bindings, solutions)` contract returns *all* solutions
  as a `List<Map>`; `bridgeBuiltin` turns the list into a choice point. `between/3`, `member/2`,
  `length/2`, `nth0/nth1`, `select/3`, `append/3`, `clause/2`, `sub_atom/5`, `current_op/3`,
  `repeat/0` (ENG-01) all pay O(#solutions × #bindings) before the first solution is used.
- **Solution**: (a) add a lazy contract — `interface LazyBuiltIn { Iterator<Map<String,Term>> solve(goal, bindings); }`
  (or a `SolutionSink` callback) consumed by a CP whose alternative pulls the next solution on demand;
  migrate the generators listed above (`between/3` and `repeat/0` natively in the machine); (b) move
  the list predicates (`member/2`, `append/3`, `select/3`, `nth0/nth1`, `last/2`, `reverse/2`) to
  the Prolog prelude of ENG-04 — with ENG-13 they are as fast as Java and become lazy, cut-transparent
  and traceable.
- **Intervention**: feature, medium. Tests: `between(1, inf, X), X > 10^7, !` under `-Xmx64m`;
  `member(X, BigList), X == last, !` memory bound.

### ENG-13 — clause selection is O(#clauses) per call with a full list copy — **High**, performance/memory
- **Evidence**: on a 20 000-fact table, `f(0,_)` (first clause matches) costs 3.9 ms per call — the
  same as `f(19999,_)` (4.1 ms): the cost is per-call setup, not unification.
- **Root cause**: `callUser()` → `clausesFor()` → `KnowledgeBase.getRulesForPredicate()` copies the
  predicate's whole clause list under `synchronized` (`new ArrayList<>(indexed)`), then `callUser`
  allocates one `Alt` lambda per clause *before* the first head unification; `renameRule()` copies
  head **and body** of every candidate before trying the head, even for ground facts
  (`Rule.isGroundFact()` exists but is unused on this path). No first-argument indexing is used by the
  v2 engine although `KnowledgeBase` maintains `firstArgIndex` (and an unused `multiArgIndex`) for
  every clause — three parallel index structures, i.e. 2× wasted memory and assert/retract work.
- **Solution**:
  1. Copy-on-write clause lists in `KnowledgeBase`: an immutable snapshot per predicate, replaced
     atomically on assert/retract — the logical update view comes for free, reads need no lock and no
     copy.
  2. The CP holds `(snapshot, index)` and advances lazily instead of a pre-built `List<Alt>`.
  3. Pre-check functor/arity and first-argument compatibility (atom/number/functor of the goal's
     dereferenced first argument vs. the clause head) before renaming; rename the **head only**, unify,
     rename the body only on success sharing the same variable map; skip renaming for
     `Rule.isGroundFact()`.
  4. Re-land first-argument indexing on the v2 path (ISS-2025-0344 made misses safe): use a
     type-faithful key (`Number.getValue()` currently maps `1` and `1.0` — and all integers beyond 2^53 —
     to the same key; harmless but wasteful) and drop `multiArgIndex` unless a use case appears.
- **Intervention**: performance refactor, medium. Tests: fact-table lookup ≥ 20× faster; `nrev`
  benchmark; existing `retract`/logical-update-view tests (`ISS-2025-0396`) must keep passing.

### ENG-14 — arithmetic hot path allocates on every evaluation — **Medium**, performance
- **Root cause**: `evalNum()` deep-copies the expression via `resolve()` (allocating a `HashSet` and
  a new `CompoundTerm` per node), then `ArithEvaluator.eval(…, new HashMap<>())` allocates an evaluator
  and an empty map; `numRel()` compares through `BigInteger` even when both operands are small
  `long`s (`bigIntegerValue()` allocates); `Number` carries three representations (48 bytes) and
  every result is a fresh object; `==`/`\==` also go through two full `resolve()` copies.
- **Solution**: evaluate directly against the binding store through a deref callback (no copy);
  `long` fast paths with `Math.addExact/subtractExact/multiplyExact` and BigInteger fallback on
  overflow; `numRel` on primitives when both fit in `long`; a small-integer cache
  (`Number.valueOf(long)` for −128..1024); `structuralEqual` walking with on-the-fly deref instead of
  `resolve()`.
- **Intervention**: performance, small-medium. Benchmark: `nrev`/`loop` ≥ 2× on arithmetic-heavy code.

### ENG-15 — miscellaneous per-step overheads — **Low**, performance
- `key()` and `KnowledgeBase.getRulesForPredicate` build `name + "/" + arity` Strings per call — use
  a `(name, arity)` key object or a two-level map.
- `CP` constructor calls the legacy `Trail.mark()` (ThreadLocal lookup) per choice point and
  `advance()` calls `Trail.rollbackTo()` per alternative even when the legacy trail is empty — check
  `Trail.size() == 0` cheaply or keep a local counter.
- `collectVars()` uses `List.contains` (O(n²) in the number of query variables).
- `drive()` checks `Thread.currentThread().isInterrupted()` on every step — poll every 1024 steps.
- `Variable("_")` builds `"_G" + counter` Strings for every anonymous variable.
- **Intervention**: micro-optimisations, small each; bundle with ENG-13/14.

### ENG-16 — name-keyed binding model (architecture) — **High** (long-term), performance/memory
- **Root cause**: variables are identified by `String` name (`Variable.equals/hashCode` by name),
  bindings live in a `HashMap<String,Term>`, dereferencing is a hash lookup, clause renaming
  allocates strings, bindings are never garbage-collected (ENG-10.1), and the `_R{id}_{name}` scheme
  can collide with user variables of the same shape (`Prolog.mapInternalVariablesToQueryVariables`
  even relies on `contains("_" + name)` heuristics on the legacy path). This is the root of the
  188 KLIPS baseline and of most memory findings.
- **Solution**: WAM-style binding *in the variable object* — a `Term ref` field on `Variable`, a
  trail of `Variable` references with conditional trailing by serial (ENG-10), O(1) dereference by
  pointer chasing, automatic reclamation of unreachable bindings by the JVM GC. Keep a
  `Map<String,Term>` adapter view for the registry built-ins (ENG-11's resolved-goal handoff makes the
  adapter trivial: the map only needs the goal's own variables). The legacy engine keeps the map model.
- **Intervention**: architecture, large (a v4-engine track). Recommended **after** ENG-10/11/12/13,
  which deliver most of the practical benefit at a fraction of the risk.

### ENG-17 — dead / legacy code and unused indexes — **Low**, maintenance
- `core/engine/CompiledClause.java`, `core/engine/Interpreter.java` (no callers), `core/terms/AtomTable.java`
  (2 callers), `LayeredMap` (legacy only), `KnowledgeBase.multiArgIndex` (no callers outside the class),
  the `firstArgIndex` maintained but unused by the default engine (ENG-13.4).
- **Intervention**: housekeeping, small; do together with ENG-13.

---

## 3. Implementation plan (waves)

Ordering minimises risk: each wave is independently releasable and verified by `mvn test` +
`./test_all_examples.sh`.

| Wave | Findings | Type | Version |
|---|---|---|---|
| **1 — correctness quick wins** | ENG-01 (`repeat`), ENG-02 (`Number(double)` + audit), ENG-03 (`length/2`), ENG-05 (bridge exceptions), ENG-08 (minor) | bugfix | 3.6.1 |
| **2 — deep-structure limit** | ENG-09 (tail-iterative walkers, 1M-element list test) | refactor | 3.6.2 |
| **3 — memory model of the machine** | ENG-10 (trust-me pop, conditional trailing, functor reuse), ENG-11 (resolved-goal bridge, no CP for deterministic built-ins) | refactor | 3.7.0 |
| **4 — meta-calls & generators on the machine** | ENG-04 (safety net in `QuerySolver` first, then native `once/ignore/forall/aggregate_all/bagof/setof/setup_call_cleanup`, Prolog prelude for list/apply predicates), ENG-12 (lazy built-in contract, native `between/repeat/length`) | feature | 3.7.0 / 3.8.0 |
| **5 — clause selection & arithmetic** | ENG-13 (COW clause lists, lazy CP, head-first renaming, first-arg indexing), ENG-14, ENG-15, ENG-17 | performance | 3.8.0 |
| **6 — isolation** | ENG-06 (per-engine flags/trace/streams) | refactor | 3.8.0 |
| **later** | ENG-16 (object bindings), ENG-07 (rational trees) | architecture | 4.0 |

Acceptance benchmarks to add under `src/test/java/.../test/perf/` (guarded so they don't slow the
suite, e.g. `@Category` or a system property): `loop(3000000)` under `-Xmx64m`; `loop2(200000)`
under `-Xmx128m`; 1 000 000-element list round trip; `between(1, inf, X), X > 10^7, !` under
`-Xmx64m`; `nrev30` KLIPS printed (target ≥ 1 MLIPS after wave 5); fact-table lookup ≤ 50 µs.

## 4. Handoff notes for the implementer

- Follow `CLAUDE.md`: allocate ISS numbers from **ISS-2025-0423** upward (grep `CHANGELOG.md` and
  `START_CHANGE` tags first), wrap every change in `START_CHANGE/END_CHANGE` tags, add one
  `@Test` per fix in `BugFixVerificationTest` (or a new `Engine*Test` class — name must match
  `*Test`), update `CHANGELOG.md`, `docs/tracking/track-issues.md`, `track-limitations.md`,
  `track-release-notes.md`, bump `pom.xml`, and run `mvn test` (baseline 935/935) plus
  `./test_all_examples.sh` (20/20) after each wave.
- While debugging, `MachineSolver` disables fast paths so ports fire through the bridge — any new
  native construct (ENG-04/12) must emit Call/Exit/Fail/Redo when `debugController != null` or
  `Trace.isTracingEnabled()`, and a new choice-point kind must carry `traceGoal/traceDepth`.
- Keep `InferenceLimitException`/`QueryCancelledException`/`DebugStopException` as plain
  `RuntimeException`s (not `PrologException`) — ENG-05 must preserve this trust model.
- The reproduction harness used for this report lives in the session scratchpad (`Bench.java`);
  its queries are reproduced verbatim in the tables above so they can be turned into tests.
