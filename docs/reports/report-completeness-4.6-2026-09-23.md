# Completeness program — 4.5.0 → 4.6.0 (2026-09-23)

Status: **DONE** — released as 4.6.0 (2026-09-24; waves Q1..Q7 implemented, Q1..Q6 independently
verified, Q7 awaiting verification; not committed). Originally: the residue of the 4.5 production-readiness program
(`report-production-readiness-2026-09-23.md`, limitations LIM-037/038 and LIM-041..046) plus the
gaps found while verifying 4.5.0, turned into implementation waves Q1..Q7. Each wave is
implemented by one agent and verified independently (full `mvn test`, `./test_all_examples.sh`,
the wave's repros, a quick benchmark check). **Agents never commit.**

Baseline (4.5.0, JDK 25, commit dd46a14): `mvn test` 1452/1452; examples 20/20 with the documented
successful-query counts (the local `test_all_examples.sh` runs the CLI with `--demo`).

Reference semantics: **ISO 13211-1 first, SWI-Prolog 9 where ISO is silent** — unchanged. Every
deliberate difference goes into `docs/references/ref-deviations.md` (the one place, since 4.5.0).

ISS allocation (grep CHANGELOG + `src/` first; the highest used at 4.5.0 is ISS-2025-0675):
Q1 0680–0709 · Q2 0710–0729 · Q3 0730–0744 · Q4 0745–0759 · Q5 0760–0774 · Q6 0775–0789 ·
Q7 0790–0799. Next free limitation: **LIM-047**.

General rules for every wave (from CLAUDE.md — read it, and read
`docs/reports/report-engine-v4-progress.md` §3 invariants before touching `core.engine.v4`):
- `START_CHANGE/END_CHANGE` tags with the ISS id; one JUnit test per fix that fails without it;
  test classes must match `*Test`; timing checks are counter- or ratio-based, never tight
  absolute bounds.
- Errors are `error(Formal, Context)` built with `core.engine.v4.Errors`; argument faults raise.
- Never `System.out`; never `Term.toString()` for user-visible output (use `core.engine.v4.Writer`).
- Keep the full suite green at the end of the wave; update `docs/tracking/track-issues.md`,
  `track-limitations.md` (close or shrink the LIM entries the wave resolves), the references and
  the manual (`tools/build-manual.sh`), and add a wave record section to this file (§10+).
- **Resources** (the dev box is shared and memory-tight; two sessions died of OOM in 4.5):
  `MAVEN_OPTS=-Xmx512m`, tests `-DargLine=-Xmx1g`, `mvn -o`, one JVM at a time, no parallel
  agents, no background builds; probe queries wrapped in `once/1` + `timeout`.

---

## 1. Wave Q1 — ISO error terms in the extended libraries and the registry (LIM-038)

| # | Defect | Fix |
|---|---|---|
| Q1.1 | 315 probed goals of the bridged extended libraries raise a **message atom** (`catch(xml_parse(1,X),E,true)` → `E = 'xml_parse: argument must be an atom.'`), so `catch(G, error(type_error(_,_),_), R)` never matches. Families: jdbc 88, filesystem 28, crypto 26, network 26, http 22, io (bridged half) 22, persistence 18, datetime 14, threading 14, logging 12, regex 12, csv 8, dcg 8, json 8, os 4, xml 4. | Convert every argument check to `Errors.instantiation/type/domain/existence/permission/representation`; keep the English text only in the context term (`context(Name/Arity, Message)` as SWI). Drive the sweep with a probe harness (every registered indicator × arities 1..4 × {all unbound, wrong-type first argument}; classify the result) kept as a JUnit test (`ExtendedLibraryErrorsTest`) that fails on any message-atom error. A host failure (I/O, SQL, network) is not an argument fault: map it to `error(existence_error(...))`/`error(permission_error(...))`/`error(io_error(Op, Culprit), _)`/`error(system_error(Msg), _)` as SWI does. |
| Q1.2 | `BuiltInRegistry.isBuiltIn(Name, Arity)` answers true for arities a built-in does not implement: `char_code(X)` → `'char_code/2 requires exactly 2 arguments'` (1 170 of 2 326 probed goals). ISO: `existence_error(procedure, char_code/1)`. | Give every registered name its exact arity set (declared at registration; derive it from the existing guards), make `isBuiltIn` exact, and let an unknown arity fall through to the normal unknown-procedure path. `permission_error` on `assertz` must still fire for every REAL arity. |
| Q1.3 | Worker threads stopped by the budget report `exception(inference_limit_exceeded)` (an atom). | Keep the control-exception trust model for the query's own thread; for `thread_join` status use `exception(error(resource_error(inference_limit), _))` or document why not (§8 of the 4.5 spec: budget is not catchable in-query — the join status is outside the query, so a term is fine). |

Done means: the probe test reports **0** message-atom errors and **0** arity-guard messages;
`EngineV4IsoErrorsTest` gains one row per family; LIM-038 closed.

## 2. Wave Q2 — missing standard predicates

| # | Missing / wrong | Fix |
|---|---|---|
| Q2.1 | `library(solution_sequences)`: `limit/2`, `offset/2`, `order_by/2`, `distinct/1,2`, `call_nth/2` → existence_error. | Native generators (lazy; `limit/2` must stop the inner goal and run its cleanup — reuse P1.10's `cutQuietly`). SWI semantics, including `order_by([asc(X),desc(Y)], G)`. |
| Q2.2 | `garbage_collect/0`, `garbage_collect_atoms/0`, `trim_stacks/0` missing. | Succeed (optionally `System.gc()` for `garbage_collect/0`, off by default in safe mode). |
| Q2.3 | `rdiv`, `rational/1,3`, `rationalize/1` documented but absent (`X is 1 rdiv 3` → type_error(evaluable)). | Decide per §8: either implement rationals (a `Rational` number kind in `core.arith.v2`, SWI 9 `prefer_rationals=false` default, `1r3` syntax) or remove them from every document and the evaluator's function list. The spec's default: **implement** `rdiv`, `rational/1`, `rationalize/1`, `numerator/1`, `denominator/1` with a BigInteger-pair value type; the `1r3` literal syntax is optional. |
| Q2.4 | `foldl/4` with a yall lambda leaves a choice point: `foldl([X,A0,A]>>(A is A0+X),[1,2,3],0,S)` answers `S = 6 ;` then `false`. SWI is deterministic. Check `maplist/2..7`, `include/3`, `exclude/3`, `partition/4`, `aggregate_all/3` with lambdas too. | Find the leftover choice point (yall copy? the library clause order?) and make them deterministic when the closure is; a test with `deterministic/1`-style checking (count choice points via a probe hook or check `call_cleanup` fires on exit). |
| Q2.5 | `print_message/2` has no `message_hook/3` and no `prolog:message//1` (LIM-043); informational messages go to `current_output` (SWI: `user_error`). | Implement both hooks and the `user_error` routing (verbosity `silent` respected). |
| Q2.6 | `format/2,3` column stops count from the start of the format call, not the stream's column (LIM-043). | Track line position per stream (`PrologStream` already counts for `line_position/2`) and use it for `~t~|`. |
| Q2.7 | `library(aggregate)`: `aggregate/3,4` (bagof-style, grouping by free variables) — check presence; `foldall/3` not needed. | Implement `aggregate/3,4` per SWI if absent. |
| Q2.8 | Sweep: every predicate named in `BUILTIN_PREDICATES_REFERENCE.md` and the manual must exist with the documented arities (P4 found `cot`, `acot`, `lsb`, `popcount` documented-but-missing). | A test that parses the reference's indicators and checks `current_predicate`/`predicate_property(_, built_in)`/evaluable existence for each; fix or un-document every miss. |

## 3. Wave Q3 — loader, modules and reader residue (LIM-044)

| # | Gap | Fix |
|---|---|---|
| Q3.1 | No `multifile/1` semantics: reconsulting a file wipes every user predicate it defined, even clauses other files added. | Track clause ownership per source file; reconsult removes only that file's clauses; `multifile/1` declared predicates accept clauses from several files; `discontiguous/1` warnings as SWI (warning, not error). |
| Q3.2 | `goal_expansion/2` not applied. | Apply `user:goal_expansion/2` (and module-local) to clause bodies at load time, to fixpoint with a depth cap, as SWI. |
| Q3.3 | `library(X)` resolves only the prelude modules and a fixed list; no `file_search_path/2`, no `absolute_file_name/3` search. | `file_search_path/2` (dynamic, multifile, with `library`, `swi`, `foreign` defaults pointing at the prelude and a user dir), `absolute_file_name/2,3` with `file_type`, `access`, `extensions`, `relative_to`, `solutions`; `use_module(library(X))` goes through it. |
| Q3.4 | `use_module(File, Imports)` imports the whole module. | Honour the import list, `except/1` and `as` renaming. |
| Q3.5 | `(a|b)` reads as `(a;b)` (SWI 7+: `'|'(a,b)` at priority 1100, and `;` inside DCG bodies treats `'|'` as alternation); `as` is not an operator. | Reader: bar as infix `'|'` at 1100; the DCG translator and `call/1` treat `'|'/2` as `;/2` in bodies (SWI does). Add `as` with the priority and type SWI 9 gives it (`current_op(P, T, as)` in SWI — the implementer confirms the value from the SWI manual's operator table and cites it in the record; do not guess). Then `:- table p/1 as subsumptive` parses. |
| Q3.6 | `make/0` does not track included files; `initialization(G, main)` halts only under the CLI. | Track `include/1` dependencies for `make/0`; for embedders, document (keep) — or expose a `Prolog.runMain()` API returning the exit code. |
| Q3.7 | `term_position/1` is the start only; `subterm_positions/1`, `comments/1` answer nothing useful. | Implement `subterm_positions` (the SWI position terms: `From-To`, `string_position`, `brace_term_position`, `list_position`, `term_position`, `parentheses_term_position`) and `comments/1` in `core.parser.v2`. |
| Q3.8 | Load lock (LIM-045): only a `thread_join` wait is recognised; a directive waiting on `thread_get_message` or `concurrent_*` workers that load files deadlock. | Make the lock per FILE rather than per engine (two threads loading different files proceed; the same file loaded concurrently waits, re-entrant for the owner), and detect a wait on a thread that waits for a file this thread is loading (cycle → `permission_error(load, source_sink, F)` with a clear message instead of a hang). |

## 4. Wave Q4 — threads and tabling residue (LIM-045, LIM-044 tabling part)

| # | Gap | Fix |
|---|---|---|
| Q4.1 | Missing `thread_signal/2`, `thread_statistics/3`, `message_queue_property/2`, `thread_send_message/3`, `mutex_property/2`, `thread_create_in_pool/4` + `thread_pool_create/3`. | Implement per SWI; `thread_signal/2` runs the goal in the target thread at its next port/inference check (the machine polls a per-thread signal queue where it polls the guard) — must be safe with the trail, i.e. run as an inserted goal, not asynchronously. |
| Q4.2 | A mutex still held when its thread ends is released silently (SWI warns). | Release + `print_message(warning, ...)`. |
| Q4.3 | Tabling store not safe for two threads producing the same table. | Shared tables with SWI's "completed tables are shared, incomplete tables are owned; a second thread waits for completion" model (or thread-local tables by default, with `:- table p/1 as shared` opting in). Choose one, document it in ref-deviations. |
| Q4.4 | Mode-directed tabling: `lattice(PI)`, `po(PI)` raise domain_error; a call with a bound moded argument evaluates with it bound (SWI: evaluates free, unifies the best answer); `min`/`max` use standard order. | Implement `lattice/1` (join predicate), `po/1` (partial order predicate), free evaluation of moded arguments, `min`/`max` by standard order of terms (that IS SWI's behaviour — verify) — tests: shortest path with `min`, longest path lattice, `po(<)` Pareto front. |
| Q4.5 | Tabled negation raises `permission_error(negate, incomplete_table, G)` for non-stratified programs (LIM-046); no `tnot/1`, no `undefined/0`. | **Minimal WFS**: `tnot/1` for tabled goals; delay a negative literal whose table is incomplete; answers carrying a non-empty delay list are reported as conditional (`undefined`) — the simplification of SLG resolution SWI documents. If the wave judges full SLG infeasible within its budget, implement `tnot/1` for the stratified case + `undefined/0` + the detection that exists today, and keep LIM-046 for the rest — record the decision. |

## 5. Wave Q5 — CLP(FD) residue (LIM-041)

| # | Gap | Fix |
|---|---|---|
| Q5.1 | Answers print domains, not residual constraints (`X #> Y` shows `X`/`Y` unconstrained; SWI `Y#=<X+ -1`). | `copy_term/3` attribute goals for clpfd (`attribute_goals//1`), used by `Answer`/`residualGoals` and the CLI: print the propagators still active on the answer's variables in SWI's normal form (`X#>=Y+1` → SWI prints `Y#=<X+ -1`; match SWI for the common forms: linear equalities/inequalities, `#\=`, `in`, `all_different`/`all_distinct`, reified). |
| Q5.2 | Missing globals: `circuit/1`, `cumulative/1,2`, `disjoint2/1`, `lex_chain/1`, `chain/2`, `automaton/3`, `zcompare/3`, `fd_degree/2`, `global_cardinality/3` options. | Implement with at least SWI-level propagation for `circuit/1` (no subtours) and `cumulative/2` (time-table). Tests: knight's tour 6×6 via `circuit`, a small job-shop via `cumulative`. |
| Q5.3 | 64-bit coefficient limit: `X*10^20 #= Y` raises `representation_error(max_integer)`. | BigInteger coefficients in linear propagators (a slow path only when a coefficient or bound overflows long). |
| Q5.4 | `labeling([min(E)])` branch-and-bound uses the eager recursive labeler per round. | Make it iterative/lazy on the same Generator; test on a 60-variable problem that would overflow a recursive labeler. |

## 6. Wave Q6 — performance residue (LIM-042, LIM-037)

| # | Gap | Fix / target |
|---|---|---|
| Q6.1 | Runtime-built goals (`call/N`, findall/forall/\+/once goals, top-level) resolve on every call. | Per-functor inline cache in `Machine` keyed by (name, arity, module) invalidated by `Engine.dispatchStamp()`. Target: `call/N` loop of 1e6 ≥ 30 % faster. |
| Q6.2 | `maplist/2..7`, `foldl/4..7`, `include/3`, `exclude/3`, `partition/4,5` are Prolog with one meta-call per element (≈1.5× a hand recursion). | Native iteration that keeps the four ports and choice points of the closure (the closure runs as a pushed goal; the iteration state lives in a frame, not a Java loop). Target: within 1.1× of the hand-written recursion; traces identical to the Prolog version's ports for the closure. |
| Q6.3 | Bridged nondeterministic io built-ins (`stream_property/2`, `current_stream/3`, …) materialise every solution; `open/3,4`, `close/1,2`, `read/1,2`, `read_term/2,3`, `set_stream/2`, `seek/4`, the byte I/O, `portray_clause/1,2` still go through the adapter (LIM-037). | Native (`NativeIo`/`NativeRead`), lazy generators for the enumerating ones. The extended libraries stay bridged (no hot-path claim). |
| Q6.4 | `predsort/3` runs each comparison as a nested drive. | Reuse one sub-machine per sort, or run the comparisons through the goal stack; target ≥ 2× on predsort 1e5. |
| Q6.5 | Benchmark table (§9 of the 4.5 spec + the P2 extras) re-measured before/after; no regression. | — |

## 7. Wave Q7 — test hardening + release 4.6.0

1. Every new predicate of Q1–Q6 in `BUILTIN_PREDICATES_REFERENCE.md` + manual; `ref-deviations.md`
   updated; LIM entries closed/shrunk; CLAUDE.md refreshed (numbers, new invariants, the per-file
   load lock, the thread signal polling, the tabling ownership model).
2. `FamousPrologProgramsTest` gains: a `solution_sequences` pipeline, a `file_search_path`
   library load, a tabled shortest path with `min`, a CLP(FD) knight's tour.
3. A final ISO/SWI conformance sweep: run the probe harness of Q1 and the documentation sweep of
   Q2.8 again, zero misses.
4. Release: pom 4.6.0, CHANGELOG, `track-release-notes.md`, README; the verifier runs the numbers.

## 8. Decisions for this program

- Reference: SWI-Prolog 9 where ISO is silent (unchanged). Rationals: implemented (Q2.3), not
  preferred (`prefer_rationals = false`), so `X is 1/3` stays a float as today.
- Library errors: the context term is `context(Name/Arity, Message)`; host failures map to SWI's
  `io_error/2`, `existence_error/2`, `permission_error/3`, `system_error/1` formals.
- Tabling across threads (Q4.3): the wave chooses; the choice goes into `ref-deviations.md`.
- WFS (Q4.5): minimal viable per the table; the extent actually delivered is recorded, the rest
  stays in LIM-046.

## 10+. Wave records

(Each wave appends its record here: what was done, ISS ids, deviations from this spec and why,
measured numbers before/after, open items.)

## 10. Wave Q1 — record

Status: **DONE** (implementation agent; not committed — awaiting independent verification).

Probe harness: `src/test/java/it/denzosoft/jprolog/core/engine/v4/ExtendedLibraryErrorsTest.java`
(every registered name x arities 0..4 x {all unbound, first argument `f(x)`}; each goal runs as
`open(Empty,read,S), set_input(S), (once(catch(G,E,true)) -> ...), close(S)` so no probe reads
the real stdin; the report goes to `target/extended-library-errors.txt`). Helpers:
`core.engine.v4.Errors` gained the library builders (`error(Formal, context(Name/Arity, Msg))`,
`host(...)` for host failures) and `builtin.LibArgs` the argument checks. Skip list: `halt`,
`thread_exit`, `thread_get_message` (blocks on the own queue), `trace`, `make` — every other name
validates its first argument before any host effect, so the probe arguments cannot open a socket,
spawn a process or touch a file.

| Step | ISS | Status | Measured (message atoms / arity-guard messages) |
|---|---|---|---|
| Harness | ISS-2025-0680 | done | before: **368 / 1 089** of 3 780 goals (+9 stdin timeouts, fixed by the empty-file input) |
| Errors / LibArgs helpers, host mapping | ISS-2025-0681 | done | |
| Q1.2 exact registry arities (273 undeclared names declared) | ISS-2025-0685 | done | 224 / 4 (the 4 were `persist/1` messages the regex mistook) |
| xml, regex, crypto | ISS-2025-0682/0683/0684 | done | 0 |
| csv, datetime, json, os, filesystem, http | ISS-2025-0686..0691 | done | 0 |
| jdbc, network, persistence, dcg, debug | ISS-2025-0692..0696 | done | 0 |
| reachable remainder: bridged io, graph, spy/trace | ISS-2025-0697 | done | 0 |
| Q1.3 join status `exception(error(resource_error(inference_limit), _))` | ISS-2025-0698 | done | |
| `EngineV4IsoErrorsTest` rows (26: one+ per family, two Q1.2) | ISS-2025-0699 | done | |
| **Final** | | | **0 / 0** of 3 825 goals |

By family before -> after (message atoms): jdbc 88->0, crypto 36->0, http 30->0, persistence
30->0, filesystem 28->0, os 22->0, network 18->0, datetime 14->0, dcg 12->0, regex 12->0,
debug 10->0, control 8->0, csv 8->0, json 8->0, xml 7->0, io 37->0 (the io/control ones were
arity guards the table now answers as unknown procedures).

Deviations from the spec (all in `ref-deviations.md` §2):
- The context is `context(Name/Arity, Message)` everywhere in the libraries except three sites
  that have no predicate at hand (graph edge parser, JDBC and socket handle tables): a plain atom.
- SQL failures map to `system_error(Message)` (no finer ISO/SWI formal); text arguments accept a
  string as well as an atom.
- An embedder's `registerBuiltIn(name, b)` WITHOUT arities keeps the historical any-arity answer
  (compatibility); `registerBuiltIn(name, b, arities...)` is exact. Every name `new Prolog()`
  registers is declared, pinned by `testISS0685_EveryRegisteredNameDeclaresItsArities`.
- Out of scope, recorded for later waves: the FFI (`java_*`) FAILS on a bad argument (22 probed
  goals) instead of raising; several library predicates succeed or fail on an unbound OUTPUT-side
  argument (the harness's `ok`/`fail` columns, 175/130) — not message atoms.

Numbers: `mvn -o test` 1457/1457 (1452 + 5 new methods: the harness, three Q1.2 tests, the Q1.3
test); `./test_all_examples.sh` 20/20 with the documented counts.

## 11. Wave Q2 — record

Status: **DONE** (implementation agent; not committed — awaiting independent verification).

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| Q2.1 solution_sequences | 0710 | done | `EngineV46StandardPredicatesTest` (4 methods) | `NativeSequences` + `Machine.pushFiltered`: lazy, the goal runs in the caller's continuation; the last solution of `limit/2` / `call_nth(G, N)` cuts the goal with the machine's `cut()` (same semantics as `!`: cleanups run, a cleanup's error propagates) — `cutQuietly` is for ABANDONED runs, not this; `distinct` uses variant keys (`NativeControl.variantKey`); `order_by` eager + stable |
| Q2.2 gc stubs | 0711 | done | `testISS0711_*` | succeed; no `System.gc()` (shared JVM) |
| Q2.3 rationals | 0712 | done | 3 methods | `Rational.of` (normalised; integer when d = 1), exact `+ - * / rdiv min max ^int`, `rational/rationalize/numerator/denominator`, exact `=:=`/standard order vs integers, float compare vs floats; `1r3` read by the v2 lexer + `number_codes`/`atom_number` (decision: **implemented**, SWI `compatibility` syntax; legacy parser not); writer `1r3`; `argKey` = the rational itself; `.jpc` already had `TERM_RATIONAL` (reader now normalises); `prefer_rationals` flag (settable), `rational_syntax` (read-only) |
| Q2.4 deterministic meta-calls | 0715 | done | 2 methods | cause: NOT yall — apply's clauses differ in the 2nd argument, the 1st-arg index cannot tell `foldl(_,[],A,A)` from `foldl(G,[X|Xs],..)`. Fix: the ISS-2025-0668 tracing-only look-ahead now always runs, allocation-free (`nextMayMatch`/`argClash`), and skips non-matching clauses |
| Q2.5 print_message hooks | 0713 | done | 1 method (+2 P4.14 tests updated) | `prolog:message//1`, `message_hook/3` (also `silent`), `verbose` flag, all kinds on `user_error`; recursion guard; DCG `M:NT` now translates to `M:NT'` (was a `:/4` predicate) |
| Q2.6 format column stops | 0714 | done | 1 method | `ColumnPrintStream` around System.out/err (re-wrapped on swap) and the `with_output_to/2` capture; `line_position(user_output)` exact; format resolves the stream first |
| Q2.7 aggregate/3,4 | 0716 | done | 1 method | pushes `bagof`/`setof` + `'$aggregate_list'/3`; also `aggregate_all/4`; compound templates; exact rational sums |
| Q2.8 documentation sweep | 0717, 0718 | done | `DocumentedPredicatesTest` (2 methods) | 9 misses: implemented `file_base_name/2`, `file_directory_name/2`, `file_extension/2`, `shell/2`, `setenv/2` (+`unsetenv/1`); un-documented `format_date/3`, `parse_date/3`; `include/1`, `meta_predicate/1` headings marked `(directive)`. Evaluable list: all 60+ names evaluate |

Numbers: `mvn -o clean compile` ok; `mvn -o test` **1472/1472** (1457 + 15: 13 in
`EngineV46StandardPredicatesTest`, 2 in `DocumentedPredicatesTest`; `EngineV4IsoErrorsTest` gained
12 rows); `./test_all_examples.sh` 20/20 with 2,0,0,1,1,0,0,0,0,0,2,1,0,0,2,0,0,0,0,0.
`safe-mode-allowlist.txt` gained the 18 new natives (none touches the host; `setenv`/`unsetenv`
are registry `os` built-ins, denied in safe mode) and lost `registry rational`.

Benchmarks (CLI, `statistics(cputime)`, -Xmx512m, shared noisy box; 4.5.0 clone vs this tree,
3 rounds, first-round warm-up outlier dropped): nrev30 x 20 000 (failure-driven) 2.36/2.37 vs
2.25..2.39 s; `loop(3 000 000)` 0.92/0.97 vs 0.92..0.98 s; arith 1e6 (`is` with `* mod // rem`)
1.00/1.20 vs 1.04..1.11 s; foldl 3e5 0.38/0.47 vs 0.31..0.45 s — no regression from the rational
kind. The look-ahead's worst case (`walk(G, [_|T])`, 3e6 calls where every call scans the other
clause): 0.70..0.85 vs 0.69..0.79 s (≈ +5 %, within noise), and those calls no longer leave a
choice point each.

Deviations (all in `ref-deviations.md` §4a): `limit/2`/`offset/2` counts must be integers (or
`infinite`); rational `**` is a float; `1r3` only on the v2 reader; `rationalize/1` by exact
continued fractions; `print_message/2` renders SWI's own messages only for format/error/unknown
terms and hands a hook `['~w'-[Line], nl, ...]` lines; `garbage_collect/0` without `System.gc()`;
`setenv/2` is an overlay; the look-ahead is shallow (principal functor per argument), not JIT
multi-argument indexing. The "informational `print_message/2` to current output" deviation is gone.

Findings for later waves:
- Q3: `current_predicate(M:N/A)` raises `type_error(predicate_indicator, M:N/A)`;
  `:- multifile user:message_hook/3` does not make the predicate defined (a call raises
  `existence_error`, SWI fails); a clause with a qualified head (`prolog:foo(1).`) is stored as a
  flat `:/2` clause and `prolog` is not a module (`current_module/1`, `predicate_property/2` do not
  see it); the legacy DCG translator (`-Djprolog.dcg=legacy`) still has the `M:NT` bug.
- Q6: `bench_nrev(K) :- range(1,30,L), nrev(L,_), K1 is K-1, bench_nrev(K1).` (20 000 deterministic
  iterations, a cut in the base clause) dies with `resource_error(memory)` at -Xmx512m on 4.5.0 as
  on this tree — memory retained across a deterministic recursion; the failure-driven form is fine.
  `current_op(P, T, rdiv)` leaves a choice point after its only answer.
- Q5/Q4: `circuit/1`, `cumulative/1,2`, `disjoint2/1`, `automaton/3`, `tnot/1` are mentioned in the
  reference only as "not implemented" prose (the sweep checks headings), so Q5/Q4 must add
  headings when they implement them.
- The IDE's Run-panel capture is a plain PrintStream: column stops there still count from the
  format call (LIM-043); wrapping it in `ColumnPrintStream` would fix it.

## 12. Wave Q3 — record

Status: **DONE** (implementation agent, 2026-09-24; not committed — awaiting independent
verification). ISS-2025-0730..0739. Test class `core/engine/v4/EngineV46LoaderTest` (17 methods).
The class does not compile against the Q2 classes (it uses `Prolog.runMain()`), so "fails without
the fix" was checked by probing the defects on the 4.5.0 build (`git archive HEAD`, scratchpad):
`p(3,B)` with a `goal_expansion/2` rule raises `existence_error(procedure, dbl/2)`;
`absolute_file_name/3` is an arity message atom; `use_module/2` and `multifile/1` do not exist as
goals; `subterm_positions(P)` stays unbound and `comments(C)` is `[]`;
`current_predicate(m:foo/1)` raises `type_error`; `(a|b) =.. L` gives `[;,a,b]`; `a as b` is a
syntax error; no discontiguous warning; and the LIM-044/045 entries (reconsult wipes other files'
clauses, make/0 ignores includes, the `thread_get_message`/`concurrent_*` load deadlocks).

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| Q3.1 multifile, ownership, discontiguous | 0730 | done | `testISS0730_*` (3) | `Rule.sourceFile`; a reload removes the file's own clauses and, for a non-multifile predicate, the asserted ones; other files' clauses always stay; `multifile/1` defines (call fails), `M:PI` form, `predicate_property(_, multifile)`; SWI discontiguous warning on `user_error`; `multifile/1`, `discontiguous/1` also goals |
| Q3.2 goal_expansion/2 | 0731 | done | `testISS0731_*` (2) | bodies + directives, control constructs + common meta-predicates, fixpoint (variant stops), module hook first, depth cap 100 -> `resource_error(goal_expansion_depth)` load error; head+body expanded as one term (shared variables) |
| Q3.3 file_search_path, absolute_file_name | 0737 | done | `testISS0737_...` | `core.engine.FileSearch`, `builtin.filesystem.AbsoluteFileName` (/2,/3: extensions, file_type, access, relative_to, solutions, file_errors); `file_search_path/2` dynamic+multifile without clauses, defaults `swi`/`library`/`foreign` applied after the user's clauses; `library(X)`/`Alias(P)` loads search it |
| Q3.4 use_module/2 lists | 0735 | done | `testISS0735_...` | `Module.ImportSpec` (only/except/aliases) mirrored into `v4.Modules.fromImports`; `PI as Name`, `except/1`; `use_module/1,2` goals (`LoadFiles`, safe-mode denied) |
| Q3.5 `\|` and `as` | 0734 | done | `testISS0734_...` | **`\|` = 1105 xfy, `as` = 700 xfx** — SWI-Prolog 9 manual, section 4.25 "Operators", table of system operators (fetched 2026-09-24 from swi-prolog.org/pldoc/man?section=operators: rows `1105 xfy \|` and `700 xfx ... as ...`). The spec said 1100 for the bar; the manual's 1105 was used (SWI wins; with 1105, `a ; b \| c` is `'\|'((a;b), c)`). Reader reads the bar through the table; goal `'\|'/2` = `;/2` in `Machine.stepN`; writer prints `'\|'(a,b)`; `:- table p/1 as subsumptive` parses |
| Q3.6 make/0 includes, runMain | 0736 | done | `testISS0736_*` (2) | file record keeps includes + mtimes; `Prolog.runMain()` (-1/0/1/N, no halt) |
| Q3.7 subterm_positions, comments | 0738 | done | `testISS0738_*` (2) | `Lexer.Token.end`, comment spans, `TermReader.withPositions(base)` (off for consult: one null test per primary); the read collector keeps comments verbatim so offsets are stream offsets |
| Q3.8 per-file load lock | 0739 | done | `testISS0739_*` (2; + ISS-0639 test unchanged) | `LoadLock` per file (re-entrant, text loads share one key); per-thread load stack; `ModuleManager` per-thread current module while loading; waits-for walk over file locks, `thread_join` and `concurrent_*` awaits (`ThreadWaits`) -> `permission_error(load, source_sink, F)`; a file that loads itself is skipped |
| extra: `current_predicate(M:PI)` | 0732 | done | `testISS0732_...` | M bound/unbound, deterministic check; unqualified enumeration hides `:/2` |
| extra: qualified heads, multifile M:PI, legacy DCG | 0733 | done | `testISS0733_*` (2) | `user:H` -> user; own module -> normal; other module created on demand, clause kept as `M:H` (as `assertz(M:H)`), body in the source module; runtime assert/retract/clause/retractall strip `user:`; `M:H` bodies run in M; unknown goal in module M finds `M:H` clauses; `predicate_property(M:H, _)`. **Legacy DCG translator kept** (still used by `core.parser.Parser` and `-Djprolog.dcg=legacy`): fixed for `M:NT` (head and body) and `\|` |

Repinned: `EngineV4DatabaseTest.testISS0499_CurrentPredicateIsLazy` (3 -> 4 current predicates:
`file_search_path/2` is defined in a fresh engine, as in SWI). `safe-mode-allowlist.txt` gained
`native multifile/1`, `native discontiguous/1` (declarations only); `use_module/1,2` and
`absolute_file_name/2,3` are safe-mode denied (by package and by name).

Numbers: `mvn -o clean compile` OK; `mvn -o test -DargLine=-Xmx1g` **1489/1489** (1472 + 17),
58 s; `./test_all_examples.sh` 20/20, counts 2,0,0,1,1,0,0,0,0,0,2,1,0,0,2,0,0,0,0,0.

Consult speed (thread CPU, min of 15-20 fresh-engine loads per JVM, alternating JVMs, 40 000
mixed contiguous clauses, 4.5.0 `HEAD` build vs this tree, load average 5-9): consult
0.071-0.080 s vs 0.071-0.074 s; `.jpc` load 0.016-0.020 s vs 0.018-0.019 s; a later noisier batch
0.118-0.126 / 0.028-0.029 vs 0.111-0.115 / 0.029-0.030 — parity. A first version cost ~15 % on the
`.jpc` load (a string key per clause for the goal_expansion check, three monitor enters per
clause); fixed before these numbers (a `goal_expansion` flag on the module, a lock-free stamp,
the predicate key built once per run of clauses, which also removed the old per-clause
`userPredicates` string).

Deviations (all in `ref-deviations.md` §4b): bar priority 1105 (manual) not 1100 (spec); a
second file ADDS clauses to a non-multifile predicate (SWI redefines it); `M:H` clauses for another
module live in the flat store (the module's own file clauses win on a qualified call); the
`file_search_path/2` defaults are not clauses; `use_module/2` import lists of prelude libraries are
not enforced (autoload); a `thread_get_message` wait is invisible to the cycle check; the `op/3`
module context stays engine-wide.

Findings for later waves:
- Q4 (threads): `ThreadWaits` now knows joins and `concurrent_*` awaits; a message-queue wait
  (`thread_get_message/1,2`) could register the queue's expected senders if Q4 wants the load
  cycle check to see it. `Modules.sync()` rebuilds under a lock now, but `Modules.load(Mod)` (a
  library parsed on first use) and the `mods` map are still unsynchronised across threads.
- Q4 (tabling): `Spec as Options` still ignores the options (LIM-044); `as` is an operator now,
  so `:- table p/1 as subsumptive` parses.
- Q6 (performance): `Modules.sync()` rebuilds EVERY user module's clause arrays on any stamp
  change, and every consulted module clause bumps the stamp — a module-file load that calls into
  modules per clause (e.g. module-local `goal_expansion/2`) is quadratic. An incremental mirror
  (per-module stamps) would fix it.
- Q7 / CLAUDE.md: the loader description (per-file lock, per-thread load stack, ownership,
  goal expansion, FileSearch) and the native count (+2: `multifile/1`, `discontiguous/1`) need a
  refresh; `FileSystemPredicates.ABS_FILE_NAME` is dead code now (the registry points to
  `AbsoluteFileName`).

## 13. Wave Q4 — record

Status: **DONE** (implementation agent, 2026-09-24; not committed — awaiting independent
verification). ISS-2025-0745..0755 (0756–0759 unused). Test class
`core/engine/v4/EngineV46ThreadsTablingTest` (15 methods). "Fails without the fix": by
construction for the new predicates and options (4.5/Q3 raise existence errors); 0747 was checked
by restoring the old load order (8/8 threads -> existence_error(freeze/2)); 0752's no-wait test
deadlocks for 60 s under the old engine-wide claim; 0746 hangs without the message-wait edge;
0748/0754 pin behaviour the old code had by construction (engine-wide op context; bound moded
argument evaluated bound: `q4fl(k, 2)` succeeded).

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| extra: per-engine working directory | 0745 | done | `testISS0745_WorkingDirectoryIsPerEngine` | `EngineState.workingDirectory` + `EngineState.file/path`; open/3,4, consult/load/use_module, absolute_file_name, file_search_path targets, the filesystem/csv/persistence/logging built-ins, the safe-mode read check; `user.dir` never written; `exists_file/1`, `exists_directory/1` added (SWI names) |
| extra: message/mutex waits in the load-cycle check | 0746 | done | `testISS0746_MessageWaitSeenByLoadCycleCheck` | `ThreadWaits`: message waits (OR over every possible sender: live Prolog threads, threads in a query or a registered wait), mutex waits (edge to the holder), live threads; `LoadLock.blocked` walk |
| extra: library autoload thread-safe | 0747 | done | `testISS0747_ConcurrentLibraryAutoload` (8 threads x 20 engines; fails with the old load order: existence_error(freeze/2)) | `Modules.Mod` collections are copy-on-write behind volatile fields, `loaded` written last under the module's monitor; `mods` is a ConcurrentHashMap |
| extra: op/3 module context per thread | 0748 | done | `testISS0748_OpModuleContextIsPerThread` | `Ops.moduleContext` is a ThreadLocal (cheap: it follows ModuleManager's per-thread current module) |
| Q4.1 thread_signal/2 | 0749 | done | `testISS0749_*` (3) | `core.engine.ThreadSignals` (per-thread box, JVM-wide pending counter); drive loop pushes queued signals as `ignore(\+ \+ G)` goals; blocked thread built-ins run them in place; `main` box shared by non-worker threads; worker machines bind their own box |
| Q4.1 thread_statistics/3, message_queue_property/2, thread_send_message/3, mutex_property/2, thread pools | 0750 | done | `testISS0750_*` (2) | `max_size/1` queues; pools: `thread_pool_create/3`, `thread_create_in_pool/4`, `thread_pool_destroy/1`, `thread_pool_property/2`, `current_thread_pool/1` |
| Q4.2 mutex held at exit | 0751 | done | `testISS0751_MutexHeldAtExitWarns` | released + `print_message(warning, format(...))` |
| Q4.3 tabling across threads | 0752 | done | `testISS0752_ConcurrentTablingStress` (8 threads x 20 engines, private and shared), `testISS0752_NoCrossThreadTablingWait` (old claim: 60 s resource_error) | table SPACES: main space for non-worker threads (old claim kept), a private space per worker machine (SWI default: private); `as shared` publishes COMPLETE tables engine-wide; incomplete tables owned; a caller never waits for another thread's evaluation — it evaluates privately (deviation: SWI waits); stale tables dropped via per-predicate stamps + abolish epoch |
| Q4.4 mode-directed tabling | 0754 | done | `testISS0754_ModeDirectedTabling`; `EngineV45LoadReadWriteTest.testISS0572_TableDirectiveForms` repinned | SWI's boot/tabling.pl semantics (read from the source, swipl-devel master, 2026-09-24): every moded argument aggregated on its own (first, last, min/max by standard order — `min(S0,S1,S) :- (S0 @< S1 -> S = S0 ; S = S1)`, sum, `lattice(PI)` = `call(PI, Old, New, Agg)`, `po(PI)` = `(call(PI, Old, New) -> Agg = Old ; Agg = New)`), answer replaced only when the aggregate is not a variant; moded arguments evaluated FREE (`TableStore.ModeSpec`, `Machine.freeModedArguments`); unknown mode `domain_error(tabled_mode, M)` (SWI's name; was `table_mode`), bad arity `domain_error(lattice_arity\|po_arity, N)`. **Deviation from the spec**: `po/1` keeps ONE aggregated answer (SWI source), so there is no Pareto front — the test pins SWI's single-answer semantics |
| Q4.5 minimal WFS | 0755 | done (minimal, see extent) | `testISS0755_TnotWellFounded`; `EngineV4TablingTest.testISS0464_*` repinned (tnot/1 exists now) | `tnot/1` (SWI's algorithm on linear tabling: unconditional answer -> fail; complete & empty -> true; incomplete or only conditional -> succeed with `tnot(G)` DELAYED + an incomplete read so the SCC iterates), `undefined/0`, `call_delays/2`; conditional answers keep their alternative delay lists (`Table.conds`); a consumer of a conditional answer adds a positive delay; at SCC completion `Tabling.simplify` resolves delays to a fixpoint (true literal dropped, false literal kills the alternative, an answer with no alternative left is removed); the CLI prints `undefined`, embedders read `Prolog.currentAnswerDelays()` |
| extra: `Spec as Options` | 0753 | done | `testISS0753_TableOptions` | `variant`, `shared`, `private` implemented; `subsumptive`, `incremental`, `opaque`, `monotonic`, `lazy`, `dynamic`, `max_answers/1`, `subgoal_abstract/1`, `answer_abstract/1` accepted with a `Warning:` on user_error (variant tabling); other options `domain_error(table_option, O)` |

Repinned: `EngineV4TablingTest.testISS0464_TnotRaisesAnExistenceError` (tnot/1 exists),
`EngineV45LoadReadWriteTest.testISS0572_TableDirectiveForms` (lattice/po supported;
`domain_error(tabled_mode, M)`); `safe-mode-allowlist.txt` gained `native call_delays/2`,
`native tnot/1`, `native undefined/0` (pure). The new thread predicates live in
`builtin.threading` (safe-mode denied); `exists_file/1`, `exists_directory/1` were already on the
deny-by-name list.

Numbers: `mvn -o clean compile` OK; `mvn -o test -DargLine=-Xmx1g` **1504/1504** (1489 + 15), 42 s;
thread + tabling classes (`EngineV46ThreadsTablingTest`, `EngineV4ThreadsTest`,
`EngineV45HardeningTest`, `EngineV41RetirementTest`, `EngineV4RetirementTest`,
`EngineV4StreamsTest`, `EngineV46LoaderTest`, `EngineV4TablingTest`, `PrologCliToplevelTest`)
3 x 154/154 (after the signal-ordering fix below; one earlier run showed the race); the signal
ordering loop (600 signal pairs, busy and blocked targets) 0 misses; `./test_all_examples.sh` 20/20,
counts 2,0,0,1,1,0,0,0,0,0,2,1,0,0,2,0,0,0,0,0.

Benchmark (CLI, best of 6 per JVM, 3 alternating JVM pairs, 4.5.0 `HEAD` build vs this tree,
load average 1.5–2): tabled `fib(1500)` x 30 with `abolish_all_tables` 0.186–0.225 s vs
0.191–0.228 s; tabled left-recursive `path` over a 3 000-edge chain x 20 0.057–0.068 vs
0.058–0.077; `nrev(400)` x 60 0.569–0.698 vs 0.557–0.659 — parity within noise. Per-step cost of
signals: one volatile read of `ThreadSignals.PENDING`.

Signal ordering (found while verifying): a signal arriving while an earlier one was being pushed or
run from a blocking built-in could overtake it. Fixed: all queued signals are taken at once; the
drive loop takes no new signal while signal goals run (`sigEnd`), nor while a blocking built-in
runs them (`ThreadSignals.enterHandler`).

Deviations (all in `ref-deviations.md` §4c): tabling across threads — private spaces per worker,
`as shared` publishes complete tables, a second thread evaluates instead of WAITING (SWI waits),
so no tabling deadlock is possible; `po/1` keeps one aggregated answer (SWI's source), not the
Pareto front the spec asked for; `sum` in a recursive SCC may not terminate; WFS is minimal
(no answer completion, no residual program, no delays through moded tables; `\+` still raises on an
incomplete ancestor table); `tnot/1` of a non-tabled goal is
`permission_error(tnot, non_tabled_procedure, PI)`; `call_delays/2` consumes the delays; a signal to
`main` is run by whichever non-worker thread polls first.

Doubts:
- `main` is every non-worker thread, so several embedder threads share one signal box and one main
  table space (the evaluation claim still serialises them); a per-embedder-thread identity would be
  cleaner but changes the P6 `main` contract.
- The message-wait deadlock check sees a thread as a possible sender only while it is a Prolog
  thread, in a query or in a registered wait; an idle embedder thread is invisible (LIM-045).
- Blocking thread built-ins now wait in 100 ms slices (signal checks); CPU cost is negligible but a
  thread blocked in non-thread Java code (socket read) sees a signal only when it returns.

Findings for later waves:
- Q5 (CLP(FD)): nothing touched; the CLP(FD) bridge still reaches the trail through `Undo`.
- Q6 (performance): `engine.tabling()` is now a ThreadLocal lookup (looked up once per tabled
  call); `Tabling.invalidate` bumps a stamp map on every assert to a TABLED predicate only;
  `answer()` builds a delay conjunction only for conditional answers. The `\+` path inside tabled
  evaluation still calls `engine.tabling()` per negation.
- Q7 / CLAUDE.md: new invariants to record — (1) a signal runs on the target's own goal stack,
  never asynchronously; no new signal while signal goals run; blocking thread built-ins run signals
  in place; (2) table spaces: a worker machine's tables are private (`Tabling.enterWorker`), only a
  COMPLETE table of a `shared` predicate crosses threads, an incomplete one never does; (3) the
  WFS delay list is trailed machine state (`Machine.setDelays`), reset at each PRODUCE clause, and
  simplification runs in `completeScc`; (4) `EngineState.file/path` for every relative file name;
  (5) `Modules.Mod` is copy-on-write. Native count +3 (`tnot/1`, `undefined/0`, `call_delays/2`);
  registry +11 (`thread_signal/2`, `thread_statistics/3`, `message_queue_property/2`,
  `mutex_property/2`, the 5 pool predicates, `exists_file/1`, `exists_directory/1`) and
  `thread_send_message/3`. `FamousPrologProgramsTest` could gain the win/1 WFS game.


## 14. Wave Q5 — record

Status: **DONE** (implementation agent, 2026-09-24; not committed — awaiting independent
verification). ISS-2025-0760..0770 (0771–0774 unused). Test class
`builtin/clpfd/v2/ClpfdV46Test` (15 methods). "Fails without the fix": by construction for the
new predicates (4.5: existence errors) and the residual forms (4.5 printed only `in/2` goals);
0768 raised `representation_error(max_integer)`; 0769 checked with a harness over the old
recursive labeler (StackOverflowError at 10 000 chained variables, fine at 5 000); 0770 checked
by restoring the two old `Machine` conditions (the 0770 and 0765 tests error with
`existence_error`).

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| Q5.1 residual constraints | 0760 | done | `testISS0760_*` (4) | new `builtin.clpfd.v2.Residuals`; `Constraint.alive/render/reifiedForm` per class; `Coroutining.residualGoals` and `copy_term/3` share one rendering state per call (constraint printed once; FD cells and auxiliaries reached through a constraint follow the answer's variables, like SWI's `term_attvars/2`); `in/2` goal omitted for `inf..sup` with a live constraint (SWI rule); `clpfd:attribute_goals//1` (prelude + native `'$clpfd_attribute_goals'/2`); top-level `L #<==> R` reifies both sides into one boolean |
| extra: `X #= Y` unifies | 0761 | done | `testISS0761_*` | SWI's `constrain_to_integer(X), X = Y` |
| Q5.2 `circuit/1` | 0762 | done | `testISS0762_*` (2) | `Globals.Circuit` (self loops, chain closing, strong connectivity) + a hidden `AllDistinct` (`Constraint.FORM_HIDDEN`); knight's tour 6x6 first solution ~40 ms (8x8 ~40 ms), 5x5 fails |
| Q5.2 `cumulative/1,2` | 0763 | done | `testISS0763_CumulativeJobShop` | `Globals.Cumulative` time-table; `S+D #= E` per task; job-shop optimum 9 |
| Q5.2 `chain/2`, `lex_chain/1`, `disjoint2/1` | 0764 | done | `testISS0764_*` | prelude decompositions |
| Q5.2 `automaton/3,8` | 0765 | done | `testISS0765_Automaton` | prelude: `tuples_in/2` + reified counter updates; SWI doc example |
| Q5.2 `zcompare/3`, `fd_degree/2` | 0766 | done | `testISS0766_*` | zcompare via reified booleans + freeze; `fd_degree` = live constraints |
| Q5.2 `global_cardinality/3` | 0767 | done | `testISS0767_*` | `consistency(value)`, `cost(Cost, Matrix)` |
| Q5.3 BigInteger coefficients | 0768 | done | `testISS0768_*` | `Linear`/`LinearNE` `bigC` (null = the untouched long fast path); exact bounds for out-of-range fixed values on the slow path |
| Q5.4 iterative branch and bound | 0769 | done | `testISS0769_*` | `Labeler.optimum`: one DFS with an explicit frame stack, bound re-imposed per branch (no restart per improvement) |
| extra: module index miss | 0770 | done | `testISS0770_ModuleIndexMissFails` | engine bug: `Modules.localClauses` + two `Machine` call sites treated an empty first-argument selection as "undefined" → `existence_error` |

Residual printing (CLI, this tree): `X #> Y` → `Y#=<X+ -1`; `X in 1..5, X #\= 3` →
`X in 1..2\/4..5`; `X in 1..3, Y in 1..3, all_different([X,Y])` →
`X in 1..3, all_different([X,Y]), Y in 1..3`; `X #= Y+Z` → `Y+Z#=X`; `B #<==> X #> 3` →
`B in 0..1, X#>=4#<==>B`; `X #> Y, X = 5` → `X = 5, Y in inf..4`; `X #= Y*Z+W` →
`W+_A#=X, Y*Z#=_A`; `X #> Y, copy_term(X-Y, C, Gs)` → `Gs = [_B#=<_A+ -1]`; freeze/dif residuals
unchanged and printed alongside.

Changed on purpose: `ref-deviations.md` §2 row `X #= Y` (now unifies); the safe-mode allowlist
gained the 11 prelude exports and 4 natives of the wave (all pure).

Numbers: `mvn -o clean compile` OK; `mvn -o test -DargLine=-Xmx1g` **1519/1519** (1504 + 15),
51 s; `./test_all_examples.sh` 20/20, counts 2,0,0,1,1,0,0,0,0,0,2,1,0,0,2,0,0,0,0,0.

Benchmark (CLI, `-Xmx1g`, a pre-Q5 build = this tree with the six CLP(FD) files at HEAD, vs this
tree; 3 alternating JVM pairs, best of 5 rounds per JVM, load 1.0–1.4; each figure is a batch):

| Batch | before | after |
|---|---|---|
| all 92 8-queens x10 | 13–19 ms | 13–19 ms |
| SEND+MORE x50 | 2 ms | 2–3 ms |
| Inkala sudoku x20 | 42–60 ms | 55–56 ms |
| 20-queens `ff` first x20 | 12–23 ms | 14–19 ms |

Parity within noise (individual rounds vary 42–181 ms for the sudoku batch on both builds).
The long fast path of `Linear`/`LinearNE` is unchanged apart from one null test.

Deviations (all in `ref-deviations.md` §4d): residual forms were taken from SWI's `clpfd.pl` as
recalled (no SWI installation available); JProlog's canonical choices: `X #= Y-2` → `X+2#=Y`,
`X+Y #>= 3` → `3#=<X+Y` (SWI: an auxiliary sum), complex expressions show JProlog's own
auxiliaries, decomposed globals (`lex_chain`, `disjoint2`, `automaton`, `zcompare`) print their
parts, `all_different` with one free variable left is not printed.

Doubts:
- The exact SWI text for `X #= 2*Y` (`2*Y#=X` chosen, SWI's `ptimes` form) and for negative
  constants in two-variable equalities is from memory; the verifier may want to check against a
  real SWI 9 if one is available.
- `zcompare/3`, `lex_chain/1`, `disjoint2/1`, `automaton/8` counters are decompositions — correct,
  weaker than SWI's dedicated propagators (LIM-041).

Findings for later waves:
- Q6 (performance): `ClpfdV2Bridge.determinedCells()` scans every FD cell after each post and
  each labeling step — O(n²) for large models (a 20 000-variable `#=<` chain: ~4 s to post,
  ~6 s to label leftmost); an event log of newly fixed variables in `ClpStore.install` would make
  it O(changes). `abs(X-Y) #\= C` goes through an auxiliary variable (20-queens written that way
  ~3.9 s vs a few ms): SWI's `absdiff_neq` propagator is a cheap win. The lazy `step()` also
  rescans the variable list from the head (leftmost) on every step.
- Q7: `FamousPrologProgramsTest` can take the knight's tour from `ClpfdV46Test` (`KNIGHT`);
  CLAUDE.md should record `Residuals` (every new `Constraint` subclass must implement `render`
  and, when it can become entailed without all variables fixed, `alive`), the `Constraint.form`
  hints (`FORM_NOT`, `FORM_HIDDEN`), and that no comment may go inside a prelude export list
  (`Prelude.header` reads it textually — a comment there silently drops the next export).
  `append/2` is missing (existence error) — not in Q2's list; `examples/test_41_clpfd.pl`'s
  `fd_dom_basic` expects the pre-4.5 list form (prints FAIL; not a JUnit test).

## 15. Wave Q6 — record

Status: **DONE** (implementation agent, 2026-09-24; not committed — awaiting independent
verification). ISS-2025-0775..0787 (0788–0789 unused). Test class
`core/engine/v4/EngineV46PerformanceTest` (15 methods); `EngineV45PerformanceTest.testISS0549_*`
repinned (maplist/foldl no longer recurse through library clauses).
Before-build: `target/classes` of this tree (Q1–Q5 applied) copied to the scratchpad
`q6/before-classes` before any Q6 change.

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| Q6.1 runtime-goal call cache | 0777 | done | `testISS0777_*` (3) | per-machine direct-mapped (name, arity) cache of `CallSite`s, validated by context module + `dispatchStamp()`; call/N appends into one array; `checkBody` builds its context string only on error. p_call(1e6) 0.24 → 0.14 s |
| Q6.2 native maplist family | 0780 | done | `testISS0780_*` (2); `testISS0549_LibraryRecursionUsesCallSites` repinned | new `NativeApply`: the first clause of each apply iteration predicate is tagged at library load; `Machine.activate` runs the two-clause window natively (same look-ahead, same CLAUSES frame + ports when traced or when both alternatives remain, closure pushed as call/N would, recursion as a `Machine.Step`). 15 trace oracles (nondeterministic closures, open lists, errors, lambdas, include/partition) byte-identical to the before-build |
| Q6.3 io built-ins native | 0785 | done | `testISS0785_StreamBuiltinsAreNative` | new `NativeStreams` (22 indicators: open/3,4, close/1,2, stream_property/2, current_stream/3, set_stream/2, seek/4, set_stream_position/2, stream_position/2, stream_position_data/3, character_count/2, line_count/2, line_position/2, get/peek/put_byte/1,2, portray_clause/1,2) — faithful ports of the registry classes; the two enumerating ones are generators over pre-filtered candidates announcing their last answer. read/1,2 and read_term/2,3 were already native (P3). Safe mode: `open` still denied by name (with read whitelists it falls back to the registry `SafeOpen`); the 20 others added to `safe-mode-allowlist.txt` |
| Q6.4 predsort | 0778, 0779 | done (target missed, see deviations) | `testISS0779_*` (2) | comparisons on the goal stack (`Machine.Step`, fail frame, own ports); plus general: lazy native context string (0778), single-candidate activation without a choice point, native call sites on body skeletons (0779) |
| Q6.5 benchmarks | — | done | — | tables below; no regression beyond noise |
| extra 1 CLP(FD) newly-fixed queue | 0781 | done | `testISS0781_ISS0782_*` | `ClpStore` change tracking (trail low-water mark, constraint mark, unbounded-variable count); `determinedCells` looks only at changed variables (+ unbounded neighbours when any variable is unbounded). 20k chain post 4.5 s → 0.065 s |
| extra 2 CLP(FD) abs(X-Y) #\= C | 0783 | done | `testISS0783_*` | `Constraint.AbsDiffNE` (SWI absdiff_neq), fix-only wake, entailed once the forbidden values are gone (no residual then); `X - Y #\= C` already was a fix-only `LinearNE`. 20-queens with abs 3.9 s → 0.013 s |
| extra 3 CLP(FD) leftmost cursor | 0782 | done | `testISS0781_ISS0782_*` | the `'$clpfd_label'` continuation resumes at the first unbound variable's list cell (all strategies). 20k chain post+label 10.6 s → 0.08 s |
| extra 4 incremental module mirror | 0784 | done | `testISS0784_*` | `Module` gets a structural version + append-only rule accessors; `Modules.syncLocked` skips unchanged modules and compiles only appended rules (copy-on-write per predicate, concurrent map); `Modules.Pred` index lazy and merged by position (was O(buckets x clauses) per build). 2 x 2000-clause module with goal_expansion: 13 s → 0.06–0.9 s; 4000: 103 s → 0.1–0.45 s |
| extra 5 tabling \+ lookup cache | 0776 | done | `testISS0776_*` | `Machine.tablingHere()` |
| extra 6 current_op/3 last alternative | 0775 | done | `testISS0775_*` | candidates pre-filtered by the bound args; probe of ~120 generator calls found one more: `catch/3` left its frame (popped on a clean exit now) |
| extra 7 budget for bridged libraries | 0786 | done | `testISS0786_*` | `LegacyBuiltinAdapter` charges one step per 64 characters of text input and one per solution, and installs the query's guard for the call (`ResourceGuard.enterBridge`); `ResourceGuard.guarded(CharSequence)` meters java.util.regex (one step per 256 characters read, interrupt polled) — used by all re_* predicates. Control exceptions unchanged (not catchable) |
| extra 8 20k-step range+nrev OOM report | 0787 | **reproduced and fixed** | `testISS0787_*` (fails without the fix: trail 744 000 entries at the bottom of a 1500-level run) | not in plain runs (`solve`/`solveStream`, -Xmx512m/128m, 5 recursion shapes: 2 MB) but under **trace/0** and an attached **DebugController with a listener** (the IDE): `bn(20000)` → `resource_error(memory)` at -Xmx512m (peak ~60 KB/level). Cause: a traced deterministic frame is kept until its Exit port, so nearly every binding was trailed and the entries (and the terms they keep alive) stayed until a backtrack. Fix: `Bindings.tidy` on every deterministic frame pop (trust-me pop and traced Exit) drops entries for variables newer than the new top choice point (never inside a forceTrail extent; undo actions kept). Also the debugger copied its whole call stack per port (O(depth) per port): now an immutable linked stack, snapshot O(1). Debug `bn(20000)`: OOM → 15 s, peak 79 MB; trace `bn(4000)`: peak 396 → 71 MB |

Numbers: `mvn -o clean compile` OK; `mvn -o test -DargLine=-Xmx1g` **1534/1534** (1519 + 15), 52 s;
`EngineV4TraceTest` untouched and green (26/26); `./test_all_examples.sh` 20/20, counts
2,0,0,1,1,0,0,0,0,0,2,1,0,0,2,0,0,0,0,0. Docs: `track-issues.md` (Q6 section), LIM-037/042/045,
`ref-deviations.md` (inference budget row), `report-engine-v4-progress.md` invariants 16–18,
`tools/manual/appendix.md` + `tools/build-manual.sh` (no predicate added or removed),
`src/test/resources/safe-mode-allowlist.txt` (+20 stream natives).

"Fails without the fix": 0775 (the probe showed `nondet` for both goals on the before-build),
0783 (before: `fd_dom(Y, D)` gives `1..5`), 0786 (before: the regex runs ~2 s and fails, no
exception), 0787 (checked by disabling `tidy`: 744 000 trail entries); the others assert on hooks
the old code does not have (`rtSiteHits`, `tablingLookups`, `runOnceCalls`, `singleClauseActivations`,
`applyLevels`, `examinedCells`, `labelCellsVisited`, `mirrorCompiles`, `isNative`) — the P2
precedent — plus behaviour pins that pass on both builds.

### Measured (JDK 25, `-Xmx1g`, load 1.0–2.0; before = `q6/before-classes`, i.e. Q1–Q5 applied)

§9 harness + P2 extras + Q6 targets: 3 interleaved before/after JVM pairs, median of the warm runs
of each JVM; range over the 3 JVMs (median of medians):

| Benchmark | before | after | change |
|---|---|---|---|
| nrev30 ×20000 | 1.24–1.29 (1.26) s | 1.00–1.02 (1.02) s | −19 % |
| fib(23) | 0.046 s | 0.041 s | −11 % |
| loop(3M) | 0.48–0.50 (0.49) s | 0.42–0.43 (0.42) s | −13 % |
| tak(18,12,6) | 0.021 s | 0.020 s | noise |
| queens(8) | 0.136 s | 0.132 s | noise |
| assert 200k + lookup | 0.227 s | 0.196 s | −14 % |
| findall 500k + msort | 0.16–0.37 s | 0.14–0.34 s | noise (GC) |
| atom ops 200k | 0.079 s | 0.051 s | −35 % |
| deriv ×100k | 0.37–0.44 (0.38) s | 0.39–0.43 (0.39) s | +2 % (noise; one set +12 %, rechecked) |
| retractall 100k / retract-first 1e5 / asserta 1e5 | 0.101 / 0.130 / 0.092 s | 0.089 / 0.112 / 0.097 s | parity |
| 200k KB + 20k bound retracts / interleave 3e4 / append(_,[L],L) 3e4 | 0.181 / 0.021 / 0.025 s | 0.190 / 0.023 / 0.027 s | parity |
| **call(foo, I) ×1e6** (Q6.1) | 0.22 s | 0.126 s | **−43 %** (target −30 %) |
| call(succ, I, _) / G = foo(x), call(G) ×1e6 | 0.163 / 0.166 s | 0.129 / 0.140 s | −21 % / −16 % |
| \+ bar(I) ×1e6 / findall ×3e5 | 0.339 / 0.090 s | 0.290 / 0.076 s | −14 % / −16 % |
| **maplist 3e5** vs hand recursion (Q6.2) | 0.291 vs 0.171 s (1.70×) | 0.160 vs 0.124 s (1.29×; other runs 1.06×) | −45 % |
| **foldl 3e5** vs hand recursion | 0.243 vs 0.135 s (1.80×) | 0.107 vs 0.105 s (1.02×) | −56 % |
| **include 3e5** vs hand recursion | 0.276 vs 0.194 s (1.42×) | 0.204 vs 0.186 s (1.10×) | −26 % |
| exclude / partition 3e5 | 0.290 / 0.299 s | 0.193 / 0.187 s | −33 % / −37 % |
| **predsort 1e5 random / reversed** (Q6.4, list prebuilt) | 0.484 / 0.262 s | 0.353 / 0.183 s | −27 % / −30 % (target −50 %: missed) |
| stream_property(_, alias(user_output)) ×1e5 (Q6.3) | 0.254 s | 0.119 s | −53 % |
| line_count(user_output, _) ×1e5 | 0.030 s | 0.007 s | −77 % |

(numlist/1 of the list is inside the apply figures; a separate 1e5 run gave maplist 0.074 → 0.040 s
against 0.045 s for the hand recursion.)

CLP(FD) (2 interleaved pairs, 3 runs per JVM) and the Q5 batches:

| Target | before | after |
|---|---|---|
| 20 000-variable `#=<` chain, post | 4.9–5.4 s | 0.09–0.10 s |
| same, post + leftmost label (first solution) | 11.3–13.6 s | 0.10 s |
| 20-queens with `abs(Q-Q1) #\= D`, first `ff` solution | 3.7–4.9 s | 0.006–0.010 s |
| 20-queens standard (`#\=` three times), first `ff` | 0.005 s | 0.006 s (noise) |
| Q5 batches: q8all ×10 / q20ff ×20 / sudoku ×20 / SEND+MORE ×50, best round | 14–16 / 16–17 / 47–63 / 2 ms | 14–16 / 13–15 / 47–54 / 2 ms (parity; a first version of 0781 was 40 % slower on the 8/20-queens batches — the plain scan is kept for ≤ 64 cells or a step that changed ≥ 1/4 of them) |

Other targets:

| Target | before | after |
|---|---|---|
| module load, 2 × 2000 clauses + module-local `goal_expansion/2` (extra 4) | 13.0–13.4 s | 0.06 s warm (0.94 s first JVM run) |
| same, 2 × 4000 clauses | 103 s | 0.11 s warm (0.45 s first) |
| same, 2 × 8000 clauses | (not run: ~14 min extrapolated) | 0.23 s warm |
| tabled `\+` loop, 2e5 negations inside one evaluation (extra 5) | 0.228–0.236 s | 0.202–0.210 s |
| `bn(20000)` (range + nrev) under an attached DebugController with a listener, -Xmx512m (extra 8) | `resource_error(memory)` | 15.3 s, peak heap 79 MB |
| `bn(4000)` under trace/0, -Xmx512m | peak heap 396 MB | 71 MB |
| same program, plain `solve`/`solveStream`, 5 recursion shapes, -Xmx512m | 2 MB (no problem) | 2 MB |

### Deviations / doubts
1. **Q6.4 target missed** (≥ 2× on predsort 1e5): −27..−30 %. The comparisons run on the goal
   stack as specified; what is left per comparison is the comparator itself (a failure-driven
   loop calling `cmp/3` costs ~140 ns, a predsort comparison ~185 ns). Recorded in LIM-042.
2. **Q6.2 "within 1.1×"**: foldl 1.02×, include 1.10×; maplist 1.06–1.29× over the runs (the
   closure goes through the run-time cache, the hand recursion through a skeleton call site; the
   remaining gap is allocation noise on this box).
3. Beyond the spec (general engine wins found while profiling, all tagged): the lazy native
   context string (0778), the single-candidate activation without a choice point and native call
   sites on skeletons (0779), trail tidying (0787, the fix of extra 8), the persistent debugger
   call stack (0787), `catch/3` determinism (0775).
4. `open/3,4` under safe mode with read whitelists still runs the registry `SafeOpen` (the native
   is denied by name, as before) — no behaviour change.
5. Extra 8 did not reproduce as reported (plain runs are fine at -Xmx128m); it reproduced under
   trace/0 and an attached debugger, which is presumably how the Q2 agent ran it.

### Findings for Q7
- Missing SWI list/ordset predicates seen by the determinism probe (existence errors):
  `nextto/3`, `max_member/2`, `list_to_set/2`, `list_to_ord_set/2`, `ord_union/3`,
  `ord_subtract/3`, `ord_memberchk/2`, `recorda/2`, `recorded/2`, `flag/3`, `current_key/1`,
  `get_flag/2` (the probe tested them in passing; worth a documentation sweep entry).
- CLAUDE.md: invariants 16–18 (trail tidying and mark holders, run-time call sites, `Machine.Step`
  iterations), `NativeApply`/`NativeStreams` in the package list, the native count, LIM-037's
  adapter count (16 io names fewer).
- `DebugController` still copies the goal snapshot per port when a listener renders; a 20 000-level
  debugged recursion takes ~15 s (≈1.4 µs per port) — fine, but the IDE trace panel could batch.

## 16. Wave Q7 — record

Status: **DONE** (implementation agent, 2026-09-24; not committed — awaiting independent
verification). ISS-2025-0790..0799 (all used). Test class `core/engine/v4/EngineV46ResidueTest`
(9 methods) + 4 methods in `test/integration/FamousPrologProgramsTest`. "Fails without the fix":
by construction for the new predicates (existence errors before) and 0798's programs (they use
them); 0792 (the old scanner loses `b/2`, `c/3` and `d/0` of the synthetic header), 0794 (`[b,a,a2]`), 0796/0797 (the goals
failed or succeeded) and 0793 (the example printed `FAIL: fd_dom_basic`) were probed on the tree
before the change.

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| residue: lists + ordsets | 0790 | done | `EngineV46ResidueTest.testISS0790_*` (2) | prelude `lists.pl` += append/2, nextto/3, max_member/2,3, min_member/2,3, list_to_set/2, proper_length/2; new prelude `ordsets.pl` (SWI definitions, 19 exports incl. ord_insert/3) |
| residue: recorded db + flag/3 | 0791 | done | `testISS0791_*` (2) | new `NativeRecords` (recorda/z 2,3, recorded/2,3, erase/1, current_key/1, flag/3); per-engine store `Engine.records()` |
| residue: prelude header comments | 0792 | done | `testISS0792_*` | `Prelude.blankComments` before the scan |
| residue: test_41 fd_dom | 0793 | done | `testISS0793_*` | example expects `1..3` / `1..2\/4..5`; 32/32 |
| residue: multifile reload order | 0794 | done | `testISS0794_*` | SWI manual 4.3.2: keep place; `KnowledgeBase.rulesAfterFirstOwnedBy/moveToEnd` |
| residue: dead absolute_file_name code | 0795 | done | — (deletion) | `FileSystemPredicates.ABS_FILE_NAME` + `doAbsFileName` deleted |
| residue: FFI raises | 0796 | done | `testISS0796_*`; harness asserts 0 ffi `fail` | |
| residue: library inputs that failed | 0797 | done (clear cases) | `testISS0797_*` | graph (unbound/non-list graph), json_get, spy(Var), string_to_atom/atom_to_number/number_to_atom/to_codes both unbound, enhanced_phrase/2,3 = phrase (was a non-executing stub) |
| FamousPrologProgramsTest +4 | 0798 | done | 4 methods | solution_sequences pipeline, file_search_path load, tabled shortest path `min`, 6x6 knight's tour |
| Sweeps rerun | — | done | `ExtendedLibraryErrorsTest`, `DocumentedPredicatesTest` | 3 942 goals: 0 message / 0 arity / 0 timeout; fail 130 -> 95, ok 180 -> 154 (review list in the report; LIM-047) |
| Release docs | 0799 | done | `EngineV45ReleaseTest` repinned | pom, flags 40600, CHANGELOG, release notes, README, track-*, reference, deviations §4e, manual |
| CLAUDE.md refresh | — | done | — | numbers (1547 tests, 264/220 natives, 443 registry names, 237 adapter-reachable), the 4.6 rules (pushFiltered + look-ahead, per-file load lock, clause ownership, qualified heads, file_search_path, per-engine cwd, signals, table spaces, WFS delays, copy-on-write modules, Constraint render/alive/FORM hints, invariants 16–18, NativeApply/NativeStreams/NativeSequences/NativeRecords, exact registry arities, `main` = every non-worker thread), the new test classes, ISS/LIM counters |

Deviation from the spec / decisions:
- **Multifile reload order** decided against SWI's documented reload semantics (manual §4.3.2,
  "Reloading files, active code and threads", fetched 2026-09-24 from
  swi-prolog.org/pldoc/man?section=loadrunningcode: an unchanged clause is kept in place, a new one
  is inserted "before the current clause", the rest is "marked for future deletion") → FIXED, not a
  deviation; the one approximation (interleaved files come back as one block) is in
  `ref-deviations.md` §4e.
- `ord_insert/3` is not SWI (DEC-10/YAP name) — provided as the task asked, listed in §4e.
- The ~130 fail / ~180 ok probe goals: the clear argument faults raise now (graph, json_get,
  spy(Var), the four text conversions with both sides unbound, enhanced_phrase); the rest was
  reviewed and is LIM-047 (text predicates on a compound, `format(f(x))`, `reverse/select` all
  unbound, `spy(Name)`/`spy(Head)`, and `phrase_with_options/4`, which never runs its grammar).
- `erase/1` of an erased record fails (SWI's behaviour not verified; §4e).

Numbers: `mvn -o clean compile` OK; `mvn -o test -DargLine=-Xmx1g` **1547/1547** (1534 + 13:
9 `EngineV46ResidueTest` + 4 `FamousPrologProgramsTest`; three existing tests repinned), 51 s;
thread/tabling/trace/performance classes (`EngineV46ThreadsTablingTest`, `EngineV4ThreadsTest`,
`EngineV45HardeningTest`, `EngineV41RetirementTest`, `EngineV4RetirementTest`,
`EngineV4StreamsTest`, `EngineV46LoaderTest`, `EngineV4TablingTest`, `PrologCliToplevelTest`,
`EngineV4TraceTest`, `EngineV45PerformanceTest`, `EngineV46PerformanceTest`,
`PerformanceRegressionTest`) 3 × 222/222; `./test_all_examples.sh` 20/20, counts
2,0,0,1,1,0,0,0,0,0,2,1,0,0,2,0,0,0,0,0; `mvn -o package -DskipTests` → `target/jprolog.jar`,
`java -jar target/jprolog.jar -g "current_prolog_flag(version,V), write(V), nl" -t halt` prints
`40600`. Sweeps: Q1 harness 3 942 goals, 0 message / 0 arity / 0 timeout (fail 95, ok 154);
Q2 `DocumentedPredicatesTest` 0 misses (the new `###` headings included).

Deleted: `FileSystemPredicates.Mode.ABS_FILE_NAME` + `doAbsFileName` (CHANGELOG 4.6.0).
LIM: LIM-047 new; LIM-043/044/045 notes refreshed.
