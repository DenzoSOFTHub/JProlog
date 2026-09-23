# JProlog Current Limitations

This document describes current limitations in JProlog implementation.
When an issue is resolved, the corresponding limitation should be removed from this file.

**Last updated**: 2026-09-23 (release 4.5.0: waves P1..P7 of the production-readiness program)

---

## LIM-046: tabled negation is not the well-founded semantics (4.5 wave P7)

**Found in 4.5 wave P7 (ISS-2025-0661, decision §8 of the production-readiness program).**
`\+ G` inside a tabled evaluation, where `G` reads a table that is still being evaluated by an
ancestor of the negation (a non-stratified program such as `:- table p/1. p(X) :- \+ p(X).` or the
`win/1` game over a cycle), raises `error(permission_error(negate, incomplete_table, G), (\+)/1)`.
4.4.0 answered such queries inconsistently (`p(a)` succeeded). SWI-Prolog answers them with the
well-founded semantics (`tnot/1`, delay lists, residual program, "undefined" answers); JProlog has
none of that: no `tnot/1`, no `undefined/0`, no answer subsumption. Stratified negation under
tabling (the negated goal's tables complete inside the negation) is unaffected.

**Workaround**: stratify the program (compute the negated relation in its own tabled predicate
first) or drop `:- table` for the predicates involved in the negative cycle.

---

## LIM-045: production-hardening residue after 4.5 wave P6

**Found in 4.5 wave P6 (ISS-2025-0620..0639).** What the wave deliberately left out:
- **Load lock** (ISS-2025-0639): a thread may load while the lock's owner is blocked in
  `thread_join` on it. Any OTHER wait still deadlocks: a directive that waits with
  `thread_get_message/1,2` for a message the loading thread sends after its load, or a
  `concurrent_*` call from a directive whose workers load files.
- **Threads**: thread, queue and mutex tables are JVM-wide, not per engine (as they were); every
  non-worker thread (CLI, IDE background solve, embedder, JUnit body) is the Prolog thread `main`
  and they share its one queue; missing `thread_signal/2`, `thread_statistics/3`,
  `message_queue_property/2`, `thread_send_message/3`, `mutex_property/2`, `thread_create_in_pool`;
  a mutex still held when its thread ends is released silently (SWI warns); a worker stopped by
  the budget reports `exception(inference_limit_exceeded)` (an atom, not a `resource_error`); the
  tabling store is still not safe for two threads producing the same table (see `Workers`).
- **Budget** (ISS-2025-0624): one pool per query, drawn in chunks of 1 024 steps, so with several
  machines the overshoot is bounded by the credit the others hold; "work" is approximated per list
  element / copied node; the bridged extended libraries (regex, xml, json, crypto, csv) are not
  charged at all — a pathological regular expression is CPU the budget cannot see.
- **Safe mode** (ISS-2025-0625): ~~`halt/0,1` stays~~ — resolved in 4.5 wave P7 (ISS-2025-0672):
  safe mode raises `permission_error(call, sandboxed, halt)` unless `SafeModeOptions.allowHalt()`
  (the CLI's `--safe` allows it); stream predicates work on the standard streams; `source_file/1,2` and
  `prolog_load_context/2` still report paths; `allowFileRead(dir)` checks the canonical path when
  the file is opened (a link swapped afterwards is not re-checked); memory and wall-clock time need
  the embedder's own limits.
- **CLI** (ISS-2025-0627): one query per input line; `--max-solutions N` ends the Nth answer with
  `.`; ~~the uncaught-error line of a parse error keeps its message-atom form~~ — resolved in 4.5
  wave P7 (ISS-2025-0671: `error(syntax_error(Msg), query)`).

**Workaround**: as noted per point.

---

## LIM-044: loading, reading and writing residue after 4.5 wave P3

**Found in 4.5 wave P3 (ISS-2025-0560..0579).** What the wave deliberately left out:
- **Mode-directed tabling**: `lattice(PI)` and `po(PI)` raise `domain_error(table_mode, M)`
  (not implemented); a call whose MODED argument is already bound evaluates with it bound (SWI
  evaluates with it free and unifies the best answer); `min`/`max` use the standard order of
  terms. `Spec as Options` ignores the options (every table is a variant table; `as` is not an
  operator, so write `table(as(p/1, subsumptive))`).
- **Loader**: loads of one engine are serialised (a per-engine lock — see LIM-045 for the one
  wait it now recognises); no `multifile/1` semantics
  (reconsulting a file wipes every user predicate it defined, even clauses other files added);
  `goal_expansion/2` is not applied; `library(X)` resolves only the prelude modules and a fixed
  list of libraries JProlog implements natively (no library search path);
  `initialization(G, main)` halts only under the CLI (ISS-2025-0636; an embedder gets the goal
  run after the load); `make/0` does not track
  included files; `use_module(File, Imports)` imports the whole module.
- **Reader**: `(a|b)` reads as `(a;b)` (SWI 7+ reads `'|'(a,b)`); `term_position/1` is the
  start position only and `subterm_positions/1`/`comments/1` answer nothing useful; reading
  more than 1 000 nesting levels re-reads the term on a helper thread (one thread per such term),
  and 200 000 levels is the hard limit (`resource_error(parser_nesting)`).
- **Console input**: `read/1` on `user_input` shares the engine's stdin reader with
  `get_char/1`, not with the CLI's own query reader (unchanged from 4.4.0).

**Workaround**: as noted per point.

---

## LIM-043: built-in conformance residue after 4.5 wave P4

**Found in 4.5 wave P4 (ISS-2025-0590..0612).** What the conformance wave deliberately left out:
- `format/2,3` column stops count from the start of the format output, not from the output
  stream's current column (SWI uses the stream's line position), so `write(abc),
  format("~t~w~10|", [x])` pads as if the line were empty.
- `format/2,3` argument faults are `error(format(Message), _)`; SWI 9 reports some of them as
  `error(format_argument_type(Directive, Arg), _)`. `~Nw`/`~Nq` right-align (a JProlog extension
  SWI ignores).
- `eof_action(reset)` behaves like `eof_code` (no tty re-arm); `read/1` on `user_input` does not
  track the past-end state.
- `print_message/2` has no `message_hook/3` and no `prolog:message//1` user extension; only
  `format/2`, `error/2` and unknown terms are rendered.
- `statistics/2`: `atoms`, `functors`, `codes`, `errors`, `warnings` answer 0; the memory keys
  are JVM heap/non-heap figures; `cputime`/`runtime` are the calling thread's CPU time.
- `format_time/3` implements the common strftime subset (`%Y %y %m %d %e %H %I %M %S %f %j %p
  %P %a %A %b %h %B %u %w %s %z %Z %F %D %T %R %c %n %t %%`); a format without `%` is a Java
  `DateTimeFormatter` pattern (the historical form).
- `last/2`, `nth0/3`, `nth1/3` on a partial list now enumerate without end (SWI): a caller that
  backtracks into them must bound the search (cut, `once/1`, a length test).
- `limit/2` and the rest of `library(solution_sequences)` do not exist.

**Workaround**: as noted per point.

---

## LIM-042: call-path caching covers compiled clause bodies only (4.5 wave P2)

**Found in 4.5 wave P2 (ISS-2025-0540..0553).** The call-site cache (ISS-2025-0540) lives on
the compiled skeleton of a clause body goal, so:
- a goal built at run time — `call/N`, the goal of `findall/3`, `forall/2`, `\+/1`, `once/1`, a
  maplist closure, a query typed at the top level — is resolved on every call (the resolution
  itself is much cheaper than in 4.4.0: no key strings, one set probe instead of ~40 comparisons);
- in a module context a goal is cached only when that module defines the predicate itself;
  goals resolved through imports, `user` or autoload are resolved per call;
- `maplist/2..7`, `foldl/4..7`, `include/3`, `exclude/3`, `partition/4,5` stay Prolog clauses in
  `library(apply)` with one meta-call per element (≈1 µs per element, about 1.5× a hand-written
  recursion); a native iteration was not done because the closure may leave choice points and
  must keep its four ports.
- `predsort/3` runs each comparison as a nested drive (≈0.5 µs per comparison).

**Workaround**: none needed for correctness; for hot loops prefer a first-order recursive
predicate over a meta-call.

---

## LIM-041: CLP(FD) — what library(clpfd) features are still missing (4.5 wave P5)

**Found in 4.5 wave P5 (ISS-2025-0640..0652).** The P5 wave made the solver SWI-compatible for
the common subset (propagation after unification, domains with holes and `inf..sup`, lazy
labeling with all SWI options and branch and bound, reification, `sum`/`scalar_product`,
`element`/`tuples_in`/`global_cardinality`, a domain-consistent `all_distinct`). What remains:

- **64-bit domains.** Bounds are longs with `inf`/`sup` markers. Ground expressions are exact
  big integers, and a value beyond the range is bound exactly when a functional constraint
  determines it (`X #= Y*10^12, Y = 10^12`), but propagation treats such a value as an infinite
  bound (sound, weaker), and a **coefficient** beyond 64 bits (`X*10^20 #= Y`) raises
  `representation_error(max_integer)`.
- **Slow convergence outside difference constraints.** A cycle of difference constraints
  (`X #> Y, Y #> X`, `X #>= Y + 5, ...`) fails at once (negative-cycle check), but a
  non-difference cycle over a huge domain (`2*X #> Y, Y #> 2*X`) still converges one bound step
  per round; it is interruptible (inference budget, Stop) but not fast. SWI has the same
  behaviour on some such systems.
- **Answers print domains, not residual constraints**: `X #> Y` answers `X`/`Y` unconstrained
  in the CLI instead of SWI's `Y#=<X+ -1`.
- **Not implemented**: `circuit/1`, `cumulative/1,2`, `disjoint2/1`, `automaton/3,8`, `chain/2`,
  `lex_chain/1`, `zcompare/3`, `fd_degree/2`, `(#=)/3`-style reified arithmetic beyond the six
  comparisons and `in/2`; `global_cardinality/3` options. `global_cardinality/2` does counting
  propagation only (not Régin's flow-based GAC).
- `labeling/2` with `min(E)`/`max(E)` runs each branch-and-bound round with the store's eager
  recursive labeler (depth = number of variables); the solutions themselves are then produced
  lazily in objective order.

---

## LIM-040: cyclic terms cannot be stored (4.5.0 wave P1, ISS-2025-0527)

Rational trees are supported as live terms (design B.17), but a cyclic term cannot be STORED:
`assertz/1`/`asserta/1` of a clause containing one, and `nb_setval/2` of a cyclic value, raise
`representation_error(cyclic_term)` (SWI-Prolog refuses cyclic clauses too). Before 4.5.0 such a
clause was stored with its cycle cut at an original cell — silently wrong once that binding was
undone. Supporting stored rational trees would need cycle-aware clause compilation (skeletons
are trees today); not planned.

## LIM-038: the EXTENDED LIBRARIES still raise message atoms, not ISO `error/2` terms

**Found in v4.4.0 (4.3 wave D, ISS-2025-0504..0512).** The ISO-core families that waves B and C
made native now raise `error(Formal, Context)` for every argument fault (see section 62 of
`docs/references/BUILTIN_PREDICATES_REFERENCE.md` and `EngineV4IsoErrorsTest`). The **bridged
extended libraries do not**: they throw a `PrologEvaluationException` whose error term is a bare
atom carrying an English sentence, e.g. `'xml_parse: argument must be an atom.'`. Such an exception
IS catchable, but only by a bare-variable catcher — `catch(G, error(type_error(atom, _), _), R)`
never matches one.

**Measured** by `scratchpad/43d/ErrProbe.java`, which calls every registered indicator at arities
1..4 with (a) every argument unbound and (b) a wrong-type first argument, and classifies what comes
back. Of 2 326 probed goals: 275 raise a proper `error/2`, 1 170 are wrong-arity complaints (see
below), and **315 raise a message atom**. By family:

| family | goals | family | goals | family | goals |
|---|---:|---|---:|---|---:|
| jdbc | 88 | filesystem | 28 | crypto | 26 |
| network | 26 | http | 22 | io (bridged half) | 22 |
| persistence | 18 | datetime | 14 | threading | 14 |
| logging | 12 | regex | 12 | csv | 8 |
| dcg (extension predicates) | 8 | json | 8 | os | 4 |
| xml | 4 | exception | 1 | | |

The single `exception` row is a false positive: `throw(foo)` throws `foo`, which is not an
`error/2` term by design. The 22 `io` ones are all in the still-bridged stream/parser half
(LIM-037): the arity guards of `writeq`, `write_canonical`, `flush_output`, `portray_clause` and
the remaining byte I/O. `put_byte/1,2` was fixed in this wave; `read/1,2`, `read_term/2,3`,
`close/1,2` and the stream-property predicates were not swept.

**Separately**, `BuiltInRegistry.isBuiltIn(Name, Arity)` answers true for arities the built-in does
not implement, so `char_code(X)` reaches `builtin.character.CharCode` and gets
`'char_code/2 requires exactly 2 arguments'` where ISO asks for
`existence_error(procedure, char_code/1)`. That is 1 170 of the 2 326 probed goals and is a
registry design question, not an error-term one; it is unchanged since 3.x.

**Workaround**: catch with a variable catcher and inspect the atom, or wrap the library call.

**Fix**: convert each library's argument validation to `Errors.instantiation/type/domain`. It is
mechanical but touches ~200 files; do it per family, driven by `ErrProbe`.

---

## LIM-037: the EXTENDED LIBRARIES still run on the eager built-in bridge

**Re-scoped again in v4.3.0 (4.2 wave C, ISS-2025-0500/0503):** `op/3`, `char_conversion/2`,
`current_char_conversion/2`, `char_type/2` and `code_type/2` are v4 natives, so **229** names can
still reach the adapter (234 before). With them the last external `core.engine.v4.Undo.record`
caller is gone: `Undo` and its `record` are package-private, and `builtin.clpfd.v2.ClpfdV2Bridge`
reaches the machine's trail through an `UndoSink` the engine installs.

**Re-scoped in v4.2.0 (4.1 wave B, ISS-2025-0496..0499).** The hot and ISO-core families are off
`core.engine.v4.LegacyBuiltinAdapter`: the `io` write family and `format/1,2,3`, the
atom/string/character/conversion families, `functor/3`/`arg/3`/`=../2`, the remaining type checks,
`succ/2`/`plus/3`/`unify_with_occurs_check/2`, the database family
(`current_predicate/1`, `retractall/1`, `abolish/1`, `dynamic/1`, `listing/0,1`), the global
variables and the ISO flags are v4 natives. Waves W3/W9 had already taken the list library, the
control and collection predicates, the term walkers, `clause/2`, `predicate_property/2`, CLP(FD)
posting/labeling, `current_op/3` and the two atom slicers.

**What is left**, measured from a live `Prolog` by `scratchpad/41b/probe/Fam2.java`: of 410
registered names, **121** are shadowed by a v4 native, **44** are control constructs or inline
built-ins the machine handles itself and never dispatches, **11** are defined by a prelude library
module (`Modules.overridesBuiltin` routes them to `callUser` before the adapter) — so **229 names
can still reach the adapter** in v4.3.0 (234 in v4.2.0, 305 before wave B; the v4.3.0 count is
126 native-shadowed). They are:

- **the extended libraries, 191 names** — `jdbc` 28, `filesystem` 15, `threading` 15, `crypto` 14,
  `ffi` 14, `graph` 13, `network` 13, `persistence` 13, `os` 12, `http` 11, `datetime` 10,
  `json` 6, `logging` 6, `regex` 6, `dcg` 5 (the non-default translators), `csv` 4, `xml` 3,
  `clpfd` 3 (`fd_dom`, `fd_size`, `indomain` — **native since 4.5 wave P5**, ISS-2025-0642/0649,
  so the count is 188 names / 226 in total after P5). **None of them has a hot-path claim**: every one is
  a call into a database, a socket, the file system, a process or a Java object, and the adapter
  hop is invisible next to what it does. They can stay bridged indefinitely.
- **19 `io` predicates** — `open/3,4`, `close/1,2`, `read/1,2`, `read_term/2,3`,
  `stream_property/2`, `set_stream/2`, `seek/4`, `set_stream_position/2`, `stream_position/2`,
  `stream_position_data/3`, `character_count/2`, `line_count/2`, `line_position/2`,
  `current_stream/3`, `get_byte/1,2`, `put_byte/1,2`, `peek_byte/1,2`, `print_message/2`,
  `portray_clause/1,2`. These are host I/O and parser integration, not inner-loop work.
- **`statistics/2`**, **`table/1`**, the 11 `debug`
  predicates (`spy/1`, `nospy/1`, `spying/1`, `leash/1`, `trace/0`, `notrace/0`, `debugging/0`,
  `profile/0`, `noprofile/0`, `profile_data/1`, `reset_profile/0`) and a handful of one-offs
  (`to_codes/2`, `atom_to_number/2`, `number_to_atom/2`, `rational/1`, `atom_gc/0`,
  `atom_table_size/1`).

A bridged deterministic built-in costs two extra term walks per call (the adapter dereferences the
goal and indexes its unbound cells by name) plus one `Map<String,Term>` per solution; a bridged
*nondeterministic* one still materialises every solution before the first is delivered. That is the
cost, and for everything in the list above it is not on a measured hot path.

`util.TermCopier` and `util.TermUtils` are still alive — re-verified in this wave, not assumed:
`TermCopier` is used by `core.engine.Rule`, `Prolog.compile`, `core.terms.Variable` and three
bridged built-ins, and `TermUtils` by `Prolog`, `ModuleManager`, `Module`, `TermParser`,
`DCGTransformer` and six bridged built-ins. Neither is reachable from the machine (it has
`Clause.instantiate` and `Unify.copy`). They go when `Rule`/`ModuleManager` stop needing them,
which is a core change, not a built-in migration.

Two behaviour differences on v4 are deliberate and approved (design B.17): rational trees are
supported (queries that raised `representation_error(cyclic_term)` now succeed;
`set_prolog_flag(occurs_check, error)` restores the ISO behaviour), and
`setup_call_cleanup/3`/`call_cleanup/2` run `Cleanup` after the goal's LAST solution rather than
eagerly after the first.

Since v4.0.0 these are simply how JProlog behaves, and since **4.1.0** there is no other engine to
compare them against (ISS-2025-0491). For the record, everything the v4 engine brought over the
deleted one: `library(yall)` lambdas (`existence_error(>>/4)` before), `partition/4`,
a terminating `sub_atom(A, B, L, Af, '')`, propagating bindings from a `when/2`-woken goal
(ISS-2025-0336), `frozen/2`, `term_attvars/2`, `copy_term/3`, `unifiable/3`, `?=/2`,
`current_table/2`, `current_module/1`, `memberchk/2`, **complete tabling** rather than a bounded
100-round re-evaluation, a fully open `append(X, Y, Z)` that enumerates and a
`member(X, PartialList)` that extends the open tail, cyclic terms, query memory that is O(live
data) rather than O(bindings), fast paths that are not disabled while debugging, and
**module-qualified built-in calls, autoloaded library modules and `meta_predicate/1`**.

## LIM-039: RESOLVED in 4.0.0 (wave W9) — a tabled evaluation is claimed by one thread

Since v4.0.0 `thread_create/2,3` and the `concurrent_*` family run on their own machines over the
same engine (LIM-024), and the engine's tabling store is shared with them. It was not thread-safe:
one variant table, one producing stack and one answer list, all plain `HashMap`/`ArrayList`, so two
threads *producing* interleaved their bookkeeping and a consumer could read a half-produced answer
set as if it were complete.

**ISS-2025-0488** claims an evaluation for one thread:

- a tabled CALL runs inside `Tabling.enterCall`/`exitCall`, so "does this variant exist, is it
  complete, do I produce it" and the frame it installs are atomic (in 4.0.0 the deleted v2 engine
  had the same protocol on `TableStore`; 4.1.0 removed that half with the engine);
- the claim is **held for the whole evaluation** — until the SCC completes, is abandoned, or the
  query ends — so a second thread never sees an EVALUATING table that is not its own;
- a worker machine hands the claim back when it finishes (`Tabling.endWorker`), abandoning whatever
  it left EVALUATING, because a worker owns no query boundary;
- reading a COMPLETE table stays effectively parallel: a consumer holds the claim only for the
  duration of the call decision.

Residual, deliberate: the wait is bounded at 60 s and then raises
`resource_error(tabling_busy)` rather than hanging, and a tabled goal that fans out into workers
which are *themselves* tabled serialises them (and, if the parent's evaluation cannot finish
without them, hits that timeout). Running several top-level `Prolog.solve` calls concurrently on
ONE `Prolog` instance is still outside the contract — a top-level query owns the
engine-wide query boundary and abandons every EVALUATING table when it ends; use
`concurrent_maplist/2,3,4`, `concurrent/3` or `thread_create/2,3` from one query instead.

## LIM-036: RESOLVED in 4.5.0 (wave P3.6, ISS-2025-0577) — .jpc source lines

`Prolog.compile` reads with the v2 reader, which stamps every clause with the line of its first
token (`TermReader.peekLine`), exactly as consult does; the legacy clause splitter is no longer
on the compile path.

## LIM-034: RESOLVED in v3.14.0 — engine state is per engine

v3.8.0 (ISS-2025-0437) made the ISO flag store, `occurs_check`, `trace/0` and the current
input/output stream per engine (or per thread). **v3.14.0 (wave W7, ISS-2025-0472/0474/0477)
finishes the job**: the stream table and its aliases, the operator store (`op/3`, `current_op/3`,
the parser table, the `.jpc` writer and the IDE formatter all read the one store), the spy points
and the profiler counters now belong to the `Prolog` instance. Each engine owns a
`core.engine.v4.EngineState` and installs it as the thread-current state around every
solve/consult; `StreamManager`, `OperatorDefinition`, `Spy` and `Profiler` keep their static APIs as
facades over it, so the legacy and v2 engines and any embedder code are unaffected. Reach a
specific engine's state with `Prolog.getEngineState()`, `Prolog.getStreams()`, `Prolog.getOps()`.

What is still shared by every `Prolog` in the JVM:

- the legacy `ConstraintStore.getInstance()` singleton (only reachable under
  `-Djprolog.clpfd=legacy`);
- the stream-handle counter (deliberately: `stream_1001` must denote at most one stream in the JVM);
- `StreamManager.setThreadLocalOutput` — a per-**thread** override by design, because the IDE
  installs it before any engine is current on the worker thread.

Consequence for embedders: `Prolog.enableSafeMode()` plus a fresh instance is now a real boundary
for streams, operators, spy points and the profiler as well as the flags. A separate JVM is still
the answer for a hard security boundary (`builtin.io` is deliberately not in the safe-mode deny
list, so `open/3,4` can still reach the host filesystem).

## LIM-027: CLOSED in 4.1.0 — open-tail generative list modes

Fully-open `append(X, Y, Z)` and open-tail `last([a|T], X)` / `maplist(G, [a|T])` used to produce
only the FIRST standard solution (open tails closed with `[]`) instead of enumerating: sound but
incomplete, a consequence of the eager built-in protocol (ISS-2025-0379/0380).

Resolved on the v4 engine in v3.13.0 (wave W6, ISS-2025-0468): `member/2` and `append/3` are the
two-clause definitions of `src/main/resources/prelude/lists.pl`, so `append(X, Y, Z)` enumerates
and `member(X, PartialList)` extends the open tail. It stayed on this list only because the v2
fallback still carried the bounded behaviour; **that engine is deleted in 4.1.0**
(ISS-2025-0491), so the limitation is closed. The deliberate consequence remains: a program that
relied on the bounded behaviour to terminate now loops.

---

## Known Limitations from 2026-06-07 Implementation Audit

The audit (`docs/reports/report-implementation-audit-2026-06-07.md`) confirmed 101
findings. The eight highest-confidence, low-risk items were fixed in v3.0.0
(ISS-2025-0245..0252). The following higher-effort items remain open:

| ID | Area | Limitation |
|----|------|------------|
| ~~LIM-017~~ | Parser | **RESOLVED v3.0.0** — clean-room v2 parser (default): single-pass `Lexer` is quote/escape/char-code aware; clause splitting is token-based. |
| ~~LIM-019~~ | Parser | **RESOLVED v3.0.0** — v2 parser (default): canonical functor `-(1,2)` and operator-as-atom (`X = -`, `foo(-,+)`) handled. `-Djprolog.parser=legacy` to fall back. |
| ~~LIM-021~~ | DCG | **RESOLVED v3.0.0** — clean-room v2 DCG translator (`core.dcg.v2.DCGTranslator`, now default): single recursive pass handling ISO head push-back, `[]`/terminal lists, strings, `{}`, `!`, `\+`, `(A,B)`/`(A;B)`/`(A\|B)`/`(A->B)`, `call//N`, and variable bodies. `-Djprolog.dcg=legacy` to fall back. |
| ~~LIM-022~~ | CLP(FD) | **RESOLVED v3.0.0** — clean-room v2 CLP(FD) (now default): interval domains (no OOM, no `TreeSet`), per-query identity store (no singleton leak), trail-backtracked **sound** labeling, real `#\=` propagation, `all_different` pigeonhole. Remaining = future *features* (`global_cardinality`, Hall-interval pruning, lazy labeling), not correctness gaps — all three delivered in 4.5 wave P5 (ISS-2025-0642/0650/0651); what is still missing is LIM-041. `-Djprolog.clpfd=legacy` to fall back. |
| ~~LIM-023~~ | Engine | **RESOLVED in 4.0.0** (waves W1-W9). The recursive engine that recursed in Java is deleted (ISS-2025-0484); the default `core.engine.v4.Machine` is iterative over mutable cells with compiled clause skeletons and first-argument indexing, every term walker is iterative and cycle-safe, and residual deep-structure `StackOverflowError`s in bridged built-ins still convert to `resource_error` terms (ISS-2025-0341). (The v2 `MachineSolver` fallback, which kept its own first-argument index — ISS-2025-0433 — is deleted in 4.1.0.) |
| ~~LIM-024~~ | Concurrency | **RESOLVED in v4.0.0** (wave W8, ISS-2025-0479/0480). `thread_create/2,3` and the `concurrent_*` family run every goal on a **fresh `core.engine.v4.Machine` over the same `Engine`** (`core.engine.v4.Workers`): shared clause store (thread-safe by birth/death generations), shared flags/operators/modules, per-thread current streams, one `ResourceGuard` per worker carrying the parent's budget, and a `copy_term`'d goal so no `Variable` cell is shared between machines. Interrupting the parent cancels the workers. (The old shared-solver behaviour survived on the fallback engines; both are deleted — the recursive one in 4.0.0, the v2 machine in 4.1.0.) |
| LIM-025 | Resource | **RESOLVED in v3.14.0** (wave W7, ISS-2025-0472): `with_output_to/2` on **both** engines, `format/3` with `atom/string/codes/chars`, `format ~@` and the `~p` portray path capture through the per-thread `StreamManager` output override alone — no `System.setOut`, no `user_output` stream swap — so two threads can capture concurrently and a capture never garbles unrelated output. The last built-ins that printed to `System.out` directly (`spy/1`, `nospy/1`, `leash/1`, `debugging/0`) were routed through `StreamManager.out()`. (Earlier: RESOLVED in v3.0.0 the `open/4` dangling-alias handle — ISS-0305; `StreamManager` map thread-safety, HTTP disconnect, JDBC statement leaks — ISS-0257..0260, 0265.) |

> Resolved in v3.0.0: LIM-018 (negative radix/char-code literals — ISS-2025-0256);
> LIM-020 (int/float distinguished as terms — ISS-2025-0261).

---

*Limitations LIM-001 through LIM-016 were all resolved as of v2.6.1.*

## Resolved Limitations

| ID | Feature | Resolved In |
|----|---------|-------------|
| LIM-001 | Coroutining (`freeze/2`, `when/2`, `dif/2`) | v2.6.0 |
| LIM-002 | Attributed variables (`put_attr/3`, `get_attr/3`) | v2.6.0 |
| LIM-003 | Global variables (`nb_setval/2`, `nb_getval/2`) | v2.6.0 |
| LIM-004 | Module-qualified calls (`Module:Goal`) | v2.6.0 |
| LIM-005 | `predicate_property/2` | v2.6.0 |
| LIM-006 | `code_type/2` | v2.6.0 |
| LIM-007 | Stream repositioning (`set_stream_position/2`) | v2.6.0 |
| LIM-008 | Arbitrary precision integers (BigInteger) | v2.6.0 |
| LIM-009 | `read_term/2` / `write_term/2` full options | v2.6.0 |
| LIM-010 | Constraint Handling Rules (CHR) | v2.6.1 |
| LIM-011 | DCG advanced features (pushback, `call//N`, if-then) | v2.6.1 |
| LIM-012 | Rational numbers (`rdiv`) | v2.6.1 |
| LIM-013 | Number literal notation (`0'a`, `0xFF`, `0o77`, `0b1010`) | v2.6.1 (already implemented) |
| LIM-014 | Multi-argument indexing | v2.6.1 |
| LIM-015 | Compiled clause cache | v2.6.1 |
| LIM-016 | Atom garbage collection | v2.6.1 |

Previously resolved:
- DCG parser limitations (ISS-2025-0040, ISS-2025-0041, ISS-2025-0042) resolved by ISS-2025-0085 Pratt parser rewrite

## Notes

- This file is automatically updated when new issues are identified
- When an issue is resolved, move it to the Resolved table
- Limitations are identified by `LIM-NNN` codes for easy reference
