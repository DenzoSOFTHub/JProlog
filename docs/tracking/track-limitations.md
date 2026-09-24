# JProlog Current Limitations

This document describes current limitations in JProlog implementation.
When an issue is resolved, the corresponding limitation should be removed from this file.

**Last updated**: 2026-09-24 (release 4.6.0 / 4.6 wave Q7: LIM-047 added (library goals that
still fail or succeed on a bad input; `phrase_with_options/4`), LIM-043/044/045 notes refreshed; 4.6 wave Q6: LIM-042 shrunk (run-time goal cache, native apply
family, predsort on the goal stack), LIM-037's io part resolved, LIM-045's budget part shrunk;
4.6 wave Q4: LIM-046 shrunk to answer completion + residual program,
LIM-045 shrunk (thread API, signals, load-cycle waits, tabling across threads done), LIM-044's
tabling part resolved; 4.6 wave Q3: LIM-044 shrunk to tabling modes + the reader/console
residue, LIM-045's load-lock part resolved; 4.6 wave Q1: LIM-038 resolved; release 4.5.0: waves P1..P7 of the production-readiness program)

---

## LIM-047: library goals that still fail or succeed on a bad input (4.6 wave Q7)

**Found in 4.6 wave Q1, reviewed in wave Q7 (ISS-2025-0796/0797).** The Q1 probe harness
(`ExtendedLibraryErrorsTest`, every registered name × arities 0..4 × {all unbound, first argument
`f(x)`}; 3 942 goals) has no message-atom error, no arity-guard message and no timeout left; its
report (`target/extended-library-errors.txt`) now lists the goals that FAIL (95, was 130) or
SUCCEED (154, was 180) on the probe arguments. Q7 made the clear cases raise — the FFI (22 goals),
the graph library (an unbound or non-list graph was the empty graph), `json_get/3`, `spy/1`,
`string_to_atom/2`, `atom_to_number/2`, `number_to_atom/2`, `to_codes/2` with both sides unbound,
and `enhanced_phrase/2,3` (now `phrase/2,3`). Most of the rest is correct (SWI does the same):
type checks and comparisons, generators on unbound arguments (`current_op/3`,
`stream_property/2`, `thread_property/2`, `predicate_property/2`, ...), output arguments that do
not unify (`get_time(f(x))`, `pid(f(x))`), open-list modes (`append/3`, `member/2`, `length/2`,
`maplist/N` with an unbound list), the write family, `thread_create(f(x), Id)` (the error is the
thread's). **Left as is, arguably should raise** (SWI's exact behaviour not verified here):
- `char_type(f(x), T)`, `code_type(f(x), T)`, `number_string(f(x), S)`, `string_chars(f(x), L)`,
  `string_codes(f(x), L)`, `atom_to_number(f(x), N)`, `number_to_atom(f(x), A)`,
  `to_codes(f(x), L)` fail; `string_to_atom(f(x), A)` succeeds with `A = 'f(x)'` — SWI's text
  predicates raise `type_error` on a compound.
- `format(f(x))`, `format(f(x), Args)` fail (SWI: a format error).
- `reverse(L, R)` and `select(X, L, R)` with everything unbound fail (SWI enumerates);
  `predsort(f(x), L, S)` fails.
- `spy(foo)` (a bare name) and `spy(f(x))` (a head) fail — SWI accepts both.
- **`phrase_with_options/4` never runs the grammar**: it goes through the old
  `builtin.dcg.EnhancedPhrase` expansion, which answers `true` for any non-trivial body
  (`phrase_with_options(undefined_nt, [a], R, [])` succeeds). `enhanced_phrase/2,3` were fixed
  (natives = `phrase/2,3`); this one keeps the registry class.

**Workaround**: check the input before the call (`must_be/2`); use `phrase/2,3` instead of
`phrase_with_options/4`.

---

## LIM-046: tabled negation — the well-founded semantics is minimal (4.5 wave P7, shrunk in 4.6 wave Q4)

**Found in 4.5 wave P7 (ISS-2025-0661, decision §8 of the production-readiness program).**
*Shrunk by 4.6 wave Q4 (ISS-2025-0755): `tnot/1`, `undefined/0` and `call_delays/2` exist; a
loop through `tnot/1` delays the negative literal, answers carry delay lists, the SCC's completion
simplifies them, and what stays conditional is reported as undefined (CLI `undefined`,
`Prolog.currentAnswerDelays()`). The win/1 game, `p :- tnot(p)` and mutual negation answer as in
SWI.* What remains:
- **No answer completion**: a conditional answer whose only support is a positive loop among
  conditional answers (all of whose external supports became false) stays *undefined*; SLG with
  answer completion (SWI) makes it false.
- **No residual program** (`call_residual_program/2`, the toplevel's "% WFS residual program"
  display); `call_delays/2` gives the delayed literals of one derivation.
- **Delays through mode-directed tables** are ignored (an aggregated answer is unconditional).
- **`\+ G`** over a table an ancestor is still evaluating keeps raising
  `error(permission_error(negate, incomplete_table, G), (\+)/1)` — use `tnot/1` for WFS.

**Workaround**: write `tnot/1` (not `\+`) for negation over tabled predicates; for the unsupported
positive-loop case, stratify that part of the program.

---

## LIM-045: production-hardening residue after 4.5 wave P6

**Found in 4.5 wave P6 (ISS-2025-0620..0639).** What the wave deliberately left out:
- **Load lock** (ISS-2025-0639, ~~any other wait deadlocks~~ — resolved in 4.6 wave Q3,
  ISS-2025-0739; ~~message waits invisible, `op/3` module context engine-wide~~ — resolved in 4.6
  wave Q4, ISS-2025-0746/0748). What remains: a message wait counts as blocked only when every
  thread the engine can see as a possible sender is blocked; a non-worker thread that is idle
  (not in a query) is not seen, so a cycle it could have broken is refused (and one only it can
  break is not detected until it becomes busy or blocked).
- **Threads**: thread, queue, mutex and pool tables are JVM-wide, not per engine (as they were);
  every non-worker thread (CLI, IDE background solve, embedder, JUnit body) is the Prolog thread
  `main` and they share its one queue, its signal queue (a signal to `main` is run by whichever
  of them polls first) and its one table space (documented as a deliberate difference in
  `ref-deviations.md` §4e since 4.6 wave Q7); ~~missing `thread_signal/2`, `thread_statistics/3`,
  `message_queue_property/2`, `thread_send_message/3`, `mutex_property/2`,
  `thread_create_in_pool/4`; a mutex held at exit released silently; the tabling store not safe for
  two threads~~ — resolved in 4.6 wave Q4 (ISS-2025-0749..0752); ~~a worker stopped by the budget
  reports an atom~~ — resolved in 4.6 wave Q1 (ISS-2025-0698). A signal reaches a thread blocked
  in a thread built-in within 100 ms at worst (they wait in slices) and a thread blocked elsewhere
  in Java code (a blocking `read/1` on a socket) only when it returns to Prolog.
- **Budget** (ISS-2025-0624): one pool per query, drawn in chunks of 1 024 steps, so with several
  machines the overshoot is bounded by the credit the others hold; "work" is approximated per list
  element / copied node; ~~the bridged extended libraries (regex, xml, json, crypto, csv) are not
  charged at all~~ — resolved in 4.6 wave Q6 (ISS-2025-0786): a bridged call is charged one step
  per 64 characters of text input and one per solution, and a regular expression one step per 256
  characters its matcher reads (backtracking included). What remains: library work that is not
  proportional to the text input (an XPath query, a JDBC or HTTP call) is one charge, and time a
  library spends blocked on the host (a socket read) is not CPU and is not charged.
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
*Shrunk by 4.6 wave Q3 (ISS-2025-0730..0739): multifile/1 and per-file clause ownership,
goal_expansion/2, file_search_path/2 + absolute_file_name/3, use_module/2 import lists, `(a|b)` as
`'|'(a,b)` and `as` as an operator, make/0 over included files, `Prolog.runMain()`,
subterm_positions/1 and comments/1, and the per-file load lock are done.*
- ~~**Mode-directed tabling**: `lattice(PI)`, `po(PI)`; bound moded arguments; `Spec as Options`~~
  — resolved in 4.6 wave Q4 (ISS-2025-0753/0754): all SWI modes, free evaluation of moded
  arguments, `shared`/`private` implemented and the other options reported with a warning
  (`ref-deviations.md` §4c). `min`/`max` use the standard order of terms, which is SWI's own rule.
- **Loader**: a clause of a non-multifile predicate in a second file is ADDED (SWI redefines the
  predicate with a warning); `goal_expansion/2` does not follow `meta_predicate` declarations of
  user predicates; the `file_search_path/2` defaults are not clauses; a clause `M:H` for a module
  that ALSO defines the predicate in its own file is not seen by a qualified call (the module's
  clauses win); a reloaded file's clauses of a multifile predicate come back as ONE block at the
  place of the file's first clause (SWI diffs clause by clause; 4.6 wave Q7, ISS-2025-0794 — they
  used to be appended); `initialization(G, main)` halts only under the CLI (an embedder uses
  `Prolog.runMain()`, ISS-2025-0736). See `ref-deviations.md` §4b.
- **Reader**: `term_position/1` is the position before the term's leading layout; subterm
  offsets count UTF-16 units; reading more than 1 000 nesting levels re-reads the term on a helper
  thread (one thread per such term), and 200 000 levels is the hard limit
  (`resource_error(parser_nesting)`).
- **Console input**: `read/1` on `user_input` shares the engine's stdin reader with
  `get_char/1`, not with the CLI's own query reader (unchanged from 4.4.0).

**Workaround**: as noted per point.

---

## LIM-043: built-in conformance residue after 4.5 wave P4

**Found in 4.5 wave P4 (ISS-2025-0590..0612).** What the conformance wave deliberately left out
(shrunk by 4.6 wave Q2: the format column stops, the `print_message/2` hooks and
`library(solution_sequences)` are done — ISS-2025-0710, 0713, 0714):
- `format/2,3` column stops start at the stream's column for the console, files and
  `with_output_to/2`; a thread-local capture installed by an embedder (the IDE's Run panel) is
  not column-tracked unless it is a `core.engine.v4.ColumnPrintStream`, so there they count from
  the start of the format call.
- `format/2,3` argument faults are `error(format(Message), _)`; SWI 9 reports some of them as
  `error(format_argument_type(Directive, Arg), _)`. `~Nw`/`~Nq` right-align (a JProlog extension
  SWI ignores).
- `eof_action(reset)` behaves like `eof_code` (no tty re-arm); `read/1` on `user_input` does not
  track the past-end state.
- `print_message/2`: SWI's own message translations are not reproduced (only `format/2`,
  `error/2` and unknown terms are rendered by default; `prolog:message//1` extends it); the lines
  handed to `message_hook/3` for a built-in translation are `['~w'-[Line], nl, ...]`;
  `message_property/2`, `print_message_lines/3` and `message_to_codes/3` do not exist.
- `statistics/2`: `atoms`, `functors`, `codes`, `errors`, `warnings` answer 0; the memory keys
  are JVM heap/non-heap figures; `cputime`/`runtime` are the calling thread's CPU time.
- `format_time/3` implements the common strftime subset (`%Y %y %m %d %e %H %I %M %S %f %j %p
  %P %a %A %b %h %B %u %w %s %z %Z %F %D %T %R %c %n %t %%`); a format without `%` is a Java
  `DateTimeFormatter` pattern (the historical form).
- `last/2`, `nth0/3`, `nth1/3` on a partial list now enumerate without end (SWI): a caller that
  backtracks into them must bound the search (cut, `once/1`, a length test).
- `library(solution_sequences)`: `reduced/1,3` and `group_by/4` do not exist.
- ~~`append/2`, `nextto/3`, `max_member/2`, `list_to_set/2`, library(ordsets), the recorded
  database, `flag/3` missing~~ — added in 4.6 wave Q7 (ISS-2025-0790/0791; also
  `min_member/2`, `max_member/3`, `min_member/3`, `proper_length/2`, `current_key/1`).
  `library(ordsets)`'s `ord_union/4`, `ord_intersection/4` and `ord_memberchk/2`'s SWI
  unrolled fast path are not provided.

**Workaround**: as noted per point.

---

## LIM-042: call-path caching — what is left (4.5 wave P2; shrunk in 4.6 wave Q6)

**Found in 4.5 wave P2 (ISS-2025-0540..0553); shrunk by 4.6 wave Q6 (ISS-2025-0777..0780).**
~~A goal built at run time (call/N, findall/forall/\+/once goals, top-level queries) is resolved
on every call~~ — a per-machine call-site cache since Q6.1 (ISS-2025-0777); ~~maplist/2..7,
foldl/4..7, include/3, exclude/3, partition/4,5 are Prolog with one meta-call per element~~ —
native frames since Q6.2 (ISS-2025-0780, same ports and choice points); ~~predsort/3 runs each
comparison as a nested drive~~ — on the goal stack since Q6.4 (ISS-2025-0779). What remains:
- in a module context a goal (a body goal or a run-time one) is cached only when that module
  defines the predicate itself; goals resolved through imports, `user` or autoload are resolved
  per call;
- the run-time cache is per machine (each top-level query starts empty) and direct-mapped (256
  entries; a collision just re-resolves);
- `predsort/3` costs ~50 ns per comparison beyond calling the comparator — the Q6.4 target (2x on
  predsort 1e5) was not reached: the comparator's own resolution and activation, the same as a
  direct call, is now the floor (measured: a failure-driven loop calling the comparator costs
  about 75 % of the sort).

**Workaround**: none needed for correctness; for hot loops prefer a first-order recursive
predicate over a meta-call.

---

## LIM-041: CLP(FD) — what library(clpfd) features are still missing (4.5 wave P5; shrunk in 4.6 wave Q5)

**Found in 4.5 wave P5 (ISS-2025-0640..0652); shrunk by 4.6 wave Q5 (ISS-2025-0760..0770).** P5
made the solver SWI-compatible for the common subset; Q5 added residual-constraint printing
(answers, `copy_term/3`, `clpfd:attribute_goals//1`), `circuit/1`, `cumulative/1,2`,
`disjoint2/1`, `lex_chain/1`, `chain/2`, `automaton/3,8`, `zcompare/3`, `fd_degree/2`,
`global_cardinality/3`, exact coefficients beyond 64 bits and an iterative branch and bound.
What remains:

- **64-bit domains.** Bounds are longs with `inf`/`sup` markers. Ground expressions,
  coefficients (since Q5) and a value a functional constraint determines are exact big integers,
  but a domain BOUND beyond the range is treated as infinite (sound, weaker): with
  `Y #= X*10^20`, `Y #< 5*10^20` does not prune `X` (`10^20*X #< 5*10^20` does).
- **Slow convergence outside difference constraints.** A cycle of difference constraints fails
  at once (negative-cycle check), but a non-difference cycle over a huge domain
  (`2*X #> Y, Y #> 2*X`) still converges one bound step per round; interruptible, not fast.
- **Propagation strength of the decomposed globals.** `lex_chain/1`, `disjoint2/1`,
  `automaton/8` counters and `zcompare/3` are reified decompositions (correct, weaker than
  SWI's dedicated propagators); `global_cardinality/2,3` does counting propagation only (not
  Régin's flow-based GAC); `cumulative` is time-table only (no edge finding); `abs(X-Y) #\= C`
  goes through an auxiliary variable (20-queens written that way is ~3.9 s with `ff`, against
  a few ms with two `#\=`; SWI has `absdiff_neq`).
- **Residual goals** print SWI's forms for the common constraints; complex expressions show
  JProlog's own decomposition (`_A` auxiliaries), and the decomposed globals print their parts
  (see `ref-deviations.md` §4d).
- **Large models**: every post and labeling step scans all FD cells for newly determined ones
  (`ClpfdV2Bridge.determinedCells`), so a 20 000-variable chain costs ~4 s to post and ~6 s to
  label leftmost (O(n²)); fine for the usual few hundred variables (a 4.6 Q6 candidate).
- Not implemented: `(#=)/3`-style reified arithmetic beyond the six comparisons and `in/2`.

---

## LIM-040: cyclic terms cannot be stored (4.5.0 wave P1, ISS-2025-0527)

Rational trees are supported as live terms (design B.17), but a cyclic term cannot be STORED:
`assertz/1`/`asserta/1` of a clause containing one, and `nb_setval/2` of a cyclic value, raise
`representation_error(cyclic_term)` (SWI-Prolog refuses cyclic clauses too). Before 4.5.0 such a
clause was stored with its cycle cut at an original cell — silently wrong once that binding was
undone. Supporting stored rational trees would need cycle-aware clause compilation (skeletons
are trees today); not planned.

## LIM-038: RESOLVED in 4.6 wave Q1 (ISS-2025-0680..0699) — the extended libraries raise ISO `error/2` terms

**Found in v4.4.0** (4.3 wave D): the bridged extended libraries threw a
`PrologEvaluationException` whose ball was a bare message atom (`'xml_parse: argument must be an
atom.'`), which only a variable catcher could see; and `BuiltInRegistry.isBuiltIn(Name, Arity)`
answered true for every arity of a name with no arity entry, so `char_code(X)` reached the Java
class and answered `'char_code/2 requires exactly 2 arguments'`.

**Resolved.** `ExtendedLibraryErrorsTest` (the successor of `scratchpad/43d/ErrProbe.java`) probes
every registered name at arities 0..4 with every argument unbound and with a wrong-type first
argument: **368 message atoms + 1 089 arity-guard messages** (of 3 780 goals) before the wave,
**0 + 0** (of 3 825) after. Every library argument check raises
`error(Formal, context(Name/Arity, Message))` (`core.engine.v4.Errors`, `builtin.LibArgs`); a
host failure maps to `existence_error/2`, `permission_error/3`, `io_error/2` or `system_error/1`
(`Errors.host`); every registered name declares its exact arities (ISS-2025-0685), so an
unimplemented arity is `existence_error(procedure, PI)` and may be user-defined; and a worker
stopped by the budget reports `exception(error(resource_error(inference_limit), _))`
(ISS-2025-0698). Deliberate differences are in `docs/references/ref-deviations.md` §2 — notably
the FFI (`java_*`) still **fails** on a bad argument instead of raising (it is a fail-on-anything
design, not a message atom; left for a later wave).

---

## LIM-037: the EXTENDED LIBRARIES still run on the eager built-in bridge

**Re-scoped in 4.6 wave Q6 (ISS-2025-0785):** the stream half of `io` is native
(`core.engine.v4.NativeStreams`): `open/3,4`, `close/1,2`, `stream_property/2` and
`current_stream/3` (generators, no longer materialising every solution), `set_stream/2`,
`seek/4`, `set_stream_position/2`, `stream_position/2`, `stream_position_data/3`,
`character_count/2`, `line_count/2`, `line_position/2`, `get_byte/1,2`, `peek_byte/1,2`,
`put_byte/1,2`, `portray_clause/1,2` (16 names; `read/1,2`, `read_term/2,3` and `print_message/2`
were already native). The "19 io predicates" bullet below is resolved; what reaches the adapter
is the extended libraries, `table/1`, the debug predicates and the one-offs listed below. The
bridged libraries are charged to the inference budget since the same wave (ISS-2025-0786).

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
- ~~**19 `io` predicates**~~ (native since 4.6 wave Q6, ISS-2025-0785) — `open/3,4`, `close/1,2`, `read/1,2`, `read_term/2,3`,
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
