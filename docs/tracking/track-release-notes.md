# JProlog - Release Notes

## Release 4.5.0 - 2026-09-23

### The production-readiness release (waves P1..P7)

Seven waves turned a five-way audit of 4.4.0 into fixes: engine semantics (P1), performance (P2),
loading/reading/writing (P3), built-in conformance (P4), CLP(FD) (P5), production hardening (P6)
and the test suite plus this release (P7). ISS-2025-0514 .. ISS-2025-0675 (ranges per wave in
CHANGELOG.md). Spec and wave records: `docs/reports/report-production-readiness-2026-09-23.md`.

**1452/1452 JUnit tests** (4.4.0: 1313), **20/20 example programs** with unchanged per-program
counts. Every deliberate difference from ISO/SWI is now listed in one place:
`docs/references/ref-deviations.md`.

#### Upgrading — READ THIS

Behaviour a program can notice (details and the full list in CHANGELOG.md and
`ref-deviations.md` §3):
- `op/3` is permanent on backtracking; `integer/1` rounds; a negative shift shifts the other way;
  `intersection/union/subtract` keep duplicates; `M:G` runs `G` in `M` even if not exported;
  `print/1` quotes; `tab/1` evaluates; global variables are per thread.
- More argument faults raise instead of failing: `string_concat(-,-,-)`, `call((fail, 1))`
  (checked before running), `plus/3` with floats, `atomic_list_concat(L, '', A)` split,
  `format ~r` without a radix, a query that does not parse (`error(syntax_error(M), query)`).
- Directives run once; consult errors are warnings and the load continues; `listing/1` has SWI's
  layout; floats print in SWI's layout (`10000000.0`, `1.0Inf`).
- `last/2`, `nth0/3`, `nth1/3` enumerate on a partial list; CLP(FD) labeling is lazy.
- The CLI loads no demo facts (use `--demo`); answers are streamed; exit codes follow `halt/1`.
- Safe mode also sandboxes `halt/0,1` (`SafeModeOptions.allowHalt()` restores it) and the native
  table; `SafeModeOptions.allowFileRead(dir)` whitelists read access.
- Tabled non-stratified negation raises `permission_error(negate, incomplete_table, G)`.
- The inference budget is one pool per query, shared with its worker threads.
- Map answers are copies (they no longer change when the next answer is computed) and omit `_X`
  variables.

#### What is new

- A runnable jar: `mvn package` → `target/jprolog.jar`; `java -jar target/jprolog.jar file.pl
  -g main -t halt`. CLI options `-g`, `-t`, `--safe`, `--budget N`, `--max-solutions N`,
  `--demo`, `--batch`, `--interactive`, `-q`.
- The loaders (`consult/1`, `[F]`, `ensure_loaded/1`, `load_files/1,2`, `make/0`, `include/1`),
  string streams, `read_term_from_atom/3`, `expand_term/2`, `term_expansion/2`, moded tabling.
- A much larger CLP(FD): `ins/2`, `sum/3`, `scalar_product/4`, reification, `element/3`,
  `tuples_in/2`, `global_cardinality/2`, `all_distinct/1` (Régin), lazy labeling with options.
- SWI threads: mutexes, selective receive, `thread_property/2`, `thread_exit/1`,
  `concurrent_forall/2,3`.
- Performance: nrev/loop −59..71 %, deriv −51..55 %, O(1) `asserta/assertz/retract`, linear
  `retractall/1`, `bagof/setof` O(n log n), `.jpc` format 0x04 (loads in 23 % of the consult time).

#### Deleted

`builtin/io/Read`, `builtin/io/ReadTerm`, `builtin/term/AtomToTerm`, `builtin/term/TermToAtom`,
`builtin/arithmetic/Between`, `DCGUtils.DCGTranslateRule` (unreachable; placeholders keep the
registry names) and the never-run `BuiltInTests.java`.

#### Known limitations

LIM-040 .. LIM-046 in `docs/tracking/track-limitations.md` (new in this release): cyclic terms
cannot be stored; CLP(FD) residue; call-site caching scope; built-in residue; loader/reader
residue; hardening residue (including the missing `thread_signal/2`); tabled negation without
WFS.

---

## Release 4.4.0 - 2026-08-26

### Wave D of 4.3: ISO error conformance, indexed `retractall/1`, `bounded = false`, the cleanup catch escape

The four items the 4.3.0 wave record named, plus one defect independent verification found.
ISS-2025-0504 .. ISS-2025-0513. Wave record:
`docs/reports/report-engine-v4-progress.md` section 19.

**1313/1313 JUnit tests**, **20/20 example programs** with unchanged per-program counts, and the
generated Reference Manual's worked examples unchanged apart from a timestamp and two gensym
numbers. The new conformance oracle, `EngineV4IsoErrorsTest`, drives 249 `Goal -> expected error
term` rows: **4.3.0 answered 161 of them (64.7%), 4.4.0 answers all 249**.

#### Upgrading — READ THIS, it is a behaviour change

Most of the wave replaces an exception carrying an English sentence with a real
`error(Formal, Context)` term, which only makes `catch/3` more useful. Six things a program can
actually notice:

```
current_prolog_flag(bounded, B)   NOW B = false. JProlog's integers are arbitrary precision
                                  (X is 10^30 is exact), so `true` was wrong and contradicted
                                  Appendix A of the manual. A program that branches on the flag
                                  now takes the other — correct — branch. max_integer/min_integer
                                  are unchanged in value; they mean the limits of the fast 64-bit
                                  representation, not a limit on arithmetic.

put_char(X), put_code(a),         NOW RAISE where they used to FAIL. Also current_input(foo),
atom_number(A, N), succ(a, X),    format(X), number_string(N, S), string_chars(S, L),
between(1, 2, a), length(foo, N)  string_length(S, L), atomic_list_concat(L, A),
                                  term_to_atom(T, A) with both unbound, plus(a, 1, X),
                                  succ(-1, X), length([a|b], N), length([a], a), length(L, -1),
                                  current_prolog_flag(nosuchflag, V),
                                  current_char_conversion(ab, X), X in a, label(a).
                                  Wrap in catch(G, _, fail) if you relied on the failure.

write_term(a, foo)                NOW type_error(list, foo); write_term(a, [quoted(true)|_]) is
                                  instantiation_error. Both used to write the term with DEFAULT
                                  options, silently ignoring the malformed list.

set_prolog_flag(foo, bar)         NOW domain_error(prolog_flag, foo) — an unknown flag is no
                                  longer created. A read-only flag is
                                  permission_error(modify, flag, F) and a rejected value is
                                  domain_error(flag_value, F+V).

op(_, xfx, ','), op(700, xfx, '|') NOW REFUSED (permission_error). Both used to succeed and could
                                  leave the reader unable to parse ordinary terms. '|' is still
                                  accepted at priority 0 or as an infix operator of priority
                                  >= 1001, which is the ISO window.

setup_call_cleanup/3,             NOW check Setup, Goal and Cleanup BEFORE Setup runs, so an
call_cleanup/2                    argument fault is raised inside the enclosing catch scope.

a cleanup that THROWS             NOW REACHES catch/3. catch(call_cleanup(throw(a), throw(b)),
                                  E, true) binds E = b; it used to reach the Java embedder as an
                                  uncaught PrologException (on 4.3.0 too — pre-existing).
                                  The CLEANUP's ball replaces the goal's, so a catcher written
                                  for the goal's ball no longer matches it:
                                  catch(catch(call_cleanup(throw(a), throw(b)), a, r1), E2, true)
                                  gives E2 = b, not r1. Nested cleanups all run and the OUTERMOST
                                  ball survives. A cleanup reached by an unwinding ball also now
                                  sees the goal's bindings instead of finding them undone.
                                  The trust model is untouched: a budget abort or a Stop inside a
                                  cleanup is still NOT catchable by catch/3.
```

Section 62 of `docs/references/BUILTIN_PREDICATES_REFERENCE.md` is the full before/after table,
including the 21 rows where JProlog deliberately differs from ISO and why.

#### Faster

```
retractall(f(Key, _))             -82% over a 20 000-clause table (188 -> 33 ms for 200 calls):
                                  it selects through the first-argument index instead of walking
                                  the whole knowledge base with a Term.unify per clause.
                                  Bulk retractall(f(_, _)) and abolish/1 are unchanged — both
                                  were benchmarked and are already single passes.
```

No benchmark is more than 5% slower than 4.3.0 (18 interleaved A/B control pairs).

#### New limitation

**LIM-038**: the bridged extended libraries (jdbc, filesystem, crypto, network, http, persistence,
datetime, threading, regex, logging, csv, dcg, json, os, xml and the stream half of io) still raise
message atoms rather than ISO `error/2` terms — 315 goals out of 2 326 probed.

---

## Release 4.3.0 - 2026-08-26

### Wave C of 4.2: indexing everywhere, `op/3` native, `char_type/2` generators

The three items the 4.2.0 wave record recommended for the next wave, plus the assert/retract
measurement it left open. ISS-2025-0500, ISS-2025-0502, ISS-2025-0503. Wave record:
`docs/reports/report-engine-v4-progress.md` section 18.

**1301/1301 JUnit tests** (1261 + 40 new), **20/20 example programs** with unchanged per-program
counts, and the generated Reference Manual's worked examples unchanged apart from timestamps and
gensym numbers. Names that can still reach `LegacyBuiltinAdapter`: **234 -> 229**.

#### Upgrading

Nothing to do. Everything below is either faster, or a mode that used to fail or leak.

```
retract/1, clause/2           SAME ANSWERS, IN THE SAME ORDER, but selected through the
                              first-argument index instead of a full scan. Emptying a
                              20 000-clause predicate went from 22.7 s to 0.3 s.
char_conversion(a, b)         NOW PER ENGINE and undone on backtracking. The conversion table was
                              a process-global static: two Prolog instances in one JVM shared it,
                              and a conversion posted under a choice point survived the failure.
op(P, T, N)                   unchanged, but on the calling engine's store and undone on
                              backtracking, as a native. Module scoping, the list form,
                              precedence-0 removal and every error term are as they were.
current_char_conversion(1, X) NOW FAILS. It raised system_error(ClassCastException).
char_type(C, digit(W))        THE PARAMETRIC FORMS WORK, in every mode: digit(Weight),
code_type(C, upper(L))        upper(Lower), lower(Upper), to_lower(Lower), to_upper(Upper).
                              char_type/2 had none of them; code_type/2 could only TEST three, so
                              code_type(0'a, lower(U)) succeeded with U unbound. It now binds
                              U = 0'A. The parameter is a character for char_type/2 and a code for
                              code_type/2; digit(Weight)'s weight is an integer in both.
char_type(C, csym)            SIX NEW CLASS NAMES for char_type/2 — csym, csymf, white, period,
                              quote, paren — and the ten char_type/2-only names (xdigit, newline,
                              end_of_file, layout, meta, solo, symbol and the atom forms of
                              digit/upper/lower) now work for code_type/2 too. Both keep their
                              historical enumeration order, with the new classes appended.
```

Removed: `it.denzosoft.jprolog.builtin.system.Op`, an `op/3` implementation that
`BuiltInFactory` never registered (it captured `OperatorTable.getDefault()` in its constructor —
the multi-engine bug the 4.2.0 notes recorded, in code nothing could reach). The live `op/3` is
unaffected. `core.engine.v4.Undo` and its `record(Runnable)` are package-private: an embedder that
called `Undo.record` (there was no supported reason to) must instead implement a v4 native.

#### Faster

Interleaved A/B against the 4.2.0 classes, one session, 6 pairs, best of 6 warm iterations per JVM,
median over runs. The noise floor on the measuring VM is ~5%.

| benchmark | 4.2.0 | 4.3.0 | change |
|---|---|---|---|
| `clause(tbl(K, _), _)` x2 000 into a 20 000-clause table | 2 948 ms | 10 ms | **-99.7%** |
| `retract(rt(K, _))` over a 2 000-clause table | 196 ms | 21 ms | **-89%** |
| `loop(1000000)` | 414.5 ms | 346.5 ms | **-16%** |
| `nrev` of 30 elements x2 000 | 345.5 ms | 302 ms | **-13%** |
| lookup in a 20 000-fact table x20 000 | 16 ms | 14 ms | **-13%** |
| dispatch over a 200-clause predicate x60 000 | 24.5 ms | 22 ms | **-10%** |
| `assertz(z(N)), retract(z(N))` x100 000 | 164.5 ms | 156.5 ms | **-5%** |

The two big wins are `retract/1` and `clause/2` finally using the index the machine has used since
wave W2; the across-the-board 10-16% comes from the index key, which used to be a string built by
concatenation (and, for an integer, a `BigInteger` and its decimal rendering) on every call with a
bound first argument.

**And a non-regression**: an independent measurement had put the assert/retract loop 5-9% slower on
4.2.0 than on 4.1.0. Re-measured over two interleaved sessions and 12 samples per side, the medians
are 164.5 ms (4.1.0) against 162.5 ms (4.2.0) — **-1.2%**, with an untouched control benchmark
moving 0.0% and -1.4% in the same sessions. There was no regression to fix.

---

## Release 4.2.0 - 2026-08-26

### Wave B of 4.1: the hot and ISO-core built-in families leave the eager bridge

94 predicate indicators move from the legacy `(goal, Map<String,Term>, List<Map<String,Term>>)`
contract to the **v4 native SPI**: the `io` write family and `format/1,2,3`, the atom / string /
character / conversion families, `functor/3` / `arg/3` / `=../2`, the remaining type checks,
`succ/2` / `plus/3` / `unify_with_occurs_check/2`, the database family, the global variables and
the ISO flags. ISS-2025-0496..0501. Wave record:
`docs/reports/report-engine-v4-progress.md` section 17.

**1261/1261 JUnit tests** (1196 + 65 new), **20/20 example programs** with unchanged per-program
counts. Names that can still reach `LegacyBuiltinAdapter`: **305 -> 234**.

#### Upgrading

Nothing to do. The migration is behaviour-preserving down to the error terms — a characterisation
run of ~250 goals over the four families is byte-identical between 4.1.0 and 4.2.0 apart from the
six deliberate changes below, and the generated Reference Manual's ~450 worked examples produce the
same answers.

```
term_string(?Term, ?String)   NEW. The SWI string twin of term_to_atom/2, both directions.
findall(Tmpl, Goal, L, Tail)  NEW. findall/3 with an open tail: L ends in Tail, not [].
listing(foo/1) / listing(foo) NOW WORKS. It raised "listing/0 takes no arguments" in every
                              release that documented it (BuiltInFactory binds one class per NAME
                              and `listing` was bound to the arity-0 class). A bare name lists
                              every arity. listing/0,1 also print through StreamManager.out(), so
                              with_output_to/2 and the IDE console capture them, and a clause is no
                              longer printed with a doubled full stop.
arg(N, T, A), N unbound       NOW ENUMERATES (ISO 8.5.2), lazily. It raised instantiation_error.
functor(f(X), N, A)           NOW ANSWERS N=f, A=1 (ISO 8.5.1). It raised instantiation_error,
                              because the mode was chosen with Term.isGround().
writeq(Stream, T)             now resolves Stream the way write/2 does: it is captured by
                              with_output_to/2 and by the IDE console (it went straight to the
                              process stdout), and reports the ISO stream errors instead of a bare
                              evaluation error for a non-stream argument.
put_code(Stream, Code)        now writes to Stream. It threw "put_code/1 requires exactly 1
                              argument" although the arity entry existed.
assertz/retract/retractall/   now raise permission_error for freeze/2, when/2, dif/2, put_attr/3,
abolish/clause on a NATIVE    get_attr/3, del_attr/2, attvar/1 and every other v4 native or prelude
or a prelude export           export, as they did before 4.1.0 deleted those built-ins' Java
                              classes. Consulting a module that DEFINES a library predicate still
                              overrides it — that rule is unchanged.
```

#### What is new in this release

- **Printing is 22-59 % faster.** `write/1` of a 2 000-element list is 59 % faster, `format/3` into
  an atom 37 %, `write/1`+`nl/0` of a small compound 22 %. The bridge used to `Unify.resolve` the
  whole goal — a complete copy of the term — before printing a single character.
- **Text and term inspection are ~50 % faster.** The `atom_*` loop -55 %, `atomic_list_concat` +
  `split_string` -41 %, `functor`/`arg`/`=..`/`succ` -53 %, `keysort/2` of 2 000 pairs -82 %.
- **Four enumerations became lazy generators**: `atom_concat(-,-,+)`, `string_concat(-,-,+)`,
  `arg(-,+,?)`, `current_predicate/1`, `nb_current/2` and `current_prolog_flag/2` produce one
  solution per redo instead of a pre-built list, so `once/1` over them stops at the first.
- **`b_setval/2` records its undo on the machine's own trail** instead of going through the
  `core.engine.v4.Undo` doorway.

#### Known limitation (LIM-037, re-scoped)

234 registered names still reach `LegacyBuiltinAdapter`: 191 of them are the extended libraries
(jdbc, filesystem, threading, crypto, ffi, graph, network, persistence, os, http, datetime, json,
logging, regex, dcg, csv, xml, clpfd), which call out to a database, a socket, the file system, a
process or a Java object and have no hot-path claim; the rest are 19 stream/parser `io` predicates
(`open`, `close`, `read`, `read_term`, `stream_property`, the byte I/O), `op/3`, `statistics/2`,
`char_type/2`, `code_type/2`, `table/1` and the 11 debug/profiler predicates.

---

## Release 4.1.0 - 2026-08-26

### Wave A of 4.1: one engine

The v2 `MachineSolver` — the default from 3.1.0 to 3.14.0, the selectable fallback of 4.0.0 — is
**deleted**, together with everything that existed only for it. ISS-2025-0491..0495. Wave record
and the starting point for wave B: `docs/reports/report-engine-v4-progress.md` section 16.

**1196/1196 JUnit tests** (one engine, one CI leg), **20/20 example programs** with unchanged
per-program counts. `src/main` is 2 825 lines and 5 files smaller.

#### Upgrading

Nothing to do unless you selected the old engine or called the selection API.

```
-Djprolog.engine=v2        GONE. Any value of the property now logs a warning and runs v4.
-Pengine-v2                GONE (the Maven profile). `mvn test` is the whole suite.
Prolog.setUseV4Engine(b)   GONE, with isUsingV4Engine(), setUseV2Engine(b), isUsingV2Engine().
                           There is one engine; nothing to select.
Variable.AttributeUnifyHook   GONE, with setAttributeUnifyHook/getAttributeUnifyHook. It was the
                           v2 engine's coroutining entry point inside Term.unify(Term, Map); v4
                           has its own wake queue (core.engine.v4.Coroutining).
core.engine.Trail          GONE. A built-in that records a backtrackable side effect calls
                           core.engine.v4.Undo.record(Runnable) instead — same contract, but the
                           action lands on the running machine's own trail.
EngineContext.handleAttributeUnification   GONE (it dispatched the hook above).
Prolog.clearSession()      kept as a no-op: there is no cross-query attributed-variable state.
TableStore                 keeps declareTable/isTabled/abolishTable/abolishAllTables/
                           getTabledPredicates; the answer cache, the in-progress set, the partial
                           cache, normalize() and enterCall/exitCall are gone (they were the v2
                           tabling driver's). The answer tables are core.engine.v4.Tabling.
thread_self(S)             now S == main on the top-level thread and S == w1 inside
                           thread_create(G, Id, [alias(w1)]); an anonymous worker still answers an
                           integer. A program that did arithmetic on the answer must stop.
```

Everything else is unchanged: `solve/1`, `solveStream/2`, `consult`, `consultWithDiagnostics`,
`compileFile`, `enableSafeMode`, `setInferenceBudget`, the `Map<String,Term>` result shape, the
trust model and the IDE debugger contract.

#### What is new in this release

- **One engine, one CI leg.** 6 classes and 2 616 lines of engine deleted, plus the second Maven
  profile and every engine-aware branch in the sources and the tests.
- **One trail.** The bridged built-ins' undo actions (`b_setval/2`, `op/3`, `setarg/3`, the CLP(FD)
  store) go on the machine's own `Bindings` trail through `core.engine.v4.Undo`; the parallel
  process-wide `Trail` and the second choice-point mark are gone.
- **A faster goal path.** The module override test is asked only when there is a registry entry to
  override, and memoised behind the module stamp: `nrev` -10 %, an indexed fact-lookup loop -7 %,
  `loop(1000000)` -9 % (medians, interleaved same-session runs).
- **An idle debugger is free.** `DebugController.needsPorts()`: a controller with no listener, no
  breakpoint, in CONTINUE mode and with no Stop pending gets no ports at all — on `loop(1000000)`
  an attached-but-idle controller cost 1.8-2.3x before and 0.96-1.13x now.
- **`thread_self/1` is SWI-shaped**, and the `main` alias follows a live thread instead of pointing
  at a dead one for the rest of the JVM's life.

#### Known limitations after this release

**LIM-037** (~310 of 416 registered predicates still run on the eager built-in bridge — none on a
measured hot path; the migration is 4.1 wave B) and **LIM-036** (`.jpc` source lines come from the
legacy parser's clause splitter). **LIM-027** closed with the engine that carried it.

---

## Release 4.0.0 - 2026-08-26

### Wave W9: retirement — the recursive engine is deleted

Ninth and last wave of the clean-room resolution core designed in
`docs/reports/report-engine-v4-design-2026-08-25.md` (section B.16 row W9, decision 1 of B.17),
ISS-2025-0484..0488. Handoff: `docs/reports/report-engine-v4-progress.md` section 15.

**1214/1214 JUnit tests on the default engine (v4) AND under `-Pengine-v2`**, **20/20 example
programs on both engines**. `src/main` is 1 409 lines and 9 files smaller.

#### Upgrading

Nothing to do unless you name the old engine or the old API.

```
-Djprolog.engine=legacy    GONE. The recursive QuerySolver is deleted; the property value no
                           longer selects anything (it is simply not "v2", so you get v4).
-Pengine-legacy            GONE (the Maven profile).
Prolog.solveLegacy(Q)      GONE. Use Prolog.solve(Q) — it runs the selected engine.
Prolog.getQuerySolver()    GONE. Use Prolog.getEngineContext(): same DebugController wiring
                           (setDebugController/getDebugController), same ResourceGuard accessors.
BuiltInWithContext         executeWithContext now takes a core.engine.SolverContext, not a
                           QuerySolver. A custom built-in that ran a sub-goal with
                           solver.solve(G, Bindings, Sols, CutStatus) calls
                           solver.solveMeta(G, Bindings, Sols) instead; CutStatus is gone.
-Djprolog.engine=v2        still selects the v2 MachineSolver, for THIS release only.
```

Everything else is unchanged: `solve/1`, `solveStream/2`, `consult`, `consultWithDiagnostics`,
`compileFile`, `enableSafeMode`, `setInferenceBudget`, the `Map<String,Term>` result shape, the
trust model and the IDE debugger contract.

#### What is new in this release

- **The recursive engine is gone.** With it: `CutStatus`, `MutableCutStatus`, `LayeredMap`,
  `CollectionBuiltInAdapter`, `BuiltInHelper` and the seven ISO control constructs that only it
  dispatched (`,/2`, `;/2`, `->/2`, `\+/1`, `call/N`, `catch/3`, `^/2` are native in both surviving
  machines). Their registry entries stay as placeholders, so `assertz(call(x))` still raises
  `permission_error(modify, static_procedure, call/1)`.
- **A worker can answer its creator.** `thread_send_message(main, Term)` and
  `thread_get_message(Term)` from the main thread now work, as in SWI: every thread that touches
  the message-queue predicates owns a queue, and the first one to do so claims the alias `main`.
- **Tabling is thread-safe.** An evaluation is claimed by one thread on both engines, so several
  workers of one query can call a tabled predicate: the first produces the table, the rest read it
  complete. Previously they interleaved and some workers saw a half-produced answer set. A
  producer that cannot finish within 60 s surfaces as `resource_error(tabling_busy)` instead of
  hanging.
- **Two bugs the manual found**: `stream_property/2` with an unbound stream used to **hang** on an
  interactive terminal (it peeked `user_input` to decide `end_of_stream`), and an answer printed a
  user-declared operator in canonical form (`Y = is_bigger(a,b)` instead of `Y = (a is_bigger b)`)
  because the answer is rendered after the engine's state has left the thread.
- **Seven more predicates are native on v4**: `sort/4`, `predsort/3`, `max_list/2`, `min_list/2`,
  `current_op/3` (lazy now — it used to build the whole operator list before the first solution),
  `nb_getval/2` and `b_getval/2`; and the CLP(FD) posting predicates `in/2`, `#=`, `#\=`, `#<`,
  `#>`, `#=<`, `#>=`, `all_different/1`, `all_distinct/1` bind determined variables through
  attributed cells. Semantics, including every ISO error term, are unchanged.

#### Known limitations after this wave

- Roughly 310 of the 416 registered predicates still run on the eager built-in bridge
  (`LegacyBuiltinAdapter`): the atom/string/character library, the I/O family, the
  assert/retract/listing family and the whole extended library (CSV, JSON, XML, HTTP, JDBC, …).
  None is on a measured hot path; migrating them is a 4.1 item (LIM-037, limit L-08).
- The v2 `MachineSolver` is still selectable and still has every gap the v4 default has closed
  (LIM-027 and the list in LIM-037). It is deleted in 4.1.

---

### Engine v4 is now the default — wave W8: default switch, threads, debugger

Eighth wave of the clean-room resolution core designed in
`docs/reports/report-engine-v4-design-2026-08-25.md` (sections B.6 and B.13, ISS-2025-0478..0483).
**Every query now runs on `core.engine.v4`.** Handoff:
`docs/reports/report-engine-v4-progress.md` section 14.

**1204/1204 JUnit tests on the default engine (v4) AND under `-Pengine-v2`** (1158 pre-existing +
46 new in `EngineV4ThreadsTest`, `EngineV4TraceTest` and `PrologCliBatchTest`), **20/20 example
programs on both engines.**

#### Upgrading

Nothing to do — but read this if you depend on the old engine's exact behaviour.

```
-Djprolog.engine=v2       the previous default (core.engine.v2.MachineSolver), kept for ONE release
Prolog.setUseV4Engine(false)   the same as =v2, at runtime
mvn test -Pengine-v2      the second CI leg (the former -Pengine-v4 is what `mvn test` now does)
```

What changes for a program that does nothing:

| | before (v2) | now (v4) |
|---|---|---|
| `X = f(X), Y = f(Y), X = Y` | hangs / `representation_error(cyclic_term)` | succeeds (rational trees; `set_prolog_flag(occurs_check, error)` restores the ISO error) |
| `setup_call_cleanup/3`, `call_cleanup/2` | `Cleanup` after the FIRST solution | after the LAST solution (ISO/SWI) |
| `append(X, Y, Z)` fully open | one eager answer | enumerates |
| `member(X, PartialList)` | stops at the open tail | extends it |
| a coroutine left by a finished query | can fire in the next one | cannot |
| tabling on a left-recursive chain | wrong answers | correct |
| `lists:append/3`, `meta_predicate/1`, yall lambdas, `partition/4`, `memberchk/2`, `frozen/2`, `unifiable/3`, `current_table/2`, `current_module/1` | `existence_error` or ignored | work |
| `when/2` woken goal's bindings | lost | propagate |
| memory of a long deterministic loop | O(bindings) — `loop(10000000)` runs out | O(1) — 64 MB |
| a user's own list predicate over 1 M elements | 7.2 s | 1.2 s |

#### What is new in this release

- **Threads really run their goals.** `thread_create/2` used to start a thread that slept 10 ms and
  recorded `completed(<goal>)` — the goal never ran, and it had to be an atom. A worker now runs on
  its own machine over the same engine: shared clause store (so `assertz`/`retract` from two threads
  are visible to both), shared flags and operators, its own current streams, its own inference
  budget counter carrying the parent's limit, and a copied goal so no variable is shared between
  threads. New `thread_create/3` (`alias/1`, `detached/1`); `thread_join/2` reports `true`,
  `false`, `exception(Ball)` or `cancelled`; `thread_self/1` reports the worker's own id; message
  queues carry **terms** rather than atoms; every thread owns a queue, so
  `thread_send_message/2` accepts a queue id, a thread id or an alias, and `thread_get_message/1`
  reads your own.
- **`concurrent_maplist/3` and `/4` are callable at all** — they were registered under names no
  Prolog goal could reach. The whole `concurrent_*` family, plus `first_solution/3`, now runs on
  per-thread machines, and interrupting the parent cancels the workers.
- **The debugger no longer changes how your program runs.** Attaching a debugger used to reroute
  `=/2`, `is/2`, the comparisons and the type checks through a slower bridge; they keep their fast
  paths now and the engine reports their ports itself. A merely running debugger costs nothing
  measurable (`nrev` went from 5.8x slower to 1.1x).
- **Tracing is usable on real programs.** It used to be quadratic: `loop(N)` under `trace/0` took
  1.9 s, 8.4 s and 33 s at N = 20 000, 50 000 and 100 000, and never finished at N = 1 000 000 with
  a gigabyte of heap. It is linear now — 0.48 s, 0.48 s, 0.74 s and **4.6 s** — and `nrev` went
  from 567x slower than untraced to ~10-18x. Two visible differences: the inline built-ins
  (`X = 1`, `Y is X+2`, `Y > 2`, `integer(Y)`, ...) now appear in the trace, and a goal that exited
  deterministically no longer prints a phantom `Fail` afterwards. The depth in parentheses is the
  real call depth now, so the trace nests properly; the indentation is capped at 40 levels.
- **A piped console no longer eats your queries.** With stdin redirected, the CLI printed the first
  solution, wrote ` ;` and read the NEXT QUERY as your answer. It now prints every solution at once
  (separated by ` ;`, terminated by `.`) whenever stdin is not a terminal, or when you pass
  `--batch` / `-q`. Interactive use is unchanged.

## Release 3.14.0 - 2026-08-26

### Engine v4 — wave W7: engine state (streams, operators, writer)

Seventh wave of the clean-room resolution core designed in
`docs/reports/report-engine-v4-design-2026-08-25.md` (sections B.11 and B.12, ISS-2025-0472..0477).
**Unlike waves W1-W6 this one is mostly engine-neutral**: the state it moves and the writer it
introduces are used by the default v2 engine and the legacy engine too, so most of it applies
whether or not you select `-Djprolog.engine=v4`. The default engine is still v2 — it becomes v4 in
wave W8. Handoff: `docs/reports/report-engine-v4-progress.md` section 13.

**1157/1157 JUnit tests on the default engine AND on v4** (1112 pre-existing + 46 new in
`EngineV4StreamsTest` and `EngineV4WriterTest`), **20/20 example programs on both engines,
byte-identical between the two engines.**

#### Heads-up: the console prints answers differently

Approved as design decision 5 (B.17). The top level now prints answers in quoted operator notation
with `_A`-style variable names, and shows the constraints an answer still carries:

```
?- X = 'a b'-1.            before:  X = -(a b, 1).        now:  X = 'a b'-1.
?- Body = (p,q).           before:  Body = ,(p, q).       now:  Body = (p,q).
?- PI = foo/1.             before:  PI = /(foo, 1).       now:  PI = foo/1.
?- A = '42'.               before:  A = 42.               now:  A = '42'.
?- X = f(Y).               before:  X = f(Y), Y = Y.      now:  X = f(Y).
?- dif(X, a).              before:  X = _G12.             now:  dif(X,a).          (v4)
?- X in 1..3.              before:  X = _G12.             now:  X in 1..3.         (v4)
```

Seven of the sixteen example programs that produce bindings print different text as a result; the
change is either a quoting/operator improvement or the removal of a spurious `Var = Var` line. If
you compare CLI output byte-for-byte in your own tests, re-baseline once.

#### What is new

- **Repositioning a text stream works.** `get_char(S,C1), seek(S,0,bof,_), get_char(S,C2)` used to
  answer `C2 = e` after `C1 = h`, because the seek moved the file channel while `get_char/2` kept
  reading a `PushbackReader`'s stale 8 KB buffer. A stream now decodes through its own buffer, which
  tracks byte position, character count, line number and line position, and a reposition flushes it.
  `read/1,2`, `read_term/2,3`, `get_char/2` and `seek/4` finally agree about where the stream is.
- **`stream_property/2` is complete** — `file_name`, `mode`, `input`/`output`, `alias`, `position`,
  `end_of_stream`, `eof_action`, `reposition`, `type`, `encoding`, `line_count` — and it accepts an
  alias as the stream argument (it was simply false for every stream opened with `alias(A)`).
- **New I/O predicates**: `set_stream/2`, `stream_position_data/3`, `character_count/2`,
  `line_count/2`, `line_position/2`, `current_stream/3`; `read_term/2,3` gained
  `term_position(Pos)`.
- **`current_op/3` sees the operators a consulted file declared.** A `:- op(700, xfx, ===).`
  directive used to work in source and stay invisible to the program. There is one operator store
  now, read by the parser, `op/3`, `current_op/3`, the writer, the `.jpc` writer and the IDE
  formatter; an `op/3` inside a module file is local to that module.
- **`write_term/2,3` implements the whole ISO option set**: `quoted`, `ignore_ops`, `numbervars`,
  `max_depth`, `portray`, `cycles`, `variable_names`, `spacing(next_argument)`. `print/1,2` gained
  the `portray/1` hook it used to document as unsupported, and `portray_clause/1,2` and
  `print_message/2` are new.
- **Printing terminates and scales.** The writer is iterative — a 1 000 000-element list and a
  200 000-deep structure print at the default JVM stack — and cycle-safe: a rational tree prints as
  `f(...)`, or as `@(_S1,[_S1=f(_S1)])` under `cycles(true)`, instead of looping.
- **Two engines in one JVM are properly isolated.** Streams and their aliases, operators, spy points
  and profiler counters belong to the `Prolog` instance (LIM-034 closed), and
  `current_input`/`current_output` are per thread within an engine — `set_output/1` on one thread no
  longer redirects another's.
- **Output capture no longer touches `System.out`.** `with_output_to/2` and `format/3` with
  `atom/string/codes/chars` capture through a per-thread override, so concurrent captures are safe
  (LIM-025 closed).
- **Deeply nested input raises a resource error** — `error(resource_error(parser_nesting), _)` past
  1000 levels — instead of blowing the Java stack or, inside `term_to_atom/2`, failing silently.

#### Upgrading

- `open/3,4` unifies `Stream` with `'$stream'(N)` instead of an atom. Every stream argument still
  accepts an atom alias, the `stream_<id>` handle and the reserved names, so ordinary
  `open(F, read, S), ..., close(S)` code is unaffected; code that compared `S` to an atom is not.
- `OperatorDefinition.setSharedOperatorTable(...)` is a deprecated no-op — the table belongs to the
  `Prolog` instance (`getOperatorTable()`).
- Java code that read spy points or profiler counters through the `Spy` / `Profiler` statics while
  no engine was current on the thread now sees a process-wide default store, not the engine's;
  use `Prolog.getEngineState()`.

---

## Release 3.13.0 - 2026-08-26

### Engine v4 (opt-in) — wave W6: modules and the Prolog prelude

Sixth wave of the clean-room resolution core designed in
`docs/reports/report-engine-v4-design-2026-08-25.md` (section B.10, ISS-2025-0466..0471). Select it
with `-Djprolog.engine=v4` or `Prolog.setUseV4Engine(true)`. **The default engine is unchanged** —
v4 becomes the default in wave W8. Handoff: `docs/reports/report-engine-v4-progress.md` section 12.

**1112/1112 JUnit tests on the default engine AND on v4** (1083 pre-existing + 29 new in
`EngineV4ModulesTest`), **20/20 example programs on both engines, byte-identical output.**

What wave W6 changes, for a user:

- **A module system that actually resolves.** Every predicate belongs to a module. `system` holds
  the built-ins, **`user` is the ordinary flat knowledge base** (declaring a second module no
  longer changes how anything in `user` is found — before this wave it did, silently), and the
  library modules `lists`, `apply`, `pairs` and `coroutining` are Prolog files shipped inside the
  jar. An unqualified call from module `M` resolves `M` -> `M`'s imports -> `user` -> the
  autoloaded libraries -> the built-ins.
- **`Module:Goal` works for built-ins and libraries.** `lists:append([1],[2],L)` was *false* on
  every JProlog engine before today; `system:atom_length(abc, N)` and `user:foo(X)` work too, and
  `a:b:Goal` runs in the innermost module. Export enforcement is unchanged: a module that defines
  a predicate answers a qualified call only if it exports it.
- **`meta_predicate/1` is honoured.** A library predicate that does `call(G, X)` now runs `G` in
  the module that called the library, so two modules with a helper of the same name each get their
  own — `maplist/3` called from `m1` and from `m2` no longer collide.
- **Libraries load when they are first used**, not at engine construction: `new Prolog()` plus a
  first query costs **1.4 ms** instead of 8.0 ms.
- **`append/3` and `member/2` are complete relations.** `append(X, Y, Z)` with all three arguments
  open now enumerates lazily (so `append(X, Y, Z), length(X, 2)` terminates) instead of stopping
  at the single standard solution, `member(X, L)` extends an unbound `L`, and `memberchk(a, L)`
  binds `L = [a|_]`. **This is a deliberate divergence from the default engine**, where those
  goals still terminate after one answer: a program that relied on the failure now loops.
- **New on v4**: `current_module/1`, and `predicate_property(Head, defined_in(M))` /
  `exported` / `imported_from(M)`.
- **CLP(FD) labeling no longer goes through variable names.** `label/1` and `labeling/2` are
  cell-based v4 generators; functionally determined variables (`C in 1..3, D #= C*2+1, label([C])`
  reports `D`) are still bound, and the `labeling/2` options behave exactly as before.
- **Performance**: on a 1 000 000-element list v4 now matches or beats the default engine on every
  list operation measured (`length/2` 693 ms vs 1084, `append/3` 881 vs 1171, `msort/2` 1325 vs
  2362, `copy_term/2` 1246 vs 1842); a *user-written* recursive list predicate is 6x faster on v4
  than on v2 (1162 ms vs 7205 ms for a 1M-element `myappend/3`).

Nothing in the default engine changed: `ModuleManager` gained a modification stamp, `Module` a
getter and `ClpfdV2Bridge` one method, all additive.

---

## Release 3.12.0 - 2026-08-26

### Engine v4 (opt-in) — wave W5: tabling

Fifth wave of the clean-room resolution core designed in
`docs/reports/report-engine-v4-design-2026-08-25.md` (section B.8, ISS-2025-0463..0465). Select it
with `-Djprolog.engine=v4` or `Prolog.setUseV4Engine(true)`. **The default engine is unchanged** —
v4 becomes the default in wave W8. Handoff: `docs/reports/report-engine-v4-progress.md` section 11.

**1083/1083 JUnit tests on the default engine AND on v4** (1065 pre-existing + 18 new in
`EngineV4TablingTest`), **20/20 example programs on both engines, byte-identical output.**

What wave W5 changes, for a user:

- **Tabling answers correctly.** The default engine's tabling is a bounded re-evaluation loop
  (100 iterations, eager, name-keyed) and returns *wrong answers* for a left-recursive predicate:
  on the classic 3 000-edge chain, `path(1, 3001)` and `path(1, 51)` both **fail**. On v4 they
  succeed, together with `findall(Y, path(1,Y), L), length(L, 3000)`. The same holds for the
  right-recursive and the doubly recursive (`path(X,Y) :- path(X,Z), path(Z,Y)`) formulations, for
  mutual recursion across two tabled predicates, and for cyclic graphs. The design's benchmark —
  a **100 000-edge chain, `path(1, 100001)`** — answers in **868 ms** (target: correct in <= 2 s);
  on v2 it fails in 40 ms.
- **Tabled evaluation is inside the engine, not beside it.** A tabled call is a generator/consumer
  choice point on the machine, so there is no per-subgoal Java recursion (the old driver inherited
  the recursive solver's 2 000-deep cap), the four-port trace and the IDE debugger see a tabled
  call like any other predicate, and the **inference budget and the Stop button reach inside the
  fixpoint** — a runaway tabled query is now abortable.
- **Robust under failure.** An evaluation abandoned by an exception, a cut or the resource guard
  discards its half-built tables, so the next call recomputes instead of reading a partial answer
  set; no table can be left half-finished between queries.
- **The tabling built-ins observe the real store.** `abolish_all_tables/0` and `abolish_table/1`
  clear the v4 answer tables (and the legacy declarations, as before); `abolish_table/1` now
  raises `instantiation_error` / `type_error(predicate_indicator, T)` on a malformed argument
  instead of failing silently, and both raise `permission_error(modify, table, ...)` if called
  from inside a running tabled evaluation. New **`current_table(?Variant, ?Status)`** (v4 only).
- **Invalidation policy** (documented in `docs/references/BUILTIN_PREDICATES_REFERENCE.md`):
  asserting to or retracting from a tabled predicate drops that predicate's tables. A change to a
  *non-tabled* predicate that a tabled one depends on is not tracked — call
  `abolish_all_tables/0`, as in XSB. Tables persist across queries; two safety caps
  (100 000 tables, 4 000 000 answers) drop the oldest completed tables at a query boundary.
- **`tnot/1` is not implemented** and raises `existence_error(procedure, tnot/1)` rather than
  behaving like `\+/1`.

Known cost, unchanged by any tabling system: the doubly recursive definition
`path(X,Y) :- path(X,Z), path(Z,Y)` builds one table per node and joins O(n) answers with O(n)
answers per table, so its full closure is cubic in the chain length (10.4 s at 400 edges on this
VM). Use the left- or right-recursive formulation for long chains.

## Release 3.11.0 - 2026-08-25

### Engine v4 (opt-in) — wave W4: coroutining and attributed variables

Fourth wave of the clean-room resolution core designed in
`docs/reports/report-engine-v4-design-2026-08-25.md` (section B.9, ISS-2025-0457..0462). Select it
with `-Djprolog.engine=v4` or `Prolog.setUseV4Engine(true)`. **The default engine is unchanged** —
v4 becomes the default in wave W8. Handoff: `docs/reports/report-engine-v4-progress.md` section 10.

**1065/1065 JUnit tests on the default engine AND on v4** (1043 pre-existing + 22 new in
`EngineV4CoroutiningTest`), **20/20 example programs on both engines, byte-identical output.**

What wave W4 changes, for an embedder:

- **A woken goal is a real goal.** Binding an attributed variable pushes its suspended goals onto
  the machine's wake queue, and the drive loop runs them before the next goal in the *current*
  binding context. So the bindings a woken goal makes propagate (`when(nonvar(X), Y = done),
  X = 1, Y == done` now succeeds — ISS-2025-0336, which still stands on the default v2 engine), it
  is traced through the four ports, the inference budget and the Stop button can abort it, and an
  exception it throws reaches the enclosing `catch/3`.
- **`freeze/2`, `frozen/2`, `when/2`, `dif/2` and `?=/2` are Prolog**, in
  `prelude/coroutining.pl`, on top of `put_attr/3`, `get_attr/3` and `attr_unify_hook`. A program
  that defines its own `freeze/2` therefore replaces the library one, as with `maplist/3` in W3.
  `when/2` now validates its condition (`instantiation_error`,
  `domain_error(when_condition, C)`) and fires exactly once for a disjunctive condition;
  `dif/2` re-suspends on the remaining unifier variables, so it decides
  `dif(f(X), f(Y)), X = 1, Y = 1` correctly.
- **The SWI attributed-variable protocol is available**: `put_attr/3`, `get_attr/3`, `del_attr/2`,
  `attvar/1`, `term_attvars/2`, `copy_term/3` and `unifiable/3`, with a user-definable
  `Module:attr_unify_hook(AttValue, Other)` called through the normal goal stack. Write your own
  constraint library in Prolog and it participates in unification.
- **CLP(FD) variables are ordinary attributed cells.** The v2 CLP(FD) bridge is a client of the
  same hook, which let the last compatibility shim of waves W1-W3 (the engine-wide name->cell
  index in `Machine`) be deleted. No behaviour change: `C in 1..3, D #= C*2+1, label([C])` still
  reports `D`, and `X in 1..3, X = 5` still fails.
- **Cross-query coroutining is gone on v4** (approved design decision 3). A query's variables die
  with the query, so a suspension left behind by a finished query can never fire in a later one:
  `when(nonvar(X), throw(leak))` followed by `X = 1` succeeds silently on v4 and throws `leak` on
  v2. If you relied on the v2.9.4 session behaviour, stay on the default engine — nothing in the
  IDE or the CLI used it.
- **New API: `Prolog.residualGoals(solution)`** — the constraints still attached to an answer
  (`freeze/2`, `when/2`, `dif/2`, CLP(FD) `in/2`, `put_attr/3`). Call it right after the `solve`
  that produced the answer. The CLI and the IDE start printing residual goals in wave W7.

---

## Release 3.10.0 - 2026-08-25

### Engine v4 (opt-in) — wave W3: native library and meta-calls

Third wave of the clean-room resolution core designed in
`docs/reports/report-engine-v4-design-2026-08-25.md` (ISS-2025-0450..0456). Select it with
`-Djprolog.engine=v4` or `Prolog.setUseV4Engine(true)`. **The default engine is unchanged** — v4
becomes the default in wave W8. Handoff: `docs/reports/report-engine-v4-progress.md` section 9.

**1043/1043 JUnit tests on the default engine AND on v4** (1024 pre-existing + 19 new in
`EngineV4LibraryTest`), **20/20 example programs on both engines, byte-identical output.**

What wave W3 changes, for an embedder:

- **No built-in on v4 falls back to the recursive solver any more.** The four-argument
  `SolverFacade.solve(...)` — the entry point `phrase/2,3`, the DCG helpers, `format ~p`/`~@` and
  the persistence transactions call — now runs on the machine. The 2 000-deep Java recursion cap
  those predicates inherited is gone (the only remaining exception is a sub-solve submitted to a
  worker thread by `concurrent/3` and friends, which W8 moves onto its own machine).
- **DCG parsing scales.** `numlist(1, 1000000, L), phrase(digits(D), L)` parses a **million tokens
  in ~2.9 s at the default JVM stack**; on v2 the same query raises
  `resource_error(stack_overflow)` even with `-Xss4m`. The inference budget and the Stop button now
  work *inside* a parse.
- **The 1 M-element list operations are now faster on v4 than on v2**, where waves W1/W2 left them
  1.3-2x slower. Same session, best-of-3, v2 -> v4: `length` 1466 -> 1001 ms, `msort`
  1257 -> 1064 ms, `copy_term` 1571 -> 1273 ms, `==` 709 -> 405 ms, `findall+member`
  2341 -> 1499 ms, `sum_list` 734 -> 318 ms, `reverse` 1147 -> 692 ms, `append` 2900 -> 1208 ms.
  The cause was never the core: it was the bridge dereferencing the goal and indexing its cells by
  name on every call. Those predicates are native now.
- **`member/2`, `append/3`, `select/3`, `nth0/3`, `nth1/3`, `clause/2`, `sub_atom/5`,
  `sub_string/5`, `bagof/3` and `setof/3` are lazy**: one solution per redo, O(1) memory, and the
  enumeration stops the moment the caller cuts. `once(member(X, MillionElementList))` no longer
  builds a million solution maps first.
- **`library(yall)` lambdas work**: `maplist([X,Y]>>(Y is X*2), [1,2,3], L)`,
  `foldl([X,A0,A]>>(A is A0+X), L, 0, S)`, and `N/[X,Y]>>Body` to share a free variable. On v2 these
  raise `existence_error(procedure, >>/4)`.
- **`maplist/2..7`, `foldl/4..7`, `include/3`, `exclude/3`, `partition/4,5` are Prolog clauses**
  loaded from `prelude/apply.pl`. They are linear, traceable and cancellable, and **a program that
  defines its own `partition/4` or `maplist/3` overrides them** — the library is consulted only
  when the knowledge base has no clause for that indicator. `maplist(dbl, L, L2)` over 200 000
  elements: 1634 ms on v2, **729 ms** on v4. `partition/4` is available again on v4.
- **`format/2,3` gains `~@`** (on both engines): `format("~@", [Goal])` runs Goal and inserts its
  output.
- **`with_output_to/2` supports `atom/1`, `string/1`, `codes/1` and `chars/1`** on v4 and captures
  through the thread-local stream, restoring whatever the IDE had installed.
- **Two correctness fixes**: `subsumes_term(f(X), f(Y))` answered *false* on v4 (ISS-2025-0456);
  and `sub_atom(Atom, B, L, A, '')` — which spins forever on v2 until the heap dies — terminates
  on v4.

Budget and cancellation coverage over seven long-running goal shapes, measured in one session:
the inference budget aborts **7/7** on v4 (4/7 on v2) and a thread interrupt cancels **4/4** on v4
(2/4 on v2).

---

## Release 3.9.0 - 2026-08-25

### Engine v4 (opt-in) — waves W1 (foundations) and W2 (clause store)

First two waves of the clean-room resolution core designed in
`docs/reports/report-engine-v4-design-2026-08-25.md` (ISS-2025-0438..0447). Select it with
`-Djprolog.engine=v4` or `Prolog.setUseV4Engine(true)`. **The default engine is unchanged** — v4
becomes the default in wave W8. Handoff and file map:
`docs/reports/report-engine-v4-progress.md`.

**1024/1024 JUnit tests on the default engine AND on v4** (989 pre-existing + 35 new),
**20/20 example programs on both engines, byte-identical output.**

What v4 changes, for an embedder:

- **Long deterministic computations no longer grow the heap.** Bindings live in the variable cell
  instead of a name-keyed map that never reclaims anything, so the JVM collects them as soon as the
  call that made them is finished. `loop(10000000)` runs in a **64 MB heap** (v2: OutOfMemoryError),
  and `loop2(1000000)` — one library built-in per iteration — takes **1.6 s** where v2 also OOMs.
  This is LIM-033, resolved on v4.
- **Six times faster on `nrev`**: ~2 008 KLIPS against ~316 KLIPS for v2 in the same session,
  because a clause is compiled once into a skeleton and activation no longer copies it or invents
  variable names.
- **Cyclic terms work instead of hanging.** `X = f(X), Y = f(Y), X = Y` succeeds in milliseconds;
  on v2 it hangs and cannot be interrupted or stopped by the inference budget — a denial of service
  reachable from untrusted code. `cyclic_term/1` and `acyclic_term/1` are real tests, and
  `set_prolog_flag(occurs_check, error)` (now an accepted flag value, as ISO requires) restores the
  ISO error behaviour. This is LIM-032, resolved on v4.
- **The database scales.** 100 000 interleaved `assertz` + call in **1.0 s** (v2: 7.3 s): clauses
  carry birth/death generations, so `assertz` appends, `retract` marks, and no per-write snapshot is
  rebuilt. The first-argument index is incremental and needs no bucket cap. A 20 000-clause lookup
  with the first argument bound costs 118-259 ns.
- **`setup_call_cleanup/3` and `call_cleanup/2` are real cleanup frames**: `Cleanup` runs exactly
  once — on deterministic exit, on failure, when the frame is cut away, or when an exception
  unwinds past it, always before the ball propagates.
- **`OutOfMemoryError` and a built-in's `StackOverflowError` become catchable ISO
  `resource_error/1` terms** *before* the query unwinds, so the running program's `catch/3` can see
  them.
- Everything else is unchanged: the same `Map<String,Term>` results, the same ISO errors, the same
  four-port trace (byte-identical output), the same IDE debugger contract, the same
  `enableSafeMode()` and `setInferenceBudget()` behaviour and the same trust model
  (`InferenceLimitException` / `QueryCancelledException` / `DebugStopException` are never
  catchable from Prolog).

Two deliberate behaviour differences on v4 (approved in design B.17): rational trees are supported,
and `setup_call_cleanup/3` runs `Cleanup` after the goal's LAST solution rather than eagerly after
the first. Both are called out above and pinned by engine-aware tests.

Two v4-only regressions found by the independent verification of these waves were fixed before the
release (both were correct on the default engine):

- **`findall/3` is opaque again** (ISS-2025-0448). The template variable kept the binding of the
  last solution, so `findall(X, member(X,[1,2]), L), var(X)` failed. The nested drive undid its
  bindings one step too late — after closing the forced-trail extent, by which point the trail had
  already been reclaimed. Every construct with the same shape was corrected as well: `\=/2`, a
  non-matching `catch/3` catcher, `subsumes_term/2`, and every meta-call sub-query
  (`bagof`, `setof`, `aggregate_all`, …).
- **Retract/assert loops are linear again** (ISS-2025-0449). Dead clauses waited for the query to
  end before being reclaimed, so a `retract`+`assertz` counter loop was quadratic:
  `cnt(100000)` took ~60 s. Compaction now happens as soon as the dead clauses outnumber the live
  ones — **0.56 s**, against 1.39 s on the default engine.

**1024/1024 JUnit tests on both engines** after these fixes.

Also in this release, on **every** engine:

- `.jpc` format 0x03: clause variables are serialised by index (a clause read back from a compiled
  file used to get one distinct variable object per occurrence of the same name) and each clause
  records its source line, so IDE line breakpoints work on compiled sources.
- `Variable.copy()` of a named variable returns the variable itself. Ten built-ins (`arg/3`,
  `member/2`, `nth0/nth1`, `select/3`, `aggregate_all/3`, …) copy a term and then unify with the
  copy, relying on the copy aliasing the original; the old "same name, different object" copy
  happened to satisfy that only because bindings were name-keyed.
- `set_prolog_flag(occurs_check, error)` is accepted (ISO 7.11.2.4 defines three values).
- `compare/3` validates its `Order` argument on v4 (`domain_error(order, _)` /
  `type_error(atom, _)`).
- New Maven profiles: `mvn test -Pengine-v4` and `mvn test -Pengine-legacy`.

## Release 3.8.0 - 2026-08-25

### Engine deep analysis — wave 5: clause selection, arithmetic and housekeeping

Fifth wave over `docs/reports/report-engine-deep-analysis-2026-08-24.md` (ISS-2025-0433..0436).
**983/983 JUnit tests, 20/20 example programs.** Timings are best-of-3 in a warmed JVM, against
the v3.7.0 build on the same machine.

- **Large fact tables are ~1200x faster to query** (ENG-13). A lookup in a 20 000-fact table went
  from **2.358 ms to 0.002 ms per call**. Each predicate now keeps a versioned immutable clause
  snapshot (no per-call copy, no lock), the choice point pulls one clause per redo instead of
  pre-building every alternative, only the clause *head* is renamed before unification (a ground
  fact is not renamed at all), and **first-argument indexing is back on the default engine** with a
  type-faithful key — so `1` and `1.0` no longer share an index bucket.
- **Arithmetic is ~20% faster** (ENG-14): `is/2` no longer deep-copies its expression, `+`/`-`/`*`
  take a primitive `long` path (exact `BigInteger` on overflow), comparisons compare primitives,
  and small integers come from a cache. `nrev` throughput **266 → 324 KLIPS**.
- **Housekeeping** (ENG-15/ENG-17): `collectVars` no longer O(n²); the Stop interrupt is polled
  every 1024 steps instead of every step; the dead `CompiledClause` and `Interpreter` classes and
  the never-read multi-argument clause index are gone (the latter was maintained on every
  assert/retract).

No user-visible semantic changes: clause order, the logical update view, ISO error terms and
arithmetic results (including exactness beyond 64 bits) are all unchanged and pinned by tests.

### Engine deep analysis — wave 6: per-engine state isolation

**989/989 JUnit tests, 20/20 example programs.**

- **One engine can no longer change another engine's behaviour** (ENG-06). ISO flags
  (`unknown`, `double_quotes`, …), `occurs_check` and `trace/0` used to live in process-global
  statics, so `set_prolog_flag/2` or `trace.` in a sandboxed engine silently reconfigured every
  other `Prolog` instance in the JVM. Each engine now owns its flag store; new
  `Prolog.getFlags()`, `Prolog.setTracing(boolean)` and `Prolog.isTracing()`.
- `current_input` / `current_output` are per thread, like the output capture the IDE already used:
  `set_output/1` on one thread no longer redirects output for all of them.
- CLP(FD) temporary variable names are generated atomically (two threads could previously get the
  same name and alias unrelated constraint variables).

**Behaviour change for embedders**: `it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled()`
and `Variable.isOccursCheckEnabled()` now report the engine *current on the calling thread*. Code
that toggles tracing from outside a query (an IDE button, a REPL command) must call
`Prolog.setTracing(boolean)` on its own engine. Still process-global: the operator table, spy
points and the profiler counters — see **LIM-034**.

## Release 3.7.0 - 2026-08-25

### Engine deep analysis — wave 3: the machine's memory model

Third wave over `docs/reports/report-engine-deep-analysis-2026-08-24.md` (ISS-2025-0429/0430).
**966/966 JUnit tests, 20/20 example programs.**

- **Deterministic recursion no longer leaks choice points or trail** (ENG-10). An exhausted choice
  point is dropped as soon as its last alternative is taken, and the trail is reclaimed whenever
  nothing can undo it. `loop(20000)` now ends with an empty choice-point stack and an empty trail.
  `loop(1000000)` fits in 256 MB (was an out-of-memory error) and is 1.5x faster at 2 GB;
  `loop(3000000)` completes for the first time.
- **Calling a built-in is no longer quadratic** (ENG-11). Built-ins receive a *resolved* goal and
  an empty binding map instead of a full copy of every binding in the query, `resolve` shares
  unchanged sub-terms, and a deterministic built-in gets no choice point at all. A loop with one
  `atom_length/2` per iteration went from *out of memory after 27 s at 2 GB* (N = 10 000) to
  **157 ms**, and 200 000 iterations now run in 639 ms inside a 128 MB heap.
  `nrev` throughput: **188 → 350 KLIPS**.
- `setarg/3` / `nb_setarg/3` keep the old identity-preserving call path.

### Engine deep analysis — wave 4: meta-calls and generators on the machine

**974/974 JUnit tests, 20/20 example programs.**

- **Security fix (ENG-04)**: the inference budget and the Stop button were bypassed by every
  meta-call. `once(Loop)`, `ignore(Loop)`, `aggregate_all(count, Loop, C)` and
  `setup_call_cleanup(true, Loop, true)` used to run forever with a budget set, and
  `forall/2` did not charge a single step. A shared `ResourceGuard` now covers both engines, 122
  broad `catch` clauses in the library no longer swallow (or re-wrap into a catchable
  `PrologException`) a budget/Stop abort, and the meta-calls run on the fast machine instead of
  the recursive legacy solver. Every escape now aborts in tens of milliseconds, uncatchable by
  `catch/3`. The budget and Stop now also apply under `-Djprolog.engine=legacy`.
- **Meta-calls got much faster**: `once(loop(100000))` 438 ms (it previously took 1.78 s for only
  5 000 iterations); `maplist(integer, L)` over 200 000 elements went from out-of-memory to
  ~0.5 s — **LIM-030** is largely resolved.
- **`between/3` is lazy (ENG-12)**: `between(1,2000000,X), X >= 2000000` went from exhausting a
  256 MB heap to **368 ms in 64 MB**, and `between(1, inf, X)` is no longer capped at a million
  solutions.

Known limitation added: **LIM-033** — query memory is still O(number of bindings) because the
binding store is name-keyed and never reclaims dead variables (ENG-16, architecture track).

## Release 3.6.2 - 2026-08-25

### Engine deep analysis — wave 2: the deep-structure limit

Second wave over `docs/reports/report-engine-deep-analysis-2026-08-24.md` (ISS-2025-0428).
**961/961 JUnit tests, 20/20 example programs.**

- **Long lists and deep terms just work.** Every term walker is now tail-iterative on the last
  argument, so a **1 000 000-element** list survives `length/2`, `sum_list/2`, `==/2`,
  `copy_term/2`, `msort/2`, `sort/2`, `ground/1`, `assertz/1`, `reverse/2`, `append/3`,
  `term_to_atom/2` and `write/1` at the **default** JVM stack size. Before, the same operations
  raised `resource_error(stack_overflow)` at roughly 20 000–30 000 elements even with `-Xss4m`,
  and a 50 000-deep `f(f(…))` failed outright (200 000 now works).
- Side effects of the rewrite: compound unification snapshots the bindings map once per top-level
  call instead of once per nesting level, and printing a long list no longer allocates one string
  per element.
- Still memory-bound (later waves): `maplist/2..5` over a million elements (LIM-030) and
  `member/2` inside `findall/3`.

## Release 3.6.1 - 2026-08-24

### Engine deep analysis — wave 1: correctness quick wins

First wave over `docs/reports/report-engine-deep-analysis-2026-08-24.md` (ISS-2025-0423..0427).
**956/956 JUnit tests, 20/20 example programs.**

- **`repeat/0` is infinite again** (ENG-01): it used to stop after exactly 1000 redos, so
  `repeat, …, Done, !` driver loops silently failed. Native lazy choice point, O(1) memory.
- **Floats stay floats** (ENG-02): `sum_list([1.5,1.5], S)` is `3.0`, not `3`; JSON/CSV/SQL
  decimals keep their type. 58 audited call sites; integer-valued built-ins (codes, indices,
  lengths, arities, precedences, counters) explicitly return ISO integers.
- **`length/2` enumerates** (ENG-03): `length(L, N), N >= 3, !` and `length([a|T], N)` work.
- **Built-in crashes are no longer swallowed** (ENG-05): a Java fault in a built-in surfaces as
  `system_error` instead of `existence_error`/silent failure, and budget/cancel/debug-stop
  exceptions can no longer be lost inside the bridge.
- **Minor** (ENG-08): unknown-procedure warnings reach the IDE console; `throw/1` copies the ball
  once; `PrologException` no longer captures a Java stack trace (exceptions are control flow).

Behavior changes: integral float results now print as `3.0` and fail `integer/1`; `length(L, N)`
with both arguments unbound is a generator (SWI semantics), not a failure; buggy built-ins report
`system_error`. New limitations recorded: LIM-031 (legacy `repeat` bound), LIM-032 (cyclic terms).

## Release 3.6.0 - 2026-06-10

### Audit wave 3 — remaining 27 confirmed findings fixed

Closes the ISS-2025-0395 open-findings roll-up (ISS-2025-0396..0422).
**935/935 JUnit tests, 20/20 example programs.**

- **retract/1 re-executable** on the default engine (ISO 8.9.3) — resolves LIM-026; `phrase/3`
  spurious cyclic_term fixed; `V^Goal` callable as a goal.
- **read/1,2 reads to the end token** (multi-line terms, several terms per line, comments) —
  resolves LIM-029; format/2,3 strict on argument mismatches; non-callable DCG heads diagnosed.
- **Text conversions**: type-faithful floats, ISO 0x/0o/0b/0'c, ISO error terms across
  atom_*/char_code/number_*; term_to_atom on non-ground terms; string/1; atom/string interop;
  float_integer_part beyond 2^63.
- **All-solutions polish**: setof standard-order witness groups, variant-witness merging,
  exact aggregate_all sums/extrema, callable validation (once/ignore/forall/findall),
  compare/sort(4)/predsort/arg/=.. ISO errors.
- **CLP(FD)**: non-linear products propagate (X*X #= 16); labeling/2 options honored.

Behavior changes: stricter errors replace silent leniency; `atom_number(A, 123.0)` → `'123.0'`;
retract backtracks.

## Release 3.5.0 - 2026-06-10

### ISO-conformance sweep — 53 confirmed defects fixed

A multi-agent empirical audit (13 domain finders running ISO-conformance queries, adversarial
verification of every finding: 98 confirmed of 111 unique) followed by two fix waves.
**866/866 JUnit tests, 20/20 example programs.**

Highlights (full list: CHANGELOG.md [3.5.0], ISS-2025-0342..0394 in track-issues.md):
- **Engine soundness**: local cut in `(->)/2`/`\+/1` conditions; catch/3 frame lifecycle;
  `existence_error` for undefined procedures per the `unknown` flag; working `halt`;
  `throw(unbound)` → `instantiation_error`; KnowledgeBase retract/index desync fixed.
- **CLP(FD) soundness**: unification respects domains; posts undone on backtracking; singleton
  domains bind; multi-variable `#\=`.
- **Strings**: `"abc" == "abc"` finally true; consistent standard order; `atomic/1`.
- **Sorting/lists**: keysort/sort/msort ISO modes+errors; proper lists with var elements accepted;
  maplist re-satisfiable; bagof/setof fresh copies; inverse modes for reverse/select/permutation.
- **Arithmetic**: float overflow/NaN → ISO evaluation_errors; big-integer text conversions exact.
- **I/O**: format/read_term/write_term succeed as goals; stream-argument forms (`write/2`, `nl/1`,
  `put_char/2`, `get_code/2`, `peek_char/2`, ...); working `set_input/set_output`; ISO stream
  errors; `print/1,2`.
- **Writer/DCG**: writeq round-trips (token merging, quoting, numbervars, float syntax);
  phrase/2,3 full body translation; DCG push-back/terminal validation.

Behavior changes: undefined procedures throw by default (set `unknown` flag to `fail` for old
behavior); `halt/1` exits the CLI; stricter ISO errors replace silent failures in several
built-ins. New limitations: LIM-026..LIM-030. Open findings: ISS-2025-0395 roll-up.

## Release 3.4.0 - 2026-06-09

### Production hardening (sandbox, resource budget, robustness, correctness)

From a multi-agent production-readiness audit
(`docs/reports/report-production-readiness-audit-2026-06-09.md`). **705/705 JUnit, 20/20 examples.**

- **Sandbox** `Prolog.enableSafeMode()` — removes host-touching built-ins (os/ffi/filesystem/network/
  http/jdbc/persistence) so untrusted programs cannot exec/reflect/read/write/connect.
- **Inference budget** `Prolog.setInferenceBudget(steps)` — uncatchable `InferenceLimitException`
  bounds CPU on runaway queries.
- **No crash on deep structures** — deep terms and deeply nested input raise a catchable
  `resource_error` instead of a raw `StackOverflowError`.
- **Correctness**: `sort`/`msort`/`sort_4` on variables; `freeze/2` binding propagation; ISO errors for
  `call/1` and `=../2`.
- First-arg indexing in v2 was attempted and **reverted** (KB index not reliably populated).

New APIs: `Prolog.enableSafeMode()/isSafeMode()`, `Prolog.setInferenceBudget(long)`,
`InferenceLimitException`.

### Repository Information
- **Tag**: v3.4.0
- **Release Date**: 2026-06-09
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 3.3.0 - 2026-06-09

### Call tracing & full debugging on the v2 engine

- **`trace/0`/`notrace/0`** now emit a real four-port trace (Call/Exit/Fail/Redo) through the default
  v2 engine — visible in the CLI (`:trace [on|off]`) and the IDE (Trace toggle).
- The **IDE debugger runs on the v2 engine** (four-port `DebugController` events fired by
  `MachineSolver`); breakpoints/stepping/Stop work on the default engine.
- **Built-ins are debuggable** (`is`, `=`, comparisons, type-checks and the 200+ registry built-ins).
- **Conditional & hit-count breakpoints** (condition goal + ignore count) — in the engine and the IDE
  Add-Breakpoint dialog.
- Fixed the legacy LCO trampoline bypassing debug ports for the last body goal.
- **Repository fix**: core `Debug*.java` source files were git-ignored (unanchored `Debug*.java`
  pattern) and missing from the published repo; anchored the temp patterns and committed the sources.

New APIs: `MachineSolver` debug hooks, `DebugController.addBreakpoint(indicator, ports, condition,
ignoreCount)` + `setConditionEvaluator`. Baseline: **690/690 JUnit, 20/20 examples.**

### Repository Information
- **Tag**: v3.3.0
- **Release Date**: 2026-06-09
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 3.2.0 - 2026-06-09

### Major IDE upgrade (editor / execution / compilation / debugging)

Driven by a multi-agent IDE UX audit (`docs/reports/report-ide-ux-analysis-2026-06-09.md`): all 4
critical issues, 14/14 HIGH and 15/21 MEDIUM resolved. **687/687 JUnit, 20/20 examples.**

Highlights: undo/redo, source formatter (Ctrl+Alt+L), code completion (Ctrl+Space), debounced
highlighting, bracket matching/auto-close, working Stop (cancellable queries), lazy streaming + result
table, clickable Problems view, Compile to .jpc, **line-accurate breakpoints** with persistence, real
stepping shortcuts, Variables tree, Watch expressions, Run-to-Cursor/Restart, settings/session
persistence, Go-to-Line/Quick-Open.

New engine APIs: `Prolog.solveStream`, `Prolog.solveLegacy`, `Prolog.getPredicateIndicatorAtLine`,
`Rule.getSourceLine/setSourceLine`, `StreamManager.out()/setThreadLocalOutput`,
`core.write.v2.PrologFormatter`, `QueryCancelledException`.

Not done (with rationale): dark theme (needs the FlatLaf external dependency — barred by the no-deps
policy); lexer-based highlighting and a new terminal-input model (large rewrites, low marginal value);
conditional/hit-count breakpoints (cross-cutting DebugController change).

### Repository Information
- **Tag**: v3.2.0
- **Release Date**: 2026-06-09
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 3.1.0 - 2026-06-09

### Clean-room v2 resolution engine is now the DEFAULT

`core.engine.v2.MachineSolver` is now the default query-resolution engine (iterative SLD — no
`StackOverflowError` on deep recursion — mutable bindings + trail, lazy enumeration). The last
engine gaps were closed (ISS-2025-0313..0319), bringing it to **full parity: 675/675 JUnit tests +
20/20 example programs**:

- ISS-0313 cyclic-term-safe resolve (`representation_error(cyclic_term)` not StackOverflow)
- ISS-0314 module integration with **export enforcement** for `Module:Goal`
- ISS-0315 profiler hook · ISS-0316 backtrackable globals (`b_setval` via legacy `Trail` rollback)
- ISS-0317 destructive `setarg/3` (built-ins receive shared term objects)
- ISS-0318 coroutining (`freeze`/`when`/`dif` via the attribute-unify hook; cross-query persistence)
- ISS-0319 tabling (`:- table` delegated to the legacy SLG solver)

Fall back to the legacy recursive solver with `-Djprolog.engine=legacy` (also 675/675).

### Repository Information
- **Tag**: v3.1.0
- **Release Date**: 2026-06-09
- **Compatibility**: Java 8+, Maven 3.6+

---

## Release 3.0.0 - 2026-06-08

### Clean-room rewrites (the headline of v3.0.0)

- **Parser — now the DEFAULT** (`core.parser.v2`, ISS-2025-0290..0293): single-pass `Lexer` +
  operator-precedence/Pratt `TermReader`. Canonical functor, operator-as-atom, postfix, `0'c`/radix,
  doubled-quote escapes, quote-aware splitting. Parses 123/130 examples (legacy: 117).
  `-Djprolog.parser=legacy` to fall back.
- **CLP(FD) — now the DEFAULT** (`builtin.clpfd.v2`, ISS-2025-0291): interval domains (no OOM),
  per-query trail-backtracked store, sound first-fail labeling, real `#\=` propagation;
  `Cmp`/`Sum`/`Mul`/`Abs`/`AllDifferent`/`Linear`/`Reified`/`Mod`. `-Djprolog.clpfd=legacy`.
- **DCG — now the DEFAULT** (`core.dcg.v2.DCGTranslator`, ISS-2025-0304): single-pass ISO translator
  (head push-back, `|`, `\+`, `call//N`, `{}`, `!`, `->`); resolves LIM-021. `-Djprolog.dcg=legacy`.
- **New standalone modules**: term writer (`core.write.v2`) and arithmetic evaluator
  (`core.arith.v2`, BigInteger/double, ISO errors, IEEE compare).
- **v2 resolution engine — OPT-IN** (`core.engine.v2.MachineSolver`, `-Djprolog.engine=v2`,
  ISS-2025-0307..0312): iterative SLD (deep recursion with no `StackOverflowError`), bindings + trail,
  lazy enumeration, cut/ITE/soft-cut/`\+`/`call/N`, native `findall`/`catch`/`throw`, `assert`/`retract`,
  built-in bridge, module-qualified calls, occurs-check. ~664/670 through v2; legacy default 675/675.
- **New operators**: `div`, `rdiv` (400 yfx, ISS-2025-0300). **New built-ins**: `setup_call_cleanup/3`,
  `call_cleanup/2`.
- **Limitations resolved**: LIM-017/018/019/020/021/022/025.

### Repository Information
- **Tag**: v3.0.0 (released)
- **Release Date**: 2026-06-08
- **Compatibility**: Java 8+, Maven 3.6+

### Implementation Audit Fixes (ISS-2025-0245..0312)

Eight fixes from a multi-agent correctness/ISO audit (101 confirmed findings; full
report in `docs/reports/report-implementation-audit-2026-06-07.md`).

**Correctness**:
- ISS-0245 `append/3` works with unbound list elements (`append([a],[X],R)` → `R=[a,X]`)
- ISS-0251 `retract((Head :- Body))` matches stored rules
- ISS-0246 `set_prolog_flag(occurs_check, …)` actually affects unification

**ISO conformance**:
- ISS-0247 `(**)/2` is the float power (`2 ** 3 =:= 8.0`); integer power is `(^)/2`
- ISS-0249 `mod/rem///div`/bitwise/shift raise `type_error(integer,_)` on floats
- ISS-0248 `is/2` + comparisons raise ISO error terms (instantiation_error, type_error(evaluable,_))
- ISS-0250 rounding functions promote to `BigInteger` instead of saturating to `Long.MAX_VALUE`

**CLP(FD)**:
- ISS-0252 `ConstraintStore` reset per top-level query (no cross-query leak; per-engine store deferred)
- ISS-0262 ADD/SUB bounds inference computed in `long` + clamped (no int overflow)
- ISS-0263 huge `in` ranges raise `resource_error` instead of OOM / infinite loop
- ISS-0264 `indomain/1` skips constraint-violating values (single-goal local consistency)

**DCG**:
- ISS-0253 `phrase/2,3` are multi-solution (enumerate all parses on backtracking)
- ISS-0254 DCG `!` threads the difference list (was mistranslated to `!/2`)
- ISS-0255 `call_dcg/3` runs the DCG body (was a stub)

**Parser**:
- ISS-0256 negative hex/octal/binary/char-code literals keep their sign (`-0xFF` → `-255`)

**Lists / strings**:
- ISS-0266 `subtract/3`/`intersection/3`/`union/3` distinguish atom `'1'` from number `1`
- ISS-0267 `split_string/4` keeps empty substrings (SWI semantics)
- ISS-0268 `atomic_list_concat` accepts numbers

**Arithmetic / database**:
- ISS-0269 `type_error(evaluable, _)` culprit is the compound `'/'(Name,Arity)`
- ISS-0270 `clause/2` ISO errors + uses the predicate index (not full-KB scan)
- ISS-0271 `min/2`/`max/2` preserve the selected operand's type (`min(2,3.0)=2`)
- ISS-0272 `gcd/2` with a float operand raises `type_error(integer,_)`
- ISS-0273 NEW `setup_call_cleanup/3` + `call_cleanup/2` (guaranteed cleanup)

**ISO/robustness (2nd re-triage)**:
- ISS-0274 `=:=`/`=\\=` IEEE semantics (`-0.0 =:= 0.0` true, `nan =:= nan` false)
- ISS-0275 `throw/1` copies the ball (`copy_term`)
- ISS-0276 `upcase_atom`/`downcase_atom` locale-independent
- ISS-0277 `atom_length/2` ISO error terms
- ISS-0278 `op/3` rejects non-integer precedence
- ISS-0279 `initialization/1` directive runs (after the file loads)

**Re-triage batch 2 (contained)**:
- ISS-0280/0281 thread-safety: `getCurrentPredicates` sync, `DebugController` concurrent collections
- ISS-0282 removed dead `,`/2 `Conjunction` built-in
- ISS-0283 `op/3` accepts a list of names
- ISS-0284 `number_string/2` integer BigInteger precision
- ISS-0285/0286/0287 trailing-newline line count, CLI UTF-8, `StreamManager` cache eviction
- ISS-0288 directive failures surfaced on stderr
- ISS-0289 hoist loop-invariant `extractVariables` (perf)

**Standard order of terms (ISO)**:
- ISS-0261 integers and floats are distinct terms: `1 \= 1.0`, `1 \== 1.0`, float sorts
  before equal integer (`compare(O,1,1.0)` → `>`), `sort/2` no longer dedups them; `.jpc`
  format v0x02 preserves int/float type + BigInteger precision

**Resource handling**:
- ISS-0257 HTTP `disconnect()` in `finally`
- ISS-0258 `StreamManager` uses `ConcurrentHashMap`
- ISS-0259/0260/0265 JDBC: don't close managed statements via `closeResultSet`; close ad-hoc statement on error; metadata + `jdbc_call_get_resultset/2` try-with-resources

**Audit correction**:
- "No first-argument indexing" was stale — it is implemented (`KnowledgeBase.getRulesWithFirstArgIndex`) and used by `QuerySolver` (~1 ms lookup over 1000 facts). LIM-023 corrected.

**Tests**: 550/550 JUnit pass (+33 new); 20/20 examples pass.

**Deferred (tracked, LIM-019, 021..025)**: parser hardening (`0'c` char codes, quote-aware
clause splitting, canonical functor notation, operator-as-atom, doubled-quote escapes),
remaining CLP(FD) soundness (cross-goal `indomain`/`#\=` trail integration, interval domains,
lazy labeling, Hall-interval `all_different`), last-call optimization, threading isolation,
remaining DCG (head pushback, `\|` alternative), `with_output_to/2` thread-safety.
(First-argument indexing is NOT deferred — it is implemented and used; audit finding was stale.)

---

## Release 2.8.2 - 2026-05-20

### Twelfth-Round String/Term/Write Fixes (ISS-2025-0233..0243)

11 fixes; 2 verified already-correct.

**Strings**:
- ISS-0233 codepoint-aware string_chars/split_string/atomic_list_concat
- ISS-0236 string_chars accepts atom input
- ISS-0237 atomic_list_concat/2 (no separator)
- ISS-0239 atom_string both-var → instantiation_error
- ISS-0240 number_string exact bit-pattern compare

**Term ops**:
- ISS-0234 =../2 number support (ISO §8.5.3)
- ISS-0235 atom_number hex/binary/octal prefixes
- ISS-0238 atom_to_term/3

**Write semantics**:
- ISS-0242 new TermFormatter — operator-aware output for write/writeln/writeq/format
- ISS-0243 term_to_atom roundtrip with operators

**Tests**: 478 JUnit (7 new), 20/20 examples.

---

## Release 2.8.1 - 2026-05-20

### Eleventh-Round List & Arithmetic Fixes (ISS-2025-0215..0231)

Deep dive into list handling + `is/2`. 12 fixes; 6 audit findings verified already-correct.

**Lists**:
- ISS-0215 length/2 fresh-vars global counter
- ISS-0216 is_list/proper_list/length cycle detection (IdentityHashMap)
- ISS-0220 sort/4 (Key, Order, List, Sorted)
- ISS-0222 maplist/5
- ISS-0221 Partition class (unregistered to avoid shadowing)
- ISS-0223 partial_list iterative + cycle detect

**Arithmetic ISO §9**:
- ISS-0224 ^/2 integer power evaluable
- ISS-0225 integer/1 truncating evaluable
- ISS-0226 sinh/cosh/tanh/asinh/acosh/atanh
- ISS-0227 log/2 base-N, cot/acot/cbrt, epsilon constant
- ISS-0229 0.0**-N → evaluation_error(undefined)
- ISS-0231 rational/rationalize evaluables

**Tests**: 471 JUnit (12 new), 20/20 examples.

---

## Release 2.8.0 - 2026-05-20

### Tenth-Round ISO Audit Fixes (ISS-2025-0195..0214)

Deep theoretical audit per ISO 13211-1 covering resolution, arithmetic, parser, built-ins, and I/O. 18 fixes applied; 3 verified already correct or design choice; 1 deferred.

- **ISS-2025-0195** setof/3 now sorts + dedups via StandardTermOrdering
- **ISS-2025-0196** bagof/3 implements free-variable witness grouping
- **ISS-2025-0198** Full ISO escape sequences in strings/atoms (octal `\NNN\`, hex `\xH+\`, line continuation), tokenizer-aware
- **ISS-2025-0199** Line-continuation `\<newline>` returns empty
- **ISS-2025-0200** double_quotes flag honored (codes|chars|atom|string)
- **ISS-2025-0201** Soft-cut `*->` operator + IfThenElse semantics
- **ISS-2025-0202** read_term/3 dispatches to passed stream via StreamManager
- **ISS-2025-0203** read/2 new arity for stream-based reading
- **ISS-2025-0204** syntax_errors option in read_term/2,3
- **ISS-2025-0205** functor/3 supports numbers as 0-ary; proper type_error on bad construction
- **ISS-2025-0209** between/3 accepts atom inf as upper bound
- **ISS-2025-0210** gcd/2 evaluable functor added
- **ISS-2025-0211** char_code/2 + atom_chars/2 handle supplementary Unicode codepoints
- **ISS-2025-0212** number_codes/2 accepts up to U+10FFFF (consistency)
- **ISS-2025-0213** PeekByte registers PushbackInputStream wrapper

**Tests**: 459 JUnit (10 new), 20/20 examples regression.

---

## Release 2.7.1 - 2026-03-25

### Cut Semantics Fixes & DCG Unicode

4 fixes for cut propagation correctness and DCG Unicode handling.

- **ISS-2025-0194**: QuerySolver.handleBuiltIn cut flag, LCO prefix goal cut propagation, body goal cut propagation from control structures, DCGTransformer supplementary Unicode in string literals

---

## Release 2.7.0 - 2026-03-25

### Ninth-Round Deep Analysis Fixes — Unicode & Precision

14 fixes across 18 files for Unicode support, arithmetic precision, and correctness.

- **ISS-2025-0193**: ReadTerm variable classification operator precedence, WriteTerm ISO quote escaping (''), TermParser hex/octal/binary BigInteger precision, Plus/3 long arithmetic, CharCode full Unicode range (0-0x10FFFF), AtomLength/StringLength codePointCount, StringCodes/AtomCodes supplementary Unicode support, Format char truncation, Include/Exclude binding accumulation, DCGTransformer unique rule-scoped variables, AggregateAll ISO term ordering, PeekChar/PeekCode pushback stream persistence, TermVariables anonymous skip

---

## Release 2.6.9 - 2026-03-25

### Eighth-Round Deep Analysis Fixes

13 fixes for unification correctness, precision, and robustness across 13 files.

- **ISS-2025-0192**: ListTerm.unify() rollback on partial failure, Union/3 Set1 dedup, Clause/2 TermCopier variable renaming, SumList integer precision, MaxList/MinList first-element init, Between long precision, PutCode supplementary Unicode, Tab negative validation, TermCopier PrologString handling, ListTerm resolveBindings optimization, Read Scanner safety, JpcWriter Rational documentation

---

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

## Release 2.5.0-packages - 2026-03-21

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