# JProlog — deliberate deviations

The single list of places where JProlog 4.6.0 **deliberately** answers differently from the
reference semantics, with the reason for each. Anything that differs and is *not* listed here is
a bug or a documented limitation (`docs/tracking/track-limitations.md`), not a design choice.

**Reference semantics**: ISO/IEC 13211-1 first; SWI-Prolog 9 where ISO is silent or where every
modern system agrees on an extension. "SWI" below means SWI-Prolog 9.

Every entry is pinned by a test, so changing one fails the suite instead of passing silently.

---

## 1. Engine design decisions (engine v4, design B.17 — approved 2026-08-25)

| Behaviour | JProlog | ISO / SWI | Why |
|---|---|---|---|
| Rational (cyclic) terms | `X = f(X)` **succeeds**; unification, comparison, `copy_term/2` and the writer are cycle-safe | ISO: undefined without occurs check; SWI: supported | SWI/YAP/SICStus behaviour; `set_prolog_flag(occurs_check, error)` gives the ISO error. A cyclic term cannot be *stored* (`assertz`, `nb_setval` raise `representation_error(cyclic_term)`, LIM-040). |
| `setup_call_cleanup/3`, `call_cleanup/2` | Cleanup runs after the goal's **last** solution, on exception, on cut, and when the query is abandoned (a streaming sink stops, a budget/Stop aborts it) | SWI: the same | ISO has no cleanup; this is SWI's contract. |
| `append(X, Y, Z)` all unbound | **enumerates** (infinitely) | SWI: the same | The library is Prolog clauses (`prelude/lists.pl`), as in SWI; the old eager Java version looped. |
| `member(X, PartialList)` | **extends** the open tail | SWI: the same | Same reason. |
| Coroutines across queries | a suspended goal (`freeze/2`, `dif/2`, `when/2`) dies with the query that created it | SWI: per toplevel query too | One query = one variable lifetime. |
| Library predicates | `member/2`, `append/3`, `maplist/N`, `foldl/4..6`, `pairs_*`, the CLP(FD) helpers … are prelude **Prolog clauses**, lazy, traceable and overridable by a user definition | SWI: library(lists) etc. | They behave exactly like SWI's library code (choice points included). |
| Console answers | quoted operator notation, `_A`-style names for fresh variables | SWI: the same | — |

## 2. Error terms (pinned row by row in `EngineV4IsoErrorsTest`)

| Goal | JProlog | ISO / SWI | Why |
|---|---|---|---|
| `arg(N, foo(a,b), A)`, N unbound | **enumerates** N = 1, 2 | ISO 8.5.2.3 (a): `instantiation_error`; SWI enumerates | Strictly more useful; every modern system does it. |
| `close(foo)`, `set_input(foo)`, `write(nosuch, x)` | `existence_error(stream, foo)` | SWI: `domain_error(stream_or_alias, foo)` | ISO 8.11.5.3 (c) supports the existence reading for an atom that names no open stream (GNU Prolog agrees); pinned by ISS-2025-0377. |
| `atom_concat(f(a), b, C)` | `type_error(atom, f(a))` | ISO 8.16.2.3: `type_error(atomic, f(a))` | `atom_concat/3` requires atoms (so `atom_concat(a, 1, R)` raises), pinned by ISS-2025-0278. |
| `format("~w", X)`, `format("~q", a)` | succeed: a non-list second argument is ONE argument | SWI: the same | ISO has no format/2. |
| `X #= Y`, both unbound | succeeds: constrains X to the integers and **unifies** X and Y (4.6, ISS-2025-0761) | library(clpfd): the same | — |
| Error context | the second argument of `error/2` is usually the atom `'Name/Arity'` (e.g. `'call/1'`, `'=:=/2'`) | ISO: implementation defined; SWI: `context(Name/Arity, Message)` | Code that matches `error(Formal, _)` — the portable way — is unaffected. |
| `format/2,3` argument faults | `error(format(Message), _)` | SWI 9 uses `format(Message)` for most, `format_argument_type/2` for some | ISS-2025-0595. |
| A query that does not parse | `error(syntax_error(Message), query)` | SWI: `error(syntax_error(M), string(Text, Pos))` | ISS-2025-0671; the formal is the ISO one. |
| Extended-library errors (4.6 wave Q1) | `error(Formal, context(Name/Arity, Message))` — SWI's shape | SWI: the same | ISS-2025-0681. The ISO-core natives keep the `'Name/Arity'` atom context above. Three sites have no predicate at hand and use a plain atom: the graph edge parser (`error(type_error(edge, E), graph)`), the JDBC handle table (`…, jdbc)`) and the socket table (`…, socket)`). |
| Host failures of the libraries (4.6 wave Q1) | missing file/directory/host/class/handle → `existence_error(Kind, X)`; access denied → `permission_error(Action, Kind, X)`; other I/O → `io_error(Op, X)`; SQL and everything else → `system_error(Message)` | SWI: the same formals; SWI's odbc/socket libraries use their own (`odbc(State, Code, Msg)`, `socket_error(Code, Msg)`) | §8 of the 4.6 spec; ISS-2025-0681. |
| Text arguments of the libraries | an atom **or a string** (`xml_parse("<a/>", X)` works) | SWI: text | ISS-2025-0681 widened the old atom-only checks. |
| `tcp_close(nosuch)`, `jdbc_disconnect(nosuch)` | succeed | SWI: `existence_error` | Closing an unknown handle is treated as idempotent (unchanged since 3.x). |
| `java_*` (FFI) with a bad argument | **fails** | SWI's JPL raises | The FFI was designed fail-on-anything (ISS-2025-0160); left for a later wave (the Q1 harness counts these as `fail`, not as message atoms). |
| `thread_join/2` status of a worker stopped by the budget | `exception(error(resource_error(inference_limit), inference_limit_exceeded))` | — | ISS-2025-0698: in the exhausted query the budget stays uncatchable (§5); the status is read by another query. |

## 3. Decisions of the 4.5.0 production-readiness program (§8 of its report)

These changed a 4.4.0 behaviour to the SWI one; they are **not** deviations from SWI, but they
are behaviour changes an upgrading program may notice.

| Behaviour | 4.4.0 | 4.5.0 |
|---|---|---|
| `op/3` (and `char_conversion/2`) on backtracking | undone | permanent (ISO/SWI) |
| `integer/1` evaluable | truncated | rounds half away from zero (SWI; `integer/1` is not an ISO evaluable) |
| negative shift count | `evaluation_error` | shifts the other way (SWI) |
| `intersection/3`, `union/3`, `subtract/3` | removed duplicates | keep duplicates (SWI library(lists)) |
| `string_concat(-, -, -)` | failed | `instantiation_error` |
| `call((fail, 1))` | failed | `type_error(callable, (fail,1))` — the body is checked before it runs (ISO 7.6.2, SWI) |
| `M:G` for a `G` that `M` does not export | failed | runs `G` in `M` (SWI) |
| `print/1` | unquoted | `portray` hook, then `writeq` (SWI) |
| `tab(Expr)` | failed on a non-integer | evaluates `Expr`; `type_error(evaluable, …)` otherwise |
| `format ~r` without a radix | hexadecimal | `format(…)` error |
| `plus/3` with floats | accepted | `type_error(integer, …)` |
| `atomic_list_concat(L, '', A)` in split mode | split into characters | `domain_error(non_empty_atom, '')` |
| global variables (`nb_setval/2`, `b_setval/2`) | shared by all threads | per thread (SWI) |
| directives in a consulted file | ran once per solution | run once (`once/1`, ISO 7.4.2), in the module being loaded |
| consult errors | first error threw | reported as warnings on `user_error`, the load continues (SWI); `Prolog.consult(String)` still throws one exception listing them |
| `listing/1` | `% Listing for p/1:` header | SWI `portray_clause` layout, no header |
| `writeq` of floats ≥ 1e15 or < 1e-4 | E notation from 1e7 | SWI's shortest layout (`10000000.0`, `1.0e15`); `1.0Inf`, `-1.0Inf`, `1.5NaN` |
| `writeq('\e')` | `'\33\'` | `'\e'` (SWI) |
| CLI demo facts (`father/2`, `likes/2`, …) | loaded at startup | only with `--demo` |
| trace ports | a deterministic exit could be followed by a phantom `Fail` port; a body failure could skip the parent's `Fail` | SWI's port sequence (no Fail after a deterministic Exit; the parent's Fail is printed) |

## 4. Remaining deliberate differences from SWI

| Behaviour | JProlog | SWI | Why / where |
|---|---|---|---|
| Consulted (static) predicates | stay modifiable by `assert/retract/abolish` | `permission_error(modify, static_procedure, …)` | Too disruptive for existing programs (decision §8); `predicate_property(P, static)` still reports them static. |
| Tabled non-stratified negation with `\+` (`p :- \+ p` tabled) | `permission_error(negate, incomplete_table, G)` | `\+` is not tabling-aware either; `tnot/1` gives WFS | Use `tnot/1` (4.6, §4c) for the well-founded answer; `\+` keeps raising rather than answering inconsistently (LIM-046). |
| `nth0(-1, [a], X)` | fails | fails (`nth0/3`); `must_be` only in `nth0/4` | Matches SWI — listed because the 4.5 spec guessed otherwise. |
| `last/2`, `nth0/3`, `nth1/3` on a partial list | enumerate without end | the same | Callers must bound the search (LIM-043). |
| `length(L, L)` | fails | differs | The 4.5 spec left the choice open; P4 picked failure (LIM-043). |
| `~Nw` / `~Nq` in `format/2` | right-align in a column of N | column stop | A JProlog extension pinned by ISS-2025-0249. |
| Threads | every non-worker thread is the Prolog thread `main` (one shared queue); thread/queue/mutex tables are JVM-wide | one `main` | LIM-045. |
| CLI exit status | 1 for both failure and an uncaught error of `-g`/`initialization(main)`; 2 for a bad option | 1 / 2 | ISS-2025-0627. |
| Inference budget | counts steps plus O(N) work per element in the list/atom natives; a bridged library call (regex, json, xml, csv, crypto, ...) is charged one step per 64 characters of text input and one per solution, and the regular expressions one step per 256 characters the matcher reads (backtracking included); exact for one machine, overshoot ≤ 1 024 steps per extra worker | no equivalent | ISS-2025-0624, ISS-2025-0786 (4.6 Q6), LIM-045. |
| Safe mode | a deny-list sandbox (`enableSafeMode`), `halt/0,1` → `permission_error(call, sandboxed, …)` unless `SafeModeOptions.allowHalt()` | library(sandbox): an allow-list | ISS-2025-0625, ISS-2025-0672. |

## 4a. Decisions of the 4.6 completeness program, wave Q2 (ISS-2025-0710..0718)

| Behaviour | JProlog | SWI | Why / where |
|---|---|---|---|
| `limit/2`, `call_nth/2` (bound N) at the last solution | cut the goal with the machine's `!`: its cleanups run then, and a cleanup's exception propagates as from `!` | `!` in the library clause | Same semantics; `pushFiltered` keeps the goal lazy instead of `nb_setarg/3` state. `EngineV46StandardPredicatesTest`. |
| `call_nth(G, 0)` / negative N | fails / `type_error(nonneg, N)` | same (`must_be(nonneg, N)`) | ISS-2025-0710. |
| `limit/2`, `offset/2` count | integer or `infinite` only (`type_error(integer, C)`) | any evaluable (`Count > 0`) | A count is an integer; ISS-2025-0710. |
| Rational `**` | always a float (`1r2 ** 2 =:= 0.25`) | a rational when `prefer_rationals` | JProlog's `**` is ISO float power for every operand (ISS-2025-0712); use `^` for exact powers. |
| Rational literal `1r3` | read by the v2 reader, `number_codes/2`, `atom_number/2`; `rational_syntax` is read-only `compatibility`; the legacy parser (`-Djprolog.parser=legacy`) does not read it | `natural` mode also reads `1/3` | ISS-2025-0712. |
| `rationalize/1` | the first continued-fraction convergent of the float's exact value that converts back to the same float | SWI's float-based convergent loop | Exact and always terminating; the same answers on the usual inputs (`rationalize(0.1) =:= 1r10`). |
| Standard order float vs rational | compared as floats, the float first on a tie | the same | Integers vs rationals compare exactly. |
| `max(E)`/`min(E)` in `aggregate/3,4` | numeric (evaluated) | numbers, or the standard order for non-numbers | As `aggregate_all/3` since ISS-2025-0522. |
| `print_message/2` | every kind on `user_error`; the built-in translation reaches `message_hook/3` as `['~w'-[Line], nl, ...]` | SWI's message translations | ISS-2025-0713; LIM-043. |
| `garbage_collect/0` | succeeds without `System.gc()` | runs the GC | An embedded engine shares its JVM (ISS-2025-0711). |
| `setenv/2`, `unsetenv/1` | a process-wide overlay read by `getenv/2` and passed to `shell/1,2`/`shell_output/3` children; `System.getenv` itself is unchanged | change the process environment | The JVM cannot change its own environment (ISS-2025-0718). |
| Clause look-ahead determinism | a clause frame is dropped when no remaining clause's head can match by a per-argument principal-functor test | JIT multi-argument indexing | Shallow but conservative (ISS-2025-0715): exits are deterministic where SWI's are for the common shapes (list in any argument position, bound constants). |

## 4b. Decisions of the 4.6 completeness program, wave Q3 (ISS-2025-0730..0739)

| Behaviour | JProlog | SWI | Why / where |
|---|---|---|---|
| The bar `\|` | `op(1105, xfy, '\|')`; `(a\|b)` is `'\|'(a,b)`, a goal `'\|'/2` is `;/2` | the same (manual §4.25: `1105 xfy \|`, `700 xfx as`) | The 4.6 spec said 1100; the SWI manual's table says 1105 — SWI wins (ISS-2025-0734). |
| A clause of a non-multifile predicate in a second file | ADDED to the predicate (both files' clauses stay) | "Redefined static procedure" warning, the first file's clauses are wiped | Loading two files that add to one predicate was always accumulative in JProlog; changing it would break programs split over files. Reconsulting a file removes only that file's clauses either way (ISS-2025-0730). |
| Reconsulting a file whose predicate has `assertz`ed clauses | the asserted clauses go (a reload resets the file's predicates), except for a multifile predicate | the same | Clauses owned by OTHER files always stay (ISS-2025-0730). |
| Discontiguous clauses | a warning on `user_error` (also for text consulted from Java) | the same warning | `:- discontiguous` or `:- dynamic` silences it (ISS-2025-0730). |
| A clause `M:H` for a module other than `user` or the one being loaded | module M is created on demand and holds the clause as an `M:H` clause of the flat store (what `assertz(M:H)` stores); calls, `clause/2`, `retract/1`, `current_predicate(M:PI)`, `predicate_property(M:H, _)` find it | a clause in module M | JProlog's module clauses are the consult-time `ModuleManager` records; runtime writes go to the flat store. When module M ALSO defines the predicate in its own file, a qualified call uses the module's clauses only (ISS-2025-0733). |
| goal_expansion/2 | applied to clause bodies and directives at load time, through the control constructs and the common meta-predicates; depth cap 100 → `resource_error(goal_expansion_depth)` load error | also expands `call/N` arguments by meta_predicate declarations, `system:goal_expansion/2`, and keeps source layout | ISS-2025-0731. |
| `file_search_path/2` defaults | not clauses: `library`, `swi`, `foreign` are applied by the file search after the user's clauses (`file_search_path(library, D)` answers only user clauses) | default clauses in `user` | Keeps a fresh engine's knowledge base empty (`listing/0`, `:save`); ISS-2025-0737. |
| `absolute_file_name/3` | options `extensions`, `file_type`, `access`, `relative_to`, `solutions`, `file_errors`; `expand` and the rest accepted and ignored | more options | ISS-2025-0737. |
| `use_module/2` of a prelude library (`library(lists)`) | the import list is accepted and not enforced (the prelude autoloads) | enforced | ISS-2025-0735. |
| `subterm_positions/1` | character offsets (UTF-16 units) in the stream; `quasi_quotation_position` and `dict_position` never occur | the same layout | ISS-2025-0738. |
| `comments/1` | `'$stream_position'(Char, Line, LinePos, Char)-String`; the byte count is the character count | stream position terms | ISS-2025-0738. |
| Load lock | per file; a wait that can never end (a cycle through file locks, `thread_join`, `concurrent_*` workers, mutexes, a full thread pool or — since 4.6 Q4 — a `thread_get_message/1,2,3` whose every possible sender is blocked) → `permission_error(load, source_sink, File)` | per file, deadlock detection that loads anyway | ISS-2025-0739, ISS-2025-0746. |
| Concurrent loads of module files | the module being loaded and (since 4.6 Q4, ISS-2025-0748) the `op/3` module context are per thread | per thread | ISS-2025-0739. |

## 4c. Decisions of the 4.6 completeness program, wave Q4 (ISS-2025-0745..0755)

| Topic | JProlog | SWI-Prolog 9 | Why |
|---|---|---|---|
| **Tabling across threads** | Table SPACES: the non-worker threads (`main`) share the engine's main space; each worker machine (`thread_create/2,3`, `concurrent_*`) has a PRIVATE space that ends with it. `:- table p/1 as shared` publishes COMPLETE tables engine-wide; incomplete tables are owned by the evaluating thread. A thread calling a shared variant that another thread is still evaluating **evaluates it itself** | private by default; `as shared`; a thread calling an incomplete shared table **waits** for its owner (with deadlock handling) | No cross-thread wait means no tabling deadlock at all, and no incomplete table is ever read by a second thread; the cost is possibly duplicated work on a shared variant raced by two threads. Several `main` threads (IDE, embedders) on one engine still serialise through the main space's evaluation claim (unchanged, ISS-2025-0488). ISS-2025-0752. |
| Table options | `variant`, `shared`, `private` implemented; `subsumptive`, `incremental`, `opaque`, `monotonic`, `lazy`, `dynamic`, `max_answers/1`, `subgoal_abstract/1`, `answer_abstract/1` accepted with a warning (variant tabling); others `domain_error(table_option, O)` | all implemented | Subsumptive tabling gives the same answers; incremental tabling is the real gap (JProlog invalidates a table only when its own predicate is modified). ISS-2025-0753. |
| Mode-directed `po(PI)` | ONE aggregated answer per index key: keep Old when `call(PI, Old, New)` succeeds, else New | the same (`boot/tabling.pl`, `update_goal/5`) | The 4.6 spec asked for a Pareto front (XSB's partial-order answer subsumption keeps every maximal answer); SWI's source keeps one — SWI wins. ISS-2025-0754. |
| Unknown table mode | `domain_error(tabled_mode, M)` | the same | 4.5 said `table_mode`. ISS-2025-0754. |
| `sum` mode in a recursive component | re-added every completion round (may not terminate) | SLG: summed once per derivation | Linear tabling re-runs an SCC until nothing changes. ISS-2025-0754. |
| **WFS extent** | `tnot/1`, `undefined/0`, `call_delays/2`, conditional answers with delay lists simplified at SCC completion; CLI prints `undefined`; `Prolog.currentAnswerDelays()` | full SLG resolution with answer completion, residual program (`call_residual_program/2`), toplevel residual display | Minimal WFS on linear tabling (decision §8). Not done (LIM-046): answer completion (an unsupported positive loop among conditional answers stays undefined, SWI makes it false), the residual program, delays through mode-directed tables, `\+` over incomplete tables (still raises). ISS-2025-0755. |
| `tnot/1` of a non-tabled goal | `permission_error(tnot, non_tabled_procedure, Name/Arity)` | an error from `'$tnot_implementation'/2` | JProlog's choice of error term. ISS-2025-0755. |
| `call_delays/2` | consumes the delays (the enclosing derivation stays unconditional); a used conditional answer is reported as the answer itself (`p`), a delayed negation as `tnot(G)` | the same literals | ISS-2025-0755. |
| Working directory | per engine; `user.dir` is never written; relative paths of every file built-in resolve against it | per process (`chdir`) | Several engines/embedders share one JVM. ISS-2025-0745. |
| `thread_signal/2` | the goal runs on the target's own goal stack at its next inference (or inside a blocking thread built-in) as `ignore(\+ \+ G)`; signals of one thread run in order; a signal to `main` is run by whichever non-worker thread polls first | at the next call port, in the target | ISS-2025-0749. |
| A mutex held at thread exit | released + `print_message(warning, format(...))` | released + warning | ISS-2025-0751. |

## 4d. Decisions of the 4.6 completeness program, wave Q5 (ISS-2025-0760..0770)

Residual constraints follow SWI-Prolog 9's printer (`clpfd:attribute_goals//1`) where its form is
known; where it is not, JProlog prints the closest canonical form below. No SWI installation was
available while implementing, so the forms were taken from SWI's `clpfd.pl` as recalled
(`attribute_goal_//1`, `scalar_product_left_right/4`, the `x_leq_y_plus_c`/`pplus`/`ptimes`
propagators); the ones marked *canonical* are JProlog's choice.

| Topic | JProlog | SWI-Prolog 9 | Why |
|---|---|---|---|
| `X #> Y`, `X #< Y`, `X #>= Y`, `X #=< Y` | `Y#=<X+ -1`, `X#=<Y+ -1`, `X#>=Y`, `Y#>=X` | the same (`x_leq_y_plus_c`, `pgeq`) | — |
| `X #= Y+Z`, `X #= Y+2`, `X #= 2*Y` | `Y+Z#=X`, `Y+2#=X`, `2*Y#=X` | `pplus`/`ptimes` forms, the same | — |
| `X #= Y-2` | `X+2#=Y` (*canonical*: the constant is kept positive) | possibly `Y+ -2#=X` | Readability; same constraint. |
| `X+Y #>= 3`, `-X-Y #>= 3` | `3#=<X+Y`, `X+Y#=< -3` (*canonical*, scalar-product layout) | an auxiliary sum variable: `X+Y#=_A, _A in 3..sup` | JProlog posts one linear constraint, so there is no auxiliary variable to print. |
| Longer linear constraints | `scalar_product_left_right/4` layout: positive coefficients left, negated negative ones right, the constant on the side that keeps it positive, variables in age order | the same layout; SWI's variable order is by address | Same text for the usual (query-order) variables. |
| Non-linear sub-expressions | each auxiliary variable prints as `_A` with its defining constraint (`X #= Y*Z+W` → `W+_A#=X, Y*Z#=_A`) | the same decomposition idea; the exact split may differ | SWI's `parse_clpfd` and JProlog's expression compiler introduce auxiliaries at slightly different places. |
| Reified comparisons | `B #<==> X #> 3` → `B in 0..1, X#>=4#<==>B`; the inner comparison in scalar-product layout | `reified_geq` etc., the same for the common cases | — |
| Decided reification | `B = 1` shows the posted comparison (or nothing when entailed) | the same | — |
| `zcompare/3` with unbound Order | three reified booleans + `freeze/2` watchers; the residual goals show them | one `pzcompare` propagator printed as `zcompare(O,A,B)` | Built in Prolog on the reification layer. |
| `lex_chain/1`, `disjoint2/1`, `automaton/3,8`, `chain/2` | decomposed into reified constraints / `tuples_in/2`; residual goals show the decomposition | `lex_chain` and `disjoint2` print their original goal | Same solutions; the printed residue is longer. |
| `automaton/8` `Template` | variables of `Template` in a counter expression stand for the current element of `Seqs`; `Seqs` unbound is unified with `Sigs` | the same | — |
| `all_different/1` with one free variable left | not printed (value elimination has made it entailed) | printed until all ground | Entailment check; the domain of the last variable says it. |
| `circuit/1` | all-distinct (domain consistent) + no-subtour (chain closing, strong connectivity) | `all_different` + `pcircuit` (SCC based) | At least SWI's pruning. |
| `cumulative/1,2` | time-table over compulsory parts; durations/amounts/limit may be variables (their bounds are used) | time-table | — |
| `global_cardinality/3` | `consistency(value)` accepted (it is what `/2` does), `cost(Cost, Matrix)` via `element/3` + `sum/3` | GAC by default; `consistency(value)` weaker | `global_cardinality/2` does counting propagation only (LIM-041). |
| `fd_degree/2` | the number of constraints still alive on the variable | the number of propagators attached | Counts one JProlog constraint per posted CLP(FD) goal. |
| Coefficients beyond 64 bits | exact (`X*10^20 #= Y`); only those constraints take the BigInteger path | exact everywhere | Domains are still 64-bit (a bound beyond it is infinite), LIM-041. |
| Branch and bound | one iterative depth-first search tightening the bound at every solution, then SWI's `(E #= Opt, labeling ; E #\= Opt, labeling)` order | SWI restarts per improvement | Same answers, same order. |

## 4e. Decisions of the 4.6 completeness program, wave Q7 (ISS-2025-0790..0798)

| Behaviour | JProlog | SWI | Why / where |
|---|---|---|---|
| **Every non-worker thread is `main`** | All threads that are not JProlog worker machines (the CLI thread, the IDE's background solve threads, every embedder thread calling `solve()`) are the one Prolog thread `main` of the engine: they share ONE `thread_signal/2` queue (a signal to `main` is run by whichever of them polls first) and ONE table space (their tabled evaluations serialise through its evaluation claim, ISS-2025-0488). `thread_self/1` answers `main` in all of them | every OS thread that enters Prolog gets its own thread identity (`PL_thread_attach_engine`), its own signal queue and its own private tables | Giving each embedder thread its own identity would change the P6 `main` contract (`thread_self/1`, `thread_signal(main, G)`, message queues) that embedders rely on. Worker machines (`thread_create/2,3`, `concurrent_*`, pools) are real Prolog threads with private queues and tables (ISS-2025-0749, ISS-2025-0752). |
| A reloaded file's clauses of a **multifile** predicate | keep their place: the reload's clauses go where the file's first clause was, the other files' clauses that followed it are moved back behind them (f1 `{p(a)}`, f2 `{p(b)}`, reconsult f1 → `[a,b]`) | the same for an unchanged file (manual §4.3.2 "Reloading files, active code and threads": an unchanged clause is kept, a new one is inserted "before the current clause", the rest is "marked for future deletion") | JProlog does not diff clause by clause: when a file's clauses are interleaved with another file's (f1, f2, f1), the reloaded clauses come back as ONE block at the first one's place. ISS-2025-0794 (4.6 Q3 appended them: `[b,a]`). |
| Recorded database references | `'$record'(N)` terms | `<record>(0x…)` blobs | JProlog has no blob type; the reference is opaque in both. ISS-2025-0791. |
| `erase/1` of an already erased record | fails | an error or failure depending on the version | ISS-2025-0791. |
| `flag/3` values | numbers (evaluated) and atoms | the same | ISS-2025-0791. |
| `ord_insert/3` | provided (= `ord_add_element/3`) | not in library(ordsets) | The DEC-10/YAP name, kept for old programs. ISS-2025-0790. |
| `ord_intersection([], I)` | `I = []` | (implementation-defined) | JProlog's choice. ISS-2025-0790. |
| FFI (`java_*`) errors | argument faults raise ISO errors; a Java exception is `error(java_exception(ClassName), context(Name/Arity, Message))`; an array index out of range FAILS | JPL: `java_exception(@(Ref))` | There is no JPL reference to the exception object; the class name and message are what a handler can use. The index fails like `arg/3`. ISS-2025-0796. |
| `enhanced_phrase/2,3` | = `phrase/2,3` | (not SWI) | The registry version never ran the grammar. ISS-2025-0797. |

## 5. The error-trust model (embedding)

`InferenceLimitException` (budget), `QueryCancelledException` (Stop / thread interrupt),
`DebugController.DebugStopException` (debugger Stop) and `ThreadExitException` (`thread_exit/1`)
are plain Java `RuntimeException`s, **not** `PrologException`s, so untrusted `catch/3` cannot
swallow them. A deep-term `StackOverflowError` inside a bridged built-in is converted to a
catchable `resource_error`. `halt/0,1` surfaces to the embedder as a `PrologException` with
`isHalt()`; the engine itself never calls `System.exit`.
