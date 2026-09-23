# JProlog — deliberate deviations

The single list of places where JProlog 4.5.0 **deliberately** answers differently from the
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
| `X #= Y`, both unbound | succeeds (posts the constraint) | library(clpfd): the same | — |
| Error context | the second argument of `error/2` is usually the atom `'Name/Arity'` (e.g. `'call/1'`, `'=:=/2'`) | ISO: implementation defined; SWI: `context(Name/Arity, Message)` | Code that matches `error(Formal, _)` — the portable way — is unaffected. |
| `format/2,3` argument faults | `error(format(Message), _)` | SWI 9 uses `format(Message)` for most, `format_argument_type/2` for some | ISS-2025-0595. |
| A query that does not parse | `error(syntax_error(Message), query)` | SWI: `error(syntax_error(M), string(Text, Pos))` | ISS-2025-0671; the formal is the ISO one. |

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
| Tabled non-stratified negation (`p :- \+ p` tabled) | `permission_error(negate, incomplete_table, G)` | well-founded semantics (`tnot/1`, undefined answers) | WFS is not implemented (LIM-046); raising is better than an inconsistent answer. |
| `(a | b)` | reads as `(a ; b)` | `'|'(a, b)` | DCG bodies and the operator table depend on it (LIM-044). |
| `as` | not an operator (use `table(as(Spec, Opts))`) | infix operator | LIM-044. |
| Mode-directed tabling `lattice(PI)`, `po(PI)` | `domain_error(table_mode, M)` | supported | LIM-044. |
| `nth0(-1, [a], X)` | fails | fails (`nth0/3`); `must_be` only in `nth0/4` | Matches SWI — listed because the 4.5 spec guessed otherwise. |
| `last/2`, `nth0/3`, `nth1/3` on a partial list | enumerate without end | the same | Callers must bound the search (LIM-043). |
| `length(L, L)` | fails | differs | The 4.5 spec left the choice open; P4 picked failure (LIM-043). |
| Informational `print_message/2` | to the current output | to `user_error` | Errors and warnings do go to `user_error` (LIM-043). |
| `~Nw` / `~Nq` in `format/2` | right-align in a column of N | column stop | A JProlog extension pinned by ISS-2025-0249. |
| Threads | every non-worker thread is the Prolog thread `main` (one shared queue); thread/queue/mutex tables are JVM-wide | one `main` | LIM-045. |
| CLI exit status | 1 for both failure and an uncaught error of `-g`/`initialization(main)`; 2 for a bad option | 1 / 2 | ISS-2025-0627. |
| Inference budget | counts steps plus O(N) work per element in the list/atom natives; exact for one machine, overshoot ≤ 1 024 steps per extra worker | no equivalent | ISS-2025-0624, LIM-045. |
| Safe mode | a deny-list sandbox (`enableSafeMode`), `halt/0,1` → `permission_error(call, sandboxed, …)` unless `SafeModeOptions.allowHalt()` | library(sandbox): an allow-list | ISS-2025-0625, ISS-2025-0672. |

## 5. The error-trust model (embedding)

`InferenceLimitException` (budget), `QueryCancelledException` (Stop / thread interrupt),
`DebugController.DebugStopException` (debugger Stop) and `ThreadExitException` (`thread_exit/1`)
are plain Java `RuntimeException`s, **not** `PrologException`s, so untrusted `catch/3` cannot
swallow them. A deep-term `StackOverflowError` inside a bridged built-in is converted to a
catchable `resource_error`. `halt/0,1` surfaces to the embedder as a `PrologException` with
`isHalt()`; the engine itself never calls `System.exit`.
