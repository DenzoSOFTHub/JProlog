# Production readiness program — 4.4.0 → 4.5.0 (2026-09-23)

Status: **DONE — released as 4.5.0** (all seven waves implemented; P1..P6 independently verified,
P7 awaiting the owner's verification; nothing committed). Originally: findings of a five-way audit of 4.4.0 (≈1,300 probes, all reproduced on the
4.4.0 build) turned into implementation waves P1..P7. Each wave is implemented by one agent and
verified independently (full `mvn test`, `./test_all_examples.sh`, the benchmark harness in §9,
and re-running the wave's repros). **Agents never commit.**

Baseline (4.4.0, JDK 25): `mvn test` 1313/1313 in 37 s; examples 20/20 with the documented
successful-query counts; benchmarks in §9.

Reference semantics: **ISO 13211-1 first, SWI-Prolog 9 where ISO is silent** (the project's
stated reference). Every place where this program changes a pinned behaviour is listed in §8
("decisions"); update the pinning tests, do not keep both.

ISS allocation (grep CHANGELOG + `src/` before using; take the next free number inside the wave's
range): P1 0514–0539 · P2 0540–0559 · P3 0560–0589 · P4 0590–0619 · P5 0640–0659 ·
P6 0620–0639 · P7 0660–0679.

General rules for every wave (from CLAUDE.md — read it, and read
`docs/reports/report-engine-v4-progress.md` §3 invariants before touching `core.engine.v4`):
- `START_CHANGE/END_CHANGE` tags with the ISS id; one JUnit test per fix that fails without it
  (in `BugFixVerificationTest` or the relevant `EngineV4*Test`); test classes must match `*Test`.
- Errors are `error(Formal, Context)` built with `core.engine.v4.Errors`; argument faults raise.
- Never `System.out`; never `Term.toString()` for user-visible output (use `core.engine.v4.Writer`).
- Keep the full suite green at the end of the wave; update `docs/tracking/track-issues.md`,
  `track-limitations.md`, and add a wave record section to this file (§10+) with what was done,
  measured numbers, and any deviation from this spec with the reason.
- Do not run `mvn clean` concurrently with anything else; the verifier runs the final numbers.

---

## 1. Wave P1 — engine semantics (core.engine.v4)

| # | Defect (repro → observed / expected) | Location / cause | Fix |
|---|---|---|---|
| P1.1 | **Answers leak later bindings.** `solve("X = f(Y) ; Y = 1")` → answer 1 is `X=f(1),Y=1`; expected `X=f(Y)` (Y unbound). CLI batch shows the same. `(true ; X=1)` shows `X=1` twice. | `Machine.snapshot` (~Machine.java:443) uses `Unify.resolve`, which keeps unbound cells live. | Copy every answer map with ONE shared var-map per answer (fresh variables, sharing preserved). Also for `solveStream` sinks. |
| P1.2 | **Anonymous query variables appear in answers**: `data(a,_)` → `{_G1=1}`. | answer construction | Drop `_` and `_`-prefixed-by-parser anonymous vars from the answer map (keep `_Foo` named vars — SWI hides them only in toplevel printing; keep them in the map). Remove the 3 test workarounds (`EngineV4IoTest:168`, `EngineV4ModulesTest:67`, `EngineV4TermTest:126`) and fix `AnonymousVariableTest:123`. |
| P1.3 | **Cut in `catch/3` Recovery is not local.** `(X=1;X=2), catch(throw(x),x,!)` → only X=1; expected X=1;X=2. `r(X) :- catch(throw(x),x,(X=1,!)). r(2).` → only X=1. | `Machine.java:~1408` installs recovery with `top.cutBarrier`. | Recovery gets a fresh barrier = `cps.size()` after popping the CATCH frame (Recovery is `call(R)`, ISO 7.8.9). |
| P1.4 | **Variable goal bound to `!` is not opaque.** `G = !, (X=1;X=2), G` → only X=1; `t6(X) :- G=(!,fail), (G ; X=alt).` fails, expected X=alt. | drive loop derefs a body variable and runs it with the clause barrier (~Machine.java:485/526). | A goal whose skeleton position was a variable runs as `call(G)` (fresh barrier). Compile-time: mark variable goals in the clause skeleton; runtime: when a body goal derefs from a Variable, wrap. |
| P1.5 | **`call/1` body not checked before running.** `call((write(a),nl,1))` prints `a` then errors; expected `type_error(callable,(write(a),nl,1))` before any output. `call((fail,1))` must raise the same (SWI does). | lazy check ~Machine.java:496. | Validate the whole control-construct body (`,`/`;`/`->`/`*->`/`\+` spine) of `call/N`, `findall`, `\+`, `catch` goal, etc. once when the goal is converted to a body; culprit = the whole goal. Update `EngineV4IsoErrorsTest` deviation 4 (it claims SWI fails — wrong). |
| P1.6 | **`setof/3` dedup before witness unification.** `setof(X, member(X,[Y,Y]), L)` → `[Y,Y]`; expected `[Y]`. | `NativeControl.java:~213` sorts copies first. | Per ISO 8.10.3.4: bagof groups (witness unified), THEN sort/dedup the group's instantiated list. |
| P1.7 | **`bagof/3` grouping is O(W²)** in distinct witnesses (10k → 3.7 s, 20k → 11.5 s, 40k > 7 min). | `NativeControl.java:181-199` pairwise `subsumes`. | SWI algorithm: build `Witness-Template` pairs, keysort by witness (standard order), group adjacent variant witnesses; groups with non-ground witnesses handled by the variant/unify step on the sorted neighbours. Lazy: do not sortDedup groups never enumerated. Target: 40k witnesses < 0.5 s. |
| P1.8 | **`between/3` overflows at 2^63.** `between(9223372036854775806, 9223372036854775807, X)` wraps and never stops; `between(9223372036854775807, inf, X)` wraps. Bigint bounds fall to eager legacy `builtin/arithmetic/Between.java` → OOM after 17 s. | `Machine.java:~1144/1168`. | Stop at `Long.MAX_VALUE` exactly; `inf` continues into BigInteger lazily; bigint bounds handled natively and lazily (remove the eager path). |
| P1.9 | **`aggregate_all/3` SWI forms wrong.** `aggregate_all(max(X,W), member(X-W,[1-a,3-b]), M)` → a list; expected `max(3,b)`. `sum(X*2)`, `max(X+1)` raise type_error; expected evaluated. Unknown/unbound spec silently becomes bag; expected error. count/sum/max/min build the full list. | `NativeControl.java:~296/299`. | Implement `count`, `count(T)`, `sum(E)`, `max(E)`, `min(E)`, `max(E,W)`, `min(E,W)`, `bag(T)`, `set(T)` per SWI; evaluate expressions; accumulate in O(1) memory for count/sum/max/min; `max` of empty → fail; `sum` of empty → 0; bad spec → `domain_error(aggregate_spec, S)`; unbound → instantiation_error. |
| P1.10 | **Cleanup not run when a query is abandoned**: `solveStream` sink returning false after the first answer of `setup_call_cleanup(true, member(X,[1,2,3]), writeln(c))` never prints `c`; the same on inference-budget abort and on cancellation (stream stays open after `setup_call_cleanup(open(F,write,S), loop, close(S))` hits the budget). | `Prolog.solveStreamWithV4Engine`, Machine query teardown. | On early stop, exception exit, budget and cancel: cut the query's choice points so CLEANUP frames run (cleanup exceptions are swallowed/logged in this teardown — the primary control exception must still propagate; the budget must not be charged for the cleanup, or give it a small fresh allowance). |
| P1.11 | **Deep terms silently mis-copied.** Beyond `MAX_ARG_DEPTH=2000` in a non-last argument, `copy_term`, `findall` results and `assertz` share or keep bound cells: `mkv(3000,V,T), copy_term(T,C), C == T` succeeds; asserted clause loses a binding at depth 2002. | `Unify.java:351-356, :425`. | Make `resolve`/`copy` fully iterative (explicit stack) with no depth cutoff — never return a partially copied term. Test at depth 1e5 and 1e6 (left-nested `+`). |
| P1.12 | **Concurrent `assertz` duplicates clauses** (4 Java threads × 300 distinct asserts → 1201–1202 clauses in 10/10 runs; 4 `thread_create` workers × 200 → 801–803, duplicate `item(3174)`). | `ClauseStore.assertRule` (~:95-111) calls `kb.addRule` outside the predicate lock; a concurrent `sync()` (~:235-255) rebuilds from the KB (already holding the clause) and the assert inserts it again. | Make KB-add + store-append one atomic step under the predicate lock (or have `sync` and `assertRule` agree on the version). Same review for `asserta`, `retract`, `retractall`, `abolish`. Stress test: 8 threads × 2000 asserts/retracts, exact counts, 20 repetitions. |
| P1.13 | **`nb_setval/2` stores live variables**: `nb_setval(k,f(X)), (X=1;X=2)`, then `nb_getval(k,V)` → `f(2)`; expected `f(_)`. | `NativeDb.java:~328` stores `m.resolve(...)` without copying. | Store a copy (`copy_term` semantics). `b_setval` keeps its trailing semantics. |
| P1.14 | **Asserting a cyclic term** stores `f(_)` silently (`X=f(X), assertz(cyc(X))`, then `cyc(f(f(f(a))))` succeeds). | clause compilation | Raise `representation_error(cyclic_term)` (SWI: cannot assert cyclic terms) — or support it; raising is the requirement. Same for `recordz`, `nb_setval` if they compile/copy. |
| P1.15 | **Trace port depth after catch**: in `w(X) :- catch(thr,_,X=caught)` the recovery `X=caught` is traced at depth 2 (inside `thr`), expected 1; `thr` shows no Exception/Fail port. | `Machine` recovery path | Restore `portDepth` to the catch goal's depth; emit an `Exception` port line (or `Fail`) for the goal being unwound. Update `EngineV4TraceTest` oracles `Trace_CatchThrow` deliberately. |
| P1.16 | **Debugger with any breakpoint is quadratic**: with one unrelated breakpoint, `len/2` over 40k list takes 47.9 s (21 ms without). | `DebugController.needsGoalSnapshot()` true whenever any breakpoint exists → `Unify.resolve(goal)` per port. | Resolve the goal only when a breakpoint matches the goal's indicator (check indicator first, cheap), or when a listener actually needs the snapshot (paused / stepping). Test: counter-based (number of resolves) not timing. |

## 2. Wave P2 — performance (call path, clause store, database)

Target (benchmark harness §9, warm run, same machine): **nrev ≥ +35 %, loop ≥ +30 %, deriv ≥ +25 %**
over the 4.4.0 baseline, and every item below linear / n log n. No semantic change; the full
suite and `EngineV4IndexingTest`'s randomised oracle stay green.

Profile of 4.4.0 (JFR, nrev+loop+deriv): `Clause.instantiate` 20 % self; **per-call overhead ≈30 %**:
`ClauseStore.lookup` 16 % incl. (ConcurrentHashMap/HashMap/String.equals), `Predicate.sync`
10 % incl. of which `KnowledgeBase.getPredicateVersion` 7.6 % — it builds `functor + "/" + arity`
**per call**; plus `isTabled` 1 %, `BuiltInRegistry.isBuiltIn` 1 %, `debugPortsActive` 2 %,
`BuiltinTable.lookup` 2.4 %, `Collections.unmodifiableList` 1.2 %.

| # | Item | Fix |
|---|---|---|
| P2.1 | Per-call predicate resolution | Call-site caching: the compiled body goal (skeleton) carries a resolved `Predicate` handle (or a per-functor `PredicateRef` object interned once per name/arity), invalidated by a global generation counter bumped on any KB structure change (new predicate, abolish, module change, consult). No string building or hashing on the hot path. |
| P2.2 | `Predicate.sync` / `getPredicateVersion` | The KB `PredEntry` object is referenced directly from the `Predicate` (no map lookup), version read as a plain/volatile field. Never build `name + "/" + arity` on a call. |
| P2.3 | Per-call flag checks | `isTabled`, `isBuiltIn`, native-table lookup, debug-port checks: cache on the `Predicate`/call-site with the same generation invalidation; hoist `debugPortsActive` to a per-drive boolean refreshed when the controller changes. |
| P2.4 | `Clause.instantiate` | Reduce allocation: avoid copying ground subterms (precompute groundness per skeleton subterm and share ground subterms), avoid `getArguments()` list wrappers (`Collections.unmodifiableList` per call) — expose arrays internally. |
| P2.5 | **`retractall/1` quadratic** — 25k 174 ms, 50k 614 ms, 100k 3.5 s. | `KnowledgeBase.retractAllClauses` / `removeOneOccurrence` front scan per clause → single pass removal (mark + compact) — O(N). |
| P2.6 | **`retract(p(_))` of the first clause in a loop** 1e5 → 13.3 s. | `ClauseStore.select(null)` → `all()` copies the whole array after every write (~ClauseStore:339-362). Keep a live array with generation filtering (logical update view via birth/death generations, which already exist) — no copy per call; compact lazily when dead > live/2. |
| P2.7 | **`asserta` in a loop** 1e5 → 5.1 s. | New array per call (~ClauseStore:283) + `rules.add(0)` in the KB global list (KB:442/508). Use a deque / gap buffer for the store, and drop or restructure the KB's global `rules` list (per-predicate lists only). |
| P2.8 | **`retract` with bound key** ~48 µs each in a 200k-clause KB. | `KnowledgeBase.retract` scans the global rules list by identity (:469) then `rules.remove(i)` → per-predicate structures, O(1)/O(log n) removal (tombstone + compaction). |
| P2.9 | **Assert interleaved with indexed calls when the predicate has a variable-headed clause**: 1e4 0.43 s, next 3e4 5.5 s (74 ms without the var clause). | every write invalidates all buckets; merge rescans the array (~ClauseStore:378-395). Maintain the var-headed clause list incrementally and merge lazily per bucket with source-order positions (clause ordinal), appending instead of rebuilding. |
| P2.10 | **native `append(_, [Last], L)` quadratic**: 1e4 8 s, 3e4 83 s (user `app/3`: 1e4 in 0.125 s). | `NativeLibrary.java:286-310` builds a fresh n-element prefix per split. Enumerate by extending a shared prefix incrementally (O(1) per solution amortised) or fall back to the prelude clause definition for the (-,-,+) mode. |
| P2.11 | `predsort/3` 1e5 → 7.6 s; `maplist`/`foldl`/`include` ≈5 µs per element. | predsort: merge sort calling the comparator via a reusable sub-machine call path without per-call query setup. maplist family: native iteration without per-element meta-call setup where possible (keep ports). |
| P2.12 | `LOGGER.fine("Rule added: " + rule)` (KnowledgeBase:~197) eagerly calls recursive `Rule.toString()` per clause; crashes consult with raw `StackOverflowError` on an 8000-deep fact. | Guard every string-building log call with `LOGGER.isLoggable(Level.FINE)` across `core/` and `builtin/` (grep `LOGGER.fine(` / `LOGGER.log(Level.FINE` with `+`). |
| P2.13 | `get_char` over 1.6 MB → 3.4 s. | Buffered char reading in `PrologStream`; avoid per-char object/term churn (cache one-char atoms). |
| P2.14 | `.jpc` load is SLOWER than consult (40k clauses: consult 84 ms, jpc 125 ms) and the file is 24 % larger than the source. | Profile `JpcReader`; the format must be faster than parsing or the feature documented as a cache only. Target: jpc load ≤ 50 % of consult time. |

## 3. Wave P3 — loading, reading and writing terms

| # | Defect | Fix |
|---|---|---|
| P3.1 | **No file loading from Prolog**: `consult/1`, `[F]` (`'.'/2` list-consult), `ensure_loaded/1`, `load_files/2`, `include/1` do not exist; `:- ensure_loaded(F)` is a silent no-op (Prolog.java:~530). A program cannot load another file. | Implement `consult/1` (atom, list, `library(X)` → prelude/known libs, relative paths resolved against the loading file's directory, `.pl` extension added if missing), `[F|Fs]`, `ensure_loaded/1` (load once, by absolute path), `load_files/2` (options `if(changed/true/not_loaded)`, `must_be_module`, ignore unknown), `:- include(F)` textual inclusion, `make/0` optional. Directive and goal forms. Must be denied in safe mode (host file access) — put the implementation in a denied package or add an explicit deny. `source_file/1,2`, `prolog_load_context/2` (`directory`, `file`, `source`, `module`) minimal. |
| P3.2 | **`:- module/2` never goes out of scope**: consult module `m1` then `m2` → `p1(X)` existence_error; module file then a plain file → plain clauses go INTO the module; a module file ending with `:- module(user,[])` never imports exports into user. Every `EngineV4ModulesTest` test appends `:- module(user, []).` to hide this. | `Machine.java:~353` uses `Modules.currentModule()` for top-level queries; nothing resets it after a consult. | The load context module is per load (a stack); after a module file finishes, the current/type-in module is `user` again and the module's exports are imported into the module that loaded it (user). Remove the workaround lines from the tests. |
| P3.3 | **`table` directive forms ignored** → programs hang: `:- table ev/1, od/1.`, `:- table([a/1,b/1])`, mode-directed `:- table sp(_,_,min).` | `Prolog.processTableDirective` (~:789), `builtin/meta/TableDirective` accept only `Name/Arity`. | Accept comma lists, lists, and mode-directed specs. Mode-directed tabling (`min`, `max`, `first`, `last`, `lattice(PI)`, `po(PI)`, `-`/`index`): implement `min`/`max`/`first`/`last`/`-` at least (answer subsumption on the moded args); anything unsupported must raise `domain_error(table_mode, M)`, never hang silently. `predicate_property(P, tabled)` true. |
| P3.4 | **Term readers use the legacy parser**: `read/1,2`, `read_term/2,3` (`builtin/io/Read.java:77`, `ReadTerm.java:110`), `term_to_atom/2`, `term_string/2`, `atom_to_term/3` (`NativeText.java:855`, `NativeTerm.java:233`). Consequences: `writeq` output doesn't read back (`'a''b'`, `f(-)`, `[-]`, `-(-)`, `(a|b)`, `"a""b"`, `0'''`, backquotes, `{}` → `{true}`, `f(===>)` → `f(=(==,>))`), user operators invisible to `term_to_atom`, syntax errors are bare atoms. | All of them on `core.parser.v2` with the engine's `Ops` and flags; syntax errors as `error(syntax_error(Desc), Context)` (and `read_term` honours `syntax_errors(error/fail/quiet)`); `read_term` options: `variables`, `variable_names`, `singletons`, `term_position` minimal, `double_quotes`, `module`. `atom_to_term/3` bindings include `_Y` named vars. The legacy parser stays only behind `-Djprolog.parser=legacy`. |
| P3.5 | **String/atom streams missing**: `open_string/2`, `with_input_from/2`, `read_term_from_atom/3`, `read_line_to_string/2`, `read_line_to_codes/2,3`, `read_string/3,5`, `read_term(S, T, [])` from such a stream. | Implement on `PrologStream` (in-memory reader). |
| P3.6 | **`.jpc` compiler uses the legacy parser** (`Prolog.compile` ~:1807): `r1(a ===> b)`, `'a''b'`, backquotes, `(===>)` fail to compile; `e({})` loads back as `e({true})`; symbol-char `op/3` lost; `:- dynamic` lost. | Compile through the v2 consult path (same directive handling as `consultWithDiagnostics`), record directives (op, dynamic, discontiguous, table, module/use_module, initialization) in the file, and add a consult-vs-jpc equivalence test over every `examples/*.pl`. |
| P3.7 | **`listing/1` not re-readable**: prints `lst(_G27,_G26) :- ;(,(>(...`, `lst(A b, it's).` (Prolog.java:1341-1395 uses `Rule.toString()`). | `portray_clause/1` semantics through `core.engine.v4.Writer`: `quoted(true)`, operators, `A`,`B`.. variable names, `numbervars`, body layout with indentation; `:- dynamic p/1.` header for dynamic predicates. Test: listing output consulted back yields `==`-variant clauses. |
| P3.8 | **Parser nesting limit rejects ordinary code**: `MAX_NESTING = 1000` (TermReader.java:50) counts right-assoc operator chains → a body of ≥1000 goals or long `;` chains raise `resource_error(parser_nesting)`. | Count only real bracket/argument nesting (or make operator-chain parsing iterative). Raise the limit for brackets to something like 100k with an iterative reader, or keep it but not for operator chains. |
| P3.9 | **Writer round-trip defects**: `writeq(-(2^2))` → `-2^2` (reads as `(-2)^2`); also `-(2**2)`, `-(1.5^a)` — the space/paren decision must look at the leftmost token of the operand, not only a direct Number (Writer.java:258-259). `writeq(-(-,-))` → `- - -` (reads as `-(-(-))`), expected `(-)-(-)`; `writeq(1-(-))` → `1- -` expected `1-(-)`: operator atoms as operands must be bracketed. `inf`/`nan` floats print as atoms (use `inf`, `nan` as SWI `1.0Inf`, `1.5NaN` and teach the reader those). `writeq('\x7F\')` emits raw DEL. `'[]'(a,b)` → `[](a,b)`, `'{}'(a,b)` → `{}(a,b)` not readable → quote them. | Fix in `core.engine.v4.Writer`; add a **property test**: random terms (atoms incl. symbol-char and operator atoms, negative numbers, operators of all types, curly, lists, strings) → `writeq` → `read_term` (v2) → `==` variant. Update the wrong pin at `BugFixVerificationTest:3751`. |
| P3.10 | Lexer escapes: `'\e'` → `e` (SWI 27), `'\s'` → `s` (SWI space), `'\z'` accepted (ISO syntax error). | `Lexer.java:344-346`: support `\e`, `\s`, `\z`(error), `\uXXXX`, `\UXXXXXXXX`, `\xHH..\`, octal `\NNN\`; unknown escape → syntax error. |
| P3.11 | `dcg_translate_rule/2` (legacy `builtin/dcg/DCGUtils`) produces wrong clauses (`\+ b` → `\+(b,S0,S1)`, `!` → `!(S0,S1)`, pushback wrong, variables disconnected); `expand_term/2` missing. | Route both through `core.dcg.v2.DCGTranslator`; add `expand_term/2` (DCG + `term_expansion/2` hook if defined), `goal_expansion` optional. |
| P3.12 | Consult of a left-deep 8000-term fact → raw `StackOverflowError` escapes consult. | After P2.12 check again; any remaining recursive walker on the consult path → iterative or converted to `resource_error`. |

## 4. Wave P4 — built-in conformance (arith, text, format, I/O, lists)

| # | Defect | Fix |
|---|---|---|
| P4.1 | `integer/1` truncates (`integer(2.5)`=2, `integer(3.7)`=3); SWI rounds (3, 4). `round(0.49999999999999994)` → 1 (expected 0), `round(4503599627370497.0)` off by one (`Math.floor(x+0.5)`, ArithEvaluator:118). `truncate`/`integer` of big integers go through `doubleValue()` (`truncate(12345678901234567891)` wrong). `(10^400+1)/10^400` → `evaluation_error(undefined)`, expected 1.0 (bigint / bigint → exact BigDecimal-based float). | Fix in `core.arith.v2.ArithEvaluator`; integer args of rounding functions returned unchanged; `round` = half away from zero via `Math.round`-safe algorithm (`Math.rint` + sign correction) and BigDecimal for |x| ≥ 2^52. Update pins `BugFixVerificationTest:1413-1427`. |
| P4.2 | Shifts: `1 << -1`, `8 >> -2` raise `evaluation_error(negative_shift)` (not an ISO error). SWI: 0 and 32. | Negative shift = shift in the other direction. Update pins `BugFix:42,53`. |
| P4.3 | Deep arithmetic: `is/2` on a left-deep expression of 10k terms → stack overflow. | Iterative evaluator (explicit stack) or at least `resource_error` conversion; iterative preferred. |
| P4.4 | `plus(1.5,2.5,X)` → 4.0 (pinned in `AdvancedArithmeticTest:160-172`); SWI: integers only → type_error(integer, 1.5). | Fix + repin. |
| P4.5 | Error context of comparisons: `1 =:= a` reports `is/2`; `X is [1,2]` culprit `[]/0`. | Context = the actual predicate indicator; `[X]` evaluates X (single element list) per SWI, longer lists `type_error(evaluable, '[|]'/2)`. |
| P4.6 | **`format/2,3`**: `~d`/`~D` truncate bigints (`9223372036854775807`, negative garbage); column fill char (``~`-t~30|``) unsupported; `~W` missing; `~+` default width 8 ignored; `~2n` prints 1 newline; `format('~w',[a,b])` doesn't raise (too many args → `format('too many arguments')` error); `~f/~e/~g` with non-number print `0.000000` (expected error); `~c` with `foo`/`-1` succeeds silently; `~a` with compound prints (expected error); `~g` of 0.1 → `0.100000` (C `%g` → `0.1`); `~r` without radix → hex (expected error); `format(codes(C,T),...)`, `format(codes(C),...)`, `format(chars(...))`, `format(string(S),...)` sinks; `~i`, `~*c`, `~e`, `~p`, `~q` complete. | `NativeIo.Fmt` (~530-720). Errors: `error(format(Msg), _)` as SWI. Add a table-driven `EngineV4FormatTest` (≥60 rows: directive, args → output or error). Repin `EngineV4IoTest:202`. |
| P4.7 | Text built-ins: `string_concat(1,2,S)` → `"nullnull"` (NativeText.java:207: `text()` null for numbers); `string_concat(X,Y,Z)` all unbound fails (pinned as deliberate: `BugFix:427`, `EngineV4TextTest:217`, IsoErrors row 362) — must raise instantiation_error; `string_length(123,L)` fails (3); `atom_string(42,S)` / `atom_string(A,42)` fail; `sub_string(abc,...)` fails for atom input; `atomic_list_concat([a,B,c],'-','a-x-c')` fails (SWI B=x); `atomic_list_concat([f(x)],X)` fails (type_error(atomic)), `atomic_list_concat([a,B],X)` fails (instantiation_error), `atomic_list_concat(abc,X)` fails (type_error(list)); `atomic_list_concat(L,'',abc)` → `[a,b,c]` (SWI: domain_error(non_empty_atom,'') — repin `EngineV4TextTest:265`); `string_upper/2`, `string_lower/2` missing; `sub_atom(abc,-1,1,A,S)` raises `type_error(integer,-1)` (should fail — SWI fails for negative B? SWI: fails silently; ISO: domain_error? use SWI); `sub_atom` indexes UTF-16 units vs `atom_length` code points (`sub_atom('a😀b',1,1,_,S)` → lone surrogate) — unify on code points everywhere; `print/1` does not quote (SWI `print` = `portray` + `writeq`) — repin `EngineV4IoTest:103`. | NativeText / NativeIo. |
| P4.8 | `char_type`/`code_type`: `xdigit(W)` missing (NativeChars.java:74-80); `code_type(X, space)` yields 28–31 (`Character.isWhitespace`) — use SWI's `iswspace` set; `code_type(-1, end_of_file)` fails; `prolog_symbol`, `prolog_var_start`, `prolog_atom_start`, `prolog_identifier_continue` missing; unknown type fails silently → `domain_error(char_type, T)`. | NativeChars. |
| P4.9 | `flatten/2` on a 6000-element flat list → stack overflow; > 10000 → `resource_error(cyclic_term)` (NativeText.java:941-966). | Iterative, no depth counter on the tail. |
| P4.10 | List library argument faults: `sum_list([a],S)`, `max_list([1,a],_)` fail (type_error expected; repin `EngineV4RetirementTest:245`); `nth0(-1,[a],X)` fails (SWI: type_error? SWI `nth0(-1,...)` → must_be(nonneg) → type_error(not_less_than_zero,-1)); `length(L,L)` raises `type_error(integer, var)` (SWI: fails? SWI: `length(L,L)` → resource error / loops — use SWI 9 behaviour: it fails... verify semantics: SWI 9.x raises? Pick: fail); `nth0(1,L,x)`, `nth1(2,L,x)`, `last(L,x)` on unbound list fail (SWI extends/enumerates); `permutation/2` order differs from SWI (`[3,2,1]` before `[3,1,2]`) — use SWI's select-based order; `intersection/3`, `union/3` remove duplicates (SWI keeps: `[1,1,2]`, `[1,1,2,3]`; repin `BugFix:280,797,993`). | NativeLibrary / prelude. |
| P4.11 | `tab(1+1)` fails (should evaluate → 2 spaces); `tab(a)` fails (type_error(evaluable,a/0)); `tab(-1)` fails (SWI: succeeds, prints nothing — keep that). Repin `BugFix:1048`, `EngineV4IoTest:142-143`. | NativeIo. |
| P4.12 | Stream conformance: `eof_action(error)` raises at the FIRST read at EOF (ISO: return `end_of_file` first, raise on the next — `past` state); `read/1` never raises; culprit `stream_1001` should be the stream term `'$stream'(1001)`; after close `get_char` raises `domain_error` (ISO `existence_error(stream,S)`); `open(F,read,s)` fails silently (ISO `uninstantiation_error(s)`); `get_char` on binary / `get_byte` on text stream succeed (ISO `permission_error(input, binary_stream, S)` / `text_stream`); `stream_property(S, badprop(x))` fails silently (`domain_error(stream_property, badprop(x))`). | IOStreamUtils / NativeIo / io. |
| P4.13 | `with_output_to/2` and the thread-local capture override an explicit `set_output(S)` to a file: `with_output_to(atom(A),(open(F,write,S),set_output(S),write(x),close(S)))` puts `x` into A. | The capture must redirect only `user_output`/the current output at entry; an explicit `set_output/1` or a write to an explicit stream wins. |
| P4.14 | `print_message(_, format(F,A))` prints the raw term. | Format it; also `message_to_codes`-like basic rendering for `error(...)` terms (SWI-style "Warning:"/"ERROR:" prefixes to user_error). |
| P4.15 | `statistics/2`: `cputime` returns a list (SWI: float seconds); `process_cputime`, `inferences`, `real_time`, `epoch`, `stack`, `localused`... missing; `statistics(runtime,[T|_])` FAILS (partial-list output arg). | Proper keys; unify the output (never test "is var"); `inferences` from the machine's step counter; `statistics/0` summary. |
| P4.16 | `abolish`: calling an abolished dynamic predicate fails silently (should `existence_error` — KB.abolishPredicate :762 leaves `dynamicPredicates`); `abolish(foo/1.5)`, `abolish(foo/100000000000)` succeed (NativeDb.java:197-201 `Math.round`) → type_error(integer) / representation_error(max_arity). Empty dynamic predicates: `predicate_property(z(_), dynamic)`, `current_predicate(z/1)` false after `dynamic(z/1)` (SWI: `predicate_property` true; `current_predicate` true for defined-dynamic). `number_of_clauses(N)` property. `predicate_property(pub(_), P)` returns both `exported` and `undefined`. | NativeDb / KB. |
| P4.17 | `assertz(m3:k(1)), m3:k(X)` → existence_error (but `clause(m3:k(X),true)` finds it). Qualified call to a non-exported predicate silently fails (`RefactorIssuesTest:90`, `AuditRound5Test:252`) — SWI returns the answer. | `M:G` calls resolve in M's own table regardless of export (export only governs import). Repin the two tests (ISS-2025-0314 decision reversed — see §8). |
| P4.18 | `op/3` is undone on backtracking (`RefactorIssuesTest:52`, `EngineV41RetirementTest:123`): `forall(member(O,[zfoo,zbar]), op(700,xfx,O))` defines nothing. ISO/SWI: permanent. | Make `op/3` (and `char_conversion/2`) non-backtrackable. Repin. |

## 5. Wave P5 — CLP(FD)

| # | Defect | Fix |
|---|---|---|
| P5.1 | No propagation after plain `=`: `X in 0..9, Y in 0..9, X+Y #= 9, X = 4` leaves Y unbound (SWI Y=5); `X #= Y+Z, Y=1, Z=2, X == 3` fails; `X in 1..5,Y in 1..5, X #= Y+1, Y=3` → domain {4} but X unbound. | `ClpfdV2Bridge.onBindCell` (:178) narrows but never binds singletons → after propagation to fixpoint, bind every variable whose domain is a singleton (through the trail). |
| P5.2 | `#\=` never removes interior values (`X in 1..5, Y in 1..5, X #\= Y, X #= 2` leaves Y in 1..5). | Domains with holes (interval lists) — `IntervalDomain` must support unions; NE removes the value when the other side is fixed. Also `X in 1..3 \/ 5..7`, `fd_dom/2` printing unions. |
| P5.3 | `label/labeling` computes ALL solutions before the first (ClpfdNative ~199 → `labelCells` then pushes a generator): `once(queens(N))` costs the whole tree; `min/max` options sort the whole list. N-queens first solution: N=8 0.48 s, 10 11 s, 12 > 4 min (plain Prolog 12-queens 0.14 s). | Lazy labeling as a Generator: choose variable (leftmost default per SWI; `ff`, `ffc`, `min`, `max`), branch on value (`up`, `down`, `step`, `enum`, `bisect`), propagate, backtrack through the trail. `min(E)`/`max(E)` via branch and bound. Target: 20-queens first solution < 1 s, 8-queens all 92 solutions < 1 s. |
| P5.4 | Big integers: `X #= 1000000000000*1000000000000` fails (saturating long, Constraint.java:193-196). `X #> Y, Y #> X` with unbounded domains → 29 s then `resource_error(memory)`. | BigInteger-safe bounds (or overflow → promote); detect non-termination of bound propagation on infinite domains: SWI answers with residual constraints immediately for `X #> Y, Y #> X`? (SWI fails quickly because of propagation over sup/inf? SWI: `X #> Y, Y #> X.` → false). Cap the number of propagation rounds on infinite domains and fail/leave residual — must answer in < 100 ms. |
| P5.5 | Missing: `ins/2` (operator exists, predicate missing), `sum/3`, `scalar_product/4`, `#<==>`, `#==>`, `#<==`, `#\/`, `#\` (reification over linear constraints), `//`, `rem`, `mod`, `^`, `abs`, `min`, `max` in expressions, `inf..sup`, `fd_inf/2`, `fd_sup/2`, `fd_size/2`, `fd_dom/2`, `tuples_in/2`, `element/3`, `global_cardinality/2` (basic), `all_distinct/1` stronger than `all_different/1` (at least Régin-lite: pigeonhole/Hall intervals). `label/1` default = leftmost (currently first-fail). | Implement in `builtin.clpfd.v2` + `ClpfdNative`. Tests: SEND+MORE (one solution, < 200 ms), sudoku (a hard one, < 2 s), magic square 3×3, reification counting (`sum` of booleans). |

## 6. Wave P6 — production hardening (sandbox, threads, CLI, budget)

| # | Defect | Fix |
|---|---|---|
| P6.1 | **Safe mode leaks host file access** beyond the documented `open/3`: `csv_write_file`, `csv_read_file` (builtin.csv), `log_to_file` + `log_error` (builtin.logging, via a static JVM-wide `FileHandler` — also redirects every engine's logging). Safe mode never touches the native `BuiltinTable`. The new P3 loaders (`consult/1`...) must be denied too. | Deny `builtin.csv` file predicates, `builtin.logging` file sinks; make the logging handler per-engine; extend `enableSafeMode` to the native table (a deny list of host-touching natives: `open/3,4`, `see/tell` family, `consult`/`load_files`/`ensure_loaded`, `absolute_file_name`, `exists_file`, …) — add `enableSafeMode(SafeModeOptions)` with `allowFileRead(dir)` whitelisting for embedders who need it. A test enumerates EVERY registered predicate (registry + native + prelude) after `enableSafeMode()` and asserts none of an explicit host-touching list survives, plus a positive "safe list" snapshot so a new built-in forces a decision. |
| P6.2 | **Budget/exceptions escape through the concurrent family**: `catch(first_solution(X,[loop],[]), E, true)` catches `'first_solution: ...InferenceLimitException...'`; same for `concurrent/3`, `concurrent_maplist`, `concurrent_and/or`; `catch(concurrent_and([throw(foo)],[]), foo, true)` doesn't catch (ball becomes an atom). `concurrent_maplist` hard 60 s cap (`f.get(DEFAULT_TIMEOUT_MS=60_000)`, ConcurrentPredicates.java:68 → `'concurrent_maplist_2: null'`). | `ConcurrentPredicates.java:107-111`: unwrap `ExecutionException.getCause()`, `rethrowIfControl(cause)`, rethrow a `PrologException` ball unchanged (copied); no fixed timeout (wait interruptibly; cancellation propagates to the workers). |
| P6.3 | Budget per worker = the parent's full limit (thread_create multiplies the budget). The budget counts steps, not work: `length(L,N), fail`, open `append(X,Y,Z), fail`, `member(a,L), fail` do O(N) work per step. | Shared budget (an AtomicLong decremented by all machines of the query); natives that do O(N) work charge the guard per element (or per 1024 elements) — `length`, `append`, `member`, `nth`, `msort`, `atom_codes`, `findall` copying, `copy_term`, `between`. |
| P6.4 | **CLI computes ALL solutions before printing** (PrologCLI.java:120, `prolog.solve`): `between(1,inf,X).` → OutOfMemoryError; side effects of every solution run before the first prompt. The CLI also loads demo facts (`father/2`, `likes/2`, `color/1`...) at startup. | Interactive: `solveStream`, one answer at a time, `;`/Enter protocol. Batch: stream answers as they come, with an optional `--max-solutions N` (default unlimited but streaming). No demo facts by default (`--demo` flag keeps them). CLI arguments: `jprolog file.pl ...` consults files; `-g Goal` runs a goal; `-t Goal` toplevel goal; `--safe` enables safe mode; `--budget N`; exit status (`halt(N)` → exit code; uncaught error in `-g` → exit 1). A runnable fat jar (`mvn package` produces `jprolog.jar` with `Main-Class: it.denzosoft.jprolog.PrologCLI`) and fix the pom's default `exec` mainClass (`it.denzosoft.jprolog.Main` does not exist). |
| P6.5 | Threads: `thread_get_message(Q, b(X))` fails when `a(1)` is at the head — must do a selective receive (scan, block until a unifiable message); `thread_get_message/3` (timeout option) errors; missing `mutex_create/1,2`, `mutex_lock/unlock`, `with_mutex/2`, `message_queue_destroy/1`, `thread_property/2`, `thread_exit/1`, `thread_join/1`, `concurrent_forall/2,3`, `thread_self/1` reporting `main`; duplicate `alias(dup)` accepted (permission_error); thread errors are atoms (`'thread_join: unknown thread alias nosuch'` → `existence_error(thread, nosuch)`); global variables shared across threads (SWI: per-thread — `nb_setval` in a worker invisible to main). | builtin.threading + Workers. |
| P6.6 | `EngineV4StreamsTest:275` joins t1 before starting t2 (not concurrent). | Fix the test to overlap the threads. |

## 7. Wave P7 — test suite as a real safety net + release

1. Remove/replace tautologies: `ISOComplianceTest` `testModuleDeclaration`(30), `testReadTerm`(161), `testWriteTerm`(172), `testFormat`(179), `testCompleteISOFeatureSet`(293), `testOperatorRemoval`(140), `testISOComplianceLevel`(297).
2. Catch-anything tests → exact error term assertions: `RefactorIssuesTest` 90/119/213, `AuditRound5Test` 62/226/252, `EngineHardeningTest:322`, `BugFixVerificationTest` 1457/3082, `ExceptionHandlingTest(builtin):75-87`, `ProductionAuditTest:127`.
3. Existence-only assertions → value assertions (`==` in the query): `MegaPredicateTest` (17 listed: 225, 480, 498, 610, 625, 654, 693, 703, …), `ProductionAuditTest` (msort/sort 25-36), `MetaPredicatesTest` (7; `:85` once must use a nondeterministic goal), `AnonymousVariableTest` (5), `ListBuiltinsTest` 26-31/106-121, `NegationAsFailureTest:107`, `JPrologComprehensiveTest:59,95`; `>= N` where the count is known (`BugFix` 256, 935, 1005, 1228, 1238); `toString()` comparisons (`BugFix:1851`, 17 `contains("1")`).
4. Dead tests: rename `BuiltInTests.java` → delete (duplicates) ; delete `OperatorDefinitionTest`/root `ExceptionHandlingTest`/`Phase1FeaturesTest` parts that drive dead legacy classes (or retarget them at the engine); remove the 5 duplicated "QuerySolver is deleted" checks except one; stale names (`testISS0331_V2EngineDebugPorts`, `testISS0348_…OnDefaultEngine`); `EngineHardeningTest:382` retarget at a still-bridged built-in.
5. `FamousPrologProgramsTest`: real programs with full answers (n-queens all 92 for N=8, Hanoi move list, quicksort, zebra puzzle, SEND+MORE via CLP(FD), Ackermann, primes sieve, DCG expression parser/evaluator).
6. Flakiness: `EngineHardeningTest:583` (50 µs wall-clock) → counter-based; interrupt races (`EngineHardeningTest:479`, `BugFix:2209`, `EngineV4ThreadsTest:249`, `EngineV4TablingTest:250`) → latch-synchronised; `AuditRound5Test:44` GC dependence → remove; operator-table mutation in `OperatorDefinitionTest`/`Phase1FeaturesTest` → per-engine; `RefactorIssuesTest:119` stream leak.
7. `test/performance/`: a `PerformanceRegressionTest` with **counter-based** or generous-ratio checks for every P2 item (e.g. retractall of 2N clauses takes < 4× the time of N; bagof with 40k witnesses < 2 s absolute; assert/retract interleave linear) — ratio tests with warm-up, no tight absolute bounds.
8. Trace oracles: `EngineV4TraceTest` 257/282 (phantom Fail after Exit on deterministic once/ignore/forall), `Trace_BacktrackRedoFail` (missing `Fail: (0) q(X)`), `Trace_Maplist` (Fail after top-level Exit, leaked `$mctx` goal), `Trace_CatchThrow` (see P1.15) — fix engine + oracles deliberately.
9. One place for deliberate deviations: `docs/references/ref-deviations.md` (B.17 + IsoErrors class comment + this program's decisions), linked from README/CLAUDE.md; fix factual errors (ISO §9.1.6.5 for `integer/1`; "as SWI does" for `call((fail,1))`).
10. Release 4.5.0: CHANGELOG, track-release-notes, README (feature list, CLI usage, jar), BUILTIN_PREDICATES_REFERENCE + manual (`tools/build-manual.sh`), version bump in pom.xml, CLAUDE.md numbers.

## 8. Decisions taken for this program (SWI-Prolog as the reference)

| Behaviour | 4.4.0 | 4.5.0 |
|---|---|---|
| `op/3` on backtracking | undone | permanent (ISO/SWI) |
| `integer/1` evaluable | truncates | rounds (SWI) |
| negative shift | `evaluation_error(negative_shift)` | shift the other way (SWI) |
| `intersection/3`, `union/3` | dedup | keep duplicates (SWI library(lists)) |
| `string_concat(-,-,-)` | fails | instantiation_error |
| `call((fail,1))` | fails | type_error(callable, (fail,1)) |
| `M:G` for non-exported G | fails silently | runs G in M (SWI) |
| `print/1` | unquoted | `portray` + `writeq` (SWI) |
| `tab(Expr)` | fails on non-integer | evaluates; `type_error` on non-evaluable |
| `format ~r` no radix | hex | error |
| `plus/3` floats | accepted | type_error(integer) |
| `atomic_list_concat(L,'',A)` split | splits into chars | domain_error(non_empty_atom,'') |
| global variables across threads | shared | per-thread (SWI) |
| CLI demo facts | loaded | only with `--demo` |
| consulted (static) predicates modifiable by assert/retract | yes | **unchanged** (too disruptive; documented in ref-deviations.md) |
| tabled non-stratified negation (`p :- \+ p` tabled) | inconsistent answer | **raise** `permission_error`/unsupported for `\+` over an incomplete table in the same SCC (WFS not implemented — LIM entry) |

## 9. Benchmark harness and baseline (4.4.0)

Harness: scratchpad `bench/bench.pl` + `Bench.java` (consult, run 3×, report warm run). Programs:
nrev30 ×20000; fib(23); loop(3,000,000); tak(18,12,6); queens(8) via permutation; assert 200k +
indexed lookup; findall 500k + msort; atom_number/atom_concat 200k; deriv ×100k.

| Benchmark (warm) | 4.4.0 |
|---|---|
| nrev30 ×20000 (≈9.9 M LI) | 3.45–3.70 s (≈2.8 MLIPS) |
| fib(23) | 0.065 s |
| loop(3M) | 1.28–1.69 s |
| tak(18,12,6) | 0.035 s |
| queens(8) permutation | 0.020 s |
| assert 200k + lookup (after retractall of 200k) | **7.6 s** (0.41 s cold on an empty predicate) |
| findall 500k + msort | 0.09 s |
| atom ops 200k | 0.11 s |
| deriv ×100k | 1.02 s |
| retractall 25k / 50k / 100k | 174 / 614 / **3525 ms** |

## 10+. Wave records

(Each wave appends its record here: what was done, ISS ids, deviations from this spec and why,
measured numbers before/after, open items.)

## 10. Wave P1 — record

Status: **DONE — awaiting independent verification** (2026-09-23; resumed after two crashed
sessions, whose uncommitted work was reviewed item by item, corrected where needed and finished).
Test class: `core/engine/v4/EngineV45SemanticsTest` (one method per item, named after its ISS id).
All 16 methods were run against the 4.4.0 classes (a clean clone of `v4.4.0` built in the
scratchpad): **16/16 fail there**, 16/16 pass on this tree.

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| P1.1 answers are copies | ISS-2025-0514 | done | testISS0514_AnswersDoNotLeakLaterBindings | `Machine.answer` → `Unify.copyAnswer` (one var map per answer, fresh cells keep the print name, attributed cells stay live for `residualGoals`); `solveStream` too. Nested sub-queries keep the non-copying snapshot (the bridge maps them back onto caller cells). |
| P1.2 anonymous vars dropped | ISS-2025-0515 | done | testISS0515_AnonymousVariablesNotInAnswers | `_Foo` kept. Workarounds removed from EngineV4IoTest/ModulesTest/TermTest; AnonymousVariableTest now asserts the exact key set. |
| P1.3 cut in Recovery local | ISS-2025-0516 | done | testISS0516_CutInCatchRecoveryIsLocal | Recovery pushed as `call(R)` with barrier `cps.size()`. |
| P1.4 variable goal opaque | ISS-2025-0517 | done | testISS0517_VariableGoalBoundToCutIsOpaque | Runtime: a goal whose skeleton is a VarRef/Variable and derefs to `!` or a control construct is re-pushed with a fresh barrier. |
| P1.5 body checked before run | ISS-2025-0518 | done | testISS0518_CallBodyCheckedBeforeRunning | Spine walk of `,`/`;`/`->`/`*->`/`\+` for call/N, the query, catch, findall/3,4, \+, once, ignore, forall, bagof/setof, aggregate_all. A goal that IS `\+ G` is not rejected as a whole — `\+/1` checks G itself (SWI: culprit `(fail;1)` for `\+ (fail;1)`). EngineV4IsoErrorsTest deviation 4 removed, row now expects the type_error. Error context stays the atom `'call/1'` (codebase convention), the test checks the formal. |
| P1.6 setof dedup after witness | ISS-2025-0519 | done | testISS0519_SetofDedupsAfterWitnessUnification | Test expectation fixed: the answer is `L = [Y]` (the query variable), as in SWI. |
| P1.7 bagof O(n log n) | ISS-2025-0520 | done | testISS0520_BagofGroupingIsNotQuadratic | Variant keys + stable sort; lazy per-group sortDedup. Test fixed: the old "variant" row used anonymous variables whose witnesses are NOT variants (ISO 8.10.2.4 → two groups); replaced by fact-based rows. |
| P1.8 between/3 exact, lazy | ISS-2025-0521 | done | testISS0521_BetweenStopsAtLongMaxAndHandlesBigints | All modes native; long counter stops exactly at its last value, continues into BigInteger for `inf`/bigint bounds; bound 3rd arg is a range test. The legacy eager `builtin/arithmetic/Between` is no longer reached from the machine (class kept, registry entry keeps permission_error). |
| P1.9 aggregate_all SWI forms | ISS-2025-0522 | done | testISS0522_AggregateAllSwiForms | O(1) accumulation via `Machine.forEachSolution`. |
| P1.10 cleanup on abandon | ISS-2025-0523 | done | testISS0523_CleanupRunsWhenQueryIsAbandoned | `Machine.cutQuietly` on sink stop / control-exception exit (top level and nested drives); cleanup exceptions swallowed; `ResourceGuard` grants a 100k-step teardown allowance. |
| P1.11 deep-term copy | ISS-2025-0524 | done | testISS0524_DeepTermsCopiedCompletely | `Unify.deepWalk` (explicit stack) past the recursion cap for resolve/copy. Also found on the assert path and fixed: `Clause.toSkeleton`/`instantiate` (iterative `mapDeep` past depth 1000), `CompoundTerm.isGround` (fully iterative), and `KnowledgeBase` rendering EVERY asserted clause with `toString()` for a `LOGGER.fine` (now guarded by `isLoggable`). Test covers copy_term and findall at 1e5 and 1e6 (left-nested `+`), assertz+call at 1e5 (1e6 checked by hand: 3.9 s, kept out of the suite for its run time). Test helper `innermost/2` fixed (it looped on the unbound leaf). |
| P1.12 concurrent assert | ISS-2025-0525 | done | testISS0525_ConcurrentAssertsAreNotDuplicated | KB write + store insert under the predicate lock; atomic generation counter; exact kbVersion adoption. 8 threads x 2000, 20 reps, plus thread_create. |
| P1.13 nb_setval copies | ISS-2025-0526 | done | testISS0526_NbSetvalStoresACopy | |
| P1.14 cyclic assert raises | ISS-2025-0527 | done | testISS0527_AssertingCyclicTermRaises | `Unify.copyAcyclic`; assert and nb_setval raise `representation_error(cyclic_term)`. recordz/3 does not exist in JProlog (N/A). |
| P1.15 trace depth after catch | ISS-2025-0528 | done | testISS0528_TraceDepthAfterCatch | `EngineV4TraceTest.testISS0481_Trace_CatchThrow` oracle updated deliberately (adds `Exception: (0) boom`). |
| P1.16 breakpoint not quadratic | ISS-2025-0529 | done | testISS0529_DebuggerBreakpointDoesNotSnapshotEveryPort | Counter-based test (`DebugController.getGoalSnapshotCount`). |

**Results.** `mvn -o clean compile` OK; `mvn -o test -DargLine="-Xmx1g"`: **1329/1329** (the 1313
baseline + 16 new), 0 failures, 0 errors, 1 min 57 s. `./test_all_examples.sh`: 20/20 PASSED,
successful-query counts **2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0** (= baseline).

Pinned behaviours changed deliberately (tests updated, not duplicated):
`EngineV4IsoErrorsTest` row `call((fail, 1))` (P1.5, deviation 4 removed);
`EngineV4TraceTest.testISS0481_Trace_CatchThrow` (P1.15, adds the Exception port);
`BugFixVerificationTest.testISS0413_AggregateAllMaxMinTypeErrorOnNonNumber`,
`testISS0414_AggregateAllSumExactAndTyped` and `EngineV4LibraryTest.testISS0452_AggregateAllForms`
(P1.9: `type_error(evaluable, a/0)` instead of `type_error(number, _)`; `max(V-W)` → `max(V, W)`);
`AnonymousVariableTest` and the three `_`-skip workarounds (P1.2).

**Measurements** (same machine, same session, 4.4.0 clone vs this tree; the machine was shared
with other heavy processes, so absolute numbers run ~2x the §9 table):

| Measure | 4.4.0 | P1 |
|---|---|---|
| bagof, 10 000 distinct witnesses | 7.56 s | 0.20 s |
| bagof, 20 000 | 60.5 s | 0.22 s |
| bagof, 40 000 | > 7 min (audit; not re-run) | 0.21–0.31 s (setof 0.32 s) |
| `len/2` over 40 000 with one unrelated breakpoint (P1.16) | 171 s (10 000: 7.3 s) | 0.59 s warm (0.39 s with no breakpoint) |
| `loop(3M)` interleaved A/B (2 pairs x 2 runs) | 3.09–3.94 s | 2.44–3.25 s |
| `nrev30 x5000` interleaved | 2.15–2.80 s | 1.87–2.13 s |
| full §9 harness run 3 (single pass each) | nrev 7.19, fib 0.128, loop 2.79, tak 0.065, queens 0.051, assert 9.78, findall 0.30, atom 0.138, deriv 2.75 | nrev 6.21, fib 0.137, loop 3.07, tak 0.059, queens 0.044, assert 9.74, findall 0.69, atom 0.141, deriv 2.66 |

No benchmark regression beyond the machine's noise (findall 500k re-run 4x each: 0.40–0.79 s vs
0.57–0.90 s). The per-goal cost added to the drive loop (P1.4: one `instanceof` pair on the raw
goal) does not show.

Deviations / notes:
- P1.5: the error context stays the atom `'call/1'` (the whole codebase builds contexts that way;
  changing it is not a P1 item). A goal that IS `\+ G` is not rejected as a whole by the outer
  check — `\+/1` checks `G` and reports `G` as the culprit, as SWI does.
- P1.8: the legacy `builtin/arithmetic/Between` class is still registered (its registry entry is
  what gives `between/3` its permission_error on assert) but the machine never dispatches to it.
- P1.11: fixing copy/resolve alone was not enough for `assertz` — three more recursive walkers
  on the assert path were made deep-safe (see the table), including a `LOGGER.fine` that
  rendered every asserted clause to a String (also a P2-relevant cost on every assert).
- P1.14: `recordz/3` does not exist in JProlog, so only assert and `nb_setval` needed it. New
  limitation LIM-040 records that cyclic terms cannot be stored.
- Test corrections made while finishing the crashed sessions' class: P1.6 expected `[_]` where
  the right answer is `[Y]`; P1.7's "variant witnesses" row used anonymous variables whose
  witnesses are not variants (ISO 8.10.2.4: two groups) — replaced by fact-based rows; P1.11's
  `innermost/2` helper looped on the unbound leaf and the final check used `\=` where `\==` was
  meant, and the huge terms were query (answer) variables — the checks now live in clauses;
  P1.5's `call((true ; _))` enumerates into the instantiation error on backtracking — now `once/1`.

Out-of-scope finding (for P4): `statistics(walltime, [T0,_])` fails — the legacy `Statistics`
built-in returns a pair that does not unify with a two-element list.

## 11. Wave P5 — record

Status: **DONE** (implementation + verification in a private clone of 4.4.0 / 59f9a0b; not
committed). ISS-2025-0640..0652. Three sessions: the first two crashed out of memory with
~2500 lines of unverified work; the third assessed, fixed and finished it.

### What was done

| Item | ISS | Status | Test (all in `builtin/clpfd/v2/ClpfdV45Test` unless noted) |
|---|---|---|---|
| P5.1 propagation after plain `=` | 0640 | done | `testISS0640_PlainUnificationPropagatesAndBindsSingletons` |
| P5.2 domains with holes, `#\=` interior removal, unions, `fd_dom` printing | 0641 | done | `testISS0641_DisequalityRemovesInteriorValues`, `testISS0641_DomainsWithHolesAndUnions` |
| P5.3 lazy labeling (Generator), SWI options, `label/1` leftmost | 0642 | done | `testISS0642_TwentyQueensFirstSolutionIsLazy`, `testISS0642_EightQueensAllSolutions`, `testISS0642_LabelingOptions` |
| P5.3 `min(E)`/`max(E)` branch and bound | 0643 | done | `testISS0643_BranchAndBoundObjectiveOrder` |
| P5.4 big integers, `inf..sup` | 0644 | done | `testISS0644_BigIntegerArithmetic` |
| P5.4 `X #> Y, Y #> X` fails fast | 0645 | done | `testISS0645_CyclicInequalitiesFailFast` |
| P5.5 `ins/2`, `sum/3`, `scalar_product/4` | 0646 | done | `testISS0646_InsSumScalarProduct`, `..._SendMoreMoneyUniqueAndFast`, `..._MagicSquare3x3` |
| P5.5 reification `#<==> #==> #<== #\/ #/\ #\` | 0647 | done | `testISS0647_ReificationAndConnectives` |
| P5.5 `// div rem mod ^ abs min max` | 0648 | done | `testISS0648_ExpressionFunctions` |
| P5.5 `fd_inf/2 fd_sup/2 fd_size/2 fd_dom/2 fd_var/1` | 0649 | done | `testISS0649_DomainReflection` |
| P5.5 `element/3 tuples_in/2 global_cardinality/2` (+ `transpose/2`) | 0650 | done | `testISS0650_ElementTuplesGcc` |
| P5.5 `all_distinct/1` domain-consistent (Régin) | 0651 | done | `testISS0651_AllDistinctIsStrongerThanAllDifferent`, `testISS0651_HardSudoku` |
| P5.5 reification operators (SWI priorities) | 0652 | done | `testISS0652_ReificationOperatorsParse` |

Where: `builtin.clpfd.v2` (`IntervalDomain` interval lists + `inf`/`sup` + exact out-of-range
values; `ClpStore` per-node hot path, `exactValue`, `assignBig`, guard poll hook; `Constraint`
LinearNE/ArithFn/Reified/Bool/InDomain/element/tuples/gcc/Régin, BigInteger constants,
`solveFor`; `ClpfdV2Bridge` domain parsing, expression compiler, negative-cycle check, branch and
bound), `core.engine.v4.ClpfdNative` (every CLP(FD) predicate native, lazy labeling generators),
`Coroutining` (the unify hook binds determined cells), `Prelude` + new
`src/main/resources/prelude/clpfd.pl` (module `clpfd`: sum/3, scalar_product/4, element/3,
tuples_in/2, global_cardinality/2, transpose/2 — a user definition wins), `OperatorTable`.

Session-3 fixes on top of the inherited work: the negative-cycle check ran AFTER propagation (so
the finite `0..10^9` case still crawled into `resource_error(memory)`) — it now runs before the
post, and is incremental (single-source search from the new edge; a 3000-variable `#<` chain
posts in 6.6 s, 4.4.0: 48 s); a big value bound to an FD variable is recorded exactly and
`solveFor`/`exactValue` bind the last variable of a functional constraint exactly (`X #= Y*Z,
Y = Z = 10^12`, `X #= Y+1, Y = 2^63-1`, `X #= Y^70, Y = 2`) instead of leaving it unbound or
raising `representation_error`; SWI's `nonrepeating_labeling_options` /
`consistent_labeling_options` errors; `EnumGen` uses `unifyOrUndo` (generator invariant); the
prelude helpers are `'$'`-private (`EngineV4ModulesTest.testISS0467` requires it).

### Tests changed on purpose (deviations from 4.4.0 pins)
- `BugFixVerificationTest.testISS0263_clpfdHugeDomainRejected` and
  `ClpfdV2EngineTest.labelHugeDomainRaisesResourceError` (renamed `labelHugeDomainIsLazy`): a
  huge domain used to raise `resource_error` because labeling materialised it; lazy labeling
  (the P5.3 fix) hands out the first value at once. Re-pinned to that.
- Four expectations in the inherited `ClpfdV45Test` were not SWI's and were corrected:
  `labeling([max], ...)` selects the VARIABLE with the largest upper bound (not a value order);
  `min(X*Y)` ties come out in labeling order (`X = 1, Y = 5` first); `X in 0..10, X rem 4 #= 1`
  has 3 solutions; a bogus `atom_to_term/3` call in the 8-queens test.

### Measured (JDK 25, `-Xmx1g`, machine under load average ~14; warm = second run in the JVM)

| Target (spec) | 4.4.0 | 4.5 P5 |
|---|---|---|
| 20-queens first solution < 1 s | (8: 0.48 s, 10: 11–17 s, 12: > 4 min) | `labeling([ff])` 0.38 s cold / 0.02–0.16 s warm; `label/1` (leftmost) 2.2–4.5 s |
| all 92 8-queens < 1 s | 1.37–2.95 s | 0.02–0.15 s (`label`), 0.05–0.12 s (`ff`) |
| SEND+MORE < 200 ms | n/a (no `ins/2`) | 19–113 ms cold, 2–22 ms warm |
| hard sudoku (Inkala) < 2 s | n/a | 0.26–0.42 s cold, 0.08–0.29 s warm |
| `X #> Y, Y #> X` < 100 ms | 42.9 s then `resource_error(memory)` | 0–11 ms warm (first query of a JVM ≈ 0.4 s, class loading) |
| same over `0..10^9` | crawl → `resource_error(memory)` | 3–14 ms |

### Suite
`mvn -o clean test -DargLine="-Xmx1g"`: **1332 run, 1330 pass** = 1313 + 19 new
(`ClpfdV45Test`). The 2 failures (`EngineV41RetirementTest.testISS0495_ThreadSelfReportsMainOnTheMainThread`,
`EngineV4RetirementTest.testISS0487_MainThreadOwnsAMessageQueue`) are **pre-existing and
order-dependent**: a clean clone of 4.4.0 (59f9a0b) in the same `/tmp` filesystem fails the same
two with 1313 run (the `main` thread alias is held by a live thread from an earlier class); both
pass when run on their own. They belong to P6/P7 (threads / suite hygiene), not P5.
Examples 01–20: consulted through `PrologCLI --batch` on 4.4.0 and on this tree — output
identical for all 20 (`test_all_examples.sh` is not in the clone; the verifier runs it).

### Open items / doubts
- LIM-041 (new): 64-bit domains (big values are exact only when a functional constraint
  determines them; big coefficients raise `representation_error`), non-difference cycles on huge
  domains still crawl (`2*X #> Y, Y #> 2*X` over `0..10^9` → controlled
  `resource_error(memory)` after ~11 s; over `inf..sup` it succeeds with nothing pruned, as SWI),
  no residual-constraint printing, missing `circuit/1`, `cumulative`, `zcompare/3`, `fd_degree/2`, ...
- `label/1` (leftmost) 20-queens is 2–4.5 s; the target is met with `ff` (the usual idiom and the
  spec's test).
- CLAUDE.md counts ("165 native indicators", "229 registry built-ins") are now stale by the CLP(FD)
  natives; to refresh at release.
- The generated manual (`docs/guides/guide-builtin-manual.{md,pdf}`) was rebuilt here but is left
  out of the patch (P1 regenerates it too): rerun `tools/build-manual.sh` after the merge.

## 12. Wave P2 — record

Status: **DONE — awaiting independent verification** (2026-09-23, one session). ISS-2025-0540..0553.
Test class: `core/engine/v4/EngineV45PerformanceTest` (17 methods). Before-build for every A/B:
a copy of `target/classes` of this tree taken before any P2 change (i.e. 4.4.0 + P1 + P5).

| Item | ISS | Status | Test |
|---|---|---|---|
| P2.1 call-site caching | 0540 | done (+ extension) | testISS0540_BodyGoalCallSiteIsCachedAndInvalidated, testISS0540_CallSiteRespectsTablingDeclaredLater, testISS0549_LibraryRecursionUsesCallSites |
| P2.2 sync without map lookup | 0541 | done | testISS0541_StoreReadsTheKnowledgeBaseHandle |
| P2.3 per-call flag checks | 0542 | done (deviation 1) | testISS0542_DispatchTablesAnswerWithoutStringKeys |
| P2.4 instantiate allocation | 0543 | done | testISS0543_InstantiateDoesNotWalkGroundSubterms, testISS0543_ArgumentViewIsUnmodifiableAndLive |
| P2.5 retractall | 0544 (KB), 0545 (v4 path) | done | testISS0544_KnowledgeBaseWritesAreLinear, testISS0545_RetractallGoesThroughTheStore |
| P2.6 retract-first loop | 0546 | done | testISS0546_RetractAndAssertaDoNotCopyThePredicate |
| P2.7 asserta loop | 0544 + 0546 | done | testISS0544, testISS0546 |
| P2.8 retract with bound key | 0544 | done | testISS0544 |
| P2.9 assert + var-headed clause | 0547 | done | testISS0547_MergedBucketViewIsIncremental |
| P2.10 append(_, [Last], L) | 0548 | done | testISS0548_AppendSplitModeIsLinear |
| P2.11 predsort / maplist family | 0549 | done (deviation 2) | testISS0549_PredsortSemantics, testISS0549_LibraryRecursionUsesCallSites |
| P2.12 lazy FINE logs | 0550 | done (deviation 3) | testISS0550_FineLogsAreLazy |
| P2.13 get_char | 0552 | done | testISS0552_GetCharSkipsTheDecoderForAscii |
| P2.14 .jpc | 0553 | done | testISS0553_JpcIsCompactAndLoadsTheSameProgram |
| **found: body-only variable age (engine semantics)** | **0551** | done | testISS0551_BodyVariableFirstBoundAfterAChoicePoint |

### What was done
- **Call path.** A compound body goal is a `Clause.Skel` carrying a `Machine.CallSite`: the
  `ClauseStore.Predicate` of a `user` predicate, or a module's own `Modules.Pred` + meta spec.
  Valid while `Engine.dispatchStamp()` (modCounts of `BuiltinTable`, `BuiltInRegistry`,
  `TableStore` + the ModuleManager stamp) is unchanged; the fast path skips stepN, the natives /
  registry / tabling probes and every `name/arity` string. `,`/`;`/`->`/`*->` in clause bodies
  are expanded lazily over the frame (`stepControlSkel`) — branches are built only when reached
  and their goals get sites. `stepN` skips its ~40 name comparisons with one set probe for any
  name it does not special-case (`stepPlain`). The store finds predicates by name then arity
  and syncs through the KB's `PredEntry` handle (a volatile field read).
- **Terms.** `CompoundTerm` stores a `Term[]` (`arity()`, `arg(i)`, adopting constructor,
  array-backed unmodifiable `getArguments()` view); ground skeleton sub-terms are shared, only
  `Skel` nodes are rebuilt, one array per node.
- **Database.** KB: one `PredEntry` + gap-buffer `RuleSeq` per predicate (O(1) asserta/assertz,
  O(1) retract of the stored Rule via a slot hint, one-pass retractall), no global list (the old
  global order is rebuilt from sequence numbers for `getRules()`), no KB first-argument index.
  Store: gap-buffer `Seq`s handed out as `View` windows (no copies; the window skips the dead
  prefix), bucket ⋈ variable-headed merge by clause ordinal, cached per bucket; `retractall/1`
  goes through the store index.
- **Library.** append/3 split mode → `lists:'$append_split'/3`; predsort array merge sort.
- **I/O.** Byte fast path in `PrologStream.decodeOne`; cached one-char atoms; `.jpc` format 0x04
  (varint integers, variable names once per clause; 0x03 still read) with an array reader.
- **ISS-2025-0551 (engine semantics, present in 4.4.0).** `p(Y) :- q(Z), X is Z+1, Y = X.`
  with `q(1). q(2).` answered only `Y = 2`: a body-only variable's cell was created after `q`'s
  choice point, so binding it by a deterministic built-in was not trailed and the frame kept
  it. Body-only slots are now created at clause activation. **Consequence for the §9 baseline:**
  in 4.4.0 `bench_atom(200000)` ran ONE real iteration (atom_number(A, 2) failed against the
  stale `A = '1'`), so its 4.4.0 time is not comparable (see the table).
- Invariants 7 (views instead of `rawArray`), 12 (activation-time body cells) and 13 (call
  sites and the dispatch stamp) updated/added in `report-engine-v4-progress.md` §3.

### Measured (JDK 25, `-Xmx1g`, machine shared, load average 9–11 during the final runs)
§9 harness, interleaved before/after/before/after, 4 runs per JVM, median of the warm runs of
each JVM (before → after, both JVMs):

| Benchmark | before (P1+P5 tree) | after P2 | change |
|---|---|---|---|
| nrev30 ×20000 | 5.23 / 5.14 s | 2.14 / 1.50 s | −59 % / −71 % (target −26 %, i.e. +35 % throughput) |
| fib(23) | 0.146 / 0.146 s | 0.115 / 0.060 s | −21 % / −59 % |
| loop(3M) | 2.29 / 2.52 s | 0.96 / 0.74 s | −58 % / −71 % (target −23 %) |
| tak(18,12,6) | 0.064 / 0.064 s | 0.041 / 0.029 s | −36 % / −55 % |
| queens(8), first solution | 0.104 / 0.067 s | 0.037 / 0.042 s | ≈ −40..−60 % |
| assert 200k + lookup (after retractall of 200k) | 8.07 / 4.84 s | 0.51 / 0.27 s | −94 % |
| findall 500k + msort (10 runs per JVM) | 0.55 / 0.42 s | 0.33 / 0.30 s | noise / faster |
| atom ops 200k | 0.054 / 0.054 s (**1 real iteration**, ISS-0551) | 0.077 / 0.076 s (200 000 real) | not comparable |
| deriv ×100k | 1.59 / 1.01 s | 0.71 / 0.49 s | −55 % / −51 % (target −20 %) |

P2 targets (before: this session's first runs on the before-build, load 6–12; after: final build):

| Target | before | after |
|---|---|---|
| retractall 25k / 50k / 100k | 0.78 / 3.86 / 7.69 s | 0.012 / 0.013 / 0.017 s |
| `retract(p(_))` first-clause loop, 1e5 | 5.97 s | 0.14 s |
| asserta loop, 1e5 | 16.1 s | 0.15 s |
| 200k-clause KB + 20k bound-key retracts (incl. fill) | 8.76 s | 0.26 s |
| assert + indexed call with a var-headed clause, 1e4 / 3e4 | 2.42 / 10.25 s | 0.016 / 0.031 s |
| `append(_, [Last], L)`, 1e4 / 3e4 | 13.2 s / (spec: 83 s) | 0.017 / 0.038 s (1e4 allocation 3.4 GB → 2.9 MB) |
| predsort 1e5 random / reversed | 2.51 / 1.14 s | 0.92 / 0.53 s |
| maplist / foldl / include 1e5 | 0.24 / 0.26 / 0.28 s | 0.074 / 0.093 / 0.146 s |
| get_char over 1.6 MB | 3.12 s | 0.80 s (the rest is the Prolog loop) |
| .jpc, 40k mixed clauses: consult vs load | 0.42 vs 0.31 s; file 3.62 MB (source 2.78) | 0.37 vs 0.086 s (23 %); file 2.83 MB |
| .jpc, 40k simple clauses: consult vs load | 0.34 vs 0.12 s; file 1.354 MB (source 1.358) | 0.12 vs 0.054 s (45 %); file 0.933 MB |

### Suite and examples
`mvn -o clean compile` OK. `mvn -o test -DargLine="-Xmx1g"`: **1365/1365** (1348 + 17), 0 failures,
0 errors, 1 min 23 s. `./test_all_examples.sh`: 20/20 PASSED, successful-query counts
2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0 (= baseline).
"Fails without the fix", checked against the before-build with probes: ISS-0551 (only `Y = 2`),
0543 (35 KB vs 0.5 KB per activation, bound 5 KB), 0548 (3.4 GB vs bound 40 MB), 0553 (old
.jpc 93 KB > 88 KB source); 0544/0545 bounds are ≥ 10× below the before times; the other tests
assert on hooks the old code does not have (siteHits, views, mergedSlots, decoderCalls, kbEntry,
modCounts) or, for 0550, scan the source (9 unguarded sites before).

### Deviations
1. P2.3: `debugPortsActive` was not hoisted into a per-drive boolean — after the call-site path it
   no longer shows in the profile (one field read + null test).
2. P2.11: no native maplist family (LIM-042: the closure may leave choice points and must keep
   its ports); instead the library recursion uses module call sites (≈2×). predsort keeps one
   nested drive per comparison (≈0.5 µs). `testISS0549_PredsortSemantics` pins semantics only
   — the change is a constant factor, the old code passes it.
3. P2.12: the 8000-deep consult no longer reaches any log call — the v2 parser rejects it first
   with `resource_error(parser_nesting)` (a P3 matter). All FINE sites are guarded; a source-scan
   test enforces it.
4. Beyond the spec: lazy `,`/`;`/`->`/`*->` expansion (P2.1) and ISS-0551 (a semantics fix,
   needed before branches could be expanded lazily — it is the same mechanism).
5. KB API: `getRulesForPredicate` returns an immutable shared snapshot (it returned a fresh
   unmodifiable copy; every caller only reads); `getRulesWithFirstArgIndex` /
   `getClauseSnapshot(f, a, arg)` are O(n) filters now (no v4 path uses them).
6. `.jpc` format bumped to 0x04 (0x03 still readable; 0x02 and older already recompiled by
   `consultSmart`).

### Found for later waves
- P3: consulting a clause nested ~8000 deep fails with `resource_error(parser_nesting)`.
- P6: `new Prolog()` runs `HttpServerPredicates.<clinit>` (builds an HTTP client and an
  SSLContext) — 3–4 % of a short JVM run in the profiles; plus the 13 WARNING lines.
- P4: `statistics(walltime, [T0,_])` (already noted by P1).
- CLAUDE.md is stale after P2 (baseline counts; KB description; the `p.select(...)` selection
  rule is now `p.view(...)`; `CompoundTerm` storage) — refresh at release.

## 13. Wave P4 — record

Status: **DONE — awaiting independent verification** (2026-09-23, one session). ISS-2025-0590..0613.
Tests: `core/engine/v4/EngineV45ConformanceTest` (23 methods, one per ISS) and
`core/engine/v4/EngineV4FormatTest` (85-row directive table + the sinks). Every defect was
reproduced on the 4.4.0 build first (`PrologCLI --batch`), so each test fails there.

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| P4.1 rounding, big-int division | 0590 | done | testISS0590_* | integer args unchanged; exact half-away rounding; BigDecimal `/` past 2^53 |
| P4.2 negative shift | 0591 | done | testISS0591_* | + primitive long fast path |
| P4.3 deep arithmetic | 0592 | done | testISS0592_* | recursive to depth 400, explicit stack beyond |
| P4.4 plus/3 integers | 0593 | done | testISS0593_* | exact (BigInteger) |
| P4.5 error context, `[X]` | 0594 | done | testISS0594_* | comparison context built lazily; `"a"` evaluates to 97 |
| P4.6 format/2,3 | 0595 | done | EngineV4FormatTest | Fmt rewritten; `~@` now runs once (it ran every solution) |
| P4.7 string built-ins | 0596 | done | testISS0596_* | any atomic text; `string_concat(-,-,-)` instantiation_error |
| P4.7 atomic_list_concat | 0597 | done | testISS0597_* | + any atomic separator |
| P4.7 string_upper/lower | 0598 | done | testISS0598_* | |
| P4.7 sub_atom code points, negatives | 0599 | done | testISS0599_* | also atom_concat/string_concat split mode |
| P4.7 print/1 quotes | 0600 | done | testISS0600_* | |
| P4.8 char_type/code_type | 0601 | done | testISS0601_* | code points; `white` = space/tab |
| P4.9 flatten | 0602 | done | testISS0602_* | cyclic check once, lazily |
| P4.10 list library | 0603 | done (deviation 1) | testISS0603_* | permutation/intersection/union/subtract = SWI clauses in prelude |
| P4.11 tab | 0604 | done | testISS0604_* | |
| P4.12 streams | 0605 | done | testISS0605_* | also get_char target type check |
| P4.13 with_output_to | 0606 | done | testISS0606_* | |
| P4.14 print_message | 0607 | done | testISS0607_* | native now |
| P4.15 statistics | 0608 | done | testISS0608_* | native now; + statistics/0 |
| extra: get_time & co. | 0609 | done | testISS0609_* | + stamp_date_time/3, date_time_stamp/2, SWI format_time order |
| P4.16 abolish / dynamic | 0610 | done | testISS0610_* | predicate_property/2 native for bound heads |
| P4.17 `M:G` | 0611 | done | testISS0611_* | |
| P4.18 op/3 permanent | 0612 | done | testISS0612_* | char_conversion/2 too |
| found: documented evaluables missing | 0613 | done | testISS0613_* | `cot`, `acot`, `lsb`, `popcount` |

### Pinned behaviours changed deliberately (tests repinned, not duplicated)
`BugFixVerificationTest`: testISS0180_* (shift), testISS0225_* (integer/1), testISS0187/0190/0192
(intersection/union duplicates, tab(-1)), testISS0188 (string_concat), testISS0378 (print quotes),
testISS0380 (last/2 now enumerates — first answer taken with `!`), testISS0409 (`~d` error is
`format(_)`). `AdvancedArithmeticTest.testPlusWithFloats`. `EngineV4IoTest` (print, tab, `~r`,
`~d`), `EngineV4TextTest` (string_concat, atomic_list_concat), `EngineV4IsoErrorsTest` (rows
atom_string/string_concat; deviations 5, 6, 8 removed; 12 rows added — 261 rows),
`EngineV4CharTypeTest` (domain_error), `EngineV4RetirementTest` (max_list),
`EngineV4LibraryTest` (last/2), `EngineV4ModulesTest`/`AuditRound5Test`/`RefactorIssuesTest`
(M:G), `EngineV4OpsTest` (3)/`EngineV41RetirementTest`/`RefactorIssuesTest` (op/3 permanent),
`EngineV4WriterTest` (print_message of an unknown term).

### Suite and examples
`mvn -o clean test -DargLine="-Xmx1g"`: **1390/1390** (1365 + 25), 0 failures, 0 errors, 2 min 31 s.
`./test_all_examples.sh`: 20/20 PASSED, successful-query counts 2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 2,
1, 0, 0, 2, 0, 0, 0, 0, 0 (= baseline). `tools/build-manual.sh` rerun.

### Measured (JDK 25, `-Xmx1g`, load average 12–15; 3 interleaved JVM pairs, warm runs)
Before = `target/classes` of the P1+P5+P2 tree (scratchpad `p2/after-classes`); P4-focused
harness (`p4/bench/bench4.pl`, timing by `statistics(walltime)`). min / median in seconds:

| Benchmark | before | after P4 |
|---|---|---|
| fib(23) | 0.046 / 0.050 | 0.045 / 0.070 |
| loop(3M) | 0.914 / 0.924 | 0.934 / 1.121 |
| tak(18,12,6) | 0.032 / 0.035 | 0.028 / 0.046 |
| queens(8) (permutation now the SWI clauses) | 0.025 / 0.037 | 0.015 / 0.017 |
| atom ops 200k | 0.127 / 0.129 | 0.121 / 0.129 |
| deriv ×100k | 0.860 / 0.878 | 0.736 / 1.002 |
| format(atom) ×100k (5 directives + column) | 0.283 / 0.432 | 0.165 / 0.236 |
| sum_list+max_list+min_list 1e5 ×50 | 0.549 / 0.641 | 0.487 / 0.802 |
| arithmetic mix ×300k (//, mod, integer, round, truncate, shifts) | 0.500 / 0.535 | 0.387 / 0.572 |
| text mix ×100k (string_concat, atomic_list_concat split/join, sub_atom) | 0.220 / 0.230 | 0.187 / 0.212 |
| flatten 3×1000 ×300 | 0.054 / 0.059 | 0.046 / 0.072 |

The machine noise is ±30 % (an earlier round had the same builds in the opposite order); the
minima show no regression. A first version of the P4 code DID cost ~20 % on loop/fib/deriv: the
comparison context was a `switch` over the operator string inside `Machine.solveBuiltin` (bigger
method, worse inlining) and every evaluation allocated an evaluator even for a bare number. Fixed
before these numbers: the context is built only on error (`ArithEvaluator.evalCompare`), a
number short-circuits `evalDeref`, and sum/max/min scan with a primitive accumulator.

### Deviations
1. P4.10 `nth0(-1, [a], X)` keeps FAILING: SWI's `nth0/3` fails for a negative index (only
   `nth0/4` calls `must_be(nonneg, _)`); the spec's `type_error` guess was not SWI.
2. P4.6 argument faults are `error(format(Message), _)` as the spec says; SWI 9 may use
   `format_argument_type(D, Arg)` for some of them (not verifiable here — no swipl installed).
   `~Nw`/`~Nq` right-alignment kept (a JProlog extension pinned by ISS-2025-0249).
3. P4.10 `last/2`, `nth0/3`, `nth1/3` on a partial list enumerate without end (SWI) — a caller
   that backtracks into them must bound the search (LIM-043). `length(L, L)` fails (spec: pick).
4. P4.14 informational messages still go to the current output (SWI: user_error); errors and
   warnings go to user_error. No `message_hook/3` (LIM-043).
5. P4.15 `cputime`/`runtime` are the calling thread's CPU time; `atoms`/`functors`/`codes` are 0.
6. P4.16 `predicate_property/2` reports `static` for consulted predicates (they stay modifiable,
   §8); library predicates that are also natives (`append/3`) report `built_in` too.
7. P4.12 `eof_action(reset)` = `eof_code`; `user_input` is exempt from the text/binary check.
8. `abolish/1` on a consulted (static) predicate is still allowed (§8: consulted predicates stay
   modifiable).
New limitation: LIM-043 (the residue above).

### Found for later waves
- P3 (writing): floats ≥ 1e7 print in E notation (`get_time(T)` shows `1.790165837229e9`; SWI
  `1790165837.229`); the CLI's uncaught-error line prints canonical operators
  (`type_error(evaluable, /(foo, 0))`, `is/2` unquoted) instead of writeq.
- P3 (reading): `read/2` with a non-stream argument raises a message-atom
  `PrologEvaluationException` ("read/2: invalid stream argument"); `read_term/2,3` resolve an
  unknown stream to stdin silently.
- P6 (CLI): the batch CLI collects ALL solutions, so a query over an enumerating predicate
  (`last([a|T], b).`) runs out of memory instead of streaming (P6.4 covers it).
- Missing library: `limit/2`, `offset/2` (library(solution_sequences)); `rational/1`, `rdiv`
  documented but absent from the v2 evaluator.
- CLAUDE.md is stale after P4 too: print/1 quoting, op/3 permanence, native count (print_message,
  statistics/0,2, string_upper/lower added to the native table).

## 14. Wave P3 — record

Status: **DONE — awaiting independent verification** (2026-09-23, one session). ISS-2025-0560..0579.
Test class: `core/engine/v4/EngineV45LoadReadWriteTest` (21 methods, one or two per ISS). The class
does not compile against the before-build (it uses the new loader API), so "fails without the fix"
was checked by probing every defect on the before-build (`target/classes` of the P1+P5+P2+P4 tree,
scratchpad `p3/before-classes`) with `PrologCLI --batch`: all reproduced (escapes read literally,
`-2^2`, `- - -`, `[](a,b)`, `1.0e7`, writeq `inf`, read/2 message atom, open_string /
read_term_from_atom / expand_term / consult / source_file missing, dcg `\+(b,S0,S1)`,
term_to_atom syntax error as a message atom, `atom_to_term` without `_Y`, `` `ab` `` a string,
`table((a/1,b/1))` failing; module scope, include, listing, .jpc and the CLI count as in §3).

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| P3.10 lexer escapes | 0560 | done | testISS0560 | `\e` `\s` `\uXXXX` `\UXXXXXXXX`; unknown escape (`\z`) = syntax error |
| P3.8/P3.12 nesting | 0561 | done | testISS0561 | infix chains iterative (`TermReader.parseOperators`, array stack, no allocation per operator); > 1 000 real nesting re-read on a 1 GB-reserved-stack helper thread, limit 200 000; `.jpc` writer/reader iterative past depth 256; 150 000-deep clause consults, copies, writes, re-reads |
| P3.9 writer round trip | 0562 | done | testISS0562 x2 (incl. 4 000-term property test) | +/-(number-first) canonical, operator atoms as operands bracketed, prefix op + `(` spaced, `'[]'(a,b)`, octal control escapes, `'_x'` quoted; reader: SWI-lenient prefix ops (`X = \+a`), `- mod(X)` is a compound |
| extra: float text | 0563 | done | testISS0563 | `Number.formatFloat` = SWI shortest layout (exponent only < 1e-4 or >= 1e15) |
| P3.9 inf/nan | 0564 | done | testISS0564 | writeq `1.0Inf`/`-1.0Inf`/`1.5NaN`; the lexer reads them |
| extra: CLI error line | 0565 | done | testISS0565 | writeq with `, ` spacing through `core.engine.v4.Writer` |
| P3.4 readers on v2 | 0566 | done | testISS0566 | new `core.engine.v4.NativeRead` (clause collector + v2 parser, fresh variable cells, options); `read_term(S, T)` kept as `read_term(S, T, [])`; Java `asserta(String)`/`retract(String)` on v2 too |
| extra: read/2 stream errors | 0567 | done | testISS0567 | via `IOStreamUtils.inputStream` |
| P3.4 term_to_atom & co. | 0568 | done | testISS0568 | syntax errors raise; atom_to_term lists `_Y` |
| P3.5 string streams | 0569 | done | testISS0569 | open_string, with_input_from, read_line_to_string/codes(2,3), read_string/3,5 |
| P3.7 listing | 0570 | done | testISS0570 | SWI portray_clause layout, `_` singletons, `:- dynamic` header, re-consult gives variant clauses; CLI `:save` writes the listing |
| P3.11 DCG/expansion | 0571 | done | testISS0571 | natives on the v2 translator; consult and `.jpc` loads apply term_expansion/2 |
| P3.3 table forms | 0572 | done | testISS0572 | comma/list/`//`/`as`; moded min/max/first/last/-; lattice/po -> domain_error(table_mode); `predicate_property(P, tabled)` |
| P3.2 module scope | 0573 | done | testISS0573 | per-load context; module restored + declared modules imported at end of load; `:- module(user,_)` switches back; workarounds removed from 3 test classes |
| P3.1 loading | 0574 | done | testISS0574 | consult/1, `[F]`, ensure_loaded/1, load_files/1,2, make/0 in `builtin.filesystem` (safe-mode denied); reconsult; use_module(File) |
| P3.1 include | 0575 | done | testISS0575 | textual, same load |
| P3.1 source_file, load ctx | 0576 | done | testISS0576 | |
| P3.6 .jpc | 0577 | done | testISS0577 x2 (examples equivalence) | compile = v2 reader, clauses recorded as read, loaded through the consult handler; LIM-036 resolved |
| extra: CLI clause count | 0578 | done | testISS0578 | |
| backquotes | 0579 | done | (in testISS0566) | `` `ab` `` = codes |

### Pinned behaviours changed deliberately (tests repinned, not duplicated)
`BugFixVerificationTest.testISS0387_WriteqSeparatesMergingSymbolicTokens` (`-(1)` and `(-)-(-)`,
the wrong pin at :3751), `testISS0390_FloatExponentLowercaseAndInfNan` (float layout, `1.0Inf`);
`EngineV4WriterTest` (`write_term(-(1),[])` = `-(1)`; portray_clause `p(A, B)`);
`EngineV4DatabaseTest.testISS0499_ListingOneWorksAndIsCaptured` (`baz.`, no header);
`EngineV4StreamsTest` (read variables are fresh cells; the nesting test goes past 200 000);
`EngineV4TermTest.testISS0498_AtomToTerm` (fresh cells); `RefactorIssuesTest.testR2` (one load);
the `:- module(user, []).` workarounds removed from `EngineV4ModulesTest` (10),
`EngineV41RetirementTest` (1), `AuditRound5Test` (1). Engine invariants 14 and 15 added to
`report-engine-v4-progress.md` §3.

### Suite and examples
`mvn -o clean compile` OK. `mvn -o test -DargLine="-Xmx1g"`: **1411/1411** (1390 + 21), 0 failures,
0 errors, 1 min 54 s. `./test_all_examples.sh`: 20/20 PASSED, successful-query counts 2, 0, 0, 1, 1,
0, 0, 0, 0, 0, 2, 1, 0, 0, 2, 0, 0, 0, 0, 0 (= baseline). `tools/build-manual.sh` rerun.

### Measured (JDK 25, load average 12–18; thread CPU time, both builds in ONE JVM through two
class loaders, alternating, min of 13 rounds; 3 JVMs)
| Measure | before (P1+P5+P2+P4) | after P3 |
|---|---|---|
| consult 40k mixed clauses | 0.080 / 0.098 / 0.111 s | 0.086 / 0.109 / 0.111 s |
| `.jpc` load, same 40k clauses (2.35 MB) | 0.022 / 0.025 / 0.031 s | 0.021 / 0.029 / 0.031 s |
| writeq 100k compounds, no floats (wall, min of 10, separate JVMs) | 187.7 / 211.9 ms | 177.0 / 211.8 ms |
| writeq 100k with 4 floats each | 56.6–69.8 ms | 64.9–70.6 ms (output 22 % longer: `10000000000.0`) |
| writeq 100k with prefix operators | 107.5–122 ms | 115–130 ms (`-(1)` form) |
A first version cost ~25 % on the `.jpc` load and ~10 % on consult (an allocated frame per compound in
the iterative `.jpc` reader, a `Pending` object per infix operator in the reader, a string key per
clause for the term_expansion check) and 2x on float printing (string splitting); fixed before
these numbers: recursive fast path to depth 256 in `JpcReader`, array stack in `TermReader`,
`KnowledgeBase.hasRules(key)`, `Number.formatFloat` returns Java's text in the plain range and
lays out the E form from the characters. Engine hot paths are untouched (no Machine change except
tabling-table creation).

### Deviations
1. `(a|b)` still reads as `(a;b)` (SWI 7+: `'|'(a,b)`); `as` is not an operator (use
   `table(as(Spec, Opts))`). Changing either touches DCG bodies and the operator table; LIM-044.
2. Mode-directed tabling: `lattice/po` raise `domain_error(table_mode, M)` as the spec allows; a
   call with the moded argument bound evaluates with it bound (LIM-044).
3. A consulted file's errors are printed as warnings on `user_error` and consult/1 succeeds
   (SWI); `Prolog.consult(String)`/`consultFile` keep throwing one PrologException listing them.
4. Directives now run ONCE (`once/1`, ISO 7.4.2) and inside the module being loaded; they ran
   every solution before (`:- member(X,[1,2]), write(X).` printed `12`).
5. `listing` no longer prints the `% Listing for p/1:` header (SWI prints none).
6. `read/1` on `user_input` shares the engine's stdin reader with `get_char/1`, not with the CLI's
   query reader (unchanged).

### Found for later waves
- P6: loads are serialised per engine by a lock — a directive that starts a thread which consults
  and joins it would deadlock; `initialization(G, main)` does not halt; the batch CLI still prints
  the demo facts' `:- dynamic` in `:save`. `PrologCLI` keeps an unused legacy `Parser` field.
- P6/P7: CLAUDE.md is stale after P3 too (loader, reader, `listing`, invariants 14–15, native
  count: NativeRead/NativeExpand add 15 indicators). The legacy `builtin/io/Read`, `ReadTerm`,
  `builtin/term/AtomToTerm`, `TermToAtom` and `builtin/dcg/DCGUtils.DCGTranslateRule` classes are
  no longer reached (natives win); delete them at release with the registry entries kept for the
  permission errors.
- P7: `Writer` options `ignore_ops`/`max_depth` are not in the round-trip property test.

## 15. Wave P6 — record

Status: **DONE — awaiting independent verification** (2026-09-23, one session). ISS-2025-0620..0639
(all twenty used). Tests: `core/engine/v4/EngineV45HardeningTest` (17 methods),
`test/cli/PrologCliToplevelTest` (7), plus deliberate edits to `EngineV4ThreadsTest`,
`EngineV4StreamsTest`, `EngineV41RetirementTest`, `EngineV4RetirementTest`. "Fails without the fix"
holds by construction for the new predicates/options and message-atom errors (4.4.0 raises
existence/message errors or hangs); the load-lock test was run with guest admission disabled and
fails with "the load deadlocked".

| Item | ISS | Status | Test | Notes |
|---|---|---|---|---|
| P6.3 shared budget + O(N) charging | 0624 | done | EngineV45HardeningTest.testISS0624 x2 | `ResourceGuard` pool + child guards; natives charge per element |
| P6.5 thread_detach race/semantics (extra 1) | 0620 | done | testISS0620 + EngineV4ThreadsTest made deterministic | |
| P6.5 ISO thread errors | 0621 | done | testISS0621 | |
| P6.2 concurrent family propagation | 0622 | done | testISS0622 | + Workers answer aliasing |
| P6.2 concurrent argument errors | 0623 | done | testISS0623 | |
| P6.5 selective receive, get/3 | 0629 | done | testISS0629 | |
| P6.5 thread_property, queue destroy | 0630 | done | testISS0630 | |
| P6.5 mutexes, thread_join/1 | 0631 | done | testISS0631 | |
| P6.5 thread_exit | 0632 | done | testISS0632 | |
| P6.5 per-thread globals | 0633 | done | testISS0633 | |
| P6.5 concurrent_forall | 0634 | done | testISS0634 | |
| P6.6 StreamsTest overlap | 0635 | done | EngineV4StreamsTest.testISS0472 (rendezvous via queues) | |
| P6.1 safe mode: native table, deny names, SafeModeOptions.allowFileRead, allowlist snapshot | 0625 | done | testISS0625 x2 + src/test/resources/safe-mode-allowlist.txt | |
| P6.1 per-engine logging | 0626 | done | testISS0626 | |
| P6.4 CLI streaming (extra 2), options, exit status, no demo facts | 0627 | done | PrologCliToplevelTest x5 | `Prolog.solveStream(String, AnswerSink)` + `Machine.hasAlternatives()` |
| P6.4 runnable jar + exec mainClass | 0628 | done | PrologCliToplevelTest.testISS0637 | `target/jprolog.jar` |
| extra 3: initialization(G, main) halts | 0636 | done | PrologCliToplevelTest.testISS0636 | |
| extra 5: unused CLI Parser field | 0637 | done | PrologCliToplevelTest.testISS0637 | |
| extra 4: lazy HTTP client | 0638 | done | testISS0638 | |
| extra 6: load lock deadlock | 0639 | done | testISS0639 (fails = "the load deadlocked" with guests disabled) | `LoadLock` + `ThreadWaits` |

### What changed, by file
- `core.engine.ResourceGuard`: shared `AtomicLong` pool, per-guard credit (chunks of 1 024), `child()`,
  `release()`, `charge(n)` (budget only — `statistics(inferences)` still counts steps). `Workers`
  runs a worker on `parent.child()`, installs per-worker globals, copies an answer with ONE variable map.
  O(N) charges: `NativeLibrary.elements/spineTail/LengthB/SortB/MemberGen/AppendB(open)/NthB(open)`,
  `Machine.lengthEnumerate`, `Unify.copy/copyAcyclic` (per node) and `Unify.poll`, `NativeText.AtomCharsB`.
- `builtin.threading.ThreadPredicates` rewritten (records under a per-thread lock, selective queues,
  mutexes, ISO errors, `main` = every non-worker thread); `ConcurrentPredicates` rewritten around
  `await` (unwrap, `rethrowIfControl`, copied balls, no timeout); `core.engine.ThreadExitException`
  (added to `ControlFlow`), `ThreadWaits`, `LoadLock` (replaces `synchronized (loadLock)` in `Prolog`).
- `Prolog`: `enableSafeMode(SafeModeOptions)` + `UNSAFE_PREDICATE_NAMES` over registry AND native
  table, `checkSafeRead` in `loadSpec`/include/use_module, `SafeOpen`; per-worker globals;
  `solveStream(String, AnswerSink)` (+ `Machine.hasAlternatives()`); `setDeferInitializationMain`,
  `takeInitializationMain`, `runOnce`. `BuiltinTable.keys()`. `BuiltInRegistry` arity entries for the
  thread family (thread_join/5 is now an unknown procedure). `LoggingPredicates` per engine.
  `HttpServerPredicates` lazy client. `PrologCLI` rewritten. `pom.xml`: `finalName`, jar manifest,
  exec mainClass.

### Suite, flakiness, examples
- `mvn -o clean compile` OK; `mvn -o test -DargLine=-Xmx1g`: **1435 run, 0 failures, 0 errors**
  (1411 + 24 new). The two order-dependent failures of P5 (`main` alias held by a live thread) are
  gone by construction (every non-worker thread is `main`).
- Thread classes (`EngineV4ThreadsTest`, `EngineV45HardeningTest`, `EngineV41RetirementTest`,
  `EngineV4RetirementTest`, `EngineV4StreamsTest`, `PrologCliToplevelTest`) run 3 times: 102/102,
  102/102, 102/102.
- `./test_all_examples.sh`: 20/20 PASSED, counts 2,0,0,1,1,0,0,0,0,0,2,1,0,0,2,0,0,0,0,0 — with the
  (untracked) script now passing `--demo` (see deviation 1). Output diffs vs before the wave, other
  than the demo-facts banner: an answer that leaves a choice point now ends `;` + `false.` (SWI) —
  test_04, test_05, test_14 — no count changes.
- Benchmarks: not measured rigorously (load average made 4.4.0 nrev take 14 s instead of 3.5 s). The
  unlimited-budget hot path is unchanged in shape (`pool != null` replaces `budget > 0`); the added
  charges are per call or per 1 024 elements.

### Deviations
1. **Demo facts vs the examples baseline.** §8 removes the CLI demo facts; test_01's queries
   (`parent(tom,bob)`, `father(tom,bob)`) only ever succeeded because of them, so the baseline count 2
   needs them. The local, untracked `test_all_examples.sh` passes `--demo` (commented); nothing else
   depends on them.
2. **Exit statuses**: `-g`/`initialization(main)` use 1 for both failure and an uncaught error, as
   the spec asks; bad command-line options exit 2.
3. **`main` identity**: every non-worker thread (not only the first one) is `main` and they share
   one queue — the fix for the order-dependent tests; a receive in tests is now selective.
4. **Load lock**: only `thread_join` waits are recognised (LIM-045); other waits are not detected
   and there is no timeout (a timeout would fail legitimately long loads).
5. **Budget**: exact for one machine; with several, overshoot bounded by the others' unused credit
   (≤ 1 024 each). The bridged extended libraries are not charged (LIM-045).
6. `csv_*_file`/`log_to_file` and `open/3,4` are removed from the registry by NAME in safe mode
   (the packages `builtin.csv`/`builtin.logging`/`builtin.io` are otherwise allowed). The native
   table has no host-touching predicate today; the same name list is applied to it defensively.

### Found for later waves
- P7 / CLAUDE.md is stale after P6: "each worker gets its own `ResourceGuard` with the parent's
  limit" (now a child of ONE pool) in the rules list, Workers and Sandbox sections; the safe-mode
  paragraph (deny list by name, native table, `SafeModeOptions`, csv/logging); the CLI paragraph
  (streaming, options, `--demo`, exit statuses, `run()`, runnable jar, the `exec` mainClass gotcha is
  fixed); `test_all_examples.sh` now passes `--demo`; the error-trust model gains
  `ThreadExitException`; the test list gains `EngineV45HardeningTest`, `PrologCliToplevelTest` and
  the baseline count (1435); `Prolog.solveStream(String, AnswerSink)`, `enableSafeMode(SafeModeOptions)`
  in the embedding API; the thread family description (ISO errors, `main`, mutexes).
- P7: the uncaught parse-error line in the CLI is still a message atom
  (`'Error parsing query: ...'`); `halt/0,1` in safe mode; `thread_signal/2` & co. (LIM-045).

## 16. Wave P7 — record

Status: **DONE — awaiting independent verification** (2026-09-23, one session). ISS-2025-0660..0675
(0676–0679 unused). New test classes: `core/engine/v4/EngineV45ReleaseTest` (6),
`test/performance/PerformanceRegressionTest` (10), helper `test/support/QueryStartLatch`.
Item log (written as the wave went, so a crashed session could resume from it):

| Item | ISS | Status | Notes |
|---|---|---|---|
| 7.1 tautologies (`ISOComplianceTest`) | 0660 | done | 7 methods rewritten with value assertions; `testISOComplianceLevel` now checks 109 indicators with `predicate_property(H, defined)` + one sentinel that must be undefined (the arity-0 probe counted a missing predicate as implemented) |
| §8 tabled non-stratified negation (never implemented by P1–P6) | 0661 | done | `\+` inside a tabled evaluation runs as an opaque sub-run; reading an incomplete table created before the negation raises `permission_error(negate, incomplete_table, G)`; WFS itself → LIM-046 |
| 7.2 catch-anything → exact error terms | 0662 | done | RefactorIssues R2/R3(eof, + stream closed)/R5, AuditRound5 test2/mustBe/test7, EngineHardening 0428, BugFix 0229/0363, ExceptionHandling(builtin) mismatch, ProductionAudit 0341 x2 (deep term/input now asserted to WORK) |
| 7.6 AuditRound5 GC test | 0664 | done | removed (could not fail); replaced by a plain live-atom interning check |
| 7.3 existence-only → value assertions | 0663 | done | MegaPredicateTest (19 methods incl. Hanoi move list), ProductionAudit msort/sort x3, MetaPredicatesTest (10; `once/1` now over a 3-answer goal), AnonymousVariableTest (11), ListBuiltinsTest (5 "verify" queries were SEPARATE queries — tautologies; append split count 4), NegationAsFailure, JPrologComprehensive x2, BugFix `>= N` x5 (exact counts) and 12 `contains("1")` sites + the `'1'`/`1` toString check (`==` in the query) |
| 7.4 dead tests | 0665 | done | `BuiltInTests.java` deleted (10 never-run type-check duplicates); `OperatorDefinitionTest`, `Phase1FeaturesTest`, root `ExceptionHandlingTest` (throw half) retargeted at the engine (they drove the legacy `OperatorDefinition`/`ArithmeticEvaluator`/`Throw` and mutated the JVM-wide static op table); 3 duplicated "QuerySolver is deleted" checks removed (Tabling/Library/Threads; the one in `EngineV4RetirementTest` stays — the spec's "5" counted 3 duplicates + the canonical one + the ENG-17 list); stale names renamed (`testISS0331_DebugPorts`, `testISS0348_StringIdentityAndAtomic`, `testISS0568_TextToTermOnTheV2Parser`); ENG-11 loop retargeted at the still-bridged `get_time/1` (asserted bridged) |
| 7.5 `FamousPrologProgramsTest` | 0666 | done | 9 real programs, full answers: 92 queens (cross-checked against brute force over 8!), Hanoi move lists, quicksort vs msort (3000), zebra (whole house list), SEND+MORE via CLP(FD), Ackermann (3,6)=509, sieve (≤3000, 430 primes; + yall variant), DCG expression evaluator; JUnit timeouts; ~5 s total |
| 7.6 flakiness | 0664 | done | `test/support/QueryStartLatch` (the query writes `go` into a thread-local stream that opens a latch): EngineHardening 0431 (4 Stop cases), BugFix 0320, EngineV4Threads 0480, EngineV4Tabling 0463 now interrupt only once the query runs; ENG-13 "< 50 µs per lookup" → 20 000-fact vs 200-fact lookup ratio (< 10x, min of 5 interleaved rounds); GC test removed (above); op-table mutation gone with the retargeted Operator/Phase1 classes; RefactorIssues R3 stream closed. `EngineV4ThreadsTest:249`'s detach race had already been fixed by P6 (ISS-2025-0620) |
| 7.7 `test/performance/PerformanceRegressionTest` | 0667 | done | 10 growth tests (N vs 4N, warm-up, min of 3 interleaved rounds, bound 10x + 50 ms): retractall, retract-first loop, asserta loop, bound-key retract, assert + indexed call with a var-headed clause, assert/retract interleave, `append(_, [X], L)`, predsort, bagof grouping (+ 40 000 witnesses < 20 s absolute), maplist/foldl; ~15 s |
| 7.8 trace oracles | 0668 | done | engine: a traced CLAUSES frame stays until its Exit and is dropped there when nothing is above it (was: dropped at activation, so a body failure never printed the parent's Fail); the if-then-else/once/ignore/forall port frame likewise; a traced frame looks ahead (per-argument key clash, before binding) so its last matching clause makes it deterministic; `'$mctx'(user, G)` prints as `G` (`M:G` otherwise) in trace and debugger ports. Oracles changed deliberately: BacktrackRedoFail, Cut, Findall, BetweenOnce, ForallIgnore, Maplist, DebuggerSeesTheSamePorts (all: no phantom Fail after a deterministic Exit, = SWI); new `testISS0668_Trace_ParentFailIsPrinted`. Traced `loop(N)` stays linear (50k–400k: 4.6–16.6 s, ~23 µs/iteration incl. formatting). Untraced execution unchanged (every change is behind `traceGoal != null` / `ftrace||fdebug`) |
| leftover: unreachable legacy classes | 0670 | done | deleted `builtin/io/Read`, `builtin/io/ReadTerm`, `builtin/term/AtomToTerm`, `builtin/term/TermToAtom`, `builtin/arithmetic/Between`, `DCGUtils.DCGTranslateRule` (inner class); registry names kept as `ControlConstruct` placeholders + arity entries (`read_term` 2,3, `term_to_atom` 2, `dcg_translate_rule` 2 — /4 was a message-atom stub, now an unknown procedure); `call_dcg/3` now translates through the v2 DCG translator; BugFix ISS0408 text test retargeted at the engine reader |
| leftover: CLI/query parse error | 0671 | done | `Prolog.solve`/`solveStream` raise `error(syntax_error(Msg), query)` (was the atom `'Error parsing query: ...'`); the CLI prints `Error: error(syntax_error('unexpected token ...'), query)` |
| leftover: halt in safe mode | 0672 | done | denied by default: `permission_error(call, sandboxed, halt/halt(N))` (SWI sandbox); `SafeModeOptions.allowHalt()` restores it; the CLI's `--safe` uses `allowHalt()` (the process is the user's). The engine itself never calls `System.exit` |
| leftover: `writeq('\e')` | 0673 | done | `'\e'` in quoted atoms and strings (was `'\33\'`) |
| leftover: `thread_signal/2` family | — | LIM-045 (already listed there by P6) | not implemented: needs an asynchronous interrupt point in every worker's drive loop |
| tests for the engine-side fixes | 0661, 0670–0673 | done | new `core/engine/v4/EngineV45ReleaseTest` (5 methods) + `PrologCliToplevelTest` +2 (0671 CLI line, 0672 `--safe` halt status) + `EngineV4TraceTest` +1 (0668) |
| 7.9 `docs/references/ref-deviations.md` | 0669 | done | B.17 decisions, the IsoErrors deviations, the §8 decisions and the per-wave deviations in one file, linked from README, CLAUDE.md, the IsoErrors class comment and the predicate reference; factual errors fixed: `integer/1` is not an ISO evaluable (the "ISO 9.1.6.5" citations in BugFix/ArithmeticEvaluator), `call((fail, 1))` cited ISO "8.15.1.3" (`\+/1`'s clause; now 7.8.3.3 / 7.6.2), and the predicate reference still said `call((fail, 1))` fails / `string_concat(-,-,-)` fails / `tab` fails |
| LIM-046 | — | new | tabled negation is not WFS (0661) |
| extra: CLP(FD) override warnings | 0674 | done | the 13 "Overriding existing built-in predicate" WARNING lines of every `new Prolog()` (26 stderr lines on each `java -jar jprolog.jar` start) are logged at FINE |
| extra: version flags | 0675 | done | `version`/`version_data`/`prolog_version` said 2.0.15; now `40500`, `jprolog(4,5,0,[])`, `'jprolog-4.5.0'` (SWI shapes) |
| release | — | pom 4.5.0; examples 20/20 (counts = baseline); `target/jprolog.jar` runs `-g "write(ok),nl" -t halt` → `ok`, exit 0 |

### Suite, reruns, examples, jar
- `mvn -o clean compile` OK. `mvn -o test -DargLine="-Xmx1g"`: **1452 run, 0 failures, 0 errors**
  (2 min 31 s). Arithmetic from P6's 1435: `OperatorDefinitionTest` 10 → 11 (+1, per-engine
  operators), `FamousPrologProgramsTest` 12 → 9 (−3: ten toy programs became nine real ones),
  `EngineV4TraceTest` +1 (0668), `PerformanceRegressionTest` +10, `EngineV45ReleaseTest` +6,
  `PrologCliToplevelTest` +2 → 1435 + 1 − 3 + 1 + 10 + 6 + 2 = **1452**. Every other rewritten class
  (ISOCompliance, MegaPredicate, Meta, Anonymous, ListBuiltins, AuditRound5, root
  ExceptionHandling, Phase1, BugFix, …) kept its method count; `BuiltInTests.java` (10) was never
  run, so deleting it does not change the count.
- Thread/timing-sensitive classes (EngineV4Threads, EngineV45Hardening, EngineV41Retirement,
  EngineV4Retirement, EngineV4Streams, PrologCliToplevel, EngineHardening, EngineV4Tabling,
  EngineV4Trace, PerformanceRegression, BugFixVerification, EngineV45Release) run 3 times in a row:
  643/643, 643/643, 643/643 (before the version-flag test was added).
- `./test_all_examples.sh` (with `--demo`): 20/20 PASSED, counts 2,0,0,1,1,0,0,0,0,0,2,1,0,0,2,0,0,0,0,0.
- `mvn -o package -DskipTests` → `target/jprolog.jar`; `java -jar target/jprolog.jar -g
  "write(ok),nl" -t halt` prints `ok`, exit 0 (and no longer 26 stderr WARNING lines, 0674).
- `tools/build-manual.sh` rerun (9073 lines, 180 pages, version 4.5.0).

### Release documentation
pom 4.5.0; CHANGELOG 4.5.0 (all seven waves with ISS ranges, behaviour changes, CLI and jar,
`.jpc` 0x04, deleted classes, new limitations); track-release-notes; track-issues (P7 entries);
track-limitations (LIM-046 new, LIM-045 halt/CLI points resolved); README (4.5.0 section, CLI and
jar usage, numbers); BUILTIN_PREDICATES_REFERENCE (halt, version flags, writeq `\e`, tabled
negation, call_dcg, dcg_translate_rule, the stale 4.3 deviation paragraph) + manual; CLAUDE.md
refreshed (numbers, KB/ClauseStore after P2 and the `p.view` rule, loader/reader natives, worker
`child()` guard, safe mode and `SafeModeOptions`, CLI, `mvn exec:java`, surefire 2.17, the
`--demo` note, `ThreadExitException`, trace-frame rule, new test classes, ISS ranges); pom
surefire comment.

### Deviations / doubts
1. §8's tabled-negation decision was never implemented by P1..P6; P7 did it (0661). The check is
   "a consumer inside the negation reads an incomplete table created BEFORE the negation" — exact
   for linear tabling, where every incomplete table is on the current evaluation's stack. Only
   while the calling thread holds the tabling claim; the negation then runs as an opaque
   sub-run (`forEachSolution`), so its trace ports inside a tabled evaluation are the sub-run's.
2. Trace change (0668) keeps traced frames until their Exit — memory is O(open calls) under
   tracing (a traced 400 000-deep tail recursion kept 400 000 frames; linear, 16.6 s). Untraced
   execution is unchanged by construction; the IDE debugger sees the same port stream.
3. `thread_signal/2` not implemented (already in LIM-045).
4. The CLI's syntax-error context is the atom `query` (SWI: `string(Text, Pos)`); recorded in
   ref-deviations.md.
5. The spec's "5 duplicated QuerySolver checks": 3 duplicates found and removed; the canonical one
   stays in `EngineV4RetirementTest` and the ENG-17 list in `EngineHardeningTest` checks other
   classes.
6. Extra (found here, fixed): the 13 CLP(FD) override WARNINGs (0674) and the 2.0.15 version flags
   (0675). Not fixed, noticed: `a :- :- b.` parses as a query (SWI: priority clash) — a P3-area
   reader leniency, left as is.
7. "Bridged" built-in count in CLAUDE.md/README (~240 names) is computed as registry names with no
   native entry, minus placeholders, inline names and prelude exports; the old "229" used a method
   not recorded, so the two numbers are not strictly comparable.

