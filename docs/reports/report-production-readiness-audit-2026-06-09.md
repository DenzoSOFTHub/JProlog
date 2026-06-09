# JProlog — Production-Readiness Audit (2026-06-09)

Multi-agent audit (6 dimensions + adversarial verification of every blocker/high finding) of JProlog
v3.3.0 (~62k LOC). Verdict is given per **deployment profile** because it differs sharply:

- **A — Embedded library**: single app, single thread, **one** engine per JVM, **trusted** programs.
- **B — Concurrent / multi-tenant server**: many threads and/or many engines in one JVM.
- **C — Untrusted-input evaluator**: runs Prolog text from end users.

## Verdict

| Dimension | A (embedded) | B (server) | C (untrusted) |
|---|---|---|---|
| Correctness & ISO | CONDITIONAL | NO | NO |
| Robustness | CONDITIONAL | NO | NO |
| Concurrency & global state | CONDITIONAL | **NO** | NO |
| Performance | CONDITIONAL | NO | NO |
| Security | CONDITIONAL | NO | **NO** |
| API / build / ops / **licensing** | **NO** | NO | NO |

**Overall: NOT production-ready as a general/external product today.** It is **conditionally usable
for profile A** (embedded, single-thread, trusted input) once the conditions below are met — but a
**licensing blocker overrides all profiles**.

Finding counts (verified): **9 blocker, 19 high, 15 medium, 3 low** (2 initial blockers were
adversarially downgraded to medium).

## Remediation in progress (findings → tests → fixes)

All findings captured as executable tests in `src/test/java/.../test/audit/ProductionAuditTest.java`
(failing first, then fixed). **Fixed so far (12 tests green, full suite still passing):**

- ✅ **ISS-2025-0338 — sandbox / safe mode** (blocker, security): `Prolog.enableSafeMode()` removes all
  host-touching built-ins (os, ffi, filesystem, network, http, jdbc, persistence — ~deny-by-package) so
  an untrusted program cannot run shell, reflect into the JVM, or touch files/sockets/DBs. Core
  logic/arith still works.
- ✅ **ISS-2025-0339 — inference budget** (blocker, DoS): `Prolog.setInferenceBudget(steps)` aborts a
  runaway query with an **uncatchable** `InferenceLimitException` (a non-PrologException, so untrusted
  `catch/3` cannot swallow it and loop). Bounds CPU.
- ✅ **ISS-2025-0335 — sort/msort/sort-4 on variables** (high, correctness): they failed on any
  non-ground list; now they require a PROPER list, not a ground one, and sort by standard order with
  variables lowest.
- ✅ **ISS-2025-0336 — freeze/2 binding propagation** (high, correctness): the frozen goal now runs on
  the v2 machine's own binding/trail (not the legacy hook), so bindings it makes survive
  (`freeze(X,Y=hello), X=1, Y==hello` succeeds).
- ✅ **ISS-2025-0337 — call/1 & =../2 ISO errors** (medium): a non-callable goal now raises
  `type_error(callable, _)` (or `instantiation_error` for a variable); under-instantiated `=..` raises
  `instantiation_error` instead of a raw non-ISO message.

**Round 2 (robustness, profile C):**
- ✅ **ISS-2025-0341 — deep structures don't crash**: a deep TERM (during resolve) and deeply nested
  untrusted INPUT (during parse) now raise a catchable `resource_error` instead of a raw
  `StackOverflowError` that would crash the embedder.
- ⚠️ **ISS-2025-0340 — first-argument indexing: ATTEMPTED & REVERTED.** Routing v2 clause lookup
  through `KnowledgeBase.getRulesWithFirstArgIndex` dropped all clauses for any predicate whose
  first-arg index was never populated (569 test regressions). Reverted; the KB index must be made
  reliably-built-for-every-predicate before v2 can use it. A clause-selection correctness test (500
  clauses) was kept.
- ℹ️ **legacy↔v2 debug divergence** — already resolved earlier: the IDE debugger now runs on the v2
  engine (ISS-2025-0331).

**Still open (larger / decision items):** the **OSS-license decision** (blocker for third-party use —
not a code fix), per-engine isolation of the remaining **global static state** (server profile),
making the KB **first-arg index** reliable then wiring v2 indexing, and lazy non-deterministic
built-ins (`between/3`, `member/2` materialise eagerly).

## Confirmed blockers (upheld by adversarial verification)

1. **No OSS license** — the repo metadata says *Proprietary, all rights reserved*, with **no LICENSE
   file** and no `<licenses>` in `pom.xml`. Legally this **forbids third-party production use** of the
   source/binary regardless of technical quality. *Decide and add a license first.*
2. **Arbitrary OS command execution from a query** — `shell/1,2`, `shell_output/3` run `/bin/sh -c`
   with goal-controlled strings (`OsPredicates.java:70-71`).
3. **Arbitrary JVM access from a query** — `java_call` / `java_new` give full reflective access to any
   class (`JavaFFI.java:135-161`).
4. **Arbitrary filesystem read/write/delete from a query** — `read_file_to_atom`, `write_atom_to_file`,
   `delete_file`, … (`BuiltInFactory.java:478-492`).

Blockers 2–4 (+ confirmed **network egress**: `tcp_connect`, `http_request`, `jdbc_connect`) mean
**untrusted input (profile C) is categorically unsafe** — a one-line query reads `/etc/passwd`, runs a
shell, opens a socket, or DoSes the JVM. There is **no sandbox / capability model / way to unregister
dangerous built-ins**, and **no CPU/step/time/memory budget** on a query.

## Key high findings (per profile)

**B — concurrency / isolation** (why a multi-tenant server is NOT_READY):
- Process-wide **static mutable state shared across engines/threads**: `StreamManager.currentOutputStream`,
  `Profiler`, `Trace.tracingEnabled`, `PrologFlags` (unsynchronized JVM-wide `HashMap`), the operator
  table / `op/3`. Two engines in one JVM interfere; one engine can't safely serve concurrent threads.
- `ConcurrentPredicates` run parallel goals on **one shared mutable `QuerySolver`** → data race.
- *(Good news, verified:* the `Trail` is `ThreadLocal`, the v2 CLP(FD) store is `ThreadLocal`, and core
  resolution state (`binding`/`trail`/`renameCounter`) is per-`MachineSolver` instance — so the
  isolation gap is narrower than it first looks, but still real for the statics above.)*

**Robustness** (conditions even for profile A):
- **No engine-level solution cap / memory bound**: `prolog.solve(String)` **buffers ALL solutions** —
  an embedder gets OOM on a high/infinite-solution query (the cap exists only in the IDE). A
  `solveStream` lazy API exists and should be used instead.
- **No embedder-facing timeout/cancellation** of a runaway query (only the IDE Stop wires it).
- The default v2 engine can still throw a **raw uncaught `StackOverflowError` on deep TERMS** (not deep
  recursion, which it fixes) and the **parser** has no nesting limit.
- `Prolog.solve(Term)` **bypasses v2** and uses the slow/quadratic/SO-prone legacy path.

**Correctness** (real defects that pass the green suite):
- **`freeze/2` fires its goal but discards the bindings it makes** → coroutining gives wrong answers
  silently (both engines).
- **`sort/2`,`msort/2`,`sort/4` FAIL on lists containing unbound variables** (should sort, vars lowest).
- `call/1` / `=../2` don't raise ISO errors for non-callable/under-instantiated inputs.
- **Engine-divergence trap**: the IDE *Debug Query* runs the **legacy** engine while *Run*/API run v2 —
  debug observations can differ from production; no differential test gates cross-engine equality.
- The "100% ISO core" badge is overstated (the same doc lists 95/90/85% sub-scores) and "20/20 examples"
  is a **crash check, not a correctness oracle** (PASS = CLI exit 0).

**Performance**: default v2 engine **ignores first-argument indexing** (linear clause scan);
nondeterministic built-ins (`between/3`, `member/2`) **eagerly materialize** their whole solution set.

**Ops**: documented test/release scripts (`/*.sh`) are **git-ignored**, so the CI/test harness isn't in
the repo (the core-source gitignore leak was just fixed in v3.3.0).

## Genuine strengths

- **Core logic is correct & trustworthy** for textbook deterministic programs: SLD/backtracking, cut,
  if-then-else, negation, **arithmetic** (ISO int/float distinction, sign rules, BigInteger, exact `=:=`),
  well-formed ISO error terms, exception propagation through `findall`. (Empirically verified, e.g.
  4-queens returns exactly the right boards.)
- **Substantive test suite**: ~700 `@Test`, ~2000 meaningful assertions, only 3 placeholders; 690/690 green.
- **Default v2 engine fixes deep-recursion `StackOverflowError`** (iterative SLD) — a real robustness win.
- **Zero external runtime dependencies** (JUnit test-only) → no transitive-CVE surface; small footprint.
- No Java native **deserialization**; XML parsing disables DOCTYPE (no XXE); `.jpc` is a hand-rolled format.
- Clean configurability via system properties; reproducible Maven/Java-8 build; a **`solveStream` lazy API** exists.

## Conditions to reach "production-ready for profile A (embedded)"

1. **Add an explicit OSS license** (or a commercial license decision). *Hard prerequisite.*
2. Embed via **`solveStream`** + your own **timeout** (run on a thread you can interrupt — cancellation
   works) and a **solution cap**; do not call `solve(String)`/`solve(Term)` for unbounded queries.
3. **Don't expose untrusted input**, and if the host app shouldn't run shell/FFI/file/net, ensure those
   built-ins are not reachable (today there's no switch — would need code to unregister them).
4. Fix the correctness defects you rely on: **freeze binding propagation**, **sort-on-variables**;
   add a **v2-vs-legacy differential** test; point the IDE debugger at v2.
5. Treat it as **single-engine, single-thread per JVM** until the global statics are made per-instance.

## To reach profile B (server) / C (untrusted) — larger work
- B: move the remaining **static mutable state to per-engine** (StreamManager current stream, PrologFlags,
  operator table, Profiler, Trace), fix `ConcurrentPredicates`, add first-arg indexing.
- C: build a **sandbox / capability model** (deny-list FFI/os/file/net/jdbc), a **resource budget**
  (inference/time/memory limits), and parser input limits. This is a substantial subsystem.
