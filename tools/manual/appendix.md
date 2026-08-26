# Appendix A — Prolog flags

`current_prolog_flag(?Flag, ?Value)` reads and `set_prolog_flag(+Flag, +Value)` sets the flags
below. Flags marked *read-only* raise `permission_error(modify, flag, Flag)` when set.

| Flag | Values | Notes |
|---|---|---|
| `bounded` | `false` | read-only; integers are unbounded |
| `max_integer`, `min_integer` | integer | read-only; limits of the fast 64-bit representation |
| `integer_rounding_function` | `toward_zero` | read-only |
| `max_arity` | `unbounded` | read-only |
| `double_quotes` | `codes`, `chars`, `atom`, `string` | how `"text"` is read (default `string`) |
| `unknown` | `error`, `fail`, `warning` | what a call to an undefined procedure does (default `error`) |
| `occurs_check` | `true`, `false` | occurs check in ordinary unification (default `false`) |
| `char_conversion` | `true`, `false` | apply the `char_conversion/2` table when reading |
| `debug` | `on`, `off` | debug mode |
| `syntax_errors` | `error`, `fail`, `quiet` | behaviour of `read/1` on a syntax error |
| `character_escapes` | `true`, `false` | escape processing in quoted atoms |
| `strict_iso` | `true`, `false` | reject non-ISO extensions in `read/1` |
| `dialect`, `prolog_version`, `version`, `version_data` | atom / term | read-only identification (`jprolog`) |
| `encoding` | atom | default text encoding (`utf8`) |
| `argv` | list | command-line arguments |
| `gc`, `optimize`, `traditional`, `write_strings`, `trace`, `initialization`, `stack_limit`, `toplevel_print_options` | see `current_prolog_flag/2` | SWI-compatible flags kept for portability |

A flag takes effect for the text read after it is set (a query is parsed as a whole before it runs):

```prolog
?- set_prolog_flag(double_quotes, codes).
true.

?- X = "ab".
X = [97, 98].

?- set_prolog_flag(unknown, fail), no_such_pred.
false.
```

# Appendix B — Engine configuration and embedding

**Subsystem selection (system properties).** The clean-room implementations are the defaults.
There is **one resolution engine** (`core.engine.v4`, since 4.0.0); the parser, the CLP(FD) solver
and the DCG translator each keep an older implementation that a property set before the engine
starts can select:

| Property | Selects |
|---|---|
| `-Djprolog.parser=legacy` | the recursive-descent parser instead of the operator-precedence parser |
| `-Djprolog.clpfd=legacy` | the first CLP(FD) implementation (read once, when the engine is created) |
| `-Djprolog.dcg=legacy` | the first DCG translator |

Only the literal value `legacy` selects a fallback. `-Djprolog.engine` no longer selects anything:
the recursive engine it could name as `legacy` was deleted in 4.0.0 and the iterative 3.x machine
it could name as `v2` in **4.1.0**, so any value logs a warning and runs the v4 core. The parser and
DCG switches are also available at runtime as `Prolog.setUseV2Parser/Dcg(boolean)`; the CLP(FD)
choice must be made before constructing the engine (or upgraded with `prolog.enableV2Clpfd()`).

**Java API summary** (`it.denzosoft.jprolog.core.engine.Prolog`):

| Method | Purpose |
|---|---|
| `consult(String program)` | load clauses and run directives (throws on the first error) |
| `consultWithDiagnostics(program, fileName)` | per-clause compilation returning every error with its line |
| `solve(String query)` | all solutions as `List<Map<String, Term>>` |
| `solveStream(query, sink)` | lazy enumeration; the sink returns `false` to stop |
| `compileFile("p.pl")`, `consultCompiled("p.jpc")`, `consultSmart("p.pl")` | binary `.jpc` compilation and cached loading |
| `asserta/assertz/retract(String clause)` | modify the database from Java |
| `nbSetval/nbGetval/nbDelete` | global variables from Java |
| `enableSafeMode()` | sandbox: remove every OS, FFI, filesystem, network, HTTP, JDBC, persistence and threading built-in (irreversible for the instance) |
| `setInferenceBudget(long steps)` | abort a query with `InferenceLimitException` after the given number of resolution steps (0 = unlimited); enforced inside meta-calls and worker threads too |
| `getFlags()`, `setTracing(boolean)`, `isTracing()` | per-engine ISO flag store and `trace/0` state (each `Prolog` instance is isolated) |
| `getPredicateIndicatorAtLine(int line)` | map a source line to its clause (IDE breakpoints) |
| `getEngineContext()` | the durable per-engine context: `setDebugController`/`getDebugController` (IDE debugger wiring) and the running query's `ResourceGuard` |
| `getEngineState()`, `getStreams()`, `getOps()` | the engine's own stream table and operator store |
| `residualGoals(solution)` | the constraints still attached to an answer's variables |

**Resource control.** A runaway query stops with `InferenceLimitException` when the budget is
exceeded and with `QueryCancelledException` when the solver thread is interrupted (the IDE Stop
button). Both are plain Java runtime exceptions that a Prolog `catch/3` cannot intercept, so
untrusted programs cannot defeat them. Deep terms or deeply nested input are reported as the
catchable ISO errors `resource_error(stack_overflow)` and `resource_error(parser_nesting)`.

# Appendix C — ISO error terms

Built-in predicates signal errors by throwing `error(Formal, Context)`. `Context` is
`context(Name/Arity, Message)` or a variable. The `Formal` terms used by JProlog:

| Formal term | Raised when |
|---|---|
| `instantiation_error` | an argument that must be bound is a variable |
| `type_error(Type, Culprit)` | an argument has the wrong type (`integer`, `atom`, `callable`, `list`, `evaluable`, …) |
| `domain_error(Domain, Culprit)` | the type is right but the value is outside the domain (`not_less_than_zero`, `operator_priority`, …) |
| `existence_error(Kind, What)` | a procedure, stream, file, variable or source does not exist |
| `permission_error(Action, Type, Culprit)` | e.g. modifying a static procedure, repositioning a stream, accessing a private procedure |
| `representation_error(Flag)` | a value cannot be represented (`character_code`, `max_arity`, `cyclic_term`) |
| `evaluation_error(Error)` | arithmetic: `zero_divisor`, `undefined`, `float_overflow`, `int_overflow`, `negative_shift` |
| `resource_error(Resource)` | `memory`, `stack_overflow`, `parser_nesting` |
| `syntax_error(Message)` | invalid text in `read/1`, `atom_to_term/3`, `number_codes/2`, … |
| `system_error` | an internal failure inside a built-in |

JProlog follows the strict ISO reading: `atom_length(123456, L)` raises `type_error(atom, 123456)`
instead of counting the digits as SWI-Prolog does.

```prolog
?- catch(atom_length(123456, L), error(E, _), true).
E = type_error(atom, 123456).

?- catch(atom_length(X, L), error(Err, _), true).
Err = instantiation_error.

?- catch(foo(1), error(existence_error(procedure, PI), _), true).
PI = foo/1.
```

# Appendix D — Standard order of terms and text conversion summary

Standard order (used by `@<`, `compare/3`, `sort/2`, `msort/2`, `setof/3`, `keysort/2`):
`Var < Number < Atom < String < Compound`. Numbers are compared by value, and when an integer and
a float are equal the float comes first. Compound terms are ordered by arity, then by functor name,
then by arguments left to right.

Text conversions at a glance:

| From \ To | atom | string | codes | chars | number |
|---|---|---|---|---|---|
| atom | — | `atom_string/2` | `atom_codes/2` | `atom_chars/2` | `atom_number/2` |
| string | `string_to_atom/2` | — | `string_codes/2` | `string_chars/2` | `number_string/2` |
| number | `atom_number/2` | `number_string/2` | `number_codes/2` | `number_chars/2` | — |
| any term | `term_to_atom/2` | `with_output_to/2` | — | — | — |

# Appendix E — Libraries not loaded by default

The source tree also documents experimental libraries that are **not** registered in the default
engine and are therefore outside the scope of this manual: expert system (`es_*`), NLP, inference
engine, AI planner, fuzzy logic, Bayesian networks, genetic algorithms, neural networks,
optimisation, simulation, workflow, CLP(R) (`clpr_*`), knowledge graphs, parsing/DSL, Datalog,
RDF, model checking, CHR (`chr_*`), BDI agents (`agent_*`), ASP (`asp_*`), explainable AI, type
inference, theorem proving, symbolic mathematics, meta-interpretation, event calculus (`ec_*`),
probabilistic logic, SAT solving, game playing, term rewriting and description logic (`dl_*`).
See `docs/references/BUILTIN_PREDICATES_REFERENCE.md` sections 29–60 for their descriptions and
`docs/guides/guide-extension.md` for how to register an extension with
`Prolog.registerBuiltInPredicate(name, builtIn)`.
