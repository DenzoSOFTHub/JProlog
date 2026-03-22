# Concurrent Execution Predicates (SWI-Prolog Compatible)

## Package Overview

JProlog's concurrent execution library provides SWI-Prolog compatible predicates for parallel goal evaluation. These predicates allow you to execute multiple Prolog goals simultaneously using Java's `ExecutorService` thread pool, achieving real CPU-level parallelism for independent tasks.

The implementation is in **`ConcurrentPredicates`** (`it.denzosoft.jprolog.builtin.threading.ConcurrentPredicates`), which implements `BuiltInWithContext` for access to the `QuerySolver`. All predicates create fresh bindings maps per thread to avoid data races, while sharing read-only access to the `KnowledgeBase`.

**Key architectural features**:
- Uses `CachedThreadPool` with daemon threads — threads are reused across calls and don't prevent JVM shutdown.
- 60-second timeout per thread to prevent permanent blocking.
- Each thread receives a copy of the current bindings — no shared mutable state.
- Results are collected via `Future<T>` for ordered, type-safe collection.
- `first_solution/3` uses `CompletionService` for efficient "first-to-finish" semantics with cancellation.

**When to use parallel predicates vs sequential**:
- Use parallel predicates when goals are **independent** (no shared variables, no side effects on shared state).
- Parallelism has thread creation overhead — use it for CPU-bound or I/O-bound tasks where the work per element is significant.
- For trivially fast per-element operations (e.g., `atom/1` checks on small lists), sequential `maplist` will be faster.

---

## Predicate Reference

### concurrent/3

```prolog
concurrent(+N, +Goals, +Options)
```

Execute a list of goals using at most N worker threads. All goals must succeed for `concurrent/3` to succeed. This is the most general concurrent execution primitive.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `N`      | integer | `+` | Maximum number of worker threads |
| `Goals`  | list | `+` | List of Prolog goals to execute in parallel |
| `Options` | list | `+` | Options list (currently unused, pass `[]`) |

**Behavior**:
- Creates a fixed thread pool of N threads.
- Submits all goals as tasks.
- Waits for all to complete (with 60s timeout per task).
- Succeeds only if ALL goals succeed. Fails if any goal fails.
- Empty goal list succeeds immediately.
- Thread pool is shut down after completion.

---

### concurrent_maplist/2

```prolog
concurrent_maplist(:Goal, +List)
```

Like `maplist/2` but executes Goal on each element in parallel. Succeeds if `call(Goal, Elem)` succeeds for every element in List.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Goal`   | callable | `:` | Goal to apply to each element |
| `List`   | list | `+` | List of elements to process |

**Behavior**:
- For each element E in List, calls `Goal(E)` in a separate thread.
- Uses the global cached thread pool.
- Succeeds if all calls succeed; fails if any call fails.
- Empty list succeeds immediately.

---

### concurrent_maplist3/3

```prolog
concurrent_maplist3(:Goal, +List, -ResultList)
```

Like `maplist/3` but executes Goal on each element in parallel. Calls `Goal(Elem, Result)` for each element, collecting results in order.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Goal`   | callable | `:` | Goal to apply: `call(Goal, Elem, Result)` |
| `List`   | list | `+` | Input list |
| `ResultList` | list | `-` | Output list, same length as input |

**Behavior**:
- For each element E, creates a fresh variable R and calls `Goal(E, R)` in a thread.
- Collects results in the original list order (not completion order).
- Fails if any call fails.
- Empty list unifies ResultList with `[]`.

---

### concurrent_maplist4/4

```prolog
concurrent_maplist4(:Goal, +List1, +List2, -ResultList)
```

Parallel maplist with two input lists. Calls `Goal(E1, E2, Result)` for corresponding elements.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Goal`   | callable | `:` | Goal to apply: `call(Goal, E1, E2, Result)` |
| `List1`  | list | `+` | First input list |
| `List2`  | list | `+` | Second input list (must have same length) |
| `ResultList` | list | `-` | Output list |

**Behavior**:
- Lists must have the same length (throws error otherwise).
- Results are collected in order.

---

### first_solution/3

```prolog
first_solution(-X, :Goals, +Options)
```

Run Goals in parallel; return the binding of X from whichever goal succeeds first. Remaining goals are cancelled.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `X`      | term | `-` | Template variable — unified with the binding from the winning goal |
| `Goals`  | list | `:` | List of alternative goals to try in parallel |
| `Options` | list | `+` | Options list (currently unused, pass `[]`) |

**Behavior**:
- Submits all goals to the thread pool simultaneously.
- Uses `CompletionService` to detect the first completed task.
- When a goal succeeds, cancels all remaining tasks and returns its bindings.
- Fails if all goals fail or the goal list is empty.
- Useful for OR-parallelism: try multiple strategies, take the first to finish.

---

### concurrent_and/2

```prolog
concurrent_and(+Goals, +Options)
```

Run all goals in parallel. Succeeds only if ALL goals succeed (AND-parallelism).

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Goals`  | list | `+` | List of goals to execute in parallel |
| `Options` | list | `+` | Options list (currently unused, pass `[]`) |

**Behavior**:
- Similar to `concurrent/3` but uses the global thread pool.
- All goals must succeed.

---

### concurrent_or/2

```prolog
concurrent_or(+Goals, -WinnerIndex)
```

Run goals in parallel; succeed with the index (1-based) of the first goal that succeeds.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Goals`  | list | `+` | List of alternative goals |
| `WinnerIndex` | integer | `-` | 1-based index of the first succeeding goal |

**Behavior**:
- Submits all goals simultaneously.
- Returns the index of the first to succeed.
- Cancels remaining goals after the winner is found.
- Fails if all goals fail.

---

## Real-World Examples

### Example 1: Parallel Data Validation

Validate multiple data constraints simultaneously — each check is independent and can run in its own thread.

```prolog
% Data validation predicates
valid_email(Email) :-
    atom_codes(Email, Codes),
    member(64, Codes).  % contains '@'

valid_age(Age) :-
    number(Age), Age >= 0, Age =< 150.

valid_name(Name) :-
    atom(Name), atom_length(Name, Len), Len > 0.

% Validate all fields of a user record in parallel
validate_user(user(Name, Age, Email)) :-
    concurrent_and([
        valid_name(Name),
        valid_age(Age),
        valid_email(Email)
    ], []).

% Usage:
% ?- validate_user(user(alice, 30, 'alice@example.com')).
% true.
%
% ?- validate_user(user('', -1, 'invalid')).
% false.
```

### Example 2: Parallel Search with First Solution

Search multiple databases or strategies simultaneously, taking the first hit.

```prolog
% Simulate searching different "databases"
search_db1(Query, Result) :-
    Query == hello,
    Result = found_in_db1.

search_db2(Query, Result) :-
    Query == world,
    Result = found_in_db2.

search_db3(Query, Result) :-
    atom(Query),
    Result = found_in_db3.

% Search all databases in parallel, return first hit
parallel_search(Query, Result) :-
    first_solution(Result,
        [search_db1(Query, Result),
         search_db2(Query, Result),
         search_db3(Query, Result)],
        []).

% Usage:
% ?- parallel_search(hello, R).
% R = found_in_db1.
%
% ?- parallel_search(anything, R).
% R = found_in_db3.
```

### Example 3: Parallel Numerical Computation

Transform a large list of numbers in parallel — useful when each transformation is computationally expensive.

```prolog
% Expensive computation (simulated)
heavy_computation(X, Result) :-
    R1 is X * X,
    R2 is R1 + X,
    R3 is R2 mod 97,
    Result is R3 * 2 + 1.

% Process a batch of inputs in parallel
process_batch(Inputs, Results) :-
    concurrent_maplist3(heavy_computation, Inputs, Results).

% Usage:
% ?- numlist(1, 1000, Inputs),
%    process_batch(Inputs, Results),
%    length(Results, N).
% N = 1000.
```

### Example 4: Parallel Hypothesis Testing

Test multiple hypotheses simultaneously and report which ones hold.

```prolog
% Hypothesis test: check if a list satisfies a property
all_positive([]).
all_positive([H|T]) :- H > 0, all_positive(T).

all_even([]).
all_even([H|T]) :- 0 is H mod 2, all_even(T).

all_small([]).
all_small([H|T]) :- H < 100, all_small(T).

% Test which hypotheses hold for the data
test_hypotheses(Data) :-
    concurrent(3, [
        (all_positive(Data) -> write('Data is all positive') ; true),
        (all_even(Data) -> write('Data is all even') ; true),
        (all_small(Data) -> write('Data is all small') ; true)
    ], []), nl.

% Usage:
% ?- test_hypotheses([2, 4, 6, 8, 10]).
% Data is all positive Data is all even Data is all small
```

### Example 5: Racing Alternative Algorithms

Compare multiple sorting or solving strategies, take the fastest.

```prolog
% Different "strategies" for solving a problem
strategy_brute_force(Problem, Solution) :-
    Problem = simple,
    Solution = brute_force_result.

strategy_heuristic(Problem, Solution) :-
    Problem = simple,
    Solution = heuristic_result.

strategy_optimal(Problem, Solution) :-
    Problem = simple,
    Solution = optimal_result.

% Race all strategies, return the first to complete
solve_racing(Problem, Solution) :-
    first_solution(Solution,
        [strategy_brute_force(Problem, Solution),
         strategy_heuristic(Problem, Solution),
         strategy_optimal(Problem, Solution)],
        []).

% Usage:
% ?- solve_racing(simple, S).
% S = brute_force_result.  (or whichever finishes first)
```

---

## Thread Safety Notes

1. **Knowledge Base reads are safe**: Multiple threads reading rules and facts concurrently works correctly. The KB is not modified during query solving.

2. **Side effects are NOT safe**: `assert`, `retract`, `write`, and other side-effecting predicates should NOT be used inside parallel goals without external synchronization. Results are undefined.

3. **Independent goals only**: Goals passed to concurrent predicates should not share unbound variables. Each thread gets its own copy of bindings.

4. **Exception propagation**: If a thread throws an exception, it's caught by the `Future.get()` call and re-thrown in the calling thread as a `PrologEvaluationException`.
