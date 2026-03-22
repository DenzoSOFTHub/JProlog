# Debug and Tracing Guide for JProlog CLI

This guide explains how to use the debug and tracing features in JProlog from the command line interface.

## Quick Start

```prolog
?- trace.              % Enable tracing (shows all goals)
?- parent(X, Y).       % Now shows CALL/EXIT/FAIL/REDO events
?- notrace.            % Disable tracing
```

## Available Debug Predicates

### trace/0 — Enable Full Tracing

Enables the 4-port tracer. Every goal is displayed with its port:

```prolog
?- trace.
true.
?- append([1,2], [3], X).
  CALL: append([1, 2], [3], _G1)
  CALL: append([2], [3], _G2)
  CALL: append([], [3], _G3)
  EXIT: append([], [3], [3])
  EXIT: append([2], [3], [2, 3])
  EXIT: append([1, 2], [3], [1, 2, 3])
X = [1, 2, 3].
```

### notrace/0 — Disable Tracing

```prolog
?- notrace.
true.
```

### spy/1 — Set Spy Point on a Predicate

Only trace specific predicates instead of everything:

```prolog
?- spy(append/3).      % Spy on append with arity 3
true.
?- spy(member/2).      % Spy on member with arity 2
true.
```

When spy points are set, only those predicates trigger debug events.

### nospy/1 — Remove Spy Point

```prolog
?- nospy(append/3).    % Remove spy point from append/3
true.
```

### leash/1 — Control Which Ports Pause Execution

By default, all 4 ports (CALL, EXIT, FAIL, REDO) are active. You can control which ports trigger pausing:

```prolog
?- leash(full).        % All ports: [call, exit, fail, redo]
?- leash(half).        % Only: [call, redo]
?- leash(loose).       % Only: [call]
?- leash(none).        % No ports — trace prints but doesn't pause
?- leash([call, fail]).% Custom: only CALL and FAIL ports
```

## The 4-Port Box Model

JProlog implements the standard Prolog 4-port debugging model:

```
         +------------------+
  CALL ->|                  |-> EXIT
         |    Predicate     |
  REDO ->|                  |-> FAIL
         +------------------+
```

| Port | When | Meaning |
|------|------|---------|
| **CALL** | A goal is first invoked | "Entering this goal" |
| **EXIT** | A goal succeeds | "This goal found a solution" |
| **FAIL** | A goal fails (no more solutions) | "This goal has no (more) solutions" |
| **REDO** | Backtracking re-enters a goal | "Trying another solution for this goal" |

## Usage Examples

### Example 1: Debugging a Simple Query

```prolog
?- trace.
?- member(X, [a, b, c]).
  CALL: member(_G1, [a, b, c])
  EXIT: member(a, [a, b, c])
X = a ;
  REDO: member(a, [a, b, c])
  CALL: member(_G1, [b, c])
  EXIT: member(b, [b, c])
X = b ;
  REDO: member(b, [b, c])
  CALL: member(_G1, [c])
  EXIT: member(c, [c])
X = c.
?- notrace.
```

### Example 2: Spy on Specific Predicates

```prolog
% Load your program
?- consult('examples/test_01_basic.pl').

% Set spy point
?- spy(parent/2).

% Query — only parent/2 calls shown
?- ancestor(tom, X).
  CALL: parent(tom, _G1)
  EXIT: parent(tom, bob)
...
```

### Example 3: Using Leash for Non-Interactive Tracing

```prolog
% Show all events but don't pause
?- leash(none).
?- trace.
?- append([1,2], [3,4], X).
% All ports printed, execution doesn't stop
```

### Example 4: Debug with Exception Handling

```prolog
?- trace.
?- catch(X is foo, Error, true).
  CALL: catch(is(_G1, foo), _G2, true)
  CALL: is(_G1, foo)
  FAIL: is(_G1, foo)        % Shows the error point
  CALL: true
  EXIT: true
  EXIT: catch(...)
Error = error(type_error(evaluable, foo/0), is/2).
```

## Debug in the IDE

The Swing IDE (`PrologIDE`) provides a graphical debugger with:

- **Step Into** — Execute one goal at a time
- **Step Over** — Execute the current goal completely, pause at the next sibling
- **Step Out** — Run until the current predicate exits
- **Continue** — Run until the next breakpoint
- **Call Stack** panel showing the current execution depth
- **Variable Bindings** panel showing current substitutions

Launch the IDE:
```bash
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
```

Use the Debug menu or Debug panel to set breakpoints and step through execution.

## Combining with Other Features

### Trace + Tabling

Tabling caches results. When tracing tabled predicates, you'll see:
- First call: normal CALL/EXIT sequence
- Cached call: immediate EXIT from cache (no recursive CALL)

```prolog
?- trace.
?- table fib/2.
?- fib(5, X).
% First calls compute normally, subsequent calls hit cache
```

### Trace + Findall

`findall/3` suppresses debug output for the inner goal by default. To see inner goals, spy on the specific predicate:

```prolog
?- spy(member/2).
?- findall(X, member(X, [1,2,3]), Xs).
```

## Tips

1. **Start with spy, not trace** — `trace` generates enormous output. Use `spy/1` on the predicate you're investigating.
2. **Use leash(half)** — Shows CALL and REDO, which is usually enough to understand control flow.
3. **Check bindings** — The EXIT port shows what variables were bound to.
4. **Look for unexpected FAIL** — If a goal fails when you expect it to succeed, the FAIL port shows exactly where.
5. **REDO indicates backtracking** — Multiple REDO events suggest non-deterministic predicates creating many choice points.
