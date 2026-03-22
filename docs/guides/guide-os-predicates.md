# JProlog OS/System Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.os` package provides predicates for
interacting with the operating system and the JVM runtime environment. These
predicates allow Prolog programs to execute shell commands, read environment
variables, query system resources (CPU, memory), obtain process information,
and measure time.

Shell commands are executed via `/bin/sh -c`, making these predicates
Linux/macOS-oriented. All predicates that produce output values do so through
unification with the final argument.

**Source file:**
`src/main/java/it/denzosoft/jprolog/builtin/os/OsPredicates.java`

**Registered in:** `BuiltInFactory` (ISS-2025-0116)

---

## Predicate Reference

### shell/1

```prolog
shell(+Command)
```

**Arguments:**
- `Command` (atom, input) -- Shell command to execute.

**Description:** Executes `Command` via `/bin/sh -c`. Succeeds if the command
exits with code 0, fails otherwise. Standard output and error from the command
are not captured.

**Example:**
```prolog
?- shell('mkdir -p /tmp/test_output').
true.
```

---

### shell2/2

```prolog
shell2(+Command, -ExitCode)
```

**Arguments:**
- `Command` (atom, input) -- Shell command to execute.
- `ExitCode` (number, output) -- The exit code returned by the command.

**Description:** Executes `Command` via `/bin/sh -c` and unifies `ExitCode`
with the process exit code. Always succeeds (even for non-zero exit codes) as
long as unification succeeds.

**Note:** This predicate is registered as `shell2` in the built-in factory,
not `shell/2`. The `/2` variant uses the name `shell2`.

**Example:**
```prolog
?- shell2('ls /nonexistent', Code).
Code = 2.
```

---

### shell_output/3

```prolog
shell_output(+Command, -Output, -ExitCode)
```

**Arguments:**
- `Command` (atom, input) -- Shell command to execute.
- `Output` (atom, output) -- The standard output of the command as an atom.
- `ExitCode` (number, output) -- The exit code of the command.

**Description:** Executes `Command`, captures its standard output into
`Output` (as a single atom with lines separated by newline characters), and
unifies `ExitCode` with the process exit code. Standard error is not captured.

**Example:**
```prolog
?- shell_output('echo hello', Out, Code).
Out = 'hello',
Code = 0.
```

---

### getenv/2

```prolog
getenv(+VarName, -Value)
```

**Arguments:**
- `VarName` (atom, input) -- Name of the environment variable.
- `Value` (atom, output) -- Value of the environment variable.

**Description:** Unifies `Value` with the value of the environment variable
named `VarName`. Fails if the variable is not set.

**Example:**
```prolog
?- getenv('HOME', Home).
Home = '/home/user'.
```

---

### hostname/1

```prolog
hostname(-Name)
```

**Arguments:**
- `Name` (atom, output) -- The hostname of the local machine.

**Description:** Unifies `Name` with the hostname of the machine running the
JVM, as returned by `InetAddress.getLocalHost().getHostName()`.

**Example:**
```prolog
?- hostname(H).
H = 'myserver.local'.
```

---

### pid/1

```prolog
pid(-ProcessId)
```

**Arguments:**
- `ProcessId` (number, output) -- The process ID of the current JVM process.

**Description:** Unifies `ProcessId` with the operating system process ID of
the currently running JVM.

**Example:**
```prolog
?- pid(P).
P = 12345.
```

---

### sleep/1

```prolog
sleep(+Seconds)
```

**Arguments:**
- `Seconds` (number, input) -- Duration to sleep, in seconds (may be
  fractional).

**Description:** Pauses execution for the specified number of seconds. The
argument is multiplied by 1000 and passed to `Thread.sleep()`, so fractional
values (e.g., `0.5` for 500ms) are supported.

**Example:**
```prolog
?- sleep(2).
true.  % resumes after 2 seconds
```

---

### system_time/1

```prolog
system_time(-MillisEpoch)
```

**Arguments:**
- `MillisEpoch` (number, output) -- Current system time in milliseconds since
  the Unix epoch (1970-01-01T00:00:00Z).

**Description:** Unifies `MillisEpoch` with the value of
`System.currentTimeMillis()`.

**Example:**
```prolog
?- system_time(T).
T = 1711036800000.
```

---

### os_name/1

```prolog
os_name(-Name)
```

**Arguments:**
- `Name` (atom, output) -- The name of the operating system.

**Description:** Unifies `Name` with the value of the `os.name` Java system
property (e.g., `'Linux'`, `'Mac OS X'`, `'Windows 10'`).

**Example:**
```prolog
?- os_name(OS).
OS = 'Linux'.
```

---

### cpu_count/1

```prolog
cpu_count(-N)
```

**Arguments:**
- `N` (number, output) -- Number of available processors.

**Description:** Unifies `N` with the number of processors available to the
JVM, as returned by `Runtime.getRuntime().availableProcessors()`.

**Example:**
```prolog
?- cpu_count(C).
C = 8.
```

---

### free_memory/1

```prolog
free_memory(-Bytes)
```

**Arguments:**
- `Bytes` (number, output) -- Free memory in the JVM heap, in bytes.

**Description:** Unifies `Bytes` with the amount of free memory in the JVM
heap, as returned by `Runtime.getRuntime().freeMemory()`.

**Example:**
```prolog
?- free_memory(F).
F = 234881024.
```

---

### total_memory/1

```prolog
total_memory(-Bytes)
```

**Arguments:**
- `Bytes` (number, output) -- Total memory allocated to the JVM heap, in
  bytes.

**Description:** Unifies `Bytes` with the total memory currently allocated to
the JVM, as returned by `Runtime.getRuntime().totalMemory()`. This is the
current heap size, not the maximum.

**Example:**
```prolog
?- total_memory(T).
T = 536870912.
```

---

## Real-World Examples

### Example 1: System Health Monitor

This program gathers CPU, memory, hostname, and OS information and produces
a health report. It flags warnings when memory usage exceeds a threshold.

```prolog
% health_monitor.pl -- System health monitoring with resource checks.

% Main entry point: run a health check and print a report.
health_check :-
    write('=== System Health Report ==='), nl, nl,
    % Gather system identity
    hostname(Host),
    os_name(OS),
    pid(Pid),
    write('Host:       '), write(Host), nl,
    write('OS:         '), write(OS), nl,
    write('JProlog PID: '), write(Pid), nl, nl,
    % Gather resource metrics
    cpu_count(CPUs),
    free_memory(Free),
    total_memory(Total),
    Used is Total - Free,
    UsagePct is (Used * 100) // Total,
    write('CPU Cores:    '), write(CPUs), nl,
    write('Heap Total:   '), format_bytes(Total), nl,
    write('Heap Used:    '), format_bytes(Used),
    write(' ('), write(UsagePct), write('%)'), nl,
    write('Heap Free:    '), format_bytes(Free), nl, nl,
    % Evaluate health
    (   UsagePct > 85
    ->  write('WARNING: Memory usage is critically high!'), nl,
        write('Recommendation: Increase JVM heap size or investigate memory leaks.'), nl
    ;   UsagePct > 70
    ->  write('CAUTION: Memory usage is elevated.'), nl,
        write('Recommendation: Monitor closely.'), nl
    ;   write('STATUS: All systems nominal.'), nl
    ),
    nl,
    % Record timestamp
    system_time(Now),
    write('Report generated at epoch ms: '), write(Now), nl,
    write('=== End Report ==='), nl.

% Format bytes into a human-readable string (KB/MB).
format_bytes(Bytes) :-
    (   Bytes > 1048576
    ->  MB is Bytes // 1048576,
        write(MB), write(' MB')
    ;   Bytes > 1024
    ->  KB is Bytes // 1024,
        write(KB), write(' KB')
    ;   write(Bytes), write(' B')
    ).

% Usage:
%   ?- health_check.
%   === System Health Report ===
%
%   Host:       server-prod-01
%   OS:         Linux
%   JProlog PID: 48291
%
%   CPU Cores:    8
%   Heap Total:   512 MB
%   Heap Used:    156 MB (30%)
%   Heap Free:    356 MB
%
%   STATUS: All systems nominal.
%
%   Report generated at epoch ms: 1711036800000
%   === End Report ===
```

---

### Example 2: Environment-Aware Configuration Loader

This program reads configuration from environment variables, applies
OS-specific defaults, and builds a configuration structure.

```prolog
% env_config.pl -- Load configuration from environment variables with OS-aware defaults.

% load_config(-Config)
% Config is a list of Key=Value pairs.
load_config(Config) :-
    os_name(OS),
    write('Detected OS: '), write(OS), nl,
    % Determine platform-specific defaults
    platform_defaults(OS, Defaults),
    % Override with environment variables where set
    apply_env_overrides(Defaults, Config),
    write('Final configuration:'), nl,
    print_config(Config).

% Platform-specific default settings.
platform_defaults(OS, Defaults) :-
    (   sub_atom(OS, _, _, _, 'Linux')
    ->  Defaults = [
            db_host='localhost',
            db_port='5432',
            log_dir='/var/log/myapp',
            temp_dir='/tmp/myapp',
            shell_path='/bin/bash'
        ]
    ;   sub_atom(OS, _, _, _, 'Mac')
    ->  Defaults = [
            db_host='localhost',
            db_port='5432',
            log_dir='/usr/local/var/log/myapp',
            temp_dir='/tmp/myapp',
            shell_path='/bin/zsh'
        ]
    ;   Defaults = [
            db_host='localhost',
            db_port='5432',
            log_dir='./logs',
            temp_dir='./tmp',
            shell_path='/bin/sh'
        ]
    ).

% For each default Key=Value, check if an environment variable
% MYAPP_<KEY> is set, and use it if so.
apply_env_overrides([], []).
apply_env_overrides([Key=Default|Rest], [Key=Value|Config]) :-
    env_var_name(Key, EnvName),
    (   getenv(EnvName, EnvVal)
    ->  Value = EnvVal,
        write('  [env] '), write(Key), write(' = '), write(EnvVal), nl
    ;   Value = Default,
        write('  [default] '), write(Key), write(' = '), write(Default), nl
    ),
    apply_env_overrides(Rest, Config).

% Convert a config key atom to an environment variable name.
% e.g., db_host -> 'MYAPP_DB_HOST'
env_var_name(Key, EnvName) :-
    atom_string(Key, KeyStr),
    upcase_atom(Key, Upper),
    atom_concat('MYAPP_', Upper, EnvName).

print_config([]).
print_config([K=V|Rest]) :-
    write('  '), write(K), write(' = '), write(V), nl,
    print_config(Rest).

% Usage (with MYAPP_DB_HOST=prod-db.example.com set in environment):
%   ?- load_config(C).
%   Detected OS: Linux
%     [env] db_host = prod-db.example.com
%     [default] db_port = 5432
%     [default] log_dir = /var/log/myapp
%     [default] temp_dir = /tmp/myapp
%     [default] shell_path = /bin/bash
%   Final configuration:
%     db_host = prod-db.example.com
%     db_port = 5432
%     log_dir = /var/log/myapp
%     temp_dir = /tmp/myapp
%     shell_path = /bin/bash
```

---

### Example 3: Build Automation with Shell Commands

This program automates a multi-step build process by executing shell commands,
checking exit codes, capturing output, and collecting results.

```prolog
% build_automation.pl -- Multi-step build pipeline with error handling.

% run_build(+Steps, -Report)
% Steps is a list of step(Name, Command) terms.
% Report is a list of result(Name, Status, Duration) terms.
run_build(Steps, Report) :-
    write('=== Build Pipeline Started ==='), nl,
    system_time(StartTime),
    run_steps(Steps, Report),
    system_time(EndTime),
    TotalMs is EndTime - StartTime,
    nl, write('=== Build Pipeline Complete ==='), nl,
    write('Total time: '), write(TotalMs), write(' ms'), nl,
    summarize_results(Report).

run_steps([], []).
run_steps([step(Name, Command)|Rest], [result(Name, Status, DurationMs)|Results]) :-
    write('['), write(Name), write('] Running: '), write(Command), nl,
    system_time(T1),
    shell_output(Command, Output, ExitCode),
    system_time(T2),
    DurationMs is T2 - T1,
    (   ExitCode =:= 0
    ->  Status = ok,
        write('['), write(Name), write('] PASSED ('),
        write(DurationMs), write(' ms)'), nl
    ;   Status = failed(ExitCode),
        write('['), write(Name), write('] FAILED with exit code '),
        write(ExitCode), nl,
        write('  Output: '), write(Output), nl
    ),
    run_steps(Rest, Results).

summarize_results(Report) :-
    count_status(Report, 0, 0, Passed, Failed),
    Total is Passed + Failed,
    write('Results: '), write(Passed), write('/'), write(Total), write(' passed'),
    (   Failed > 0
    ->  write(' ('), write(Failed), write(' FAILED)'), nl,
        write('Failed steps:'), nl,
        print_failures(Report)
    ;   nl
    ).

count_status([], P, F, P, F).
count_status([result(_, ok, _)|Rest], P0, F0, P, F) :-
    P1 is P0 + 1,
    count_status(Rest, P1, F0, P, F).
count_status([result(_, failed(_), _)|Rest], P0, F0, P, F) :-
    F1 is F0 + 1,
    count_status(Rest, P0, F1, P, F).

print_failures([]).
print_failures([result(Name, failed(Code), Ms)|Rest]) :-
    write('  - '), write(Name), write(' (exit '), write(Code),
    write(', '), write(Ms), write(' ms)'), nl,
    print_failures(Rest).
print_failures([result(_, ok, _)|Rest]) :-
    print_failures(Rest).

% Usage:
%   ?- run_build([
%        step(clean,   'rm -rf build/'),
%        step(compile, 'javac -d build/ src/*.java'),
%        step(test,    'java -cp build/ TestRunner'),
%        step(package, 'jar cf app.jar -C build/ .')
%      ], Report).
%   === Build Pipeline Started ===
%   [clean] Running: rm -rf build/
%   [clean] PASSED (12 ms)
%   [compile] Running: javac -d build/ src/*.java
%   [compile] PASSED (3450 ms)
%   [test] Running: java -cp build/ TestRunner
%   [test] PASSED (890 ms)
%   [package] Running: jar cf app.jar -C build/ .
%   [package] PASSED (156 ms)
%
%   === Build Pipeline Complete ===
%   Total time: 4508 ms
%   Results: 4/4 passed
```

---

### Example 4: Process Management and Resource Monitoring

This program implements a simple process manager that monitors JVM resource
usage over time and optionally triggers garbage collection or alerts.

```prolog
% process_monitor.pl -- Monitor JVM resources and trigger actions on thresholds.

% monitor(+IntervalSec, +Iterations, +MaxUsagePct)
% Poll resource usage every IntervalSec seconds for Iterations rounds.
% Alert if heap usage exceeds MaxUsagePct percent.
monitor(IntervalSec, Iterations, MaxUsagePct) :-
    pid(Pid),
    hostname(Host),
    write('Monitor started on '), write(Host),
    write(' (PID '), write(Pid), write(')'), nl,
    write('Polling every '), write(IntervalSec), write('s for '),
    write(Iterations), write(' iterations'), nl,
    write('Alert threshold: '), write(MaxUsagePct), write('% heap usage'), nl, nl,
    monitor_loop(IntervalSec, Iterations, MaxUsagePct, 1, []).

monitor_loop(_, 0, _, _, Readings) :-
    write('Monitoring complete.'), nl,
    analyze_readings(Readings).
monitor_loop(Interval, N, MaxPct, Round, Readings) :-
    N > 0,
    free_memory(Free),
    total_memory(Total),
    Used is Total - Free,
    Pct is (Used * 100) // Total,
    system_time(Ts),
    write('[Round '), write(Round), write('] '),
    write('Used: '), write(Pct), write('% of '),
    MB is Total // 1048576,
    write(MB), write(' MB heap'), nl,
    % Check threshold
    (   Pct > MaxPct
    ->  write('  ALERT: Usage '), write(Pct),
        write('% exceeds threshold '), write(MaxPct), write('%!'), nl,
        % Attempt to reclaim memory via shell gc trigger
        write('  Requesting garbage collection...'), nl,
        shell('kill -0 $PPID')  % Lightweight no-op to confirm process alive
    ;   true
    ),
    sleep(Interval),
    N1 is N - 1,
    Round1 is Round + 1,
    monitor_loop(Interval, N1, MaxPct, Round1, [Pct|Readings]).

% Analyze collected readings to report min, max, and average.
analyze_readings([]) :- write('No data collected.'), nl.
analyze_readings(Readings) :-
    Readings \= [],
    min_list(Readings, Min),
    max_list(Readings, Max),
    sum_list(Readings, Sum),
    length(Readings, Count),
    Avg is Sum // Count,
    nl, write('=== Resource Summary ==='), nl,
    write('Samples:  '), write(Count), nl,
    write('Min usage: '), write(Min), write('%'), nl,
    write('Max usage: '), write(Max), write('%'), nl,
    write('Avg usage: '), write(Avg), write('%'), nl.

% Usage:
%   ?- monitor(5, 6, 80).
%   Monitor started on server-01 (PID 48291)
%   Polling every 5s for 6 iterations
%   Alert threshold: 80% heap usage
%
%   [Round 1] Used: 34% of 512 MB heap
%   [Round 2] Used: 38% of 512 MB heap
%   [Round 3] Used: 82% of 512 MB heap
%     ALERT: Usage 82% exceeds threshold 80%!
%     Requesting garbage collection...
%   [Round 4] Used: 45% of 512 MB heap
%   [Round 5] Used: 47% of 512 MB heap
%   [Round 6] Used: 49% of 512 MB heap
%   Monitoring complete.
%
%   === Resource Summary ===
%   Samples:  6
%   Min usage: 34%
%   Max usage: 82%
%   Avg usage: 49%
```

---

### Example 5: Cross-Platform Deployment Script

This program implements a deployment workflow that adapts commands based on
the detected OS, executes deployment steps, and writes a deployment log.

```prolog
% deploy.pl -- Cross-platform deployment script using OS detection.

deploy(AppName, Version, TargetDir) :-
    os_name(OS),
    hostname(Host),
    system_time(StartTime),
    write('Deploying '), write(AppName), write(' v'), write(Version), nl,
    write('Target: '), write(Host), write(':'), write(TargetDir), nl,
    write('Platform: '), write(OS), nl, nl,
    % Step 1: Prepare target directory
    deploy_step('Prepare directory',
        OS, TargetDir, PrepStatus),
    % Step 2: Copy artifacts
    atom_concat(AppName, '.jar', JarName),
    deploy_copy_artifact(OS, JarName, TargetDir, CopyStatus),
    % Step 3: Set permissions (Unix only)
    deploy_permissions(OS, TargetDir, JarName, PermStatus),
    % Step 4: Verify deployment
    deploy_verify(TargetDir, JarName, VerifyStatus),
    system_time(EndTime),
    Duration is EndTime - StartTime,
    % Write deployment log
    write_deployment_log(AppName, Version, Host, OS,
        [PrepStatus, CopyStatus, PermStatus, VerifyStatus], Duration).

deploy_step(Label, _OS, TargetDir, ok) :-
    atom_concat('mkdir -p ', TargetDir, Cmd),
    shell_output(Cmd, _, Code),
    Code =:= 0, !,
    write('[OK] '), write(Label), nl.
deploy_step(Label, _, _, failed) :-
    write('[FAIL] '), write(Label), nl.

deploy_copy_artifact(_OS, JarName, TargetDir, ok) :-
    atom_concat('cp dist/', JarName, T1),
    atom_concat(T1, ' ', T2),
    atom_concat(T2, TargetDir, T3),
    atom_concat(T3, '/', Cmd),
    shell_output(Cmd, _, Code),
    Code =:= 0, !,
    write('[OK] Copy artifact '), write(JarName), nl.
deploy_copy_artifact(_, JarName, _, failed) :-
    write('[FAIL] Copy artifact '), write(JarName), nl.

deploy_permissions(OS, TargetDir, JarName, ok) :-
    (   sub_atom(OS, _, _, _, 'Linux') ; sub_atom(OS, _, _, _, 'Mac')
    ), !,
    atom_concat('chmod 755 ', TargetDir, T1),
    atom_concat(T1, '/', T2),
    atom_concat(T2, JarName, Cmd),
    shell_output(Cmd, _, Code),
    (   Code =:= 0
    ->  write('[OK] Set permissions'), nl
    ;   write('[WARN] chmod returned '), write(Code), nl
    ).
deploy_permissions(_, _, _, skipped) :-
    write('[SKIP] Permissions (Windows)'), nl.

deploy_verify(TargetDir, JarName, ok) :-
    atom_concat(TargetDir, '/', T1),
    atom_concat(T1, JarName, FullPath),
    atom_concat('test -f ', FullPath, Cmd),
    shell_output(Cmd, _, Code),
    Code =:= 0, !,
    write('[OK] Artifact verified at '), write(FullPath), nl.
deploy_verify(_, JarName, failed) :-
    write('[FAIL] Artifact '), write(JarName), write(' not found after deploy'), nl.

write_deployment_log(App, Ver, Host, OS, Statuses, Duration) :-
    nl, write('=== Deployment Summary ==='), nl,
    write('Application: '), write(App), write(' v'), write(Ver), nl,
    write('Host: '), write(Host), nl,
    write('OS: '), write(OS), nl,
    write('Duration: '), write(Duration), write(' ms'), nl,
    write('Step results: '), write(Statuses), nl.

% Usage:
%   ?- deploy('myapp', '1.3.2', '/opt/myapp').
%   Deploying myapp v1.3.2
%   Target: server-01:/opt/myapp
%   Platform: Linux
%
%   [OK] Prepare directory
%   [OK] Copy artifact myapp.jar
%   [OK] Set permissions
%   [OK] Artifact verified at /opt/myapp/myapp.jar
%
%   === Deployment Summary ===
%   Application: myapp v1.3.2
%   Host: server-01
%   OS: Linux
%   Duration: 245 ms
%   Step results: [ok,ok,ok,ok]
```
