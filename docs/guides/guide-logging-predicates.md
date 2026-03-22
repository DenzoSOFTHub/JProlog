# Logging Predicates Guide

## Package Overview

The JProlog logging package provides structured logging capabilities through six built-in predicates. Built on top of Java's `java.util.logging` framework, it supports multiple log levels, file output with append mode, and runtime level configuration.

The logging system uses a singleton `Logger` named `"JProlog"`. By default, logging is set to `INFO` level and outputs to the console via parent handlers. Log output can be redirected to files at any time, and the log level can be changed dynamically during program execution.

**Source**: `it.denzosoft.jprolog.builtin.logging.LoggingPredicates`
**Registered in**: `BuiltInFactory` (lines 479-484)

---

## Predicate Reference

### log_info/1

```prolog
log_info(+Message)
```

Logs a message at the INFO level. INFO is the default active level, so these messages are visible immediately.

| Argument  | Type | Mode | Description                      |
|-----------|------|------|----------------------------------|
| `Message` | atom | `+`  | The message text to log (atom).  |

**Behavior**: Resolves the argument through current bindings, extracts the atom name, and logs it at `Level.INFO`. Always succeeds (adds current bindings to solutions). Throws `PrologEvaluationException` if the argument is not an atom after resolution.

---

### log_warning/1

```prolog
log_warning(+Message)
```

Logs a message at the WARNING level.

| Argument  | Type | Mode | Description                         |
|-----------|------|------|-------------------------------------|
| `Message` | atom | `+`  | The warning message text (atom).    |

**Behavior**: Identical to `log_info/1` but uses `Level.WARNING`. Warning messages are displayed when the log level is set to `warning`, `info`, `debug`, or `all`.

---

### log_error/1

```prolog
log_error(+Message)
```

Logs a message at the ERROR (SEVERE) level.

| Argument  | Type | Mode | Description                       |
|-----------|------|------|-----------------------------------|
| `Message` | atom | `+`  | The error message text (atom).    |

**Behavior**: Logs at `Level.SEVERE`, the highest active log level. Error messages are visible at all active log levels except `off`.

---

### log_debug/1

```prolog
log_debug(+Message)
```

Logs a message at the DEBUG (FINE) level.

| Argument  | Type | Mode | Description                       |
|-----------|------|------|-----------------------------------|
| `Message` | atom | `+`  | The debug message text (atom).    |

**Behavior**: Logs at `Level.FINE`. These messages are only visible when the log level is set to `debug` or `all`. By default (level = `info`), debug messages are suppressed.

---

### log_level/1

```prolog
log_level(+Level)
```

Sets the global logging level. Messages below the configured level are suppressed.

| Argument | Type | Mode | Description                                               |
|----------|------|------|-----------------------------------------------------------|
| `Level`  | atom | `+`  | One of: `debug`, `info`, `warning`, `error`, `off`, `all` |

**Level hierarchy** (from most to least verbose):

| Level     | Java Level    | Shows                           |
|-----------|---------------|---------------------------------|
| `all`     | `Level.ALL`   | All messages                    |
| `debug`   | `Level.FINE`  | debug + info + warning + error  |
| `info`    | `Level.INFO`  | info + warning + error          |
| `warning` | `Level.WARNING` | warning + error               |
| `error`   | `Level.SEVERE` | error only                     |
| `off`     | `Level.OFF`   | No messages                     |

**Behavior**: Case-insensitive level matching. Throws `PrologEvaluationException` for unrecognized level names.

---

### log_to_file/1

```prolog
log_to_file(+FilePath)
```

Redirects log output to a file. The file is opened in append mode, so existing content is preserved.

| Argument   | Type | Mode | Description                              |
|------------|------|------|------------------------------------------|
| `FilePath` | atom | `+`  | Absolute or relative path to the log file. |

**Behavior**: Creates a `FileHandler` with `SimpleFormatter` and adds it to the logger. If a previous file handler was active, it is closed and removed first (only one file handler at a time). Console output via parent handlers remains active alongside the file handler. Throws `PrologEvaluationException` if the file cannot be opened.

**Note**: The Java `FileHandler` supports log rotation patterns in the file path (e.g., `'app_%g.log'` for rotating files), though the rotation count defaults to 1.

---

## Real-World Examples

### Example 1: Application Logging Framework

A structured logging system with configurable levels and file output, suitable for a production application.

```prolog
%% application_logging.pl
%% A structured logging framework for a web service simulation.
%% Demonstrates level-based filtering, file output, and categorized messages.

%% Initialize the logging subsystem: set level and output file.
init_logging(Level, FilePath) :-
    log_level(Level),
    log_to_file(FilePath),
    log_info('logging_subsystem_initialized').

%% Log a structured application event with a category prefix.
%% Category is prepended to the message for filtering in log analysis tools.
log_event(Category, Message) :-
    atom_concat('[', Category, Prefix1),
    atom_concat(Prefix1, '] ', Prefix2),
    atom_concat(Prefix2, Message, FullMessage),
    log_info(FullMessage).

%% Log an application startup sequence with multiple subsystem checks.
startup_sequence :-
    init_logging(info, 'app_server.log'),
    log_info('========== APPLICATION STARTUP =========='),
    log_event(config, 'loading_configuration_from_disk'),
    check_subsystem(database),
    check_subsystem(cache),
    check_subsystem(auth),
    log_info('========== STARTUP COMPLETE ==========').

%% Simulate subsystem health checks with appropriate log levels.
check_subsystem(database) :-
    log_debug('attempting_database_connection_on_port_5432'),
    log_event(health, 'database_connection_established'),
    !.
check_subsystem(cache) :-
    log_debug('probing_cache_server_on_port_6379'),
    log_warning('cache_server_not_responding_using_local_fallback'),
    !.
check_subsystem(auth) :-
    log_event(health, 'auth_service_verified'),
    !.
check_subsystem(Service) :-
    atom_concat('unknown_subsystem_', Service, Msg),
    log_error(Msg).

%% Usage:
%% ?- startup_sequence.
%% Output to app_server.log:
%%   INFO: logging_subsystem_initialized
%%   INFO: ========== APPLICATION STARTUP ==========
%%   INFO: [config] loading_configuration_from_disk
%%   INFO: [health] database_connection_established
%%   WARNING: cache_server_not_responding_using_local_fallback
%%   INFO: [health] auth_service_verified
%%   INFO: ========== STARTUP COMPLETE ==========
```

---

### Example 2: Audit Trail System

Logs user actions with timestamps to a persistent audit file, suitable for compliance tracking.

```prolog
%% audit_trail.pl
%% Records user actions with timestamps to an audit log file.
%% Useful for compliance, security monitoring, and activity tracking.

%% Database of user roles for authorization checks.
user_role(admin_01, admin).
user_role(user_42, standard).
user_role(user_73, standard).
user_role(auditor_05, auditor).

%% Sensitive operations that require audit logging.
sensitive_op(delete_record).
sensitive_op(modify_permissions).
sensitive_op(export_data).
sensitive_op(view_audit_log).

%% Initialize the audit subsystem with a dedicated log file.
init_audit(AuditFile) :-
    log_to_file(AuditFile),
    log_level(info),
    log_info('audit_trail_initialized').

%% Record an audited action. Logs the user, action, and resource.
%% Sensitive operations get a WARNING-level entry for easy filtering.
audit_action(User, Action, Resource) :-
    atom_concat(User, ':', Part1),
    atom_concat(Part1, Action, Part2),
    atom_concat(Part2, ':', Part3),
    atom_concat(Part3, Resource, AuditEntry),
    ( sensitive_op(Action) ->
        log_warning(AuditEntry)
    ;
        log_info(AuditEntry)
    ).

%% Verify authorization before performing an action.
%% Unauthorized attempts are logged at ERROR level.
authorized_action(User, Action, Resource) :-
    ( user_role(User, admin) ->
        audit_action(User, Action, Resource)
    ; user_role(User, auditor), Action = view_audit_log ->
        audit_action(User, Action, Resource)
    ; sensitive_op(Action) ->
        atom_concat('UNAUTHORIZED_ACCESS_ATTEMPT:', User, ErrPart1),
        atom_concat(ErrPart1, ':', ErrPart2),
        atom_concat(ErrPart2, Action, ErrMsg),
        log_error(ErrMsg),
        fail
    ;
        audit_action(User, Action, Resource)
    ).

%% Simulate a day of user activity for audit purposes.
simulate_workday :-
    init_audit('audit_trail.log'),
    authorized_action(admin_01, delete_record, 'customer_db.record_991'),
    authorized_action(user_42, read_data, 'reports.quarterly_q3'),
    authorized_action(auditor_05, view_audit_log, 'audit_2025'),
    ( authorized_action(user_73, modify_permissions, 'acl.group_finance')
    ; log_info('unauthorized_action_blocked_for_user_73')
    ),
    log_info('workday_audit_complete').

%% Usage:
%% ?- simulate_workday.
%% Audit file will contain entries like:
%%   INFO: audit_trail_initialized
%%   WARNING: admin_01:delete_record:customer_db.record_991
%%   INFO: user_42:read_data:reports.quarterly_q3
%%   INFO: auditor_05:view_audit_log:audit_2025
%%   ERROR: UNAUTHORIZED_ACCESS_ATTEMPT:user_73:modify_permissions
%%   INFO: unauthorized_action_blocked_for_user_73
%%   INFO: workday_audit_complete
```

---

### Example 3: Debug Tracing for Specific Modules

Enable verbose debug logging for targeted modules while keeping other output quiet.

```prolog
%% debug_tracing.pl
%% Selective debug tracing for Prolog modules.
%% Demonstrates toggling verbose output for specific subsystems.

%% Track which modules have debug tracing enabled.
:- dynamic trace_enabled/1.

%% Enable debug tracing for a named module.
enable_trace(Module) :-
    ( trace_enabled(Module) ->
        true
    ;
        assert(trace_enabled(Module))
    ),
    log_level(debug),
    atom_concat('trace_enabled_for_module:', Module, Msg),
    log_debug(Msg).

%% Disable debug tracing for a named module.
disable_trace(Module) :-
    retract(trace_enabled(Module)),
    atom_concat('trace_disabled_for_module:', Module, Msg),
    log_info(Msg),
    %% If no modules are traced, raise log level back to info.
    ( trace_enabled(_) ->
        true
    ;
        log_level(info),
        log_info('all_traces_disabled_reverting_to_info_level')
    ).

%% Conditional debug log: only emits if tracing is active for the module.
trace_log(Module, Message) :-
    ( trace_enabled(Module) ->
        atom_concat('[TRACE:', Module, P1),
        atom_concat(P1, '] ', P2),
        atom_concat(P2, Message, FullMsg),
        log_debug(FullMsg)
    ;
        true  % Silently succeed when tracing is off
    ).

%% Simulate a parser module with trace points.
parse_expression(Expr, Result) :-
    trace_log(parser, 'entering_parse_expression'),
    atom_concat('input_expression:', Expr, TraceInput),
    trace_log(parser, TraceInput),
    ( Expr = '2+3' ->
        Result = 5,
        trace_log(parser, 'recognized_addition_operator'),
        trace_log(parser, 'result_computed:5')
    ;
        Result = error,
        trace_log(parser, 'unrecognized_expression_format')
    ),
    trace_log(parser, 'exiting_parse_expression').

%% Simulate a database module with trace points.
db_query(Table, Key, Value) :-
    trace_log(database, 'entering_db_query'),
    atom_concat('lookup:', Table, T1),
    atom_concat(T1, '.', T2),
    atom_concat(T2, Key, TraceKey),
    trace_log(database, TraceKey),
    ( Table = users, Key = id_42 ->
        Value = 'John Smith',
        trace_log(database, 'record_found')
    ;
        Value = not_found,
        trace_log(database, 'record_not_found')
    ).

%% Demo: trace only the parser, leave database quiet.
demo_selective_tracing :-
    log_to_file('trace_output.log'),
    log_info('=== starting selective trace demo ==='),
    enable_trace(parser),
    parse_expression('2+3', R1),
    db_query(users, id_42, R2),
    log_info('=== now enabling database trace ==='),
    enable_trace(database),
    db_query(users, id_99, R3),
    disable_trace(parser),
    disable_trace(database),
    log_info('=== demo complete ===').

%% Usage:
%% ?- demo_selective_tracing.
%% Only parser trace messages appear initially; database traces
%% appear only after enable_trace(database) is called.
```

---

### Example 4: Error Reporting Pipeline

Capture errors during batch processing, log them to file, and generate a summary report.

```prolog
%% error_reporting.pl
%% Batch processing pipeline that captures errors, logs them,
%% and produces a summary count at the end.

:- dynamic error_count/1.
:- dynamic warning_count/1.

%% Initialize error reporting counters.
init_error_report(LogFile) :-
    log_to_file(LogFile),
    log_level(all),
    ( retract(error_count(_)) -> true ; true ),
    ( retract(warning_count(_)) -> true ; true ),
    assert(error_count(0)),
    assert(warning_count(0)),
    log_info('error_reporting_pipeline_initialized').

%% Record an error and increment the counter.
record_error(Source, Description) :-
    atom_concat('ERROR[', Source, P1),
    atom_concat(P1, ']: ', P2),
    atom_concat(P2, Description, Msg),
    log_error(Msg),
    retract(error_count(N)),
    N1 is N + 1,
    assert(error_count(N1)).

%% Record a warning and increment the counter.
record_warning(Source, Description) :-
    atom_concat('WARN[', Source, P1),
    atom_concat(P1, ']: ', P2),
    atom_concat(P2, Description, Msg),
    log_warning(Msg),
    retract(warning_count(N)),
    N1 is N + 1,
    assert(warning_count(N1)).

%% Process a batch of data records. Each record may succeed or fail.
process_record(Record) :-
    ( validate_record(Record) ->
        atom_concat('processed_record:', Record, Msg),
        log_debug(Msg)
    ;
        atom_concat('validation_failed_for:', Record, Desc),
        record_error(batch_processor, Desc)
    ).

%% Validation rules: records must match expected patterns.
validate_record(rec_ok_1).
validate_record(rec_ok_2).
validate_record(rec_ok_3).
%% rec_bad_1, rec_bad_2 have no matching clause and will fail validation.

%% Generate a summary report after processing.
generate_summary :-
    error_count(Errors),
    warning_count(Warnings),
    number_codes(Errors, EC), atom_codes(EA, EC),
    number_codes(Warnings, WC), atom_codes(WA, WC),
    atom_concat('=== SUMMARY: errors=', EA, S1),
    atom_concat(S1, ' warnings=', S2),
    atom_concat(S2, WA, S3),
    atom_concat(S3, ' ===', Summary),
    log_info(Summary).

%% Run the full batch processing pipeline.
run_batch_pipeline :-
    init_error_report('batch_errors.log'),
    log_info('starting_batch_of_5_records'),
    process_record(rec_ok_1),
    process_record(rec_bad_1),
    process_record(rec_ok_2),
    process_record(rec_bad_2),
    process_record(rec_ok_3),
    record_warning(batch_processor, 'batch_took_longer_than_expected'),
    generate_summary,
    log_info('batch_pipeline_finished').

%% Usage:
%% ?- run_batch_pipeline.
%% batch_errors.log will contain:
%%   INFO: error_reporting_pipeline_initialized
%%   INFO: starting_batch_of_5_records
%%   FINE: processed_record:rec_ok_1
%%   SEVERE: ERROR[batch_processor]: validation_failed_for:rec_bad_1
%%   FINE: processed_record:rec_ok_2
%%   SEVERE: ERROR[batch_processor]: validation_failed_for:rec_bad_2
%%   FINE: processed_record:rec_ok_3
%%   WARNING: WARN[batch_processor]: batch_took_longer_than_expected
%%   INFO: === SUMMARY: errors=2 warnings=1 ===
%%   INFO: batch_pipeline_finished
```

---

### Example 5: Performance Monitoring

Log timing of operations and compute basic statistics to identify bottlenecks.

```prolog
%% performance_monitoring.pl
%% Measures elapsed time for operations and logs performance data.
%% Uses Prolog's built-in timing to detect slow operations.

:- dynamic perf_entry/2.  % perf_entry(OperationName, ElapsedMs)

%% Clear all collected performance data.
reset_perf_data :-
    retractall(perf_entry(_, _)).

%% Execute a goal and log its execution time.
%% Uses get_time/1 (if available) or walltime for measurement.
timed_operation(Name, Goal) :-
    log_debug(Name),
    statistics(walltime, [Start|_]),
    ( call(Goal) -> true ; true ),
    statistics(walltime, [End|_]),
    Elapsed is End - Start,
    assert(perf_entry(Name, Elapsed)),
    number_codes(Elapsed, EC), atom_codes(EA, EC),
    atom_concat(Name, ':elapsed_ms=', P1),
    atom_concat(P1, EA, PerfMsg),
    ( Elapsed > 100 ->
        log_warning(PerfMsg)
    ;
        log_info(PerfMsg)
    ).

%% Compute and log summary statistics for all recorded operations.
log_perf_summary :-
    findall(E, perf_entry(_, E), Times),
    length(Times, Count),
    ( Count > 0 ->
        sum_list(Times, Total),
        Max is max(Total, 1),
        number_codes(Count, CC), atom_codes(CA, CC),
        number_codes(Total, TC), atom_codes(TA, TC),
        atom_concat('perf_summary:operations=', CA, S1),
        atom_concat(S1, ',total_ms=', S2),
        atom_concat(S2, TA, Summary),
        log_info(Summary)
    ;
        log_info('perf_summary:no_operations_recorded')
    ).

%% Helper: sum a list of numbers.
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.

%% Simulated workload operations of varying cost.
operation_fast :- true.
operation_medium :- between(1, 1000, _), fail ; true.
operation_slow :- between(1, 50000, _), fail ; true.

%% Run a performance profiling session.
run_perf_session :-
    log_to_file('perf_monitor.log'),
    log_level(debug),
    reset_perf_data,
    log_info('=== performance profiling session start ==='),
    timed_operation(op_fast_lookup, operation_fast),
    timed_operation(op_medium_compute, operation_medium),
    timed_operation(op_slow_aggregate, operation_slow),
    timed_operation(op_fast_validate, operation_fast),
    log_perf_summary,
    log_info('=== performance profiling session end ===').

%% Usage:
%% ?- run_perf_session.
%% perf_monitor.log will contain timing entries like:
%%   INFO: === performance profiling session start ===
%%   INFO: op_fast_lookup:elapsed_ms=0
%%   INFO: op_medium_compute:elapsed_ms=12
%%   WARNING: op_slow_aggregate:elapsed_ms=450
%%   INFO: op_fast_validate:elapsed_ms=0
%%   INFO: perf_summary:operations=4,total_ms=462
%%   INFO: === performance profiling session end ===
```
