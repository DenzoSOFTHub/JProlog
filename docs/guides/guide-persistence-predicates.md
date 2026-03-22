# Persistence Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.persistence` package provides predicates for saving, loading, exporting, and restoring the JProlog clause database. It bridges the gap between Prolog's in-memory knowledge base and persistent storage, enabling:

- Saving the entire clause database to Prolog source files and loading them back
- Selective export of individual predicates by functor/arity
- JSON export and import for interoperability with other systems
- In-memory snapshots for transactional rollback patterns
- Marking predicates as "persistent" for automatic tracking
- Clearing all dynamic clauses from the database

This package implements the `BuiltInWithContext` interface because it needs access to the `Prolog` engine instance to read and manipulate the knowledge base. The engine's rules are serialized to standard Prolog text format (`head :- body.`) for `.pl` files, and to a JSON array format for `.json` files.

Snapshots are stored in a static `ConcurrentHashMap` keyed by auto-generated handles of the form `snapshot_N`. They persist for the lifetime of the JVM process.

**Source file**: `src/main/java/it/denzosoft/jprolog/builtin/persistence/PersistencePredicates.java`

---

## Predicate Reference

### db_save(+Filename)

Save the entire clause database to a Prolog source file.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Filename | atom | input | File path to write (e.g., `'backup.pl'`) |

Writes all rules in the knowledge base as standard Prolog clauses. Facts are written as `head.` and rules as `head :- body1, body2, ...`.

**Errors**: Throws `evaluation_error` if the argument is not an atom or if a file I/O error occurs.

```prolog
?- db_save('my_database.pl').
true.
```

---

### db_load(+Filename)

Load clauses from a Prolog source file into the current database.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Filename | atom | input | File path to read (e.g., `'backup.pl'`) |

Uses `engine.consult()` to parse and add all clauses from the file. Existing clauses are not removed; the loaded clauses are added to the current database.

**Errors**: Throws `evaluation_error` if the file does not exist, is unreadable, or contains syntax errors.

```prolog
?- db_load('my_database.pl').
true.
```

---

### db_save_predicate(+Functor/Arity, +Filename)

Save only the clauses of a specific predicate to a file.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Functor/Arity | compound | input | Predicate indicator (e.g., `employee/3`) |
| Filename | atom | input | File path to write |

Filters the knowledge base for rules whose head matches the given functor and arity, then writes only those clauses.

**Errors**: Throws `evaluation_error` if the first argument is not in `Functor/Arity` format, if functor is not an atom, or if arity is not a number.

```prolog
?- db_save_predicate(employee/3, 'employees.pl').
true.
```

---

### persist(+Functor/Arity)

Mark a predicate as persistent. When marked, the system records a backing file path of the form `functor_arity.pl`.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Functor/Arity | compound | input | Predicate indicator to mark as persistent |

The persistent marking is stored in a static map accessible via `getPersistentPredicates()`. This can be used by external code (e.g., custom assert/retract wrappers) to trigger automatic saves.

**Errors**: Throws `evaluation_error` if the argument is not in `Functor/Arity` format.

```prolog
?- persist(customer/4).
true.
% Backing file will be: customer_4.pl
```

---

### unpersist(+Functor/Arity)

Remove the persistence marking from a predicate.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Functor/Arity | compound | input | Predicate indicator to unmark |

**Errors**: Throws `evaluation_error` if the argument is not in `Functor/Arity` format. Succeeds even if the predicate was not previously marked.

```prolog
?- unpersist(customer/4).
true.
```

---

### db_export_json(+Filename)

Export the entire clause database as a JSON file.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Filename | atom | input | File path for JSON output (e.g., `'db.json'`) |

The output format is a JSON array of objects, each with `"head"` (string) and `"body"` (array of strings) fields:

```json
[
  {"head": "parent(tom, bob)", "body": []},
  {"head": "ancestor(X, Y)", "body": ["parent(X, Y)"]},
  {"head": "ancestor(X, Y)", "body": ["parent(X, Z)", "ancestor(Z, Y)"]}
]
```

Facts have an empty body array. Strings are properly escaped for JSON.

**Errors**: Throws `evaluation_error` on file I/O errors.

```prolog
?- db_export_json('knowledge_base.json').
true.
```

---

### db_import_json(+Filename)

Import clauses from a JSON file into the current database.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Filename | atom | input | File path of JSON to import |

Expects the same JSON format produced by `db_export_json/1`. Parses each object, reconstructs Prolog clauses (facts or rules), and consults them into the engine. Existing clauses are not removed.

**Errors**: Throws `evaluation_error` on file I/O or parse errors.

```prolog
?- db_import_json('knowledge_base.json').
true.
```

---

### db_snapshot(-Handle)

Take an in-memory snapshot of the current database state.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Handle | atom | output | Snapshot handle (e.g., `snapshot_1`) for later restoration |

The snapshot captures a serialized copy of all current rules as Prolog text. Handles are auto-generated and unique across the JVM session. Snapshots persist in memory until the JVM terminates.

```prolog
?- db_snapshot(S).
S = snapshot_1.
```

---

### db_restore(+Handle)

Restore the database to a previously saved snapshot. This is a destructive operation: all current clauses are cleared and replaced with the snapshot content.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Handle | atom | input | Snapshot handle returned by `db_snapshot/1` |

First calls `db_clear` internally (abolishes all user-defined predicates), then consults the saved Prolog text from the snapshot.

**Errors**: Throws `evaluation_error` if the handle is unknown.

```prolog
?- db_restore(snapshot_1).
true.
```

---

### db_clear

Clear all dynamic (user-defined) clauses from the database. Takes no arguments.

This predicate iterates over all rules currently in the knowledge base, collects their predicate indicators (functor/arity), and abolishes each predicate via `KnowledgeBase.abolishPredicate/2` (accessed reflectively).

**Warning**: This removes all user-defined clauses. Built-in predicates are not affected.

```prolog
?- db_clear.
true.
```

---

## Real-World Examples

### Example 1: Knowledge Base Backup/Restore System with Versioned Snapshots

A complete backup manager that maintains multiple named versions of the database, allows listing available backups, and supports restoring to any previous version.

```prolog
% ============================================================
% Versioned Backup Manager
% Maintains named snapshots with timestamps and descriptions.
% Supports backup, restore, list, and diff operations.
% ============================================================

:- dynamic backup_registry/3.  % backup_registry(Name, Handle, Description)
:- dynamic backup_counter/1.
backup_counter(0).

% ---- Create a named backup ----

create_backup(Name, Description) :-
    (   backup_registry(Name, _, _)
    ->  write('Error: backup "'), write(Name),
        write('" already exists. Use a different name.'), nl, fail
    ;   true
    ),
    db_snapshot(Handle),
    assert(backup_registry(Name, Handle, Description)),
    increment_backup_counter,
    write('Backup "'), write(Name), write('" created ('),
    write(Handle), write(')'), nl.

% ---- Restore from a named backup ----

restore_backup(Name) :-
    (   backup_registry(Name, Handle, _Desc)
    ->  % First, save current state as auto-backup before restore
        atom_concat('pre_restore_', Name, AutoName),
        (   backup_registry(AutoName, _, _)
        ->  true  % Auto-backup already exists
        ;   db_snapshot(AutoHandle),
            assert(backup_registry(AutoName, AutoHandle, 'Auto-backup before restore'))
        ),
        db_restore(Handle),
        % Re-assert the backup registry since db_restore cleared it
        reassert_registry,
        write('Database restored to "'), write(Name), write('".'), nl,
        write('Pre-restore state saved as "'), write(AutoName), write('".'), nl
    ;   write('Error: no backup named "'), write(Name), write('".'), nl,
        fail
    ).

% ---- List all available backups ----

list_backups :-
    write('=== Available Backups ==='), nl,
    forall(
        backup_registry(Name, Handle, Desc),
        (write('  '), write(Name), write(' ('), write(Handle),
         write(') - '), write(Desc), nl)
    ),
    backup_counter(Count),
    write('Total: '), write(Count), write(' backups'), nl.

% ---- Save database to disk with a named backup ----

save_to_disk(Name, Directory) :-
    atom_concat(Directory, '/', DirSlash),
    atom_concat(DirSlash, Name, PathBase),
    atom_concat(PathBase, '.pl', PrologFile),
    atom_concat(PathBase, '.json', JsonFile),
    db_save(PrologFile),
    db_export_json(JsonFile),
    write('Saved to: '), write(PrologFile), nl,
    write('Exported: '), write(JsonFile), nl.

% ---- Load from disk ----

load_from_disk(Filename) :-
    % Create a safety backup first
    create_backup(pre_load, 'Auto-backup before loading from disk'),
    db_load(Filename),
    write('Loaded: '), write(Filename), nl.

% ---- Helpers ----

increment_backup_counter :-
    retract(backup_counter(N)),
    N1 is N + 1,
    assert(backup_counter(N1)).

% After db_restore clears everything, we need to re-populate
% the backup registry from a saved copy. In practice, we keep
% the registry in the static Java map (snapshots), but we also
% need the Prolog-side registry for listing.
reassert_registry :-
    % The backup_registry facts are lost after db_restore.
    % A production system would save registry metadata externally.
    % For this example, we note that the registry is rebuilt on load.
    true.

% ============================================================
% Usage scenario: manage an employee database
% ============================================================

% Seed data
:- dynamic employee/4.  % employee(Id, Name, Department, Salary)

setup_sample_data :-
    assert(employee(1, 'Alice', engineering, 95000)),
    assert(employee(2, 'Bob', marketing, 72000)),
    assert(employee(3, 'Carol', engineering, 105000)),
    assert(employee(4, 'Dave', sales, 68000)),
    assert(employee(5, 'Eve', engineering, 112000)).

% Full workflow:
%
% ?- setup_sample_data.
% ?- create_backup(v1_initial, 'Initial employee data').
% Backup "v1_initial" created (snapshot_1)
%
% % Make changes
% ?- retract(employee(2, _, _, _)),
%    assert(employee(2, 'Bob', engineering, 85000)).
% ?- assert(employee(6, 'Frank', marketing, 70000)).
%
% ?- create_backup(v2_reorg, 'After department reorganization').
% Backup "v2_reorg" created (snapshot_2)
%
% ?- list_backups.
% === Available Backups ===
%   v1_initial (snapshot_1) - Initial employee data
%   v2_reorg (snapshot_2) - After department reorganization
% Total: 2 backups
%
% % Oops, need to go back
% ?- restore_backup(v1_initial).
% Database restored to "v1_initial".
%
% % Save to disk for external archival
% ?- save_to_disk(quarterly_backup, '/tmp/backups').
% Saved to: /tmp/backups/quarterly_backup.pl
% Exported: /tmp/backups/quarterly_backup.json
```

---

### Example 2: Data Migration -- Export to JSON, Transform, Import New Schema

This example demonstrates exporting data in one schema, transforming it, and importing it under a new schema -- a common ETL (Extract-Transform-Load) pattern.

```prolog
% ============================================================
% Data Migration Pipeline
% Migrates from an old schema (person/3) to a new schema
% (contact/4) by exporting to JSON, transforming, and importing.
% ============================================================

:- dynamic person/3.       % OLD schema: person(Name, Phone, City)
:- dynamic contact/4.      % NEW schema: contact(Id, FullName, Phone, Location)
:- dynamic migration_log/3. % migration_log(Step, Status, Detail)

% ---- Old schema data ----

setup_old_data :-
    assert(person('Alice Smith', '555-0101', 'New York')),
    assert(person('Bob Jones', '555-0202', 'Los Angeles')),
    assert(person('Carol White', '555-0303', 'Chicago')),
    assert(person('Dave Brown', '555-0404', 'New York')),
    assert(person('Eve Davis', '555-0505', 'San Francisco')).

% ---- Migration pipeline ----

run_migration(ExportFile, ImportFile) :-
    write('=== Starting Data Migration ==='), nl,

    % Step 1: Pre-migration snapshot for safety
    write('Step 1: Creating pre-migration snapshot...'), nl,
    db_snapshot(PreSnap),
    assert(migration_log(snapshot, ok, PreSnap)),

    % Step 2: Export current data to JSON
    write('Step 2: Exporting to JSON...'), nl,
    db_export_json(ExportFile),
    assert(migration_log(export, ok, ExportFile)),
    write('  Exported to: '), write(ExportFile), nl,

    % Step 3: Count records before migration
    findall(N, person(N, _, _), OldNames),
    length(OldNames, OldCount),
    write('  Records in old schema: '), write(OldCount), nl,

    % Step 4: Transform data -- create new schema records
    write('Step 3: Transforming to new schema...'), nl,
    transform_all_records(1, TransformCount),
    write('  Transformed: '), write(TransformCount), write(' records'), nl,

    % Step 5: Remove old schema data
    write('Step 4: Removing old schema...'), nl,
    retractall(person(_, _, _)),
    assert(migration_log(cleanup, ok, 'Old person/3 records removed')),

    % Step 6: Save the new schema only
    write('Step 5: Saving new schema to Prolog file...'), nl,
    db_save_predicate(contact/4, ImportFile),
    assert(migration_log(save_new, ok, ImportFile)),
    write('  Saved to: '), write(ImportFile), nl,

    % Step 7: Verify
    write('Step 6: Verifying migration...'), nl,
    verify_migration(OldCount),

    write('=== Migration Complete ==='), nl.

% ---- Transform old records to new schema ----

transform_all_records(NextId, Count) :-
    findall(
        person(Name, Phone, City),
        person(Name, Phone, City),
        Persons
    ),
    transform_list(Persons, NextId, Count).

transform_list([], _, 0).
transform_list([person(Name, Phone, City)|Rest], Id, Count) :-
    % Transform: add Id, rename City to Location with region prefix
    city_to_location(City, Location),
    assert(contact(Id, Name, Phone, Location)),
    NextId is Id + 1,
    transform_list(Rest, NextId, RestCount),
    Count is RestCount + 1.

% Enrich city names with region during migration
city_to_location('New York', 'US-East/New York').
city_to_location('Los Angeles', 'US-West/Los Angeles').
city_to_location('Chicago', 'US-Central/Chicago').
city_to_location('San Francisco', 'US-West/San Francisco').
city_to_location(City, City).  % Default: keep unchanged

% ---- Verification ----

verify_migration(ExpectedCount) :-
    findall(Id, contact(Id, _, _, _), Ids),
    length(Ids, ActualCount),
    (   ActualCount =:= ExpectedCount
    ->  write('  PASS: '), write(ActualCount),
        write(' contacts migrated successfully.'), nl,
        assert(migration_log(verify, ok, 'Count matches'))
    ;   write('  FAIL: Expected '), write(ExpectedCount),
        write(' but found '), write(ActualCount), nl,
        assert(migration_log(verify, fail, 'Count mismatch'))
    ).

% ---- Rollback if needed ----

rollback_migration :-
    (   migration_log(snapshot, ok, Handle)
    ->  write('Rolling back to pre-migration state...'), nl,
        db_restore(Handle),
        write('Rollback complete.'), nl
    ;   write('No pre-migration snapshot found.'), nl,
        fail
    ).

% ---- Print migration log ----

show_migration_log :-
    write('=== Migration Log ==='), nl,
    forall(
        migration_log(Step, Status, Detail),
        (write('  ['), write(Status), write('] '),
         write(Step), write(': '), write(Detail), nl)
    ).

% Usage:
% ?- setup_old_data.
% ?- run_migration('/tmp/old_data.json', '/tmp/contacts.pl').
% === Starting Data Migration ===
% Step 1: Creating pre-migration snapshot...
% Step 2: Exporting to JSON...
%   Exported to: /tmp/old_data.json
%   Records in old schema: 5
% Step 3: Transforming to new schema...
%   Transformed: 5 records
% Step 4: Removing old schema...
% Step 5: Saving new schema to Prolog file...
%   Saved to: /tmp/contacts.pl
% Step 6: Verifying migration...
%   PASS: 5 contacts migrated successfully.
% === Migration Complete ===
%
% ?- contact(Id, Name, Phone, Location).
% Id = 1, Name = 'Alice Smith', Phone = '555-0101', Location = 'US-East/New York' ;
% Id = 2, Name = 'Bob Jones', Phone = '555-0202', Location = 'US-West/Los Angeles' ;
% ...
%
% % If something went wrong:
% ?- rollback_migration.
% Rolling back to pre-migration state...
% Rollback complete.
```

---

### Example 3: Transaction System -- Snapshot Before Changes, Restore on Error

Implements a transactional wrapper around database modifications, providing commit/rollback semantics using snapshots.

```prolog
% ============================================================
% Transaction Manager
% Provides begin_transaction/1, commit/1, rollback/1 with
% nested transaction support and automatic rollback on failure.
% ============================================================

:- dynamic transaction_stack/2.  % transaction_stack(TxId, SnapshotHandle)
:- dynamic tx_counter/1.
tx_counter(0).

% ---- Begin a new transaction ----
% Takes a snapshot and returns a transaction ID.

begin_transaction(TxId) :-
    retract(tx_counter(N)),
    N1 is N + 1,
    assert(tx_counter(N1)),
    number_codes(N1, NC), atom_codes(NA, NC),
    atom_concat('tx_', NA, TxId),
    db_snapshot(Handle),
    assert(transaction_stack(TxId, Handle)),
    write('Transaction '), write(TxId), write(' started.'), nl.

% ---- Commit a transaction ----
% Removes the snapshot (changes become permanent).

commit(TxId) :-
    (   retract(transaction_stack(TxId, _Handle))
    ->  write('Transaction '), write(TxId), write(' committed.'), nl
    ;   write('Error: no active transaction '), write(TxId), nl,
        fail
    ).

% ---- Rollback a transaction ----
% Restores the database to the state at begin_transaction.

rollback(TxId) :-
    (   retract(transaction_stack(TxId, Handle))
    ->  db_restore(Handle),
        write('Transaction '), write(TxId), write(' rolled back.'), nl
    ;   write('Error: no active transaction '), write(TxId), nl,
        fail
    ).

% ---- Execute a goal within a transaction ----
% Automatically commits on success, rolls back on failure or exception.

with_transaction(Goal) :-
    begin_transaction(TxId),
    (   catch(
            (call(Goal) -> commit(TxId) ; rollback(TxId), fail),
            Error,
            (rollback(TxId),
             write('Transaction failed with error: '), write(Error), nl,
             fail)
        )
    ).

% ============================================================
% Example: Bank account transfers with transaction safety
% ============================================================

:- dynamic account/2.  % account(Name, Balance)

setup_accounts :-
    assert(account(alice, 10000)),
    assert(account(bob, 5000)),
    assert(account(carol, 7500)),
    assert(account(dave, 3000)).

% ---- Transfer money between accounts ----

transfer(From, To, Amount) :-
    account(From, FromBalance),
    FromBalance >= Amount,
    !,
    NewFromBalance is FromBalance - Amount,
    account(To, ToBalance),
    NewToBalance is ToBalance + Amount,
    retract(account(From, FromBalance)),
    assert(account(From, NewFromBalance)),
    retract(account(To, ToBalance)),
    assert(account(To, NewToBalance)),
    write('Transferred '), write(Amount),
    write(' from '), write(From),
    write(' to '), write(To), nl.
transfer(From, _, Amount) :-
    account(From, Balance),
    write('Insufficient funds: '), write(From),
    write(' has '), write(Balance),
    write(' but needs '), write(Amount), nl,
    fail.

% ---- Batch transfer with all-or-nothing semantics ----

batch_transfer(Transfers) :-
    with_transaction(execute_transfers(Transfers)).

execute_transfers([]).
execute_transfers([transfer(From, To, Amount)|Rest]) :-
    transfer(From, To, Amount),
    execute_transfers(Rest).

% ---- Show all balances ----

show_balances :-
    write('=== Account Balances ==='), nl,
    forall(
        account(Name, Balance),
        (write('  '), write(Name), write(': $'), write(Balance), nl)
    ),
    findall(B, account(_, B), Balances),
    sum_list(Balances, Total),
    write('  Total: $'), write(Total), nl.

sum_list([], 0).
sum_list([H|T], Sum) :- sum_list(T, Rest), Sum is H + Rest.

% Usage:
%
% ?- setup_accounts.
% ?- show_balances.
% === Account Balances ===
%   alice: $10000
%   bob: $5000
%   carol: $7500
%   dave: $3000
%   Total: $25500
%
% % Successful batch: all transfers complete
% ?- batch_transfer([transfer(alice, bob, 2000),
%                    transfer(carol, dave, 1500)]).
% Transaction tx_1 started.
% Transferred 2000 from alice to bob
% Transferred 1500 from carol to dave
% Transaction tx_1 committed.
%
% ?- show_balances.
%   alice: $8000
%   bob: $7000
%   carol: $6000
%   dave: $4500
%
% % Failed batch: second transfer fails, ALL changes roll back
% ?- batch_transfer([transfer(alice, bob, 3000),
%                    transfer(dave, carol, 99999)]).
% Transaction tx_2 started.
% Transferred 3000 from alice to bob
% Insufficient funds: dave has 4500 but needs 99999
% Transaction tx_2 rolled back.
% false.
%
% ?- show_balances.
%   alice: $8000   <-- unchanged, rollback preserved state
%   bob: $7000
%   carol: $6000
%   dave: $4500
```

---

### Example 4: Multi-Format Data Store

Manages a configuration and data system that can save predicates as either Prolog source or JSON depending on the use case, with automatic format selection.

```prolog
% ============================================================
% Multi-Format Data Store
% Provides a unified interface for saving/loading data in
% either Prolog or JSON format. Format is selected based on
% the intended consumer (Prolog reload vs. external tools).
% ============================================================

:- dynamic store_config/2.  % store_config(Key, Value)
:- dynamic data_catalog/3.  % data_catalog(PredicateName, Arity, Format)

% ---- Register a predicate with its preferred storage format ----

register_data(Functor, Arity, Format) :-
    member(Format, [prolog, json, both]),
    !,
    retractall(data_catalog(Functor, Arity, _)),
    assert(data_catalog(Functor, Arity, Format)),
    write('Registered '), write(Functor), write('/'), write(Arity),
    write(' with format: '), write(Format), nl.

% ---- Save a predicate using its registered format ----

save_data(Functor, Arity, BaseDir) :-
    data_catalog(Functor, Arity, Format),
    !,
    save_in_format(Functor, Arity, BaseDir, Format).
save_data(Functor, Arity, BaseDir) :-
    % Default to Prolog format
    save_in_format(Functor, Arity, BaseDir, prolog).

save_in_format(Functor, Arity, BaseDir, prolog) :-
    build_filename(BaseDir, Functor, Arity, '.pl', Filename),
    db_save_predicate(Functor/Arity, Filename),
    write('Saved '), write(Functor), write('/'), write(Arity),
    write(' -> '), write(Filename), nl.

save_in_format(Functor, Arity, BaseDir, json) :-
    build_filename(BaseDir, Functor, Arity, '.json', Filename),
    % Save entire DB to JSON, then the file contains everything.
    % For predicate-specific JSON, we save full DB and note the format.
    db_export_json(Filename),
    write('Exported '), write(Functor), write('/'), write(Arity),
    write(' -> '), write(Filename), nl.

save_in_format(Functor, Arity, BaseDir, both) :-
    save_in_format(Functor, Arity, BaseDir, prolog),
    save_in_format(Functor, Arity, BaseDir, json).

% ---- Save all registered predicates ----

save_all_data(BaseDir) :-
    write('=== Saving All Registered Data ==='), nl,
    forall(
        data_catalog(F, A, _Format),
        save_data(F, A, BaseDir)
    ),
    write('=== Save Complete ==='), nl.

% ---- Load data from appropriate format ----

load_data(Functor, Arity, BaseDir) :-
    data_catalog(Functor, Arity, Format),
    !,
    load_in_format(Functor, Arity, BaseDir, Format).
load_data(Functor, Arity, BaseDir) :-
    load_in_format(Functor, Arity, BaseDir, prolog).

load_in_format(Functor, Arity, BaseDir, prolog) :-
    build_filename(BaseDir, Functor, Arity, '.pl', Filename),
    db_load(Filename),
    write('Loaded '), write(Filename), nl.

load_in_format(Functor, Arity, BaseDir, json) :-
    build_filename(BaseDir, Functor, Arity, '.json', Filename),
    db_import_json(Filename),
    write('Imported '), write(Filename), nl.

load_in_format(Functor, Arity, BaseDir, both) :-
    % Prefer Prolog format for loading
    load_in_format(Functor, Arity, BaseDir, prolog).

% ---- Filename builder ----

build_filename(BaseDir, Functor, Arity, Extension, Filename) :-
    atom_concat(BaseDir, '/', S1),
    atom_concat(S1, Functor, S2),
    atom_concat(S2, '_', S3),
    number_codes(Arity, AC), atom_codes(AA, AC),
    atom_concat(S3, AA, S4),
    atom_concat(S4, Extension, Filename).

% ============================================================
% Usage: Configuration and product catalog system
% ============================================================

:- dynamic product/4.     % product(Sku, Name, Price, Category)
:- dynamic config/2.      % config(Key, Value)
:- dynamic audit_log/3.   % audit_log(Action, Target, Detail)

setup_store_example :-
    % Products: shared with external inventory system (JSON)
    assert(product(sku001, 'Widget A', 29.99, hardware)),
    assert(product(sku002, 'Widget B', 49.99, hardware)),
    assert(product(sku003, 'Service Plan', 9.99, subscription)),

    % Config: internal Prolog reload only
    assert(config(tax_rate, 0.08)),
    assert(config(currency, usd)),
    assert(config(store_name, 'Acme Corp')),

    % Audit log: both formats for compliance
    assert(audit_log(create, product, 'Initial catalog loaded')),

    % Register formats
    register_data(product, 4, json),
    register_data(config, 2, prolog),
    register_data(audit_log, 3, both).

% Usage:
% ?- setup_store_example.
% ?- save_all_data('/tmp/store').
% === Saving All Registered Data ===
% Exported product/4 -> /tmp/store/product_4.json
% Saved config/2 -> /tmp/store/config_2.pl
% Saved audit_log/3 -> /tmp/store/audit_log_3.pl
% Exported audit_log/3 -> /tmp/store/audit_log_3.json
% === Save Complete ===
%
% After clearing and reloading:
% ?- db_clear.
% ?- load_data(config, 2, '/tmp/store').
% Loaded /tmp/store/config_2.pl
% ?- config(tax_rate, Rate).
% Rate = 0.08.
```

---

### Example 5: Incremental Backup -- Save Only Predicates That Changed

Tracks which predicates have been modified since the last backup and only saves those, reducing I/O for large knowledge bases.

```prolog
% ============================================================
% Incremental Backup System
% Tracks modifications to predicates and only backs up those
% that changed since the last full or incremental backup.
% ============================================================

:- dynamic last_backup_hash/2.  % last_backup_hash(Functor/Arity, Hash)
:- dynamic dirty_predicate/2.   % dirty_predicate(Functor, Arity)
:- dynamic backup_manifest/3.   % backup_manifest(Timestamp, Type, Files)

% ---- Mark a predicate as dirty (changed since last backup) ----
% Call this after any assert/retract operation.

mark_dirty(Functor, Arity) :-
    (   dirty_predicate(Functor, Arity)
    ->  true  % Already marked
    ;   assert(dirty_predicate(Functor, Arity))
    ).

% ---- Tracked assert/retract wrappers ----

tracked_assert(Fact) :-
    assert(Fact),
    functor(Fact, F, A),
    mark_dirty(F, A).

tracked_retract(Fact) :-
    retract(Fact),
    functor(Fact, F, A),
    mark_dirty(F, A).

% ---- Full backup: save everything ----

full_backup(BackupDir) :-
    write('=== Full Backup ==='), nl,
    get_backup_timestamp(Ts),
    findall(F/A,
        (dirty_predicate(F, A) ; last_backup_hash(F/A, _)),
        AllPreds0),
    sort(AllPreds0, AllPreds),
    % Also find all predicates currently in the database
    find_all_predicates(CurrentPreds),
    union_lists(AllPreds, CurrentPreds, TotalPreds),
    save_predicates(TotalPreds, BackupDir, SavedFiles),
    update_hashes(TotalPreds),
    retractall(dirty_predicate(_, _)),
    assert(backup_manifest(Ts, full, SavedFiles)),
    length(SavedFiles, FileCount),
    write('Full backup complete: '), write(FileCount),
    write(' predicates saved.'), nl.

% ---- Incremental backup: save only dirty predicates ----

incremental_backup(BackupDir) :-
    findall(F/A, dirty_predicate(F, A), DirtyList),
    (   DirtyList = []
    ->  write('No changes since last backup.'), nl
    ;   write('=== Incremental Backup ==='), nl,
        get_backup_timestamp(Ts),
        save_predicates(DirtyList, BackupDir, SavedFiles),
        update_hashes(DirtyList),
        retractall(dirty_predicate(_, _)),
        assert(backup_manifest(Ts, incremental, SavedFiles)),
        length(SavedFiles, FileCount),
        write('Incremental backup: '), write(FileCount),
        write(' predicates saved.'), nl
    ).

% ---- Save a list of predicates ----

save_predicates([], _, []).
save_predicates([F/A|Rest], Dir, [Filename|Files]) :-
    atom_concat(Dir, '/', S1),
    atom_concat(S1, F, S2),
    atom_concat(S2, '_', S3),
    number_codes(A, AC), atom_codes(AA, AC),
    atom_concat(S3, AA, S4),
    atom_concat(S4, '.pl', Filename),
    (   catch(db_save_predicate(F/A, Filename), _, true)
    ->  write('  Saved: '), write(F), write('/'), write(A), nl
    ;   write('  Skip: '), write(F), write('/'), write(A),
        write(' (no clauses)'), nl
    ),
    save_predicates(Rest, Dir, Files).

% ---- Restore from a backup directory ----

restore_incremental(BackupDir, PredicateList) :-
    write('=== Restoring from backup ==='), nl,
    restore_list(PredicateList, BackupDir).

restore_list([], _).
restore_list([F/A|Rest], Dir) :-
    atom_concat(Dir, '/', S1),
    atom_concat(S1, F, S2),
    atom_concat(S2, '_', S3),
    number_codes(A, AC), atom_codes(AA, AC),
    atom_concat(S3, AA, S4),
    atom_concat(S4, '.pl', Filename),
    (   catch(db_load(Filename), _, fail)
    ->  write('  Restored: '), write(F), write('/'), write(A), nl
    ;   write('  Missing: '), write(Filename), nl
    ),
    restore_list(Rest, Dir).

% ---- Show backup history ----

show_backup_history :-
    write('=== Backup History ==='), nl,
    forall(
        backup_manifest(Ts, Type, Files),
        (write('  ['), write(Ts), write('] '),
         write(Type), write(': '),
         length(Files, N), write(N), write(' files'), nl)
    ).

show_pending_changes :-
    findall(F/A, dirty_predicate(F, A), Dirty),
    (   Dirty = []
    ->  write('No pending changes.'), nl
    ;   write('Pending changes:'), nl,
        forall(member(P, Dirty),
               (write('  '), write(P), nl))
    ).

% ---- Helpers ----

find_all_predicates(Preds) :-
    % Simplified: would normally inspect the knowledge base
    findall(F/A, last_backup_hash(F/A, _), Preds).

union_lists([], L, L).
union_lists([H|T], L2, Result) :-
    (   member(H, L2)
    ->  union_lists(T, L2, Result)
    ;   union_lists(T, [H|L2], Result)
    ).

update_hashes([]).
update_hashes([F/A|Rest]) :-
    retractall(last_backup_hash(F/A, _)),
    assert(last_backup_hash(F/A, backed_up)),
    update_hashes(Rest).

get_backup_timestamp(Ts) :-
    findall(_, backup_manifest(_, _, _), All),
    length(All, N),
    N1 is N + 1,
    number_codes(N1, NC), atom_codes(NA, NC),
    atom_concat('backup_', NA, Ts).

% Usage:
% ?- tracked_assert(sensor(temp_1, 22.5, kitchen)).
% ?- tracked_assert(sensor(temp_2, 19.3, bedroom)).
% ?- tracked_assert(sensor(humidity_1, 65, bathroom)).
% ?- tracked_assert(alert_rule(high_temp, temp, '>', 30)).
%
% ?- show_pending_changes.
% Pending changes:
%   sensor/3
%   alert_rule/4
%
% ?- full_backup('/tmp/iot_backup').
% === Full Backup ===
%   Saved: sensor/3
%   Saved: alert_rule/4
% Full backup complete: 2 predicates saved.
%
% ?- tracked_assert(sensor(temp_3, 31.2, garage)).
% ?- show_pending_changes.
% Pending changes:
%   sensor/3
%
% ?- incremental_backup('/tmp/iot_backup').
% === Incremental Backup ===
%   Saved: sensor/3
% Incremental backup: 1 predicates saved.
%
% ?- show_backup_history.
% === Backup History ===
%   [backup_1] full: 2 files
%   [backup_2] incremental: 1 files
```
