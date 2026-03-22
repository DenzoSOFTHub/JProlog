# JProlog File System Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.filesystem` package provides predicates for
interacting with the host file system from within Prolog programs. These
predicates cover file and directory existence checks, creation and deletion,
copying and renaming, reading and writing file contents, and path manipulation.

All path arguments are atoms. Relative paths are resolved against the JVM
working directory (as reported by `System.getProperty("user.dir")`). Operations
that modify the file system (create, delete, rename, copy, write) succeed
deterministically or throw an evaluation error on I/O failure.

**Source file:**
`src/main/java/it/denzosoft/jprolog/builtin/filesystem/FileSystemPredicates.java`

**Registered in:** `BuiltInFactory` (ISS-2025-0115)

---

## Predicate Reference

### file_exists/1

```prolog
file_exists(+Path)
```

**Arguments:**
- `Path` (atom, input) -- Path to a file.

**Description:** Succeeds if `Path` refers to an existing regular file. Fails
silently if the file does not exist or if the path points to a directory.

**Example:**
```prolog
?- file_exists('config.ini').
true.
```

---

### directory_exists/1

```prolog
directory_exists(+Path)
```

**Arguments:**
- `Path` (atom, input) -- Path to a directory.

**Description:** Succeeds if `Path` refers to an existing directory. Fails
silently if the directory does not exist or if the path points to a regular
file.

**Example:**
```prolog
?- directory_exists('/tmp').
true.
```

---

### make_directory/1

```prolog
make_directory(+Path)
```

**Arguments:**
- `Path` (atom, input) -- Path of the directory to create.

**Description:** Creates a single directory. The parent directory must already
exist. Throws an evaluation error if creation fails (e.g., parent missing,
permission denied).

**Example:**
```prolog
?- make_directory('output').
true.
```

---

### make_directory_path/1

```prolog
make_directory_path(+Path)
```

**Arguments:**
- `Path` (atom, input) -- Path of the directory tree to create.

**Description:** Creates the directory and all necessary parent directories
recursively. Equivalent to `mkdir -p`. Succeeds even if the directory already
exists.

**Example:**
```prolog
?- make_directory_path('project/src/main/prolog').
true.
```

---

### delete_file/1

```prolog
delete_file(+Path)
```

**Arguments:**
- `Path` (atom, input) -- Path to the file to delete.

**Description:** Deletes the file at `Path`. Throws an evaluation error if the
file does not exist or cannot be deleted.

**Example:**
```prolog
?- delete_file('temp_output.txt').
true.
```

---

### delete_directory/1

```prolog
delete_directory(+Path)
```

**Arguments:**
- `Path` (atom, input) -- Path to the directory to delete.

**Description:** Deletes the directory at `Path`. The directory must be empty.
Throws an evaluation error if it does not exist, is not empty, or cannot be
deleted.

**Example:**
```prolog
?- delete_directory('old_output').
true.
```

---

### rename_file/2

```prolog
rename_file(+OldPath, +NewPath)
```

**Arguments:**
- `OldPath` (atom, input) -- Current path of the file or directory.
- `NewPath` (atom, input) -- Desired new path.

**Description:** Renames (moves) a file or directory from `OldPath` to
`NewPath`. If `NewPath` already exists, it is replaced. Throws an evaluation
error on failure.

**Example:**
```prolog
?- rename_file('data.csv', 'data_backup.csv').
true.
```

---

### copy_file/2

```prolog
copy_file(+Source, +Destination)
```

**Arguments:**
- `Source` (atom, input) -- Path of the file to copy.
- `Destination` (atom, input) -- Path of the destination file.

**Description:** Copies the file at `Source` to `Destination`. If
`Destination` already exists, it is replaced. Throws an evaluation error on
failure.

**Example:**
```prolog
?- copy_file('template.pl', 'new_module.pl').
true.
```

---

### file_size/2

```prolog
file_size(+Path, -Size)
```

**Arguments:**
- `Path` (atom, input) -- Path to a file.
- `Size` (number, output) -- Size of the file in bytes.

**Description:** Unifies `Size` with the size in bytes of the file at `Path`.
Throws an evaluation error if the file does not exist.

**Example:**
```prolog
?- file_size('report.txt', S).
S = 4096.
```

---

### file_modified/2

```prolog
file_modified(+Path, -Timestamp)
```

**Arguments:**
- `Path` (atom, input) -- Path to a file.
- `Timestamp` (number, output) -- Last modification time in milliseconds since
  the Unix epoch.

**Description:** Unifies `Timestamp` with the last-modified time of the file
at `Path`, expressed as milliseconds since 1970-01-01T00:00:00Z.

**Example:**
```prolog
?- file_modified('data.csv', T).
T = 1711036800000.
```

---

### directory_files/2

```prolog
directory_files(+Directory, -Files)
```

**Arguments:**
- `Directory` (atom, input) -- Path to a directory.
- `Files` (list of atoms, output) -- List of file and subdirectory names
  (not full paths) contained in the directory.

**Description:** Unifies `Files` with a list of atoms representing the names
of all entries in `Directory`. Does not include `.` or `..`. The order is
system-dependent. Throws an evaluation error if `Directory` is not a valid
directory.

**Example:**
```prolog
?- directory_files('/tmp/myproject', Files).
Files = ['main.pl', 'utils.pl', 'tests'].
```

---

### working_directory/2

```prolog
working_directory(-OldDir, +NewDir)
```

**Arguments:**
- `OldDir` (atom, output) -- The current working directory before the change.
- `NewDir` (atom, input) -- The new working directory to set.

**Description:** Unifies `OldDir` with the current JVM working directory, then
changes it to `NewDir`. Note: this changes the `user.dir` system property,
which affects path resolution in subsequent file system operations.

**Example:**
```prolog
?- working_directory(Old, '/tmp').
Old = '/home/user/project'.
```

---

### absolute_file_name/2

```prolog
absolute_file_name(+RelativePath, -AbsolutePath)
```

**Arguments:**
- `RelativePath` (atom, input) -- A relative or absolute path.
- `AbsolutePath` (atom, output) -- The normalized absolute path.

**Description:** Resolves `RelativePath` against the current working directory
and normalizes the result (removing `.` and `..` components). Unifies the
result with `AbsolutePath`.

**Example:**
```prolog
?- absolute_file_name('src/../lib/utils.pl', Abs).
Abs = '/home/user/project/lib/utils.pl'.
```

---

### read_file_to_atom/2

```prolog
read_file_to_atom(+Path, -Content)
```

**Arguments:**
- `Path` (atom, input) -- Path to the file to read.
- `Content` (atom, output) -- The entire content of the file as a single atom.

**Description:** Reads the entire contents of the file at `Path` and unifies
`Content` with an atom containing those contents. Throws an evaluation error
if the file does not exist or cannot be read.

**Example:**
```prolog
?- read_file_to_atom('greeting.txt', C).
C = 'Hello, World!\n'.
```

---

### write_atom_to_file/2

```prolog
write_atom_to_file(+Path, +Content)
```

**Arguments:**
- `Path` (atom, input) -- Path of the file to write.
- `Content` (atom, input) -- The content to write.

**Description:** Writes the atom `Content` to the file at `Path`, overwriting
any existing content. Creates the file if it does not exist. Throws an
evaluation error on failure.

**Example:**
```prolog
?- write_atom_to_file('output.txt', 'Result: 42\n').
true.
```

---

## Real-World Examples

### Example 1: Build System -- Scan, Compile, and Report

This program scans a source directory for `.pl` files, reads each one,
reports its size, and writes a summary manifest.

```prolog
% build.pl -- Scan a directory for Prolog source files and produce a build manifest.

% Entry point: build all .pl files in the given source directory.
build(SrcDir, ManifestPath) :-
    directory_files(SrcDir, AllFiles),
    include(is_prolog_file, AllFiles, PlFiles),
    length(PlFiles, Count),
    write('Found '), write(Count), write(' Prolog source files.'), nl,
    build_manifest_lines(SrcDir, PlFiles, Lines),
    atomic_list_concat(Lines, '\n', ManifestContent),
    write_atom_to_file(ManifestPath, ManifestContent),
    write('Manifest written to '), write(ManifestPath), nl.

% Check whether a filename ends with '.pl'.
is_prolog_file(Name) :-
    atom_string(Name, Str),
    atom_length(Str, Len),
    Len > 3,
    sub_atom(Name, _, 3, 0, '.pl').

% Build a list of manifest lines: "filename: SIZE bytes" for each file.
build_manifest_lines(_, [], []).
build_manifest_lines(Dir, [File|Rest], [Line|Lines]) :-
    atom_concat(Dir, '/', DirSlash),
    atom_concat(DirSlash, File, FullPath),
    file_size(FullPath, Size),
    number_chars(Size, SizeChars),
    atom_chars(SizeAtom, SizeChars),
    atomic_list_concat([File, ': ', SizeAtom, ' bytes'], Line),
    build_manifest_lines(Dir, Rest, Lines).

% Usage:
%   ?- build('src/prolog', 'build_manifest.txt').
%   Found 12 Prolog source files.
%   Manifest written to build_manifest.txt
```

---

### Example 2: Log Rotation

This program checks the size of a log file and, if it exceeds a threshold,
renames it with a numeric suffix and creates a fresh empty log file.

```prolog
% log_rotation.pl -- Rotate log files when they exceed a size threshold.

% rotate_log(+LogPath, +MaxBytes)
% If the log file exceeds MaxBytes, rotate it. Otherwise, do nothing.
rotate_log(LogPath, MaxBytes) :-
    (   file_exists(LogPath)
    ->  file_size(LogPath, Size),
        (   Size > MaxBytes
        ->  find_next_rotation_index(LogPath, 1, Index),
            number_chars(Index, IC),
            atom_chars(IdxAtom, IC),
            atom_concat(LogPath, '.', Tmp1),
            atom_concat(Tmp1, IdxAtom, RotatedPath),
            rename_file(LogPath, RotatedPath),
            write('Rotated: '), write(LogPath),
            write(' -> '), write(RotatedPath), nl,
            write_atom_to_file(LogPath, ''),
            write('Created fresh log: '), write(LogPath), nl
        ;   write('Log size '), write(Size),
            write(' bytes, under threshold '), write(MaxBytes),
            write('. No rotation needed.'), nl
        )
    ;   write_atom_to_file(LogPath, ''),
        write('Created new log file: '), write(LogPath), nl
    ).

% Find the first available rotation index (e.g., app.log.1, app.log.2, ...).
find_next_rotation_index(LogPath, N, N) :-
    number_chars(N, NC),
    atom_chars(NAtom, NC),
    atom_concat(LogPath, '.', Tmp),
    atom_concat(Tmp, NAtom, Candidate),
    \+ file_exists(Candidate), !.
find_next_rotation_index(LogPath, N, Index) :-
    N1 is N + 1,
    find_next_rotation_index(LogPath, N1, Index).

% Usage:
%   ?- rotate_log('app.log', 1048576).
%   Rotated: app.log -> app.log.1
%   Created fresh log: app.log
```

---

### Example 3: Project Scaffolding

This program creates the directory structure for a new Prolog project with
standard directories and starter files.

```prolog
% scaffold.pl -- Create a new Prolog project directory structure.

scaffold_project(ProjectName) :-
    make_directory_path(ProjectName),
    subdirs(Subdirs),
    create_subdirs(ProjectName, Subdirs),
    create_starter_files(ProjectName),
    write('Project "'), write(ProjectName), write('" scaffolded successfully.'), nl.

subdirs(['src', 'src/modules', 'tests', 'docs', 'lib', 'bin']).

create_subdirs(_, []).
create_subdirs(Root, [Dir|Rest]) :-
    atom_concat(Root, '/', Tmp),
    atom_concat(Tmp, Dir, FullDir),
    make_directory_path(FullDir),
    write('  Created: '), write(FullDir), nl,
    create_subdirs(Root, Rest).

create_starter_files(Root) :-
    % Create main entry point
    atom_concat(Root, '/src/main.pl', MainPath),
    write_atom_to_file(MainPath,
        ':- module(main, [start/0]).\n\nstart :- write(\'Hello from project!\'), nl.\n'),
    write('  Created: '), write(MainPath), nl,
    % Create test runner
    atom_concat(Root, '/tests/run_tests.pl', TestPath),
    write_atom_to_file(TestPath,
        ':- module(run_tests, [run/0]).\n\nrun :- write(\'All tests passed.\'), nl.\n'),
    write('  Created: '), write(TestPath), nl,
    % Create project config
    atom_concat(Root, '/project.conf', ConfPath),
    write_atom_to_file(ConfPath, 'name=my_project\nversion=0.1.0\nauthor=unknown\n'),
    write('  Created: '), write(ConfPath), nl.

% Usage:
%   ?- scaffold_project('my_new_project').
%     Created: my_new_project/src
%     Created: my_new_project/src/modules
%     Created: my_new_project/tests
%     Created: my_new_project/docs
%     Created: my_new_project/lib
%     Created: my_new_project/bin
%     Created: my_new_project/src/main.pl
%     Created: my_new_project/tests/run_tests.pl
%     Created: my_new_project/project.conf
%   Project "my_new_project" scaffolded successfully.
```

---

### Example 4: File Synchronization Between Two Directories

This program compares the contents of two directories and copies any files
present in the source but missing in the destination.

```prolog
% sync_dirs.pl -- One-way file synchronization from source to destination.

sync_directories(SrcDir, DstDir) :-
    (   directory_exists(DstDir)
    ->  true
    ;   make_directory_path(DstDir)
    ),
    directory_files(SrcDir, SrcFiles),
    directory_files(DstDir, DstFiles),
    find_missing(SrcFiles, DstFiles, Missing),
    length(Missing, MissingCount),
    length(SrcFiles, TotalCount),
    write('Source files: '), write(TotalCount), nl,
    write('Missing in destination: '), write(MissingCount), nl,
    copy_missing_files(SrcDir, DstDir, Missing),
    write('Synchronization complete.'), nl.

% find_missing(+SrcList, +DstList, -Missing)
% Files in SrcList that are not in DstList.
find_missing([], _, []).
find_missing([F|Rest], DstFiles, [F|Missing]) :-
    \+ member(F, DstFiles), !,
    find_missing(Rest, DstFiles, Missing).
find_missing([_|Rest], DstFiles, Missing) :-
    find_missing(Rest, DstFiles, Missing).

% Copy each missing file from SrcDir to DstDir.
copy_missing_files(_, _, []).
copy_missing_files(SrcDir, DstDir, [File|Rest]) :-
    atom_concat(SrcDir, '/', S1),
    atom_concat(S1, File, SrcPath),
    atom_concat(DstDir, '/', D1),
    atom_concat(D1, File, DstPath),
    % Only copy regular files, skip subdirectories
    (   file_exists(SrcPath)
    ->  copy_file(SrcPath, DstPath),
        file_size(SrcPath, Size),
        write('  Copied: '), write(File),
        write(' ('), write(Size), write(' bytes)'), nl
    ;   write('  Skipped directory: '), write(File), nl
    ),
    copy_missing_files(SrcDir, DstDir, Rest).

% Usage:
%   ?- sync_directories('/data/primary', '/data/backup').
%   Source files: 25
%   Missing in destination: 3
%     Copied: report_march.csv (15234 bytes)
%     Copied: schema_v2.sql (8901 bytes)
%     Skipped directory: archive
%   Synchronization complete.
```

---

### Example 5: Configuration Backup with Timestamp Suffix

This program reads a list of configuration file paths, copies each one with
a timestamp-based suffix derived from the current system time, and writes a
backup log.

```prolog
% config_backup.pl -- Back up configuration files with a timestamp suffix.

backup_configs(ConfigFiles, BackupDir, LogPath) :-
    (   directory_exists(BackupDir)
    ->  true
    ;   make_directory_path(BackupDir)
    ),
    system_time(Now),
    timestamp_suffix(Now, Suffix),
    backup_each(ConfigFiles, BackupDir, Suffix, LogEntries),
    atomic_list_concat(LogEntries, '\n', LogContent),
    write_atom_to_file(LogPath, LogContent),
    write('Backup log written to '), write(LogPath), nl.

% Convert a millisecond timestamp to a compact suffix string.
% We use integer division to get a readable YYYYMMDD-like value.
timestamp_suffix(MillisEpoch, Suffix) :-
    Seconds is MillisEpoch // 1000,
    number_chars(Seconds, SC),
    atom_chars(Suffix, SC).

% Backup each configuration file and collect log entries.
backup_each([], _, _, []).
backup_each([CfgPath|Rest], BackupDir, Suffix, [Entry|Entries]) :-
    (   file_exists(CfgPath)
    ->  % Extract the filename from the full path by reading it as an atom.
        % A simple approach: find the last '/' and take the rest.
        extract_filename(CfgPath, BaseName),
        atom_concat(BaseName, '.bak_', T1),
        atom_concat(T1, Suffix, BackupName),
        atom_concat(BackupDir, '/', T2),
        atom_concat(T2, BackupName, BackupPath),
        copy_file(CfgPath, BackupPath),
        file_size(CfgPath, Size),
        number_chars(Size, SzC),
        atom_chars(SzAtom, SzC),
        atomic_list_concat(['OK: ', CfgPath, ' -> ', BackupPath, ' (', SzAtom, ' bytes)'], Entry),
        write('  Backed up: '), write(CfgPath), nl
    ;   atomic_list_concat(['MISSING: ', CfgPath], Entry),
        write('  Warning: '), write(CfgPath), write(' not found, skipping.'), nl
    ),
    backup_each(Rest, BackupDir, Suffix, Entries).

% Extract filename after the last '/' separator.
extract_filename(Path, FileName) :-
    atom_chars(Path, Chars),
    reverse(Chars, Rev),
    take_until_slash(Rev, RevName),
    reverse(RevName, NameChars),
    atom_chars(FileName, NameChars).

take_until_slash([], []).
take_until_slash(['/'|_], []) :- !.
take_until_slash([C|Rest], [C|Out]) :-
    take_until_slash(Rest, Out).

% Usage:
%   ?- backup_configs(
%        ['/etc/myapp/database.conf', '/etc/myapp/server.conf', '/etc/myapp/logging.conf'],
%        '/var/backups/myapp',
%        '/var/backups/myapp/backup.log'
%      ).
%     Backed up: /etc/myapp/database.conf
%     Backed up: /etc/myapp/server.conf
%     Warning: /etc/myapp/logging.conf not found, skipping.
%   Backup log written to /var/backups/myapp/backup.log
```
