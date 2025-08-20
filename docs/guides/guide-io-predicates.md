# JProlog I/O Predicates Guide

**Version**: JProlog v2.0.6  
**Last Updated**: August 2025  
**Compatibility**: All JProlog versions 2.0+

---

## Table of Contents

1. [Introduction](#introduction)
2. [Basic Output Predicates](#basic-output-predicates)
3. [Basic Input Predicates](#basic-input-predicates)
4. [Stream Operations](#stream-operations)
5. [File Operations](#file-operations)
6. [Formatted I/O](#formatted-io)
7. [Character I/O](#character-io)
8. [Code I/O](#code-io)
9. [Stream Properties](#stream-properties)
10. [Working Examples](#working-examples)
11. [Best Practices](#best-practices)
12. [Troubleshooting](#troubleshooting)

---

## Introduction

JProlog provides comprehensive I/O capabilities following ISO Prolog standards. This includes console I/O, file operations, stream management, and character-level operations. All I/O operations are stream-based, with standard input, output, and error streams available by default.

### Core Concepts

- **Stream**: A source or sink for character data
- **Current Input/Output**: Default streams for read/write operations
- **Character**: Single character represented as atom
- **Character Code**: Integer representation of character (ASCII/Unicode)
- **Term**: Structured Prolog data for read_term/write_term operations

---

## Basic Output Predicates

### `write/1` - Write Term

Writes a term to current output stream in functional notation.

**Syntax:**
```prolog
write(+Term)
```

**Examples:**
```prolog
% Write simple terms
?- write(hello).
hello
true.

?- write(123).
123
true.

?- write([1,2,3]).
[1.0, 2.0, 3.0]
true.

% Write complex structures
?- write(person(john, age(25), city(rome))).
person(john, age(25.0), city(rome))
true.

% Write variables
?- X = mary, write(X).
mary
X = mary.

% Chain multiple writes
?- write('User: '), write(alice), write(' Age: '), write(30).
User: alice Age: 30
true.
```

### `writeln/1` - Write Term with Newline

Writes a term followed by a newline character.

**Syntax:**
```prolog
writeln(+Term)
```

**Examples:**
```prolog
% Write with automatic newline
?- writeln('Hello World').
Hello World

true.

% Multiple lines
?- writeln('Line 1'), writeln('Line 2'), writeln('Line 3').
Line 1
Line 2
Line 3
true.

% Useful for formatted output
print_person(Name, Age) :-
    write('Name: '), writeln(Name),
    write('Age: '), writeln(Age).

?- print_person(alice, 25).
Name: alice
Age: 25
true.
```

### `nl/0` - Write Newline

Writes a newline character to current output.

**Syntax:**
```prolog
nl
```

**Examples:**
```prolog
% Simple newline
?- write('First line'), nl, write('Second line').
First line
Second line
true.

% Format lists vertically
print_list([]).
print_list([H|T]) :-
    write(H), nl,
    print_list(T).

?- print_list([apple, banana, cherry]).
apple
banana
cherry
true.
```

---

## Basic Input Predicates

### `read/1` - Read Term

Reads a Prolog term from current input stream.

**Syntax:**
```prolog
read(-Term)
```

**Examples:**
```prolog
% Read user input (interactive)
?- write('Enter a term: '), read(X).
Enter a term: hello.
X = hello.

?- write('Enter a number: '), read(N).
Enter a number: 42.
N = 42.

% Read complex terms
?- write('Enter person data: '), read(Person).
Enter person data: person(alice, 30).
Person = person(alice, 30).

% Simple interactive loop
ask_name :-
    write('What is your name? '),
    read(Name),
    write('Hello '), write(Name), nl.

?- ask_name.
What is your name? alice.
Hello alice
true.
```

### `get_char/1` - Read Single Character

Reads a single character from current input.

**Syntax:**
```prolog
get_char(-Char)
```

**Examples:**
```prolog
% Read single character
?- write('Press any key: '), get_char(C).
Press any key: a
C = a.

% Character-based menu
show_menu :-
    writeln('Choose option:'),
    writeln('1. View data'),
    writeln('2. Add data'),
    writeln('3. Exit'),
    write('Your choice: '),
    get_char(Choice),
    process_choice(Choice).

process_choice('1') :- writeln('Viewing data...').
process_choice('2') :- writeln('Adding data...').
process_choice('3') :- writeln('Goodbye!').
process_choice(_) :- writeln('Invalid choice').
```

### `get_code/1` - Read Character Code

Reads a character and returns its ASCII/Unicode code.

**Syntax:**
```prolog
get_code(-Code)
```

**Examples:**
```prolog
% Read character code
?- write('Enter character: '), get_code(Code).
Enter character: A
Code = 65.

% Convert between char and code
?- get_char(C), char_code(C, Code).
a
C = a,
Code = 97.

% Check for specific characters
read_until_newline :-
    get_code(Code),
    (   Code =:= 10 ->  % Newline code
        writeln('Found newline!')
    ;   char_code(Char, Code),
        write(Char),
        read_until_newline
    ).
```

---

## Stream Operations

### `open/3` - Open File Stream

Opens a file and creates a stream handle.

**Syntax:**
```prolog
open(+File, +Mode, -Stream)
```

**Modes:**
- `read`: Open for reading
- `write`: Open for writing (creates/truncates)
- `append`: Open for appending

**Examples:**
```prolog
% Open file for reading
?- open('data.txt', read, Stream).
Stream = <stream>(data.txt).

% Open for writing
?- open('output.txt', write, OutStream).
OutStream = <stream>(output.txt).

% Open for appending
?- open('log.txt', append, LogStream).
LogStream = <stream>(log.txt).

% Always close streams when done
process_file(FileName) :-
    open(FileName, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    atom_codes(Content, Codes),
    writeln(Content).
```

### `close/1` - Close Stream

Closes a stream and releases resources.

**Syntax:**
```prolog
close(+Stream)
```

**Examples:**
```prolog
% Safe file processing
safe_write_file(FileName, Data) :-
    open(FileName, write, Stream),
    write(Stream, Data),
    close(Stream).

% Exception-safe pattern  
safe_process_file(File) :-
    open(File, read, Stream),
    catch(
        (process_stream_data(Stream), close(Stream)),
        Error,
        (close(Stream), throw(Error))
    ).
```

### `current_input/1` and `current_output/1` - Current Streams

Get the current input or output stream.

**Syntax:**
```prolog
current_input(-Stream)
current_output(-Stream)
```

**Examples:**
```prolog
% Get current streams
?- current_input(In), current_output(Out).
In = <stream>(stdin),
Out = <stream>(stdout).

% Temporary redirection pattern
with_output_to_file(File, Goal) :-
    current_output(OldOut),
    open(File, write, NewOut),
    set_output(NewOut),
    call(Goal),
    close(NewOut),
    set_output(OldOut).
```

---

## File Operations

### File Reading Examples

```prolog
% Read entire file as terms
read_file_terms(FileName, Terms) :-
    open(FileName, read, Stream),
    read_terms_from_stream(Stream, Terms),
    close(Stream).

read_terms_from_stream(Stream, [Term|Terms]) :-
    read(Stream, Term),
    Term \== end_of_file, !,
    read_terms_from_stream(Stream, Terms).
read_terms_from_stream(_, []).

% Read file as character codes
read_file_codes(FileName, Codes) :-
    open(FileName, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream).

read_stream_to_codes(Stream, [Code|Codes]) :-
    get_code(Stream, Code),
    Code \== -1, !,  % Not end of file
    read_stream_to_codes(Stream, Codes).
read_stream_to_codes(_, []).
```

### File Writing Examples

```prolog
% Write terms to file
write_terms_to_file(FileName, Terms) :-
    open(FileName, write, Stream),
    write_terms_to_stream(Stream, Terms),
    close(Stream).

write_terms_to_stream(_, []).
write_terms_to_stream(Stream, [Term|Terms]) :-
    write_term(Stream, Term, [quoted(true)]),
    write(Stream, '.'), nl(Stream),
    write_terms_to_stream(Stream, Terms).

% Append to log file
log_message(Message) :-
    get_time(Now),
    format_time(atom(TimeStr), '%Y-%m-%d %H:%M:%S', Now),
    open('application.log', append, Stream),
    format(Stream, '[~w] ~w~n', [TimeStr, Message]),
    close(Stream).

% Usage
?- log_message('Application started').
true.
```

---

## Formatted I/O

### `format/2` - Formatted Output

Writes formatted output using format specifiers.

**Syntax:**
```prolog
format(+Format, +Arguments)
format(+Stream, +Format, +Arguments)
```

**Format Specifiers:**
- `~w`: Write argument
- `~d`: Write integer  
- `~f`: Write float
- `~a`: Write atom
- `~s`: Write string/codes
- `~n`: Newline
- `~~`: Literal tilde

**Examples:**
```prolog
% Basic formatting
?- format('Hello ~w!~n', [world]).
Hello world!
true.

% Multiple arguments
?- format('Name: ~w, Age: ~d, Score: ~2f~n', [alice, 25, 95.7]).
Name: alice, Age: 25, Score: 95.70
true.

% Formatted tables
print_person_table(People) :-
    format('~w~t~20|~w~t~30|~w~n', ['Name', 'Age', 'City']),
    format('~`-t~30|~n', []),  % Separator line
    print_people(People).

print_people([]).
print_people([person(Name, Age, City)|Rest]) :-
    format('~w~t~20|~d~t~30|~w~n', [Name, Age, City]),
    print_people(Rest).

% Usage
?- print_person_table([person(alice, 25, rome), person(bob, 30, milan)]).
Name                Age       City
------------------------------
alice               25        rome
bob                 30        milan
true.
```

---

## Character I/O

### Stream-Specific Character Operations

```prolog
% Read/write characters to specific streams
read_chars_from_file(FileName, Chars) :-
    open(FileName, read, Stream),
    read_chars_from_stream(Stream, Chars),
    close(Stream).

read_chars_from_stream(Stream, [Char|Chars]) :-
    get_char(Stream, Char),
    Char \== end_of_file, !,
    read_chars_from_stream(Stream, Chars).
read_chars_from_stream(_, []).

% Write characters to file
write_chars_to_file(FileName, Chars) :-
    open(FileName, write, Stream),
    write_chars_to_stream(Stream, Chars),
    close(Stream).

write_chars_to_stream(_, []).
write_chars_to_stream(Stream, [Char|Chars]) :-
    put_char(Stream, Char),
    write_chars_to_stream(Stream, Chars).
```

### `put_char/2` and `put_code/2` - Write Characters

```prolog
% Write specific characters
?- put_char('A'), put_char('B'), put_char('C'), nl.
ABC
true.

% Write character codes
?- put_code(65), put_code(66), put_code(67), nl.
ABC
true.

% Generate alphabet
write_alphabet :-
    between(65, 90, Code),  % A-Z
    put_code(Code),
    fail.
write_alphabet :- nl.

?- write_alphabet.
ABCDEFGHIJKLMNOPQRSTUVWXYZ
true.
```

---

## Code I/O

### Character Code Conversion

```prolog
% Convert between characters and codes
?- char_code('A', Code).
Code = 65.

?- char_code(Char, 97).
Char = 'a'.

% Process text character by character
process_text_codes(Text, ProcessedCodes) :-
    atom_codes(Text, Codes),
    maplist(process_code, Codes, ProcessedCodes).

process_code(Code, UpperCode) :-
    (   Code >= 97, Code =< 122 ->  % lowercase a-z
        UpperCode is Code - 32       % Convert to uppercase
    ;   UpperCode = Code
    ).

?- process_text_codes('Hello World', Upper).
Upper = [72, 69, 76, 76, 79, 32, 87, 79, 82, 76, 68].
```

---

## Stream Properties

### Stream Information

```prolog
% Check stream properties
stream_property(Stream, Property)

% Example properties:
% - input/output
% - file_name(Name)
% - mode(Mode)
% - end_of_stream(Status)

check_stream_info(Stream) :-
    (   stream_property(Stream, input) ->
        writeln('Stream is for input')
    ;   writeln('Stream is for output')
    ),
    (   stream_property(Stream, file_name(Name)) ->
        format('File name: ~w~n', [Name])
    ;   writeln('No file name (console or memory)')
    ).
```

---

## Working Examples

### Example 1: Log File Manager

```prolog
% Advanced log file management
:- dynamic(log_level/1).

log_level(info).  % Default level

% Set log level
set_log_level(Level) :-
    retractall(log_level(_)),
    assert(log_level(Level)).

% Log with level checking
log(Level, Message) :-
    log_level(CurrentLevel),
    should_log(Level, CurrentLevel),
    get_time(Now),
    format_time(atom(TimeStr), '%Y-%m-%d %H:%M:%S', Now),
    open('app.log', append, Stream),
    format(Stream, '[~w] ~w: ~w~n', [TimeStr, Level, Message]),
    close(Stream).

log(Level, Format, Args) :-
    format(atom(Message), Format, Args),
    log(Level, Message).

% Log level hierarchy
should_log(error, _) :- !.
should_log(warning, Level) :- Level \== error, !.
should_log(info, Level) :- memberchk(Level, [info, warning, error]), !.
should_log(debug, debug) :- !.
should_log(_, _) :- fail.

% Usage examples
?- log(info, 'Application started').
true.

?- log(error, 'Failed to connect to database').
true.

?- log(debug, 'Processing user ~w with ID ~d', [alice, 123]).
true.

% View recent logs
view_recent_logs(N) :-
    open('app.log', read, Stream),
    read_last_n_lines(Stream, N, Lines),
    close(Stream),
    print_lines(Lines).
```

### Example 2: CSV File Processor

```prolog
% CSV file reading and processing
read_csv_file(FileName, Rows) :-
    open(FileName, read, Stream),
    read_csv_lines(Stream, Lines),
    close(Stream),
    maplist(parse_csv_line, Lines, Rows).

read_csv_lines(Stream, [Line|Lines]) :-
    read_line_to_codes(Stream, Codes),
    Codes \== end_of_file, !,
    atom_codes(Line, Codes),
    read_csv_lines(Stream, Lines).
read_csv_lines(_, []).

parse_csv_line(Line, Fields) :-
    atom_codes(Line, Codes),
    split_codes(Codes, 44, FieldCodes),  % 44 = comma
    maplist(codes_to_trimmed_atom, FieldCodes, Fields).

split_codes([], _, [[]]) :- !.
split_codes([Sep|Rest], Sep, [[]|Fields]) :- !,
    split_codes(Rest, Sep, Fields).
split_codes([C|Rest], Sep, [[C|Field1]|Fields]) :-
    split_codes(Rest, Sep, [Field1|Fields]).

codes_to_trimmed_atom(Codes, Atom) :-
    trim_codes(Codes, TrimmedCodes),
    atom_codes(Atom, TrimmedCodes).

% Write CSV file
write_csv_file(FileName, Rows) :-
    open(FileName, write, Stream),
    maplist(write_csv_row(Stream), Rows),
    close(Stream).

write_csv_row(Stream, Row) :-
    write_csv_fields(Stream, Row),
    nl(Stream).

write_csv_fields(_, []) :- !.
write_csv_fields(Stream, [Field]) :- !,
    write(Stream, Field).
write_csv_fields(Stream, [Field|Fields]) :-
    write(Stream, Field),
    write(Stream, ','),
    write_csv_fields(Stream, Fields).

% Example usage
?- write_csv_file('people.csv', [
    ['Name', 'Age', 'City'],
    ['Alice', '25', 'Rome'],
    ['Bob', '30', 'Milan']
]).
true.

?- read_csv_file('people.csv', Data).
Data = [['Name', 'Age', 'City'], ['Alice', '25', 'Rome'], ['Bob', '30', 'Milan']].
```

### Example 3: Configuration File Manager

```prolog
% Configuration file management
:- dynamic(config/2).

% Load configuration from file
load_config(FileName) :-
    retractall(config(_, _)),
    open(FileName, read, Stream),
    read_config_entries(Stream),
    close(Stream).

read_config_entries(Stream) :-
    read_line_to_codes(Stream, Line),
    (   Line == end_of_file -> true
    ;   process_config_line(Line),
        read_config_entries(Stream)
    ).

process_config_line(Codes) :-
    atom_codes(Line, Codes),
    atom_concat(Line, '', CleanLine),  % Remove whitespace
    (   atom_concat(Key, Rest, CleanLine),
        atom_concat('=', Value, Rest) ->
        assert(config(Key, Value))
    ;   true  % Skip invalid lines
    ).

% Save configuration to file
save_config(FileName) :-
    open(FileName, write, Stream),
    forall(config(Key, Value),
           format(Stream, '~w=~w~n', [Key, Value])),
    close(Stream).

% Configuration accessors
get_config(Key, Value) :-
    config(Key, Value).

get_config(Key, Default, Value) :-
    (   config(Key, Value) -> true
    ;   Value = Default
    ).

set_config(Key, Value) :-
    retractall(config(Key, _)),
    assert(config(Key, Value)).

% Example usage
?- set_config(database_host, 'localhost').
true.

?- set_config(database_port, '5432').
true.

?- save_config('app.conf').
true.

?- load_config('app.conf').
true.

?- get_config(database_host, Host).
Host = 'localhost'.
```

---

## Best Practices

### 1. Always Close Streams

```prolog
% Good: Exception-safe stream handling
safe_file_operation(File, Operation) :-
    open(File, read, Stream),
    catch(
        (call(Operation, Stream), close(Stream)),
        Error,
        (close(Stream), throw(Error))
    ).

% Or use setup_call_cleanup/3 if available
process_file_safely(File, Goal) :-
    setup_call_cleanup(
        open(File, read, Stream),
        call(Goal, Stream),
        close(Stream)
    ).
```

### 2. Handle End-of-Stream Properly

```prolog
% Good: Check for end of stream
read_all_terms(Stream, Terms) :-
    read(Stream, Term),
    (   Term == end_of_file ->
        Terms = []
    ;   Terms = [Term|RestTerms],
        read_all_terms(Stream, RestTerms)
    ).
```

### 3. Use Appropriate I/O Predicates

```prolog
% For structured data: use read/write
save_facts(File, Facts) :-
    open(File, write, Stream),
    forall(member(Fact, Facts), 
           (write_term(Stream, Fact, [quoted(true)]), 
            write(Stream, '.'), nl(Stream))),
    close(Stream).

% For text processing: use character/code operations  
count_words_in_file(File, Count) :-
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    count_words(Codes, Count).
```

### 4. Use Formatting for User Output

```prolog
% Good: Formatted, readable output
print_report(Users) :-
    format('~w~n', ['User Report']),
    format('~`=t~50|~n', []),
    format('~w~t~20|~w~t~30|~w~n', ['Name', 'Age', 'Status']),
    format('~`-t~50|~n', []),
    forall(member(user(Name, Age, Status), Users),
           format('~w~t~20|~d~t~30|~w~n', [Name, Age, Status])).
```

---

## Troubleshooting

### Common Issues

1. **Stream not closed properly**
   ```prolog
   % This can cause resource leaks
   bad_file_read(File, Data) :-
       open(File, read, Stream),
       read(Stream, Data).  % Stream never closed!
   
   % Always close streams
   good_file_read(File, Data) :-
       open(File, read, Stream),
       read(Stream, Data),
       close(Stream).
   ```

2. **Not checking for end of file**
   ```prolog
   % Can cause infinite loops
   bad_read_loop(Stream) :-
       read(Stream, Term),
       process(Term),
       bad_read_loop(Stream).  % Never terminates!
   
   % Check for end_of_file
   good_read_loop(Stream) :-
       read(Stream, Term),
       (   Term == end_of_file -> true
       ;   process(Term),
           good_read_loop(Stream)
       ).
   ```

3. **File permission errors**
   ```prolog
   % Handle file access errors
   safe_open(File, Mode, Stream) :-
       catch(open(File, Mode, Stream),
             error(existence_error(source_sink, File), _),
             (format('Error: File ~w not found~n', [File]), fail)).
   ```

### Debugging Tips

1. **Check current streams**
   ```prolog
   ?- current_input(In), current_output(Out).
   In = <stream>(stdin),
   Out = <stream>(stdout).
   ```

2. **Verify file operations**
   ```prolog
   % Test file existence before opening
   file_exists(File) :-
       catch(open(File, read, Stream),
             _, fail),
       close(Stream).
   ```

3. **Use trace for I/O debugging**
   ```prolog
   ?- trace, read_file_terms('data.txt', Terms).
   ```

---

## Launch Instructions

To test I/O predicates in JProlog:

```bash
# Start JProlog CLI
java -cp target/classes it.denzosoft.jprolog.PrologCLI

# Or launch IDE
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
```

---

**Next Steps**: Explore [Knowledge Base Guide](guide-knowledge-base.md) for database operations, or [Meta-Predicates Guide](guide-meta-predicates.md) for advanced querying.

---

*This guide is part of the JProlog documentation series. For more information, see the [User Manual](../USER_MANUAL.md) or [Quick Start Guide](guide-quick-start.md).*