% ===================================================================
% TEST 30: Advanced I/O and Stream Processing
% ===================================================================
% Tests: Stream operations, binary I/O, formatted output, file handling

% Advanced file operations
test_file_operations :-
    open('test_data.txt', write, OutStream),
    write_data(OutStream),
    close(OutStream),
    open('test_data.txt', read, InStream),
    read_data(InStream, Data),
    close(InStream),
    process_data(Data).

write_data(Stream) :-
    write(Stream, 'Line 1\n'),
    write(Stream, 'Line 2\n'),
    write(Stream, 'Line 3\n').

read_data(Stream, Data) :-
    read_line_to_codes(Stream, Line),
    (   Line = end_of_file
    ->  Data = []
    ;   read_data(Stream, RestData),
        Data = [Line|RestData]
    ).

process_data([]).
process_data([Line|Rest]) :-
    atom_codes(Atom, Line),
    format('Read: ~w~n', [Atom]),
    process_data(Rest).

% Binary I/O operations
test_binary_io :-
    open('binary_data.bin', write, OutStream, [type(binary)]),
    write_binary_data(OutStream),
    close(OutStream),
    open('binary_data.bin', read, InStream, [type(binary)]),
    read_binary_data(InStream, Data),
    close(InStream),
    format('Binary data: ~w~n', [Data]).

write_binary_data(Stream) :-
    put_byte(Stream, 72),  % 'H'
    put_byte(Stream, 101), % 'e'
    put_byte(Stream, 108), % 'l'
    put_byte(Stream, 108), % 'l'
    put_byte(Stream, 111). % 'o'

read_binary_data(Stream, Data) :-
    get_byte(Stream, Byte),
    (   Byte = -1
    ->  Data = []
    ;   read_binary_data(Stream, RestData),
        Data = [Byte|RestData]
    ).

% Formatted output with format/2
test_formatted_output :-
    X = 42,
    Y = 3.14159,
    Name = 'Alice',
    format('Integer: ~d~n', [X]),
    format('Float: ~2f~n', [Y]),
    format('String: ~w~n', [Name]),
    format('Formatted: ~w is ~d years old~n', [Name, X]).

% Stream manipulation and pipes
test_stream_processing :-
    open_string("line1\nline2\nline3\n", InStream),
    process_lines(InStream),
    close(InStream).

process_lines(Stream) :-
    read_line_to_codes(Stream, Line),
    (   Line = end_of_file
    ->  true
    ;   atom_codes(Atom, Line),
        format('Processing: ~w~n', [Atom]),
        process_lines(Stream)
    ).

% Character-based I/O
test_character_io :-
    open_string("Hello World", Stream),
    read_chars(Stream, Chars),
    close(Stream),
    format('Characters: ~w~n', [Chars]).

read_chars(Stream, Chars) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Chars = []
    ;   read_chars(Stream, RestChars),
        Chars = [Char|RestChars]
    ).

% Stream properties and positioning
test_stream_properties :-
    open('test_file.txt', write, Stream),
    stream_property(Stream, mode(Mode)),
    stream_property(Stream, type(Type)),
    format('Stream mode: ~w, type: ~w~n', [Mode, Type]),
    close(Stream).

% Memory-based streams
test_memory_streams :-
    with_output_to(string(S), (
        write('Hello '),
        write('World'),
        nl
    )),
    format('String output: ~w~n', [S]).

% Test queries:
% ?- test_file_operations.
% ?- test_binary_io.
% ?- test_formatted_output.
% ?- test_stream_processing.
% ?- test_character_io.
% ?- test_stream_properties.
% ?- test_memory_streams.