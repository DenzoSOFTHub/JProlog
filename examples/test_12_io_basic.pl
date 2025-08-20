% ===================================================================
% TEST 12: Basic I/O Operations
% ===================================================================
% Tests: write/1, writeln/1, nl/0, read/1, get_char/1, put_char/1

% Output formatting
greet_user(Name) :-
    write('Hello, '),
    write(Name),
    write('!'),
    nl.

print_list([]).
print_list([H|T]) :-
    write(H),
    write(' '),
    print_list(T).

print_numbered_list(List) :-
    print_numbered_list(List, 1).

print_numbered_list([], _).
print_numbered_list([H|T], N) :-
    write(N),
    write('. '),
    writeln(H),
    N1 is N + 1,
    print_numbered_list(T, N1).

% Table formatting
print_table_header :-
    writeln('Name      | Score | Grade'),
    writeln('----------|-------|------').

print_student_row(Name, Score, Grade) :-
    write(Name),
    write('      | '),
    write(Score),
    write('    | '),
    writeln(Grade).

% Character processing
echo_chars(0) :- !.
echo_chars(N) :-
    N > 0,
    get_char(Char),
    put_char(Char),
    N1 is N - 1,
    echo_chars(N1).

count_input_chars(Count) :-
    count_chars(0, Count).

count_chars(Acc, Count) :-
    get_char(Char),
    (Char = end_of_file ->
        Count = Acc
    ;   Acc1 is Acc + 1,
        count_chars(Acc1, Count)
    ).

% Menu system
show_menu :-
    nl,
    writeln('=== MENU ==='),
    writeln('1. Option One'),
    writeln('2. Option Two'),
    writeln('3. Exit'),
    write('Choose: ').

process_choice(1) :-
    writeln('You chose option one!').
process_choice(2) :-
    writeln('You chose option two!').
process_choice(3) :-
    writeln('Goodbye!').
process_choice(_) :-
    writeln('Invalid choice!').

% Debug output
debug_term(Term) :-
    write('DEBUG: '),
    write(Term),
    nl.

trace_call(Goal) :-
    write('Calling: '),
    write(Goal),
    nl,
    call(Goal),
    write('Success: '),
    write(Goal),
    nl.

% Test usage (these would need interactive testing):
% ?- greet_user(alice).
% ?- print_list([apple, banana, cherry]).
% ?- print_numbered_list([first, second, third]).
% ?- debug_term(f(a, b, c)).
% ?- trace_call(member(X, [1, 2, 3])).