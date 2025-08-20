% DCG Test 10: CSV Parser
% Tests: Field parsing, quote handling, row/column structure

% CSV field characters (not comma, newline, or quote)
field_char(C) --> [C], { C \= 44, C \= 10, C \= 13, C \= 34 }.

% Simple field (no quotes)
simple_field([]) --> [].
simple_field([C|Cs]) --> field_char(C), simple_field(Cs).

% Quoted field (handles quotes inside)
quoted_field(Chars) --> [34], quoted_content(Chars), [34].  % " content "

quoted_content([]) --> [].
quoted_content([34|Cs]) --> [34,34], quoted_content(Cs).    % "" becomes "
quoted_content([C|Cs]) --> [C], { C \= 34 }, quoted_content(Cs).

% CSV field (quoted or unquoted)
csv_field(field(Chars)) --> simple_field(Chars).
csv_field(field(Chars)) --> quoted_field(Chars).

% CSV row (fields separated by commas)
csv_row([Field]) --> csv_field(Field).
csv_row([Field|Fields]) --> csv_field(Field), [44], csv_row(Fields).  % comma

% End of line
eol --> [10].     % newline
eol --> [13].     % carriage return  
eol --> [13,10].  % CR+LF

% CSV file (multiple rows)
csv_file([]) --> [].
csv_file([Row]) --> csv_row(Row).
csv_file([Row|Rows]) --> csv_row(Row), eol, csv_file(Rows).

% Helper to extract field text
field_text(field(Chars), Text) :- atom_codes(Text, Chars).

% Process CSV data
process_csv(Data, ProcessedRows) :-
    phrase(csv_file(Rows), Data),
    maplist(process_row, Rows, ProcessedRows).

process_row(Fields, TextFields) :-
    maplist(field_text, Fields, TextFields).

% Test queries:
% ?- phrase(csv_field(F), [104,101,108,108,111]).   % Expected: F = field([104,101,108,108,111])
% ?- phrase(csv_row(R), [97,44,98,44,99]).          % Expected: R = [field([97]),field([98]),field([99])]
% ?- phrase(csv_file(F), [97,44,98,10,99,44,100]).  % Expected: F = [[field([97]),field([98])],[field([99]),field([100])]]