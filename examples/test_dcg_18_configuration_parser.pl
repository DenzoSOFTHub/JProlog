% DCG Test 18: Configuration File Parser
% Tests: Key-value pairs, sections, comments

% Whitespace and comments
ws --> [].
ws --> [32], ws.  % space
ws --> [9], ws.   % tab

comment --> [35], comment_text.  % # comment
comment_text --> [].
comment_text --> [C], { C \= 10 }, comment_text.

eol --> [10].     % newline
eol --> [13].     % carriage return
eol --> [13,10].  % CRLF

% Skip blank lines and comments
skip_blanks --> [].
skip_blanks --> ws, eol, skip_blanks.
skip_blanks --> ws, comment, eol, skip_blanks.

% Configuration identifiers
config_id([C|Cs]) --> config_char(C), config_id_rest(Cs).
config_id([C]) --> config_char(C).

config_id_rest([]) --> [].
config_id_rest([C|Cs]) --> config_char(C), config_id_rest(Cs).
config_id_rest([95|Cs]) --> [95], config_id_rest(Cs).     % underscore
config_id_rest([45|Cs]) --> [45], config_id_rest(Cs).     % hyphen
config_id_rest([46|Cs]) --> [46], config_id_rest(Cs).     % dot

config_char(C) --> [C], { (C >= 97, C =< 122); (C >= 65, C =< 90); (C >= 48, C =< 57) }.

% Values
config_value(string(Chars)) --> quoted_string(Chars).
config_value(number(N)) --> config_number(N).
config_value(boolean(true)) --> [116,114,117,101].        % true
config_value(boolean(false)) --> [102,97,108,115,101].    % false
config_value(identifier(Id)) --> config_id(Id).

% Quoted strings
quoted_string(Chars) --> [34], quoted_chars(Chars), [34].  % "string"

quoted_chars([]) --> [].
quoted_chars([92,C|Cs]) --> [92,C], quoted_chars(Cs).     % escaped char
quoted_chars([C|Cs]) --> [C], { C \= 34, C \= 92 }, quoted_chars(Cs).

% Numbers (integers and floats)
config_number(N) --> digits(Ds), { number_codes(N, Ds) }.
config_number(N) --> digits(D1), [46], digits(D2), 
    { append(D1, [46|D2], All), number_codes(N, All) }.

digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [C], { C >= 48, C =< 57, D = C }.

% Key-value pairs
key_value(config(Key, Value)) --> 
    config_id(Key), ws, [61], ws, config_value(Value).     % key = value

% Sections
section_header(section(Name)) -->
    [91], ws, config_id(Name), ws, [93].                   % [section]

% Configuration entries
config_entry(E) --> key_value(E).
config_entry(E) --> section_header(E).

% Configuration file
config_file([]) --> skip_blanks.
config_file([Entry|Entries]) -->
    skip_blanks,
    config_entry(Entry), ws, eol,
    config_file(Entries).

% INI file format
ini_file(ini(Sections)) --> ini_sections(Sections).

ini_sections([]) --> skip_blanks.
ini_sections([Section|Sections]) -->
    skip_blanks,
    ini_section(Section),
    ini_sections(Sections).

ini_section(section(Name, Entries)) -->
    section_header(section(Name)), ws, eol,
    ini_entries(Entries).

ini_entries([]) --> [].
ini_entries([Entry|Entries]) -->
    skip_blanks,
    key_value(Entry), ws, eol,
    ini_entries(Entries).

% Test queries:
% ?- phrase(config_id(Id), [115,101,114,118,101,114]).       % Expected: Id = [115,101,114,118,101,114] ("server")
% ?- phrase(key_value(KV), [112,111,114,116,61,56,48]).      % Expected: KV = config([112,111,114,116],number(80))
% ?- phrase(section_header(S), [91,100,97,116,97,98,97,115,101,93]). % Expected: S = section([100,97,116,97,98,97,115,101])