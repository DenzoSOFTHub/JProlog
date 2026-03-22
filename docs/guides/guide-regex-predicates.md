# JProlog Regular Expression Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.regex` package provides predicates for
pattern matching, extraction, replacement, and splitting of atoms using Java
regular expressions. These predicates wrap `java.util.regex.Pattern` and
`java.util.regex.Matcher`, giving Prolog programs full access to Java's regex
engine.

All pattern and input arguments must be atoms. Patterns follow Java regex
syntax (which is largely compatible with Perl-style regular expressions).
Capture groups use parentheses `()` in the pattern and are returned as Prolog
lists.

**Source file:**
`src/main/java/it/denzosoft/jprolog/builtin/regex/RegexPredicates.java`

**Registered in:** `BuiltInFactory` (ISS-2025-0117)

---

## Predicate Reference

### re_match/2

```prolog
re_match(+Pattern, +String)
```

**Arguments:**
- `Pattern` (atom, input) -- A Java regular expression pattern.
- `String` (atom, input) -- The string to search.

**Description:** Succeeds if `Pattern` matches anywhere within `String` (uses
`Matcher.find()`, not `Matcher.matches()`). Fails if no match is found. Throws
an evaluation error if `Pattern` has invalid regex syntax.

**Example:**
```prolog
?- re_match('[0-9]+', 'Order #12345').
true.

?- re_match('^[A-Z]+$', 'hello').
false.
```

---

### re_matchsub/3

```prolog
re_matchsub(+Pattern, +String, -Groups)
```

**Arguments:**
- `Pattern` (atom, input) -- A Java regular expression with capture groups.
- `String` (atom, input) -- The string to search.
- `Groups` (list of atoms, output) -- List containing the full match (group 0)
  followed by each capture group (group 1, group 2, ...).

**Description:** Finds the first match of `Pattern` in `String` and unifies
`Groups` with a list of atoms representing the matched groups. Group 0 is
always the full match. If a capture group did not participate in the match,
it is returned as the empty atom `''`. Fails if no match is found.

**Example:**
```prolog
?- re_matchsub('(\\w+)@(\\w+\\.\\w+)', 'user@example.com', G).
G = ['user@example.com', 'user', 'example.com'].
```

---

### re_replace/4

```prolog
re_replace(+Pattern, +Replacement, +String, -Result)
```

**Arguments:**
- `Pattern` (atom, input) -- A Java regular expression.
- `Replacement` (atom, input) -- The replacement string. May use backreferences
  like `$1`, `$2` to refer to captured groups.
- `String` (atom, input) -- The input string.
- `Result` (atom, output) -- The result after replacing **all** occurrences.

**Description:** Replaces all occurrences of `Pattern` in `String` with
`Replacement` (uses `String.replaceAll()`). Unifies the result with `Result`.

**Example:**
```prolog
?- re_replace('\\s+', '-', 'hello   world  test', R).
R = 'hello-world-test'.
```

---

### re_split/3

```prolog
re_split(+Pattern, +String, -Parts)
```

**Arguments:**
- `Pattern` (atom, input) -- A Java regular expression used as the delimiter.
- `String` (atom, input) -- The string to split.
- `Parts` (list of atoms, output) -- The resulting parts after splitting.

**Description:** Splits `String` around occurrences of `Pattern` and unifies
`Parts` with a list of the resulting substrings. Uses `String.split()`.

**Example:**
```prolog
?- re_split('[,;]\\s*', 'apple, banana; cherry, date', Parts).
Parts = ['apple', 'banana', 'cherry', 'date'].
```

---

### re_findall/3

```prolog
re_findall(+Pattern, +String, -Matches)
```

**Arguments:**
- `Pattern` (atom, input) -- A Java regular expression.
- `String` (atom, input) -- The string to search.
- `Matches` (list of atoms, output) -- All non-overlapping matches.

**Description:** Finds all non-overlapping occurrences of `Pattern` in
`String` and unifies `Matches` with a list of the matched substrings (group 0
of each match). Returns an empty list if there are no matches.

**Example:**
```prolog
?- re_findall('[A-Z][a-z]+', 'Alice met Bob and Charlie', Names).
Names = ['Alice', 'Bob', 'Charlie'].
```

---

## Real-World Examples

### Example 1: Log File Parser

This program parses structured log lines to extract timestamps, severity
levels, and messages. It then filters for errors and warnings.

```prolog
% log_parser.pl -- Parse and analyze structured log data.
%
% Expected log format:
%   2024-03-21 14:30:45 [ERROR] Database connection failed: timeout
%   2024-03-21 14:30:46 [INFO] Retrying connection (attempt 2/5)

% parse_log_line(+Line, -Timestamp, -Level, -Message)
% Extract the three components from a single log line.
parse_log_line(Line, Timestamp, Level, Message) :-
    re_matchsub(
        '(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}) \\[(\\w+)\\] (.*)',
        Line, Groups),
    Groups = [_, Timestamp, Level, Message].

% parse_all_lines(+Lines, -Entries)
% Parse a list of log line atoms into structured entries.
parse_all_lines([], []).
parse_all_lines([Line|Rest], Entries) :-
    (   parse_log_line(Line, Ts, Lev, Msg)
    ->  Entries = [log(Ts, Lev, Msg)|More]
    ;   Entries = More  % skip malformed lines
    ),
    parse_all_lines(Rest, More).

% filter_by_level(+Entries, +Level, -Filtered)
filter_by_level([], _, []).
filter_by_level([log(Ts, Lev, Msg)|Rest], Level, [log(Ts, Lev, Msg)|Filtered]) :-
    Lev = Level, !,
    filter_by_level(Rest, Level, Filtered).
filter_by_level([_|Rest], Level, Filtered) :-
    filter_by_level(Rest, Level, Filtered).

% analyze_log(+LogText)
% Split the log text into lines, parse, and report findings.
analyze_log(LogText) :-
    re_split('\\n', LogText, Lines),
    parse_all_lines(Lines, Entries),
    length(Entries, Total),
    write('Total log entries: '), write(Total), nl, nl,
    % Count by severity
    filter_by_level(Entries, 'ERROR', Errors),
    filter_by_level(Entries, 'WARN', Warnings),
    filter_by_level(Entries, 'INFO', Infos),
    length(Errors, ErrCount),
    length(Warnings, WarnCount),
    length(Infos, InfoCount),
    write('Errors:   '), write(ErrCount), nl,
    write('Warnings: '), write(WarnCount), nl,
    write('Info:     '), write(InfoCount), nl, nl,
    % Print error details
    (   Errors \= []
    ->  write('=== Error Details ==='), nl,
        print_entries(Errors)
    ;   write('No errors found.'), nl
    ).

print_entries([]).
print_entries([log(Ts, Lev, Msg)|Rest]) :-
    write('  ['), write(Ts), write('] '),
    write(Lev), write(': '), write(Msg), nl,
    print_entries(Rest).

% Usage:
%   ?- analyze_log('2024-03-21 14:30:45 [ERROR] DB connection timeout\n2024-03-21 14:30:46 [INFO] Retry attempt 2\n2024-03-21 14:30:47 [WARN] Slow query detected\n2024-03-21 14:30:48 [ERROR] Out of memory').
%   Total log entries: 4
%
%   Errors:   2
%   Warnings: 1
%   Info:     1
%
%   === Error Details ===
%     [2024-03-21 14:30:45] ERROR: DB connection timeout
%     [2024-03-21 14:30:48] ERROR: Out of memory
```

---

### Example 2: Email Validator with Detailed Error Reporting

This program validates email addresses against multiple rules and provides
specific error messages for each violation.

```prolog
% email_validator.pl -- Validate email addresses with detailed diagnostics.

% validate_email(+Email, -Status)
% Status is either 'valid' or invalid(Reasons) where Reasons is a list.
validate_email(Email, Status) :-
    findall(Reason, email_violation(Email, Reason), Reasons),
    (   Reasons = []
    ->  Status = valid
    ;   Status = invalid(Reasons)
    ).

% Each clause checks one rule. If the rule is violated, Reason is unified.
email_violation(Email, 'Missing @ symbol') :-
    \+ re_match('@', Email).

email_violation(Email, 'Multiple @ symbols') :-
    re_findall('@', Email, Matches),
    length(Matches, Count),
    Count > 1.

email_violation(Email, 'Local part is empty') :-
    re_match('^@', Email).

email_violation(Email, 'Domain part is empty') :-
    re_match('@$', Email).

email_violation(Email, 'Local part contains invalid characters') :-
    re_match('@', Email),  % only check if @ is present
    re_matchsub('^([^@]+)@', Email, [_, LocalPart]),
    \+ re_match('^[a-zA-Z0-9.!#$%&*+/=?^_`{|}~-]+$', LocalPart).

email_violation(Email, 'Domain contains invalid characters') :-
    re_matchsub('@([^@]+)$', Email, [_, Domain]),
    \+ re_match('^[a-zA-Z0-9.-]+$', Domain).

email_violation(Email, 'Domain has no dot (no TLD)') :-
    re_matchsub('@([^@]+)$', Email, [_, Domain]),
    \+ re_match('\\.', Domain).

email_violation(Email, 'Domain starts or ends with hyphen') :-
    re_matchsub('@([^@]+)$', Email, [_, Domain]),
    (   re_match('^-', Domain)
    ;   re_match('-$', Domain)
    ).

email_violation(Email, 'Consecutive dots in local part') :-
    re_matchsub('^([^@]+)@', Email, [_, LocalPart]),
    re_match('\\.\\.', LocalPart).

% validate_list(+Emails)
% Validate a list of emails and print a report.
validate_list([]).
validate_list([Email|Rest]) :-
    validate_email(Email, Status),
    write(Email), write(': '),
    (   Status = valid
    ->  write('VALID')
    ;   Status = invalid(Reasons),
        write('INVALID'), nl,
        print_reasons(Reasons)
    ), nl,
    validate_list(Rest).

print_reasons([]).
print_reasons([R|Rs]) :-
    write('    - '), write(R), nl,
    print_reasons(Rs).

% Usage:
%   ?- validate_list([
%        'alice@example.com',
%        'bob@',
%        'charlie@@host.org',
%        'dana..jones@test.com',
%        'eve@-bad-domain.com',
%        'valid.user+tag@mail.example.org'
%      ]).
%   alice@example.com: VALID
%   bob@: INVALID
%       - Domain part is empty
%       - Domain has no dot (no TLD)
%   charlie@@host.org: INVALID
%       - Multiple @ symbols
%   dana..jones@test.com: INVALID
%       - Consecutive dots in local part
%   eve@-bad-domain.com: INVALID
%       - Domain starts or ends with hyphen
%   valid.user+tag@mail.example.org: VALID
```

---

### Example 3: Source Code Tokenizer

This program tokenizes simple source code into meaningful tokens using
regex patterns, classifying each token by type.

```prolog
% tokenizer.pl -- Tokenize source code into classified tokens.

% token_patterns(-Patterns)
% Each pattern is type-regex. Tried in order; first match wins.
token_patterns([
    keyword-'\\b(if|else|while|for|return|function|var|let|const)\\b',
    number-'\\b\\d+(\\.\\d+)?\\b',
    string-'"[^"]*"',
    operator-'[+\\-*/=<>!&|]+',
    identifier-'[a-zA-Z_][a-zA-Z0-9_]*',
    punctuation-'[{}()\\[\\];,.]',
    whitespace-'\\s+'
]).

% tokenize(+Code, -Tokens)
% Produce a list of token(Type, Value) from the input code string.
tokenize(Code, Tokens) :-
    token_patterns(Patterns),
    tokenize_loop(Code, Patterns, Tokens).

tokenize_loop(Code, _, []) :-
    % Base case: nothing left or only whitespace
    (   Code = ''
    ;   re_match('^\\s*$', Code)
    ), !.
tokenize_loop(Code, Patterns, Tokens) :-
    % Try each pattern at the beginning of Code
    try_patterns(Patterns, Code, Patterns, Type, Match, Rest),
    (   Type = whitespace
    ->  Tokens = MoreTokens  % skip whitespace tokens
    ;   Tokens = [token(Type, Match)|MoreTokens]
    ),
    tokenize_loop(Rest, Patterns, MoreTokens).

% try_patterns(+RemainingPatterns, +Code, +AllPatterns, -Type, -Match, -Rest)
try_patterns([Type-Regex|_], Code, _, Type, Match, Rest) :-
    atom_concat('^(', Regex, T1),
    atom_concat(T1, ')', AnchoredPattern),
    re_matchsub(AnchoredPattern, Code, [Match|_]),
    Match \= '', !,
    atom_length(Match, Len),
    atom_length(Code, CodeLen),
    RestLen is CodeLen - Len,
    sub_atom(Code, Len, RestLen, 0, Rest).
try_patterns([_|More], Code, All, Type, Match, Rest) :-
    try_patterns(More, Code, All, Type, Match, Rest).

% print_tokens(+Tokens)
print_tokens([]).
print_tokens([token(Type, Value)|Rest]) :-
    write(Type), write(': '), write(Value), nl,
    print_tokens(Rest).

% Usage:
%   ?- tokenize('var x = 42 + y;', Tokens), print_tokens(Tokens).
%   keyword: var
%   identifier: x
%   operator: =
%   number: 42
%   operator: +
%   identifier: y
%   punctuation: ;
```

---

### Example 4: Data Cleaner -- Normalize Messy Input

This program takes raw contact data and normalizes phone numbers, dates,
and names into a standard format.

```prolog
% data_cleaner.pl -- Normalize messy contact data fields.

% normalize_phone(+Raw, -Clean)
% Strip all non-digit characters, then format as (XXX) XXX-XXXX for US numbers.
normalize_phone(Raw, Clean) :-
    re_replace('[^0-9]', '', Raw, DigitsOnly),
    atom_length(DigitsOnly, Len),
    (   Len =:= 11, sub_atom(DigitsOnly, 0, 1, _, '1')
    ->  % Strip leading country code '1'
        sub_atom(DigitsOnly, 1, 10, 0, TenDigits),
        format_us_phone(TenDigits, Clean)
    ;   Len =:= 10
    ->  format_us_phone(DigitsOnly, Clean)
    ;   Clean = DigitsOnly  % return digits as-is for non-US numbers
    ).

format_us_phone(TenDigits, Formatted) :-
    sub_atom(TenDigits, 0, 3, _, Area),
    sub_atom(TenDigits, 3, 3, _, Prefix),
    sub_atom(TenDigits, 6, 4, _, Line),
    atomic_list_concat(['(', Area, ') ', Prefix, '-', Line], Formatted).

% normalize_date(+Raw, -Clean)
% Recognize multiple date formats and convert to YYYY-MM-DD.
normalize_date(Raw, Clean) :-
    (   % MM/DD/YYYY or MM-DD-YYYY
        re_matchsub('^(\\d{1,2})[/-](\\d{1,2})[/-](\\d{4})$', Raw, [_, M, D, Y])
    ->  pad_two(M, MP), pad_two(D, DP),
        atomic_list_concat([Y, '-', MP, '-', DP], Clean)
    ;   % YYYY/MM/DD or YYYY-MM-DD (already good, just normalize separator)
        re_matchsub('^(\\d{4})[/-](\\d{1,2})[/-](\\d{1,2})$', Raw, [_, Y, M, D])
    ->  pad_two(M, MP), pad_two(D, DP),
        atomic_list_concat([Y, '-', MP, '-', DP], Clean)
    ;   % Month DD, YYYY (e.g., "March 21, 2024")
        re_matchsub('^([A-Za-z]+)\\s+(\\d{1,2}),?\\s+(\\d{4})$', Raw, [_, MonName, D, Y])
    ->  month_number(MonName, M),
        pad_two(M, MP), pad_two(D, DP),
        atomic_list_concat([Y, '-', MP, '-', DP], Clean)
    ;   Clean = Raw  % return as-is if unrecognized
    ).

pad_two(Atom, Padded) :-
    atom_string(Atom, S),
    atom_length(S, L),
    (   L =:= 1
    ->  atom_concat('0', Atom, Padded)
    ;   Padded = Atom
    ).

month_number(Name, Num) :-
    downcase_atom(Name, Lower),
    member(Lower-Num, [
        'january'-'1', 'february'-'2', 'march'-'3', 'april'-'4',
        'may'-'5', 'june'-'6', 'july'-'7', 'august'-'8',
        'september'-'9', 'october'-'10', 'november'-'11', 'december'-'12',
        'jan'-'1', 'feb'-'2', 'mar'-'3', 'apr'-'4',
        'jun'-'6', 'jul'-'7', 'aug'-'8', 'sep'-'9',
        'oct'-'10', 'nov'-'11', 'dec'-'12'
    ]), !.
month_number(_, '0').

% normalize_name(+Raw, -Clean)
% Trim extra whitespace and apply title case.
normalize_name(Raw, Clean) :-
    re_replace('^\\s+|\\s+$', '', Raw, Trimmed),
    re_replace('\\s+', ' ', Trimmed, SingleSpaced),
    re_split(' ', SingleSpaced, Parts),
    maplist(title_case_word, Parts, TitleParts),
    atomic_list_concat(TitleParts, ' ', Clean).

title_case_word(Word, TitleWord) :-
    atom_chars(Word, [First|Rest]),
    upcase_atom(First, Upper),
    atom_chars(LowerRest, Rest),
    downcase_atom(LowerRest, Lower),
    atom_chars(Lower, LowerChars),
    atom_chars(TitleWord, [Upper|LowerChars]).

% clean_contact(+RawName, +RawPhone, +RawDate, -Result)
clean_contact(RawName, RawPhone, RawDate, contact(Name, Phone, Date)) :-
    normalize_name(RawName, Name),
    normalize_phone(RawPhone, Phone),
    normalize_date(RawDate, Date).

% Usage:
%   ?- clean_contact('  john   DOE  ', '1-555-867-5309', '3/21/2024', R).
%   R = contact('John Doe', '(555) 867-5309', '2024-03-21').
%
%   ?- clean_contact('jane smith', '(800) 555.1234', 'March 15, 2024', R).
%   R = contact('Jane Smith', '(800) 555-1234', '2024-03-15').
```

---

### Example 5: URL Parser

This program extracts the protocol, host, optional port, path, and query
string from URLs.

```prolog
% url_parser.pl -- Parse URLs into structured components.

% parse_url(+URL, -Components)
% Components is a structure: url(Protocol, Host, Port, Path, Query).
% Missing components are represented by the atom 'none'.
parse_url(URL, url(Protocol, Host, Port, Path, Query)) :-
    % Full pattern with optional port, path, and query
    (   re_matchsub(
            '^([a-zA-Z][a-zA-Z0-9+.-]*)://([^/:?#]+)(?::(\\d+))?([^?#]*)?(?:\\?([^#]*))?',
            URL, Groups)
    ->  Groups = [_, Protocol, Host | OptionalParts],
        extract_optional(OptionalParts, 0, RawPort),
        extract_optional(OptionalParts, 1, RawPath),
        extract_optional(OptionalParts, 2, RawQuery),
        % Normalize port
        (   RawPort = '' -> Port = none ; Port = RawPort ),
        % Normalize path
        (   RawPath = '' -> Path = '/' ; Path = RawPath ),
        % Normalize query
        (   RawQuery = '' -> Query = none ; Query = RawQuery )
    ;   % Fail gracefully
        Protocol = none, Host = none, Port = none,
        Path = none, Query = none,
        write('Warning: Could not parse URL: '), write(URL), nl
    ).

extract_optional(List, Index, Value) :-
    (   nth0(Index, List, Value)
    ->  true
    ;   Value = ''
    ).

% parse_query_params(+QueryString, -Params)
% Parse a query string like 'key1=val1&key2=val2' into a list of Key-Value pairs.
parse_query_params(none, []) :- !.
parse_query_params(QueryString, Params) :-
    re_split('&', QueryString, Pairs),
    maplist(parse_one_param, Pairs, Params).

parse_one_param(PairAtom, Key-Value) :-
    (   re_matchsub('^([^=]+)=(.*)$', PairAtom, [_, Key, Value])
    ->  true
    ;   Key = PairAtom, Value = ''
    ).

% analyze_url(+URL)
% Parse a URL and print its components in a readable format.
analyze_url(URL) :-
    parse_url(URL, url(Proto, Host, Port, Path, Query)),
    write('URL:      '), write(URL), nl,
    write('Protocol: '), write(Proto), nl,
    write('Host:     '), write(Host), nl,
    write('Port:     '), write(Port), nl,
    write('Path:     '), write(Path), nl,
    write('Query:    '), write(Query), nl,
    (   Query \= none
    ->  parse_query_params(Query, Params),
        write('Params:'), nl,
        print_params(Params)
    ;   true
    ), nl.

print_params([]).
print_params([K-V|Rest]) :-
    write('  '), write(K), write(' = '), write(V), nl,
    print_params(Rest).

% batch_analyze(+URLs)
% Analyze multiple URLs and classify by protocol.
batch_analyze(URLs) :-
    write('=== URL Analysis Report ==='), nl, nl,
    maplist(analyze_url, URLs),
    classify_protocols(URLs).

classify_protocols(URLs) :-
    findall(Proto,
        (member(U, URLs), parse_url(U, url(Proto, _, _, _, _)), Proto \= none),
        Protos),
    sort(Protos, UniqueProtos),
    write('Protocols found: '), write(UniqueProtos), nl.

% Usage:
%   ?- batch_analyze([
%        'https://api.example.com:8443/v2/users?page=3&limit=50',
%        'http://localhost/health',
%        'ftp://files.corp.net/reports/2024/q1.csv',
%        'https://search.example.org/find?q=prolog+regex&lang=en'
%      ]).
%   === URL Analysis Report ===
%
%   URL:      https://api.example.com:8443/v2/users?page=3&limit=50
%   Protocol: https
%   Host:     api.example.com
%   Port:     8443
%   Path:     /v2/users
%   Query:    page=3&limit=50
%   Params:
%     page = 3
%     limit = 50
%
%   URL:      http://localhost/health
%   Protocol: http
%   Host:     localhost
%   Port:     none
%   Path:     /health
%   Query:    none
%
%   URL:      ftp://files.corp.net/reports/2024/q1.csv
%   Protocol: ftp
%   Host:     files.corp.net
%   Port:     none
%   Path:     /reports/2024/q1.csv
%   Query:    none
%
%   URL:      https://search.example.org/find?q=prolog+regex&lang=en
%   Protocol: https
%   Host:     search.example.org
%   Port:     none
%   Path:     /find
%   Query:    q=prolog+regex&lang=en
%   Params:
%     q = prolog+regex
%     lang = en
%
%   Protocols found: [ftp, http, https]
```
