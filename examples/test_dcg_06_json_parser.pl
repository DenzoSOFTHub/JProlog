% DCG Test 06: JSON Parser
% Tests: Complex nested structure parsing

% Whitespace
ws --> [].
ws --> [32], ws.  % space
ws --> [9], ws.   % tab
ws --> [10], ws.  % newline
ws --> [13], ws.  % carriage return

% JSON value types
json_value(null) --> ws, [110,117,108,108], ws.        % "null"
json_value(true) --> ws, [116,114,117,101], ws.        % "true"
json_value(false) --> ws, [102,97,108,115,101], ws.    % "false"
json_value(N) --> ws, json_number(N), ws.
json_value(S) --> ws, json_string(S), ws.
json_value(A) --> ws, json_array(A), ws.
json_value(O) --> ws, json_object(O), ws.

% JSON numbers (simplified - integers only)
json_number(N) --> json_digits(Ds), { number_codes(N, Ds) }.

json_digits([D|Ds]) --> json_digit(D), json_digits(Ds).
json_digits([D]) --> json_digit(D).

json_digit(D) --> [C], { C >= 48, C =< 57, D = C }.

% JSON strings (simplified - no escape sequences)
json_string(string(Chars)) --> [34], json_string_chars(Chars), [34].  % quotes

json_string_chars([]) --> [].
json_string_chars([C|Cs]) --> [C], { C \= 34 }, json_string_chars(Cs).

% JSON arrays
json_array([]) --> [91], ws, [93].  % empty array []
json_array([V|Vs]) --> [91], ws, json_value(V), json_array_rest(Vs), ws, [93].

json_array_rest([]) --> [].
json_array_rest([V|Vs]) --> ws, [44], ws, json_value(V), json_array_rest(Vs).  % comma

% JSON objects (simplified)
json_object([]) --> [123], ws, [125].  % empty object {}
json_object([K-V|Pairs]) --> [123], ws, json_pair(K-V), json_object_rest(Pairs), ws, [125].

json_pair(K-V) --> json_string(K), ws, [58], ws, json_value(V).  % colon

json_object_rest([]) --> [].
json_object_rest([P|Ps]) --> ws, [44], ws, json_pair(P), json_object_rest(Ps).

% Test queries:
% ?- phrase(json_value(V), [110,117,108,108]).      % Expected: V = null
% ?- phrase(json_number(N), [49,50,51]).            % Expected: N = 123
% ?- phrase(json_array(A), [91,49,44,50,93]).       % Expected: A = [1,2]