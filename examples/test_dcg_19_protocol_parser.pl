% DCG Test 19: Network Protocol Parser
% Tests: HTTP headers, protocol messages, binary data

% HTTP Protocol Parser

% Characters  
crlf --> [13,10].
lf --> [10].
sp --> [32].
ht --> [9].
colon --> [58].

% HTTP version
http_version(version(Major, Minor)) -->
    [72,84,84,80,47],  % "HTTP/"
    digit(Major), [46], digit(Minor).

digit(D) --> [C], { C >= 48, C =< 57, D is C - 48 }.

% HTTP methods
method(get) --> [71,69,84].         % GET
method(post) --> [80,79,83,84].     % POST  
method(put) --> [80,85,84].         % PUT
method(delete) --> [68,69,76,69,84,69]. % DELETE

% URI (simplified)
uri(Chars) --> uri_chars(Chars).

uri_chars([C|Cs]) --> uri_char(C), uri_chars(Cs).
uri_chars([C]) --> uri_char(C).

uri_char(C) --> [C], { 
    (C >= 65, C =< 90);     % A-Z
    (C >= 97, C =< 122);    % a-z  
    (C >= 48, C =< 57);     % 0-9
    member(C, [47,45,95,46,63,61,38,37]) % /-_.?=&%
}.

% Request line
request_line(request(Method, URI, Version)) -->
    method(Method), sp,
    uri(URI), sp,
    http_version(Version), crlf.

% Header names and values
header_name([C|Cs]) --> header_char(C), header_name_rest(Cs).
header_name([C]) --> header_char(C).

header_name_rest([]) --> [].
header_name_rest([C|Cs]) --> header_char(C), header_name_rest(Cs).
header_name_rest([45|Cs]) --> [45], header_name_rest(Cs).  % hyphen

header_char(C) --> [C], { 
    (C >= 65, C =< 90);     % A-Z
    (C >= 97, C =< 122)     % a-z
}.

header_value([]) --> [].
header_value([C|Cs]) --> [C], { C \= 13, C \= 10 }, header_value(Cs).

% HTTP headers
header(header(Name, Value)) -->
    header_name(Name), colon, sp,
    header_value(Value), crlf.

% Headers list
headers([]) --> crlf.
headers([H|Hs]) --> header(H), headers(Hs).

% HTTP request
http_request(http_req(RequestLine, Headers)) -->
    request_line(RequestLine),
    headers(Headers).

% HTTP response
status_line(status(Version, Code, Reason)) -->
    http_version(Version), sp,
    status_code(Code), sp,
    reason_phrase(Reason), crlf.

status_code(Code) --> digit(D1), digit(D2), digit(D3),
    { Code is D1 * 100 + D2 * 10 + D3 }.

reason_phrase([C|Cs]) --> [C], { C \= 13 }, reason_phrase(Cs).
reason_phrase([]) --> [].

http_response(http_resp(StatusLine, Headers)) -->
    status_line(StatusLine),
    headers(Headers).

% Simple protocol messages
message_type(ping) --> [112,105,110,103].      % "ping"
message_type(pong) --> [112,111,110,103].      % "pong"
message_type(data) --> [100,97,116,97].        % "data"

message_length(L) --> 
    digit(D1), digit(D2), digit(D3), digit(D4),
    { L is D1 * 1000 + D2 * 100 + D3 * 10 + D4 }.

protocol_message(msg(Type, Length, Data)) -->
    message_type(Type), sp,
    message_length(Length), sp,
    message_data(Length, Data).

message_data(0, []) --> [].
message_data(N, [C|Cs]) --> 
    { N > 0 }, [C], 
    { N1 is N - 1 }, 
    message_data(N1, Cs).

% Test queries:
% ?- phrase(method(M), [71,69,84]).                         % Expected: M = get
% ?- phrase(http_version(V), [72,84,84,80,47,49,46,49]).    % Expected: V = version(1,1)
% ?- phrase(header(H), [72,111,115,116,58,32,101,120,97,109,112,108,101,46,99,111,109,13,10]). % Host: example.com