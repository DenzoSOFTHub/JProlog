# HTTP Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.http` package provides a complete HTTP server and client toolkit for JProlog. It enables Prolog programs to:

- Start and manage embedded HTTP servers on configurable ports
- Register path-based request handlers and process incoming requests
- Send plain-text and JSON responses with custom status codes
- Make outgoing HTTP GET, POST, PUT, and DELETE requests
- Perform URL encoding and decoding

The server uses Java's built-in `com.sun.net.httpserver.HttpServer` with a queueing model: incoming requests are placed into a blocking queue, and Prolog code retrieves them one at a time via `http_get_request/2`. This design allows Prolog's single-threaded execution model to handle HTTP traffic without concurrency issues. The client side uses Java's `HttpClient` with a 30-second default timeout.

All server handles are atoms of the form `http_server_N` (auto-generated). Request IDs follow the pattern `http_server_N_req_M`. These opaque handles are used to correlate requests with responses.

**Source file**: `src/main/java/it/denzosoft/jprolog/builtin/http/HttpServerPredicates.java`

---

## Predicate Reference

### http_server(+Port, -ServerHandle)

Start an embedded HTTP server listening on the specified TCP port.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Port | integer | input | TCP port number to listen on (e.g., 8080) |
| ServerHandle | atom | output | Opaque handle for referencing this server instance |

A default root handler (`/`) is created that queues all incoming requests. The server runs on daemon threads using a cached thread pool, so it will not prevent the JVM from exiting.

**Errors**: Throws `evaluation_error` if Port is not a number, or if the port is already in use.

```prolog
?- http_server(8080, Server).
Server = http_server_1.
```

---

### http_stop(+ServerHandle)

Stop a running HTTP server and release all associated resources.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| ServerHandle | atom | input | Handle returned by `http_server/2` |

Stops the server with a 1-second graceful shutdown delay. All pending exchanges for this server are discarded.

**Errors**: Throws `evaluation_error` if no server exists with the given handle.

```prolog
?- http_stop(http_server_1).
true.
```

---

### http_handler(+ServerHandle, +Path, +HandlerAtom)

Register a new URL path context on a running server.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| ServerHandle | atom | input | Handle returned by `http_server/2` |
| Path | atom | input | URL path prefix (e.g., `'/api/users'`) |
| HandlerAtom | atom | input | Identifier for this handler (stored for identification) |

All requests to paths matching the registered prefix are queued and retrievable via `http_get_request/2`. The HandlerAtom is recorded for identification purposes but all requests go to the same queue per server.

**Errors**: Throws `evaluation_error` if the server handle is unknown.

```prolog
?- http_handler(Server, '/api/items', items_handler).
true.
```

---

### http_get_request(+ServerHandle, -Request)

Block and wait for the next incoming HTTP request (up to 30 seconds).

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| ServerHandle | atom | input | Handle returned by `http_server/2` |
| Request | compound | output | Term of the form `request(Method, Path, Headers, Body, RequestId)` |

The returned `Request` term has the following structure:

- **Method**: atom -- HTTP method in lowercase (`get`, `post`, `put`, `delete`)
- **Path**: atom -- request URI path, including query string if present (e.g., `'/api/items?id=5'`)
- **Headers**: list -- list of `header(Name, Value)` terms with lowercase header names
- **Body**: atom -- request body as a string (empty string `''` for GET requests)
- **RequestId**: atom -- unique identifier used to send a response via `http_reply/4` or `http_reply_json/3`

**Fails** if no request arrives within 30 seconds (timeout).

**Errors**: Throws `evaluation_error` if the server handle is unknown.

```prolog
?- http_get_request(Server, Request).
Request = request(get, '/api/items', [header(host, 'localhost:8080')], '', http_server_1_req_1).
```

---

### http_reply(+ServerHandle, +RequestId, +StatusCode, +Body)

Send a plain-text HTTP response to a pending request.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| ServerHandle | atom | input | Server handle (accepted but keyed on RequestId) |
| RequestId | atom | input | Request ID from the `request/5` term |
| StatusCode | integer | input | HTTP status code (e.g., 200, 404, 500) |
| Body | atom | input | Response body as plain text |

Sets `Content-Type: text/plain; charset=utf-8`.

**Errors**: Throws `evaluation_error` if StatusCode is not a number or if no pending request matches the RequestId.

```prolog
?- http_reply(Server, ReqId, 200, 'Hello, World!').
true.
```

---

### http_reply_json(+ServerHandle, +RequestId, +JsonTerm)

Send a JSON HTTP response with status code 200.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| ServerHandle | atom | input | Server handle (accepted but keyed on RequestId) |
| RequestId | atom | input | Request ID from the `request/5` term |
| JsonTerm | atom | input | JSON string to send as the response body |

Sets `Content-Type: application/json; charset=utf-8` and always returns status 200.

**Errors**: Throws `evaluation_error` if no pending request matches the RequestId.

```prolog
?- http_reply_json(Server, ReqId, '{"status":"ok","count":42}').
true.
```

---

### http_client_get(+Url, -Response)

Make an outgoing HTTP GET request to the specified URL.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Url | atom | input | Full URL including scheme (e.g., `'http://example.com/api'`) |
| Response | compound | output | Term of the form `response(StatusCode, Headers, Body)` |

The returned `Response` term contains:

- **StatusCode**: integer -- HTTP response status code
- **Headers**: list -- list of `header(Name, Value)` terms
- **Body**: atom -- response body as a string

Timeout is 30 seconds.

```prolog
?- http_client_get('http://httpbin.org/get', Response).
Response = response(200, [...], '{"args":{},...}').
```

---

### http_client_post(+Url, +PostBody, -Response)

Make an outgoing HTTP POST request.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Url | atom | input | Full URL including scheme |
| PostBody | atom | input | Request body to send |
| Response | compound | output | Term of the form `response(StatusCode, Headers, Body)` |

Sends with `Content-Type: text/plain; charset=utf-8`. Timeout is 30 seconds.

```prolog
?- http_client_post('http://httpbin.org/post', '{"key":"value"}', Response).
Response = response(200, [...], '...').
```

---

### http_open(+Url, +Options, -Response)

General-purpose HTTP request with configurable method, headers, and body.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Url | atom | input | Full URL including scheme |
| Options | list | input | List of option terms (see below) |
| Response | compound | output | Term of the form `response(StatusCode, Headers, Body)` |

**Supported options:**

| Option | Description |
|--------|-------------|
| `method(get)` | HTTP GET (default) |
| `method(post)` | HTTP POST |
| `method(put)` | HTTP PUT |
| `method(delete)` | HTTP DELETE |
| `body(Data)` | Request body as an atom |
| `header(Name, Value)` | Add a custom HTTP header |

**Errors**: Throws `evaluation_error` if Options is not a list.

```prolog
?- http_open('http://example.com/api/items/5',
             [method(put), body('{"name":"updated"}'),
              header('Content-Type', 'application/json')],
             Response).
Response = response(200, [...], '...').
```

---

### url_encode(+Text, -Encoded)

URL-encode a text string using UTF-8.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Text | atom | input | Plain text to encode |
| Encoded | atom | output | URL-encoded string |

```prolog
?- url_encode('hello world', E).
E = 'hello+world'.

?- url_encode('a=1&b=2', E).
E = 'a%3D1%26b%3D2'.
```

---

### url_decode(+Encoded, -Text)

Decode a URL-encoded string back to plain text using UTF-8.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Encoded | atom | input | URL-encoded string |
| Text | atom | output | Decoded plain text |

```prolog
?- url_decode('hello+world', T).
T = 'hello world'.
```

---

## Real-World Examples

### Example 1: REST API Server for CRUD Operations on a Knowledge Base

This example creates a REST API that manages a collection of books stored as Prolog facts. It supports creating, reading, updating, and deleting book records through HTTP endpoints.

```prolog
% ============================================================
% REST API Server: Book Collection Manager
% Endpoints:
%   GET  /api/books       - list all books
%   GET  /api/books?id=N  - get a specific book
%   POST /api/books       - add a new book (body: "title,author,year")
%   DELETE /api/books?id=N - remove a book
% ============================================================

:- dynamic book/4.  % book(Id, Title, Author, Year)

% Seed data
book(1, 'The Art of Prolog', 'Sterling & Shapiro', 1994).
book(2, 'Craft of Prolog', 'OKeefe', 1990).
book(3, 'Programming in Prolog', 'Clocksin & Mellish', 2003).

% Counter for generating IDs
:- dynamic next_book_id/1.
next_book_id(4).

% ---- Server startup ----

start_book_server(Port) :-
    http_server(Port, Server),
    http_handler(Server, '/api/books', books_handler),
    write('Book API server started on port '), write(Port), nl,
    serve_loop(Server).

% ---- Main request loop ----

serve_loop(Server) :-
    http_get_request(Server, Request),
    !,
    handle_request(Server, Request),
    serve_loop(Server).
serve_loop(Server) :-
    % Timeout - keep listening
    serve_loop(Server).

% ---- Route dispatcher ----

handle_request(Server, request(get, Path, _Headers, _Body, ReqId)) :-
    atom_concat('/api/books', _, Path),
    !,
    handle_get_books(Server, Path, ReqId).

handle_request(Server, request(post, '/api/books', _Headers, Body, ReqId)) :-
    !,
    handle_create_book(Server, Body, ReqId).

handle_request(Server, request(delete, Path, _Headers, _Body, ReqId)) :-
    atom_concat('/api/books', _, Path),
    !,
    handle_delete_book(Server, Path, ReqId).

handle_request(Server, request(_Method, _Path, _Headers, _Body, ReqId)) :-
    http_reply(Server, ReqId, 404, '{"error":"Not Found"}').

% ---- GET /api/books and GET /api/books?id=N ----

handle_get_books(Server, '/api/books', ReqId) :-
    % List all books
    findall(
        Book,
        (book(Id, Title, Author, Year),
         format_book_json(Id, Title, Author, Year, Book)),
        Books
    ),
    join_with_commas(Books, BookList),
    atom_concat('{"books":[', BookList, Tmp),
    atom_concat(Tmp, ']}', Json),
    http_reply_json(Server, ReqId, Json).

handle_get_books(Server, Path, ReqId) :-
    % Extract id from query string: /api/books?id=N
    atom_concat('/api/books?id=', IdAtom, Path),
    atom_number(IdAtom, Id),
    (   book(Id, Title, Author, Year)
    ->  format_book_json(Id, Title, Author, Year, Json),
        http_reply_json(Server, ReqId, Json)
    ;   http_reply(Server, ReqId, 404, '{"error":"Book not found"}')
    ).

% ---- POST /api/books ----
% Body format: "Title,Author,Year"

handle_create_book(Server, Body, ReqId) :-
    split_csv(Body, [Title, Author, YearAtom]),
    atom_number(YearAtom, Year),
    next_book_id(Id),
    retract(next_book_id(Id)),
    NextId is Id + 1,
    assert(next_book_id(NextId)),
    assert(book(Id, Title, Author, Year)),
    format_book_json(Id, Title, Author, Year, Json),
    http_reply_json(Server, ReqId, Json).

handle_create_book(Server, _Body, ReqId) :-
    http_reply(Server, ReqId, 400,
               '{"error":"Bad request. Body format: Title,Author,Year"}').

% ---- DELETE /api/books?id=N ----

handle_delete_book(Server, Path, ReqId) :-
    atom_concat('/api/books?id=', IdAtom, Path),
    atom_number(IdAtom, Id),
    (   retract(book(Id, _, _, _))
    ->  http_reply_json(Server, ReqId, '{"deleted":true}')
    ;   http_reply(Server, ReqId, 404, '{"error":"Book not found"}')
    ).

% ---- Helpers ----

format_book_json(Id, Title, Author, Year, Json) :-
    number_codes(Id, IdCodes), atom_codes(IdAtom, IdCodes),
    number_codes(Year, YearCodes), atom_codes(YearAtom, YearCodes),
    atom_concat('{"id":', IdAtom, S1),
    atom_concat(S1, ',"title":"', S2),
    atom_concat(S2, Title, S3),
    atom_concat(S3, '","author":"', S4),
    atom_concat(S4, Author, S5),
    atom_concat(S5, '","year":', S6),
    atom_concat(S6, YearAtom, S7),
    atom_concat(S7, '}', Json).

% Split an atom on commas into a list of atoms
split_csv(Atom, Parts) :-
    atom_codes(Atom, Codes),
    split_codes(Codes, 0',, [], Parts).

split_codes([], _Sep, Acc, [Part]) :-
    reverse(Acc, RevCodes),
    atom_codes(Part, RevCodes).
split_codes([Sep|Rest], Sep, Acc, [Part|Parts]) :-
    reverse(Acc, RevCodes),
    atom_codes(Part, RevCodes),
    split_codes(Rest, Sep, [], Parts).
split_codes([C|Rest], Sep, Acc, Parts) :-
    C \= Sep,
    split_codes(Rest, Sep, [C|Acc], Parts).

join_with_commas([], '').
join_with_commas([X], X).
join_with_commas([X|Xs], Result) :-
    join_with_commas(Xs, Rest),
    atom_concat(X, ',', Tmp),
    atom_concat(Tmp, Rest, Result).

% Usage:
% ?- start_book_server(8080).
%
% Then from a terminal:
%   curl http://localhost:8080/api/books
%   curl http://localhost:8080/api/books?id=1
%   curl -X POST -d "Godel Escher Bach,Hofstadter,1979" http://localhost:8080/api/books
%   curl -X DELETE http://localhost:8080/api/books?id=2
```

---

### Example 2: Webhook Receiver -- Accept POST Notifications and Store Data

This example creates a webhook endpoint that receives JSON-like notification payloads, parses them, stores them as Prolog facts, and provides a summary endpoint.

```prolog
% ============================================================
% Webhook Receiver: Notification Processing System
% Listens for POST requests on /webhook, stores events,
% serves event summary on GET /events
% ============================================================

:- dynamic event/4.      % event(Timestamp, Source, Type, Payload)
:- dynamic event_count/1.
event_count(0).

start_webhook_receiver(Port) :-
    http_server(Port, Server),
    http_handler(Server, '/webhook', webhook_handler),
    http_handler(Server, '/events', events_handler),
    http_handler(Server, '/health', health_handler),
    write('Webhook receiver started on port '), write(Port), nl,
    write('  POST /webhook  - receive notifications'), nl,
    write('  GET  /events   - list stored events'), nl,
    write('  GET  /health   - health check'), nl,
    webhook_loop(Server).

webhook_loop(Server) :-
    http_get_request(Server, Request),
    !,
    process_webhook(Server, Request),
    webhook_loop(Server).
webhook_loop(Server) :-
    webhook_loop(Server).  % Retry after timeout

% ---- POST /webhook ----
% Expected body format: "source:type:payload"
% Example: "github:push:main-branch-updated"

process_webhook(Server, request(post, '/webhook', Headers, Body, ReqId)) :-
    !,
    get_timestamp(Timestamp),
    parse_notification(Body, Source, Type, Payload),
    assert(event(Timestamp, Source, Type, Payload)),
    increment_event_count,
    event_count(Count),
    number_codes(Count, CountCodes),
    atom_codes(CountAtom, CountCodes),
    atom_concat('{"accepted":true,"event_number":', CountAtom, Tmp),
    atom_concat(Tmp, '}', ResponseJson),
    http_reply_json(Server, ReqId, ResponseJson),
    write('Event received: '), write(Source), write('/'), write(Type), nl.

% ---- GET /events ----

process_webhook(Server, request(get, '/events', _H, _B, ReqId)) :-
    !,
    findall(
        EventJson,
        (event(Ts, Src, Type, Pay),
         format_event_json(Ts, Src, Type, Pay, EventJson)),
        EventJsons
    ),
    join_list_json(EventJsons, ArrayContent),
    atom_concat('{"events":[', ArrayContent, Tmp),
    atom_concat(Tmp, ']}', Json),
    http_reply_json(Server, ReqId, Json).

% ---- GET /health ----

process_webhook(Server, request(get, '/health', _H, _B, ReqId)) :-
    !,
    event_count(Count),
    number_codes(Count, CC), atom_codes(CA, CC),
    atom_concat('{"status":"healthy","events_processed":', CA, T1),
    atom_concat(T1, '}', Json),
    http_reply_json(Server, ReqId, Json).

% ---- Catch-all ----

process_webhook(Server, request(_Method, _Path, _H, _B, ReqId)) :-
    http_reply(Server, ReqId, 404, '{"error":"Unknown endpoint"}').

% ---- Helpers ----

parse_notification(Body, Source, Type, Payload) :-
    atom_codes(Body, Codes),
    split_on_colon(Codes, [SourceCodes, TypeCodes, PayloadCodes]),
    atom_codes(Source, SourceCodes),
    atom_codes(Type, TypeCodes),
    atom_codes(Payload, PayloadCodes).
parse_notification(Body, unknown, raw, Body).
    % Fallback: store unparseable bodies as raw events

split_on_colon(Codes, Parts) :-
    split_codes_on(Codes, 0':, [], Parts).

split_codes_on([], _Sep, Acc, [RevAcc]) :-
    reverse(Acc, RevAcc).
split_codes_on([Sep|Rest], Sep, Acc, [RevAcc|More]) :-
    reverse(Acc, RevAcc),
    split_codes_on(Rest, Sep, [], More).
split_codes_on([C|Rest], Sep, Acc, Parts) :-
    C \= Sep,
    split_codes_on(Rest, Sep, [C|Acc], Parts).

get_timestamp(Timestamp) :-
    % Use event count as a simple monotonic timestamp
    event_count(N),
    number_codes(N, NC), atom_codes(NA, NC),
    atom_concat('t_', NA, Timestamp).

increment_event_count :-
    retract(event_count(N)),
    N1 is N + 1,
    assert(event_count(N1)).

format_event_json(Ts, Src, Type, Pay, Json) :-
    atom_concat('{"timestamp":"', Ts, S1),
    atom_concat(S1, '","source":"', S2),
    atom_concat(S2, Src, S3),
    atom_concat(S3, '","type":"', S4),
    atom_concat(S4, Type, S5),
    atom_concat(S5, '","payload":"', S6),
    atom_concat(S6, Pay, S7),
    atom_concat(S7, '"}', Json).

join_list_json([], '').
join_list_json([X], X).
join_list_json([X|Xs], Result) :-
    join_list_json(Xs, Rest),
    atom_concat(X, ',', Tmp),
    atom_concat(Tmp, Rest, Result).

% Usage:
% ?- start_webhook_receiver(9090).
%
% Send events:
%   curl -X POST -d "github:push:main-updated" http://localhost:9090/webhook
%   curl -X POST -d "jira:ticket:BUG-1234-assigned" http://localhost:9090/webhook
%
% Query stored events:
%   curl http://localhost:9090/events
%   curl http://localhost:9090/health
```

---

### Example 3: API Client -- Consume a REST API and Build a Local Database

This program fetches data from an external REST API, parses the JSON-like responses, and asserts the data as Prolog facts for subsequent querying.

```prolog
% ============================================================
% API Client: Fetch Remote Data and Build Local Knowledge Base
% Demonstrates http_client_get/2, response parsing, and
% converting external data into Prolog facts for reasoning.
% ============================================================

:- dynamic remote_user/4.    % remote_user(Id, Name, Email, City)
:- dynamic fetch_status/2.   % fetch_status(Endpoint, StatusCode)

% ---- Fetch a single endpoint and record status ----

fetch_endpoint(Url, Body) :-
    write('Fetching: '), write(Url), nl,
    http_client_get(Url, response(Status, _Headers, Body)),
    assert(fetch_status(Url, Status)),
    write('  Status: '), write(Status), nl.

% ---- Fetch user data from a REST API ----
% This example uses a hypothetical API at localhost; adapt the URL
% to point to any JSON API returning user records.

fetch_users(BaseUrl) :-
    atom_concat(BaseUrl, '/api/users', Url),
    fetch_endpoint(Url, Body),
    parse_users(Body),
    count_users(Count),
    write('Imported '), write(Count), write(' users.'), nl.

% ---- Simple parser for JSON user array ----
% Expected format per user object in the response:
%   "id":N,"name":"...","email":"...","city":"..."
% This is a simplified extractor, not a full JSON parser.

parse_users(Body) :-
    atom_codes(Body, Codes),
    extract_user_objects(Codes).

extract_user_objects(Codes) :-
    find_next_user_block(Codes, IdStr, NameStr, EmailStr, CityStr, Rest),
    !,
    atom_codes(Name, NameStr),
    atom_codes(Email, EmailStr),
    atom_codes(City, CityStr),
    number_codes(Id, IdStr),
    assert(remote_user(Id, Name, Email, City)),
    extract_user_objects(Rest).
extract_user_objects(_).  % No more user blocks

find_next_user_block(Codes, Id, Name, Email, City, Rest) :-
    % Scan forward to find "id": pattern
    append(_, [0'",0'i,0'd,0'",0':|IdRest], Codes),
    extract_value(IdRest, Id, AfterIdRaw),
    find_field(AfterIdRaw, "name", Name, AfterName),
    find_field(AfterName, "email", Email, AfterEmail),
    find_field(AfterEmail, "city", City, Rest).

extract_value([0' |Rest], Val, After) :- extract_value(Rest, Val, After).
extract_value([0'"|Rest], Val, After) :-
    !, extract_until_quote(Rest, Val, After).
extract_value(Codes, Val, After) :-
    extract_until_delim(Codes, Val, After).

extract_until_quote([], [], []).
extract_until_quote([0'"|Rest], [], Rest).
extract_until_quote([C|Rest], [C|Val], After) :-
    extract_until_quote(Rest, Val, After).

extract_until_delim([], [], []).
extract_until_delim([C|Rest], [], [C|Rest]) :-
    member(C, [0',, 0'}, 0']]), !.
extract_until_delim([C|Rest], [C|Val], After) :-
    extract_until_delim(Rest, Val, After).

find_field(Codes, FieldName, Value, Rest) :-
    atom_codes(FieldAtom, FieldName),
    atom_codes(FieldAtom, FN),
    append([0'"], FN, FNQuoted1),
    append(FNQuoted1, [0'",0':], Pattern),
    append(_, Pattern, Codes, Tail),
    extract_value(Tail, Value, Rest).

% ---- Query the imported data ----

count_users(Count) :-
    findall(Id, remote_user(Id, _, _, _), Ids),
    length(Ids, Count).

users_in_city(City, Users) :-
    findall(Name, remote_user(_, Name, _, City), Users).

% ---- Multi-endpoint fetcher with error reporting ----

fetch_multiple_endpoints(BaseUrl, Endpoints) :-
    fetch_all(BaseUrl, Endpoints),
    report_fetch_summary.

fetch_all(_, []).
fetch_all(BaseUrl, [Ep|Rest]) :-
    atom_concat(BaseUrl, Ep, Url),
    (   fetch_endpoint(Url, _)
    ->  true
    ;   write('  FAILED: '), write(Url), nl
    ),
    fetch_all(BaseUrl, Rest).

report_fetch_summary :-
    findall(S, fetch_status(_, S), Statuses),
    length(Statuses, Total),
    include(is_success_status, Statuses, OkStatuses),
    length(OkStatuses, OkCount),
    FailCount is Total - OkCount,
    write('Fetch summary: '), write(OkCount), write(' succeeded, '),
    write(FailCount), write(' failed out of '),
    write(Total), write(' requests.'), nl.

is_success_status(S) :- S >= 200, S < 300.

% Usage:
% ?- fetch_users('http://localhost:3000').
% Fetching: http://localhost:3000/api/users
%   Status: 200
% Imported 5 users.
%
% ?- users_in_city('New York', Users).
% Users = ['Alice Smith', 'Bob Jones'].
%
% ?- fetch_multiple_endpoints('http://localhost:3000',
%        ['/api/users', '/api/products', '/api/orders']).
```

---

### Example 4: Microservice Health Check Endpoint

A lightweight server that serves system status information on `/health`, suitable for integration with container orchestrators or monitoring systems.

```prolog
% ============================================================
% Health Check Microservice
% Serves /health with system status, uptime, and diagnostics.
% Designed for use with load balancers and monitoring tools.
% ============================================================

:- dynamic health_start_time/1.
:- dynamic health_check_count/1.
:- dynamic service_dependency/3.  % service_dependency(Name, Url, Status)

health_check_count(0).

% ---- Register known service dependencies ----

register_dependency(Name, Url) :-
    assert(service_dependency(Name, Url, unknown)).

% ---- Start the health server ----

start_health_server(Port) :-
    retractall(health_start_time(_)),
    assert(health_start_time(0)),
    http_server(Port, Server),
    http_handler(Server, '/health', health_handler),
    http_handler(Server, '/health/live', liveness_handler),
    http_handler(Server, '/health/ready', readiness_handler),
    http_handler(Server, '/health/dependencies', deps_handler),
    write('Health server running on port '), write(Port), nl,
    health_loop(Server).

health_loop(Server) :-
    http_get_request(Server, Request),
    !,
    dispatch_health(Server, Request),
    health_loop(Server).
health_loop(Server) :-
    health_loop(Server).

% ---- Liveness probe: always returns 200 if the server is running ----

dispatch_health(Server, request(get, '/health/live', _H, _B, ReqId)) :-
    !,
    http_reply_json(Server, ReqId, '{"status":"alive"}').

% ---- Readiness probe: checks all dependencies ----

dispatch_health(Server, request(get, '/health/ready', _H, _B, ReqId)) :-
    !,
    check_all_dependencies(AllOk),
    (   AllOk = true
    ->  http_reply_json(Server, ReqId, '{"ready":true}')
    ;   http_reply(Server, ReqId, 503, '{"ready":false}')
    ).

% ---- Full health status ----

dispatch_health(Server, request(get, '/health', _H, _B, ReqId)) :-
    !,
    increment_health_checks,
    health_check_count(Count),
    number_codes(Count, CC), atom_codes(CA, CC),
    atom_concat('{"status":"healthy","checks_served":', CA, T1),
    atom_concat(T1, '}', Json),
    http_reply_json(Server, ReqId, Json).

% ---- Dependencies detail ----

dispatch_health(Server, request(get, '/health/dependencies', _H, _B, ReqId)) :-
    !,
    findall(DepJson,
        (service_dependency(Name, Url, _),
         check_single_dependency(Name, Url, DepJson)),
        DepJsons),
    join_json_array(DepJsons, ArrayStr),
    atom_concat('{"dependencies":[', ArrayStr, T1),
    atom_concat(T1, ']}', Json),
    http_reply_json(Server, ReqId, Json).

dispatch_health(Server, request(_, _, _H, _B, ReqId)) :-
    http_reply(Server, ReqId, 404, '{"error":"Unknown health endpoint"}').

% ---- Dependency checking ----

check_all_dependencies(AllOk) :-
    findall(Ok,
        (service_dependency(_, Url, _),
         (   http_client_get(Url, response(Status, _, _)),
             Status >= 200, Status < 300
         ->  Ok = true
         ;   Ok = false
         )),
        Results),
    (   member(false, Results)
    ->  AllOk = false
    ;   AllOk = true
    ).

check_single_dependency(Name, Url, Json) :-
    (   http_client_get(Url, response(Status, _, _)),
        Status >= 200, Status < 300
    ->  StatusStr = 'ok',
        retractall(service_dependency(Name, Url, _)),
        assert(service_dependency(Name, Url, ok))
    ;   StatusStr = 'down',
        retractall(service_dependency(Name, Url, _)),
        assert(service_dependency(Name, Url, down))
    ),
    atom_concat('{"name":"', Name, S1),
    atom_concat(S1, '","url":"', S2),
    atom_concat(S2, Url, S3),
    atom_concat(S3, '","status":"', S4),
    atom_concat(S4, StatusStr, S5),
    atom_concat(S5, '"}', Json).

increment_health_checks :-
    retract(health_check_count(N)),
    N1 is N + 1,
    assert(health_check_count(N1)).

join_json_array([], '').
join_json_array([X], X).
join_json_array([X|Xs], Result) :-
    join_json_array(Xs, Rest),
    atom_concat(X, ',', Tmp),
    atom_concat(Tmp, Rest, Result).

% Usage:
% ?- register_dependency(database, 'http://localhost:5432/health').
% ?- register_dependency(cache, 'http://localhost:6379/ping').
% ?- start_health_server(8081).
%
% Kubernetes-style probes:
%   curl http://localhost:8081/health/live    -> {"status":"alive"}
%   curl http://localhost:8081/health/ready   -> {"ready":true} or 503
%   curl http://localhost:8081/health         -> {"status":"healthy","checks_served":1}
%   curl http://localhost:8081/health/dependencies -> detailed status
```

---

### Example 5: URL Shortener Service

A complete URL shortener that accepts long URLs, generates short codes, stores the mappings, and redirects visitors.

```prolog
% ============================================================
% URL Shortener Service
% POST /shorten  body="https://example.com/long/url"
%   -> returns {"short_code":"abc12","short_url":"http://localhost:8080/abc12"}
% GET  /abc12    -> 301 redirect to the original URL
% GET  /stats    -> usage statistics
% ============================================================

:- dynamic url_mapping/3.   % url_mapping(ShortCode, LongUrl, ClickCount)
:- dynamic code_counter/1.
code_counter(1000).

start_shortener(Port) :-
    http_server(Port, Server),
    http_handler(Server, '/shorten', shorten_handler),
    http_handler(Server, '/stats', stats_handler),
    write('URL Shortener running on port '), write(Port), nl,
    shortener_loop(Server, Port).

shortener_loop(Server, Port) :-
    http_get_request(Server, Request),
    !,
    handle_shortener(Server, Port, Request),
    shortener_loop(Server, Port).
shortener_loop(Server, Port) :-
    shortener_loop(Server, Port).

% ---- POST /shorten ----

handle_shortener(Server, Port, request(post, '/shorten', _H, LongUrl, ReqId)) :-
    LongUrl \= '',
    !,
    % Check if URL already shortened
    (   url_mapping(ExistingCode, LongUrl, _)
    ->  Code = ExistingCode
    ;   generate_short_code(Code),
        assert(url_mapping(Code, LongUrl, 0))
    ),
    number_codes(Port, PC), atom_codes(PA, PC),
    atom_concat('http://localhost:', PA, BaseUrl),
    atom_concat(BaseUrl, '/', BaseSlash),
    atom_concat(BaseSlash, Code, ShortUrl),
    atom_concat('{"short_code":"', Code, S1),
    atom_concat(S1, '","short_url":"', S2),
    atom_concat(S2, ShortUrl, S3),
    atom_concat(S3, '","original":"', S4),
    url_encode(LongUrl, EncodedUrl),
    atom_concat(S4, EncodedUrl, S5),
    atom_concat(S5, '"}', Json),
    http_reply_json(Server, ReqId, Json).

% ---- GET /stats ----

handle_shortener(Server, _Port, request(get, '/stats', _H, _B, ReqId)) :-
    !,
    findall(
        EntryJson,
        (url_mapping(Code, Url, Clicks),
         format_stats_entry(Code, Url, Clicks, EntryJson)),
        Entries
    ),
    length(Entries, Total),
    number_codes(Total, TC), atom_codes(TA, TC),
    join_json_array(Entries, EntriesStr),
    atom_concat('{"total_urls":', TA, S1),
    atom_concat(S1, ',"urls":[', S2),
    atom_concat(S2, EntriesStr, S3),
    atom_concat(S3, ']}', Json),
    http_reply_json(Server, ReqId, Json).

% ---- GET /<code> -- redirect ----

handle_shortener(Server, _Port, request(get, Path, _H, _B, ReqId)) :-
    atom_concat('/', Code, Path),
    Code \= '',
    Code \= 'shorten',
    Code \= 'stats',
    url_mapping(Code, LongUrl, Clicks),
    !,
    % Increment click count
    NewClicks is Clicks + 1,
    retract(url_mapping(Code, LongUrl, Clicks)),
    assert(url_mapping(Code, LongUrl, NewClicks)),
    % Send a redirect response with Location header
    % (Since http_reply/4 is plain text, we send an HTML redirect)
    atom_concat('<html><head><meta http-equiv="refresh" content="0;url=',
                LongUrl, R1),
    atom_concat(R1, '"></head><body>Redirecting to ', R2),
    atom_concat(R2, LongUrl, R3),
    atom_concat(R3, '</body></html>', RedirectBody),
    http_reply(Server, ReqId, 301, RedirectBody).

handle_shortener(Server, _Port, request(_, _, _H, _B, ReqId)) :-
    http_reply(Server, ReqId, 404, '{"error":"Not found"}').

% ---- Short code generation ----
% Generates codes like "a1b2c" using a base-36 encoding of a counter.

generate_short_code(Code) :-
    retract(code_counter(N)),
    N1 is N + 1,
    assert(code_counter(N1)),
    encode_base36(N, Code).

encode_base36(N, Code) :-
    base36_digits(N, [], Digits),
    atom_codes(Code, Digits).

base36_digits(0, Acc, Acc) :- Acc \= [].
base36_digits(N, Acc, Digits) :-
    N > 0,
    Rem is N mod 36,
    base36_char(Rem, C),
    N1 is N // 36,
    base36_digits(N1, [C|Acc], Digits).

base36_char(D, C) :- D < 10, !, C is D + 0'0.
base36_char(D, C) :- C is D - 10 + 0'a.

format_stats_entry(Code, Url, Clicks, Json) :-
    number_codes(Clicks, CC), atom_codes(CA, CC),
    atom_concat('{"code":"', Code, S1),
    atom_concat(S1, '","url":"', S2),
    url_encode(Url, SafeUrl),
    atom_concat(S2, SafeUrl, S3),
    atom_concat(S3, '","clicks":', S4),
    atom_concat(S4, CA, S5),
    atom_concat(S5, '}', Json).

join_json_array([], '').
join_json_array([X], X).
join_json_array([X|Xs], Result) :-
    join_json_array(Xs, Rest),
    atom_concat(X, ',', Tmp),
    atom_concat(Tmp, Rest, Result).

% Usage:
% ?- start_shortener(8080).
%
% Shorten a URL:
%   curl -X POST -d "https://docs.example.com/very/long/path?p=1&q=2" \
%        http://localhost:8080/shorten
%   -> {"short_code":"rs","short_url":"http://localhost:8080/rs",...}
%
% Visit the short URL:
%   curl -L http://localhost:8080/rs
%   -> Redirects to the original URL
%
% View statistics:
%   curl http://localhost:8080/stats
%   -> {"total_urls":1,"urls":[{"code":"rs","url":"...","clicks":1}]}
```
