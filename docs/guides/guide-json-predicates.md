# JSON Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.json` package provides JSON parsing, serialization, querying, and manipulation predicates for JProlog. It bridges the gap between JSON data formats and Prolog's term-based representation, enabling JProlog programs to process API responses, configuration files, and data interchange formats.

**Source file:** `JsonPredicates.java`

### JSON-to-Prolog Term Mapping

| JSON Type     | Prolog Representation                          | Example                              |
|---------------|------------------------------------------------|--------------------------------------|
| Object        | `json([key1=val1, key2=val2, ...])`           | `json([name=alice, age=30])`         |
| Array         | Prolog list `[elem1, elem2, ...]`             | `[1, 2, 3]`                         |
| String        | Atom                                           | `hello`                              |
| Number        | Number                                         | `42`, `3.14`                         |
| `true`        | `@(true)`                                      |                                      |
| `false`       | `@(false)`                                     |                                      |
| `null`        | `@(null)`                                      |                                      |

This mapping is used consistently across all predicates. Nested structures are represented recursively -- a JSON object inside an array becomes a `json(...)` term inside a Prolog list.

---

## Predicate Reference

### json_parse/2

```prolog
json_parse(+JsonString, -Term)
```

Parses a JSON string into its Prolog term representation. Supports the full JSON specification including nested objects, arrays, strings, numbers, booleans, and null.

| Argument   | Type | Mode | Description                            |
|------------|------|------|----------------------------------------|
| JsonString | atom | +    | A valid JSON string                    |
| Term       | term | -    | The resulting Prolog term              |

**Errors:**
- `evaluation_error` if JsonString is not an atom.
- `evaluation_error` if the JSON is malformed (unexpected characters, unterminated strings, etc.).

**Escape handling:** Standard JSON escapes are supported: `\"`, `\\`, `\/`, `\n`, `\t`, `\r`.

---

### json_serialize/2

```prolog
json_serialize(+Term, -JsonString)
```

Serializes a Prolog term back into a JSON string. The term must follow the mapping conventions described above.

| Argument   | Type | Mode | Description                             |
|------------|------|------|------------------------------------------|
| Term       | term | +    | A Prolog term in JSON representation     |
| JsonString | atom | -    | The resulting JSON string                |

**Behavior details:**
- Numbers that are whole values are serialized without decimal points (e.g., `42.0` becomes `42`).
- Atoms are serialized as JSON strings with proper escaping.
- `@(true)`, `@(false)`, `@(null)` serialize to the JSON literals `true`, `false`, `null`.
- The empty list `[]` serializes to `[]`.

---

### json_get/3

```prolog
json_get(+JsonTerm, +Path, -Value)
```

Extracts a value from a parsed JSON term by navigating a path. Supports both simple key lookup and nested path navigation using the `/` operator.

| Argument | Type        | Mode | Description                                   |
|----------|-------------|------|-----------------------------------------------|
| JsonTerm | term        | +    | A parsed JSON term (from json_parse/2)        |
| Path     | atom/number/compound | + | Key (atom), array index (number), or nested path (Key1/Key2) |
| Value    | term        | -    | The extracted value                           |

**Path syntax:**
- `key` -- looks up a key in a JSON object
- `N` (integer) -- indexes into a JSON array (0-based)
- `key1/key2/key3` -- navigates nested structures (left-associative `/` operator)
- `key/0` -- access first element of an array nested under a key

**Fails** (without error) if the path does not exist.

---

### json_set/4

```prolog
json_set(+JsonTerm, +Key, +Value, -NewJsonTerm)
```

Sets or updates a key-value pair in a JSON object term. If the key already exists, its value is replaced. If the key does not exist, the pair is appended.

| Argument    | Type | Mode | Description                                |
|-------------|------|------|--------------------------------------------|
| JsonTerm    | term | +    | A `json([...])` object term                |
| Key         | atom | +    | The key to set                             |
| Value       | term | +    | The new value                              |
| NewJsonTerm | term | -    | The updated `json([...])` object           |

**Errors:**
- `evaluation_error` if Key is not an atom.
- `evaluation_error` if JsonTerm is not a `json(...)` term.

**Note:** This operates on the top level of the object only. For nested updates, extract the nested object with json_get/3, update it, then rebuild the parent with json_set/4.

---

### json_keys/2

```prolog
json_keys(+JsonObject, -Keys)
```

Extracts the list of keys from a JSON object term.

| Argument   | Type | Mode | Description                            |
|------------|------|------|----------------------------------------|
| JsonObject | term | +    | A `json([...])` object term            |
| Keys       | list | -    | A Prolog list of key atoms             |

**Errors:**
- `evaluation_error` if the argument is not a `json(...)` term.

---

### json_member/3

```prolog
json_member(+JsonObject, ?Key, ?Value)
```

Accesses key-value pairs in a JSON object. Can enumerate all pairs on backtracking when Key and/or Value are unbound, or look up a specific key.

| Argument   | Type | Mode | Description                                  |
|------------|------|------|----------------------------------------------|
| JsonObject | term | +    | A `json([...])` object term                  |
| Key        | atom | ?    | A key in the object (can be unbound)         |
| Value      | term | ?    | The corresponding value (can be unbound)     |

**Backtracking:** Produces one solution per key-value pair in the object. This makes it suitable for iterating over all members with `findall/3` or `forall/2`.

**Errors:**
- `evaluation_error` if the first argument is not a `json(...)` term.

---

## Esempi Reali (Real Examples)

### Example 1: REST API Response Processing

Parse a JSON API response, extract nested fields, and transform the data into Prolog facts for further reasoning.

```prolog
% ============================================================
% REST API response processing
% Parses a user-list API response, extracts user records,
% and loads them as Prolog facts for querying.
% ============================================================

:- dynamic api_user/4.  % api_user(Id, Name, Email, IsActive)

% Simulated API response (in practice, this would come from http_get/3).
sample_response('{"status":"ok","users":[{"id":101,"name":"Alice Chen","email":"alice@corp.com","active":true},{"id":102,"name":"Bob Martinez","email":"bob@corp.com","active":true},{"id":103,"name":"Carol Wu","email":"carol@corp.com","active":false}],"total":3}').

% Parse the API response and load users into the knowledge base.
load_users_from_api(ResponseJson) :-
    json_parse(ResponseJson, Term),
    json_get(Term, status, Status),
    (   Status == ok
    ->  json_get(Term, users, UserList),
        load_user_list(UserList),
        json_get(Term, total, Total),
        write('Loaded '), write(Total), write(' users'), nl
    ;   write('API error: status = '), write(Status), nl,
        fail
    ).

% Recursively process the user array.
load_user_list([]).
load_user_list([UserObj | Rest]) :-
    json_get(UserObj, id, Id),
    json_get(UserObj, name, Name),
    json_get(UserObj, email, Email),
    json_get(UserObj, active, ActiveTerm),
    normalize_bool(ActiveTerm, Active),
    assert(api_user(Id, Name, Email, Active)),
    load_user_list(Rest).

% Convert @(true)/@(false) to simple atoms for easier use.
normalize_bool(@(true), true).
normalize_bool(@(false), false).

% Query helpers built on the loaded facts.
active_users(Users) :-
    findall(Name, api_user(_, Name, _, true), Users).

find_user_by_email(Email, Name) :-
    api_user(_, Name, Email, _).

% --- Usage ---
% ?- sample_response(R), load_users_from_api(R).
%    Loaded 3 users
%
% ?- active_users(Users).
%    Users = ['Alice Chen', 'Bob Martinez']
%
% ?- find_user_by_email('carol@corp.com', Name).
%    Name = 'Carol Wu'
%
% ?- api_user(Id, Name, _, false).
%    Id = 103, Name = 'Carol Wu'
```

---

### Example 2: Configuration File Management

Read a JSON configuration structure, modify settings programmatically, and serialize back to JSON.

```prolog
% ============================================================
% JSON configuration management
% Read, modify, validate, and write back application config.
% ============================================================

% Default configuration as JSON.
default_config('{"database":{"host":"localhost","port":5432,"pool_size":10},"logging":{"level":"info","file":"app.log"},"features":{"cache_enabled":true,"max_retries":3}}').

% Load configuration from a JSON string.
load_config(JsonStr, Config) :-
    json_parse(JsonStr, Config),
    write('Configuration loaded successfully'), nl.

% Read a top-level config section.
config_section(Config, Section, SectionData) :-
    json_get(Config, Section, SectionData).

% Read a nested config value using path navigation.
config_value(Config, Section, Key, Value) :-
    json_get(Config, Section/Key, Value).

% Update a top-level section's key and return the new config.
% This extracts the section, updates the key within it,
% then puts the section back into the root config.
update_config(Config, Section, Key, NewValue, UpdatedConfig) :-
    json_get(Config, Section, SectionObj),
    json_set(SectionObj, Key, NewValue, NewSectionObj),
    json_set(Config, Section, NewSectionObj, UpdatedConfig).

% Validate that required keys exist in a config section.
validate_section(Config, Section, RequiredKeys) :-
    json_get(Config, Section, SectionObj),
    json_keys(SectionObj, ActualKeys),
    forall(
        member(K, RequiredKeys),
        (   member(K, ActualKeys)
        ->  true
        ;   write('MISSING required key: '),
            write(Section), write('.'), write(K), nl,
            fail
        )
    ).

% Display all configuration values for a section.
show_section(Config, Section) :-
    json_get(Config, Section, SectionObj),
    write('--- '), write(Section), write(' ---'), nl,
    forall(
        json_member(SectionObj, Key, Value),
        (write('  '), write(Key), write(' = '), write(Value), nl)
    ).

% Export configuration back to JSON.
export_config(Config, JsonStr) :-
    json_serialize(Config, JsonStr).

% --- Usage ---
% ?- default_config(J), load_config(J, C),
%    config_value(C, database, host, Host),
%    config_value(C, database, port, Port).
%    Host = localhost, Port = 5432
%
% ?- default_config(J), load_config(J, C),
%    update_config(C, database, host, 'db.production.internal', C2),
%    update_config(C2, logging, level, debug, C3),
%    export_config(C3, Output).
%    Output = '{"database":{"host":"db.production.internal","port":5432,...},...}'
%
% ?- default_config(J), load_config(J, C),
%    validate_section(C, database, [host, port, pool_size]).
%    true
%
% ?- default_config(J), load_config(J, C), show_section(C, logging).
%    --- logging ---
%      level = info
%      file = app.log
```

---

### Example 3: Data Transformation Pipeline

Parse JSON data from one format, transform it into a different structure, and serialize the result.

```prolog
% ============================================================
% Data transformation pipeline
% Takes a flat list of sales records, groups by region,
% computes totals, and outputs a summary JSON.
% ============================================================

% Input: array of sale objects.
sales_data('[{"region":"north","product":"Widget","amount":1500},{"region":"south","product":"Gadget","amount":2300},{"region":"north","product":"Gadget","amount":800},{"region":"south","product":"Widget","amount":1100},{"region":"north","product":"Doohickey","amount":450}]').

% Parse the sales data and extract each record as a Prolog structure.
parse_sales(JsonStr, Sales) :-
    json_parse(JsonStr, SalesList),
    extract_records(SalesList, Sales).

extract_records([], []).
extract_records([Obj | Rest], [sale(Region, Product, Amount) | Sales]) :-
    json_get(Obj, region, Region),
    json_get(Obj, product, Product),
    json_get(Obj, amount, Amount),
    extract_records(Rest, Sales).

% Compute the total sales amount for a given region.
region_total(Sales, Region, Total) :-
    findall(Amt,
        member(sale(Region, _, Amt), Sales),
        Amounts),
    sum_list(Amounts, Total).

% Get all unique regions from the sales data.
unique_regions(Sales, Regions) :-
    findall(R, member(sale(R, _, _), Sales), AllRegions),
    sort(AllRegions, Regions).

% Count distinct products sold in a region.
region_products(Sales, Region, Products) :-
    findall(P, member(sale(Region, P, _), Sales), AllProducts),
    sort(AllProducts, Products).

% Build a summary JSON object for one region.
% Output: json([region=R, total=T, product_count=N, products=[...]])
region_summary(Sales, Region, Summary) :-
    region_total(Sales, Region, Total),
    region_products(Sales, Region, Products),
    length(Products, ProductCount),
    Summary = json([region=Region, total=Total,
                    product_count=ProductCount, products=Products]).

% Build the complete summary report as JSON.
build_report(JsonStr, ReportJson) :-
    parse_sales(JsonStr, Sales),
    unique_regions(Sales, Regions),
    maplist_region_summary(Sales, Regions, Summaries),
    sum_all(Sales, GrandTotal),
    Report = json([report=summaries, grand_total=GrandTotal,
                   regions=Summaries]),
    json_serialize(Report, ReportJson).

maplist_region_summary(_, [], []).
maplist_region_summary(Sales, [R | Rs], [S | Ss]) :-
    region_summary(Sales, R, S),
    maplist_region_summary(Sales, Rs, Ss).

sum_all(Sales, Total) :-
    findall(A, member(sale(_, _, A), Sales), Amounts),
    sum_list(Amounts, Total).

sum_list([], 0).
sum_list([H | T], Sum) :- sum_list(T, Rest), Sum is H + Rest.

% --- Usage ---
% ?- sales_data(J), build_report(J, Report).
%    Report = '{"report":"summaries","grand_total":6150,
%               "regions":[{"region":"north","total":2750,
%               "product_count":3,"products":["Doohickey","Gadget","Widget"]},
%               {"region":"south","total":3400,"product_count":2,
%               "products":["Gadget","Widget"]}]}'
%
% ?- sales_data(J), parse_sales(J, Sales), region_total(Sales, north, T).
%    T = 2750
```

---

### Example 4: Building a JSON-Based Message Protocol

Define, construct, parse, and validate messages for a simple inter-service communication protocol.

```prolog
% ============================================================
% JSON-based message protocol
% Defines message types, constructs protocol messages,
% parses incoming messages, and dispatches to handlers.
% ============================================================

% Valid message types and their required fields.
message_schema(ping, [sender, timestamp]).
message_schema(pong, [sender, timestamp, reply_to]).
message_schema(data, [sender, timestamp, topic, payload]).
message_schema(error, [sender, timestamp, code, message]).

% Construct a protocol message as a JSON term.
build_message(Type, Fields, MsgJson) :-
    message_schema(Type, _),
    MsgTerm = json([type=Type | Fields]),
    json_serialize(MsgTerm, MsgJson).

% Parse an incoming JSON message string into structured form.
parse_message(JsonStr, Type, Fields) :-
    json_parse(JsonStr, Term),
    json_get(Term, type, Type),
    json_keys(Term, AllKeys),
    exclude_key(type, AllKeys, FieldKeys),
    extract_fields(Term, FieldKeys, Fields).

exclude_key(_, [], []).
exclude_key(Exclude, [K | Ks], Result) :-
    (   K == Exclude
    ->  exclude_key(Exclude, Ks, Result)
    ;   Result = [K | Rest],
        exclude_key(Exclude, Ks, Rest)
    ).

extract_fields(_, [], []).
extract_fields(Term, [K | Ks], [K=V | Fs]) :-
    json_get(Term, K, V),
    extract_fields(Term, Ks, Fs).

% Validate a message against its schema (all required fields present).
validate_message(Term, Type) :-
    json_get(Term, type, Type),
    message_schema(Type, RequiredFields),
    forall(
        member(Field, RequiredFields),
        (   json_get(Term, Field, _)
        ->  true
        ;   write('Validation error: missing field '), write(Field),
            write(' in '), write(Type), write(' message'), nl,
            fail
        )
    ).

% Dispatch a parsed message to the appropriate handler.
dispatch_message(JsonStr) :-
    json_parse(JsonStr, Term),
    json_get(Term, type, Type),
    (   validate_message(Term, Type)
    ->  handle_message(Type, Term)
    ;   write('Message rejected: validation failed'), nl
    ).

handle_message(ping, Term) :-
    json_get(Term, sender, Sender),
    write('PING received from '), write(Sender), nl,
    write('  -> Sending PONG reply'), nl.

handle_message(data, Term) :-
    json_get(Term, sender, Sender),
    json_get(Term, topic, Topic),
    json_get(Term, payload, Payload),
    write('DATA received from '), write(Sender), nl,
    write('  Topic:   '), write(Topic), nl,
    write('  Payload: '), write(Payload), nl.

handle_message(error, Term) :-
    json_get(Term, code, Code),
    json_get(Term, message, Msg),
    write('ERROR ['), write(Code), write(']: '), write(Msg), nl.

% --- Usage ---
% ?- build_message(data,
%        [sender=service_a, timestamp='2025-03-21T10:00:00',
%         topic=temperature, payload=json([value=22.5, unit=celsius])],
%        Json).
%    Json = '{"type":"data","sender":"service_a",...}'
%
% ?- dispatch_message('{"type":"ping","sender":"monitor","timestamp":"2025-03-21T10:00:00"}').
%    PING received from monitor
%      -> Sending PONG reply
%
% ?- dispatch_message('{"type":"error","sender":"db","timestamp":"now","code":503,"message":"connection refused"}').
%    ERROR [503]: connection refused
```

---

### Example 5: Validating JSON Data Against Expected Schema Structure

Check that a JSON document conforms to a structural schema defined in Prolog.

```prolog
% ============================================================
% JSON schema validation
% Define expected structure as Prolog terms and validate
% incoming JSON documents against it.
% ============================================================

% Schema definitions.
% A schema is one of:
%   string       - expects an atom
%   number       - expects a number
%   boolean      - expects @(true) or @(false)
%   nullable(S)  - allows @(null) or a value matching schema S
%   array(S)     - expects a list, each element matching schema S
%   object(KVs)  - expects a json(...), KVs is [key:Schema, ...]

% Define the schema for a user profile API response.
user_profile_schema(
    object([
        id : number,
        username : string,
        email : string,
        verified : boolean,
        bio : nullable(string),
        roles : array(string),
        address : object([
            city : string,
            country : string,
            zip : nullable(string)
        ])
    ])
).

% Main validation entry point.
validate_json(JsonStr, Schema) :-
    json_parse(JsonStr, Term),
    validate_term(Term, Schema, root).

% Validate a term against a schema, tracking the path for errors.
validate_term(Term, string, Path) :-
    (   Term = @(_) -> fail ; true ),   % exclude booleans/null
    (   atom(Term)
    ->  true
    ;   write('Schema error at '), write(Path),
        write(': expected string, got '), write(Term), nl, fail
    ).

validate_term(Term, number, Path) :-
    (   number(Term)
    ->  true
    ;   write('Schema error at '), write(Path),
        write(': expected number, got '), write(Term), nl, fail
    ).

validate_term(@(true), boolean, _).
validate_term(@(false), boolean, _).
validate_term(Term, boolean, Path) :-
    Term \= @(true), Term \= @(false),
    write('Schema error at '), write(Path),
    write(': expected boolean, got '), write(Term), nl, fail.

validate_term(@(null), nullable(_), _).
validate_term(Term, nullable(Inner), Path) :-
    Term \= @(null),
    validate_term(Term, Inner, Path).

validate_term(List, array(ElemSchema), Path) :-
    is_list(List),
    validate_array(List, ElemSchema, Path, 0).
validate_term(Term, array(_), Path) :-
    \+ is_list(Term),
    write('Schema error at '), write(Path),
    write(': expected array, got '), write(Term), nl, fail.

validate_term(Term, object(Fields), Path) :-
    Term = json(_),
    validate_object_fields(Term, Fields, Path).
validate_term(Term, object(_), Path) :-
    Term \= json(_),
    write('Schema error at '), write(Path),
    write(': expected object'), nl, fail.

% Validate each element of an array.
validate_array([], _, _, _).
validate_array([H | T], Schema, Path, Idx) :-
    validate_term(H, Schema, Path/Idx),
    Idx1 is Idx + 1,
    validate_array(T, Schema, Path, Idx1).

% Validate all required fields in an object.
validate_object_fields(_, [], _).
validate_object_fields(Obj, [Key:Schema | Rest], Path) :-
    (   json_get(Obj, Key, Value)
    ->  validate_term(Value, Schema, Path/Key),
        validate_object_fields(Obj, Rest, Path)
    ;   write('Schema error at '), write(Path),
        write(': missing required field '), write(Key), nl,
        fail
    ).

% --- Usage ---
% Valid document:
% ?- validate_json(
%      '{"id":42,"username":"alice","email":"alice@example.com",
%        "verified":true,"bio":null,"roles":["admin","editor"],
%        "address":{"city":"Portland","country":"US","zip":"97201"}}',
%      user_profile_schema(S)), validate_json(..., S).
%    true
%
% Using the predicate directly:
% ?- user_profile_schema(S),
%    validate_json(
%      '{"id":42,"username":"alice","email":"alice@example.com","verified":true,"bio":null,"roles":["admin","editor"],"address":{"city":"Portland","country":"US","zip":"97201"}}',
%      S).
%    true
%
% Invalid document (missing email, wrong type for id):
% ?- user_profile_schema(S),
%    validate_json(
%      '{"id":"not-a-number","username":"bob","verified":false,"bio":"Hi","roles":[],"address":{"city":"NYC","country":"US","zip":null}}',
%      S).
%    Schema error at root/id: expected number, got not-a-number
%    Schema error at root: missing required field email
%    false
```
