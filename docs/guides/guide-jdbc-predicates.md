# JProlog JDBC Predicates — User Guide

This guide covers all JDBC built-in predicates for database connectivity, SQL execution, transactions, prepared statements, stored procedures, and LOB handling.

## 1. Connection Management

### Loading a JDBC Driver

```prolog
% Load the driver class (required for some JDBC drivers)
?- jdbc_driver_load('org.h2.Driver').
?- jdbc_driver_load('com.mysql.cj.jdbc.Driver').
?- jdbc_driver_load('org.postgresql.Driver').
```

### Opening a Connection

```prolog
% Without credentials (e.g., H2 in-memory, SQLite)
?- jdbc_connect('jdbc:h2:mem:testdb', Conn).
% Conn = '$jdbc_conn_1'

% With credentials
?- jdbc_connect('jdbc:mysql://localhost:3306/mydb', 'root', 'password', Conn).
% Conn = '$jdbc_conn_2'

% PostgreSQL example
?- jdbc_connect('jdbc:postgresql://localhost/mydb', 'user', 'pass', Conn).
```

### Closing a Connection

```prolog
?- jdbc_disconnect(Conn).
```

## 2. Simple SQL Execution (Without Parameters)

### SELECT — Reading Data

```prolog
% jdbc_query/3 returns all rows as a list of row(...) terms
?- jdbc_query(Conn, 'SELECT id, name, age FROM users', Rows).
% Rows = [row(1, 'Alice', 30), row(2, 'Bob', 25), row(3, 'Charlie', 35)]

% Process rows in Prolog
print_users(Conn) :-
    jdbc_query(Conn, 'SELECT name, age FROM users', Rows),
    member(row(Name, Age), Rows),
    write(Name), write(' is '), write(Age), write(' years old'), nl,
    fail ; true.

% SELECT with WHERE (build SQL as atom)
find_adults(Conn, Adults) :-
    jdbc_query(Conn, 'SELECT name FROM users WHERE age >= 18', Rows),
    findall(Name, member(row(Name), Rows), Adults).
```

### INSERT, UPDATE, DELETE

```prolog
% jdbc_execute_update/3 returns the number of affected rows
?- jdbc_execute_update(Conn, 'INSERT INTO users(name, age) VALUES (''Diana'', 28)', N).
% N = 1

?- jdbc_execute_update(Conn, 'UPDATE users SET age = 31 WHERE name = ''Alice''', N).
% N = 1

?- jdbc_execute_update(Conn, 'DELETE FROM users WHERE age < 20', N).
% N = 0

% DDL: CREATE TABLE
?- jdbc_execute_update(Conn,
    'CREATE TABLE products(id INT PRIMARY KEY, name VARCHAR(100), price DECIMAL(10,2))', _).
```

## 3. Prepared Statements (With Parameters)

Prepared statements prevent SQL injection and support typed parameters.

### Prepare, Set Parameters, Execute

```prolog
% Step 1: Prepare the statement
?- jdbc_prepare(Conn, 'SELECT * FROM users WHERE age > ? AND city = ?', Stmt).

% Step 2: Set parameters individually
?- jdbc_set_param(Stmt, 1, 25).       % param 1 = integer 25
?- jdbc_set_param(Stmt, 2, 'Milan').   % param 2 = string 'Milan'

% Step 3: Execute the prepared query
?- jdbc_execute_prepared_query(Stmt, Rows).
% Rows = [row(1, 'Alice', 30, 'Milan'), ...]

% Step 4: Close the statement
?- jdbc_close_statement(Stmt).
```

### Batch Parameter Setting

```prolog
% Set all parameters at once with a list
?- jdbc_prepare(Conn, 'INSERT INTO users(name, age, city) VALUES (?, ?, ?)', Stmt),
   jdbc_set_params(Stmt, ['Eve', 22, 'Rome']),
   jdbc_execute_prepared_update(Stmt, N),
   jdbc_close_statement(Stmt).
% N = 1
```

### Prepared INSERT/UPDATE/DELETE

```prolog
% Prepared INSERT
insert_user(Conn, Name, Age, City) :-
    jdbc_prepare(Conn, 'INSERT INTO users(name, age, city) VALUES (?, ?, ?)', Stmt),
    jdbc_set_params(Stmt, [Name, Age, City]),
    jdbc_execute_prepared_update(Stmt, _),
    jdbc_close_statement(Stmt).

% Prepared UPDATE
update_age(Conn, Name, NewAge) :-
    jdbc_prepare(Conn, 'UPDATE users SET age = ? WHERE name = ?', Stmt),
    jdbc_set_params(Stmt, [NewAge, Name]),
    jdbc_execute_prepared_update(Stmt, N),
    jdbc_close_statement(Stmt),
    (N > 0 -> write('Updated') ; write('Not found')), nl.

% Prepared DELETE
delete_user(Conn, Name) :-
    jdbc_prepare(Conn, 'DELETE FROM users WHERE name = ?', Stmt),
    jdbc_set_param(Stmt, 1, Name),
    jdbc_execute_prepared_update(Stmt, N),
    jdbc_close_statement(Stmt),
    write(N), write(' row(s) deleted'), nl.
```

### NULL values

```prolog
% Use the atom 'null' to set NULL
?- jdbc_set_param(Stmt, 3, null).
```

## 4. Transaction Management

```prolog
% Disable autocommit to start a transaction
?- jdbc_set_autocommit(Conn, false).

% Execute multiple operations
?- jdbc_execute_update(Conn, 'INSERT INTO accounts(id, balance) VALUES (1, 1000)', _).
?- jdbc_execute_update(Conn, 'INSERT INTO accounts(id, balance) VALUES (2, 2000)', _).

% Commit the transaction
?- jdbc_commit(Conn).

% Or rollback on error
transfer(Conn, From, To, Amount) :-
    jdbc_set_autocommit(Conn, false),
    (
        jdbc_prepare(Conn, 'UPDATE accounts SET balance = balance - ? WHERE id = ?', S1),
        jdbc_set_params(S1, [Amount, From]),
        jdbc_execute_prepared_update(S1, _),
        jdbc_close_statement(S1),
        jdbc_prepare(Conn, 'UPDATE accounts SET balance = balance + ? WHERE id = ?', S2),
        jdbc_set_params(S2, [Amount, To]),
        jdbc_execute_prepared_update(S2, _),
        jdbc_close_statement(S2),
        jdbc_commit(Conn)
    ;
        jdbc_rollback(Conn),
        write('Transaction rolled back'), nl, fail
    ),
    jdbc_set_autocommit(Conn, true).
```

## 5. Database Metadata

```prolog
% List all tables
?- jdbc_tables(Conn, Tables).
% Tables = ['USERS', 'PRODUCTS', 'ORDERS']

% List columns of a table
?- jdbc_columns(Conn, 'USERS', Columns).
% Columns = [column('ID', 'INTEGER', 10), column('NAME', 'VARCHAR', 100), column('AGE', 'INTEGER', 10)]
```

## 6. Stored Procedures

### Simple Procedure (no OUT parameters)

```prolog
% Prepare the callable statement
?- jdbc_prepare_call(Conn, '{call update_statistics(?)}', CallStmt).

% Set IN parameter
?- jdbc_call_set_param(CallStmt, 1, 'users').

% Execute
?- jdbc_call_execute(CallStmt).

% Close
?- jdbc_close_statement(CallStmt).
```

### Procedure with OUT Parameters

```prolog
% Procedure: get_user_count(IN table_name, OUT count)
call_get_count(Conn, TableName, Count) :-
    jdbc_prepare_call(Conn, '{call get_user_count(?, ?)}', CS),
    jdbc_call_set_param(CS, 1, TableName),
    jdbc_call_register_out(CS, 2, integer),    % Register OUT param as integer
    jdbc_call_execute(CS),
    jdbc_call_get_result(CS, 2, Count),        % Retrieve the OUT value
    jdbc_close_statement(CS).
% ?- call_get_count(Conn, 'users', N).
% N = 42
```

### Supported OUT Parameter Types

`integer`, `bigint`, `double`, `decimal`, `varchar`, `boolean`, `date`, `timestamp`

### Procedure Returning a ResultSet

```prolog
get_active_users(Conn, Users) :-
    jdbc_prepare_call(Conn, '{call get_active_users()}', CS),
    jdbc_call_execute(CS),
    jdbc_call_get_resultset(CS, Users),    % Returns list of row(...) terms
    jdbc_close_statement(CS).
```

## 7. CLOB and BLOB Handling

### Writing CLOBs (Large Text)

```prolog
% With a prepared statement
store_document(Conn, DocId, Content) :-
    jdbc_prepare(Conn, 'INSERT INTO documents(id, content) VALUES (?, ?)', Stmt),
    jdbc_set_param(Stmt, 1, DocId),
    jdbc_set_clob(Stmt, 2, Content),           % Set CLOB from atom/string
    jdbc_execute_prepared_update(Stmt, _),
    jdbc_close_statement(Stmt).
```

### Reading CLOBs

```prolog
% Read the CLOB content as an atom
?- jdbc_get_clob(Conn, 'SELECT content FROM documents WHERE id = 1', Text).
% Text = 'This is a very long document text...'
```

### Writing BLOBs (Binary Data)

```prolog
% From a file
store_image(Conn, ImageId, FilePath) :-
    jdbc_prepare(Conn, 'INSERT INTO images(id, data) VALUES (?, ?)', Stmt),
    jdbc_set_param(Stmt, 1, ImageId),
    jdbc_set_blob(Stmt, 2, FilePath),          % Set BLOB from file path
    jdbc_execute_prepared_update(Stmt, _),
    jdbc_close_statement(Stmt).

% From a list of bytes
store_small_binary(Conn, Id, Bytes) :-
    jdbc_prepare(Conn, 'INSERT INTO bindata(id, data) VALUES (?, ?)', Stmt),
    jdbc_set_param(Stmt, 1, Id),
    jdbc_set_blob_bytes(Stmt, 2, Bytes),       % Set BLOB from byte list [0-255]
    jdbc_execute_prepared_update(Stmt, _),
    jdbc_close_statement(Stmt).
% ?- store_small_binary(Conn, 1, [72, 101, 108, 108, 111]).
```

### Reading BLOBs

```prolog
% Save BLOB to file
?- jdbc_get_blob_to_file(Conn, 'SELECT data FROM images WHERE id = 1', '/tmp/output.png').

% Read BLOB as byte list
?- jdbc_get_blob_bytes(Conn, 'SELECT data FROM bindata WHERE id = 1', Bytes).
% Bytes = [72, 101, 108, 108, 111]
```

## 8. Date and Timestamp Handling

Dates and timestamps are handled as string atoms in ISO format.

### Writing Dates

```prolog
% Simple SQL with literal dates
?- jdbc_execute_update(Conn,
    'INSERT INTO events(name, event_date) VALUES (''Meeting'', ''2026-03-20'')', _).

% With prepared statements
store_event(Conn, Name, Date) :-
    jdbc_prepare(Conn, 'INSERT INTO events(name, event_date) VALUES (?, ?)', Stmt),
    jdbc_set_params(Stmt, [Name, Date]),       % Date as '2026-03-20'
    jdbc_execute_prepared_update(Stmt, _),
    jdbc_close_statement(Stmt).
% ?- store_event(Conn, 'Meeting', '2026-03-20').

% Timestamp with time
store_log(Conn, Message, Timestamp) :-
    jdbc_prepare(Conn, 'INSERT INTO logs(message, created_at) VALUES (?, ?)', Stmt),
    jdbc_set_params(Stmt, [Message, Timestamp]),   % '2026-03-20 14:30:00'
    jdbc_execute_prepared_update(Stmt, _),
    jdbc_close_statement(Stmt).
```

### Reading Dates

```prolog
% Dates come back as atoms in the driver's default format
?- jdbc_query(Conn, 'SELECT name, event_date FROM events', Rows).
% Rows = [row('Meeting', '2026-03-20')]

% Date comparison in SQL
?- jdbc_query(Conn, 'SELECT name FROM events WHERE event_date > ''2026-01-01''', Rows).
```

### Stored Procedures with Date OUT Parameters

```prolog
get_last_login(Conn, UserId, LastLogin) :-
    jdbc_prepare_call(Conn, '{call get_last_login(?, ?)}', CS),
    jdbc_call_set_param(CS, 1, UserId),
    jdbc_call_register_out(CS, 2, timestamp),  % Register as timestamp type
    jdbc_call_execute(CS),
    jdbc_call_get_result(CS, 2, LastLogin),    % Returns as atom e.g. '2026-03-20 14:30:00.0'
    jdbc_close_statement(CS).
```

## 9. Complete Example: User Management System

```prolog
% Initialize database
init_db(Conn) :-
    jdbc_connect('jdbc:h2:mem:testdb', Conn),
    jdbc_execute_update(Conn,
        'CREATE TABLE users(id INT AUTO_INCREMENT PRIMARY KEY, name VARCHAR(100), email VARCHAR(200), age INT, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, bio CLOB, avatar BLOB)', _).

% Add a user with all fields
add_user(Conn, Name, Email, Age, Bio) :-
    jdbc_prepare(Conn, 'INSERT INTO users(name, email, age, bio) VALUES (?, ?, ?, ?)', Stmt),
    jdbc_set_param(Stmt, 1, Name),
    jdbc_set_param(Stmt, 2, Email),
    jdbc_set_param(Stmt, 3, Age),
    jdbc_set_clob(Stmt, 4, Bio),
    jdbc_execute_prepared_update(Stmt, _),
    jdbc_close_statement(Stmt).

% Find users by age range
find_users_by_age(Conn, MinAge, MaxAge, Users) :-
    jdbc_prepare(Conn, 'SELECT name, email, age FROM users WHERE age BETWEEN ? AND ?', Stmt),
    jdbc_set_params(Stmt, [MinAge, MaxAge]),
    jdbc_execute_prepared_query(Stmt, Users),
    jdbc_close_statement(Stmt).

% Safe transfer with transaction
safe_update(Conn, Updates) :-
    jdbc_set_autocommit(Conn, false),
    (   run_updates(Conn, Updates),
        jdbc_commit(Conn)
    ;   jdbc_rollback(Conn), fail
    ),
    jdbc_set_autocommit(Conn, true).

run_updates(_, []).
run_updates(Conn, [sql(SQL, Params)|Rest]) :-
    jdbc_prepare(Conn, SQL, Stmt),
    jdbc_set_params(Stmt, Params),
    jdbc_execute_prepared_update(Stmt, _),
    jdbc_close_statement(Stmt),
    run_updates(Conn, Rest).
```

## 10. Predicate Reference Summary

| Predicate | Description |
|-----------|-------------|
| **Connection** | |
| `jdbc_driver_load/1` | Load JDBC driver class |
| `jdbc_connect/2` | Connect without credentials |
| `jdbc_connect/4` | Connect with user/password |
| `jdbc_disconnect/1` | Close connection |
| **Simple SQL** | |
| `jdbc_query/3` | Execute SELECT, return rows |
| `jdbc_execute_update/3` | Execute INSERT/UPDATE/DELETE |
| **Prepared Statements** | |
| `jdbc_prepare/3` | Prepare parameterized SQL |
| `jdbc_set_param/3` | Set single parameter (1-based) |
| `jdbc_set_params/2` | Set all parameters from list |
| `jdbc_execute_prepared_query/2` | Execute prepared SELECT |
| `jdbc_execute_prepared_update/2` | Execute prepared DML |
| `jdbc_close_statement/1` | Close prepared/callable stmt |
| **Transactions** | |
| `jdbc_set_autocommit/2` | Enable/disable autocommit |
| `jdbc_commit/1` | Commit transaction |
| `jdbc_rollback/1` | Rollback transaction |
| **Metadata** | |
| `jdbc_tables/2` | List database tables |
| `jdbc_columns/3` | List table columns |
| **Stored Procedures** | |
| `jdbc_prepare_call/3` | Prepare callable statement |
| `jdbc_call_set_param/3` | Set IN parameter |
| `jdbc_call_register_out/3` | Register OUT parameter type |
| `jdbc_call_execute/1` | Execute stored procedure |
| `jdbc_call_get_result/3` | Get OUT parameter value |
| `jdbc_call_get_resultset/2` | Get procedure's ResultSet |
| **CLOB/BLOB** | |
| `jdbc_set_clob/3` | Set CLOB from text |
| `jdbc_set_blob/3` | Set BLOB from file |
| `jdbc_set_blob_bytes/3` | Set BLOB from byte list |
| `jdbc_get_clob/3` | Read CLOB as text |
| `jdbc_get_blob_to_file/3` | Save BLOB to file |
| `jdbc_get_blob_bytes/3` | Read BLOB as byte list |
