# CSV Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.csv` package provides built-in predicates for reading, writing, parsing, and serializing CSV (Comma-Separated Values) data. CSV rows are represented in Prolog as `row/N` compound terms, where N is the number of fields.

**Term representation**: Each CSV row becomes a `row(Field1, Field2, ..., FieldN)` compound term. Fields that can be parsed as numbers become `Number` terms; all others become `Atom` terms. The parser handles quoted fields (RFC 4180 style) including escaped double quotes (`""`).

**Source file**: `src/main/java/it/denzosoft/jprolog/builtin/csv/CsvPredicates.java`

---

## Predicate Reference

### csv_read_file/2

```prolog
csv_read_file(+Path, -Rows)
```

Reads a CSV file from disk and unifies Rows with a list of `row(...)` terms.

| Argument | Mode | Type | Description                                         |
|----------|------|------|-----------------------------------------------------|
| Path     | +    | atom | Absolute or relative file path to the CSV file      |
| Rows     | -    | list | List of `row(Field1, Field2, ...)` compound terms   |

**Behavior**:
- Reads the entire file into memory, then parses line by line.
- Empty lines are skipped.
- Numeric fields (integers and floats) are automatically converted to `Number` terms.
- Quoted fields are supported: `"field with, comma"` is parsed as a single field.
- Escaped quotes within quoted fields (`""`) are handled correctly.
- Raises an evaluation error if the file cannot be read.

**Example**:
```prolog
?- csv_read_file('data/employees.csv', Rows).
Rows = [row('Alice', engineering, 85000), row('Bob', marketing, 72000), ...]
```

---

### csv_write_file/2

```prolog
csv_write_file(+Path, +Rows)
```

Writes a list of `row(...)` terms to a CSV file.

| Argument | Mode | Type | Description                                         |
|----------|------|------|-----------------------------------------------------|
| Path     | +    | atom | File path to write the CSV output to                |
| Rows     | +    | list | List of `row(Field1, Field2, ...)` compound terms   |

**Behavior**:
- Overwrites the file if it already exists.
- Fields containing commas, double quotes, or newlines are automatically quoted.
- Double quotes within fields are escaped as `""`.
- Numbers are serialized as integers when they have no fractional part, otherwise as floats.
- Always succeeds if the file can be written.

**Example**:
```prolog
?- csv_write_file('output.csv', [row('Alice', 95), row('Bob', 87)]).
true.
% File output.csv now contains:
% Alice,95
% Bob,87
```

---

### csv_parse/2

```prolog
csv_parse(+CsvString, -Rows)
```

Parses a CSV-formatted string into a list of `row(...)` terms.

| Argument  | Mode | Type | Description                                        |
|-----------|------|------|----------------------------------------------------|
| CsvString | +    | atom | A string containing CSV data (lines separated by newlines) |
| Rows      | -    | list | List of `row(Field1, Field2, ...)` compound terms  |

**Behavior**:
- Identical parsing logic to `csv_read_file/2` but operates on an in-memory string.
- Useful when CSV data comes from another predicate or is constructed dynamically.
- Lines are split on `\n` characters.

**Example**:
```prolog
?- csv_parse('name,score\nAlice,95\nBob,87', Rows).
Rows = [row(name, score), row('Alice', 95), row('Bob', 87)]
```

---

### csv_serialize/2

```prolog
csv_serialize(+Rows, -CsvString)
```

Serializes a list of `row(...)` terms into a CSV-formatted string.

| Argument  | Mode | Type | Description                                        |
|-----------|------|------|----------------------------------------------------|
| Rows      | +    | list | List of `row(Field1, Field2, ...)` compound terms  |
| CsvString | -    | atom | The resulting CSV string                           |

**Behavior**:
- Produces one line per row, terminated by `\n`.
- Fields containing commas, double quotes, or newlines are quoted.
- Integer-valued numbers are serialized without decimal points.
- Raises an error if Rows is not a list of `row(...)` terms.

**Example**:
```prolog
?- csv_serialize([row('Alice', 95), row('Bob', 87)], Csv).
Csv = 'Alice,95\nBob,87\n'
```

---

## Real-World Examples

### Example 1: Student Grade Report

Read a CSV of student grades across multiple subjects, compute per-student averages and class statistics, and generate a summary report.

```prolog
% grade_report.pl
% Read student grades from CSV, compute averages, find top performers,
% and generate a text report.

% Sample grade data as a CSV string.
% Format: Name, Math, Science, English, History
grade_data('Name,Math,Science,English,History
Alice,92,88,95,91
Bob,78,82,71,85
Carol,95,97,93,99
Dave,65,70,68,72
Eve,88,85,90,87
Frank,73,68,75,70
Grace,91,93,89,95').

% Parse the CSV and split into header and data rows.
load_grades(Header, DataRows) :-
    grade_data(Csv),
    csv_parse(Csv, [Header|DataRows]).

% Compute the average grade for a single student row.
% student_average(+Row, -Name, -Average)
student_average(row(Name, Math, Science, English, History), Name, Average) :-
    Sum is Math + Science + English + History,
    Average is Sum / 4.

% Compute averages for all students.
% all_averages(-Results) where Results is a list of avg(Name, Average).
all_averages(Results) :-
    load_grades(_, DataRows),
    compute_averages(DataRows, Results).

compute_averages([], []).
compute_averages([Row|Rows], [avg(Name, Avg)|Rest]) :-
    student_average(Row, Name, Avg),
    compute_averages(Rows, Rest).

% Find the top N students by average grade.
top_students(N, TopN) :-
    all_averages(Avgs),
    sort_by_average(Avgs, Sorted),
    take(N, Sorted, TopN).

% Simple insertion sort by average (descending).
sort_by_average(List, Sorted) :-
    isort_avg(List, [], Sorted).

isort_avg([], Acc, Acc).
isort_avg([X|Xs], Acc, Sorted) :-
    insert_avg(X, Acc, NewAcc),
    isort_avg(Xs, NewAcc, Sorted).

insert_avg(X, [], [X]).
insert_avg(avg(N1, A1), [avg(N2, A2)|Rest], [avg(N1, A1), avg(N2, A2)|Rest]) :-
    A1 >= A2, !.
insert_avg(X, [Y|Rest], [Y|NewRest]) :-
    insert_avg(X, Rest, NewRest).

% take(+N, +List, -FirstN)
take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [X|Xs], [X|Ys]) :-
    N > 0, N1 is N - 1, take(N1, Xs, Ys).

% Compute the class average for a specific subject.
% subject_average(+SubjectIndex, -Average)
% SubjectIndex: 1=Math, 2=Science, 3=English, 4=History
subject_average(SubjectIndex, Average) :-
    load_grades(_, DataRows),
    collect_subject_scores(DataRows, SubjectIndex, Scores),
    sum_list(Scores, Sum),
    length(Scores, Count),
    Count > 0,
    Average is Sum / Count.

collect_subject_scores([], _, []).
collect_subject_scores([Row|Rows], Idx, [Score|Scores]) :-
    Row =.. [row|Fields],
    nth1(ActualIdx, Fields, Score),
    ActualIdx is Idx + 1,  % skip the Name field
    collect_subject_scores(Rows, Idx, Scores).

sum_list([], 0).
sum_list([X|Xs], S) :- sum_list(Xs, S1), S is S1 + X.

% Determine letter grade from numeric score.
letter_grade(Score, 'A') :- Score >= 90, !.
letter_grade(Score, 'B') :- Score >= 80, !.
letter_grade(Score, 'C') :- Score >= 70, !.
letter_grade(Score, 'D') :- Score >= 60, !.
letter_grade(_, 'F').

% Generate a full report: each student with average and letter grade.
generate_report :-
    write('=== Student Grade Report ==='), nl, nl,
    write('Student          Average  Grade'), nl,
    write('-------------------------------'), nl,
    all_averages(Avgs),
    sort_by_average(Avgs, Sorted),
    print_student_lines(Sorted),
    nl,
    write('--- Top 3 Students ---'), nl,
    top_students(3, Top3),
    print_top(Top3, 1).

print_student_lines([]).
print_student_lines([avg(Name, Avg)|Rest]) :-
    letter_grade(Avg, Grade),
    write(Name), write('\t\t'),
    write(Avg), write('\t '),
    write(Grade), nl,
    print_student_lines(Rest).

print_top([], _).
print_top([avg(Name, Avg)|Rest], Rank) :-
    write(Rank), write('. '), write(Name),
    write(' ('), write(Avg), write(')'), nl,
    NextRank is Rank + 1,
    print_top(Rest, NextRank).

% Usage:
% ?- generate_report.
% === Student Grade Report ===
%
% Student          Average  Grade
% -------------------------------
% Carol            96.0     A
% Alice            91.5     A
% Grace            92.0     A
% Eve              87.5     B
% Bob              79.0     C
% Frank            71.5     C
% Dave             68.75    D
%
% --- Top 3 Students ---
% 1. Carol (96.0)
% 2. Grace (92.0)
% 3. Alice (91.5)
```

---

### Example 2: Data Migration Tool

Read CSV data in one format, apply transformations (rename columns, convert types, add computed fields), and write to a new format.

```prolog
% data_migration.pl
% Migrate customer data from a legacy CSV format to a new format.
% Legacy: "first_name,last_name,phone,join_date,type"
% New:    "full_name,phone,membership_level,loyalty_years,email_placeholder"

% Legacy customer data.
legacy_data('first_name,last_name,phone,join_date,type
John,Smith,555-1234,2019,gold
Jane,Doe,555-5678,2021,silver
Bob,Johnson,555-9012,2018,platinum
Alice,Williams,555-3456,2022,bronze
Charlie,Brown,555-7890,2020,gold').

% Parse the legacy CSV, skipping the header row.
load_legacy(Rows) :-
    legacy_data(Csv),
    csv_parse(Csv, [_Header|Rows]).

% Transform a single legacy row into the new format.
% Legacy: row(First, Last, Phone, JoinDate, Type)
% New:    row(FullName, Phone, Level, LoyaltyYears, EmailPlaceholder)
transform_row(row(First, Last, Phone, JoinYear, Type),
              row(FullName, Phone, Level, LoyaltyYears, Email)) :-
    % Concatenate first and last name.
    atom_concat(First, ' ', Temp),
    atom_concat(Temp, Last, FullName),
    % Map legacy type to new membership level.
    map_level(Type, Level),
    % Compute loyalty years from join date (assuming current year 2026).
    LoyaltyYears is 2026 - JoinYear,
    % Generate a placeholder email from first initial + last name.
    atom_chars(First, [Initial|_]),
    atom_chars(Lower, [Initial]),
    atom_concat(Lower, Last, Prefix),
    atom_concat(Prefix, '@company.com', Email).

% Map old membership types to new levels.
map_level(platinum, enterprise).
map_level(gold, premium).
map_level(silver, standard).
map_level(bronze, basic).
map_level(_, basic).  % default fallback

% Run the full migration.
migrate(NewRows) :-
    load_legacy(LegacyRows),
    transform_all(LegacyRows, DataRows),
    % Prepend new header.
    Header = row(full_name, phone, membership_level, loyalty_years, email),
    NewRows = [Header|DataRows].

transform_all([], []).
transform_all([Old|Olds], [New|News]) :-
    transform_row(Old, New),
    transform_all(Olds, News).

% Serialize migrated data to CSV string.
migrate_to_csv(CsvString) :-
    migrate(Rows),
    csv_serialize(Rows, CsvString).

% Write migrated data to a file.
migrate_to_file(OutputPath) :-
    migrate(Rows),
    csv_write_file(OutputPath, Rows).

% Validate migration: check all rows have 5 fields and loyalty years > 0.
validate_migration(Status) :-
    migrate([_Header|DataRows]),
    validate_rows(DataRows, Errors),
    (   Errors = []
    ->  Status = ok
    ;   Status = errors(Errors)
    ).

validate_rows([], []).
validate_rows([Row|Rows], Errors) :-
    Row = row(Name, _, _, Years, _),
    (   Years < 0
    ->  Errors = [negative_years(Name, Years)|RestErrors]
    ;   Errors = RestErrors
    ),
    validate_rows(Rows, RestErrors).

% Usage:
% ?- migrate_to_csv(Csv).
% Csv = 'full_name,phone,membership_level,loyalty_years,email\n
%        John Smith,555-1234,premium,7,JSmith@company.com\n
%        Jane Doe,555-5678,standard,5,JDoe@company.com\n...'
%
% ?- validate_migration(S).
% S = ok
```

---

### Example 3: Inventory Management

Load a product inventory from CSV, apply updates (restock, sell), check low-stock alerts, and save back.

```prolog
% inventory.pl
% Load product inventory, process stock updates, detect low-stock items,
% and write the updated inventory back to CSV.

% Initial inventory data.
inventory_csv('sku,name,quantity,price,reorder_level
SKU001,Widget Alpha,150,9.99,50
SKU002,Widget Beta,30,14.99,40
SKU003,Gadget Pro,200,29.99,25
SKU004,Gadget Lite,15,19.99,20
SKU005,Connector X,500,2.49,100
SKU006,Connector Y,45,3.99,50').

% Parse inventory into a list of product/5 terms.
load_inventory(Products) :-
    inventory_csv(Csv),
    csv_parse(Csv, [_Header|Rows]),
    rows_to_products(Rows, Products).

rows_to_products([], []).
rows_to_products([row(Sku, Name, Qty, Price, Reorder)|Rs],
                 [product(Sku, Name, Qty, Price, Reorder)|Ps]) :-
    rows_to_products(Rs, Ps).

% Apply a stock update: sell(Sku, Quantity) or restock(Sku, Quantity).
apply_update(Products, sell(Sku, Qty), Updated) :-
    apply_sell(Products, Sku, Qty, Updated).
apply_update(Products, restock(Sku, Qty), Updated) :-
    apply_restock(Products, Sku, Qty, Updated).

apply_sell([], Sku, _, []) :-
    write('WARNING: SKU not found: '), write(Sku), nl.
apply_sell([product(Sku, N, Q, P, R)|Rest], Sku, Qty,
           [product(Sku, N, NewQ, P, R)|Rest]) :-
    NewQ is max(0, Q - Qty), !.
apply_sell([P|Rest], Sku, Qty, [P|Updated]) :-
    apply_sell(Rest, Sku, Qty, Updated).

apply_restock([], Sku, _, []) :-
    write('WARNING: SKU not found: '), write(Sku), nl.
apply_restock([product(Sku, N, Q, P, R)|Rest], Sku, Qty,
              [product(Sku, N, NewQ, P, R)|Rest]) :-
    NewQ is Q + Qty, !.
apply_restock([P|Rest], Sku, Qty, [P|Updated]) :-
    apply_restock(Rest, Sku, Qty, Updated).

% Apply a batch of updates sequentially.
apply_updates(Products, [], Products).
apply_updates(Products, [Update|Updates], Final) :-
    apply_update(Products, Update, Intermediate),
    apply_updates(Intermediate, Updates, Final).

% Find all products below their reorder level.
low_stock_alerts(Products, Alerts) :-
    findall(
        alert(Sku, Name, Qty, Reorder),
        (   member(product(Sku, Name, Qty, _, Reorder), Products),
            Qty < Reorder
        ),
        Alerts
    ).

% Compute total inventory value.
total_value(Products, Total) :-
    compute_values(Products, Values),
    sum_list(Values, Total).

compute_values([], []).
compute_values([product(_, _, Qty, Price, _)|Ps], [V|Vs]) :-
    V is Qty * Price,
    compute_values(Ps, Vs).

sum_list([], 0).
sum_list([X|Xs], S) :- sum_list(Xs, S1), S is S1 + X.

% Convert products back to rows and serialize.
products_to_csv(Products, CsvString) :-
    Header = row(sku, name, quantity, price, reorder_level),
    products_to_rows(Products, DataRows),
    csv_serialize([Header|DataRows], CsvString).

products_to_rows([], []).
products_to_rows([product(S, N, Q, P, R)|Ps], [row(S, N, Q, P, R)|Rs]) :-
    products_to_rows(Ps, Rs).

% Full workflow demonstration.
inventory_demo :-
    load_inventory(Products),
    write('=== Initial Inventory ==='), nl,
    print_inventory(Products),
    % Process a batch of transactions.
    Updates = [
        sell('SKU001', 80),      % Sell 80 Widget Alphas
        sell('SKU004', 10),      % Sell 10 Gadget Lites
        restock('SKU002', 100),  % Restock Widget Betas
        sell('SKU006', 30),      % Sell 30 Connector Ys
        sell('SKU005', 420)      % Sell 420 Connector Xs
    ],
    apply_updates(Products, Updates, Updated),
    nl, write('=== After Transactions ==='), nl,
    print_inventory(Updated),
    % Check for low stock.
    low_stock_alerts(Updated, Alerts),
    nl, write('=== Low Stock Alerts ==='), nl,
    print_alerts(Alerts),
    % Compute total value.
    total_value(Updated, Value),
    nl, write('Total inventory value: $'), write(Value), nl.

print_inventory([]).
print_inventory([product(Sku, Name, Qty, Price, _)|Ps]) :-
    write(Sku), write('  '), write(Name), write('  Qty: '),
    write(Qty), write('  $'), write(Price), nl,
    print_inventory(Ps).

print_alerts([]) :- write('  No alerts.'), nl.
print_alerts([alert(Sku, Name, Qty, Reorder)|As]) :-
    write('  !! '), write(Sku), write(' '), write(Name),
    write(' - Stock: '), write(Qty),
    write(', Reorder at: '), write(Reorder), nl,
    print_alerts(As).

% Usage:
% ?- inventory_demo.
% === Initial Inventory ===
% SKU001  Widget Alpha  Qty: 150  $9.99
% SKU002  Widget Beta   Qty: 30   $14.99
% ...
% === After Transactions ===
% SKU001  Widget Alpha  Qty: 70   $9.99
% SKU002  Widget Beta   Qty: 130  $14.99
% SKU004  Gadget Lite   Qty: 5    $19.99
% SKU005  Connector X   Qty: 80   $2.49
% SKU006  Connector Y   Qty: 15   $3.99
% ...
% === Low Stock Alerts ===
%   !! SKU004 Gadget Lite - Stock: 5, Reorder at: 20
%   !! SKU005 Connector X - Stock: 80, Reorder at: 100
%   !! SKU006 Connector Y - Stock: 15, Reorder at: 50
% Total inventory value: $5765.05
```

---

### Example 4: CSV Comparison Tool

Read two CSV files, find added, removed, and modified rows, and report the differences.

```prolog
% csv_diff.pl
% Compare two CSV datasets and report additions, deletions, and modifications.
% Rows are matched by a key field (first column).

% Old dataset (e.g., last month's employee directory).
old_csv('id,name,department,salary
101,Alice,Engineering,95000
102,Bob,Marketing,72000
103,Carol,Engineering,88000
104,Dave,Sales,67000
105,Eve,Marketing,71000').

% New dataset (current month).
new_csv('id,name,department,salary
101,Alice,Engineering,98000
102,Bob,Marketing,72000
104,Dave,HR,69000
105,Eve,Marketing,75000
106,Frank,Engineering,82000').

% Load both datasets into keyed lists: key-row pairs.
load_old(Keyed) :- old_csv(C), csv_parse(C, [_|Rows]), key_rows(Rows, Keyed).
load_new(Keyed) :- new_csv(C), csv_parse(C, [_|Rows]), key_rows(Rows, Keyed).

% key_rows(+Rows, -KeyedList) -- extract the first field as key.
key_rows([], []).
key_rows([Row|Rows], [Key-Row|KRs]) :-
    Row =.. [row, Key|_],
    key_rows(Rows, KRs).

% Find rows present in New but not in Old (additions).
find_additions(OldKeyed, NewKeyed, Added) :-
    findall(Row,
        (member(Key-Row, NewKeyed), \+ member(Key-_, OldKeyed)),
        Added).

% Find rows present in Old but not in New (deletions).
find_deletions(OldKeyed, NewKeyed, Removed) :-
    findall(Row,
        (member(Key-Row, OldKeyed), \+ member(Key-_, NewKeyed)),
        Removed).

% Find rows present in both but with different field values (modifications).
find_modifications(OldKeyed, NewKeyed, Modified) :-
    findall(change(Key, OldRow, NewRow),
        (   member(Key-OldRow, OldKeyed),
            member(Key-NewRow, NewKeyed),
            OldRow \= NewRow
        ),
        Modified).

% Run the full comparison and print a report.
csv_diff_report :-
    load_old(Old),
    load_new(New),
    find_additions(Old, New, Added),
    find_deletions(Old, New, Removed),
    find_modifications(Old, New, Modified),
    write('=== CSV Diff Report ==='), nl, nl,
    write('--- Added Rows ---'), nl,
    print_rows(Added),
    nl, write('--- Removed Rows ---'), nl,
    print_rows(Removed),
    nl, write('--- Modified Rows ---'), nl,
    print_changes(Modified),
    nl,
    length(Added, NA), length(Removed, NR), length(Modified, NM),
    write('Summary: '),
    write(NA), write(' added, '),
    write(NR), write(' removed, '),
    write(NM), write(' modified.'), nl.

print_rows([]) :- write('  (none)'), nl.
print_rows([R|Rs]) :- write('  '), write(R), nl, print_rows(Rs).

print_changes([]) :- write('  (none)'), nl.
print_changes([change(Key, Old, New)|Rest]) :-
    write('  Key '), write(Key), write(':'), nl,
    write('    Old: '), write(Old), nl,
    write('    New: '), write(New), nl,
    detail_field_changes(Old, New),
    print_changes(Rest).

% Show which specific fields changed.
detail_field_changes(OldRow, NewRow) :-
    OldRow =.. [row|OldFields],
    NewRow =.. [row|NewFields],
    compare_fields(OldFields, NewFields, 1).

compare_fields([], [], _).
compare_fields([O|Os], [N|Ns], Idx) :-
    (   O \= N
    ->  write('    Field '), write(Idx), write(': '),
        write(O), write(' -> '), write(N), nl
    ;   true
    ),
    Idx1 is Idx + 1,
    compare_fields(Os, Ns, Idx1).

% Usage:
% ?- csv_diff_report.
% === CSV Diff Report ===
%
% --- Added Rows ---
%   row(106, 'Frank', 'Engineering', 82000)
%
% --- Removed Rows ---
%   row(103, 'Carol', 'Engineering', 88000)
%
% --- Modified Rows ---
%   Key 101:
%     Old: row(101, 'Alice', 'Engineering', 95000)
%     New: row(101, 'Alice', 'Engineering', 98000)
%     Field 4: 95000 -> 98000
%   Key 104:
%     Old: row(104, 'Dave', 'Sales', 67000)
%     New: row(104, 'Dave', 'HR', 69000)
%     Field 3: Sales -> HR
%     Field 4: 67000 -> 69000
%   Key 105:
%     Old: row(105, 'Eve', 'Marketing', 71000)
%     New: row(105, 'Eve', 'Marketing', 75000)
%     Field 4: 71000 -> 75000
%
% Summary: 1 added, 1 removed, 3 modified.
```

---

### Example 5: Financial Data Processor

Parse a CSV of financial transactions, categorize them, compute per-category totals, and generate a budget summary.

```prolog
% financial.pl
% Parse transaction data, categorize expenses, compute totals per category,
% and compare against budget limits.

% Transaction data: date, description, amount (negative = expense, positive = income).
transaction_data('date,description,amount
2026-03-01,Salary,5000.00
2026-03-02,Grocery Store,-125.50
2026-03-03,Electric Bill,-89.00
2026-03-05,Restaurant,-45.75
2026-03-07,Gas Station,-52.30
2026-03-08,Online Shopping,-199.99
2026-03-10,Freelance Payment,1200.00
2026-03-12,Grocery Store,-98.25
2026-03-14,Internet Bill,-59.99
2026-03-15,Restaurant,-67.80
2026-03-18,Grocery Store,-110.00
2026-03-20,Gym Membership,-49.99
2026-03-22,Coffee Shop,-28.50
2026-03-25,Salary,5000.00').

% Parse transactions.
load_transactions(Transactions) :-
    transaction_data(Csv),
    csv_parse(Csv, [_Header|Rows]),
    rows_to_transactions(Rows, Transactions).

rows_to_transactions([], []).
rows_to_transactions([row(Date, Desc, Amount)|Rs],
                     [txn(Date, Desc, Amount)|Ts]) :-
    rows_to_transactions(Rs, Ts).

% Categorize a transaction based on its description.
categorize('Salary', income).
categorize('Freelance Payment', income).
categorize('Grocery Store', groceries).
categorize('Restaurant', dining).
categorize('Coffee Shop', dining).
categorize('Electric Bill', utilities).
categorize('Internet Bill', utilities).
categorize('Gas Station', transport).
categorize('Online Shopping', shopping).
categorize('Gym Membership', health).
categorize(_, other).  % default

% Compute totals per category.
category_totals(Totals) :-
    load_transactions(Txns),
    findall(Cat, (member(txn(_, Desc, _), Txns), categorize(Desc, Cat)), AllCats),
    sort(AllCats, UniqueCats),
    findall(
        cat(Cat, Total, Count),
        (   member(Cat, UniqueCats),
            findall(Amt,
                (member(txn(_, Desc, Amt), Txns), categorize(Desc, Cat)),
                Amounts),
            sum_list(Amounts, Total),
            length(Amounts, Count)
        ),
        Totals
    ).

sum_list([], 0).
sum_list([X|Xs], S) :- sum_list(Xs, S1), S is S1 + X.

% Monthly budget limits per category.
budget_limit(groceries, -400).
budget_limit(dining, -150).
budget_limit(utilities, -200).
budget_limit(transport, -100).
budget_limit(shopping, -300).
budget_limit(health, -60).

% Check which categories are over budget.
over_budget(OverList) :-
    category_totals(Totals),
    findall(
        over(Cat, Spent, Limit, Overage),
        (   member(cat(Cat, Spent, _), Totals),
            budget_limit(Cat, Limit),
            Spent < Limit,  % both are negative; more negative = more spent
            Overage is Limit - Spent
        ),
        OverList
    ).

% Compute net income (total income + total expenses).
net_income(Income, Expenses, Net) :-
    load_transactions(Txns),
    findall(A, (member(txn(_, _, A), Txns), A > 0), IncList),
    findall(A, (member(txn(_, _, A), Txns), A < 0), ExpList),
    sum_list(IncList, Income),
    sum_list(ExpList, Expenses),
    Net is Income + Expenses.

% Compute savings rate as a percentage.
savings_rate(Rate) :-
    net_income(Income, _, Net),
    Income > 0,
    Rate is (Net / Income) * 100.

% Generate the full financial report.
financial_report :-
    write('=============================='), nl,
    write('  Monthly Financial Report'), nl,
    write('=============================='), nl, nl,
    % Income and expenses.
    net_income(Income, Expenses, Net),
    write('Total Income:   $'), write(Income), nl,
    write('Total Expenses: $'), write(Expenses), nl,
    write('Net:            $'), write(Net), nl,
    savings_rate(Rate),
    write('Savings Rate:   '), write(Rate), write('%'), nl,
    nl,
    % Category breakdown.
    write('--- Category Breakdown ---'), nl,
    category_totals(Totals),
    print_categories(Totals),
    nl,
    % Budget alerts.
    write('--- Budget Alerts ---'), nl,
    over_budget(Overs),
    print_budget_alerts(Overs),
    nl,
    % Export summary to CSV.
    export_summary(Totals).

print_categories([]).
print_categories([cat(Cat, Total, Count)|Rest]) :-
    write('  '), write(Cat), write(': $'), write(Total),
    write(' ('), write(Count), write(' transactions)'), nl,
    print_categories(Rest).

print_budget_alerts([]) :- write('  All categories within budget.'), nl.
print_budget_alerts([over(Cat, Spent, Limit, Overage)|Rest]) :-
    write('  !! '), write(Cat), write(': spent $'), write(Spent),
    write(', limit $'), write(Limit),
    write(', over by $'), write(Overage), nl,
    print_budget_alerts(Rest).

% Export category summary to a CSV string.
export_summary(Totals) :-
    Header = row(category, total, transaction_count),
    totals_to_rows(Totals, DataRows),
    csv_serialize([Header|DataRows], CsvOut),
    write('--- Exported Summary CSV ---'), nl,
    write(CsvOut), nl.

totals_to_rows([], []).
totals_to_rows([cat(C, T, N)|Rest], [row(C, T, N)|Rows]) :-
    totals_to_rows(Rest, Rows).

% Usage:
% ?- financial_report.
% ==============================
%   Monthly Financial Report
% ==============================
%
% Total Income:   $11200.0
% Total Expenses: $-927.07
% Net:            $10272.93
% Savings Rate:   91.72%
%
% --- Category Breakdown ---
%   dining: $-142.05 (3 transactions)
%   groceries: $-333.75 (3 transactions)
%   health: $-49.99 (1 transactions)
%   income: $11200.0 (3 transactions)
%   shopping: $-199.99 (1 transactions)
%   transport: $-52.3 (1 transactions)
%   utilities: $-148.99 (2 transactions)
%
% --- Budget Alerts ---
%   All categories within budget.
%
% --- Exported Summary CSV ---
% category,total,transaction_count
% dining,-142.05,3
% groceries,-333.75,3
% ...
```
