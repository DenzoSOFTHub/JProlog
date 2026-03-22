# Date/Time Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.datetime` package provides date and time manipulation predicates for JProlog. It covers current time retrieval, date arithmetic, date decomposition, formatting, and parsing. Dates are represented as ISO 8601 string atoms (`'YYYY-MM-DD'` for dates, ISO local date-time for timestamps), and timestamps as epoch milliseconds.

**Source file:** `DateTimePredicates.java`

### Date and Time Representations

| Concept           | Prolog Representation               | Example                       |
|-------------------|--------------------------------------|-------------------------------|
| Date              | Atom in `YYYY-MM-DD` format         | `'2025-03-21'`               |
| DateTime          | Atom in ISO 8601 local format       | `'2025-03-21T14:30:00'`     |
| Epoch timestamp   | Number (milliseconds since epoch)   | `1742572200000`              |
| Day of week       | Lowercase atom                      | `monday`, `friday`           |
| Duration unit     | Atom: `days`, `weeks`, `months`, `years` | `days`                  |

All date strings are parsed using Java's `LocalDate.parse()` (ISO 8601). DateTime strings are parsed with `LocalDateTime.parse()`. If a date-only string is given where a datetime is expected, it is interpreted as midnight (`T00:00:00`).

---

## Predicate Reference

### get_time/1

```prolog
get_time(-Timestamp)
```

Unifies Timestamp with the current system time as epoch milliseconds (a number).

| Argument  | Type   | Mode | Description                              |
|-----------|--------|------|------------------------------------------|
| Timestamp | number | -    | Current time in milliseconds since epoch |

---

### now/1

```prolog
now(-DateTimeAtom)
```

Unifies DateTimeAtom with the current local date and time as an ISO 8601 string atom (e.g., `'2025-03-21T14:30:15.123'`).

| Argument     | Type | Mode | Description                                |
|--------------|------|------|--------------------------------------------|
| DateTimeAtom | atom | -    | Current datetime in ISO local format       |

---

### today/1

```prolog
today(-DateAtom)
```

Unifies DateAtom with the current local date as an atom in `YYYY-MM-DD` format.

| Argument | Type | Mode | Description                           |
|----------|------|------|---------------------------------------|
| DateAtom | atom | -    | Current date as `'YYYY-MM-DD'`        |

---

### date_add/4

```prolog
date_add(+Date, +Amount, +Unit, -NewDate)
```

Adds a specified amount of time to a date and returns the resulting date. Negative amounts subtract from the date.

| Argument | Type   | Mode | Description                                             |
|----------|--------|------|---------------------------------------------------------|
| Date     | atom   | +    | Starting date in `YYYY-MM-DD` format                   |
| Amount   | number | +    | Amount to add (positive) or subtract (negative)        |
| Unit     | atom   | +    | Unit of time: `days`, `weeks`, `months`, or `years`    |
| NewDate  | atom   | -    | Resulting date in `YYYY-MM-DD` format                  |

**Errors:**
- `evaluation_error` if Amount is not a number.
- `evaluation_error` if Unit is not one of the supported values.
- Parsing error if Date is not a valid ISO date string.

---

### date_diff/4

```prolog
date_diff(+Date1, +Date2, +Unit, -Diff)
```

Computes the difference between two dates in the specified unit. The result is `Date2 - Date1`, so a positive value means Date2 is after Date1.

| Argument | Type   | Mode | Description                                             |
|----------|--------|------|---------------------------------------------------------|
| Date1    | atom   | +    | First date in `YYYY-MM-DD` format                      |
| Date2    | atom   | +    | Second date in `YYYY-MM-DD` format                     |
| Unit     | atom   | +    | Unit of measurement: `days`, `weeks`, `months`, `years` |
| Diff     | number | -    | The difference (Date2 - Date1) in the given unit       |

**Errors:**
- `evaluation_error` if Unit is not one of the supported values.

---

### day_of_week/2

```prolog
day_of_week(+Date, -DayName)
```

Determines the day of the week for a given date.

| Argument | Type | Mode | Description                                                     |
|----------|------|------|-----------------------------------------------------------------|
| Date     | atom | +    | A date in `YYYY-MM-DD` format                                  |
| DayName  | atom | -    | Lowercase day name: `monday`, `tuesday`, ..., `sunday`         |

---

### date_parts/4

```prolog
date_parts(+Date, -Year, -Month, -Day)
```

Decomposes a date into its year, month, and day components.

| Argument | Type   | Mode | Description                          |
|----------|--------|------|--------------------------------------|
| Date     | atom   | +    | A date in `YYYY-MM-DD` format       |
| Year     | number | -    | The year component (e.g., 2025)     |
| Month    | number | -    | The month component (1-12)          |
| Day      | number | -    | The day of the month (1-31)         |

---

### time_parts/4

```prolog
time_parts(+DateTime, -Hour, -Minute, -Second)
```

Decomposes a datetime into its time components. If a date-only string is provided, it is treated as midnight (00:00:00).

| Argument | Type   | Mode | Description                                  |
|----------|--------|------|----------------------------------------------|
| DateTime | atom   | +    | A datetime in ISO format, or a date string   |
| Hour     | number | -    | The hour component (0-23)                    |
| Minute   | number | -    | The minute component (0-59)                  |
| Second   | number | -    | The second component (0-59)                  |

---

### format_time/3

```prolog
format_time(+Format, +Timestamp, -Formatted)
```

Formats a timestamp or datetime atom according to a Java `DateTimeFormatter` pattern string.

| Argument  | Type        | Mode | Description                                             |
|-----------|-------------|------|---------------------------------------------------------|
| Format    | atom        | +    | A Java DateTimeFormatter pattern (e.g., `'yyyy/MM/dd'`) |
| Timestamp | number/atom | +    | Epoch millis (number) or ISO datetime atom              |
| Formatted | atom        | -    | The formatted date/time string                          |

**Common format patterns:**

| Pattern       | Output Example       | Description            |
|---------------|----------------------|------------------------|
| `yyyy-MM-dd`  | `2025-03-21`         | ISO date               |
| `dd/MM/yyyy`  | `21/03/2025`         | European date          |
| `MM-dd-yyyy`  | `03-21-2025`         | US date                |
| `HH:mm:ss`    | `14:30:00`           | 24-hour time           |
| `hh:mm a`     | `02:30 PM`           | 12-hour time           |
| `yyyy-MM-dd HH:mm` | `2025-03-21 14:30` | Date and time        |
| `EEEE`        | `Friday`             | Full day name          |
| `MMM dd, yyyy`| `Mar 21, 2025`       | Human-readable date    |

---

### parse_time/3

```prolog
parse_time(+Format, +String, -Timestamp)
```

Parses a date/time string according to the given format pattern and returns the result as epoch milliseconds.

| Argument  | Type   | Mode | Description                                              |
|-----------|--------|------|----------------------------------------------------------|
| Format    | atom   | +    | A Java DateTimeFormatter pattern                         |
| String    | atom   | +    | The date/time string to parse                            |
| Timestamp | number | -    | The parsed date/time as epoch milliseconds               |

**Errors:**
- `evaluation_error` if the string does not match the format.

---

## Esempi Reali (Real Examples)

### Example 1: Age Calculator from Birth Date

Compute a person's age in years, months, and days from their birth date, and determine their zodiac sign.

```prolog
% ============================================================
% Age calculator
% Computes exact age from birth date and determines zodiac sign.
% ============================================================

% Calculate age in complete years.
age_years(BirthDate, Age) :-
    today(Today),
    date_diff(BirthDate, Today, years, Age).

% Calculate detailed age breakdown: years, months, and days.
age_detailed(BirthDate, Years, Months, ExtraDays) :-
    today(Today),
    date_diff(BirthDate, Today, years, Years),
    % Compute the date "Years" years after birth to find remaining months.
    date_add(BirthDate, Years, years, AfterYears),
    date_diff(AfterYears, Today, months, Months),
    % Compute remaining days after full months.
    date_add(AfterYears, Months, months, AfterMonths),
    date_diff(AfterMonths, Today, days, ExtraDays).

% Determine zodiac sign from birth date.
zodiac_sign(BirthDate, Sign) :-
    date_parts(BirthDate, _, Month, Day),
    zodiac(Month, Day, Sign).

zodiac(1, Day, capricorn)   :- Day =< 19.
zodiac(1, Day, aquarius)    :- Day >= 20.
zodiac(2, Day, aquarius)    :- Day =< 18.
zodiac(2, Day, pisces)      :- Day >= 19.
zodiac(3, Day, pisces)      :- Day =< 20.
zodiac(3, Day, aries)       :- Day >= 21.
zodiac(4, Day, aries)       :- Day =< 19.
zodiac(4, Day, taurus)      :- Day >= 20.
zodiac(5, Day, taurus)      :- Day =< 20.
zodiac(5, Day, gemini)      :- Day >= 21.
zodiac(6, Day, gemini)      :- Day =< 20.
zodiac(6, Day, cancer)      :- Day >= 21.
zodiac(7, Day, cancer)      :- Day =< 22.
zodiac(7, Day, leo)         :- Day >= 23.
zodiac(8, Day, leo)         :- Day =< 22.
zodiac(8, Day, virgo)       :- Day >= 23.
zodiac(9, Day, virgo)       :- Day =< 22.
zodiac(9, Day, libra)       :- Day >= 23.
zodiac(10, Day, libra)      :- Day =< 22.
zodiac(10, Day, scorpio)    :- Day >= 23.
zodiac(11, Day, scorpio)    :- Day =< 21.
zodiac(11, Day, sagittarius):- Day >= 22.
zodiac(12, Day, sagittarius):- Day =< 21.
zodiac(12, Day, capricorn)  :- Day >= 22.

% Full profile for a person.
person_profile(Name, BirthDate) :-
    age_detailed(BirthDate, Years, Months, Days),
    zodiac_sign(BirthDate, Sign),
    day_of_week(BirthDate, BornOn),
    write(Name), write(' (born '), write(BirthDate),
    write(', a '), write(BornOn), write(')'), nl,
    write('  Age: '), write(Years), write(' years, '),
    write(Months), write(' months, '),
    write(Days), write(' days'), nl,
    write('  Zodiac: '), write(Sign), nl.

% --- Usage ---
% ?- person_profile('Alice', '1990-07-15').
%    Alice (born 1990-07-15, a sunday)
%      Age: 34 years, 8 months, 6 days
%      Zodiac: cancer
%
% ?- age_years('2000-01-01', Age).
%    Age = 25
%
% ?- zodiac_sign('1985-11-30', Sign).
%    Sign = sagittarius
```

---

### Example 2: Business Day Counter (Skip Weekends)

Count business days between two dates, skipping Saturdays and Sundays.

```prolog
% ============================================================
% Business day counter
% Counts weekdays between two dates, excluding weekends.
% Also supports adding N business days to a date.
% ============================================================

% Check if a date falls on a weekend.
is_weekend(Date) :-
    day_of_week(Date, Day),
    (Day == saturday ; Day == sunday).

% Check if a date is a weekday.
is_weekday(Date) :-
    \+ is_weekend(Date).

% Count business days between Start and End (exclusive of End).
% Result is always non-negative when End >= Start.
business_days_between(Start, End, Count) :-
    date_diff(Start, End, days, TotalDays),
    (   TotalDays =< 0
    ->  Count = 0
    ;   count_weekdays(Start, TotalDays, 0, Count)
    ).

count_weekdays(_, 0, Acc, Acc).
count_weekdays(Current, Remaining, Acc, Count) :-
    Remaining > 0,
    (   is_weekday(Current)
    ->  NewAcc is Acc + 1
    ;   NewAcc = Acc
    ),
    date_add(Current, 1, days, NextDay),
    NewRemaining is Remaining - 1,
    count_weekdays(NextDay, NewRemaining, NewAcc, Count).

% Add N business days to a date (skipping weekends).
add_business_days(Date, 0, Date).
add_business_days(Date, N, Result) :-
    N > 0,
    date_add(Date, 1, days, NextDay),
    (   is_weekday(NextDay)
    ->  N1 is N - 1,
        add_business_days(NextDay, N1, Result)
    ;   add_business_days(NextDay, N, Result)
    ).

% Subtract N business days from a date.
subtract_business_days(Date, 0, Date).
subtract_business_days(Date, N, Result) :-
    N > 0,
    date_add(Date, -1, days, PrevDay),
    (   is_weekday(PrevDay)
    ->  N1 is N - 1,
        subtract_business_days(PrevDay, N1, Result)
    ;   subtract_business_days(PrevDay, N, Result)
    ).

% Calculate a project timeline: given a start date and a list
% of task durations (in business days), compute each milestone.
project_timeline(StartDate, TaskDurations, Milestones) :-
    compute_milestones(StartDate, TaskDurations, 1, Milestones).

compute_milestones(_, [], _, []).
compute_milestones(Current, [Duration | Rest], TaskNum, [milestone(TaskNum, EndDate) | Ms]) :-
    add_business_days(Current, Duration, EndDate),
    NextTaskNum is TaskNum + 1,
    compute_milestones(EndDate, Rest, NextTaskNum, Ms).

% Print a project schedule.
print_timeline(StartDate, TaskNames, Durations) :-
    write('Project start: '), write(StartDate), nl,
    day_of_week(StartDate, StartDay),
    write('  ('), write(StartDay), write(')'), nl,
    print_tasks(StartDate, TaskNames, Durations, 1).

print_tasks(_, [], [], _).
print_tasks(Current, [Name | Names], [Dur | Durs], N) :-
    add_business_days(Current, Dur, EndDate),
    day_of_week(EndDate, EndDay),
    write('  Task '), write(N), write(': '), write(Name), nl,
    write('    Duration: '), write(Dur), write(' business days'), nl,
    write('    Ends: '), write(EndDate), write(' ('), write(EndDay), write(')'), nl,
    N1 is N + 1,
    print_tasks(EndDate, Names, Durs, N1).

% --- Usage ---
% ?- business_days_between('2025-03-17', '2025-03-28', Count).
%    Count = 9     (Mon Mar 17 to Thu Mar 27 = 9 weekdays)
%
% ?- add_business_days('2025-03-21', 5, Result).
%    Result = '2025-03-28'    (Fri + 5 biz days = next Fri)
%
% ?- print_timeline('2025-04-01',
%        [design, implementation, testing, deployment],
%        [3, 10, 5, 2]).
%    Project start: 2025-04-01
%      (tuesday)
%      Task 1: design
%        Duration: 3 business days
%        Ends: 2025-04-04 (friday)
%      Task 2: implementation
%        Duration: 10 business days
%        Ends: 2025-04-18 (friday)
%      ...
```

---

### Example 3: Scheduling System (Conflict Detection)

A meeting scheduler that detects time conflicts and finds the next available slot.

```prolog
% ============================================================
% Meeting scheduling system
% Stores meetings, detects conflicts, and finds open slots.
% ============================================================

:- dynamic meeting/4.  % meeting(Id, Date, StartHour, DurationHours)

% Add a meeting if no conflict exists.
schedule_meeting(Id, Date, StartHour, Duration) :-
    EndHour is StartHour + Duration,
    (   has_conflict(Date, StartHour, EndHour)
    ->  write('CONFLICT: cannot schedule meeting '), write(Id), nl,
        show_conflicts(Date, StartHour, EndHour),
        fail
    ;   assert(meeting(Id, Date, StartHour, Duration)),
        format_hour(StartHour, StartStr),
        format_hour(EndHour, EndStr),
        write('Scheduled: '), write(Id), write(' on '), write(Date),
        write(' '), write(StartStr), write('-'), write(EndStr), nl
    ).

% Check if a proposed time window overlaps with any existing meeting.
has_conflict(Date, Start, End) :-
    meeting(_, Date, ExStart, ExDur),
    ExEnd is ExStart + ExDur,
    Start < ExEnd,
    End > ExStart.

% Show which meetings conflict.
show_conflicts(Date, Start, End) :-
    forall(
        (meeting(Id, Date, ExStart, ExDur),
         ExEnd is ExStart + ExDur,
         Start < ExEnd, End > ExStart),
        (format_hour(ExStart, S), ExEndH is ExStart + ExDur,
         format_hour(ExEndH, E),
         write('  Conflicts with '), write(Id),
         write(' ('), write(S), write('-'), write(E), write(')'), nl)
    ).

% Format an hour number as a readable string (e.g., 14 -> '14:00').
format_hour(H, Str) :-
    HH is truncate(H),
    MM is truncate((H - HH) * 60),
    number_codes(HH, HCodes),
    number_codes(MM, MCodes),
    atom_codes(HA, HCodes),
    atom_codes(MA, MCodes),
    atom_concat(HA, ':', T),
    (MM < 10 -> atom_concat(T, '0', T2), atom_concat(T2, MA, Str) ;
                atom_concat(T, MA, Str)).

% Find the next available slot of given duration on a date.
% Searches between MinHour and MaxHour.
find_available_slot(Date, Duration, MinHour, MaxHour, SlotStart) :-
    meetings_on_date(Date, Meetings),
    sort_meetings(Meetings, Sorted),
    find_gap(Sorted, MinHour, MaxHour, Duration, SlotStart).

meetings_on_date(Date, Meetings) :-
    findall(m(Start, Dur), meeting(_, Date, Start, Dur), Meetings).

% Simple insertion sort for meetings by start time.
sort_meetings([], []).
sort_meetings([H | T], Sorted) :-
    sort_meetings(T, SortedT),
    insert_meeting(H, SortedT, Sorted).

insert_meeting(M, [], [M]).
insert_meeting(m(S1, D1), [m(S2, D2) | Rest], [m(S1, D1), m(S2, D2) | Rest]) :-
    S1 =< S2.
insert_meeting(m(S1, D1), [m(S2, D2) | Rest], [m(S2, D2) | NewRest]) :-
    S1 > S2,
    insert_meeting(m(S1, D1), Rest, NewRest).

% Find a gap between meetings that fits the requested duration.
find_gap([], Start, MaxHour, Duration, Start) :-
    End is Start + Duration,
    End =< MaxHour.
find_gap([m(MStart, _) | _], Current, _, Duration, Current) :-
    End is Current + Duration,
    End =< MStart.
find_gap([m(MStart, MDur) | Rest], Current, MaxHour, Duration, SlotStart) :-
    MEnd is MStart + MDur,
    NewCurrent is max(Current, MEnd),
    find_gap(Rest, NewCurrent, MaxHour, Duration, SlotStart).

% List all meetings for a specific date.
show_schedule(Date) :-
    day_of_week(Date, DayName),
    write('Schedule for '), write(Date), write(' ('), write(DayName), write('):'), nl,
    findall(m(S, D, Id), meeting(Id, Date, S, D), Meetings),
    (   Meetings == []
    ->  write('  No meetings scheduled'), nl
    ;   forall(
            member(m(S, D, Id), Meetings),
            (E is S + D, format_hour(S, SS), format_hour(E, ES),
             write('  '), write(SS), write('-'), write(ES),
             write('  '), write(Id), nl)
        )
    ).

% --- Usage ---
% ?- schedule_meeting(standup, '2025-03-24', 9, 0.5).
%    Scheduled: standup on 2025-03-24 9:00-9:30
%
% ?- schedule_meeting(design_review, '2025-03-24', 10, 2).
%    Scheduled: design_review on 2025-03-24 10:00-12:00
%
% ?- schedule_meeting(lunch_talk, '2025-03-24', 11, 1).
%    CONFLICT: cannot schedule meeting lunch_talk
%      Conflicts with design_review (10:00-12:00)
%
% ?- find_available_slot('2025-03-24', 1, 9, 17, Slot).
%    Slot = 9.5   (i.e., 9:30, right after standup)
%
% ?- show_schedule('2025-03-24').
%    Schedule for 2025-03-24 (monday):
%      9:00-9:30  standup
%      10:00-12:00  design_review
```

---

### Example 4: Log Timestamp Analysis

Parse log timestamps, compute durations between events, and identify slow operations.

```prolog
% ============================================================
% Log timestamp analysis
% Parse log entries, compute operation durations, identify
% slow operations and peak activity periods.
% ============================================================

:- dynamic log_entry/4.  % log_entry(Id, Timestamp, Operation, Status)

% Parse and load a log entry.
% Timestamps are in 'yyyy-MM-dd HH:mm:ss' format.
load_log_entry(Id, TimestampStr, Operation, Status) :-
    parse_time('yyyy-MM-dd HH:mm:ss', TimestampStr, Millis),
    assert(log_entry(Id, Millis, Operation, Status)).

% Compute the duration between two log entries in seconds.
entry_duration_seconds(StartId, EndId, Seconds) :-
    log_entry(StartId, StartMs, _, _),
    log_entry(EndId, EndMs, _, _),
    Seconds is (EndMs - StartMs) / 1000.

% Find operations that took longer than a threshold (in seconds).
slow_operations(ThresholdSec, SlowOps) :-
    findall(
        slow(Op, DurSec),
        (   log_entry(StartId, StartMs, Op, start),
            log_entry(EndId, EndMs, Op, end),
            DurSec is (EndMs - StartMs) / 1000,
            DurSec > ThresholdSec
        ),
        SlowOps
    ).

% Analyze a sequence of operations and report timings.
analyze_operations :-
    findall(Op, log_entry(_, _, Op, start), Ops),
    sort(Ops, UniqueOps),
    write('=== Operation Duration Report ==='), nl,
    forall(
        member(Op, UniqueOps),
        analyze_single_op(Op)
    ).

analyze_single_op(Op) :-
    log_entry(_, StartMs, Op, start),
    log_entry(_, EndMs, Op, end),
    DurSec is (EndMs - StartMs) / 1000,
    format_time('HH:mm:ss', StartMs, StartStr),
    format_time('HH:mm:ss', EndMs, EndStr),
    write('  '), write(Op), write(': '),
    write(StartStr), write(' -> '), write(EndStr),
    write(' ('), write(DurSec), write('s)'),
    (DurSec > 5 -> write(' [SLOW]') ; true),
    nl.

% Determine the peak hour: the hour with the most log events.
peak_hour(Hour, Count) :-
    findall(H,
        (log_entry(_, Ms, _, _), format_time('HH', Ms, HStr),
         atom_codes(HStr, Codes), number_codes(H, Codes)),
        Hours),
    count_occurrences(Hours, Counts),
    max_count(Counts, Hour, Count).

count_occurrences([], []).
count_occurrences([H | T], Result) :-
    count_occurrences(T, Rest),
    update_count(H, Rest, Result).

update_count(H, [], [H-1]).
update_count(H, [H-C | Rest], [H-C1 | Rest]) :- C1 is C + 1.
update_count(H, [Other | Rest], [Other | Updated]) :-
    Other = K-_, K \== H,
    update_count(H, Rest, Updated).

max_count([H-C], H, C).
max_count([H-C | Rest], MaxH, MaxC) :-
    max_count(Rest, RestH, RestC),
    (C >= RestC -> MaxH = H, MaxC = C ; MaxH = RestH, MaxC = RestC).

% Compute total elapsed time of a batch of log entries.
total_elapsed(FirstId, LastId, ElapsedStr) :-
    log_entry(FirstId, StartMs, _, _),
    log_entry(LastId, EndMs, _, _),
    TotalSec is (EndMs - StartMs) / 1000,
    Hours is truncate(TotalSec / 3600),
    Mins is truncate((TotalSec - Hours * 3600) / 60),
    Secs is truncate(TotalSec - Hours * 3600 - Mins * 60),
    number_codes(Hours, HC), atom_codes(HA, HC),
    number_codes(Mins, MC), atom_codes(MA, MC),
    number_codes(Secs, SC), atom_codes(SA, SC),
    atom_concat(HA, 'h ', T1),
    atom_concat(T1, MA, T2),
    atom_concat(T2, 'm ', T3),
    atom_concat(T3, SA, T4),
    atom_concat(T4, 's', ElapsedStr).

% --- Usage ---
% ?- load_log_entry(1, '2025-03-21 10:00:00', db_query, start).
% ?- load_log_entry(2, '2025-03-21 10:00:08', db_query, end).
% ?- load_log_entry(3, '2025-03-21 10:00:10', render, start).
% ?- load_log_entry(4, '2025-03-21 10:00:12', render, end).
%
% ?- analyze_operations.
%    === Operation Duration Report ===
%      db_query: 10:00:00 -> 10:00:08 (8.0s) [SLOW]
%      render: 10:00:10 -> 10:00:12 (2.0s)
%
% ?- slow_operations(5, SlowOps).
%    SlowOps = [slow(db_query, 8.0)]
%
% ?- total_elapsed(1, 4, Elapsed).
%    Elapsed = '0h 0m 12s'
```

---

### Example 5: Deadline Tracker with Days Remaining

Track project deadlines, compute days remaining, send alerts for overdue or upcoming items, and generate a dashboard.

```prolog
% ============================================================
% Deadline tracker
% Manages deadlines, computes urgency, and generates alerts.
% ============================================================

:- dynamic deadline/4.  % deadline(Id, Title, DueDate, Priority)

% Priority levels: critical, high, medium, low.
priority_weight(critical, 4).
priority_weight(high, 3).
priority_weight(medium, 2).
priority_weight(low, 1).

% Add a new deadline.
add_deadline(Id, Title, DueDate, Priority) :-
    assert(deadline(Id, Title, DueDate, Priority)),
    write('Added: '), write(Title), write(' (due '), write(DueDate), write(')'), nl.

% Compute days remaining (negative = overdue).
days_remaining(DueDate, Remaining) :-
    today(Today),
    date_diff(Today, DueDate, days, Remaining).

% Classify the urgency of a deadline.
urgency(DueDate, overdue) :-
    days_remaining(DueDate, R), R < 0.
urgency(DueDate, due_today) :-
    days_remaining(DueDate, 0).
urgency(DueDate, urgent) :-
    days_remaining(DueDate, R), R > 0, R =< 3.
urgency(DueDate, upcoming) :-
    days_remaining(DueDate, R), R > 3, R =< 7.
urgency(DueDate, on_track) :-
    days_remaining(DueDate, R), R > 7.

% Get all deadlines sorted by urgency score (lower = more urgent).
urgency_score(DueDate, Priority, Score) :-
    days_remaining(DueDate, Days),
    priority_weight(Priority, W),
    Score is Days - W * 3.  % Higher priority items rank more urgently.

% Generate the full dashboard.
show_dashboard :-
    today(Today),
    day_of_week(Today, DayName),
    write('========================================'), nl,
    write('  DEADLINE DASHBOARD'), nl,
    write('  '), write(Today), write(' ('), write(DayName), write(')'), nl,
    write('========================================'), nl, nl,
    show_section_header('OVERDUE'),
    show_deadlines_by_urgency(overdue),
    show_section_header('DUE TODAY'),
    show_deadlines_by_urgency(due_today),
    show_section_header('URGENT (1-3 days)'),
    show_deadlines_by_urgency(urgent),
    show_section_header('UPCOMING (4-7 days)'),
    show_deadlines_by_urgency(upcoming),
    show_section_header('ON TRACK (7+ days)'),
    show_deadlines_by_urgency(on_track), nl,
    summary_stats.

show_section_header(Title) :-
    write('--- '), write(Title), write(' ---'), nl.

show_deadlines_by_urgency(UrgencyLevel) :-
    findall(
        dl(Id, Title, DueDate, Priority),
        (deadline(Id, Title, DueDate, Priority), urgency(DueDate, UrgencyLevel)),
        Deadlines
    ),
    (   Deadlines == []
    ->  write('  (none)'), nl
    ;   forall(
            member(dl(_, Title, DueDate, Priority), Deadlines),
            show_deadline_line(Title, DueDate, Priority)
        )
    ).

show_deadline_line(Title, DueDate, Priority) :-
    days_remaining(DueDate, Days),
    write('  ['), write(Priority), write('] '),
    write(Title), write(' - '),
    (   Days < 0
    ->  AbsDays is abs(Days),
        write(AbsDays), write(' days OVERDUE')
    ;   Days =:= 0
    ->  write('DUE TODAY')
    ;   write(Days), write(' days left')
    ), nl.

% Summary statistics.
summary_stats :-
    findall(D, deadline(_, _, D, _), All),
    length(All, Total),
    findall(D, (deadline(_, _, D, _), urgency(D, overdue)), Overdue),
    length(Overdue, OverdueCount),
    findall(D, (deadline(_, _, D, _), days_remaining(D, R), R >= 0, R =< 3), UrgentList),
    length(UrgentList, UrgentCount),
    write('Total: '), write(Total), write(' deadlines | '),
    write('Overdue: '), write(OverdueCount), write(' | '),
    write('Due within 3 days: '), write(UrgentCount), nl.

% Utility: find the next deadline chronologically.
next_deadline(Id, Title, DueDate) :-
    findall(dl(D, I, T),
        (deadline(I, T, D, _), days_remaining(D, R), R >= 0),
        Upcoming),
    sort(Upcoming, [dl(DueDate, Id, Title) | _]).

% --- Usage ---
% ?- add_deadline(1, 'Q1 Report', '2025-03-20', critical).
% ?- add_deadline(2, 'Code Review', '2025-03-22', high).
% ?- add_deadline(3, 'Design Doc', '2025-03-25', medium).
% ?- add_deadline(4, 'Sprint Demo', '2025-03-28', high).
% ?- add_deadline(5, 'Tax Filing', '2025-04-15', critical).
%
% ?- show_dashboard.
%    ========================================
%      DEADLINE DASHBOARD
%      2025-03-21 (friday)
%    ========================================
%
%    --- OVERDUE ---
%      [critical] Q1 Report - 1 days OVERDUE
%    --- DUE TODAY ---
%      (none)
%    --- URGENT (1-3 days) ---
%      [high] Code Review - 1 days left
%    --- UPCOMING (4-7 days) ---
%      [medium] Design Doc - 4 days left
%      [high] Sprint Demo - 7 days left
%    --- ON TRACK (7+ days) ---
%      [critical] Tax Filing - 25 days left
%
%    Total: 5 deadlines | Overdue: 1 | Due within 3 days: 2
%
% ?- next_deadline(Id, Title, Date).
%    Id = 2, Title = 'Code Review', Date = '2025-03-22'
```
