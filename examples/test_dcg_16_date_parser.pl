% DCG Test 16: Date and Time Parser
% Tests: Date format parsing, validation

% Basic digits
digit(D) --> [C], { C >= 48, C =< 57, D is C - 48 }.

% Two digit number
two_digits(N) --> digit(D1), digit(D2), { N is D1 * 10 + D2 }.

% Four digit number  
four_digits(N) --> digit(D1), digit(D2), digit(D3), digit(D4),
    { N is D1 * 1000 + D2 * 100 + D3 * 10 + D4 }.

% Date separators
slash --> [47].     % /
dash --> [45].      % -
dot --> [46].       % .
colon --> [58].     % :
space --> [32].     % space

% Month names
month_name(1) --> [106,97,110].         % jan
month_name(2) --> [102,101,98].         % feb  
month_name(3) --> [109,97,114].         % mar
month_name(4) --> [97,112,114].         % apr
month_name(5) --> [109,97,121].         % may
month_name(6) --> [106,117,110].        % jun
month_name(7) --> [106,117,108].        % jul
month_name(8) --> [97,117,103].         % aug
month_name(9) --> [115,101,112].        % sep
month_name(10) --> [111,99,116].        % oct
month_name(11) --> [110,111,118].       % nov
month_name(12) --> [100,101,99].        % dec

% Date formats
date_mdy(date(M,D,Y)) --> two_digits(M), slash, two_digits(D), slash, four_digits(Y).
date_dmy(date(M,D,Y)) --> two_digits(D), slash, two_digits(M), slash, four_digits(Y).
date_ymd(date(M,D,Y)) --> four_digits(Y), dash, two_digits(M), dash, two_digits(D).

date_text(date(M,D,Y)) --> 
    month_name(M), space, two_digits(D), space, four_digits(Y).

% Time formats  
time_hm(time(H,M)) --> two_digits(H), colon, two_digits(M).
time_hms(time(H,M,S)) --> two_digits(H), colon, two_digits(M), colon, two_digits(S).

% Combined date-time
datetime(datetime(D,T)) --> date_ymd(D), space, time_hms(T).

% Date validation
valid_date(date(M,D,Y)) :-
    Y >= 1900, Y =< 2100,
    M >= 1, M =< 12,
    D >= 1,
    days_in_month(M, Y, MaxDays),
    D =< MaxDays.

days_in_month(2, Y, 29) :- leap_year(Y), !.
days_in_month(2, _, 28) :- !.
days_in_month(M, _, 31) :- member(M, [1,3,5,7,8,10,12]), !.
days_in_month(M, _, 30) :- member(M, [4,6,9,11]).

leap_year(Y) :- 
    (Y mod 400 =:= 0 ; (Y mod 4 =:= 0, Y mod 100 =\= 0)).

% Time validation
valid_time(time(H,M)) :- H >= 0, H =< 23, M >= 0, M =< 59.
valid_time(time(H,M,S)) :- H >= 0, H =< 23, M >= 0, M =< 59, S >= 0, S =< 59.

% Parse and validate
parse_date(Input, Date) :-
    phrase(date_ymd(Date), Input),
    valid_date(Date).

parse_time(Input, Time) :-
    phrase(time_hms(Time), Input),
    valid_time(Time).

% Test queries:
% ?- phrase(date_mdy(D), [49,50,47,50,53,47,50,48,50,51]).    % Expected: D = date(12,25,2023)
% ?- phrase(time_hms(T), [49,52,58,51,48,58,52,53]).          % Expected: T = time(14,30,45)
% ?- parse_date([50,48,50,51,45,49,50,45,50,53], D).          % Expected: D = date(12,25,2023)