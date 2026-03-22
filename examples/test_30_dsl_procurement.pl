% ===================================================================
% TEST 30: DSL for Public Procurement Management (Gare d'Appalto)
% ===================================================================
% A complete DSL compiler and interpreter in Prolog for managing:
%   - Tender types (open, restricted, negotiated, direct_award)
%   - Participants (companies with qualifications)
%   - Bids/Offers (technical + economic scores)
%   - Tender lifecycle (draft -> published -> evaluation -> awarded -> closed)
%   - Award criteria (lowest_price, best_value, quality_only)
%   - Automatic adjudication with ranking and validation

% =====================================================
% SECTION 1: DSL COMPILER - Parse DSL commands to AST
% =====================================================

% 1.1 DSL Command syntax (as Prolog terms representing the AST):
%
%   create_tender(Id, Type, Criterion, Budget)
%   publish_tender(Id)
%   register_participant(Name, Qualifications)
%   submit_bid(TenderId, Participant, TechScore, EconScore, Amount)
%   evaluate_tender(Id)
%   award_tender(Id)
%   close_tender(Id)
%   query_status(Id)
%   query_ranking(Id)
%   query_winner(Id)

% 1.2 DSL Compiler: validate and compile a program (list of commands)
compile_program(Commands, CompiledProgram) :-
    validate_commands(Commands, []),
    CompiledProgram = program(Commands).

% 1.3 Command validation
validate_commands([], _).
validate_commands([Cmd|Rest], Seen) :-
    validate_command(Cmd, Seen),
    update_seen(Cmd, Seen, NewSeen),
    validate_commands(Rest, NewSeen).

validate_command(create_tender(Id, Type, Criterion, Budget), Seen) :-
    atom(Id),
    valid_tender_type(Type),
    valid_criterion(Criterion),
    number(Budget), Budget > 0,
    \+ member(tender(Id), Seen).

validate_command(publish_tender(Id), Seen) :-
    atom(Id),
    member(tender(Id), Seen).

validate_command(register_participant(Name, Quals), _) :-
    atom(Name),
    is_list(Quals),
    Quals \= [].

validate_command(submit_bid(TenderId, Participant, TechScore, EconScore, Amount), Seen) :-
    atom(TenderId), atom(Participant),
    member(tender(TenderId), Seen),
    member(participant(Participant), Seen),
    number(TechScore), TechScore >= 0, TechScore =< 100,
    number(EconScore), EconScore >= 0, EconScore =< 100,
    number(Amount), Amount > 0.

validate_command(evaluate_tender(Id), Seen) :-
    atom(Id), member(tender(Id), Seen).

validate_command(award_tender(Id), Seen) :-
    atom(Id), member(tender(Id), Seen).

validate_command(close_tender(Id), Seen) :-
    atom(Id), member(tender(Id), Seen).

validate_command(query_status(Id), Seen) :-
    atom(Id), member(tender(Id), Seen).

validate_command(query_ranking(Id), Seen) :-
    atom(Id), member(tender(Id), Seen).

validate_command(query_winner(Id), Seen) :-
    atom(Id), member(tender(Id), Seen).

% Track known entities for validation
update_seen(create_tender(Id, _, _, _), Seen, [tender(Id)|Seen]).
update_seen(register_participant(Name, _), Seen, [participant(Name)|Seen]).
update_seen(Cmd, Seen, Seen) :-
    Cmd \= create_tender(_, _, _, _),
    Cmd \= register_participant(_, _).

% 1.4 Valid tender types
valid_tender_type(open).
valid_tender_type(restricted).
valid_tender_type(negotiated).
valid_tender_type(direct_award).

% 1.5 Valid award criteria
valid_criterion(lowest_price).
valid_criterion(best_value).
valid_criterion(quality_only).

% =====================================================
% SECTION 2: DSL INTERPRETER - Execute compiled programs
% =====================================================

% 2.1 State representation
% State = state(Tenders, Participants, Bids, Log)
% Tender = tender(Id, Type, Criterion, Budget, Status)
% Participant = participant(Name, Qualifications)
% Bid = bid(TenderId, Participant, TechScore, EconScore, Amount)
% Log = list of log(Action, Details) entries

initial_state(state([], [], [], [])).

% 2.2 Main interpreter: execute a compiled program
interpret(program(Commands), FinalState) :-
    initial_state(S0),
    execute_commands(Commands, S0, FinalState).

execute_commands([], State, State).
execute_commands([Cmd|Rest], State0, FinalState) :-
    execute_command(Cmd, State0, State1),
    execute_commands(Rest, State1, FinalState).

% 2.3 Command executors

% CREATE TENDER
execute_command(create_tender(Id, Type, Criterion, Budget),
    state(Tenders, Parts, Bids, Log),
    state([tender(Id, Type, Criterion, Budget, draft)|Tenders], Parts, Bids,
          [log(create_tender, Id)|Log])).

% PUBLISH TENDER (draft -> published)
execute_command(publish_tender(Id),
    state(Tenders0, Parts, Bids, Log),
    state(Tenders1, Parts, Bids, [log(publish_tender, Id)|Log])) :-
    transition_status(Id, draft, published, Tenders0, Tenders1).

% REGISTER PARTICIPANT
execute_command(register_participant(Name, Quals),
    state(Tenders, Parts, Bids, Log),
    state(Tenders, [participant(Name, Quals)|Parts], Bids,
          [log(register_participant, Name)|Log])).

% SUBMIT BID (tender must be published)
execute_command(submit_bid(TenderId, Participant, TechScore, EconScore, Amount),
    state(Tenders, Parts, Bids, Log),
    state(Tenders, Parts,
          [bid(TenderId, Participant, TechScore, EconScore, Amount)|Bids],
          [log(submit_bid, bid(TenderId, Participant))|Log])) :-
    member(tender(TenderId, _, _, _, published), Tenders),
    member(participant(Participant, _), Parts).

% EVALUATE TENDER (published -> evaluation)
execute_command(evaluate_tender(Id),
    state(Tenders0, Parts, Bids, Log),
    state(Tenders1, Parts, Bids, [log(evaluate_tender, Id)|Log])) :-
    transition_status(Id, published, evaluation, Tenders0, Tenders1).

% AWARD TENDER (evaluation -> awarded)
execute_command(award_tender(Id),
    state(Tenders0, Parts, Bids, Log),
    state(Tenders1, Parts, Bids, [log(award_tender, Id)|Log])) :-
    transition_status(Id, evaluation, awarded, Tenders0, Tenders1),
    % Must have at least one bid
    member(bid(Id, _, _, _, _), Bids).

% CLOSE TENDER (awarded -> closed)
execute_command(close_tender(Id),
    state(Tenders0, Parts, Bids, Log),
    state(Tenders1, Parts, Bids, [log(close_tender, Id)|Log])) :-
    transition_status(Id, awarded, closed, Tenders0, Tenders1).

% QUERY STATUS
execute_command(query_status(Id),
    state(Tenders, Parts, Bids, Log),
    state(Tenders, Parts, Bids, [log(query_status, status(Id, Status))|Log])) :-
    member(tender(Id, _, _, _, Status), Tenders).

% QUERY RANKING
execute_command(query_ranking(Id),
    state(Tenders, Parts, Bids, Log),
    state(Tenders, Parts, Bids, [log(query_ranking, ranking(Id, Ranked))|Log])) :-
    member(tender(Id, _, Criterion, _, _), Tenders),
    compute_ranking(Id, Criterion, Bids, Ranked).

% QUERY WINNER
execute_command(query_winner(Id),
    state(Tenders, Parts, Bids, Log),
    state(Tenders, Parts, Bids, [log(query_winner, winner(Id, Winner))|Log])) :-
    member(tender(Id, _, Criterion, Budget, _), Tenders),
    compute_ranking(Id, Criterion, Bids, Ranked),
    Ranked = [ranked(Winner, _, _)|_],
    % Winner's bid must be within budget
    member(bid(Id, Winner, _, _, Amt), Bids),
    Amt =< Budget.

% 2.4 State transition for tender status
transition_status(Id, FromStatus, ToStatus,
    [tender(Id, Type, Crit, Budget, FromStatus)|Rest],
    [tender(Id, Type, Crit, Budget, ToStatus)|Rest]).
transition_status(Id, FromStatus, ToStatus,
    [Other|Rest0], [Other|Rest1]) :-
    Other \= tender(Id, _, _, _, _),
    transition_status(Id, FromStatus, ToStatus, Rest0, Rest1).

% 2.5 Valid status transitions
valid_transition(draft, published).
valid_transition(published, evaluation).
valid_transition(evaluation, awarded).
valid_transition(awarded, closed).

% =====================================================
% SECTION 3: SCORING AND RANKING ENGINE
% =====================================================

% 3.1 Compute ranking based on criterion
compute_ranking(TenderId, Criterion, AllBids, Ranked) :-
    findall(bid(TenderId, P, T, E, A),
            member(bid(TenderId, P, T, E, A), AllBids),
            TenderBids),
    score_bids(Criterion, TenderBids, ScoredBids),
    sort_ranked(ScoredBids, Ranked).

% 3.2 Score each bid based on criterion
score_bids(_, [], []).
score_bids(Criterion, [bid(_, P, Tech, Econ, Amt)|Rest],
           [ranked(P, Score, Amt)|ScoredRest]) :-
    compute_score(Criterion, Tech, Econ, Amt, Score),
    score_bids(Criterion, Rest, ScoredRest).

% 3.3 Scoring formulas per criterion type
% lowest_price: score = 100 - normalized_amount (lower is better)
compute_score(lowest_price, _, _, Amount, Score) :-
    Score is 10000 / Amount.

% best_value: weighted 60% technical + 40% economic
compute_score(best_value, TechScore, EconScore, _, Score) :-
    Score is TechScore * 60 / 100 + EconScore * 40 / 100.

% quality_only: 100% technical score
compute_score(quality_only, TechScore, _, _, TechScore).

% 3.4 Sort ranked list by descending score (insertion sort)
sort_ranked([], []).
sort_ranked([H|T], Sorted) :-
    sort_ranked(T, SortedT),
    insert_ranked(H, SortedT, Sorted).

insert_ranked(X, [], [X]).
insert_ranked(ranked(P1, S1, A1), [ranked(P2, S2, A2)|T],
              [ranked(P1, S1, A1), ranked(P2, S2, A2)|T]) :-
    S1 >= S2.
insert_ranked(ranked(P1, S1, A1), [ranked(P2, S2, A2)|T],
              [ranked(P2, S2, A2)|Rest]) :-
    S1 < S2,
    insert_ranked(ranked(P1, S1, A1), T, Rest).

% =====================================================
% SECTION 4: BUSINESS RULES AND VALIDATION
% =====================================================

% 4.1 Check if a participant is qualified for a tender type
qualified_for(participant(_, Quals), open) :-
    member(registered, Quals).
qualified_for(participant(_, Quals), restricted) :-
    member(registered, Quals),
    member(certified, Quals).
qualified_for(participant(_, Quals), negotiated) :-
    member(registered, Quals),
    member(certified, Quals),
    member(invited, Quals).
qualified_for(participant(_, Quals), direct_award) :-
    member(registered, Quals),
    member(sole_provider, Quals).

% 4.2 Validate bid against tender rules
valid_bid(bid(TenderId, Participant, Tech, Econ, Amount),
          state(Tenders, Parts, _, _)) :-
    member(tender(TenderId, Type, _, Budget, published), Tenders),
    member(participant(Participant, Quals), Parts),
    qualified_for(participant(Participant, Quals), Type),
    Tech >= 0, Tech =< 100,
    Econ >= 0, Econ =< 100,
    Amount > 0, Amount =< Budget.

% 4.3 Check for anomalously low bids (below 50% of budget)
anomalous_bid(bid(TenderId, Participant, _, _, Amount),
              state(Tenders, _, _, _)) :-
    member(tender(TenderId, _, _, Budget, _), Tenders),
    Threshold is Budget * 50 / 100,
    Amount < Threshold,
    Participant \= none.

% 4.4 Minimum number of bids per tender type
min_bids(open, 1).
min_bids(restricted, 3).
min_bids(negotiated, 1).
min_bids(direct_award, 1).

% Check if tender has enough bids
enough_bids(TenderId, Type, Bids) :-
    min_bids(Type, Min),
    findall(P, member(bid(TenderId, P, _, _, _), Bids), Ps),
    length(Ps, N),
    N >= Min.

% 4.5 Budget compliance check
within_budget(bid(TenderId, _, _, _, Amount), Tenders) :-
    member(tender(TenderId, _, _, Budget, _), Tenders),
    Amount =< Budget.

% =====================================================
% SECTION 5: QUERY UTILITIES
% =====================================================

% 5.1 Get tender status from state
get_status(Id, state(Tenders, _, _, _), Status) :-
    member(tender(Id, _, _, _, Status), Tenders).

% 5.2 Get all bids for a tender
get_bids(TenderId, state(_, _, Bids, _), TenderBids) :-
    findall(bid(TenderId, P, T, E, A),
            member(bid(TenderId, P, T, E, A), Bids),
            TenderBids).

% 5.3 Get winner for a tender
get_winner(TenderId, State, Winner, Score) :-
    State = state(Tenders, _, Bids, _),
    member(tender(TenderId, _, Criterion, Budget, _), Tenders),
    compute_ranking(TenderId, Criterion, Bids, [ranked(Winner, Score, Amt)|_]),
    Amt =< Budget.

% 5.4 Count bids per tender
count_bids(TenderId, state(_, _, Bids, _), N) :-
    findall(P, member(bid(TenderId, P, _, _, _), Bids), Ps),
    length(Ps, N).

% 5.5 Get log entries
get_log(state(_, _, _, Log), Log).

% 5.6 Filter log by action
filter_log(_, [], []).
filter_log(Action, [log(Action, Details)|Rest], [log(Action, Details)|Filtered]) :-
    filter_log(Action, Rest, Filtered).
filter_log(Action, [log(Other, _)|Rest], Filtered) :-
    Other \= Action,
    filter_log(Action, Rest, Filtered).

% =====================================================
% SECTION 6: COMPLETE SCENARIO PROGRAMS
% =====================================================

% 6.1 Scenario: Open tender with lowest price criterion
scenario_open_lowest(FinalState) :-
    compile_program([
        create_tender(tender_001, open, lowest_price, 100000),
        register_participant(acme_corp, [registered, certified]),
        register_participant(beta_inc, [registered]),
        register_participant(gamma_ltd, [registered, certified]),
        publish_tender(tender_001),
        submit_bid(tender_001, acme_corp, 80, 70, 85000),
        submit_bid(tender_001, beta_inc, 60, 80, 72000),
        submit_bid(tender_001, gamma_ltd, 90, 75, 91000),
        evaluate_tender(tender_001),
        award_tender(tender_001),
        query_winner(tender_001)
    ], Program),
    interpret(Program, FinalState).

% 6.2 Scenario: Best value tender
scenario_best_value(FinalState) :-
    compile_program([
        create_tender(tender_002, open, best_value, 200000),
        register_participant(alpha_spa, [registered, certified]),
        register_participant(omega_srl, [registered]),
        publish_tender(tender_002),
        submit_bid(tender_002, alpha_spa, 95, 60, 180000),
        submit_bid(tender_002, omega_srl, 70, 90, 150000),
        evaluate_tender(tender_002),
        award_tender(tender_002),
        query_winner(tender_002)
    ], Program),
    interpret(Program, FinalState).

% 6.3 Scenario: Quality only tender
scenario_quality_only(FinalState) :-
    compile_program([
        create_tender(tender_003, open, quality_only, 500000),
        register_participant(tech_corp, [registered, certified]),
        register_participant(innov_inc, [registered]),
        publish_tender(tender_003),
        submit_bid(tender_003, tech_corp, 85, 50, 450000),
        submit_bid(tender_003, innov_inc, 92, 70, 480000),
        evaluate_tender(tender_003),
        award_tender(tender_003),
        query_winner(tender_003)
    ], Program),
    interpret(Program, FinalState).

% 6.4 Scenario: Full lifecycle (create -> close)
scenario_full_lifecycle(FinalState) :-
    compile_program([
        create_tender(tender_004, open, lowest_price, 50000),
        register_participant(quick_build, [registered]),
        publish_tender(tender_004),
        submit_bid(tender_004, quick_build, 70, 70, 45000),
        evaluate_tender(tender_004),
        award_tender(tender_004),
        close_tender(tender_004),
        query_status(tender_004)
    ], Program),
    interpret(Program, FinalState).

% 6.5 Scenario: Multiple tenders in parallel
scenario_multi_tender(FinalState) :-
    compile_program([
        create_tender(t_a, open, lowest_price, 100000),
        create_tender(t_b, open, best_value, 200000),
        register_participant(company_x, [registered, certified]),
        register_participant(company_y, [registered]),
        publish_tender(t_a),
        publish_tender(t_b),
        submit_bid(t_a, company_x, 80, 70, 90000),
        submit_bid(t_a, company_y, 75, 80, 80000),
        submit_bid(t_b, company_x, 90, 85, 180000),
        submit_bid(t_b, company_y, 85, 90, 170000),
        evaluate_tender(t_a),
        evaluate_tender(t_b),
        award_tender(t_a),
        award_tender(t_b),
        query_winner(t_a),
        query_winner(t_b)
    ], Program),
    interpret(Program, FinalState).

% =====================================================
% SECTION 7: ADVANCED ANALYSIS PREDICATES
% =====================================================

% 7.1 Find all qualified participants for a tender
qualified_participants(TenderId, State, Qualified) :-
    State = state(Tenders, Parts, _, _),
    member(tender(TenderId, Type, _, _, _), Tenders),
    findall(Name,
            (member(participant(Name, Quals), Parts),
             qualified_for(participant(Name, Quals), Type)),
            Qualified).

% 7.2 Compute average bid amount for a tender
avg_bid_amount(TenderId, State, Avg) :-
    State = state(_, _, Bids, _),
    findall(A, member(bid(TenderId, _, _, _, A), Bids), Amounts),
    Amounts \= [],
    sum_list_acc(Amounts, 0, Total),
    length(Amounts, N),
    Avg is Total / N.

sum_list_acc([], Acc, Acc).
sum_list_acc([H|T], Acc, Total) :-
    Acc1 is Acc + H,
    sum_list_acc(T, Acc1, Total).

% 7.3 Find anomalous bids in a tender
find_anomalous(TenderId, State, AnomalousBids) :-
    State = state(Tenders, _, Bids, _),
    findall(bid(TenderId, P, T, E, A),
            (member(bid(TenderId, P, T, E, A), Bids),
             anomalous_bid(bid(TenderId, P, T, E, A), State)),
            AnomalousBids).

% 7.4 Compare two participants across all tenders
compare_participants(P1, P2, State, P1Wins, P2Wins) :-
    State = state(Tenders, _, Bids, _),
    findall(TId,
            (member(tender(TId, _, Crit, _, _), Tenders),
             compute_ranking(TId, Crit, Bids, Ranked),
             better_rank(P1, P2, Ranked)),
            P1WinList),
    findall(TId,
            (member(tender(TId, _, Crit, _, _), Tenders),
             compute_ranking(TId, Crit, Bids, Ranked),
             better_rank(P2, P1, Ranked)),
            P2WinList),
    length(P1WinList, P1Wins),
    length(P2WinList, P2Wins).

better_rank(P1, P2, Ranked) :-
    nth_rank(P1, Ranked, R1),
    nth_rank(P2, Ranked, R2),
    R1 < R2.

nth_rank(P, [ranked(P, _, _)|_], 1) :- !.
nth_rank(P, [_|Rest], N) :-
    nth_rank(P, Rest, N1),
    N is N1 + 1.

% 7.5 Tender statistics summary
tender_summary(TenderId, State, summary(TenderId, Type, Status, NumBids, AvgAmt)) :-
    get_status(TenderId, State, Status),
    State = state(Tenders, _, Bids, _),
    member(tender(TenderId, Type, _, _, _), Tenders),
    count_bids(TenderId, State, NumBids),
    (NumBids > 0 ->
        avg_bid_amount(TenderId, State, AvgAmt)
    ;   AvgAmt = 0
    ).
