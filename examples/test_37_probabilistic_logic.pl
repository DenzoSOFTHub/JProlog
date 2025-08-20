% ===================================================================
% TEST 37: Probabilistic Logic Programming
% ===================================================================
% Tests: Probabilistic facts, uncertain reasoning, Bayesian networks

% Probabilistic facts with uncertainty
probabilistic_fact(rain, 0.3).
probabilistic_fact(sprinkler, 0.1).
probabilistic_fact(traffic_jam, 0.2).

% Conditional probabilities
conditional_prob(wet_grass, [rain], 0.8).
conditional_prob(wet_grass, [sprinkler], 0.9).
conditional_prob(wet_grass, [rain, sprinkler], 0.95).
conditional_prob(late_arrival, [traffic_jam], 0.7).
conditional_prob(late_arrival, [rain, traffic_jam], 0.9).

% Probabilistic inference
prob(Fact, Probability) :-
    probabilistic_fact(Fact, Probability).

prob(Fact, Probability) :-
    conditional_prob(Fact, Conditions, CondProb),
    all_conditions_hold(Conditions, ConditionsProb),
    Probability is CondProb * ConditionsProb.

all_conditions_hold([], 1.0).
all_conditions_hold([Condition|Rest], TotalProb) :-
    prob(Condition, CondProb),
    all_conditions_hold(Rest, RestProb),
    TotalProb is CondProb * RestProb.

% Bayesian network inference
bayes_network_inference(Query, Evidence, Probability) :-
    findall(P, (
        possible_world(World),
        satisfies_evidence(World, Evidence),
        satisfies_query(World, Query),
        world_probability(World, P)
    ), Probs),
    sum_list(Probs, QueryAndEvidenceProb),
    
    findall(P, (
        possible_world(World),
        satisfies_evidence(World, Evidence),
        world_probability(World, P)
    ), EvidenceProbs),
    sum_list(EvidenceProbs, EvidenceProb),
    
    (   EvidenceProb > 0
    ->  Probability is QueryAndEvidenceProb / EvidenceProb
    ;   Probability = 0
    ).

possible_world(World) :-
    World = [rain-R, sprinkler-S, wet_grass-W],
    member(R, [true, false]),
    member(S, [true, false]),
    member(W, [true, false]).

satisfies_evidence(World, Evidence) :-
    forall(member(Fact-Value, Evidence),
           member(Fact-Value, World)).

satisfies_query(World, Query) :-
    forall(member(Fact-Value, Query),
           member(Fact-Value, World)).

world_probability(World, Probability) :-
    findall(P, (
        member(Fact-Value, World),
        fact_probability_in_world(Fact, Value, World, P)
    ), Probs),
    product_list(Probs, Probability).

fact_probability_in_world(Fact, true, World, Prob) :-
    probabilistic_fact(Fact, Prob).
fact_probability_in_world(Fact, false, World, Prob) :-
    probabilistic_fact(Fact, TrueProb),
    Prob is 1 - TrueProb.

product_list([], 1).
product_list([H|T], Product) :-
    product_list(T, RestProduct),
    Product is H * RestProduct.

% Monte Carlo sampling
monte_carlo_estimate(Query, Samples, Probability) :-
    monte_carlo_samples(Samples, Query, Successes, Total),
    Probability is Successes / Total.

monte_carlo_samples(0, _, 0, 0).
monte_carlo_samples(N, Query, Successes, Total) :-
    N > 0,
    N1 is N - 1,
    monte_carlo_samples(N1, Query, RestSuccesses, RestTotal),
    sample_world(World),
    Total is RestTotal + 1,
    (   satisfies_query(World, Query)
    ->  Successes is RestSuccesses + 1
    ;   Successes = RestSuccesses
    ).

sample_world(World) :-
    sample_fact(rain, Rain),
    sample_fact(sprinkler, Sprinkler),
    sample_dependent_fact(wet_grass, [rain-Rain, sprinkler-Sprinkler], WetGrass),
    World = [rain-Rain, sprinkler-Sprinkler, wet_grass-WetGrass].

sample_fact(Fact, Value) :-
    probabilistic_fact(Fact, Prob),
    random(R),
    (   R < Prob
    ->  Value = true
    ;   Value = false
    ).

sample_dependent_fact(Fact, Conditions, Value) :-
    extract_true_conditions(Conditions, TrueConditions),
    (   conditional_prob(Fact, TrueConditions, Prob)
    ->  true
    ;   Prob = 0.1  % Default low probability
    ),
    random(R),
    (   R < Prob
    ->  Value = true
    ;   Value = false
    ).

extract_true_conditions([], []).
extract_true_conditions([Cond-true|Rest], [Cond|TrueRest]) :-
    extract_true_conditions(Rest, TrueRest).
extract_true_conditions([_-false|Rest], TrueRest) :-
    extract_true_conditions(Rest, TrueRest).

% Fuzzy logic operations
fuzzy_and(A, B, Result) :-
    Result is min(A, B).

fuzzy_or(A, B, Result) :-
    Result is max(A, B).

fuzzy_not(A, Result) :-
    Result is 1 - A.

% Linguistic variables
linguistic_var(temperature, [cold-0.2, warm-0.5, hot-0.3]).
linguistic_var(humidity, [low-0.1, medium-0.6, high-0.3]).

fuzzy_rule(comfortable, Comfort) :-
    linguistic_var(temperature, TempValues),
    linguistic_var(humidity, HumidValues),
    member(warm-TempDegree, TempValues),
    member(medium-HumidDegree, HumidValues),
    fuzzy_and(TempDegree, HumidDegree, Comfort).

% Probabilistic logic programming tests
test_probabilistic_inference :-
    prob(rain, RainProb),
    format('Probability of rain: ~2f~n', [RainProb]),
    prob(wet_grass, WetGrassProb),
    format('Probability of wet grass: ~2f~n', [WetGrassProb]).

test_bayesian_inference :-
    bayes_network_inference([wet_grass-true], [rain-true], Prob),
    format('P(wet_grass=true | rain=true) = ~2f~n', [Prob]).

test_monte_carlo :-
    monte_carlo_estimate([wet_grass-true], 1000, Prob),
    format('Monte Carlo estimate of wet grass: ~2f~n', [Prob]).

test_fuzzy_logic :-
    fuzzy_rule(comfortable, Comfort),
    format('Fuzzy comfort level: ~2f~n', [Comfort]).

% Test queries:
% ?- test_probabilistic_inference.
% ?- test_bayesian_inference.
% ?- test_monte_carlo.
% ?- test_fuzzy_logic.
% ?- prob(wet_grass, P).
% ?- monte_carlo_estimate([rain-true], 1000, P).