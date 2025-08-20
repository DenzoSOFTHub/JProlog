% DCG Test 14: State Machine Parser
% Tests: State transitions, finite automata simulation

% States for a simple automaton
% State machine: accepts strings ending with 'ab'

% State transitions
state(start, a, got_a).
state(start, b, start).
state(got_a, a, got_a).
state(got_a, b, accept).
state(accept, a, got_a).
state(accept, b, start).

% DCG simulation of state machine
accepts_ab --> machine(start).

machine(accept) --> [].
machine(State) --> [Input], { state(State, Input, NextState) }, machine(NextState).

% More complex state machine: binary number divisible by 3
% States represent remainder when dividing by 3
% rem0 = remainder 0, rem1 = remainder 1, rem2 = remainder 2

binary_div3 --> div3_machine(rem0).

div3_machine(rem0) --> [].      % Accept state (divisible by 3)
div3_machine(rem0) --> [48], div3_machine(rem0).  % '0': rem = (rem*2) mod 3 = 0
div3_machine(rem0) --> [49], div3_machine(rem1).  % '1': rem = (0*2+1) mod 3 = 1
div3_machine(rem1) --> [48], div3_machine(rem2).  % '0': rem = (1*2) mod 3 = 2  
div3_machine(rem1) --> [49], div3_machine(rem0).  % '1': rem = (1*2+1) mod 3 = 0
div3_machine(rem2) --> [48], div3_machine(rem1).  % '0': rem = (2*2) mod 3 = 1
div3_machine(rem2) --> [49], div3_machine(rem2).  % '1': rem = (2*2+1) mod 3 = 2

% Generic state machine framework
generic_machine(InitialState, FinalStates, Transitions) -->
    run_machine(InitialState, FinalStates, Transitions).

run_machine(State, FinalStates, _) --> 
    [], { member(State, FinalStates) }.

run_machine(State, FinalStates, Transitions) -->
    [Input], 
    { member(transition(State, Input, NextState), Transitions) },
    run_machine(NextState, FinalStates, Transitions).

% Traffic light state machine
traffic_light_transitions([
    transition(red, timer, green),
    transition(green, timer, yellow),
    transition(yellow, timer, red),
    transition(red, emergency, red),
    transition(green, emergency, red),
    transition(yellow, emergency, red)
]).

traffic_light(Inputs) -->
    { traffic_light_transitions(Trans) },
    generic_machine(red, [red, green, yellow], Trans).

% Test queries:
% ?- phrase(accepts_ab, [97,98]).                   % Expected: true
% ?- phrase(accepts_ab, [97,97,98]).                % Expected: true  
% ?- phrase(accepts_ab, [97]).                      % Expected: false
% ?- phrase(binary_div3, [49,49]).                  % Expected: true (binary 11 = 3)
% ?- phrase(binary_div3, [49,49,48]).               % Expected: true (binary 110 = 6)