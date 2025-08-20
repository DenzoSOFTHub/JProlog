% DCG Test 01: Basic Terminal and Non-terminal Parsing
% Tests: Basic DCG syntax, terminal matching, simple recursion

% Simple terminal matching
a --> [a].
b --> [b].
c --> [c].

% Simple sequence
abc --> a, b, c.

% Simple repetition (0 or more a's)
as --> [].
as --> [a], as.

% Simple repetition (1 or more a's)  
as_plus --> [a].
as_plus --> [a], as_plus.

% Test queries:
% ?- phrase(a, [a]).                    % Expected: true
% ?- phrase(a, [b]).                    % Expected: false
% ?- phrase(abc, [a,b,c]).              % Expected: true
% ?- phrase(abc, [a,b]).                % Expected: false
% ?- phrase(as, []).                    % Expected: true
% ?- phrase(as, [a,a,a]).               % Expected: true
% ?- phrase(as_plus, [a]).              % Expected: true
% ?- phrase(as_plus, [a,a,a]).          % Expected: true
% ?- phrase(as_plus, []).               % Expected: false