% ---------------------------------------------------------------------------
% JProlog engine v4 prelude — library(pairs)
%
% ISS-2025-0468: three-line relational definitions that work in every mode,
% where the Java built-ins were one-directional. Autoloaded on first reference.
% ---------------------------------------------------------------------------

:- module(pairs, [pairs_keys_values/3, pairs_keys/2, pairs_values/2]).

pairs_keys_values([], [], []).
pairs_keys_values([K-V|T], [K|Ks], [V|Vs]) :- pairs_keys_values(T, Ks, Vs).

pairs_keys([], []).
pairs_keys([K-_|T], [K|Ks]) :- pairs_keys(T, Ks).

pairs_values([], []).
pairs_values([_-V|T], [V|Vs]) :- pairs_values(T, Vs).
