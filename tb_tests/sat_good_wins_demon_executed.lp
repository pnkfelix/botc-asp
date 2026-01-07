% Test: Good wins when the demon is executed (no replacement)
%
% Scenario:
% - Day 1: The demon is executed
% - No Scarlet Woman trigger (only 5 players, threshold is 5)
% - Result: Good wins because the demon is dead
%
% This tests the basic victory condition.

#include "base.lp".

needs_night(2).

% 5 players for a valid game
assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, sarah, chef).
assigned(0, felix, poisoner).    % Minion
assigned(0, steph, imp).         % Demon

% Day 1: Execute the demon
executed(steph, 1).

% Assert: steph should have demon role at day(1, exec) time
:- not has_demon_role(steph, day(1, exec)).

% Assert: steph should be dead at day(1, exec) time
:- not dead(steph, day(1, exec)).

% Assert: There should be no alive demon at day(1, exec)
:- any_alive_demon(day(1, exec)).

% Assert: Good wins at day(1, exec)
:- not good_wins(day(1, exec)).
