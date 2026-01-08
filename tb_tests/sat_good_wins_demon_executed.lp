% Test: Good wins when the demon is executed (no replacement)
%
% Scenario:
% - Day 1: The demon is executed
% - No Scarlet Woman trigger (threshold is 5, we have 5 alive at exec)
% - Result: Good wins because the demon is dead
%
% This tests the basic victory condition.

#include "base.lp".

needs_night(2).

% 8 players - use the default player_count from base.lp
% TB distribution for 8: 5 townsfolk, 1 outsider, 1 minion, 1 demon
assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, courtney, investigator).
assigned(0, neha, slayer).
assigned(0, pratik, butler).       % Outsider
assigned(0, felix, poisoner).      % Minion
assigned(0, steph, imp).           % Demon

% Day 1: Execute the demon
executed(steph, 1).

% Assert: steph should have demon role at day(1, exec) time
:- not has_demon_role(steph, day(1, exec)).

% Assert: steph should be dead at day(1, exec) time
:- not dead_at_time(steph, day(1, exec)).

% Assert: There should be no alive demon at day(1, exec)
:- any_alive_demon(day(1, exec)).

% Assert: Good wins at day(1, exec)
:- not good_wins(day(1, exec)).
