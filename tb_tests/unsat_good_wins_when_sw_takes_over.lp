% Test: Good does NOT win when SW takes over after demon execution
%
% Scenario:
% - 8 players (above SW threshold of 5)
% - Day 1: The demon is executed
% - Scarlet Woman takes over
% - Result: Good should NOT win because SW is alive with demon role
%
% This is UNSAT because we assert good_wins but SW takes over.

#include "base.lp".

needs_night(2).

% 8 players - use the default player_count from base.lp
% TB distribution for 8: 5 townsfolk, 1 outsider, 1 minion, 1 demon
assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, courtney, investigator).
assigned(0, neha, slayer).
assigned(0, pratik, butler).          % Outsider
assigned(0, felix, scarlet_woman).    % Minion - will become demon
assigned(0, steph, imp).              % Original demon

% Day 1: Execute the demon
executed(steph, 1).

% Assert: SW takes over (8 players >= 5 threshold)
:- not sw_becomes_demon(felix, 1).

% Assert: felix should have demon role after takeover
:- not has_demon_role(felix, day(1, exec)).

% Assert: There IS an alive demon (felix)
:- not any_alive_demon(day(1, exec)).

% This makes it UNSAT: assert good wins, but they shouldn't
:- not good_wins(day(1, exec)).
