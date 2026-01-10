% Test: Night 2 - No Dashii kills a player
#include "base.lp".

needs_night(2).

% Full assignment for 8 players (5 townsfolk, 1 outsider, 1 minion, 1 demon)
assigned(0, amanda, clockmaker).
assigned(0, rob, dreamer).
assigned(0, taylor, oracle).
assigned(0, courtney, flowergirl).
assigned(0, steph, no_dashii).
assigned(0, felix, cerenovus).
assigned(0, neha, town_crier).
assigned(0, pratik, mutant).

% Night 1: No one dies (first night has no demon kill)
% Day 1: No execution
% Night 2: No Dashii kills rob

% No Dashii targets rob on night 2 (role order 9)
player_chooses(no_dashii, steph, point(rob), night(2, 9, 2)).

% Assert: rob should be dead
:- not died(rob, _).

% Assert: rob should have no_dashii_dead token during night 2
:- not reminder_on(no_dashii_dead, rob, night(2, 9, 3)).

% Assert: steph (demon) survives
:- died(steph, _).
