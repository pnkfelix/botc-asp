% Test: Night 2 - Vigormortis kills a player
#include "base.lp".

needs_night(2).

% Full assignment for 8 players (6 townsfolk, 0 outsiders, 1 minion, 1 demon)
% Vigormortis has -1 outsider adjustment
assigned(0, amanda, clockmaker).
assigned(0, rob, dreamer).
assigned(0, taylor, oracle).
assigned(0, courtney, flowergirl).
assigned(0, steph, vigormortis).
assigned(0, felix, cerenovus).
assigned(0, neha, town_crier).
assigned(0, pratik, sage).

% Night 1: No one dies (first night has no demon kill)
% Day 1: No execution
% Night 2: Vigormortis kills rob

% Vigormortis targets rob on night 2 (role order 7)
player_chooses(vigormortis, steph, point(rob), night(2, 7, 2)).

% Assert: rob should be dead
:- not died(rob, _).

% Assert: rob should have vigor_dead token during night 2
:- not reminder_on(vigor_dead, rob, night(2, 7, 3)).

% Assert: steph (demon) survives
:- died(steph, _).
