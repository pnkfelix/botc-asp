% Test: Night 2 - Fang Gu kills a player (non-outsider)
#include "base.lp".

needs_night(2).

% Full assignment for 8 players (4 townsfolk, 2 outsiders, 1 minion, 1 demon)
% Fang Gu has +1 outsider adjustment
assigned(0, amanda, clockmaker).
assigned(0, rob, dreamer).
assigned(0, taylor, oracle).
assigned(0, courtney, flowergirl).
assigned(0, steph, fang_gu).
assigned(0, felix, cerenovus).
assigned(0, neha, mutant).
assigned(0, pratik, barber).

% Night 1: No one dies (first night has no demon kill)
% Day 1: No execution
% Night 2: Fang Gu kills rob (a townsfolk)

% Fang Gu targets rob on night 2 (role order 6)
player_chooses(fang_gu, steph, point(rob), night(2, 6, 2)).

% Assert: rob should be dead
:- not died(rob, _).

% Assert: rob should have fang_gu_dead token during night 2
:- not reminder_on(fang_gu_dead, rob, night(2, 6, 3)).

% Assert: steph (demon) survives (Fang Gu doesn't die when killing non-outsider)
:- died(steph, _).
