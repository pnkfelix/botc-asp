% Test: Night 2 - Fang Gu kills a player (non-outsider)
#include "base.lp".

needs_night(2).

assigned(0, amanda, clockmaker).
assigned(0, rob, dreamer).
assigned(0, taylor, oracle).
assigned(0, steph, fang_gu).
assigned(0, felix, witch).

% Night 1: No one dies (first night has no demon kill)
% Day 1: No execution
% Night 2: Fang Gu kills rob (a townsfolk)

% Fang Gu targets rob on night 2 (role order 6)
player_chooses(fang_gu, steph, point(rob), night(2, 6, 2)).

% Assert: rob should be dead
:- not died(rob, _).

% Assert: rob should have fang_gu_dead token during night 2
:- not reminder_on(fang_gu_dead, rob, night(2, 6, 3)).

% Assert: everyone else survives (Fang Gu doesn't die when killing non-outsider)
:- died(amanda, _).
:- died(taylor, _).
:- died(steph, _).
:- died(felix, _).
