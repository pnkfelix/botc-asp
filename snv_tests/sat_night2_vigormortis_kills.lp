% Test: Night 2 - Vigormortis kills a player
#include "base.lp".

needs_night(2).

assigned(0, amanda, clockmaker).
assigned(0, rob, dreamer).
assigned(0, taylor, oracle).
assigned(0, steph, vigormortis).
assigned(0, felix, witch).

% Night 1: No one dies (first night has no demon kill)
% Day 1: No execution
% Night 2: Vigormortis kills rob

% Vigormortis targets rob on night 2 (role order 7)
player_chooses(vigormortis, steph, point(rob), night(2, 7, 2)).

% Assert: rob should be dead
:- not died(rob, _).

% Assert: rob should have vigor_dead token during night 2
:- not reminder_on(vigor_dead, rob, night(2, 7, 3)).

% Assert: everyone else survives
:- died(amanda, _).
:- died(taylor, _).
:- died(steph, _).
:- died(felix, _).
