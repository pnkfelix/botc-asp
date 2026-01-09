% Test: Night 2 - No Dashii kills a player
#include "base.lp".

needs_night(2).

assigned(0, amanda, clockmaker).
assigned(0, rob, dreamer).
assigned(0, taylor, oracle).
assigned(0, steph, no_dashii).
assigned(0, felix, witch).

% Night 1: No one dies (first night has no demon kill)
% Day 1: No execution
% Night 2: No Dashii kills rob

% No Dashii targets rob on night 2 (role order 9)
player_chooses(no_dashii, steph, point(rob), night(2, 9, 2)).

% Assert: rob should be dead at start of day 2
:- not died(rob, _).

% Assert: rob should have no_dashii_dead token during night 2
:- not reminder_on(no_dashii_dead, rob, night(2, 9, 3)).

% Assert: everyone else survives
:- died(amanda, _).
:- died(taylor, _).
:- died(steph, _).
:- died(felix, _).
