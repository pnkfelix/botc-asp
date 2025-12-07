% Test: Night 2 - Imp kills a player
#include "base.lp".

night_number(2).
day_number(2).

assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Night 1: No one dies (first night has no demon kill)
% Day 1: No execution
% Night 2: Imp kills rob

% Imp targets rob on night 2
player_chooses(imp, steph, point(rob), night(2, 4, 2)).

% Assert: rob should be dead at start of day 2
:- not died(rob, _).

% Assert: rob should have imp_dead token during night 2
:- not reminder_on(imp_dead, rob, night(2, 4, 3)).

% Assert: everyone else survives
:- died(amanda, _).
:- died(taylor, _).
:- died(steph, _).
:- died(felix, _).
