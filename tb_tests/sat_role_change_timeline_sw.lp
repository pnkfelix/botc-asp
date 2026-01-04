% Test: Scarlet Woman becomes demon and is told at dawn(N+1)
% When the demon is executed and SW becomes the new demon,
% there should be a timeline event for ST telling SW about the role change.
#include "base.lp".

#const player_count = 7.
needs_night(2).

% Setup: 7 players (need 5+ alive for SW trigger)
% Distribution for 7 players: 5 townsfolk, 0 outsiders, 1 minion, 1 demon
% Using valid player names from players.lp
assigned(0, amanda, washerwoman).
assigned(0, rob, chef).
assigned(0, taylor, empath).
assigned(0, courtney, fortune_teller).
assigned(0, neha, slayer).
assigned(0, steph, imp).              % The original demon
assigned(0, felix, scarlet_woman).    % SW - will become demon

% Day 1: Execute the demon
executed(steph, 1).

% Assert: SW trigger should fire (demon executed with 5+ alive)
:- not sw_becomes_demon(felix, 1).

% Assert: felix should be assigned imp role at day 1
:- not assigned(1, felix, imp).

% Assert: role_changed_at_day should track this
:- not role_changed_at_day(felix, scarlet_woman, imp, 1).

% Assert: generic role_changed should also work
:- not role_changed(felix, scarlet_woman, imp, 1).

% Assert: ST should tell felix about role change at dawn(2)
% (after night 2, which follows day 1)
:- not st_tells_role_change(felix, imp, dawn(2)).

% Assert: timeline event should be generated
:- not d_st_tells_role_change(felix, imp, dawn(2)).
