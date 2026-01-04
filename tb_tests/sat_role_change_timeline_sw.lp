% Test: Scarlet Woman becomes demon and is told at dawn(N+1)
% When the demon is executed and SW becomes the new demon,
% there should be a timeline event for ST telling SW about the role change.
#include "base.lp".

needs_night(2).

% Setup: 7 players (need 5+ alive for SW trigger)
assigned(0, alice, washerwoman).
assigned(0, bob, chef).
assigned(0, carol, empath).
assigned(0, dave, fortune_teller).
assigned(0, eve, imp).           % The original demon
assigned(0, fiona, scarlet_woman). % SW - will become demon
assigned(0, greg, poisoner).

% Poisoner won't target imp
:- player_chooses(poisoner, _, point(P), _), assigned(0, P, imp).

% Day 1: Execute the demon
executed(eve, 1).

% Assert: SW trigger should fire (demon executed with 5+ alive)
:- not sw_becomes_demon(fiona, 1).

% Assert: fiona should be assigned imp role at day 1
:- not assigned(1, fiona, imp).

% Assert: role_changed_at_day should track this
:- not role_changed_at_day(fiona, scarlet_woman, imp, 1).

% Assert: generic role_changed should also work
:- not role_changed(fiona, scarlet_woman, imp, 1).

% Assert: ST should tell fiona about role change at dawn(2)
% (after night 2, which follows day 1)
:- not st_tells_role_change(fiona, imp, dawn(2)).

% Assert: timeline event should be generated
:- not d_st_tells_role_change(fiona, imp, dawn(2)).
