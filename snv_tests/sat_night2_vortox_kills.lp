% Test: Night 2 - Vortox kills a player
#include "base.lp".

needs_night(2).

assigned(0, amanda, clockmaker).
assigned(0, rob, dreamer).
assigned(0, taylor, oracle).
assigned(0, steph, vortox).
assigned(0, felix, witch).

% Night 1: No one dies (first night has no demon kill)
% Day 1: An execution happens (to avoid Vortox special win condition)
executed(amanda, 1).

% Night 2: Vortox kills rob

% Vortox targets rob on night 2 (role order 8)
player_chooses(vortox, steph, point(rob), night(2, 8, 2)).

% Assert: rob should be dead
:- not died(rob, _).

% Assert: rob should have vortox_dead token during night 2
:- not reminder_on(vortox_dead, rob, night(2, 8, 3)).

% Assert: other players survive (amanda died from execution)
:- died(taylor, _).
:- died(steph, _).
:- died(felix, _).
