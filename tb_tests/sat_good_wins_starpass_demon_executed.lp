% Test: Good wins when the demon who received starpass is executed
%
% Scenario:
% - Night 2: Imp kills itself, starpass to a minion
% - Day 2: The new demon (former minion) is executed
% - Result: Good wins because all demons are dead
%
% This tests that the victory condition correctly tracks demon roles
% through role transfers (starpass) and triggers good_wins when the
% transferred demon is eliminated.

#include "base.lp".

needs_night(3).

% 8 players - use the default player_count from base.lp
% TB distribution for 8: 5 townsfolk, 1 outsider, 1 minion, 1 demon
assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, courtney, investigator).
assigned(0, neha, slayer).
assigned(0, pratik, butler).       % Outsider
assigned(0, felix, poisoner).      % Minion - will receive starpass
assigned(0, steph, imp).           % Original demon

% Night 1: No kills (first night)

% Night 2: Imp kills itself (starpass trigger)
player_chooses(imp, steph, point(steph), night(2, 4, 2)).

% felix becomes the new Imp via starpass
:- not starpass_target(felix, 2).

% Day 2: Execute felix (the new demon)
executed(felix, 2).

% Assert: felix should have demon role at day(2, exec) time
:- not has_demon_role(felix, day(2, exec)).

% Assert: felix should be dead at day(2, exec) time
:- not dead(felix, day(2, exec)).

% Assert: There should be no alive demon at day(2, exec)
:- any_alive_demon(day(2, exec)).

% Assert: Good wins at day(2, exec)
:- not good_wins(day(2, exec)).
