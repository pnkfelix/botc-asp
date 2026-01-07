% Test: Scarlet Woman becomes demon and is told immediately at execution
% When the demon is executed and SW becomes the new demon,
% there should be a timeline event for ST telling SW about the role change.

% Include files directly (not base.lp) so we can set player_count first
#const player_count = 5.
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

needs_night(2).

% Setup: 5 players (need 5+ alive at day start for SW trigger)
% Distribution for 5 players: 3 townsfolk, 0 outsiders, 1 minion, 1 demon
% Using passive roles to minimize solver complexity
% Note: chairs 0-4 are valid for player_count=5 (amanda, rob, taylor, courtney, steph)
assigned(0, amanda, soldier).
assigned(0, rob, mayor).
assigned(0, taylor, slayer).
assigned(0, courtney, imp).           % The original demon (chair 3)
assigned(0, steph, scarlet_woman).    % SW - will become demon (chair 4)

% Day 1: Execute the demon (5 alive at day start, SW trigger fires)
executed(courtney, 1).

% Assert: SW trigger should fire (demon executed with 5 alive at day start)
:- not sw_becomes_demon(steph, 1).

% Assert: steph should be assigned imp role at start of night 2 (after SW triggered on day 1)
% With the new semantics, assigned(N, P, R) means "at start of night N, P has role R"
:- not assigned(2, steph, imp).

% Assert: role_changed_at_day should track this
:- not role_changed_at_day(steph, scarlet_woman, imp, 1).

% Assert: generic role_changed should also work
:- not role_changed(steph, scarlet_woman, imp, 1).

% Assert: ST should tell steph about role change immediately at execution
:- not st_tells_role_change(steph, imp, day(1, exec)).

% Assert: timeline event should be generated
:- not d_st_tells_role_change(steph, imp, day(1, exec)).
