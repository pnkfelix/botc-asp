% Test: Imp starpass and minion is told at dawn(N)
% When the Imp kills itself and starpass occurs,
% there should be a timeline event for ST telling the new demon about the role change.
#include "base.lp".

#const player_count = 5.
needs_night(2).

% Setup: 5 players with passive roles to minimize solver complexity
% Distribution for 5 players: 3 townsfolk, 0 outsiders, 1 minion, 1 demon
% Note: chairs 0-4 are valid for player_count=5 (amanda, rob, taylor, courtney, steph)
assigned(0, amanda, soldier).
assigned(0, rob, mayor).
assigned(0, taylor, slayer).
assigned(0, courtney, imp).       % The original Imp (chair 3)
assigned(0, steph, poisoner).     % Minion - will become new Imp (chair 4)

% Poisoner won't target their own demon
:- player_chooses(poisoner, _, point(Demon), _), assigned(0, Demon, imp).

% Night 2: Imp targets itself (starpass)
player_chooses(imp, courtney, point(courtney), night(2, 4, 2)).

% Assert: starpass should trigger
:- not imp_starpass_triggered(courtney, 2).

% Assert: steph should become the starpass target
:- not starpass_target(steph, 2).

% Assert: steph should be assigned imp role at time 2
:- not assigned(2, steph, imp).

% Assert: role_changed_at_night should track this
:- not role_changed_at_night(steph, poisoner, imp, 2).

% Assert: generic role_changed should also work
:- not role_changed(steph, poisoner, imp, 2).

% Assert: ST should tell steph about role change at dawn(2)
% (immediately after night 2, before day 2)
:- not st_tells_role_change(steph, imp, dawn(2)).

% Assert: timeline event should be generated
:- not d_st_tells_role_change(steph, imp, dawn(2)).
