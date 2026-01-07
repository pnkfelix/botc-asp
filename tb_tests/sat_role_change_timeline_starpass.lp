% Test: Imp starpass and minion is told immediately
% When the Imp kills itself and starpass occurs,
% there should be a timeline event for ST telling the new demon about the role change.
% The notification happens immediately at the time of the role change.

% Include files directly (not base.lp) so we can set player_count first
#const player_count = 5.
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

needs_night(3).  % Need night 3 to check assigned(3, steph, imp)

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

% Assert: steph should be assigned imp role at start of night 3 (after starpass at night 2)
% With the new semantics, assigned(N, P, R) means "at start of night N, P has role R"
:- not assigned(3, steph, imp).

% Assert: role_changed_at_night should track this
:- not role_changed_at_night(steph, poisoner, imp, 2).

% Assert: generic role_changed should also work
:- not role_changed(steph, poisoner, imp, 2).

% Assert: ST should tell steph about role change immediately at night(2, 4, 3)
% (the same time as the starpass)
:- not st_tells_role_change(steph, imp, night(2, 4, 3)).

% Assert: timeline event should be generated
:- not d_st_tells_role_change(steph, imp, night(2, 4, 3)).
