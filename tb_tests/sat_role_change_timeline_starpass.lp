% Test: Imp starpass and minion is told at dawn(N)
% When the Imp kills itself and starpass occurs,
% there should be a timeline event for ST telling the new demon about the role change.
#include "base.lp".

needs_night(2).

assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).          % The original Imp
assigned(0, felix, poisoner).     % Minion - will become new Imp

% Poisoner won't target their own demon
:- player_chooses(poisoner, _, point(Demon), _), assigned(0, Demon, imp).

% Night 2: Imp targets itself (starpass)
player_chooses(imp, steph, point(steph), night(2, 4, 2)).

% Assert: starpass should trigger
:- not imp_starpass_triggered(steph, 2).

% Assert: felix should become the starpass target
:- not starpass_target(felix, 2).

% Assert: felix should be assigned imp role at time 2
:- not assigned(2, felix, imp).

% Assert: role_changed_at_night should track this
:- not role_changed_at_night(felix, poisoner, imp, 2).

% Assert: generic role_changed should also work
:- not role_changed(felix, poisoner, imp, 2).

% Assert: ST should tell felix about role change at dawn(2)
% (immediately after night 2, before day 2)
:- not st_tells_role_change(felix, imp, dawn(2)).

% Assert: timeline event should be generated
:- not d_st_tells_role_change(felix, imp, dawn(2)).
