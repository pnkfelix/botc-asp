% Test: Imp kills itself (starpass) and a minion becomes the new Imp
#include "base.lp".

needs_night(3).

assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).          % The original Imp
assigned(0, felix, poisoner).     % Minion - will become new Imp

% Poisoner won't target their own demon (minions know who the demon is)
:- player_chooses(poisoner, _, point(Demon), _), assigned(0, Demon, imp).

% Note: When a minion transforms into a demon via starpass, their reminder
% tokens should be removed (e.g., poi_poisoned). The current game logic may
% not fully model this. The poisoner choice rule also still fires for night 3,
% which is a known limitation - in a real game, the transformed minion
% wouldn't act as their old role.

% Night 2: Imp targets itself (starpass)
player_chooses(imp, steph, point(steph), night(2, 4, 2)).

% Assert: steph (original Imp) should die
:- not died(steph, _).

% Assert: starpass should trigger
:- not imp_starpass_triggered(steph, 2).

% Assert: felix (the only minion) should become the starpass target
:- not starpass_target(felix, 2).

% Assert: felix should be assigned imp role at time 2
:- not assigned(2, felix, imp).

% Assert: felix should be current_demon on night 3
:- not current_demon(felix, 3).

% Assert: steph should be replaced as demon on night 3
:- not replaced_as_demon(steph, 3).

% Night 3: New Imp (felix) kills someone
player_chooses(imp, felix, point(rob), night(3, 4, 2)).

% Assert: rob should die from the new Imp's kill
:- not died(rob, _).
