% Test: Imp kills itself (starpass) and a minion becomes the new Imp
#include "base.lp".

needs_night(2).

assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).          % The original Imp
assigned(0, felix, poisoner).     % Minion - will become new Imp

% Poisoner won't target their own demon (minions know who the demon is)
:- player_chooses(poisoner, _, point(Demon), _), assigned(0, Demon, imp).

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
