% Test: Imp kills itself but no living minions exist - starpass cannot complete
% This should be UNSATISFIABLE because starpass requires a living minion
#include "base.lp".

needs_night(2).

assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).          % The Imp
assigned(0, felix, poisoner).     % Minion - but will die before starpass

% Day 1: Felix (the only minion) is executed
executed(felix, 1).

% Night 2: Imp targets itself (starpass attempt)
player_chooses(imp, steph, point(steph), night(2, 4, 2)).

% Assert: starpass should trigger (Imp did kill itself)
:- not imp_starpass_triggered(steph, 2).

% Assert: felix is dead and cannot be eligible for starpass
:- alive(felix, night(2, 4, 3)).

% The game has no valid solution because:
% - imp_starpass_triggered(steph, 2) is true
% - No minion is eligible for starpass (felix is dead)
% - The constraint { starpass_target(...) } = 1 requires exactly one target
% - But there are no eligible targets, making this UNSATISFIABLE
