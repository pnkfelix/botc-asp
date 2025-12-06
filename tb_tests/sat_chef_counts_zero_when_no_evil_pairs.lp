% Test: Chef correctly counts zero evil pairs
% With poisoner (chair 0) and imp (chair 4) in 8-player game,
% they are NOT neighbors, so chef should get count(0)
#include "base.lp".

assigned(0, amanda, poisoner).  % chair 0
assigned(0, steph, imp).        % chair 4 (not adjacent to 0)
assigned(0, rob, chef).         % chair 1

% Chef must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, rob, _).

% Assert: Chef is told count(0) - no pairs of adjacent evil players
:- not st_tells(chef, rob, count(0), _).
