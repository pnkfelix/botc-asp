% Test: Chef correctly counts evil pairs
% With poisoner (chair 0) and imp (chair 7) in 8-player game,
% they are neighbors (circular), so chef should get count(1)
#include "base.lp".

assigned(0, amanda, poisoner).  % chair 0
assigned(0, pratik, imp).       % chair 7
assigned(0, rob, chef).         % chair 1

% Chef must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, rob, _).

% Assert: Chef is told count(1) - one pair of adjacent evil players
:- not st_tells_core(chef, rob, count(1), _).
