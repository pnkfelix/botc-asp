% Test: Spy registering as evil can create an evil pair for Chef
% With spy (chair 0) next to imp (chair 7), if spy registers as evil,
% chef should be able to get count(1)
#include "base.lp".

assigned(0, amanda, spy).       % chair 0
assigned(0, pratik, imp).       % chair 7 (neighbor of chair 0)
assigned(0, rob, chef).         % chair 1

% Chef must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, rob, _).

% Assert: Chef CAN be told count(1) - spy registers as evil next to imp
:- not st_tells(chef, rob, count(1), _).
