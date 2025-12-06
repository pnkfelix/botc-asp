% Test: Virgin doesn't kill an evil nominator
#include "base.lp".

assigned(0, amanda, chef).
assigned(0, rob, virgin).         % chair 1 - the virgin
assigned(0, taylor, washerwoman).
assigned(0, steph, imp).          % will nominate virgin
assigned(0, felix, poisoner).

% Virgin must not be poisoned
:- reminder_on(poi_poisoned, rob, _).

% Day 1: steph (demon, not townsfolk) nominates rob (virgin)
nomination(steph, rob, 1, 1).

st_ends_day(1).

% Assert: virgin ability did NOT trigger (steph is evil, not townsfolk)
:- virgin_triggered(_, _, _).

% Assert: steph should still be alive
:- not alive(steph, day(1, exec)).
