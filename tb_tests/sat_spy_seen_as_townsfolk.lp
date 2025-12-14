% Test: Spy CAN be seen as townsfolk by a functioning washerwoman
% AND the ST can tell the washerwoman that the spy is "the Chef"
#include "base.lp".

assigned(0, amanda, spy).
assigned(0, rob, washerwoman).

% Washerwoman must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, rob, _).

% Assert: washerwoman CAN see amanda (spy) as the townsfolk
:- not reminder_on(ww_townsfolk, amanda, _).

% Assert: ST CAN tell the washerwoman that amanda is "the Chef"
% (even though amanda is actually a Spy, she registers as townsfolk)
:- not st_tells(washerwoman, rob, info(_, _, chef), _).
