% Test: Functioning Slayer kills the demon when shooting them
#include "base.lp".

assigned(0, amanda, slayer).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).          % the demon
assigned(0, felix, poisoner).

% Slayer must not be poisoned
:- reminder_on(poi_poisoned, amanda, _).

% Day 1: amanda (slayer) shoots steph (demon)
slayer_shot(amanda, steph, 1).

% Assert: steph (demon) should die
:- not died(steph, day(1, 0)).

% Assert: slayer gets "no ability" token
:- not reminder_on(sla_no_ability, amanda, day(1, 0)).
