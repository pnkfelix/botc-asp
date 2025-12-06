% Test: Slayer kills the demon when shooting them
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

st_ends_day(1).

% Assert: slayer ability worked
:- not slayer_kills(amanda, steph, 1).

% Assert: steph (demon) is dead
:- alive(steph, day(1, exec)).
