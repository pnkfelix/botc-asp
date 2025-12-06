% Test: Slayer doesn't kill a non-demon
#include "base.lp".

assigned(0, amanda, slayer).
assigned(0, rob, washerwoman).    % not the demon
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Slayer must not be poisoned
:- reminder_on(poi_poisoned, amanda, _).

% Day 1: amanda (slayer) shoots rob (not the demon)
slayer_shot(amanda, rob, 1).

st_ends_day(1).

% Assert: slayer ability did NOT kill
:- slayer_kills(_, _, _).

% Assert: rob is still alive
:- not alive(rob, day(1, exec)).
