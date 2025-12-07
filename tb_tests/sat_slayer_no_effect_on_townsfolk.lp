% Test: Slayer doesn't kill a townsfolk (they don't register as demon)
#include "base.lp".

assigned(0, amanda, slayer).
assigned(0, rob, washerwoman).    % not a demon, can't register as demon
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Slayer must not be poisoned
:- reminder_on(poi_poisoned, amanda, _).

% Day 1: amanda (slayer) shoots rob (townsfolk)
slayer_shot(amanda, rob, 1).

% Assert: rob should NOT die (townsfolk can't register as demon)
:- died(rob, _).

% Assert: slayer still gets "no ability" token (shot was used)
:- not reminder_on(sla_no_ability, amanda, day(1, 0)).
