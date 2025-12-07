% Test: Slayer shoots Recluse but Recluse survives
% This test shows Recluse does NOT have to die - they can register as outsider
#include "base.lp".

assigned(0, amanda, slayer).
assigned(0, rob, recluse).           % Recluse CAN register as outsider (their true category)
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Slayer must not be poisoned
:- reminder_on(poi_poisoned, amanda, _).

% Day 1: amanda (slayer) shoots rob (recluse)
slayer_shot(amanda, rob, 1).

% Assert: rob (recluse) does NOT die - recluse registers as outsider
:- died(rob, _).

% Assert: slayer gets "no ability" token (shot was still used)
:- not reminder_on(sla_no_ability, amanda, day(1, 0)).
