% Test: Slayer CAN kill Recluse (Recluse may register as demon)
% This test shows it's POSSIBLE for Recluse to die when shot by Slayer
#include "base.lp".

assigned(0, amanda, slayer).
assigned(0, rob, recluse).           % Recluse can register as demon
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Slayer must not be poisoned
:- reminder_on(poi_poisoned, amanda, _).

% Day 1: amanda (slayer) shoots rob (recluse)
slayer_shot(amanda, rob, 1).

% Assert: rob (recluse) dies - this is POSSIBLE if recluse registers as demon
:- not died(rob, day(1, 0)).

% Assert: slayer gets "no ability" token
:- not reminder_on(sla_no_ability, amanda, day(1, 0)).
