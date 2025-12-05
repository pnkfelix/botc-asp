% Test: Poisoned washerwoman CAN receive false info (non-townsfolk as townsfolk)
#include "base.lp".

assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, neha, imp).

% Washerwoman MUST be poisoned
:- not reminder_on(poi_poisoned, rob, _).

% Assert: washerwoman CAN see neha (imp/demon) as the townsfolk
% This should only be possible because washerwoman is poisoned
:- not reminder_on(ww_townsfolk, neha, _).
