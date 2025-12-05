% Test: Baron CANNOT be seen as townsfolk by a functioning washerwoman
#include "base.lp".

assigned(0, amanda, baron).
assigned(0, rob, washerwoman).

% Washerwoman must not be poisoned (so they're functioning)
:- reminder_on(poisoner_poisoned, rob, _).

% Assert: washerwoman CANNOT see amanda (baron) as the townsfolk
% (If this is SAT, the test fails - baron shouldn't register as townsfolk)
:- not reminder_on(washerwoman_townsfolk, amanda, _).
