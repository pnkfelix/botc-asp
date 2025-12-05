% Test: Spy CAN be seen as townsfolk by a functioning washerwoman
#include "base.lp".

assigned(0, amanda, spy).
assigned(0, rob, washerwoman).

% Washerwoman must not be poisoned (so they're functioning)
:- reminder_on(poisoner_poisoned, rob, _).

% Assert: washerwoman CAN see amanda (spy) as the townsfolk
:- not reminder_on(washerwoman_townsfolk, amanda, _).
