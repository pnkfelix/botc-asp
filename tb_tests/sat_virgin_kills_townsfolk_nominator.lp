% Test: Virgin kills a Townsfolk who nominates her and ends the day
#include "base.lp".

assigned(0, amanda, chef).        % chair 0 - townsfolk, will nominate virgin
assigned(0, rob, virgin).         % chair 1 - the virgin
assigned(0, taylor, washerwoman).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Virgin must not be poisoned
:- reminder_on(poi_poisoned, rob, _).

% Day 1: amanda (townsfolk) nominates rob (virgin)
nomination(amanda, rob, 1, 1).

% No st_ends_day needed - virgin trigger ends the day

% Assert: virgin ability triggered
:- not virgin_triggered(amanda, 1, 1).

% Assert: day ends from virgin trigger
:- not day_ends(1).

% Assert: amanda should be dead at end of day
:- alive(amanda, day(1, exec)).
