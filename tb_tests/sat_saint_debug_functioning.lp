% DEBUG TEST: Check if functioning(sam, day(1, exec)) is derived
% This is a minimal test to isolate functioning predicate issues

#const player_count = 5.

#include "../botc.lp".
#include "../tb.lp".

needs_night(1).

% Define the 5 players for this test
name(felix; amanda; rob; taylor; sam).
chair(felix, 0).
chair(amanda, 1).
chair(rob, 2).
chair(taylor, 3).
chair(sam, 4).

% Fixed role assignments
assigned(0, felix, imp).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, sam, saint).

% Sam must not be poisoned
:- reminder_on(poi_poisoned, sam, _).

% Test 1: player(sam) should be true
:- not player(sam).

% Test 2: time(day(1, exec)) should be true
:- not time(day(1, exec)).

% Test 3: functioning(sam, day(1, exec)) should be true (sam not impaired)
:- not functioning(sam, day(1, exec)).
