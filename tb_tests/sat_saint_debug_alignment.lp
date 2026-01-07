% DEBUG TEST: Check if alignment(sam, good, day(1, exec)) is derived
% This is a minimal test to isolate alignment predicate issues

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

% Test 1: actual_category(sam, good) should be true
:- not actual_category(sam, good).

% Test 2: time(day(1, exec)) should be true
:- not time(day(1, exec)).

% Test 3: alignment(sam, good, day(1, exec)) should be true
:- not alignment(sam, good, day(1, exec)).
