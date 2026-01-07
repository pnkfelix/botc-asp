% Check Saint rule conditions individually
#const player_count = 5.

#include "../botc.lp".
#include "../tb.lp".

needs_night(1).

name(felix; amanda; rob; taylor; sam).
chair(felix, 0).
chair(amanda, 1).
chair(rob, 2).
chair(taylor, 3).
chair(sam, 4).

assigned(0, felix, imp).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, sam, saint).

executed(sam, 1).

% Check each condition for the Saint rule:
% 1. executed(sam, 1) - given above
:- not executed(sam, 1).

% 2. assigned(0, sam, saint) - given above
:- not assigned(0, sam, saint).

% 3. actual_category(sam, good) - should be derived
:- not actual_category(sam, good).

% 4. outsider(saint) - prerequisite for actual_category
:- not outsider(saint).

% 5. time(day(1, exec)) - should exist
:- not time(day(1, exec)).

% 6. player(sam) - needed for functioning
:- not player(sam).
