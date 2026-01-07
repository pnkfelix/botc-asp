% DEBUG TEST: Check actual_category without alignment
% Verifies outsider(saint) and actual_category derivation

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

% Check outsider(saint) is true
:- not outsider(saint).

% Check actual_category(sam, good) is true
:- not actual_category(sam, good).
