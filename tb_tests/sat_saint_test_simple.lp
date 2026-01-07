% Minimal Saint test - no poison constraint
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

% Execute the Saint
executed(sam, 1).

% If sam is NOT poisoned, evil should win
% Use implication: if functioning(sam) then evil_wins must hold
:- functioning(sam, day(1, exec)), not evil_wins(day(1, exec)).
