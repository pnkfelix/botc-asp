% Test: 6 players with baron CANNOT have 3 outsiders
% In small games (< 7 players), Baron only adds 1 outsider, so max is 2 (1 base + 1 from baron)
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 6.

% Baron + 3 outsiders should be unsatisfiable (only 2 outsiders allowed)
assigned(0, taylor, baron).
assigned(0, felix, drunk).
assigned(0, kunjal, butler).
assigned(0, amanda, recluse).
