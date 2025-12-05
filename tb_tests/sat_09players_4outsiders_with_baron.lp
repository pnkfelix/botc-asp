% Test: 9 players with baron should allow 4 outsiders (2 base + 2 from baron)
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 9.

% Force 4 outsiders with baron as minion
assigned(0, felix, drunk).
assigned(0, kunjal, butler).
assigned(0, amanda, recluse).
assigned(0, rob, saint).
assigned(0, taylor, baron).
