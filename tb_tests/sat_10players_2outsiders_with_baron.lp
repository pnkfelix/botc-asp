% Test: 10 players with baron should have 2 outsiders (0 base + 2 from baron)
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 10.

% Baron + 2 outsiders
assigned(0, taylor, baron).
assigned(0, felix, drunk).
assigned(0, kunjal, butler).
