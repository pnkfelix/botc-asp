% Test: 9 players should have exactly 2 outsiders (base)
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 9.

% Force exactly these two outsiders
assigned(0, felix, drunk).
assigned(0, kunjal, butler).

% No baron (to ensure it's base count)
:- assigned(0, _, baron).
