% Test: 15 players should have 2 base outsiders
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 15.

% Force 2 outsiders
assigned(0, felix, drunk).
assigned(0, kunjal, butler).

% No baron
:- assigned(0, _, baron).
