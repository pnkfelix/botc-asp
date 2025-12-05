% Test: 14 players should have 1 base outsider
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 14.

% Force 1 outsider
assigned(0, felix, drunk).

% No baron
:- assigned(0, _, baron).
