% Test: 9 players should NOT allow 3 outsiders (without baron)
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 9.

% Force 3 outsiders - should be invalid without baron
assigned(0, p1, drunk).
assigned(0, p2, butler).
assigned(0, p3, recluse).

% No baron in game
:- assigned(0, _, baron).
