% Test: 7 players should have 0 base outsiders
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 7.

% No outsiders - should be valid
:- assigned(0, _, R), outsider(R).
