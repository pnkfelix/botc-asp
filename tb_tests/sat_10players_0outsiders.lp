% Test: 10 players should have 0 base outsiders
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 10.

% No outsiders assigned - should be valid
:- assigned(0, _, R), outsider(R).
