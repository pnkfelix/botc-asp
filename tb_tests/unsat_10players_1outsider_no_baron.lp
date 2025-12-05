% Test: 10 players should NOT have 1 outsider without baron
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 10.

% Force 1 outsider
assigned(0, p1, drunk).

% No baron
:- assigned(0, _, baron).
