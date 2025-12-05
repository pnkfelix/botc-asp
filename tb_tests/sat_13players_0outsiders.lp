% Test: 13 players should have 0 base outsiders
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 13.

% No outsiders
:- assigned(0, _, R), outsider(R).

% No baron
:- assigned(0, _, baron).
