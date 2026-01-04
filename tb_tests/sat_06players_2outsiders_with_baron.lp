% Test: 6 players with baron should have 2 outsiders (1 base + 1 from baron)
% In small games (< 7 players), Baron only adds 1 outsider instead of 2
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 6.

% Baron + 2 outsiders (1 base for 6 players + 1 from baron)
% Note: must use players with chairs 0-5 for 6-player game
assigned(0, taylor, baron).
assigned(0, felix, drunk).
assigned(0, courtney, butler).
