% Test: 5 players with baron should have 1 outsider (0 base + 1 from baron)
% In small games (< 7 players), Baron only adds 1 outsider instead of 2
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 5.

% Baron + 1 outsider (0 base for 5 players + 1 from baron)
% Note: must use players with chairs 0-4 for 5-player game
assigned(0, taylor, baron).
assigned(0, courtney, drunk).
