% Baron
% For small games (5-6 players), Baron only swaps in 1 outsider
% For standard games (7+ players), Baron swaps in 2 outsiders

% Small game: only +1 outsider (separate from standard because the breakpoint is a game design rule)
causes_outsider_mod(baron, 1) :- player_count < 7.
% Standard game: +2 outsiders (replaces 2 townsfolk slots; consumed by bag-building logic in botc.lp)
causes_outsider_mod(baron, 2) :- player_count >= 7.
