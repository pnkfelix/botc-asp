% Baron
% For small games (5-6 players), Baron only swaps in 1 outsider
% For standard games (7+ players), Baron swaps in 2 outsiders

causes_outsider_mod(baron, 1) :- player_count < 7.
causes_outsider_mod(baron, 2) :- player_count >= 7.
