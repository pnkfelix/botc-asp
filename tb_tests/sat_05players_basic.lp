% Test: 5-player game should be satisfiable with correct role distribution
% Expected: SATISFIABLE with 3 townsfolk, 0 outsiders, 1 minion, 1 demon

#include "botc.lp".
#include "tb.lp".
#include "players.lp".

#const player_count = 5.

% Only run through night 1 for this test
needs_night(1).

% For 5 players, execution is optional (skip execution constraint from inst.lp)
% In a real game, day 1 execution happens but we don't need it for role distribution test

#show distrib/1.
#show bag/1.
#show game_chair/2.
#show assigned/3.
