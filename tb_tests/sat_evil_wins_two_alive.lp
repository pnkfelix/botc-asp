% Test: Evil wins when only 2 players are alive at start of day, with demon among them
% This tests the evil_wins rule in game_end.lp
%
% Scenario: 4 players
% - Start: felix (imp), alice (poisoner), bob (washerwoman), carol (chef)
% - Day 1: Execute alice (3 remaining)
% - Night 2: Imp kills bob (2 remaining at dawn 2)
% - Day 2, 0: 2 alive (felix=demon, carol=townsfolk) = EVIL WINS

#const player_count = 4.

#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

needs_night(2).

% Fixed seating
chair(felix, 0).
chair(alice, 1).
chair(bob, 2).
chair(carol, 3).

% Fixed role assignments
assigned(0, felix, imp).
assigned(0, alice, poisoner).
assigned(0, bob, washerwoman).
assigned(0, carol, chef).

% Day 1: Execute alice
executed(alice, 1).

% Night 2: Imp kills bob
player_chooses(imp, felix, point(bob), night(2, 4, 2)).

% Day 2, 0: 2 alive (felix=demon, carol=townsfolk)
% Evil should win at the start of day 2
:- not evil_wins(day(2, 0)).

% Also verify d_game_over event fires
:- not d_game_over(day(2, 0), evil).
