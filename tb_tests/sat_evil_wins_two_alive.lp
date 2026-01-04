% Test: Evil wins when only 2 players are alive at start of day, with demon among them
% This tests the evil_wins rule in game_end.lp
%
% Scenario: 4 players
% - Start: felix (imp), amanda (poisoner), rob (washerwoman), taylor (chef)
% - Day 1: Execute amanda (3 remaining)
% - Night 2: Imp kills rob (2 remaining at dawn 2)
% - Day 2, 0: 2 alive (felix=demon, taylor=townsfolk) = EVIL WINS

#const player_count = 4.

#include "../botc.lp".
#include "../tb.lp".
% Note: Don't include players.lp - we define exactly the 4 players needed

needs_night(2).

% Define only the 4 players for this test (no extra players from players.lp)
name(felix; amanda; rob; taylor).
chair(felix, 0).
chair(amanda, 1).
chair(rob, 2).
chair(taylor, 3).

% Fixed role assignments
assigned(0, felix, imp).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).

% Day 1: Execute amanda
executed(amanda, 1).

% Night 2: Imp kills rob
player_chooses(imp, felix, point(rob), night(2, 4, 2)).

% Day 2, 0: 2 alive (felix=demon, taylor=townsfolk)
% Evil should win at the start of day 2
:- not evil_wins(day(2, 0)).

% Also verify d_game_over event fires
:- not d_game_over(day(2, 0), evil).
