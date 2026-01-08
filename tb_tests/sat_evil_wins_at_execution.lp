% Test: Evil wins at execution time when only 2 players remain alive
% This tests the evil_wins(day(N, exec)) rule in game_end.lp
%
% Scenario: 5 players start, 3 deaths during night/day, then execution brings to 2
% - Start: felix (imp), amanda (poisoner), rob (washerwoman), taylor (chef), steph (saint)
% - Day 1: Execute rob (4 remaining)
% - Night 2: Imp kills taylor (3 remaining at dawn 2)
% - Day 2: 3 alive - game does NOT end
% - Day 2: Execute steph (not the demon) -> 2 remaining at day(2, exec)
% - Evil should win at day(2, exec) because only 2 alive and demon is one of them

#const player_count = 5.

#include "../botc.lp".
#include "../tb.lp".

needs_night(2).

% Define exactly 5 players for this test
name(felix; amanda; rob; taylor; steph).
chair(felix, 0).
chair(amanda, 1).
chair(rob, 2).
chair(taylor, 3).
chair(steph, 4).

% Fixed role assignments
assigned(0, felix, imp).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, steph, ravenkeeper).

% Day 1: Execute rob
executed(rob, 1).

% Night 2: Imp kills taylor
player_chooses(imp, felix, point(taylor), night(2, 4, 2)).

% Day 2: 3 alive (felix, amanda, steph)
% Execute steph (not the demon)
executed(steph, 2).

% At day(2, exec): 2 alive (felix=demon, amanda=minion)
% Evil should win at execution time, NOT at day 3 start
:- not evil_wins(day(2, exec)).

% Verify d_game_over event fires at the right time
:- not d_game_over(day(2, exec), evil).

% Game should be over so day 3 shouldn't be active
:- not game_over(night(3, 0, 0)).
