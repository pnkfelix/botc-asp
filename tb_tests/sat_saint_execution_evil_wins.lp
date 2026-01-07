% Test: Evil wins when the Saint is executed
% The Saint's ability: "If you are executed, your team loses."
%
% Scenario: 5 players
% - felix (imp), amanda (poisoner), rob (washerwoman), taylor (chef), sam (saint)
% - Day 1: Town mistakenly executes sam (the Saint)
% - Evil wins at day(1, exec)

#const player_count = 5.

#include "../botc.lp".
#include "../tb.lp".

needs_night(1).

% Define the 5 players for this test
name(felix; amanda; rob; taylor; sam).
chair(felix, 0).
chair(amanda, 1).
chair(rob, 2).
chair(taylor, 3).
chair(sam, 4).

% Fixed role assignments
assigned(0, felix, imp).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, sam, saint).

% Saint must not be poisoned (so their ability is functioning)
:- poisoned(sam, _).

% Day 1: Execute the Saint
executed(sam, 1).

% Evil should win at the execution
:- not evil_wins(day(1, exec)).

% Verify d_game_over event fires
:- not d_game_over(day(1, exec), evil).

% Verify game is over after the execution
:- not game_over(day(1, exec)).
