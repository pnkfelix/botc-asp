% Test: Evil wins when the Saint is executed
% The Saint's ability: "If you are executed, your team loses."
%
% Scenario: 6 players (allows 1 outsider)
% - felix (imp), amanda (poisoner), rob (washerwoman), taylor (chef), kim (empath), sam (saint)
% - Day 1: Town mistakenly executes sam (the Saint)
% - Evil wins at day(1, exec)

#const player_count = 6.

#include "../botc.lp".
#include "../tb.lp".

needs_night(1).

% Define the 6 players for this test
name(felix; amanda; rob; taylor; kim; sam).
chair(felix, 0).
chair(amanda, 1).
chair(rob, 2).
chair(taylor, 3).
chair(kim, 4).
chair(sam, 5).

% Fixed role assignments (1 demon, 1 minion, 3 townsfolk, 1 outsider)
assigned(0, felix, imp).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, kim, empath).
assigned(0, sam, saint).

% Saint must not be poisoned (so their ability is functioning)
:- reminder_on(poi_poisoned, sam, _).

% Day 1: Execute the Saint
executed(sam, 1).

% Evil should win at the execution
:- not evil_wins(day(1, exec)).

% Verify d_game_over event fires
:- not d_game_over(day(1, exec), evil).

% Verify game is over after the execution
:- not game_over(day(1, exec)).
