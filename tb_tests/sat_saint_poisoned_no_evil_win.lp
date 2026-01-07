% Test: Poisoned Saint execution does NOT cause evil to win
% The Saint's ability only triggers if they are functioning (not poisoned).
%
% Scenario: 5 players
% - felix (imp), amanda (poisoner), rob (washerwoman), taylor (chef), sam (saint)
% - Night 1: Poisoner poisons sam (the Saint)
% - Day 1: Town executes sam (the Saint), but Saint is poisoned
% - Evil does NOT win because Saint's ability is not functioning
% - Game continues normally

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

% Night 1: Poisoner poisons the Saint
% Poisoner is role order 1, player_chooses is substep 2
player_chooses(poisoner, amanda, point(sam), night(1, 1, 2)).

% Verify Saint has poison token during day 1
:- not reminder_on(poi_poisoned, sam, day(1, 0)).
:- not reminder_on(poi_poisoned, sam, day(1, exec)).

% Day 1: Execute the Saint (while poisoned)
executed(sam, 1).

% Evil should NOT win at execution (Saint is poisoned, ability not functioning)
:- evil_wins(day(1, exec)).

% Game should not be over yet (demon still alive, more than 2 players)
:- game_over(day(1, exec)).
