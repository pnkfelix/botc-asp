% Test: Player is executed when they receive majority votes
% 8 players, need 5 votes for majority (8/2 + 1 = 5)
#include "base.lp".

assigned(0, amanda, chef).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Day 1: rob nominates steph
nomination(rob, steph, 1, 1).

% steph gets 5 votes (majority)
vote(amanda, 1, 1).
vote(rob, 1, 1).
vote(taylor, 1, 1).
vote(felix, 1, 1).
vote(courtney, 1, 1).

% ST ends the day
st_ends_day(1).

% Assert: steph should be executed
:- not executed(steph, 1).

% Assert: steph should be dead at end of day
:- alive(steph, day(1, exec)).
