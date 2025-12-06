% Test: When multiple nominations have majority, highest vote count wins
% 8 players, need 5 votes for majority
#include "base.lp".

assigned(0, amanda, chef).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Day 1: two nominations
nomination(rob, steph, 1, 1).    % steph gets 5 votes
nomination(amanda, felix, 1, 2). % felix gets 6 votes

% steph gets 5 votes (majority)
vote(amanda, 1, 1).
vote(rob, 1, 1).
vote(taylor, 1, 1).
vote(felix, 1, 1).
vote(courtney, 1, 1).

% felix gets 6 votes (higher majority)
vote(amanda, 1, 2).
vote(rob, 1, 2).
vote(taylor, 1, 2).
vote(steph, 1, 2).
vote(courtney, 1, 2).
vote(neha, 1, 2).

% ST ends the day
st_ends_day(1).

% Assert: felix should be executed (higher vote count)
:- not executed(felix, 1).
:- executed(steph, 1).
