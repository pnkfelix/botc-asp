% Test: When votes tie, no one is executed
% 8 players, need 5 votes for majority
#include "base.lp".

assigned(0, amanda, chef).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Day 1: two nominations with equal votes
nomination(rob, steph, 1, 1).
nomination(amanda, felix, 1, 2).

% steph gets 5 votes
vote(amanda, 1, 1).
vote(rob, 1, 1).
vote(taylor, 1, 1).
vote(felix, 1, 1).
vote(courtney, 1, 1).

% felix also gets 5 votes (tie!)
vote(amanda, 1, 2).
vote(rob, 1, 2).
vote(taylor, 1, 2).
vote(steph, 1, 2).
vote(courtney, 1, 2).

% ST ends the day
st_ends_day(1).

% Assert: no one should be executed due to tie
:- executed(_, 1).
:- not no_execution(1).
