% Test: No execution when votes don't reach majority
% 8 players, need 5 votes for majority
#include "base.lp".

assigned(0, amanda, chef).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Day 1: rob nominates steph
nomination(rob, steph, 1, 1).

% steph gets only 4 votes (not majority)
vote(amanda, 1, 1).
vote(rob, 1, 1).
vote(taylor, 1, 1).
vote(felix, 1, 1).

% ST ends the day
st_ends_day(1).

% Assert: no one should be executed
:- executed(_, 1).

% Assert: no_execution should be derived
:- not no_execution(1).

% Assert: steph should still be alive
:- not alive(steph, day(1, exec)).
