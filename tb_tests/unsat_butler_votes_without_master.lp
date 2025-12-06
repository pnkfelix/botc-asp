% Test: Butler cannot vote when master doesn't vote
% This should be UNSAT
#include "base.lp".

assigned(0, amanda, butler).      % chair 0 - the butler
assigned(0, rob, washerwoman).    % chair 1 - butler's master
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Butler chooses rob as master
:- not player_chooses(butler, amanda, point(rob), _).

% Day 1: taylor nominates steph
nomination(taylor, steph, 1, 1).

% Butler (amanda) tries to vote, but master (rob) doesn't
vote(amanda, 1, 1).
% rob does NOT vote

st_ends_day(1).
