% Test: Butler can vote when master votes
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

% Master (rob) votes, so butler (amanda) can vote too
vote(rob, 1, 1).
vote(amanda, 1, 1).

st_ends_day(1).

% This should be satisfiable
