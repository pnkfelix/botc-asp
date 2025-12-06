% Test: A player cannot nominate twice in the same day
% This should be UNSAT
#include "base.lp".

assigned(0, amanda, chef).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% rob tries to nominate twice
nomination(rob, steph, 1, 1).
nomination(rob, felix, 1, 2).

st_ends_day(1).
