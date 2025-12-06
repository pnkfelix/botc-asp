% Test: Dead players cannot nominate
% This should be UNSAT because we try to have a dead player nominate
#include "base.lp".

assigned(0, amanda, chef).
assigned(0, rob, washerwoman).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

% Kill steph during night 1 (demon attack - we'll fake it)
died(steph, night(1, 0, 0)).

% Try to have dead steph nominate on day 1 - should fail constraint
nomination(steph, rob, 1, 1).
st_ends_day(1).
