% Test: Functioning undertaker can learn the executed player's role
% This SAT test shows the scenario is possible
#include "base.lp".

assigned(0, amanda, undertaker).
assigned(0, rob, chef).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

night_number(2).
day_number(2).

% Rob (chef) was executed on day 1
executed(rob, 1).

% Amanda is not poisoned
:- reminder_on(poi_poisoned, amanda, _).

% Amanda is not killed tonight (must be alive at undertaker time)
:- reminder_on(imp_dead, amanda, _).

% Assert: A model exists where undertaker is told chef
:- not st_tells_core(undertaker, amanda, point(chef), _).
