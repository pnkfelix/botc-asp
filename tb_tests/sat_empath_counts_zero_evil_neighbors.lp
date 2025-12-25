% Test: Empath correctly counts zero evil neighbors
% Empath at chair 1 (rob), with good neighbors on both sides
% Rob's neighbors are amanda (0) and taylor (2), both good
#include "base.lp".

assigned(0, amanda, chef).      % chair 0 - neighbor of empath (good)
assigned(0, rob, empath).       % chair 1
assigned(0, taylor, virgin).    % chair 2 - neighbor of empath (good)
assigned(0, steph, imp).        % chair 4 - not neighbor of empath
assigned(0, felix, poisoner).   % chair 5 - not neighbor of empath

% Empath must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, rob, _).

% Assert: Empath is told count(0) - no evil neighbors
:- not st_tells_core(empath, rob, count(0), _).
