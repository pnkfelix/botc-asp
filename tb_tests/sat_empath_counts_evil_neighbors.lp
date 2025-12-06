% Test: Empath correctly counts evil neighbors
% Empath at chair 1 (rob), with poisoner at chair 0 (amanda)
% Rob's neighbors are amanda (0) and taylor (2)
% Amanda is evil, taylor is good, so empath gets count(1)
#include "base.lp".

assigned(0, amanda, poisoner).  % chair 0 - neighbor of empath
assigned(0, rob, empath).       % chair 1
assigned(0, taylor, chef).      % chair 2 - neighbor of empath (good)
assigned(0, steph, imp).        % chair 4 - not neighbor of empath

% Empath must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, rob, _).

% Assert: Empath is told count(1) - one evil neighbor (amanda)
:- not st_tells(empath, rob, count(1), _).
