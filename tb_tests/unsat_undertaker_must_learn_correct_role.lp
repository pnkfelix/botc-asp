% Test: Functioning undertaker MUST learn the correct role
% This UNSAT test proves: if not poisoned, undertaker cannot be told wrong role
% Combined with sat_undertaker_learns_executed_role.lp, this proves:
%   not poisoned -> must be told correct role
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

% Assert the negation: undertaker is NOT told chef
% UNSAT proves this is impossible when functioning and alive
:- st_tells_core(undertaker, amanda, point(chef), _).
