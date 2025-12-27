% Test: Washerwoman token inertia should not cause constraint violation
%
% Scenario:
% - Washerwoman is poisoned on night 1, so she gets false info
% - ST places ww_townsfolk token on the poisoner (a minion, not townsfolk)
% - Poisoner is executed on day 1
% - On night 2, washerwoman is no longer poisoned (functioning)
% - The ww_townsfolk token still exists via inertia on the (now dead) poisoner
%
% Bug (before fix): The old constraint checked token validity at ALL times,
% so on night 2 when washerwoman is functioning and the token is on a minion,
% it would incorrectly fail.
%
% Fix: The constraint now only applies at the moment of info delivery (night 1),
% so this scenario should be SATISFIABLE.

#include "base.lp".

#const night_count = 2.
#const day_count = 1.

assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, neha, imp).

% Poisoner poisons the washerwoman on night 1
:- not player_chooses(poisoner, amanda, point(rob), night(1, _, 2)).

% Washerwoman must be poisoned (constraint to verify setup)
:- not reminder_on(poi_poisoned, rob, night(1, _, _)).

% ST places ww_townsfolk on the poisoner (only valid because washerwoman is poisoned)
:- not reminder_on(ww_townsfolk, amanda, _).

% Poisoner is executed on day 1
executed(amanda, 1).

% Verify washerwoman is functioning on night 2 (no longer poisoned since poisoner is dead)
:- not functioning(rob, night(2, _, _)).

% The test should be SATISFIABLE - the token inertia should not cause problems
