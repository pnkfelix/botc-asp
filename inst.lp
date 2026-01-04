#include "botc.lp".
#include "tb.lp".
#include "players.lp".

#const player_count = 8.
% We are demoing three nights, so we write needs_night(3).
needs_night(3).
% These roles must be in play, but the solver chooses which players get them
% assert_distrib(drunk).
% assert_distrib(poisoner).
% assert_distrib(washerwoman).
% assert_distrib(librarian).
% assert_distrib(investigator).

% Anchor a few players to specific tokens to reduce solution jumping
% received(felix, chef).
% received(amanda, poisoner).
% received(rob, washerwoman).

% Town executes exactly one living player at the end of each day
% (unless the game has already ended at the start of this day)
{ executed(P, N) : alive(P, day(N, 0)) } = 1 :- day_number(N), not game_over(day(N, 0)).

% ===========================================================================
% House rules: Discourage unrealistic but technically legal states
% These aren't strictly against the rules, but produce uninteresting models.
% Comment out if you need to explore these edge cases.
% ===========================================================================

% Info roles should not see themselves in their pair of players
% (gives zero new information - a good ST would never do this)
% NOTE: These constraints are now defined generically in tb.lp via never_applied_to/2

% Poisoner should not target evil players (defeats the purpose)
:- player_chooses(poisoner, _, point(Target), _), actual_category(Target, evil).

% Demon should target living players at night, not already dead ones
% (Self-targeting for starpass is still allowed)
% Note: Special cases like Shabbaloth will need separate handling
:- player_chooses(imp, Imp, point(Target), T), Target != Imp, not alive(Target, T).

% st_tells predicates for player-observed info (unwrapped for convenience):
%   st_tells(role_or_player, value)           e.g. st_tells(chef, 2)
%   st_tells(role_or_player, value, night)    e.g. st_tells(empath, 1, 2)

#show distrib/1.
#show bag/1.
#show bluff/1.
%#show time/1.
%#show next/2.
%#show impaired/2.
#show registers_as/3.

% For grimoire rendering (player state and positions)
#show game_chair/2.
#show assigned/3.
#show received/2.
#show reminder_on/3.

% Delta predicates (d_ prefix = events/changes, not ongoing state)
% Parser derives alive/dead state from d_died events
#show d_st_tells/4.
#show d_player_chooses/4.
#show d_executed/2.
#show d_died/2.
#show d_death_announced/2.
#show d_game_over/2.

% Show dawn time points so they appear in the timeline
#show time(dawn(N)) : day_number(N).
