#include "botc.lp".
#include "tb.lp".
#include "players.lp".
#include "preferences.lp".

#const player_count = 8.
% We are demoing three nights, so we write needs_night(3).
needs_night(3).
% These roles must be in play, but the solver chooses which players get them
assert_distrib(drunk; poisoner; washerwoman; librarian; investigator).

% Anchor a few players to specific tokens to reduce solution jumping
received(felix, chef).
received(amanda, poisoner).
received(rob, washerwoman).

% Town executes exactly one living player at the end of each day
{ executed(P, N) : alive(P, day(N, 0)) } = 1 :- day_number(N).

% st_tells predicates for player-observed info (unwrapped for convenience):
%   st_tells(role_or_player, value)           e.g. st_tells(chef, 2)
%   st_tells(role_or_player, value, night)    e.g. st_tells(empath, 1, 2)

#show distrib/1.
#show assigned/3.
#show received/2.
%#show time/1.
%#show next/2.
#show reminder_on/3.
%#show impaired/2.
#show registers_as/3.

% For grimoire rendering (seating positions)
#show game_chair/2.

% Delta predicates (d_ prefix = events/changes, not ongoing state)
% Parser derives alive/dead state from d_died events
#show d_st_tells/4.
#show d_player_chooses/4.
#show d_executed/2.
#show d_died/2.
