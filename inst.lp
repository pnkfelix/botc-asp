#include "botc.lp".
#include "tb.lp".
#include "players.lp".

#const player_count = 8.
#const night_count = 1.
assigned(0, felix, drunk).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, librarian).
assigned(0, courtney, investigator).

% ===========================================================================
% Example: Player-visible constraints using simplified st_tells
% ===========================================================================
% The simplified st_tells/2 predicates let you express what players observed
% without needing implementation details like exact timestamps.
%
% st_tells(Role, Info) - "the role was told Info"
%   Example: :- not st_tells(chef, count(2)).
%            Constraint: the chef must have been told count 2
%
% st_tells(Player, Info) - "the player was told Info"
%   Example: :- not st_tells(amanda, count(2)).
%            Constraint: amanda must have been told count 2
%
% These derive from st_tells_core(Role, Player, Info, Time), the detailed form.
% ===========================================================================

#show distrib/1.
#show assigned/3.
%#show received/2.
%#show time/1.
%#show next/2.
#show reminder_on/3.
%#show impaired/2.
#show registers_as/3.
