#include "botc.lp".
#include "tb.lp".
#include "players.lp".

#const player_count = 8.
% We are demoing three nights, so we write needs_night(3).
needs_night(3).
assigned(0, felix, drunk).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, librarian).
assigned(0, courtney, investigator).

% ===========================================================================
% Example: Player-visible constraints using simplified st_tells
% ===========================================================================
% The simplified st_tells predicates let you express what players observed
% without needing implementation details like timestamps or wrapper terms.
%
% Info values are unwrapped for convenience:
%   count(N) -> N      (chef, empath)
%   info(P1,P2,R) -> R (washerwoman, librarian, investigator)
%   point(R) -> R      (undertaker, ravenkeeper)
%   yes/no -> yes/no   (fortune_teller)
%
% --- st_tells/2: for one-time info or when night doesn't matter ---
%   :- not st_tells(chef, 2).             % chef was told 2
%   :- not st_tells(washerwoman, chef).   % washerwoman was told "chef"
%   :- not st_tells(undertaker, imp).     % undertaker learned "imp"
%   :- not st_tells(fortune_teller, yes). % FT was told yes (at some point)
%
% --- st_tells/3: includes night number for recurring info roles ---
%   :- not st_tells(empath, 1, 2).          % empath told 1 on night 2
%   :- not st_tells(fortune_teller, yes, 1). % FT told yes on night 1
%   :- not st_tells(fortune_teller, no, 3).  % FT told no on night 3
%   :- not st_tells(undertaker, chef, 2).   % undertaker learned chef on night 2
%
% Can also query by player name:
%   :- not st_tells(amanda, 2).      % amanda was told 2
%   :- not st_tells(amanda, 1, 2).   % amanda was told 1 on night 2
%
% These derive from st_tells_core(Role, Player, Info, Time), the detailed form.
% ===========================================================================

#show distrib/1.
#show assigned/3.
#show received/2.
%#show time/1.
%#show next/2.
#show reminder_on/3.
%#show impaired/2.
#show registers_as/3.

% For grimoire/timeline rendering
#show game_chair/2.
#show alive/2.
#show st_tells/4.
#show player_chooses/4.
