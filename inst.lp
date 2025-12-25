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
% without needing implementation details like timestamps or wrapper terms.
%
% Info values are unwrapped for convenience:
%   count(N) -> N      (chef, empath)
%   info(P1,P2,R) -> R (washerwoman, librarian, investigator)
%   point(R) -> R      (undertaker, ravenkeeper)
%   yes/no -> yes/no   (fortune_teller)
%
% Examples by role:
%   :- not st_tells(chef, 2).           % chef was told 2
%   :- not st_tells(empath, 1).         % empath was told 1
%   :- not st_tells(fortune_teller, yes). % FT was told yes
%   :- not st_tells(washerwoman, chef). % washerwoman was told "chef"
%   :- not st_tells(undertaker, imp).   % undertaker learned "imp"
%
% Can also query by player name:
%   :- not st_tells(amanda, 2).         % amanda was told 2
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
