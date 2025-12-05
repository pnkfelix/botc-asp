#include "botc.lp".
#include "tb.lp".
#include "players.lp".

#const player_count = 8.
assigned(0, felix, drunk).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).

% Force poisoner to poison felix (not the washerwoman)
reminder_on(poisoner_poisoned, felix, night(1, 1, 3)).

% Try to force the washerwoman to see the poisoner as the townsfolk (should be UNSAT)
reminder_on(washerwoman_townsfolk, amanda, night(1, 2, 1)).

#show distrib/1.
#show assigned/3.
%#show received/2.
%#show time/1.
%#show next/2.
#show reminder_on/3.
%#show impaired/2.
#show registers_as/3.
