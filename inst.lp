#include "botc.lp".
#include "tb.lp".
#include "players.lp".

#const player_count = 8.
assigned(0, felix, drunk).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, librarian).
assigned(0, courtney, investigator).

#show distrib/1.
#show assigned/3.
%#show received/2.
%#show time/1.
%#show next/2.
#show reminder_on/3.
%#show impaired/2.
#show registers_as/3.
