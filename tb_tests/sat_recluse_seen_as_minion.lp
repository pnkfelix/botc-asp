% Test: Recluse CAN be seen as minion by a functioning investigator
% AND the ST can tell the investigator that felix is "the Baron"
#include "base.lp".

assigned(0, felix, recluse).
assigned(0, amanda, poisoner).
assigned(0, courtney, investigator).

% Investigator must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, courtney, _).

% Assert: investigator CAN see felix (recluse) as the minion
:- not reminder_on(inv_minion, felix, _).

% Assert: ST CAN tell the investigator that felix is "the Baron"
% (even though felix is actually a Recluse, she registers as minion)
:- not st_tells(investigator, courtney, info(_, _, baron), _).
