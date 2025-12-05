% Test: Recluse CAN be seen as minion by a functioning investigator
#include "base.lp".

assigned(0, felix, recluse).
assigned(0, amanda, poisoner).
assigned(0, courtney, investigator).

% Investigator must not be poisoned (so they're functioning)
:- reminder_on(poisoner_poisoned, courtney, _).

% Assert: investigator CAN see felix (recluse) as the minion
:- not reminder_on(investigator_minion, felix, _).
