% Test: Functioning investigator sees an actual minion
% AND the ST must tell the investigator the actual role (poisoner)
#include "base.lp".

assigned(0, amanda, poisoner).
assigned(0, courtney, investigator).

% Investigator must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, courtney, _).

% Assert: investigator sees amanda (poisoner) as the minion
:- not reminder_on(inv_minion, amanda, _).

% Assert: ST must tell the investigator that amanda is "the Poisoner"
% (since amanda is a real minion, not a recluse who can misregister)
:- not st_tells(investigator, courtney, info(_, _, poisoner), _).
