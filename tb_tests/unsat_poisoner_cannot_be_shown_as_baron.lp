% Test: A real poisoner CANNOT be shown as baron by functioning investigator
#include "base.lp".

assigned(0, amanda, poisoner).
assigned(0, courtney, investigator).

% Investigator must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, courtney, _).

% Force the inv_minion token on amanda
:- not reminder_on(inv_minion, amanda, _).

% Assert: ST CANNOT tell the investigator that amanda is "the Baron"
% (If this is SAT, the test fails - poisoner shouldn't be claimable as baron)
:- not st_tells_core(investigator, courtney, point(baron), _).
