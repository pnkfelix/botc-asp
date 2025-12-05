% Test: Functioning librarian sees an actual outsider
#include "base.lp".

assigned(0, felix, drunk).
assigned(0, amanda, poisoner).
assigned(0, taylor, librarian).

% Librarian must not be poisoned (so they're functioning)
:- reminder_on(poisoner_poisoned, taylor, _).

% Assert: librarian sees felix (drunk/outsider) as the outsider
:- not reminder_on(librarian_outsider, felix, _).
