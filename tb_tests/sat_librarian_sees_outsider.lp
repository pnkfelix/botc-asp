% Test: Functioning librarian sees an actual outsider
% AND the ST can tell the librarian that felix is "the Drunk"
#include "base.lp".

assigned(0, felix, drunk).
assigned(0, amanda, poisoner).
assigned(0, taylor, librarian).

% Librarian must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, taylor, _).

% Assert: librarian sees felix (drunk/outsider) as the outsider
:- not reminder_on(lib_outsider, felix, _).

% Assert: ST CAN tell the librarian that felix is "the Drunk"
:- not st_tells(librarian, taylor, point(drunk), _).
