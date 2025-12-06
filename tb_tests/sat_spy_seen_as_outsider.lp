% Test: Spy CAN be seen as outsider by a functioning librarian
% AND the ST can tell the librarian that the spy is "the Drunk"
#include "base.lp".

assigned(0, amanda, spy).
assigned(0, taylor, librarian).

% Librarian must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, taylor, _).

% Assert: librarian CAN see amanda (spy) as the outsider
:- not reminder_on(lib_outsider, amanda, _).

% Assert: ST CAN tell the librarian that amanda is "the Drunk"
% (because amanda is a Spy who can misregister as outsider)
:- not st_tells(librarian, taylor, point(drunk), _).
