% Test: A real drunk CANNOT be shown as butler by functioning librarian
#include "base.lp".

assigned(0, felix, drunk).
assigned(0, taylor, librarian).

% Librarian must not be poisoned (so they're functioning)
:- reminder_on(poi_poisoned, taylor, _).

% Force the lib_outsider token on felix
:- not reminder_on(lib_outsider, felix, _).

% Assert: ST CANNOT tell the librarian that felix is "the Butler"
% (If this is SAT, the test fails - drunk shouldn't be claimable as butler)
:- not st_tells_core(librarian, taylor, point(butler), _).
