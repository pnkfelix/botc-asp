% Test: Spy can register as townsfolk to washerwoman AND outsider to librarian
% AND minion (spy) to investigator, all in the same night
#include "base.lp".

assigned(0, amanda, spy).
assigned(0, rob, washerwoman).
assigned(0, taylor, librarian).
assigned(0, courtney, investigator).

% All info roles must be functioning
:- reminder_on(poi_poisoned, rob, _).
:- reminder_on(poi_poisoned, taylor, _).
:- reminder_on(poi_poisoned, courtney, _).

% Assert: Washerwoman CAN see amanda (spy) as the townsfolk
:- not reminder_on(ww_townsfolk, amanda, _).

% Assert: Librarian CAN see amanda (spy) as the outsider
:- not reminder_on(lib_outsider, amanda, _).

% Assert: Investigator CAN see amanda (spy) as the minion
:- not reminder_on(inv_minion, amanda, _).

% Assert: ST can tell each info role a different role for amanda
:- not st_tells(washerwoman, rob, point(chef), _).
:- not st_tells(librarian, taylor, point(drunk), _).
:- not st_tells(investigator, courtney, point(spy), _).
