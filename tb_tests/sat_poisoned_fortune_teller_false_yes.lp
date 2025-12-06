% Test: Poisoned Fortune Teller can get false "yes"
% When malfunctioning, ST can say yes even if neither chosen is demon
#include "base.lp".

assigned(0, amanda, fortune_teller).  % chair 0
assigned(0, rob, chef).               % chair 1
assigned(0, taylor, virgin).          % chair 2
assigned(0, steph, imp).              % chair 4 - the demon
assigned(0, felix, poisoner).         % chair 5

% FT is poisoned
:- not reminder_on(poi_poisoned, amanda, _).

% FT chooses rob and taylor (neither is demon)
:- not ft_choice(amanda, rob, _).
:- not ft_choice(amanda, taylor, _).

% Red herring must not be on rob or taylor
:- reminder_on(ft_red_herring, rob, _).
:- reminder_on(ft_red_herring, taylor, _).

% Assert: ST can tell the poisoned FT "yes" (even though it's wrong)
:- not st_tells(fortune_teller, amanda, yes, _).
