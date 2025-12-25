% Test: Fortune Teller correctly detects the demon
% FT chooses 2 players, one of which is the demon - should get "yes"
#include "base.lp".

assigned(0, amanda, fortune_teller).  % chair 0
assigned(0, rob, chef).               % chair 1
assigned(0, taylor, virgin).          % chair 2
assigned(0, steph, imp).              % chair 4 - the demon
assigned(0, felix, poisoner).         % chair 5

% FT must not be poisoned
:- reminder_on(poi_poisoned, amanda, _).

% FT chooses steph (demon) and rob (not demon)
:- not ft_choice(amanda, steph, _).
:- not ft_choice(amanda, rob, _).

% Assert: FT should be told "yes" (one of the chosen is the demon)
:- not st_tells_core(fortune_teller, amanda, yes, _).
