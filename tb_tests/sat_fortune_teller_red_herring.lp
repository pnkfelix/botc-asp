% Test: Fortune Teller's red herring triggers "yes"
% FT chooses 2 players including the red herring - should get "yes"
#include "base.lp".

assigned(0, amanda, fortune_teller).  % chair 0
assigned(0, rob, chef).               % chair 1 - red herring
assigned(0, taylor, virgin).          % chair 2
assigned(0, steph, imp).              % chair 4 - the demon
assigned(0, felix, poisoner).         % chair 5

% FT must not be poisoned
:- reminder_on(poi_poisoned, amanda, _).

% Red herring is on rob
:- not reminder_on(ft_red_herring, rob, _).

% FT chooses rob (red herring) and taylor (not demon)
:- not ft_choice(amanda, rob, _).
:- not ft_choice(amanda, taylor, _).

% Assert: FT should be told "yes" (red herring pings as demon)
:- not st_tells(fortune_teller, amanda, yes, _).
