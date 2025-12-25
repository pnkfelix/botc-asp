% Test: Fortune Teller correctly gets "no" when neither chosen is demon
% FT chooses 2 players, neither is the demon or red herring
#include "base.lp".

assigned(0, amanda, fortune_teller).  % chair 0
assigned(0, rob, chef).               % chair 1
assigned(0, taylor, virgin).          % chair 2
assigned(0, steph, imp).              % chair 4 - the demon
assigned(0, felix, poisoner).         % chair 5

% FT must not be poisoned
:- reminder_on(poi_poisoned, amanda, _).

% FT chooses rob and taylor (neither is demon)
:- not ft_choice(amanda, rob, _).
:- not ft_choice(amanda, taylor, _).

% Red herring must not be on rob or taylor
:- reminder_on(ft_red_herring, rob, _).
:- reminder_on(ft_red_herring, taylor, _).

% Assert: FT should be told "no"
:- not st_tells_core(fortune_teller, amanda, no, _).
