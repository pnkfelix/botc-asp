% Test: Red herring cannot be placed on the demon
% This should be UNSAT because the constraint forbids this
#include "base.lp".

assigned(0, amanda, fortune_teller).  % chair 0
assigned(0, rob, chef).               % chair 1
assigned(0, taylor, virgin).          % chair 2
assigned(0, steph, imp).              % chair 4 - the demon
assigned(0, felix, poisoner).         % chair 5

% Try to force red herring on the demon
:- not reminder_on(ft_red_herring, steph, _).
