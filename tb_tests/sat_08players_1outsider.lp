% Test: 8 players should have exactly 1 outsider (base)
#include "base.lp".

% Force exactly 1 outsider
assigned(0, felix, drunk).

% No baron (to ensure it's base count)
:- assigned(0, _, baron).
