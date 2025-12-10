% Test: Undertaker gets no info when no execution occurred
% When no one is executed, undertaker cannot receive any st_tells
#include "base.lp".

assigned(0, amanda, undertaker).
assigned(0, rob, chef).
assigned(0, taylor, virgin).
assigned(0, steph, imp).
assigned(0, felix, poisoner).

night_number(2).
day_number(2).

% No execution on day 1

% Assert: Trying to force undertaker to get info should be UNSAT
:- not st_tells(undertaker, amanda, _, _).
