% ============================================================================
% Recluse - Outsider
% You might register as evil & as a Minion or Demon, even if dead.
% ============================================================================

% Recluse can register as evil and/or as minion/demon
% (Recluse is already good and an outsider, so no misregistration needed for those)
may_mis_register(P) :- assigned(0, P, recluse).
may_misregister_as(P, evil) :- assigned(0, P, recluse).
may_misregister_as(P, minion) :- assigned(0, P, recluse).
may_misregister_as(P, demon) :- assigned(0, P, recluse).
