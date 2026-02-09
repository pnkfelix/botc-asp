% ============================================================================
% Recluse - Outsider
% You might register as evil & as a Minion or Demon, even if dead.
% ============================================================================

% Flag: Recluse bypasses the default registers_as logic (which would lock them to good/outsider)
may_mis_register(P) :- assigned(0, P, recluse).
% Each category the Recluse may falsely register as; separate atoms because ST chooses independently per query
may_misregister_as(P, evil) :- assigned(0, P, recluse).
may_misregister_as(P, minion) :- assigned(0, P, recluse).
may_misregister_as(P, demon) :- assigned(0, P, recluse).
