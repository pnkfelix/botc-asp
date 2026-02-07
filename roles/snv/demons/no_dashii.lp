% No Dashii
% Each night*, choose a player: they die. Your 2 Townsfolk neighbours are poisoned.

other_night_substep(no_dashii, 1, st_asks(choose_target)).
other_night_substep(no_dashii, 2, player_chooses(kill_target)).
other_night_substep(no_dashii, 3, st_places(no_dashii_dead)).

% Choice rule: ND picks exactly one target each other-night (choice cannot be deterministic).
{ player_chooses(no_dashii, ND, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(no_dashii, RoleOrd),
    character_assignment_state_at_time(T, ND, no_dashii),
    alive(ND, T),
    game_active(night(N, 0, 0)).

% Places death token; deterministic derivation from choice, so separate from the choice rule above.
reminder_on(no_dashii_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(no_dashii, RoleOrd),
    player_chooses(no_dashii, ND, point(Target), night(N, RoleOrd, 2)),
    functioning(ND, night(N, RoleOrd, 3)).

demon_kill_token(no_dashii_dead).

% Projects reminder into script-level snv_demon_killed/2; uses night-number (not full time) so other roles (e.g. Sage) can query it.
snv_demon_killed(Target, N) :-
    night_number(N), N > 1,
    other_night_role_order(no_dashii, RoleOrd),
    reminder_on(no_dashii_dead, Target, night(N, RoleOrd, 3)).

% State-change times for neighbor recomputation; three rules needed because night/dawn/day have different functors.
nd_state_time(night(N, 0, 0)) :- night_number(N).
nd_state_time(dawn(N)) :- day_number(N).
nd_state_time(day(N, exec)) :- day_number(N).

% Nearest clockwise townsfolk neighbor; uses negation of "closer exists" pattern, requiring the helper below.
nd_townsfolk_neighbor_clockwise(ND, P, T) :-
    assigned(0, ND, no_dashii),
    nd_state_time(T),
    alive(ND, T),
    player(P), P != ND,
    alive(P, T),
    assigned(0, P, R), townsfolk(R),
    clockwise_distance(ND, P, D),
    not nd_closer_townsfolk_clockwise(ND, D, T).

% Auxiliary for clockwise minimum: witnesses a strictly closer townsfolk; can't fold into neighbor rule due to negation stratification.
nd_closer_townsfolk_clockwise(ND, D, T) :-
    assigned(0, ND, no_dashii),
    nd_state_time(T),
    alive(ND, T),
    player(P), P != ND,
    alive(P, T),
    assigned(0, P, R), townsfolk(R),
    clockwise_distance(ND, P, D),
    player(P2), P2 != ND, P2 != P,
    alive(P2, T),
    assigned(0, P2, R2), townsfolk(R2),
    clockwise_distance(ND, P2, D2),
    D2 < D.

% Nearest counter-clockwise townsfolk neighbor; separate from clockwise because distance direction is reversed (P->ND not ND->P).
nd_townsfolk_neighbor_counterclockwise(ND, P, T) :-
    assigned(0, ND, no_dashii),
    nd_state_time(T),
    alive(ND, T),
    player(P), P != ND,
    alive(P, T),
    assigned(0, P, R), townsfolk(R),
    clockwise_distance(P, ND, D),
    not nd_closer_townsfolk_counterclockwise(ND, D, T).

% Auxiliary for counter-clockwise minimum; mirrors clockwise helper but with reversed distance argument order.
nd_closer_townsfolk_counterclockwise(ND, D, T) :-
    assigned(0, ND, no_dashii),
    nd_state_time(T),
    alive(ND, T),
    player(P), P != ND,
    alive(P, T),
    assigned(0, P, R), townsfolk(R),
    clockwise_distance(P, ND, D),
    player(P2), P2 != ND, P2 != P,
    alive(P2, T),
    assigned(0, P2, R2), townsfolk(R2),
    clockwise_distance(P2, ND, D2),
    D2 < D.

% Poison for clockwise neighbor; two separate reminder rules needed because clockwise and counter-clockwise are distinct predicates.
reminder_on(nd_poisoned, P, T) :-
    nd_state_time(T),
    nd_townsfolk_neighbor_clockwise(ND, P, T),
    alive(ND, T).

% Poison for counter-clockwise neighbor; can't merge with clockwise rule since the neighbor predicates differ.
reminder_on(nd_poisoned, P, T) :-
    nd_state_time(T),
    nd_townsfolk_neighbor_counterclockwise(ND, P, T),
    alive(ND, T).

% Removes poison when P is no longer a neighbor (ND died or seating changed); needs negation of both directions so can't fold into placement rules.
token_removed(nd_poisoned, P, T) :-
    nd_state_time(T),
    reminder_on(nd_poisoned, P, TPrev),
    next(TPrev, T),
    not nd_townsfolk_neighbor_clockwise(_, P, T),
    not nd_townsfolk_neighbor_counterclockwise(_, P, T).

causes_impairment(nd_poisoned).
