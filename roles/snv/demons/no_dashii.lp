% No Dashii
% Each night*, choose a player: they die. Your 2 Townsfolk neighbours are poisoned.

other_night_substep(no_dashii, 1, st_asks(choose_target)).
other_night_substep(no_dashii, 2, player_chooses(kill_target)).
other_night_substep(no_dashii, 3, st_places(no_dashii_dead)).

% No Dashii chooses a target each night (nights after the first)
{ player_chooses(no_dashii, ND, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(no_dashii, RoleOrd),
    current_demon(ND, N),
    assigned(0, ND, no_dashii),
    alive(ND, night(N, 0, 0)),
    game_active(night(N, 0, 0)).

% Place the death token on the target
reminder_on(no_dashii_dead, Target, night(N, RoleOrd, 3)) :-
    player_chooses(no_dashii, ND, point(Target), night(N, RoleOrd, 2)),
    other_night_role_order(no_dashii, RoleOrd),
    functioning(ND, night(N, RoleOrd, 3)).

snv_demon_killed(Target, T) :-
    reminder_on(no_dashii_dead, Target, T).

no_dashii_kill_succeeds(P, T) :-
    reminder_on(no_dashii_dead, P, T),
    not protected_from_demon(P, T).

dead_at_time(P, T) :- no_dashii_kill_succeeds(P, T).

died(P, T) :-
    final_night_time(N, T),
    no_dashii_kill_succeeds(P, T),
    alive(P, night(N, 0, 0)).

% Find the No Dashii's townsfolk neighbors
% A "townsfolk neighbor" is the closest living Townsfolk in each direction

% Clockwise townsfolk neighbor
nd_townsfolk_neighbor_clockwise(ND, P, T) :-
    assigned(0, ND, no_dashii),
    time(T),
    alive(ND, T),
    player(P), P != ND,
    alive(P, T),
    assigned(0, P, R), townsfolk(R),
    clockwise_distance(ND, P, D),
    not nd_closer_townsfolk_clockwise(ND, D, T).

nd_closer_townsfolk_clockwise(ND, D, T) :-
    assigned(0, ND, no_dashii),
    time(T),
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

% Counter-clockwise townsfolk neighbor
nd_townsfolk_neighbor_counterclockwise(ND, P, T) :-
    nd_townsfolk_neighbor_clockwise(P, ND, T),
    assigned(0, ND, no_dashii),
    assigned(0, P, R), townsfolk(R).

% The two townsfolk neighbors are poisoned while No Dashii is alive
reminder_on(nd_poisoned, P, T) :-
    nd_townsfolk_neighbor_clockwise(ND, P, T),
    alive(ND, T).

reminder_on(nd_poisoned, P, T) :-
    nd_townsfolk_neighbor_counterclockwise(ND, P, T),
    alive(ND, T).

causes_impairment(nd_poisoned).
