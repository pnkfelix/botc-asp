% No Dashii
% Each night*, choose a player: they die. Your 2 Townsfolk neighbours are poisoned.

other_night_substep(no_dashii, 1, st_asks(choose_target)).
other_night_substep(no_dashii, 2, player_chooses(kill_target)).
other_night_substep(no_dashii, 3, st_places(no_dashii_dead)).

% No Dashii chooses a target each night (nights after the first)
% Check role at action time T, not night start (handles Snake Charmer swaps etc.)
{ player_chooses(no_dashii, ND, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(no_dashii, RoleOrd),
    character_assignment_state_at_time(T, ND, no_dashii),
    alive(ND, T),
    game_active(night(N, 0, 0)).

% Place the death token on the target
reminder_on(no_dashii_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(no_dashii, RoleOrd),
    player_chooses(no_dashii, ND, point(Target), night(N, RoleOrd, 2)),
    functioning(ND, night(N, RoleOrd, 3)).

% Register as demon kill token (generic handling in botc.lp)
demon_kill_token(no_dashii_dead).

% snv_demon_killed(Target, N) - Target was killed by demon on night N
% Uses night number only (not specific time) so Sage can check without time mismatch
snv_demon_killed(Target, N) :-
    night_number(N), N > 1,
    other_night_role_order(no_dashii, RoleOrd),
    reminder_on(no_dashii_dead, Target, night(N, RoleOrd, 3)).

% Find the No Dashii's townsfolk neighbors
% A "townsfolk neighbor" is the closest living Townsfolk in each direction
%
% OPTIMIZATION: Neighbors only change when someone dies. Compute at state-change
% points (night start, dawn, after execution) instead of every time(T).
% This reduces O(all_substeps) to O(nights + days).

nd_state_time(night(N, 0, 0)) :- night_number(N).
nd_state_time(dawn(N)) :- day_number(N).
nd_state_time(day(N, exec)) :- day_number(N).

% Clockwise townsfolk neighbor (computed at state times only)
nd_townsfolk_neighbor_clockwise(ND, P, T) :-
    assigned(0, ND, no_dashii),
    nd_state_time(T),
    alive(ND, T),
    player(P), P != ND,
    alive(P, T),
    assigned(0, P, R), townsfolk(R),
    clockwise_distance(ND, P, D),
    not nd_closer_townsfolk_clockwise(ND, D, T).

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

% Counter-clockwise townsfolk neighbor
% Counter-clockwise distance from ND to P is the clockwise distance from P to ND
nd_townsfolk_neighbor_counterclockwise(ND, P, T) :-
    assigned(0, ND, no_dashii),
    nd_state_time(T),
    alive(ND, T),
    player(P), P != ND,
    alive(P, T),
    assigned(0, P, R), townsfolk(R),
    clockwise_distance(P, ND, D),  % counter-clockwise distance from ND to P
    not nd_closer_townsfolk_counterclockwise(ND, D, T).

nd_closer_townsfolk_counterclockwise(ND, D, T) :-
    assigned(0, ND, no_dashii),
    nd_state_time(T),
    alive(ND, T),
    player(P), P != ND,
    alive(P, T),
    assigned(0, P, R), townsfolk(R),
    clockwise_distance(P, ND, D),  % counter-clockwise distance from ND to P
    player(P2), P2 != ND, P2 != P,
    alive(P2, T),
    assigned(0, P2, R2), townsfolk(R2),
    clockwise_distance(P2, ND, D2),  % counter-clockwise distance from ND to P2
    D2 < D.

% Place poison token at state times (inertia carries it forward)
reminder_on(nd_poisoned, P, T) :-
    nd_state_time(T),
    nd_townsfolk_neighbor_clockwise(ND, P, T),
    alive(ND, T).

reminder_on(nd_poisoned, P, T) :-
    nd_state_time(T),
    nd_townsfolk_neighbor_counterclockwise(ND, P, T),
    alive(ND, T).

% Remove poison at state times when ND dies or neighbor changes
token_removed(nd_poisoned, P, T) :-
    nd_state_time(T),
    reminder_on(nd_poisoned, P, TPrev),
    next(TPrev, T),
    not nd_townsfolk_neighbor_clockwise(_, P, T),
    not nd_townsfolk_neighbor_counterclockwise(_, P, T).

causes_impairment(nd_poisoned).
