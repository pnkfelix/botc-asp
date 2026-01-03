% Clockmaker
% You start knowing how many steps from the Demon to its nearest Minion.

first_night_substep(clockmaker, 1, st_tells(clockmaker_info)).

% Calculate clockwise distance from demon to each minion
demon_to_minion_distance(D, M, Dist) :-
    assigned(0, D, DR), demon(DR),
    assigned(0, M, MR), minion(MR),
    clockwise_distance(D, M, Dist).

% The minimum distance to any minion (in either direction)
% Clockwise distance D is equivalent to counter-clockwise distance (player_count - D)
demon_to_minion_min_dist(D, M, MinDist) :-
    demon_to_minion_distance(D, M, Dist),
    MinDist = #min { Dist; player_count - Dist }.

% The nearest minion distance from the demon
nearest_minion_distance(MinDist) :-
    MinDist = #min { D : demon_to_minion_min_dist(_, _, D) }.

% Clockmaker learns the distance when functioning
st_tells_core(clockmaker, CM, count(N), T) :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(clockmaker, RoleOrd),
    assigned(0, CM, clockmaker),
    functioning(CM, T),
    nearest_minion_distance(N).

% When poisoned/drunk, can get any count
{ st_tells_core(clockmaker, CM, count(N), T) : N = 0..player_count/2 } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(clockmaker, RoleOrd),
    assigned(0, CM, clockmaker),
    not functioning(CM, T).
