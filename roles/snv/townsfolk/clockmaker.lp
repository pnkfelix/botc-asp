% Clockmaker
% You start knowing how many steps from the Demon to its nearest Minion.

first_night_substep(clockmaker, 1, st_tells(clockmaker_info)).

% Helper: clockwise distance from demon to each minion (can't inline: feeds into min-distance calculation below)
demon_to_minion_distance(D, M, Dist) :-
    assigned(0, D, DR), demon(DR),
    assigned(0, M, MR), minion(MR),
    clockwise_distance(D, M, Dist).

% Helper: shorter of clockwise/counter-clockwise distance per demon-minion pair (can't fold into aggregate: #min over both directions must happen per pair first)
demon_to_minion_min_dist(D, M, MinDist) :-
    demon_to_minion_distance(D, M, Dist),
    MinDist = #min { Dist; player_count - Dist }.

% Aggregate helper: global minimum across all demon-minion pairs (can't fold into tell: aggregate must be computed before use in st_tells_core)
nearest_minion_distance(MinDist) :-
    MinDist = #min { D : demon_to_minion_min_dist(_, _, D) }.

% Functioning tell: deterministic count from nearest_minion_distance (can't merge with not-functioning: this is a derived fact, not a choice)
st_tells_core(clockmaker, CM, count(N), T) :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(clockmaker, RoleOrd),
    assigned(0, CM, clockmaker),
    functioning(CM, T),
    nearest_minion_distance(N).

% Not-functioning tell: ST freely chooses any count 0..player_count/2 (can't merge with functioning: choice rule vs derived fact)
{ st_tells_core(clockmaker, CM, count(N), T) : N = 0..player_count/2 } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(clockmaker, RoleOrd),
    assigned(0, CM, clockmaker),
    not functioning(CM, T).
