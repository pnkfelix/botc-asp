% Tea Lady
% If both your alive neighbours are good, they can't die.

other_night_substep(tea_lady, 1, st_checks(tl_neighbors)).

% Helper: identifies the Tea Lady's alive neighbors via the generic living_neighbor predicate
tl_alive_neighbor(TL, P, T) :-
    assigned(0, TL, tea_lady),
    living_neighbor(TL, P, T).

% Helper: true when both alive neighbors are actually good and Tea Lady is alive + functioning
tl_both_neighbors_good(TL, T) :-
    assigned(0, TL, tea_lady),
    alive(TL, T),
    functioning(TL, T),
    time(T),
    tl_alive_neighbor(TL, P1, T),
    tl_alive_neighbor(TL, P2, T),
    P1 != P2,
    actual_category(P1, good),
    actual_category(P2, good).

% Both good neighbors receive protection (applies to all death types)
tea_lady_protects(P, T) :-
    tl_both_neighbors_good(TL, T),
    tl_alive_neighbor(TL, P, T).

% Bridge: Tea Lady protection grants demon kill immunity
protected_from_demon(P, T) :- tea_lady_protects(P, T).

% Bridge: Tea Lady protection grants execution immunity (separate from demon: different time structure)
protected_from_execution(P, N) :-
    tea_lady_protects(P, day(N, exec)).

% Constraint: execution of a protected player must not produce a death (execution happens but they survive)
:- executed(P, N), protected_from_execution(P, N), not died(P, day(N, exec)).
