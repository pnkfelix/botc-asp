% Tea Lady
% If both your alive neighbours are good, they can't die.

other_night_substep(tea_lady, 1, st_checks(tl_neighbors)).

% Tea Lady's alive neighbors
tl_alive_neighbor(TL, P, T) :-
    assigned(0, TL, tea_lady),
    living_neighbor(TL, P, T).

% Both neighbors are good
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

% If both neighbors are good, they are protected
% Protection applies to all death types
tea_lady_protects(P, T) :-
    tl_both_neighbors_good(TL, T),
    tl_alive_neighbor(TL, P, T).

% Protected from demon kills
protected_from_demon(P, T) :- tea_lady_protects(P, T).

% Protected from execution
protected_from_execution(P, N) :-
    tea_lady_protects(P, day(N, exec)).

% Override execution death
:- executed(P, N), protected_from_execution(P, N), not died(P, day(N, exec)).
% Actually, we need to prevent the death, not just complain about it
% Let's use a different approach - the execution happens but they don't die

% Tea Lady's neighbors can't die when protected (soft constraint for ST choice)
% In practice, this means demon can't kill them and execution doesn't kill them
