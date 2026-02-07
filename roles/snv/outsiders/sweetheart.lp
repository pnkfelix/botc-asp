% Sweetheart
% When you die, 1 player is drunk from now on.

other_night_substep(sweetheart, 1, st_places(sweetheart_drunk)).

% Helper: Sweetheart died during the night (separate rules per time type: different unification)
sweetheart_died(Sweetheart, N) :-
    assigned(0, Sweetheart, sweetheart),
    died(Sweetheart, T),
    time(T),
    T = night(N, _, _).

% Helper: Sweetheart died during the day
sweetheart_died(Sweetheart, N) :-
    assigned(0, Sweetheart, sweetheart),
    died(Sweetheart, T),
    time(T),
    T = day(N, _).

% Helper: Sweetheart died at dawn
sweetheart_died(Sweetheart, N) :-
    assigned(0, Sweetheart, sweetheart),
    died(Sweetheart, T),
    time(T),
    T = dawn(N).

% ST chooses exactly one player to become permanently drunk upon Sweetheart's death
{ sweetheart_drunk_target(P, N) : player(P) } = 1 :-
    sweetheart_died(_, N).

% Permanent drunk token for all subsequent nights (can't merge with day: different time structure)
reminder_on(sweetheart_drunk, P, T) :-
    sweetheart_drunk_target(P, N),
    time(T), T = night(N2, _, _), N2 >= N.

% Permanent drunk token for all subsequent days (can't merge with night: different unification)
reminder_on(sweetheart_drunk, P, T) :-
    sweetheart_drunk_target(P, N),
    time(T), T = day(N2, _), N2 >= N.

% Permanent drunk token for all subsequent dawns (separate from night/day: distinct time type)
reminder_on(sweetheart_drunk, P, T) :-
    sweetheart_drunk_target(P, N),
    time(T), T = dawn(N2), N2 >= N.

causes_impairment(sweetheart_drunk).
