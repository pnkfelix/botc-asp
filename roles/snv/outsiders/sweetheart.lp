% Sweetheart
% When you die, 1 player is drunk from now on.

other_night_substep(sweetheart, 1, st_places(sweetheart_drunk)).

% When the Sweetheart dies, the ST chooses a player to become drunk permanently
sweetheart_died(Sweetheart, N) :-
    assigned(0, Sweetheart, sweetheart),
    died(Sweetheart, T),
    time(T),
    T = night(N, RO, S).

sweetheart_died(Sweetheart, N) :-
    assigned(0, Sweetheart, sweetheart),
    died(Sweetheart, T),
    time(T),
    T = day(N, Phase).

sweetheart_died(Sweetheart, N) :-
    assigned(0, Sweetheart, sweetheart),
    died(Sweetheart, T),
    time(T),
    T = dawn(N).

% ST chooses one player to be drunk from now on
{ sweetheart_drunk_target(P, N) : player(P) } = 1 :-
    sweetheart_died(_, N).

% The chosen player is drunk from now on (permanent)
reminder_on(sweetheart_drunk, P, T) :-
    sweetheart_drunk_target(P, N),
    time(T), T = night(N2, RO, S), N2 >= N.

reminder_on(sweetheart_drunk, P, T) :-
    sweetheart_drunk_target(P, N),
    time(T), T = day(N2, Phase), N2 >= N.

reminder_on(sweetheart_drunk, P, T) :-
    sweetheart_drunk_target(P, N),
    time(T), T = dawn(N2), N2 >= N.

causes_impairment(sweetheart_drunk).
