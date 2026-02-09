% Minstrel
% When a Minion dies by execution, all other players (except Travellers) are drunk until dusk tomorrow.

other_night_substep(minstrel, 1, st_places(minstrel_drunk)).

% Helper: true if any minion was executed on day N (trigger condition for Minstrel)
minion_executed(N) :-
    day_number(N),
    executed(P, N),
    assigned(0, P, R),
    minion(R).

% Helper: Minstrel ability fires if alive and functioning when a minion is executed
minstrel_triggers(N) :-
    minion_executed(N),
    assigned(0, Minstrel, minstrel),
    alive(Minstrel, day(N, exec)),
    functioning(Minstrel, day(N, exec)).

% Place drunk token on all other players for the duration of the effect
reminder_on(minstrel_drunk, P, T) :-
    minstrel_triggers(N),
    player(P),
    assigned(0, Minstrel, minstrel),
    P != Minstrel,
    minstrel_drunk_active(N, T).

% Active at execution time on the triggering day
minstrel_drunk_active(N, day(N, exec)) :- day_number(N).
% Active during the following night (can't merge with day rule: different time structure)
minstrel_drunk_active(N, T) :- day_number(N), time(T), T = night(M, _, _), M = N+1.
% Active at dawn of next day (separate from night: dawn is a distinct time type)
minstrel_drunk_active(N, dawn(N+1)) :- day_number(N), day_number(N+1).

% Remove drunk token at dusk of next day (effect expires; day(N+1, 0) is the start of that day)
token_removed(minstrel_drunk, P, day(N+1, 0)) :-
    minstrel_triggers(N),
    player(P),
    assigned(0, Minstrel, minstrel),
    P != Minstrel,
    day_number(N+1).

causes_impairment(minstrel_drunk).
