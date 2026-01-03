% Minstrel
% When a Minion dies by execution, all other players (except Travellers) are drunk until dusk tomorrow.

other_night_substep(minstrel, 1, st_places(minstrel_drunk)).

% Check if a minion was executed today
minion_executed(N) :-
    day_number(N),
    executed(P, N),
    assigned(0, P, R),
    minion(R).

% When minion executed, Minstrel triggers if functioning
minstrel_triggers(N) :-
    minion_executed(N),
    assigned(0, Minstrel, minstrel),
    alive(Minstrel, day(N, exec)),
    functioning(Minstrel, day(N, exec)).

% All other players become drunk until dusk tomorrow
% Drunk starts at execution and lasts until dusk of next day
reminder_on(minstrel_drunk, P, T) :-
    minstrel_triggers(N),
    player(P),
    assigned(0, Minstrel, minstrel),
    P != Minstrel,
    minstrel_drunk_active(N, T).

% Drunk is active from execution until dusk next day
minstrel_drunk_active(N, day(N, exec)) :- day_number(N).
minstrel_drunk_active(N, T) :- day_number(N), time(T), T = night(M, _, _), M = N+1.
minstrel_drunk_active(N, dawn(N+1)) :- day_number(N), day_number(N+1).

% Token removed at dusk (start of day N+1, after dawn)
token_removed(minstrel_drunk, P, day(N+1, 0)) :-
    minstrel_triggers(N),
    player(P),
    assigned(0, Minstrel, minstrel),
    P != Minstrel,
    day_number(N+1).

causes_impairment(minstrel_drunk).
