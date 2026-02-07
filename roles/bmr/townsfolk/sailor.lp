% Sailor
% Each night, choose an alive player: either you or they are drunk until dusk. You can't die.

first_night_substep(sailor, 1, st_asks(choose_target)).
first_night_substep(sailor, 2, player_chooses(target)).
first_night_substep(sailor, 3, st_places(sailor_drunk)).

other_night_substep(sailor, 1, st_asks(choose_target)).
other_night_substep(sailor, 2, player_chooses(target)).
other_night_substep(sailor, 3, st_places(sailor_drunk)).

% Night 1 choice: Sailor picks a player (can't merge with other nights: different role order, no alive check)
{ player_chooses(sailor, Sailor, point(P), night(1, RoleOrd, 2)) : player(P) } = 1 :-
    first_night_role_order(sailor, RoleOrd),
    assigned(0, Sailor, sailor).

% Other nights choice: adds alive filter (separate from night 1: uses other_night_role_order + alive check)
{ player_chooses(sailor, Sailor, point(P), night(N, RoleOrd, 2)) : player(P), alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(sailor, RoleOrd),
    assigned(0, Sailor, sailor),
    alive(Sailor, night(N, 0, 0)).

% Night 1: ST chooses either Sailor or target to be drunk (can't merge with other nights: different role order)
{ sailor_drunk_target(Sailor, night(1, RoleOrd, 3));
  sailor_drunk_target(Target, night(1, RoleOrd, 3)) } = 1 :-
    first_night_role_order(sailor, RoleOrd),
    assigned(0, Sailor, sailor),
    player_chooses(sailor, Sailor, point(Target), night(1, RoleOrd, 2)).

% Other nights: ST chooses drunk target (separate from night 1: uses other_night_role_order + alive check)
{ sailor_drunk_target(Sailor, night(N, RoleOrd, 3));
  sailor_drunk_target(Target, night(N, RoleOrd, 3)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(sailor, RoleOrd),
    assigned(0, Sailor, sailor),
    alive(Sailor, night(N, 0, 0)),
    player_chooses(sailor, Sailor, point(Target), night(N, RoleOrd, 2)).

% Place drunk token on whichever player ST chose
reminder_on(sailor_drunk, P, T) :- sailor_drunk_target(P, T).

% Remove drunk token at start of next night (drunk lasts "until dusk")
token_removed(sailor_drunk, P, night(N+1, 0, 0)) :-
    reminder_on(sailor_drunk, P, day(N, exec)),
    night_number(N+1).

% Sailor drunk token causes impairment
causes_impairment(sailor_drunk).

% Sailor can't die while functioning: grants blanket death protection at every time point
protected_from_death(Sailor, T) :-
    assigned(0, Sailor, sailor),
    functioning(Sailor, T),
    time(T).

% Bridge: death protection implies demon kill protection (subsumes protected_from_demon)
protected_from_demon(P, T) :- protected_from_death(P, T).

% Constraint: Sailor cannot die from non-execution causes while protected (execution is the exception)
:- died(P, T), protected_from_death(P, T), not died_from_execution(P, T).

% Helper: identifies execution deaths (allows the constraint above to exempt them)
died_from_execution(P, T) :- executed(P, N), T = day(N, exec).
