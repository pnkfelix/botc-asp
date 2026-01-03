% Sailor
% Each night, choose an alive player: either you or they are drunk until dusk. You can't die.

first_night_substep(sailor, 1, st_asks(choose_target)).
first_night_substep(sailor, 2, player_chooses(target)).
first_night_substep(sailor, 3, st_places(sailor_drunk)).

other_night_substep(sailor, 1, st_asks(choose_target)).
other_night_substep(sailor, 2, player_chooses(target)).
other_night_substep(sailor, 3, st_places(sailor_drunk)).

% First night: Sailor chooses an alive player
{ player_chooses(sailor, Sailor, point(P), night(1, RoleOrd, 2)) : player(P) } = 1 :-
    first_night_role_order(sailor, RoleOrd),
    assigned(0, Sailor, sailor).

% Other nights: Sailor chooses an alive player (if alive)
{ player_chooses(sailor, Sailor, point(P), night(N, RoleOrd, 2)) : player(P), alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(sailor, RoleOrd),
    assigned(0, Sailor, sailor),
    alive(Sailor, night(N, 0, 0)).

% ST chooses which one is drunk: either Sailor or target
% On first night
{ sailor_drunk_target(Sailor, night(1, RoleOrd, 3));
  sailor_drunk_target(Target, night(1, RoleOrd, 3)) } = 1 :-
    first_night_role_order(sailor, RoleOrd),
    assigned(0, Sailor, sailor),
    player_chooses(sailor, Sailor, point(Target), night(1, RoleOrd, 2)).

% On other nights
{ sailor_drunk_target(Sailor, night(N, RoleOrd, 3));
  sailor_drunk_target(Target, night(N, RoleOrd, 3)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(sailor, RoleOrd),
    assigned(0, Sailor, sailor),
    alive(Sailor, night(N, 0, 0)),
    player_chooses(sailor, Sailor, point(Target), night(N, RoleOrd, 2)).

% Place the drunk token
reminder_on(sailor_drunk, P, T) :- sailor_drunk_target(P, T).

% Remove drunk token at dusk (start of next night)
token_removed(sailor_drunk, P, night(N+1, 0, 0)) :-
    reminder_on(sailor_drunk, P, day(N, exec)),
    night_number(N+1).

% Sailor drunk token causes impairment
causes_impairment(sailor_drunk).

% Sailor cannot die (protected from all deaths while functioning)
% This is a strong form of protection - bypasses most death causes
protected_from_death(Sailor, T) :-
    assigned(0, Sailor, sailor),
    functioning(Sailor, T),
    time(T).

% Protected from demon kills
protected_from_demon(P, T) :- protected_from_death(P, T).

% Prevent Sailor from dying to other causes
:- died(P, T), protected_from_death(P, T), not died_from_execution(P, T).

% Execution is the exception - Sailor CAN be executed
died_from_execution(P, T) :- executed(P, N), T = day(N, exec).
