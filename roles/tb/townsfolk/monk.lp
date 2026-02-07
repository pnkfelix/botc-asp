% Monk

other_night_substep(monk, 1, st_asks(choose_target)).
other_night_substep(monk, 2, player_chooses(target)).
other_night_substep(monk, 3, st_places(monk_protected)).

% Monk chooses a player to protect (functioning)
{ player_chooses(monk, Monk, point(P), T) : player(P), P != Monk } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    character_assignment_state_at_time(T, Monk, monk),
    alive(Monk, night(N, 0, 0)),
    functioning(Monk, T).

% Monk chooses a player to protect (not functioning - choice still happens)
{ player_chooses(monk, Monk, point(P), T) : player(P), P != Monk } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    character_assignment_state_at_time(T, Monk, monk),
    alive(Monk, night(N, 0, 0)),
    not functioning(Monk, T).

% Place protection token (only if functioning)
reminder_on(monk_protected, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    T = night(N, RoleOrd, 2),
    player_chooses(monk, Monk, point(P), T),
    character_assignment_state_at_time(T, Monk, monk),
    functioning(Monk, T).

token_removed(monk_protected, P, night(N, 0, 0)) :-
    night_number(N), N > 1,
    reminder_on(monk_protected, P, day(N-1, exec)).

:- reminder_on(monk_protected, P, T), token_removed(monk_protected, P, T).

protected_from_demon(P, T) :- reminder_on(monk_protected, P, T).
