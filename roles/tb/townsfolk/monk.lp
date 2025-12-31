% Monk

other_night_substep(monk, 1, st_asks(choose_target)).
other_night_substep(monk, 2, player_chooses(target)).
other_night_substep(monk, 3, st_places(monk_protected)).

{ player_chooses(monk, Monk, point(P), night(N, RoleOrd, 2)) : player(P), P != Monk } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    assigned(0, Monk, monk),
    alive(Monk, night(N, 0, 0)),
    functioning(Monk, night(N, RoleOrd, 2)).

{ player_chooses(monk, Monk, point(P), night(N, RoleOrd, 2)) : player(P), P != Monk } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    assigned(0, Monk, monk),
    alive(Monk, night(N, 0, 0)),
    not functioning(Monk, night(N, RoleOrd, 2)).

reminder_on(monk_protected, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    player_chooses(monk, Monk, point(P), night(N, RoleOrd, 2)),
    assigned(0, Monk, monk),
    functioning(Monk, night(N, RoleOrd, 2)).

token_removed(monk_protected, P, night(N, 0, 0)) :-
    night_number(N), N > 1,
    reminder_on(monk_protected, P, day(N-1, exec)).

:- reminder_on(monk_protected, P, T), token_removed(monk_protected, P, T).

protected_from_demon(P, T) :- reminder_on(monk_protected, P, T).
