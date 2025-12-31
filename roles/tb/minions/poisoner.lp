% Poisoner

first_night_substep(poisoner, 1, st_asks(choose_target)).
first_night_substep(poisoner, 2, player_chooses(target)).
first_night_substep(poisoner, 3, st_places(poisoned)).

other_night_substep(poisoner, 1, st_asks(choose_target)).
other_night_substep(poisoner, 2, player_chooses(target)).
other_night_substep(poisoner, 3, st_places(poisoned)).

{ player_chooses(poisoner, Poisoner, point(P), T) : player(P), P != Poisoner } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(poisoner, RoleOrd),
    assigned(0, Poisoner, poisoner).

{ player_chooses(poisoner, Poisoner, point(P), T) : player(P), P != Poisoner } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(poisoner, RoleOrd),
    assigned(0, Poisoner, poisoner),
    alive(Poisoner, night(N, 0, 0)).

reminder_on(poi_poisoned, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(poisoner, RoleOrd),
    player_chooses(poisoner, _, point(P), night(1, RoleOrd, 2)).

reminder_on(poi_poisoned, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(poisoner, RoleOrd),
    player_chooses(poisoner, _, point(P), night(N, RoleOrd, 2)).

token_removed(poi_poisoned, P, night(N, 0, 0)) :-
    night_number(N), N > 1,
    reminder_on(poi_poisoned, P, day(N-1, exec)),
    assigned(0, _, poisoner).

:- reminder_on(poi_poisoned, P, T), token_removed(poi_poisoned, P, T).

causes_impairment(poi_poisoned).
