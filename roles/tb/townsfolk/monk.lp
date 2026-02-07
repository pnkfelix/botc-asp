% Monk

other_night_substep(monk, 1, st_asks(choose_target)).
other_night_substep(monk, 2, player_chooses(target)).
other_night_substep(monk, 3, st_places(monk_protected)).

% Functioning: Monk picks someone to protect (can't merge with not-functioning: token placement depends on this split)
{ player_chooses(monk, Monk, point(P), T) : player(P), P != Monk } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    character_assignment_state_at_time(T, Monk, monk),
    alive(Monk, night(N, 0, 0)),
    functioning(Monk, T).

% Not functioning: Monk still makes a choice (woken up as normal) but it won't produce a token
{ player_chooses(monk, Monk, point(P), T) : player(P), P != Monk } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    character_assignment_state_at_time(T, Monk, monk),
    alive(Monk, night(N, 0, 0)),
    not functioning(Monk, T).

% Only a functioning Monk's choice produces the protection token (no token if poisoned/drunk)
reminder_on(monk_protected, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(monk, RoleOrd),
    T = night(N, RoleOrd, 2),
    player_chooses(monk, Monk, point(P), T),
    character_assignment_state_at_time(T, Monk, monk),
    functioning(Monk, T).

% Protection expires at the start of the next night (it only lasts one night-day cycle)
token_removed(monk_protected, P, night(N, 0, 0)) :-
    night_number(N), N > 1,
    reminder_on(monk_protected, P, day(N-1, exec)).

% Token removal takes effect: a token can't simultaneously exist and be removed at the same time
:- reminder_on(monk_protected, P, T), token_removed(monk_protected, P, T).

% Bridge: the monk_protected token grants the generic protected_from_demon status used by kill logic
protected_from_demon(P, T) :- reminder_on(monk_protected, P, T).
