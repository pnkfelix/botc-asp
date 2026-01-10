% Ravenkeeper

other_night_substep(ravenkeeper, 1, st_asks(choose_player)).
other_night_substep(ravenkeeper, 2, player_chooses(target)).
other_night_substep(ravenkeeper, 3, st_tells(rk_info)).

% A player who received the ravenkeeper token and died tonight triggers their ability
rk_died_tonight(RK, N) :-
    received(RK, ravenkeeper),
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    reminder_on(imp_dead, RK, night(N, RoleOrd, 1)).

% Anyone who received ravenkeeper token and died makes a choice
{ rk_choice(RK, P, night(N, RoleOrd, 2)) : player(P) } = 1 :-
    received(RK, ravenkeeper),
    other_night_role_order(ravenkeeper, RoleOrd),
    night_number(N), N > 1,
    rk_died_tonight(RK, N).

rk_choice(RK, P, night(N, RoleOrd, 3)) :-
    rk_choice(RK, P, night(N, RoleOrd, 2)).

:- player_chooses(ravenkeeper, RK, point(P), T),
   not rk_choice(RK, P, T).

player_chooses(ravenkeeper, RK, point(P), T) :-
    rk_choice(RK, P, T).

% Actual functioning ravenkeeper gets true info
st_tells_core(ravenkeeper, RK, point(R), night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    assigned(0, RK, ravenkeeper),
    rk_died_tonight(RK, N),
    functioning(RK, night(N, RoleOrd, 3)),
    rk_choice(RK, P, night(N, RoleOrd, 3)),
    may_register_as_role(P, R, night(N, RoleOrd, 3)).

% Non-functioning ravenkeeper gets arbitrary info
{ st_tells_core(ravenkeeper, RK, point(R), night(N, RoleOrd, 3)) : role(R) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    assigned(0, RK, ravenkeeper),
    rk_died_tonight(RK, N),
    not functioning(RK, night(N, RoleOrd, 3)).

% Drunk/Marionette who think they're Ravenkeeper get arbitrary info
{ st_tells_core(ravenkeeper, RK, point(R), night(N, RoleOrd, 3)) : role(R) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    received(RK, ravenkeeper),
    not assigned(0, RK, ravenkeeper),
    rk_died_tonight(RK, N).
