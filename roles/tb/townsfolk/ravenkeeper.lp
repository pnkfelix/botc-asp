% Ravenkeeper

other_night_substep(ravenkeeper, 1, st_asks(choose_player)).
other_night_substep(ravenkeeper, 2, player_chooses(target)).
other_night_substep(ravenkeeper, 3, st_tells(rk_info)).

rk_died_tonight(RK, N) :-
    assigned(0, RK, ravenkeeper),
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    reminder_on(imp_dead, RK, night(N, RoleOrd, 1)).

{ rk_choice(RK, P, night(N, RoleOrd, 2)) : player(P) } = 1 :-
    assigned(0, RK, ravenkeeper),
    other_night_role_order(ravenkeeper, RoleOrd),
    night_number(N), N > 1,
    rk_died_tonight(RK, N).

rk_choice(RK, P, night(N, RoleOrd, 3)) :-
    rk_choice(RK, P, night(N, RoleOrd, 2)).

:- player_chooses(ravenkeeper, RK, point(P), T),
   not rk_choice(RK, P, T).

player_chooses(ravenkeeper, RK, point(P), T) :-
    rk_choice(RK, P, T).

st_tells_core(ravenkeeper, RK, point(R), night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    assigned(0, RK, ravenkeeper),
    rk_died_tonight(RK, N),
    functioning(RK, night(N, RoleOrd, 3)),
    rk_choice(RK, P, night(N, RoleOrd, 3)),
    may_register_as_role(P, R, night(N, RoleOrd, 3)).

{ st_tells_core(ravenkeeper, RK, point(R), night(N, RoleOrd, 3)) : role(R) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    assigned(0, RK, ravenkeeper),
    rk_died_tonight(RK, N),
    not functioning(RK, night(N, RoleOrd, 3)).
