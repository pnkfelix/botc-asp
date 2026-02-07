% Ravenkeeper

other_night_substep(ravenkeeper, 1, st_asks(choose_player)).
other_night_substep(ravenkeeper, 2, player_chooses(target)).
other_night_substep(ravenkeeper, 3, st_tells(rk_info)).

% Ravenkeeper dies tonight (has imp_dead token at their substep)
rk_died_tonight(RK, N) :-
    T = night(N, RoleOrd, 1),
    character_assignment_state_at_time(T, RK, ravenkeeper),
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    reminder_on(imp_dead, RK, T).

% Ravenkeeper chooses a player to learn about (dying ability)
{ rk_choice(RK, P, T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    character_assignment_state_at_time(T, RK, ravenkeeper),
    other_night_role_order(ravenkeeper, RoleOrd),
    night_number(N), N > 1,
    rk_died_tonight(RK, N).

rk_choice(RK, P, night(N, RoleOrd, 3)) :-
    rk_choice(RK, P, night(N, RoleOrd, 2)).

:- player_chooses(ravenkeeper, RK, point(P), T),
   not rk_choice(RK, P, T).

player_chooses(ravenkeeper, RK, point(P), T) :-
    rk_choice(RK, P, T).

% Ravenkeeper learns role of chosen player (functioning)
st_tells_core(ravenkeeper, RK, point(R), T) :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    character_assignment_state_at_time(T, RK, ravenkeeper),
    rk_died_tonight(RK, N),
    functioning(RK, T),
    rk_choice(RK, P, T),
    may_register_as_role(P, R, T).

% Ravenkeeper learns arbitrary role (not functioning)
{ st_tells_core(ravenkeeper, RK, point(R), T) : role(R) } = 1 :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    character_assignment_state_at_time(T, RK, ravenkeeper),
    rk_died_tonight(RK, N),
    not functioning(RK, T).
