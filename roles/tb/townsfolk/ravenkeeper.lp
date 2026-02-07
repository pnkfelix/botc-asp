% Ravenkeeper

other_night_substep(ravenkeeper, 1, st_asks(choose_player)).
other_night_substep(ravenkeeper, 2, player_chooses(target)).
other_night_substep(ravenkeeper, 3, st_tells(rk_info)).

% Helper: RK's ability triggers only if they have the imp_dead token (they're dying tonight)
rk_died_tonight(RK, N) :-
    T = night(N, RoleOrd, 1),
    character_assignment_state_at_time(T, RK, ravenkeeper),
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    reminder_on(imp_dead, RK, T).

% RK picks one player to learn about; gated on rk_died_tonight (only fires if RK is dying)
{ rk_choice(RK, P, T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    character_assignment_state_at_time(T, RK, ravenkeeper),
    other_night_role_order(ravenkeeper, RoleOrd),
    night_number(N), N > 1,
    rk_died_tonight(RK, N).

% Carry the RK's choice forward from substep 2 (choose) to substep 3 (tell)
rk_choice(RK, P, night(N, RoleOrd, 3)) :-
    rk_choice(RK, P, night(N, RoleOrd, 2)).

% Constraint: player_chooses for RK must correspond to an rk_choice (no spurious choices)
:- player_chooses(ravenkeeper, RK, point(P), T),
   not rk_choice(RK, P, T).

% Bridge: rk_choice generates the public player_chooses atom for grimoire display
player_chooses(ravenkeeper, RK, point(P), T) :-
    rk_choice(RK, P, T).

% Functioning: ST tells a role the chosen player may register as (deterministic from may_register_as_role)
st_tells_core(ravenkeeper, RK, point(R), T) :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    character_assignment_state_at_time(T, RK, ravenkeeper),
    rk_died_tonight(RK, N),
    functioning(RK, T),
    rk_choice(RK, P, T),
    may_register_as_role(P, R, T).

% Not functioning: ST may name any role (choice rule, no truthfulness constraint)
{ st_tells_core(ravenkeeper, RK, point(R), T) : role(R) } = 1 :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(ravenkeeper, RoleOrd),
    character_assignment_state_at_time(T, RK, ravenkeeper),
    rk_died_tonight(RK, N),
    not functioning(RK, T).
