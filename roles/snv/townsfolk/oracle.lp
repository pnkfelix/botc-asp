% Oracle
% Each night*, you learn how many dead players are evil.

other_night_substep(oracle, 1, st_tells(oracle_info)).

% Count dead evil players
dead_evil_count(N, T) :-
    time(T),
    N = #count { P : dead_at_time(P, T), registers_as(P, evil, T) }.

% Oracle learns the count when functioning (other nights only)
st_tells_core(oracle, Oracle, count(N), T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(oracle, RoleOrd),
    assigned(0, Oracle, oracle),
    alive(Oracle, night(NightNum, 0, 0)),
    functioning(Oracle, T),
    dead_evil_count(N, T).

% When not functioning, can get any count from 0 to total dead
{ st_tells_core(oracle, Oracle, count(N), T) : N = 0..player_count } = 1 :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(oracle, RoleOrd),
    assigned(0, Oracle, oracle),
    alive(Oracle, night(NightNum, 0, 0)),
    not functioning(Oracle, T).
