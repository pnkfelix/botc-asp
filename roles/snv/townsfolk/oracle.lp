% Oracle
% Each night*, you learn how many dead players are evil.

other_night_substep(oracle, 1, st_tells(oracle_info)).

% Helper: aggregate count of dead players who register as evil at time T.
% Factored out because #count cannot appear directly in a rule head argument.
dead_evil_count(N, T) :-
    time(T),
    N = #count { P : dead_at_time(P, T), registers_as(P, evil, T) }.

% Functioning tell: deterministic count of dead evil players.
% Can't merge with not-functioning rule: this is a normal rule, not a choice rule.
st_tells_core(oracle, Oracle, count(N), T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(oracle, RoleOrd),
    assigned(0, Oracle, oracle),
    alive(Oracle, night(NightNum, 0, 0)),
    functioning(Oracle, T),
    dead_evil_count(N, T).

% Not-functioning tell: ST freely chooses any count from 0..player_count (choice rule).
% Can't merge with functioning rule: different semantics (choice vs deterministic).
{ st_tells_core(oracle, Oracle, count(N), T) : N = 0..player_count } = 1 :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(oracle, RoleOrd),
    assigned(0, Oracle, oracle),
    alive(Oracle, night(NightNum, 0, 0)),
    not functioning(Oracle, T).
