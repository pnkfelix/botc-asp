% Undertaker

other_night_substep(undertaker, 1, st_tells(undertaker_info)).

% Helper: links night N to the player executed on day N-1; separate to keep main rules readable
executed_previous_day(P, N) :-
    night_number(N), N > 1,
    executed(P, N-1).

% Functioning: candidate messages are roles the executed player may register as (truthful options)
st_may_tell(undertaker, UT, point(R), T) :-
    T = night(N, RoleOrd, 1),
    night_number(N), N > 1,
    other_night_role_order(undertaker, RoleOrd),
    character_assignment_state_at_time(T, UT, undertaker),
    alive(UT, night(N, 0, 0)),
    functioning(UT, T),
    executed_previous_day(P, N),
    may_register_as_role(P, R, T).

% Not functioning: candidate messages include every role (no truthfulness constraint)
st_may_tell(undertaker, UT, point(R), T) :-
    T = night(N, RoleOrd, 1),
    night_number(N), N > 1,
    other_night_role_order(undertaker, RoleOrd),
    character_assignment_state_at_time(T, UT, undertaker),
    alive(UT, night(N, 0, 0)),
    not functioning(UT, T),
    executed_previous_day(_, N),
    role(R).

% ST picks exactly one from the candidate pool; separate from st_may_tell to decouple pool from selection
{ st_tells_core(undertaker, UT, Msg, T) : st_may_tell(undertaker, UT, Msg, T) } = 1 :-
    T = night(N, RoleOrd, 1),
    night_number(N), N > 1,
    other_night_role_order(undertaker, RoleOrd),
    character_assignment_state_at_time(T, UT, undertaker),
    alive(UT, night(N, 0, 0)),
    executed_previous_day(_, N).
