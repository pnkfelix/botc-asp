% Undertaker

other_night_substep(undertaker, 1, st_tells(undertaker_info)).

executed_previous_day(P, N) :-
    night_number(N), N > 1,
    executed(P, N-1).

st_may_tell(undertaker, UT, point(R), night(N, RoleOrd, 1)) :-
    night_number(N), N > 1,
    other_night_role_order(undertaker, RoleOrd),
    assigned(0, UT, undertaker),
    alive(UT, night(N, 0, 0)),
    functioning(UT, night(N, RoleOrd, 1)),
    executed_previous_day(P, N),
    may_register_as_role(P, R, night(N, RoleOrd, 1)).

st_may_tell(undertaker, UT, point(R), night(N, RoleOrd, 1)) :-
    night_number(N), N > 1,
    other_night_role_order(undertaker, RoleOrd),
    assigned(0, UT, undertaker),
    alive(UT, night(N, 0, 0)),
    not functioning(UT, night(N, RoleOrd, 1)),
    executed_previous_day(_, N),
    role(R).

% Drunk/Marionette who think they're Undertaker - arbitrary role info
st_may_tell(undertaker, UT, point(R), night(N, RoleOrd, 1)) :-
    night_number(N), N > 1,
    other_night_role_order(undertaker, RoleOrd),
    received(UT, undertaker),
    not assigned(0, UT, undertaker),
    alive(UT, night(N, 0, 0)),
    executed_previous_day(_, N),
    role(R).

{ st_tells_core(undertaker, UT, Msg, T) : st_may_tell(undertaker, UT, Msg, T) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(undertaker, RoleOrd),
    T = night(N, RoleOrd, 1),
    assigned(0, UT, undertaker),
    alive(UT, night(N, 0, 0)),
    executed_previous_day(_, N).

% Also fire st_tells_core for Drunk/Marionette who think they're Undertaker
{ st_tells_core(undertaker, UT, Msg, T) : st_may_tell(undertaker, UT, Msg, T) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(undertaker, RoleOrd),
    T = night(N, RoleOrd, 1),
    received(UT, undertaker),
    not assigned(0, UT, undertaker),
    alive(UT, night(N, 0, 0)),
    executed_previous_day(_, N).
