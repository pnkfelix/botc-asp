% Flowergirl
% Each night*, you learn if a Demon voted today.

other_night_substep(flowergirl, 1, st_tells(flowergirl_info)).

% voted(Player, Day) is an input predicate indicating a player voted on a nomination that day
% demon_voted(Day) is true if the demon voted on day N

demon_voted(N) :-
    day_number(N),
    voted(P, N),
    current_demon(P, N).

% Flowergirl learns yes/no when functioning (other nights only)
st_tells_core(flowergirl, FG, yes, T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(flowergirl, RoleOrd),
    assigned(0, FG, flowergirl),
    alive(FG, night(NightNum, 0, 0)),
    functioning(FG, T),
    demon_voted(NightNum - 1).

st_tells_core(flowergirl, FG, no, T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(flowergirl, RoleOrd),
    assigned(0, FG, flowergirl),
    alive(FG, night(NightNum, 0, 0)),
    functioning(FG, T),
    not demon_voted(NightNum - 1).

% When not functioning, can get either result
{ st_tells_core(flowergirl, FG, yes, T);
  st_tells_core(flowergirl, FG, no, T) } = 1 :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(flowergirl, RoleOrd),
    assigned(0, FG, flowergirl),
    alive(FG, night(NightNum, 0, 0)),
    not functioning(FG, T).
