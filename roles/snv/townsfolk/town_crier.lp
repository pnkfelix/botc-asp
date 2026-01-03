% Town Crier
% Each night*, you learn if a Minion nominated today.

other_night_substep(town_crier, 1, st_tells(town_crier_info)).

% nominated(Player, Day) is an input predicate indicating a player made a nomination that day
% minion_nominated(Day) is true if a minion nominated on day N

minion_nominated(N) :-
    day_number(N),
    nominated(P, N),
    assigned(0, P, R),
    minion(R).

% Town Crier learns yes/no when functioning (other nights only)
st_tells_core(town_crier, TC, yes, T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(town_crier, RoleOrd),
    assigned(0, TC, town_crier),
    alive(TC, night(NightNum, 0, 0)),
    functioning(TC, T),
    minion_nominated(NightNum - 1).

st_tells_core(town_crier, TC, no, T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(town_crier, RoleOrd),
    assigned(0, TC, town_crier),
    alive(TC, night(NightNum, 0, 0)),
    functioning(TC, T),
    not minion_nominated(NightNum - 1).

% When not functioning, can get either result
{ st_tells_core(town_crier, TC, yes, T);
  st_tells_core(town_crier, TC, no, T) } = 1 :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(town_crier, RoleOrd),
    assigned(0, TC, town_crier),
    alive(TC, night(NightNum, 0, 0)),
    not functioning(TC, T).
