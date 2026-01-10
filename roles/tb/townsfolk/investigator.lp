% Investigator

first_night_substep(investigator, 1, st_places(inv_minion)).
first_night_substep(investigator, 2, st_places(inv_wrong)).
first_night_substep(investigator, 3, st_tells(inv_info)).

% Place reminder tokens for anyone who received the investigator token
% (includes both actual investigators and drunks/marionettes who think they are)
{ reminder_on(inv_minion, P, night(1, RoleOrd, 1)) : player(P) } = 1 :-
    first_night_role_order(investigator, RoleOrd),
    received(_, investigator).

{ reminder_on(inv_wrong, P, night(1, RoleOrd, 2)) : player(P) } = 1 :-
    first_night_role_order(investigator, RoleOrd),
    received(_, investigator).

:- reminder_on(inv_minion, P, _),
   reminder_on(inv_wrong, P, _).

:- T = night(1, RoleOrd, 3),
   first_night_role_order(investigator, RoleOrd),
   reminder_on(inv_minion, P, T),
   assigned(0, InvestigatorPlayer, investigator),
   functioning(InvestigatorPlayer, T),
   not registers_as(P, minion, T).

{ st_tells_core(investigator, INV, info(First, Second, R), T) : may_register_as_role(PMinion, R, T) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(investigator, RoleOrd),
    assigned(0, INV, investigator),
    reminder_on(inv_minion, PMinion, T),
    reminder_on(inv_wrong, PWrong, T),
    chair_ordered(PMinion, PWrong, First, Second),
    functioning(INV, T).

{ st_tells_core(investigator, INV, info(First, Second, R), T) : minion(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(investigator, RoleOrd),
    assigned(0, INV, investigator),
    reminder_on(inv_minion, PMinion, T),
    reminder_on(inv_wrong, PWrong, T),
    chair_ordered(PMinion, PWrong, First, Second),
    not functioning(INV, T).
