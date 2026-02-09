% Investigator

first_night_substep(investigator, 1, st_places(inv_minion)).
first_night_substep(investigator, 2, st_places(inv_wrong)).
first_night_substep(investigator, 3, st_tells(inv_info)).

% ST secretly picks one player to be the minion-reference; separate from inv_wrong placement below
{ reminder_on(inv_minion, P, T) : player(P) } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(investigator, RoleOrd),
    character_assignment_state_at_time(T, _, investigator).

% ST picks a second player as the decoy; must be a distinct step so chair_ordered can pair them
{ reminder_on(inv_wrong, P, T) : player(P) } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(investigator, RoleOrd),
    character_assignment_state_at_time(T, _, investigator).

% The two tokens must be on different players (otherwise the "two players, one is X" info is degenerate)
:- reminder_on(inv_minion, P, _),
   reminder_on(inv_wrong, P, _).

% When functioning, the inv_minion token must actually be on someone who registers as minion
:- T = night(1, RoleOrd, 3),
   first_night_role_order(investigator, RoleOrd),
   reminder_on(inv_minion, P, T),
   character_assignment_state_at_time(T, InvestigatorPlayer, investigator),
   functioning(InvestigatorPlayer, T),
   not registers_as(P, minion, T).

% Functioning: ST tells a role the minion-reference may register as (constrained to truthful options)
{ st_tells_core(investigator, INV, info(First, Second, R), T) : may_register_as_role(PMinion, R, T) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(investigator, RoleOrd),
    character_assignment_state_at_time(T, INV, investigator),
    reminder_on(inv_minion, PMinion, T),
    reminder_on(inv_wrong, PWrong, T),
    chair_ordered(PMinion, PWrong, First, Second),
    functioning(INV, T).

% Not functioning (poisoned/drunk): ST may name any minion role (no truthfulness constraint)
{ st_tells_core(investigator, INV, info(First, Second, R), T) : minion(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(investigator, RoleOrd),
    character_assignment_state_at_time(T, INV, investigator),
    reminder_on(inv_minion, PMinion, T),
    reminder_on(inv_wrong, PWrong, T),
    chair_ordered(PMinion, PWrong, First, Second),
    not functioning(INV, T).
