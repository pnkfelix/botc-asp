% Washerwoman

first_night_substep(washerwoman, 1, st_places(ww_townsfolk)).
first_night_substep(washerwoman, 2, st_places(ww_wrong)).
first_night_substep(washerwoman, 3, st_tells(ww_info)).

% ST secretly picks one player to be the townsfolk-reference; separate from ww_wrong placement below
{ reminder_on(ww_townsfolk, P, T) : player(P) } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(washerwoman, RoleOrd),
    character_assignment_state_at_time(T, _, washerwoman).

% ST picks a second player as the decoy; must be a distinct step so chair_ordered can pair them
{ reminder_on(ww_wrong, P, T) : player(P) } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(washerwoman, RoleOrd),
    character_assignment_state_at_time(T, _, washerwoman).

% The two tokens must be on different players (otherwise the "two players, one is X" info is degenerate)
:- reminder_on(ww_townsfolk, P, T1),
   reminder_on(ww_wrong, P, T2).

% When functioning, the ww_townsfolk token must actually be on someone who registers as townsfolk
:- T = night(1, RoleOrd, 3),
   first_night_role_order(washerwoman, RoleOrd),
   reminder_on(ww_townsfolk, P, T),
   character_assignment_state_at_time(T, WasherwomanPlayer, washerwoman),
   functioning(WasherwomanPlayer, T),
   not registers_as(P, townsfolk, T).

% Functioning: ST tells a role the townsfolk-reference may register as (constrained to truthful options)
{ st_tells_core(washerwoman, WW, info(First, Second, R), T) : may_register_as_role(PTownsfolk, R, T) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(washerwoman, RoleOrd),
    character_assignment_state_at_time(T, WW, washerwoman),
    reminder_on(ww_townsfolk, PTownsfolk, T),
    reminder_on(ww_wrong, PWrong, T),
    chair_ordered(PTownsfolk, PWrong, First, Second),
    functioning(WW, T).

% Not functioning (poisoned/drunk): ST may name any townsfolk role (no truthfulness constraint)
{ st_tells_core(washerwoman, WW, info(First, Second, R), T) : townsfolk(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(washerwoman, RoleOrd),
    character_assignment_state_at_time(T, WW, washerwoman),
    reminder_on(ww_townsfolk, PTownsfolk, T),
    reminder_on(ww_wrong, PWrong, T),
    chair_ordered(PTownsfolk, PWrong, First, Second),
    not functioning(WW, T).
