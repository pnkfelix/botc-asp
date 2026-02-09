% Librarian

first_night_substep(librarian, 1, st_places(lib_outsider)).
first_night_substep(librarian, 2, st_places(lib_wrong)).
first_night_substep(librarian, 3, st_tells(lib_info)).

% ST secretly picks one player to be the outsider-reference; separate from lib_wrong placement below
{ reminder_on(lib_outsider, P, T) : player(P) } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, _, librarian).

% ST picks a second player as the decoy; must be a distinct step so chair_ordered can pair them
{ reminder_on(lib_wrong, P, T) : player(P) } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, _, librarian).

% The two tokens must be on different players (otherwise the "two players, one is X" info is degenerate)
:- reminder_on(lib_outsider, P, _),
   reminder_on(lib_wrong, P, _).

% When functioning, the lib_outsider token must actually be on someone who registers as outsider
:- T = night(1, RoleOrd, 3),
   first_night_role_order(librarian, RoleOrd),
   reminder_on(lib_outsider, P, T),
   character_assignment_state_at_time(T, LibrarianPlayer, librarian),
   functioning(LibrarianPlayer, T),
   not registers_as(P, outsider, T).

% Functioning: ST tells a role the outsider-reference may register as (constrained to truthful options)
{ st_tells_core(librarian, LIB, info(First, Second, R), T) : may_register_as_role(POutsider, R, T) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, LIB, librarian),
    reminder_on(lib_outsider, POutsider, T),
    reminder_on(lib_wrong, PWrong, T),
    chair_ordered(POutsider, PWrong, First, Second),
    functioning(LIB, T).

% Not functioning (poisoned/drunk): ST may name any outsider role (no truthfulness constraint)
{ st_tells_core(librarian, LIB, info(First, Second, R), T) : outsider(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, LIB, librarian),
    reminder_on(lib_outsider, POutsider, T),
    reminder_on(lib_wrong, PWrong, T),
    chair_ordered(POutsider, PWrong, First, Second),
    not functioning(LIB, T).
