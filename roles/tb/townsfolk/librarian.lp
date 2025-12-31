% Librarian

first_night_substep(librarian, 1, st_places(lib_outsider)).
first_night_substep(librarian, 2, st_places(lib_wrong)).
first_night_substep(librarian, 3, st_tells(lib_info)).

{ reminder_on(lib_outsider, P, night(1, RoleOrd, 1)) : player(P) } = 1 :-
    first_night_role_order(librarian, RoleOrd),
    assigned(0, _, librarian).

{ reminder_on(lib_wrong, P, night(1, RoleOrd, 2)) : player(P) } = 1 :-
    first_night_role_order(librarian, RoleOrd),
    assigned(0, _, librarian).

:- reminder_on(lib_outsider, P, _),
   reminder_on(lib_wrong, P, _).

:- T = night(1, RoleOrd, 3),
   first_night_role_order(librarian, RoleOrd),
   reminder_on(lib_outsider, P, T),
   assigned(0, LibrarianPlayer, librarian),
   functioning(LibrarianPlayer, T),
   not registers_as(P, outsider, T).

{ st_tells_core(librarian, LIB, info(First, Second, R), T) : may_register_as_role(POutsider, R, T) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    assigned(0, LIB, librarian),
    reminder_on(lib_outsider, POutsider, T),
    reminder_on(lib_wrong, PWrong, T),
    chair_ordered(POutsider, PWrong, First, Second),
    functioning(LIB, T).

{ st_tells_core(librarian, LIB, info(First, Second, R), T) : outsider(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    assigned(0, LIB, librarian),
    reminder_on(lib_outsider, POutsider, T),
    reminder_on(lib_wrong, PWrong, T),
    chair_ordered(POutsider, PWrong, First, Second),
    not functioning(LIB, T).
