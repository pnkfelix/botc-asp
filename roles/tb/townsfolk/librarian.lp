% Librarian

first_night_substep(librarian, 1, st_places(lib_outsider)).
first_night_substep(librarian, 2, st_places(lib_wrong)).
first_night_substep(librarian, 3, st_tells(lib_info)).

% Helper: some player registers as outsider at the librarian's info step
lib_outsider_available :-
    first_night_role_order(librarian, RoleOrd),
    T = night(1, RoleOrd, 3),
    player(P),
    registers_as(P, outsider, T).

% ST secretly picks one player to be the outsider-reference (only when outsiders are available)
{ reminder_on(lib_outsider, P, T) : player(P) } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, _, librarian),
    lib_outsider_available.

% ST picks a second player as the decoy (only when outsiders are available)
{ reminder_on(lib_wrong, P, T) : player(P) } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, _, librarian),
    lib_outsider_available.

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

% Functioning: ST tells a role the outsider-reference may register as (constrained to outsider roles)
{ st_tells_core(librarian, LIB, info(First, Second, R), T) : may_register_as_role(POutsider, R, T), outsider(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, LIB, librarian),
    reminder_on(lib_outsider, POutsider, T),
    reminder_on(lib_wrong, PWrong, T),
    chair_ordered(POutsider, PWrong, First, Second),
    functioning(LIB, T).

% Not functioning: ST may choose to lie with "no outsiders" even when outsiders exist
{ lib_tells_no_outsiders(LIB, T) } :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, LIB, librarian),
    not functioning(LIB, T),
    lib_outsider_available.

st_tells_core(librarian, LIB, no_outsiders, T) :- lib_tells_no_outsiders(LIB, T).

% Not functioning (poisoned/drunk): ST may name any outsider role (no truthfulness constraint)
% Only fires if the ST didn't choose to say "no outsiders" instead
{ st_tells_core(librarian, LIB, info(First, Second, R), T) : outsider(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, LIB, librarian),
    reminder_on(lib_outsider, POutsider, T),
    reminder_on(lib_wrong, PWrong, T),
    chair_ordered(POutsider, PWrong, First, Second),
    not functioning(LIB, T),
    not lib_tells_no_outsiders(LIB, T).

% No outsiders in game: ST tells librarian there are no outsiders (always truthful)
st_tells_core(librarian, LIB, no_outsiders, T) :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    character_assignment_state_at_time(T, LIB, librarian),
    not lib_outsider_available.
