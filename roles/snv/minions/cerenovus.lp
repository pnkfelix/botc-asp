% Cerenovus
% Each night, choose a player & a good character: they are "mad" they are this character tomorrow, or might be executed.

first_night_substep(cerenovus, 1, st_asks(choose_target_and_character)).
first_night_substep(cerenovus, 2, player_chooses(target_and_character)).
first_night_substep(cerenovus, 3, st_places(cerenovus_mad)).
first_night_substep(cerenovus, 4, st_tells(cerenovus_target_info)).

other_night_substep(cerenovus, 1, st_asks(choose_target_and_character)).
other_night_substep(cerenovus, 2, player_chooses(target_and_character)).
other_night_substep(cerenovus, 3, st_places(cerenovus_mad)).
other_night_substep(cerenovus, 4, st_tells(cerenovus_target_info)).

% Night 1 choice: Cerenovus picks a player and character (can't merge with other nights: different role order)
{ cerenovus_chooses(Cerenovus, P, Role, night(1, RoleOrd, 2)) :
    player(P), P != Cerenovus, good_character(Role) } = 1 :-
    first_night_role_order(cerenovus, RoleOrd),
    assigned(0, Cerenovus, cerenovus).

% Other nights choice: adds alive check (separate from night 1: uses other_night_role_order)
{ cerenovus_chooses(Cerenovus, P, Role, night(N, RoleOrd, 2)) :
    player(P), P != Cerenovus, good_character(Role) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(cerenovus, RoleOrd),
    assigned(0, Cerenovus, cerenovus),
    alive(Cerenovus, night(N, 0, 0)).

% Bridge target player to player_chooses (separate from role bridge: different value type)
player_chooses(cerenovus, Cerenovus, point(P), T) :-
    cerenovus_chooses(Cerenovus, P, _, T).

% Bridge character choice to player_chooses (separate from target bridge: different value)
player_chooses(cerenovus, Cerenovus, point(Role), T) :-
    cerenovus_chooses(Cerenovus, _, Role, T).

% Place mad token on chosen player when functioning (token tracks madness obligation for the next day)
reminder_on(cerenovus_mad, P, night(N, RoleOrd, 3)) :-
    cerenovus_chooses(Cerenovus, P, _, night(N, RoleOrd, 2)),
    functioning(Cerenovus, night(N, RoleOrd, 3)).

% Tell target which role they must be mad as when functioning (separate substep from token placement)
st_tells_core(cerenovus, P, mad_as(Role), night(N, RoleOrd, 4)) :-
    cerenovus_chooses(Cerenovus, P, Role, night(N, RoleOrd, 2)),
    functioning(Cerenovus, night(N, RoleOrd, 4)).

% Helper: track which character the player must be mad about (separate from reminder_on: carries Role data)
cerenovus_mad_character(P, Role, N) :-
    cerenovus_chooses(Cerenovus, P, Role, night(N, _, _)),
    functioning(Cerenovus, night(N, _, 3)).

% Remove mad token at start of next night (madness lasts only one day)
token_removed(cerenovus_mad, P, night(N+1, 0, 0)) :-
    reminder_on(cerenovus_mad, P, day(N, exec)),
    night_number(N+1).

% Integrity constraint: token cannot simultaneously exist and be removed at the same time point
:- reminder_on(cerenovus_mad, P, T), token_removed(cerenovus_mad, P, T).

% Helper: if player broke madness while mad token active, ST may execute them (input: cerenovus_broke_madness)
cerenovus_may_execute(P, N) :-
    cerenovus_broke_madness(P, N),
    reminder_on(cerenovus_mad, P, day(N, 0)).
