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

% Cerenovus chooses a player and a good character on first night
{ cerenovus_chooses(Cerenovus, P, Role, night(1, RoleOrd, 2)) :
    player(P), P != Cerenovus, good_character(Role) } = 1 :-
    first_night_role_order(cerenovus, RoleOrd),
    assigned(0, Cerenovus, cerenovus).

% Cerenovus chooses on other nights (if alive)
{ cerenovus_chooses(Cerenovus, P, Role, night(N, RoleOrd, 2)) :
    player(P), P != Cerenovus, good_character(Role) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(cerenovus, RoleOrd),
    assigned(0, Cerenovus, cerenovus),
    alive(Cerenovus, night(N, 0, 0)).

% Convert to player_chooses for compatibility
player_chooses(cerenovus, Cerenovus, point(P), T) :-
    cerenovus_chooses(Cerenovus, P, _, T).

player_chooses(cerenovus, Cerenovus, point(Role), T) :-
    cerenovus_chooses(Cerenovus, _, Role, T).

% The "mad" token is placed on the chosen player
% They must pretend to be the specified character the next day
reminder_on(cerenovus_mad, P, night(N, RoleOrd, 3)) :-
    cerenovus_chooses(Cerenovus, P, _, night(N, RoleOrd, 2)),
    functioning(Cerenovus, night(N, RoleOrd, 3)).

% Storyteller wakes the target and tells them what role they must be mad as
st_tells_core(cerenovus, P, mad_as(Role), night(N, RoleOrd, 4)) :-
    cerenovus_chooses(Cerenovus, P, Role, night(N, RoleOrd, 2)),
    functioning(Cerenovus, night(N, RoleOrd, 4)).

% Track which character the player must be mad about
cerenovus_mad_character(P, Role, N) :-
    cerenovus_chooses(Cerenovus, P, Role, night(N, _, _)),
    functioning(Cerenovus, night(N, _, 3)).

% Remove madness at end of day
token_removed(cerenovus_mad, P, night(N+1, 0, 0)) :-
    reminder_on(cerenovus_mad, P, day(N, exec)),
    night_number(N+1).

:- reminder_on(cerenovus_mad, P, T), token_removed(cerenovus_mad, P, T).

% cerenovus_broke_madness(Player, Day) - input predicate if they failed to be mad
% If they break madness, ST may execute them
cerenovus_may_execute(P, N) :-
    cerenovus_broke_madness(P, N),
    reminder_on(cerenovus_mad, P, day(N, 0)).
