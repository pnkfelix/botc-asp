% Butler

first_night_substep(butler, 1, st_asks(choose_master)).
first_night_substep(butler, 2, player_chooses(master)).
first_night_substep(butler, 3, st_places(but_master)).

{ player_chooses(butler, Butler, point(P), T) : player(P), P != Butler } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(butler, RoleOrd),
    character_assignment_state_at_time(T, Butler, butler).

reminder_on(but_master, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(butler, RoleOrd),
    player_chooses(butler, _, point(P), night(1, RoleOrd, 2)).

% TODO: Add other night butler choice and voting restriction
