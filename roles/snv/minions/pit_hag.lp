% Pit-Hag
% Each night*, choose a player & a character they become (if not in play). If a Demon is made, deaths tonight are arbitrary.

other_night_substep(pit_hag, 1, st_asks(choose_target_and_character)).
other_night_substep(pit_hag, 2, player_chooses(target_and_character)).
other_night_substep(pit_hag, 3, st_places(pit_hag_change)).

% Helper: role is in play from initial assignment (separate from pit_hag source: different origin)
role_in_play(R) :- assigned(0, _, R).
% Helper: role is in play from pit_hag change onwards (separate from assigned: tracks dynamic changes)
role_in_play(R) :- pit_hag_changed(_, R, N), night_number(N2), N2 >= N.

% Other nights choice: Pit-Hag picks a player and a not-in-play character (other nights only: no night 1 action)
{ pit_hag_chooses(PitHag, P, Role, night(N, RoleOrd, 2)) :
    player(P), P != PitHag, role(Role), not role_in_play(Role) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(pit_hag, RoleOrd),
    assigned(0, PitHag, pit_hag),
    alive(PitHag, night(N, 0, 0)).

% Bridge target player to player_chooses (separate from role bridge: different value type)
player_chooses(pit_hag, PitHag, point(P), T) :-
    pit_hag_chooses(PitHag, P, _, T).

% Bridge character choice to player_chooses (separate from target bridge: different value)
player_chooses(pit_hag, PitHag, point(Role), T) :-
    pit_hag_chooses(PitHag, _, Role, T).

% Helper: record the change when functioning (separate from role_assignment_changes_to: tracks night number for role_in_play)
pit_hag_changed(P, Role, N) :-
    pit_hag_chooses(PitHag, P, Role, night(N, RoleOrd, 2)),
    functioning(PitHag, night(N, RoleOrd, 3)).

% Bridge: apply role change at dawn via the generic role assignment system
role_assignment_changes_to(dawn(N), P, Role) :-
    pit_hag_changed(P, Role, N),
    day_number(N).

% Helper: true when Pit-Hag created a demon this night (triggers arbitrary deaths rule)
pit_hag_created_demon(N) :-
    pit_hag_changed(_, Role, N),
    demon(Role).

% Flag: when a demon is created, deaths tonight become arbitrary (ST chooses freely)
pit_hag_arbitrary_deaths(N) :-
    pit_hag_created_demon(N).

% Note: arbitrary death override for demon kill mechanics may need further refinement
