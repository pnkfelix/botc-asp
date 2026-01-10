% Pit-Hag
% Each night*, choose a player & a character they become (if not in play). If a Demon is made, deaths tonight are arbitrary.

other_night_substep(pit_hag, 1, st_asks(choose_target_and_character)).
other_night_substep(pit_hag, 2, player_chooses(target_and_character)).
other_night_substep(pit_hag, 3, st_places(pit_hag_change)).

% Characters currently in play
role_in_play(R) :- assigned(0, _, R).
role_in_play(R) :- pit_hag_changed(_, R, N), night_number(N2), N2 >= N.

% Pit-Hag chooses a player and a character (not in play) on other nights
{ pit_hag_chooses(PitHag, P, Role, night(N, RoleOrd, 2)) :
    player(P), P != PitHag, role(Role), not role_in_play(Role) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(pit_hag, RoleOrd),
    assigned(0, PitHag, pit_hag),
    alive(PitHag, night(N, 0, 0)).

% Convert to player_chooses for compatibility
player_chooses(pit_hag, PitHag, point(P), T) :-
    pit_hag_chooses(PitHag, P, _, T).

player_chooses(pit_hag, PitHag, point(Role), T) :-
    pit_hag_chooses(PitHag, _, Role, T).

% The player becomes the new character (if Pit-Hag is functioning)
pit_hag_changed(P, Role, N) :-
    pit_hag_chooses(PitHag, P, Role, night(N, RoleOrd, 2)),
    functioning(PitHag, night(N, RoleOrd, 3)).

% Role change is effective at dawn - use fine-grained predicate
role_assignment_changes_to(dawn(N), P, Role) :-
    pit_hag_changed(P, Role, N),
    day_number(N).

% Track if a demon was created this night
pit_hag_created_demon(N) :-
    pit_hag_changed(_, Role, N),
    demon(Role).

% If a demon is made, deaths tonight are arbitrary
% This means the ST can choose who dies (or no one dies)
pit_hag_arbitrary_deaths(N) :-
    pit_hag_created_demon(N).

% When arbitrary deaths are in effect, the normal demon kill is optional
% This is modeled by allowing any death pattern (including none)
% Note: This interacts with demon kill mechanics and may need refinement
