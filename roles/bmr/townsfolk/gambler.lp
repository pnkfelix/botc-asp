% Gambler
% Each night*, choose a player & guess their character: if you guess wrong, you die.

other_night_substep(gambler, 1, st_asks(choose_player_and_role)).
other_night_substep(gambler, 2, player_chooses(target)).
other_night_substep(gambler, 3, player_chooses(role_guess)).
other_night_substep(gambler, 4, st_places(gambler_dead)).

% Gambler chooses a player and guesses their role
{ gambler_target(Gambler, P, night(N, RoleOrd, 2)) : player(P) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(gambler, RoleOrd),
    assigned(0, Gambler, gambler),
    alive(Gambler, night(N, 0, 0)).

{ gambler_guess(Gambler, R, night(N, RoleOrd, 3)) : role(R) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(gambler, RoleOrd),
    assigned(0, Gambler, gambler),
    alive(Gambler, night(N, 0, 0)).

% Connect to player_chooses
player_chooses(gambler, Gambler, point(P), T) :- gambler_target(Gambler, P, T).
player_chooses(gambler, Gambler, point(R), T) :- gambler_guess(Gambler, R, T).

% Check if guess is correct
gambler_correct(Gambler, N) :-
    night_number(N), N > 1,
    assigned(0, Gambler, gambler),
    other_night_role_order(gambler, RoleOrd),
    gambler_target(Gambler, P, night(N, RoleOrd, 2)),
    gambler_guess(Gambler, R, night(N, RoleOrd, 3)),
    assigned(0, P, R).

% Gambler dies if they guess wrong while functioning
gambler_dies(Gambler, N) :-
    night_number(N), N > 1,
    assigned(0, Gambler, gambler),
    alive(Gambler, night(N, 0, 0)),
    other_night_role_order(gambler, RoleOrd),
    functioning(Gambler, night(N, RoleOrd, 4)),
    not gambler_correct(Gambler, N).

reminder_on(gambler_dead, Gambler, night(N, RoleOrd, 4)) :-
    other_night_role_order(gambler, RoleOrd),
    gambler_dies(Gambler, N).

% Gambler death
dead(P, T) :- reminder_on(gambler_dead, P, T).
died(P, T) :- gambler_dies(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)).
