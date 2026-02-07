% Gambler
% Each night*, choose a player & guess their character: if you guess wrong, you die.

other_night_substep(gambler, 1, st_asks(choose_player_and_role)).
other_night_substep(gambler, 2, player_chooses(target)).
other_night_substep(gambler, 3, player_chooses(role_guess)).
other_night_substep(gambler, 4, st_places(gambler_dead)).

% Target selection: Gambler picks a player (separate from guess: different substep time)
{ gambler_target(Gambler, P, night(N, RoleOrd, 2)) : player(P) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(gambler, RoleOrd),
    assigned(0, Gambler, gambler),
    alive(Gambler, night(N, 0, 0)).

% Role guess: Gambler picks a character (separate from target: different substep and value domain)
{ gambler_guess(Gambler, R, night(N, RoleOrd, 3)) : role(R) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(gambler, RoleOrd),
    assigned(0, Gambler, gambler),
    alive(Gambler, night(N, 0, 0)).

% Bridge target to player_chooses (separate from guess bridge: different source predicate and time)
player_chooses(gambler, Gambler, point(P), T) :- gambler_target(Gambler, P, T).
% Bridge guess to player_chooses (separate from target bridge: different value type)
player_chooses(gambler, Gambler, point(R), T) :- gambler_guess(Gambler, R, T).

% Helper: guess is correct when target's actual role matches the guessed character
gambler_correct(Gambler, N) :-
    night_number(N), N > 1,
    assigned(0, Gambler, gambler),
    other_night_role_order(gambler, RoleOrd),
    gambler_target(Gambler, P, night(N, RoleOrd, 2)),
    gambler_guess(Gambler, R, night(N, RoleOrd, 3)),
    assigned(0, P, R).

% Gambler dies if functioning and guess was wrong (negation of gambler_correct)
gambler_dies(Gambler, N) :-
    night_number(N), N > 1,
    assigned(0, Gambler, gambler),
    alive(Gambler, night(N, 0, 0)),
    other_night_role_order(gambler, RoleOrd),
    functioning(Gambler, night(N, RoleOrd, 4)),
    not gambler_correct(Gambler, N).

% Place death token on the Gambler for grimoire display
reminder_on(gambler_dead, Gambler, night(N, RoleOrd, 4)) :-
    other_night_role_order(gambler, RoleOrd),
    gambler_dies(Gambler, N).

% Mark Gambler as dead at the instant of the kill (for mid-night state tracking)
dead_at_time(P, T) :- reminder_on(gambler_dead, P, T).
% Record Gambler death at final night time for dawn processing (separate: different timing semantics)
died(P, T) :- gambler_dies(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)).
