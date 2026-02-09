% Exorcist
% Each night*, choose a player (different to last night): the Demon, if chosen, learns who you are then doesn't wake tonight.

other_night_substep(exorcist, 1, st_asks(choose_target)).
other_night_substep(exorcist, 2, player_chooses(target)).
other_night_substep(exorcist, 3, st_places(exorcist_chosen)).

% Exorcist picks one alive player each night (other-night only; no first-night action)
{ player_chooses(exorcist, Exorcist, point(P), night(N, RoleOrd, 2)) : player(P), alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(exorcist, RoleOrd),
    assigned(0, Exorcist, exorcist),
    alive(Exorcist, night(N, 0, 0)).

% Constraint: cannot choose the same player as last night (enforces "different to last night" text)
:- player_chooses(exorcist, Exorcist, point(P), night(N, RoleOrd, 2)),
   player_chooses(exorcist, Exorcist, point(P), night(N-1, RoleOrd2, 2)),
   other_night_role_order(exorcist, RoleOrd),
   other_night_role_order(exorcist, RoleOrd2),
   night_number(N), N > 2.

% Place reminder token on chosen player if Exorcist is functioning (token drives demon-block check)
reminder_on(exorcist_chosen, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(exorcist, RoleOrd),
    player_chooses(exorcist, Exorcist, point(P), night(N, RoleOrd, 2)),
    functioning(Exorcist, night(N, RoleOrd, 2)).

% Remove token at dawn (exorcism only lasts one night)
token_removed(exorcist_chosen, P, dawn(N)) :-
    reminder_on(exorcist_chosen, P, day(N, exec)).

% Helper: true when Exorcist chose the demon while functioning (triggers demon notification + block)
exorcist_chose_demon(Exorcist, Demon, N) :-
    night_number(N), N > 1,
    assigned(0, Exorcist, exorcist),
    assigned(0, Demon, D), demon(D),
    other_night_role_order(exorcist, RoleOrd),
    player_chooses(exorcist, Exorcist, point(Demon), night(N, RoleOrd, 2)),
    functioning(Exorcist, night(N, RoleOrd, 2)).

% Helper: marks the demon as exorcised on this night (can't inline: provides a named concept)
demon_exorcised(Demon, N) :- exorcist_chose_demon(_, Demon, N).

% Bridge to generic demon_blocked: prevents the demon from acting tonight (used by all demon kill rules)
demon_blocked(Demon, N) :- demon_exorcised(Demon, N).
