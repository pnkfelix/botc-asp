% Exorcist
% Each night*, choose a player (different to last night): the Demon, if chosen, learns who you are then doesn't wake tonight.

other_night_substep(exorcist, 1, st_asks(choose_target)).
other_night_substep(exorcist, 2, player_chooses(target)).
other_night_substep(exorcist, 3, st_places(exorcist_chosen)).

% Exorcist chooses a player each night (different from last night)
{ player_chooses(exorcist, Exorcist, point(P), night(N, RoleOrd, 2)) : player(P), alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(exorcist, RoleOrd),
    assigned(0, Exorcist, exorcist),
    alive(Exorcist, night(N, 0, 0)).

% Cannot choose same player as last night
:- player_chooses(exorcist, Exorcist, point(P), night(N, RoleOrd, 2)),
   player_chooses(exorcist, Exorcist, point(P), night(N-1, RoleOrd2, 2)),
   other_night_role_order(exorcist, RoleOrd),
   other_night_role_order(exorcist, RoleOrd2),
   night_number(N), N > 2.

% Place reminder on chosen player
reminder_on(exorcist_chosen, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(exorcist, RoleOrd),
    player_chooses(exorcist, Exorcist, point(P), night(N, RoleOrd, 2)),
    functioning(Exorcist, night(N, RoleOrd, 2)).

% Token removed at dawn
token_removed(exorcist_chosen, P, dawn(N)) :-
    reminder_on(exorcist_chosen, P, day(N, exec)).

% If demon is chosen, demon learns who the Exorcist is
% (ST wakes demon briefly to show them the Exorcist)
exorcist_chose_demon(Exorcist, Demon, N) :-
    night_number(N), N > 1,
    assigned(0, Exorcist, exorcist),
    assigned(0, Demon, D), demon(D),
    other_night_role_order(exorcist, RoleOrd),
    player_chooses(exorcist, Exorcist, point(Demon), night(N, RoleOrd, 2)),
    functioning(Exorcist, night(N, RoleOrd, 2)).

% Demon doesn't wake tonight if exorcised
demon_exorcised(Demon, N) :- exorcist_chose_demon(_, Demon, N).

% This prevents demon kill actions (used by demon role files)
% demon_blocked(Demon, N) means the demon cannot act on night N
demon_blocked(Demon, N) :- demon_exorcised(Demon, N).
