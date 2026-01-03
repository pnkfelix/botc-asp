% Professor
% Once per game, at night*, choose a dead player: if they are a Townsfolk, they are resurrected.

other_night_substep(professor, 1, st_asks(choose_dead_player)).
other_night_substep(professor, 2, player_chooses(target)).
other_night_substep(professor, 3, st_places(professor_alive)).

% Track if Professor has used their ability
professor_used_ability(Professor, N) :-
    assigned(0, Professor, professor),
    night_number(N),
    professor_chose(Professor, _, N).

% Professor may choose a dead player (once per game, on nights after the first)
{ professor_chose(Professor, P, N) : player(P), dead(P, night(N, 0, 0)) } <= 1 :-
    night_number(N), N > 1,
    other_night_role_order(professor, RoleOrd),
    assigned(0, Professor, professor),
    alive(Professor, night(N, 0, 0)),
    not professor_used_ability_before(Professor, N).

professor_used_ability_before(Professor, N) :-
    professor_used_ability(Professor, M),
    night_number(N),
    M < N.

% Connect to player_chooses
player_chooses(professor, Professor, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(professor, RoleOrd),
    professor_chose(Professor, P, N).

% If the chosen player is a Townsfolk and Professor is functioning, they are resurrected
professor_resurrects(P, N) :-
    professor_chose(Professor, P, N),
    assigned(0, Professor, professor),
    night_number(N), N > 1,
    other_night_role_order(professor, RoleOrd),
    functioning(Professor, night(N, RoleOrd, 2)),
    assigned(0, P, R),
    townsfolk(R).

% Mark resurrection
reminder_on(professor_alive, P, night(N, RoleOrd, 3)) :-
    other_night_role_order(professor, RoleOrd),
    professor_resurrects(P, N).

% Resurrected player becomes alive
% This overrides the dead state
resurrected(P, N) :- professor_resurrects(P, N).

% Alive after resurrection
alive(P, T) :-
    resurrected(P, N),
    time(T),
    T = night(M, R, S), M >= N.

alive(P, T) :-
    resurrected(P, N),
    time(T),
    T = day(M, Phase), M >= N.

alive(P, T) :-
    resurrected(P, N),
    time(T),
    T = dawn(M), M >= N.

% Remove death state after resurrection
% The player is no longer dead after being resurrected
:- dead(P, T), resurrected(P, N), time(T), T = night(M, R, S), M > N.
