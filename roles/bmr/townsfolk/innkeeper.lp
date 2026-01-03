% Innkeeper
% Each night*, choose 2 players: they can't die tonight, but 1 is drunk until dusk.

other_night_substep(innkeeper, 1, st_asks(choose_two)).
other_night_substep(innkeeper, 2, player_chooses(target1)).
other_night_substep(innkeeper, 3, player_chooses(target2)).
other_night_substep(innkeeper, 4, st_places(innkeeper_protected)).
other_night_substep(innkeeper, 5, st_places(innkeeper_drunk)).

% Innkeeper chooses 2 different alive players
{ ik_choice(IK, P, 1, night(N, RoleOrd, 2)) : player(P), alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(innkeeper, RoleOrd),
    assigned(0, IK, innkeeper),
    alive(IK, night(N, 0, 0)).

{ ik_choice(IK, P, 2, night(N, RoleOrd, 3)) : player(P), alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(innkeeper, RoleOrd),
    assigned(0, IK, innkeeper),
    alive(IK, night(N, 0, 0)).

% Choices must be different players
:- ik_choice(IK, P, 1, T1), ik_choice(IK, P, 2, T2),
   T1 = night(N, R, 2), T2 = night(N, R, 3).

% Connect to player_chooses
player_chooses(innkeeper, IK, point(P), T) :- ik_choice(IK, P, 1, T).
player_chooses(innkeeper, IK, point(P), T) :- ik_choice(IK, P, 2, T).

% Both players are protected (if Innkeeper is functioning)
reminder_on(ik_protected, P, night(N, RoleOrd, 4)) :-
    night_number(N), N > 1,
    other_night_role_order(innkeeper, RoleOrd),
    assigned(0, IK, innkeeper),
    functioning(IK, night(N, RoleOrd, 4)),
    ik_choice(IK, P, _, night(N, RoleOrd, _)).

% One of them is drunk (ST chooses)
{ ik_drunk_target(P1, night(N, RoleOrd, 5));
  ik_drunk_target(P2, night(N, RoleOrd, 5)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(innkeeper, RoleOrd),
    assigned(0, IK, innkeeper),
    functioning(IK, night(N, RoleOrd, 5)),
    ik_choice(IK, P1, 1, night(N, RoleOrd, 2)),
    ik_choice(IK, P2, 2, night(N, RoleOrd, 3)).

reminder_on(ik_drunk, P, T) :- ik_drunk_target(P, T).

% Protection from demon kills
protected_from_demon(P, T) :- reminder_on(ik_protected, P, T).

% Innkeeper drunk causes impairment
causes_impairment(ik_drunk).

% Remove tokens at dawn
token_removed(ik_protected, P, dawn(N)) :-
    reminder_on(ik_protected, P, day(N-1, exec)).

token_removed(ik_drunk, P, dawn(N)) :-
    reminder_on(ik_drunk, P, day(N-1, exec)).
