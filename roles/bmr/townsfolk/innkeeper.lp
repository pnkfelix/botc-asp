% Innkeeper
% Each night*, choose 2 players: they can't die tonight, but 1 is drunk until dusk.

other_night_substep(innkeeper, 1, st_asks(choose_two)).
other_night_substep(innkeeper, 2, player_chooses(target1)).
other_night_substep(innkeeper, 3, player_chooses(target2)).
other_night_substep(innkeeper, 4, st_places(innkeeper_protected)).
other_night_substep(innkeeper, 5, st_places(innkeeper_drunk)).

% Target 1 selection: picks one alive player (separate from target 2: different substep time)
{ ik_choice(IK, P, 1, night(N, RoleOrd, 2)) : player(P), alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(innkeeper, RoleOrd),
    assigned(0, IK, innkeeper),
    alive(IK, night(N, 0, 0)).

% Target 2 selection: separate slot for second player (can't merge with target 1: different substep)
{ ik_choice(IK, P, 2, night(N, RoleOrd, 3)) : player(P), alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(innkeeper, RoleOrd),
    assigned(0, IK, innkeeper),
    alive(IK, night(N, 0, 0)).

% Constraint: two targets must be different players
:- ik_choice(IK, P, 1, T1), ik_choice(IK, P, 2, T2),
   T1 = night(N, R, 2), T2 = night(N, R, 3).

% Bridge target 1 to player_chooses (separate rules per slot: different source times)
player_chooses(innkeeper, IK, point(P), T) :- ik_choice(IK, P, 1, T).
% Bridge target 2 to player_chooses
player_chooses(innkeeper, IK, point(P), T) :- ik_choice(IK, P, 2, T).

% Place protection token on both chosen players if Innkeeper is functioning
reminder_on(ik_protected, P, night(N, RoleOrd, 4)) :-
    night_number(N), N > 1,
    other_night_role_order(innkeeper, RoleOrd),
    assigned(0, IK, innkeeper),
    functioning(IK, night(N, RoleOrd, 4)),
    ik_choice(IK, P, _, night(N, RoleOrd, _)).

% ST chooses which of the two targets becomes drunk (disjunctive choice; exactly one)
{ ik_drunk_target(P1, night(N, RoleOrd, 5));
  ik_drunk_target(P2, night(N, RoleOrd, 5)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(innkeeper, RoleOrd),
    assigned(0, IK, innkeeper),
    functioning(IK, night(N, RoleOrd, 5)),
    ik_choice(IK, P1, 1, night(N, RoleOrd, 2)),
    ik_choice(IK, P2, 2, night(N, RoleOrd, 3)).

% Place drunk token on the chosen target
reminder_on(ik_drunk, P, T) :- ik_drunk_target(P, T).

% Bridge: ik_protected token grants protection from demon kills
protected_from_demon(P, T) :- reminder_on(ik_protected, P, T).

% Innkeeper drunk causes impairment
causes_impairment(ik_drunk).

% Remove protection token at dawn (protection only lasts one night)
token_removed(ik_protected, P, dawn(N)) :-
    reminder_on(ik_protected, P, day(N-1, exec)).

% Remove drunk token at dawn (separate from protection: different token type)
token_removed(ik_drunk, P, dawn(N)) :-
    reminder_on(ik_drunk, P, day(N-1, exec)).
