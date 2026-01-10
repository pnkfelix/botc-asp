% Shabaloth
% Each night*, choose 2 players: they die. A dead player you chose last night might be regurgitated.

other_night_substep(shabaloth, 1, st_asks(choose_two)).
other_night_substep(shabaloth, 2, player_chooses(target1)).
other_night_substep(shabaloth, 3, player_chooses(target2)).
other_night_substep(shabaloth, 4, st_places(shabaloth_dead)).
other_night_substep(shabaloth, 5, st_places(shabaloth_regurgitated)).

% Shabaloth chooses 2 players to kill
{ shabaloth_target(Shabaloth, P, 1, N) : player(P), P != Shabaloth, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(shabaloth, RoleOrd),
    assigned(0, Shabaloth, shabaloth),
    alive(Shabaloth, night(N, 0, 0)),
    not demon_blocked(Shabaloth, N),
    game_active(night(N, 0, 0)).

{ shabaloth_target(Shabaloth, P, 2, N) : player(P), P != Shabaloth, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(shabaloth, RoleOrd),
    assigned(0, Shabaloth, shabaloth),
    alive(Shabaloth, night(N, 0, 0)),
    not demon_blocked(Shabaloth, N),
    game_active(night(N, 0, 0)).

% Targets must be different
:- shabaloth_target(S, P, 1, N), shabaloth_target(S, P, 2, N).

% Connect to player_chooses
player_chooses(shabaloth, Shabaloth, point(P), night(N, RoleOrd, 2)) :-
    other_night_role_order(shabaloth, RoleOrd),
    shabaloth_target(Shabaloth, P, 1, N).

player_chooses(shabaloth, Shabaloth, point(P), night(N, RoleOrd, 3)) :-
    other_night_role_order(shabaloth, RoleOrd),
    shabaloth_target(Shabaloth, P, 2, N).

% Place the dead token when Shabaloth targets someone (if functioning)
reminder_on(shabaloth_dead, P, night(N, RoleOrd, 4)) :-
    shabaloth_target(Shabaloth, P, _, N),
    assigned(0, Shabaloth, shabaloth),
    night_number(N), N > 1,
    other_night_role_order(shabaloth, RoleOrd),
    functioning(Shabaloth, night(N, RoleOrd, 4)).

% Kill succeeds where the token is present and target is not protected
shabaloth_kill_succeeds(P, T) :-
    reminder_on(shabaloth_dead, P, T),
    not protected_from_demon(P, T).

dead_at_time(P, T) :- shabaloth_kill_succeeds(P, T).

% Death at final night time (works because token persists)
died(P, T) :-
    final_night_time(N, T),
    shabaloth_kill_succeeds(P, T),
    alive(P, night(N, 0, 0)).

% === Regurgitation ===
% A dead player killed last night might come back to life
shabaloth_killed_last_night(P, N) :-
    night_number(N), N > 2,
    shabaloth_target(_, P, _, N-1),
    died(P, T), time(T), T = night(N-1, R, S).

% ST may choose to regurgitate one of last night's victims
{ shabaloth_regurgitates(P, N) : shabaloth_killed_last_night(P, N) } <= 1 :-
    night_number(N), N > 2,
    assigned(0, Shabaloth, shabaloth),
    alive(Shabaloth, night(N, 0, 0)),
    functioning(Shabaloth, night(N, 0, 0)).

reminder_on(shabaloth_alive, P, night(N, RoleOrd, 5)) :-
    other_night_role_order(shabaloth, RoleOrd),
    shabaloth_regurgitates(P, N).

% Regurgitated player comes back to life
resurrected(P, N) :- shabaloth_regurgitates(P, N).

alive(P, T) :-
    shabaloth_regurgitates(P, N),
    time(T), T = night(M, R, S), M >= N.

alive(P, T) :-
    shabaloth_regurgitates(P, N),
    time(T), T = day(M, Phase), M >= N.
