% Shabaloth
% Each night*, choose 2 players: they die. A dead player you chose last night might be regurgitated.

other_night_substep(shabaloth, 1, st_asks(choose_two)).
other_night_substep(shabaloth, 2, player_chooses(target1)).
other_night_substep(shabaloth, 3, player_chooses(target2)).
other_night_substep(shabaloth, 4, st_places(shabaloth_dead)).
other_night_substep(shabaloth, 5, st_places(shabaloth_regurgitated)).

% First target selection; separate from second target because they map to different substeps
{ shabaloth_target(Shabaloth, P, 1, N) : player(P), P != Shabaloth, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(shabaloth, RoleOrd),
    assigned(0, Shabaloth, shabaloth),
    alive(Shabaloth, night(N, 0, 0)),
    not demon_blocked(Shabaloth, N),
    game_active(night(N, 0, 0)).

% Second target selection; can't merge with first: each maps to its own player_chooses substep
{ shabaloth_target(Shabaloth, P, 2, N) : player(P), P != Shabaloth, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(shabaloth, RoleOrd),
    assigned(0, Shabaloth, shabaloth),
    alive(Shabaloth, night(N, 0, 0)),
    not demon_blocked(Shabaloth, N),
    game_active(night(N, 0, 0)).

% The two targets must be different players (can't kill the same person twice)
:- shabaloth_target(S, P, 1, N), shabaloth_target(S, P, 2, N).

% Bridge first target to player_chooses at substep 2
player_chooses(shabaloth, Shabaloth, point(P), night(N, RoleOrd, 2)) :-
    other_night_role_order(shabaloth, RoleOrd),
    shabaloth_target(Shabaloth, P, 1, N).

% Bridge second target to player_chooses at substep 3
player_chooses(shabaloth, Shabaloth, point(P), night(N, RoleOrd, 3)) :-
    other_night_role_order(shabaloth, RoleOrd),
    shabaloth_target(Shabaloth, P, 2, N).

% Place kill token on both targets if functioning (both kills share one token type)
reminder_on(shabaloth_dead, P, night(N, RoleOrd, 4)) :-
    shabaloth_target(Shabaloth, P, _, N),
    assigned(0, Shabaloth, shabaloth),
    night_number(N), N > 1,
    other_night_role_order(shabaloth, RoleOrd),
    functioning(Shabaloth, night(N, RoleOrd, 4)).

% Register kill token so botc.lp's generic death logic processes it
demon_kill_token(shabaloth_dead).

% Remove stale kill tokens from previous night's targets (prevents inertia-carried kills)
token_removed(shabaloth_dead, P, night(N, RoleOrd, 4)) :-
    reminder_on(shabaloth_dead, P, night(N, 0, 0)),
    night_number(N), N > 1,
    other_night_role_order(shabaloth, RoleOrd).

% === Regurgitation ===

% Helper: identifies players the Shabaloth killed last night (candidates for regurgitation)
shabaloth_killed_last_night(P, N) :-
    night_number(N), N > 2,
    shabaloth_target(_, P, _, N-1),
    died(P, T), time(T), T = night(N-1, R, S).

% ST may optionally bring back one of last night's victims (<=1 means 0 or 1; only if functioning)
{ shabaloth_regurgitates(P, N) : shabaloth_killed_last_night(P, N) } <= 1 :-
    night_number(N), N > 2,
    assigned(0, Shabaloth, shabaloth),
    alive(Shabaloth, night(N, 0, 0)),
    functioning(Shabaloth, night(N, 0, 0)).

% Place the "alive" token on the regurgitated player for grimoire display
reminder_on(shabaloth_alive, P, night(N, RoleOrd, 5)) :-
    other_night_role_order(shabaloth, RoleOrd),
    shabaloth_regurgitates(P, N).

% Register the resurrection event for generic resurrection handling
resurrected(P, N) :- shabaloth_regurgitates(P, N).

% Regurgitated player is alive for all subsequent nights (separate from days: different time structure)
alive(P, T) :-
    shabaloth_regurgitates(P, N),
    time(T), T = night(M, R, S), M >= N.

% Regurgitated player is alive for all subsequent days (can't merge with nights: different unification)
alive(P, T) :-
    shabaloth_regurgitates(P, N),
    time(T), T = day(M, Phase), M >= N.
