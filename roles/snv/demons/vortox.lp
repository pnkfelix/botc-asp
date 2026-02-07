% Vortox
% Each night*, choose a player: they die. Townsfolk abilities yield false info. Each day, if no one is executed, evil wins.

other_night_substep(vortox, 1, st_asks(choose_target)).
other_night_substep(vortox, 2, player_chooses(kill_target)).
other_night_substep(vortox, 3, st_places(vortox_dead)).

% Choice rule: Vortox picks exactly one target each other-night (choice cannot be deterministic).
{ player_chooses(vortox, Vortox, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(vortox, RoleOrd),
    character_assignment_state_at_time(T, Vortox, vortox),
    alive(Vortox, T),
    game_active(night(N, 0, 0)).

% Places death token; deterministic derivation from choice, separate because it checks functioning at substep 3.
reminder_on(vortox_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(vortox, RoleOrd),
    player_chooses(vortox, Vortox, point(Target), night(N, RoleOrd, 2)),
    functioning(Vortox, night(N, RoleOrd, 3)).

demon_kill_token(vortox_dead).

% Projects reminder into script-level snv_demon_killed/2; uses night-number so other roles can query without knowing substep.
snv_demon_killed(Target, N) :-
    night_number(N), N > 1,
    other_night_role_order(vortox, RoleOrd),
    reminder_on(vortox_dead, Target, night(N, RoleOrd, 3)).

% Auxiliary: existential check avoids repeating assigned/3 pattern in every downstream rule that needs "vortox exists".
vortox_in_play :-
    assigned(0, _, vortox).

% Places false-info impairment on all townsfolk at game start; inertia carries it forward so no per-time rule needed.
reminder_on(vortox_false_info, P, night(1, 0, 0)) :-
    vortox_in_play,
    assigned(0, P, R),
    townsfolk(R).

causes_impairment(vortox_false_info).

% Auxiliary: pre-computes whether an execution occurred; needed because negation ("not executed") requires a ground check.
execution_happened(N) :-
    executed(_, N).

% Evil wins if no execution happened; separate from execution_happened because negation requires the pre-computed auxiliary.
evil_wins(day(N, exec)) :-
    day_number(N),
    vortox_in_play,
    alive(Vortox, day(N, 0)),
    assigned(0, Vortox, vortox),
    not execution_happened(N),
    game_active(day(N, 0)).
