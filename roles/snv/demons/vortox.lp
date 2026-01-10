% Vortox
% Each night*, choose a player: they die. Townsfolk abilities yield false info. Each day, if no one is executed, evil wins.

other_night_substep(vortox, 1, st_asks(choose_target)).
other_night_substep(vortox, 2, player_chooses(kill_target)).
other_night_substep(vortox, 3, st_places(vortox_dead)).

% Vortox chooses a target each night (nights after the first)
% Check role at action time T, not night start (handles Snake Charmer swaps etc.)
{ player_chooses(vortox, Vortox, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(vortox, RoleOrd),
    character_assignment_state_at_time(T, Vortox, vortox),
    alive(Vortox, T),
    game_active(night(N, 0, 0)).

% Place the death token on the target
reminder_on(vortox_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(vortox, RoleOrd),
    player_chooses(vortox, Vortox, point(Target), night(N, RoleOrd, 2)),
    functioning(Vortox, night(N, RoleOrd, 3)).

% Register as demon kill token (generic handling in botc.lp)
demon_kill_token(vortox_dead).

% snv_demon_killed(Target, N) - Target was killed by demon on night N
snv_demon_killed(Target, N) :-
    night_number(N), N > 1,
    other_night_role_order(vortox, RoleOrd),
    reminder_on(vortox_dead, Target, night(N, RoleOrd, 3)).

% Vortox is in play
vortox_in_play :-
    assigned(0, _, vortox).

% All Townsfolk are effectively poisoned (their abilities yield false info)
% This is modeled by making all townsfolk "impaired" when Vortox is in play
reminder_on(vortox_false_info, P, T) :-
    vortox_in_play,
    assigned(0, P, R),
    townsfolk(R),
    time(T).

causes_impairment(vortox_false_info).

% Each day, if no one is executed, evil wins
% Track if someone was executed each day
execution_happened(N) :-
    executed(_, N).

% If no execution on a day and game is still active, evil wins
evil_wins(day(N, exec)) :-
    day_number(N),
    vortox_in_play,
    alive(Vortox, day(N, 0)),
    assigned(0, Vortox, vortox),
    not execution_happened(N),
    game_active(day(N, 0)).
