% Fang Gu
% Each night*, choose a player: they die. The 1st Outsider this kills becomes an evil Fang Gu & you die instead. [+1 Outsider]

causes_outsider_mod(fang_gu, 1).

other_night_substep(fang_gu, 1, st_asks(choose_target)).
other_night_substep(fang_gu, 2, player_chooses(kill_target)).
other_night_substep(fang_gu, 3, st_places(fang_gu_dead)).

% Choice rule: FG picks exactly one target each other-night (choice cannot be deterministic).
{ player_chooses(fang_gu, FG, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(fang_gu, RoleOrd),
    character_assignment_state_at_time(T, FG, fang_gu),
    alive(FG, T),
    game_active(night(N, 0, 0)).

% Auxiliary: existential check needed because fang_gu_jumped/3 args can't appear in negation directly.
fang_gu_has_jumped :- fang_gu_jumped(_, _, _).

% Detects first-outsider-kill case; separated from normal kill because negation (not has_jumped) and outsider check diverge.
fang_gu_targets_outsider(FG, Target, N) :-
    player_chooses(fang_gu, FG, point(Target), night(N, RoleOrd, 2)),
    other_night_role_order(fang_gu, RoleOrd),
    assigned(0, Target, R),
    outsider(R),
    not fang_gu_has_jumped,
    functioning(FG, night(N, RoleOrd, 3)).

% Separate predicate marks the jump event; downstream rules key off fang_gu_jumped not _targets_outsider.
fang_gu_jumped(FG, Target, N) :-
    fang_gu_targets_outsider(FG, Target, N).

% Places death token on old FG; separate from normal-kill reminder because the target here is FG, not the chosen player.
reminder_on(fang_gu_dead, FG, night(N, RoleOrd, 3)) :-
    fang_gu_jumped(FG, _, N),
    other_night_role_order(fang_gu, RoleOrd).

% Feeds script-level demon-kill tracking; jump kill (old FG dies) is separate from normal kill below.
snv_demon_killed(FG, N) :-
    fang_gu_jumped(FG, _, N).

% Directly marks old FG dead; can't rely on generic demon_kill because demon_kill_blocked suppresses it for the jumper.
dead_at_time(FG, T) :-
    fang_gu_jumped(FG, _, N),
    T = night(N, RoleOrd, 3),
    other_night_role_order(fang_gu, RoleOrd).

% Triggers died/2 for old FG; separate from dead_at_time because died requires alive-check at night start.
died(FG, T) :-
    fang_gu_jumped(FG, _, N),
    T = night(N, RoleOrd, 3),
    other_night_role_order(fang_gu, RoleOrd),
    alive(FG, night(N, 0, 0)).

% Role-change: outsider becomes fang_gu; must use role_assignment_changes_to (not assigned) for mid-game identity swap.
role_assignment_changes_to(night(N, RoleOrd, 3), Target, fang_gu) :-
    fang_gu_jumped(_, Target, N),
    other_night_role_order(fang_gu, RoleOrd).

% Jumped target registers evil at night times; three separate rules needed because time types (night/day/dawn) have different arities.
registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = night(N2, _, _), N2 >= N.

% Jumped target registers evil at day times; can't merge with night rule due to different time functor arity.
registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = day(N2, _), N2 >= N.

% Jumped target registers evil at dawn times; dawn/1 vs night/3 vs day/2 prevents unification in a single rule.
registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = dawn(N2), N2 >= N.

% Normal (non-jump) kill path; negation of outsider-target means this can't merge with the jump path above.
fang_gu_normal_kill(FG, Target, N) :-
    night_number(N), N > 1,
    player_chooses(fang_gu, FG, point(Target), night(N, RoleOrd, 2)),
    other_night_role_order(fang_gu, RoleOrd),
    functioning(FG, night(N, RoleOrd, 3)),
    not fang_gu_targets_outsider(FG, Target, N).

% Death token for normal kill; separate from jump-death reminder because target is the chosen player, not FG itself.
reminder_on(fang_gu_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    fang_gu_normal_kill(_, Target, N),
    other_night_role_order(fang_gu, RoleOrd).

demon_kill_token(fang_gu_dead).

% Blocks generic demon-kill for the old FG who "jumped"; it dies via explicit dead_at_time above, not the kill pipeline.
demon_kill_blocked(fang_gu_dead, P, T) :-
    fang_gu_jumped(P, _, _),
    reminder_on(fang_gu_dead, P, T).

% Script-level demon-kill for normal path; separate from jump snv_demon_killed because the victim is different (target vs old FG).
snv_demon_killed(Target, N) :-
    fang_gu_normal_kill(_, Target, N).
