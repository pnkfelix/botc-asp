% Fang Gu
% Each night*, choose a player: they die. The 1st Outsider this kills becomes an evil Fang Gu & you die instead. [+1 Outsider]

causes_outsider_mod(fang_gu, 1).

other_night_substep(fang_gu, 1, st_asks(choose_target)).
other_night_substep(fang_gu, 2, player_chooses(kill_target)).
other_night_substep(fang_gu, 3, st_places(fang_gu_dead)).

% Fang Gu chooses a target each night (nights after the first)
% Check role at action time T, not night start (handles Snake Charmer swaps etc.)
{ player_chooses(fang_gu, FG, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(fang_gu, RoleOrd),
    character_assignment_state_at_time(T, FG, fang_gu),
    alive(FG, T),
    game_active(night(N, 0, 0)).

% Track if this is the first outsider kill
fang_gu_has_jumped :- fang_gu_jumped(_, _, _).

% Check if target is an outsider (and this is first outsider kill)
fang_gu_targets_outsider(FG, Target, N) :-
    player_chooses(fang_gu, FG, point(Target), night(N, RoleOrd, 2)),
    other_night_role_order(fang_gu, RoleOrd),
    assigned(0, Target, R),
    outsider(R),
    not fang_gu_has_jumped,
    functioning(FG, night(N, RoleOrd, 3)).

% The "jump" happens: outsider becomes Fang Gu, old Fang Gu dies
fang_gu_jumped(FG, Target, N) :-
    fang_gu_targets_outsider(FG, Target, N).

% Old Fang Gu dies when jumping
reminder_on(fang_gu_dead, FG, night(N, RoleOrd, 3)) :-
    fang_gu_jumped(FG, _, N),
    other_night_role_order(fang_gu, RoleOrd).

% snv_demon_killed(Target, N) - Target was killed by demon on night N
snv_demon_killed(FG, N) :-
    fang_gu_jumped(FG, _, N).

dead_at_time(FG, T) :-
    fang_gu_jumped(FG, _, N),
    T = night(N, RoleOrd, 3),
    other_night_role_order(fang_gu, RoleOrd).

died(FG, T) :-
    fang_gu_jumped(FG, _, N),
    T = night(N, RoleOrd, 3),
    other_night_role_order(fang_gu, RoleOrd),
    alive(FG, night(N, 0, 0)).

% The outsider becomes an evil Fang Gu
% Use role_assignment_changes_to so character_assignment_state_at_time is updated
role_assignment_changes_to(night(N, RoleOrd, 3), Target, fang_gu) :-
    fang_gu_jumped(_, Target, N),
    other_night_role_order(fang_gu, RoleOrd).

registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = night(N2, _, _), N2 >= N.

registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = day(N2, _), N2 >= N.

registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = dawn(N2), N2 >= N.

% Normal kill (non-outsider or after first jump)
fang_gu_normal_kill(FG, Target, N) :-
    night_number(N), N > 1,
    player_chooses(fang_gu, FG, point(Target), night(N, RoleOrd, 2)),
    other_night_role_order(fang_gu, RoleOrd),
    functioning(FG, night(N, RoleOrd, 3)),
    not fang_gu_targets_outsider(FG, Target, N).

reminder_on(fang_gu_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    fang_gu_normal_kill(_, Target, N),
    other_night_role_order(fang_gu, RoleOrd).

% Register as demon kill token (generic handling in botc.lp)
demon_kill_token(fang_gu_dead).

% Block the generic kill for targets that "jumped" (became the new Fang Gu)
demon_kill_blocked(fang_gu_dead, P, T) :-
    fang_gu_jumped(P, _, _),
    reminder_on(fang_gu_dead, P, T).

snv_demon_killed(Target, N) :-
    fang_gu_normal_kill(_, Target, N).
