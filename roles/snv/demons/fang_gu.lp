% Fang Gu
% Each night*, choose a player: they die. The 1st Outsider this kills becomes an evil Fang Gu & you die instead. [+1 Outsider]

causes_outsider_mod(fang_gu, 1).

other_night_substep(fang_gu, 1, st_asks(choose_target)).
other_night_substep(fang_gu, 2, player_chooses(kill_target)).
other_night_substep(fang_gu, 3, st_places(fang_gu_dead)).

% Fang Gu chooses a target each night (nights after the first)
{ player_chooses(fang_gu, FG, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(fang_gu, RoleOrd),
    current_demon(FG, N),
    assigned(0, FG, fang_gu),
    alive(FG, night(N, 0, 0)),
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

snv_demon_killed(FG, T) :-
    fang_gu_jumped(FG, _, N),
    T = night(N, RoleOrd, 3),
    other_night_role_order(fang_gu, RoleOrd).

dead(FG, T) :-
    fang_gu_jumped(FG, _, N),
    T = night(N, RoleOrd, 3),
    other_night_role_order(fang_gu, RoleOrd).

died(FG, T) :-
    fang_gu_jumped(FG, _, N),
    T = night(N, RoleOrd, 3),
    other_night_role_order(fang_gu, RoleOrd),
    alive(FG, night(N, 0, 0)).

% The outsider becomes an evil Fang Gu
assigned(N, Target, fang_gu) :-
    fang_gu_jumped(_, Target, N).

registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = night(N2, _, _), N2 >= N.

registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = day(N2, _), N2 >= N.

registers_as(Target, evil, T) :-
    fang_gu_jumped(_, Target, N),
    time(T), T = dawn(N2), N2 >= N.

% New Fang Gu becomes current demon
current_demon(Target, N2) :-
    fang_gu_jumped(_, Target, N),
    night_number(N2), N2 > N.

replaced_as_demon(FG, N+1) :-
    fang_gu_jumped(FG, _, N).

% Normal kill (non-outsider or after first jump)
fang_gu_normal_kill(FG, Target, N) :-
    player_chooses(fang_gu, FG, point(Target), night(N, RoleOrd, 2)),
    other_night_role_order(fang_gu, RoleOrd),
    functioning(FG, night(N, RoleOrd, 3)),
    not fang_gu_targets_outsider(FG, Target, N).

reminder_on(fang_gu_dead, Target, night(N, RoleOrd, 3)) :-
    fang_gu_normal_kill(_, Target, N),
    other_night_role_order(fang_gu, RoleOrd).

snv_demon_killed(Target, T) :-
    fang_gu_normal_kill(_, Target, N),
    T = night(N, RoleOrd, 3),
    other_night_role_order(fang_gu, RoleOrd).

fang_gu_kill_succeeds(P, T) :-
    reminder_on(fang_gu_dead, P, T),
    not protected_from_demon(P, T),
    not fang_gu_jumped(P, _, _).

dead(P, T) :- fang_gu_kill_succeeds(P, T).

died(P, T) :-
    final_night_time(N, T),
    fang_gu_kill_succeeds(P, T),
    alive(P, night(N, 0, 0)).
