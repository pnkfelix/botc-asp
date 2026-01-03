% Vigormortis
% Each night*, choose a player: they die. Minions you kill keep their ability & poison 1 Townsfolk neighbor. [-1 Outsider]

causes_outsider_mod(vigormortis, -1).

other_night_substep(vigormortis, 1, st_asks(choose_target)).
other_night_substep(vigormortis, 2, player_chooses(kill_target)).
other_night_substep(vigormortis, 3, st_places(vigor_dead)).

% Vigormortis chooses a target each night (nights after the first)
{ player_chooses(vigormortis, Vigor, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(vigormortis, RoleOrd),
    current_demon(Vigor, N),
    assigned(0, Vigor, vigormortis),
    alive(Vigor, night(N, 0, 0)),
    game_active(night(N, 0, 0)).

% Place the death token on the target
reminder_on(vigor_dead, Target, night(N, RoleOrd, 3)) :-
    player_chooses(vigormortis, Vigor, point(Target), night(N, RoleOrd, 2)),
    other_night_role_order(vigormortis, RoleOrd),
    functioning(Vigor, night(N, RoleOrd, 3)).

snv_demon_killed(Target, T) :-
    reminder_on(vigor_dead, Target, T).

vigor_kill_succeeds(P, T) :-
    reminder_on(vigor_dead, P, T),
    not protected_from_demon(P, T).

dead(P, T) :- vigor_kill_succeeds(P, T).

died(P, T) :-
    final_night_time(N, T),
    vigor_kill_succeeds(P, T),
    alive(P, night(N, 0, 0)).

% Track minions killed by Vigormortis
vigor_killed_minion(Minion, N) :-
    vigor_kill_succeeds(Minion, night(N, _, _)),
    assigned(0, Minion, R),
    minion(R).

% Minions killed by Vigor keep their ability (do not lose it on death)
% This is implicit - they can still use their ability while dead
vigor_minion_keeps_ability(Minion) :-
    vigor_killed_minion(Minion, _).

% One Townsfolk neighbor of the dead minion is poisoned
% The ST chooses which neighbor
{ vigor_poison_target(Minion, Neighbor, N) :
    living_neighbor(Minion, Neighbor, night(N, RoleOrd, 3)),
    assigned(0, Neighbor, R), townsfolk(R) } = 1 :-
    vigor_killed_minion(Minion, N),
    other_night_role_order(vigormortis, RoleOrd).

% The chosen townsfolk is poisoned permanently (as long as the minion stays dead)
reminder_on(vigor_poisoned, P, T) :-
    vigor_poison_target(_, P, N),
    time(T), T = night(N2, _, _), N2 >= N.

reminder_on(vigor_poisoned, P, T) :-
    vigor_poison_target(_, P, N),
    time(T), T = day(N2, _), N2 >= N.

reminder_on(vigor_poisoned, P, T) :-
    vigor_poison_target(_, P, N),
    time(T), T = dawn(N2), N2 >= N.

causes_impairment(vigor_poisoned).
