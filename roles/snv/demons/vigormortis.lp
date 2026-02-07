% Vigormortis
% Each night*, choose a player: they die. Minions you kill keep their ability & poison 1 Townsfolk neighbor. [-1 Outsider]

causes_outsider_mod(vigormortis, -1).

other_night_substep(vigormortis, 1, st_asks(choose_target)).
other_night_substep(vigormortis, 2, player_chooses(kill_target)).
other_night_substep(vigormortis, 3, st_places(vigor_dead)).

% Choice rule: Vigor picks exactly one target each other-night (choice cannot be deterministic).
{ player_chooses(vigormortis, Vigor, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(vigormortis, RoleOrd),
    character_assignment_state_at_time(T, Vigor, vigormortis),
    alive(Vigor, T),
    game_active(night(N, 0, 0)).

% Places death token; deterministic derivation from choice, separate because it checks functioning at substep 3.
reminder_on(vigor_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(vigormortis, RoleOrd),
    player_chooses(vigormortis, Vigor, point(Target), night(N, RoleOrd, 2)),
    functioning(Vigor, night(N, RoleOrd, 3)).

demon_kill_token(vigor_dead).

% Projects reminder into script-level snv_demon_killed/2; uses night-number so other roles can query without knowing substep.
snv_demon_killed(Target, N) :-
    night_number(N), N > 1,
    other_night_role_order(vigormortis, RoleOrd),
    reminder_on(vigor_dead, Target, night(N, RoleOrd, 3)).

% Pre-filters successful kills (not protected); needed as intermediate because minion check and poison choice depend on it.
vigor_kill_succeeds(P, T) :-
    reminder_on(vigor_dead, P, T),
    not protected_from_demon(P, T).

% Identifies minion kills specifically; separate from vigor_kill_succeeds because the minion check filters a subset for ability retention.
vigor_killed_minion(Minion, N) :-
    vigor_kill_succeeds(Minion, night(N, _, _)),
    assigned(0, Minion, R),
    minion(R).

% Marks minion as retaining ability post-death; separate predicate so other rules can check ability-retention without re-deriving.
vigor_minion_keeps_ability(Minion) :-
    vigor_killed_minion(Minion, _).

% Choice rule: ST picks exactly one townsfolk neighbor to poison; must be a choice rule because multiple neighbors may qualify.
{ vigor_poison_target(Minion, Neighbor, N) :
    living_neighbor(Minion, Neighbor, night(N, RoleOrd, 3)),
    assigned(0, Neighbor, R), townsfolk(R) } = 1 :-
    vigor_killed_minion(Minion, N),
    other_night_role_order(vigormortis, RoleOrd).

% Places poison token at kill-time; separate from choice rule because this is a deterministic consequence, and inertia propagates it forward.
reminder_on(vigor_poisoned, P, night(N, RoleOrd, 3)) :-
    vigor_poison_target(_, P, N),
    other_night_role_order(vigormortis, RoleOrd).

causes_impairment(vigor_poisoned).
