% Zombuul
% Each night*, if no-one died today, choose a player: they die. The 1st time you die, you live but register as dead.

other_night_substep(zombuul, 1, st_asks(choose_target)).
other_night_substep(zombuul, 2, player_chooses(target)).
other_night_substep(zombuul, 3, st_places(zombuul_dead)).

% Helper: someone died during the day (from execution); separate from dawn deaths below
someone_died_today(N) :-
    day_number(N),
    died(_, day(N, _)).

% Helper: someone died at dawn (from overnight effects resolving); separate because dawn is a distinct time
someone_died_today(N) :-
    day_number(N),
    died(_, dawn(N)).

% Zombuul's kill is conditional: only fires if no one died the preceding day
zombuul_can_kill(N) :-
    night_number(N), N > 1,
    day_number(N-1),
    not someone_died_today(N-1).

% Zombuul picks a living target; uses zombuul_actually_alive (not alive) because Zombuul may appear dead
{ zombuul_target(Zombuul, P, N) : player(P), P != Zombuul, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(zombuul, RoleOrd),
    assigned(0, Zombuul, zombuul),
    zombuul_actually_alive(Zombuul, night(N, 0, 0)),
    zombuul_can_kill(N),
    not demon_blocked(Zombuul, N),
    game_active(night(N, 0, 0)).

% Bridge: generate the public player_chooses atom from the internal target selection
player_chooses(zombuul, Zombuul, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(zombuul, RoleOrd),
    zombuul_target(Zombuul, P, N).

% Place kill token on target if functioning (token placement, not kill resolution)
reminder_on(zombuul_dead, P, night(N, RoleOrd, 3)) :-
    zombuul_target(Zombuul, P, N),
    assigned(0, Zombuul, zombuul),
    night_number(N), N > 1,
    other_night_role_order(zombuul, RoleOrd),
    functioning(Zombuul, night(N, RoleOrd, 3)).

% Register kill token so botc.lp's generic death logic processes it
demon_kill_token(zombuul_dead).

% Remove stale kill tokens from previous night's targets (prevents inertia-carried kills)
token_removed(zombuul_dead, P, night(N, RoleOrd, 3)) :-
    reminder_on(zombuul_dead, P, night(N, 0, 0)),
    night_number(N), N > 1,
    other_night_role_order(zombuul, RoleOrd).

% === Zombuul's "fake death" ability ===

% Track whether the one-shot fake death has been used (prevents re-use)
zombuul_fake_death_used(Zombuul) :-
    assigned(0, Zombuul, zombuul),
    zombuul_faked_death(Zombuul, _).

% Zombuul "would die" from any demon kill (uses generic token detection, not per-demon predicates)
zombuul_would_die(Zombuul, T) :-
    assigned(0, Zombuul, zombuul),
    final_night_time(N, T),
    demon_kill_token(Token),
    reminder_on(Token, Zombuul, T),
    not protected_from_demon(Zombuul, T).

% Zombuul "would die" from execution (separate from kill above: different time structure)
zombuul_would_die(Zombuul, day(N, exec)) :-
    assigned(0, Zombuul, zombuul),
    executed(Zombuul, N).

% First death is faked (intercepts the would-die event when the ability hasn't been used yet)
zombuul_fakes_death(Zombuul, T) :-
    zombuul_would_die(Zombuul, T),
    not zombuul_fake_death_used(Zombuul).

% Record that the fake death happened (consumed by zombuul_fake_death_used to prevent re-use)
zombuul_faked_death(Zombuul, T) :- zombuul_fakes_death(Zombuul, T).

% Faking death places the "appears dead" token (the shroud without actual death)
reminder_on(zombuul_appears_dead, Zombuul, T) :- zombuul_fakes_death(Zombuul, T).

% Helper: Zombuul is truly alive when they have the appears-dead token (overrides normal alive check)
zombuul_actually_alive(Zombuul, T) :-
    assigned(0, Zombuul, zombuul),
    reminder_on(zombuul_appears_dead, Zombuul, T).

% Block the generic pipeline from producing died when Zombuul fakes death
demon_kill_blocked(Token, Zombuul, T) :-
    zombuul_fakes_death(Zombuul, T),
    demon_kill_token(Token),
    reminder_on(Token, Zombuul, T).

% Second death is real: the fake death has already been used, so this time the Zombuul actually dies
zombuul_truly_dies(Zombuul, T) :-
    zombuul_would_die(Zombuul, T),
    zombuul_fake_death_used(Zombuul).

% Generate the actual died/2 atom for the true death
died(Zombuul, T) :- zombuul_truly_dies(Zombuul, T).
