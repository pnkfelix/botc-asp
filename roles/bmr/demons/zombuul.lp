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

% Kill resolves at substep 3: functioning + no protection = success
zombuul_kill_succeeds(P, T) :-
    zombuul_target(Zombuul, P, N),
    assigned(0, Zombuul, zombuul),
    night_number(N), N > 1,
    other_night_role_order(zombuul, RoleOrd),
    T = night(N, RoleOrd, 3),
    functioning(Zombuul, T),
    not protected_from_demon(P, T).

% Place the kill token on the victim for grimoire display
reminder_on(zombuul_dead, P, T) :- zombuul_kill_succeeds(P, T).

% Mark victim as dead at the instant of the kill (for mid-night state tracking)
dead_at_time(P, T) :- zombuul_kill_succeeds(P, T).
% Record the death at the final night time for dawn processing (separate from dead_at_time for timing)
died(P, T) :- zombuul_kill_succeeds(P, KillTime), KillTime = night(N, _, _), final_night_time(N, T), alive(P, night(N, 0, 0)).

% === Zombuul's "fake death" ability ===

% Track whether the one-shot fake death has been used (prevents re-use)
zombuul_fake_death_used(Zombuul) :-
    assigned(0, Zombuul, zombuul),
    zombuul_faked_death(Zombuul, _).

% Zombuul "would die" from a demon kill (separate from execution below: different death source)
zombuul_would_die(Zombuul, T) :-
    assigned(0, Zombuul, zombuul),
    imp_kill_succeeds(Zombuul, T).

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

% Prevent the died/2 atom from being generated when faking (the fake intercepts the death)
:- zombuul_fakes_death(Zombuul, T), died(Zombuul, T).

% Second death is real: the fake death has already been used, so this time the Zombuul actually dies
zombuul_truly_dies(Zombuul, T) :-
    zombuul_would_die(Zombuul, T),
    zombuul_fake_death_used(Zombuul).

% Generate the actual died/2 atom for the true death
died(Zombuul, T) :- zombuul_truly_dies(Zombuul, T).
