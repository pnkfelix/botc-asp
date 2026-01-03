% Zombuul
% Each night*, if no-one died today, choose a player: they die. The 1st time you die, you live but register as dead.

other_night_substep(zombuul, 1, st_asks(choose_target)).
other_night_substep(zombuul, 2, player_chooses(target)).
other_night_substep(zombuul, 3, st_places(zombuul_dead)).

% Check if anyone died today (before this night)
someone_died_today(N) :-
    day_number(N),
    died(_, day(N, _)).

someone_died_today(N) :-
    day_number(N),
    died(_, dawn(N)).

% Zombuul only kills if no one died today
zombuul_can_kill(N) :-
    night_number(N), N > 1,
    day_number(N-1),
    not someone_died_today(N-1).

% Zombuul chooses a target if they can kill (and not blocked by Exorcist)
{ zombuul_target(Zombuul, P, N) : player(P), P != Zombuul, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(zombuul, RoleOrd),
    assigned(0, Zombuul, zombuul),
    zombuul_actually_alive(Zombuul, night(N, 0, 0)),
    zombuul_can_kill(N),
    not demon_blocked(Zombuul, N),
    game_active(night(N, 0, 0)).

% Connect to player_chooses
player_chooses(zombuul, Zombuul, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(zombuul, RoleOrd),
    zombuul_target(Zombuul, P, N).

% Zombuul kill succeeds if functioning
zombuul_kill_succeeds(P, T) :-
    zombuul_target(Zombuul, P, N),
    assigned(0, Zombuul, zombuul),
    night_number(N), N > 1,
    other_night_role_order(zombuul, RoleOrd),
    T = night(N, RoleOrd, 3),
    functioning(Zombuul, T),
    not protected_from_demon(P, T).

reminder_on(zombuul_dead, P, T) :- zombuul_kill_succeeds(P, T).

dead(P, T) :- zombuul_kill_succeeds(P, T).
died(P, T) :- zombuul_kill_succeeds(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)).

% === Zombuul's "fake death" ability ===
% The first time Zombuul would die, they don't actually die but register as dead

% Track if Zombuul has used their fake death
zombuul_fake_death_used(Zombuul) :-
    assigned(0, Zombuul, zombuul),
    zombuul_faked_death(Zombuul, _).

% When Zombuul would die for the first time
zombuul_would_die(Zombuul, T) :-
    assigned(0, Zombuul, zombuul),
    (imp_kill_succeeds(Zombuul, T) ; executed(Zombuul, N), T = day(N, exec)).

zombuul_fakes_death(Zombuul, T) :-
    zombuul_would_die(Zombuul, T),
    not zombuul_fake_death_used(Zombuul).

zombuul_faked_death(Zombuul, T) :- zombuul_fakes_death(Zombuul, T).

% Zombuul appears dead (has shroud) but is actually alive
reminder_on(zombuul_appears_dead, Zombuul, T) :- zombuul_fakes_death(Zombuul, T).

% Zombuul registers as dead but is actually alive
zombuul_actually_alive(Zombuul, T) :-
    assigned(0, Zombuul, zombuul),
    reminder_on(zombuul_appears_dead, Zombuul, T).

% When checking for game end, Zombuul counts as alive if they've faked death
% This modifies how alive_count works for evil wins

% Prevent actual death when faking
:- zombuul_fakes_death(Zombuul, T), died(Zombuul, T).

% Zombuul truly dies the second time
zombuul_truly_dies(Zombuul, T) :-
    zombuul_would_die(Zombuul, T),
    zombuul_fake_death_used(Zombuul).

died(Zombuul, T) :- zombuul_truly_dies(Zombuul, T).
