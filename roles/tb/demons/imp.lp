% Imp

other_night_substep(imp, 1, st_asks(choose_target)).
other_night_substep(imp, 2, player_chooses(kill_target)).
other_night_substep(imp, 3, st_places(imp_dead)).

{ player_chooses(imp, Imp, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    current_demon(Imp, N),
    alive(Imp, night(N, 0, 0)).

% House rule: Imp cannot target itself if all minions are dead
:- player_chooses(imp, Imp, point(Imp), night(N, RoleOrd, 2)),
   other_night_role_order(imp, RoleOrd),
   night_number(N), N > 1,
   not any_minion_alive(night(N, 0, 0)).

any_minion_alive(T) :-
    assigned(0, P, R),
    minion(R),
    alive(P, T),
    time(T).

current_demon(P, N) :-
    night_number(N),
    assigned(0, P, R),
    demon(R),
    not replaced_as_demon(P, N).

replaced_as_demon(P, N) :-
    assigned(0, P, R),
    demon(R),
    sw_becomes_demon(_, TriggerDay),
    night_number(N),
    N > TriggerDay.

current_demon(SW, N) :-
    sw_becomes_demon(SW, TriggerDay),
    night_number(N),
    N > TriggerDay.

reminder_on(imp_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    player_chooses(imp, Imp, point(Target), night(N, RoleOrd, 2)),
    functioning(Imp, night(N, RoleOrd, 3)).

% When Imp moves the DIE token to a new target, remove it from any previous target.
% The Imp has only ONE DIE token that moves from victim to victim.
token_removed(imp_dead, OldTarget, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    player_chooses(imp, Imp, point(NewTarget), night(N, RoleOrd, 2)),
    functioning(Imp, night(N, RoleOrd, 3)),
    reminder_on(imp_dead, OldTarget, night(N, 0, 0)),  % Token was on OldTarget at start of night
    OldTarget != NewTarget.  % Only remove if moving to different target

% Constraint: imp_dead token cannot be on a player at the same time it's being removed
:- reminder_on(imp_dead, P, T), token_removed(imp_dead, P, T).

imp_kill_succeeds(P, T) :-
    reminder_on(imp_dead, P, T),
    not protected_from_demon(P, T).

dead(P, T) :- imp_kill_succeeds(P, T).

died(P, T) :-
    final_night_time(N, T),
    imp_kill_succeeds(P, T),
    alive(P, night(N, 0, 0)).

% Starpass

imp_starpass_triggered(Imp, N) :-
    night_number(N), N > 1,
    current_demon(Imp, N),
    other_night_role_order(imp, RoleOrd),
    player_chooses(imp, Imp, point(Imp), night(N, RoleOrd, 2)),
    imp_kill_succeeds(Imp, night(N, RoleOrd, 3)).

eligible_for_starpass(Minion, N) :-
    imp_starpass_triggered(_, N),
    assigned(0, Minion, R),
    minion(R),
    other_night_role_order(imp, RoleOrd),
    alive(Minion, night(N, RoleOrd, 3)).

{ starpass_target(Minion, N) : eligible_for_starpass(Minion, N) } = 1 :-
    imp_starpass_triggered(_, N),
    eligible_for_starpass(_, N).

assigned(N, Minion, imp) :- starpass_target(Minion, N).

replaced_as_demon(P, NextN) :-
    imp_starpass_triggered(P, TriggerNight),
    night_number(NextN),
    NextN > TriggerNight.

current_demon(Minion, NextN) :-
    starpass_target(Minion, TriggerNight),
    night_number(NextN),
    NextN > TriggerNight.
