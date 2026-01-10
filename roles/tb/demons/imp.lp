% Imp

other_night_substep(imp, 1, st_asks(choose_target)).
other_night_substep(imp, 2, player_chooses(kill_target)).
other_night_substep(imp, 3, st_places(imp_dead)).

% Imp chooses a target each night (nights after the first)
% Check role at action time T, not night start (handles Snake Charmer swaps etc.)
{ player_chooses(imp, Imp, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    character_assignment_state_at_time(T, Imp, imp),
    alive(Imp, T),
    game_active(night(N, 0, 0)).

% House rule: Imp cannot target itself if all minions are dead
:- player_chooses(imp, Imp, point(Imp), night(N, RoleOrd, 2)),
   other_night_role_order(imp, RoleOrd),
   night_number(N), N > 1,
   not any_minion_alive(night(N, 0, 0)).

% Check if any player who is CURRENTLY a minion is alive
% (not just initially assigned as minion - role may have changed via SW, etc.)
any_minion_alive(T) :-
    character_assignment_state_at_time(T, P, R),
    minion(R),
    alive(P, T).

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

% Register imp_dead as a demon kill token (generic handling in botc.lp)
demon_kill_token(imp_dead).

% Local helper for starpass logic
imp_kill_succeeds(P, T) :-
    reminder_on(imp_dead, P, T),
    not protected_from_demon(P, T).

% Starpass

imp_starpass_triggered(Imp, N) :-
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    T = night(N, RoleOrd, 2),
    character_assignment_state_at_time(T, Imp, imp),
    player_chooses(imp, Imp, point(Imp), T),
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

% Role change happens at the same time as imp death (night N, substep 3)
% This ensures there's never a moment without a demon during starpass,
% which is important for the victory condition check.
role_assignment_changes_to(night(N, RoleOrd, 3), Minion, imp) :-
    starpass_target(Minion, N),
    other_night_role_order(imp, RoleOrd).
