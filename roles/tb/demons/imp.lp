% Imp

other_night_substep(imp, 1, st_asks(choose_target)).
other_night_substep(imp, 2, player_chooses(kill_target)).
other_night_substep(imp, 3, st_places(imp_dead)).

% Imp picks any player (including self for starpass); uses character_assignment_state_at_time for mid-night swaps
{ player_chooses(imp, Imp, point(P), T) : player(P) } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    character_assignment_state_at_time(T, Imp, imp),
    alive(Imp, T),
    game_active(night(N, 0, 0)).

% Self-target (starpass) is only legal when at least one minion is alive to receive the demon role
:- player_chooses(imp, Imp, point(Imp), night(N, RoleOrd, 2)),
   other_night_role_order(imp, RoleOrd),
   night_number(N), N > 1,
   not any_minion_alive(night(N, 0, 0)).

% Helper: true if any current minion is alive; uses current role (not initial) to handle SW promotion etc.
any_minion_alive(T) :-
    character_assignment_state_at_time(T, P, R),
    minion(R),
    alive(P, T).

% Functioning Imp's choice places the kill token on the target (no token if poisoned/drunk)
reminder_on(imp_dead, Target, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    player_chooses(imp, Imp, point(Target), night(N, RoleOrd, 2)),
    functioning(Imp, night(N, RoleOrd, 3)).

% The Imp has one kill token; placing it on a new target removes it from the previous one
token_removed(imp_dead, OldTarget, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    player_chooses(imp, Imp, point(NewTarget), night(N, RoleOrd, 2)),
    functioning(Imp, night(N, RoleOrd, 3)),
    reminder_on(imp_dead, OldTarget, night(N, 0, 0)),
    OldTarget != NewTarget.

% Token removal takes effect: can't simultaneously exist and be removed at the same time
:- reminder_on(imp_dead, P, T), token_removed(imp_dead, P, T).

% Register imp_dead as a demon kill token so botc.lp's generic death logic processes it
demon_kill_token(imp_dead).

% Helper: the kill succeeds unless the target has demon protection (Monk, etc.)
imp_kill_succeeds(P, T) :-
    reminder_on(imp_dead, P, T),
    not protected_from_demon(P, T).

% --- Starpass ---

% Starpass triggers when the Imp targets itself and the kill is not blocked by protection
imp_starpass_triggered(Imp, N) :-
    night_number(N), N > 1,
    other_night_role_order(imp, RoleOrd),
    T = night(N, RoleOrd, 2),
    character_assignment_state_at_time(T, Imp, imp),
    player_chooses(imp, Imp, point(Imp), T),
    imp_kill_succeeds(Imp, night(N, RoleOrd, 3)).

% Eligible recipients: initial minions who are still alive (uses assigned(0,...) per game rules)
eligible_for_starpass(Minion, N) :-
    imp_starpass_triggered(_, N),
    assigned(0, Minion, R),
    minion(R),
    other_night_role_order(imp, RoleOrd),
    alive(Minion, night(N, RoleOrd, 3)).

% Exactly one eligible minion is chosen to become the new Imp (ST's choice if multiple candidates)
{ starpass_target(Minion, N) : eligible_for_starpass(Minion, N) } = 1 :-
    imp_starpass_triggered(_, N),
    eligible_for_starpass(_, N).

% The role change fires at the same substep as the Imp's death, ensuring no demon-less moment
role_assignment_changes_to(night(N, RoleOrd, 3), Minion, imp) :-
    starpass_target(Minion, N),
    other_night_role_order(imp, RoleOrd).
