% Pukka
% Each night, choose a player: they are poisoned. The previously poisoned player dies then becomes healthy.

first_night_substep(pukka, 1, st_asks(choose_target)).
first_night_substep(pukka, 2, player_chooses(target)).
first_night_substep(pukka, 3, st_places(pukka_poisoned)).

other_night_substep(pukka, 1, st_asks(choose_target)).
other_night_substep(pukka, 2, player_chooses(target)).
other_night_substep(pukka, 3, st_places(pukka_poisoned)).
other_night_substep(pukka, 4, st_places(pukka_dead)).

% Night 1: Pukka poisons someone (no alive check needed; can't merge with other-nights: different role order)
{ pukka_target(Pukka, P, 1) : player(P), P != Pukka } = 1 :-
    first_night_role_order(pukka, RoleOrd),
    assigned(0, Pukka, pukka).

% Other nights: Pukka poisons a new living target (also gated on alive, demon_blocked, game_active)
{ pukka_target(Pukka, P, N) : player(P), P != Pukka, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd),
    assigned(0, Pukka, pukka),
    alive(Pukka, night(N, 0, 0)),
    not demon_blocked(Pukka, N),
    game_active(night(N, 0, 0)).

% Bridge night 1: generate player_chooses from pukka_target (separate: first_night_role_order)
player_chooses(pukka, Pukka, point(P), night(1, RoleOrd, 2)) :-
    first_night_role_order(pukka, RoleOrd),
    pukka_target(Pukka, P, 1).

% Bridge other nights: generate player_chooses from pukka_target (separate: other_night_role_order)
player_chooses(pukka, Pukka, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd),
    pukka_target(Pukka, P, N).

% Night 1: place poison on the chosen target if functioning (separate: first_night_role_order)
reminder_on(pukka_poisoned, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(pukka, RoleOrd),
    pukka_target(Pukka, P, 1),
    assigned(0, Pukka, pukka),
    functioning(Pukka, night(1, RoleOrd, 2)).

% Other nights: place poison on the chosen target if functioning
reminder_on(pukka_poisoned, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd),
    pukka_target(Pukka, P, N),
    assigned(0, Pukka, pukka),
    functioning(Pukka, night(N, RoleOrd, 2)).

% Pukka's poison impairs the victim (consumed by the generic functioning/impaired system)
causes_impairment(pukka_poisoned).

% Place kill token on previously poisoned player when Pukka picks a new target (delayed kill mechanic)
reminder_on(pukka_dead, PrevTarget, night(N, RoleOrd, 4)) :-
    reminder_on(pukka_poisoned, PrevTarget, night(N, 0, 0)),
    pukka_target(Pukka, NewTarget, N),
    NewTarget != PrevTarget,
    assigned(0, Pukka, pukka),
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd),
    functioning(Pukka, night(N, RoleOrd, 4)).

% Register kill token so botc.lp's generic death logic processes it
demon_kill_token(pukka_dead).

% Remove stale kill tokens from previous night's targets (prevents inertia-carried kills)
token_removed(pukka_dead, P, night(N, RoleOrd, 4)) :-
    reminder_on(pukka_dead, P, night(N, 0, 0)),
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd).

% Remove old poison when new target is chosen (the old victim either died or was freed)
token_removed(pukka_poisoned, P, night(N, RoleOrd, 4)) :-
    reminder_on(pukka_poisoned, P, night(N, 0, 0)),
    pukka_target(_, NewTarget, N),
    other_night_role_order(pukka, RoleOrd),
    night_number(N), N > 1.
