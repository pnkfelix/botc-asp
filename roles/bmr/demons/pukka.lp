% Pukka
% Each night, choose a player: they are poisoned. The previously poisoned player dies then becomes healthy.

first_night_substep(pukka, 1, st_asks(choose_target)).
first_night_substep(pukka, 2, player_chooses(target)).
first_night_substep(pukka, 3, st_places(pukka_poisoned)).

other_night_substep(pukka, 1, st_asks(choose_target)).
other_night_substep(pukka, 2, player_chooses(target)).
other_night_substep(pukka, 3, st_places(pukka_poisoned)).
other_night_substep(pukka, 4, st_places(pukka_dead)).

% First night: Pukka poisons someone (they die the following night)
{ pukka_target(Pukka, P, 1) : player(P), P != Pukka } = 1 :-
    first_night_role_order(pukka, RoleOrd),
    assigned(0, Pukka, pukka).

% Other nights: Pukka poisons someone new
{ pukka_target(Pukka, P, N) : player(P), P != Pukka, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd),
    assigned(0, Pukka, pukka),
    alive(Pukka, night(N, 0, 0)),
    not demon_blocked(Pukka, N),
    game_active(night(N, 0, 0)).

% Connect to player_chooses
player_chooses(pukka, Pukka, point(P), night(1, RoleOrd, 2)) :-
    first_night_role_order(pukka, RoleOrd),
    pukka_target(Pukka, P, 1).

player_chooses(pukka, Pukka, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd),
    pukka_target(Pukka, P, N).

% Place poisoned token on new target
reminder_on(pukka_poisoned, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(pukka, RoleOrd),
    pukka_target(Pukka, P, 1),
    assigned(0, Pukka, pukka),
    functioning(Pukka, night(1, RoleOrd, 2)).

reminder_on(pukka_poisoned, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd),
    pukka_target(Pukka, P, N),
    assigned(0, Pukka, pukka),
    functioning(Pukka, night(N, RoleOrd, 2)).

% Pukka poison causes impairment
causes_impairment(pukka_poisoned).

% Previously poisoned player dies when new target is poisoned
% (The death happens at the same time as the new poison)
pukka_kill_succeeds(PrevTarget, T) :-
    reminder_on(pukka_poisoned, PrevTarget, night(N, 0, 0)),
    pukka_target(Pukka, NewTarget, N),
    NewTarget != PrevTarget,
    assigned(0, Pukka, pukka),
    night_number(N), N > 1,
    other_night_role_order(pukka, RoleOrd),
    T = night(N, RoleOrd, 4),
    functioning(Pukka, T),
    not protected_from_demon(PrevTarget, T).

reminder_on(pukka_dead, P, T) :- pukka_kill_succeeds(P, T).

dead_at_time(P, T) :- pukka_kill_succeeds(P, T).
died(P, T) :- pukka_kill_succeeds(P, KillTime), KillTime = night(N, _, _), final_night_time(N, T), alive(P, night(N, 0, 0)).

% Remove old poison when person dies or new target is chosen
token_removed(pukka_poisoned, P, night(N, RoleOrd, 4)) :-
    reminder_on(pukka_poisoned, P, night(N, 0, 0)),
    pukka_target(_, NewTarget, N),
    other_night_role_order(pukka, RoleOrd),
    night_number(N), N > 1.
