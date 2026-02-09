% Snake Charmer
% Each night, choose an alive player: a chosen Demon swaps characters & alignments with you & is then poisoned.

first_night_substep(snake_charmer, 1, st_asks(choose_target)).
first_night_substep(snake_charmer, 2, player_chooses(target)).
first_night_substep(snake_charmer, 3, st_places(snake_charmer_swap)).

other_night_substep(snake_charmer, 1, st_asks(choose_target)).
other_night_substep(snake_charmer, 2, player_chooses(target)).
other_night_substep(snake_charmer, 3, st_places(snake_charmer_swap)).

% Night 1 choice: pick a player (can't merge with other nights: different role order, no alive check needed)
{ player_chooses(snake_charmer, SC, point(P), night(1, RoleOrd, 2)) : player(P), P != SC } = 1 :-
    first_night_role_order(snake_charmer, RoleOrd),
    assigned(0, SC, snake_charmer).

% Other nights choice: pick a player if alive (separate from night 1: uses other_night_role_order, adds alive guard)
{ player_chooses(snake_charmer, SC, point(P), night(N, RoleOrd, 2)) : player(P), P != SC } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(snake_charmer, RoleOrd),
    assigned(0, SC, snake_charmer),
    alive(SC, night(N, 0, 0)).

% Night 1 swap detection: chosen player is demon and SC is functioning (can't merge with other nights: different role order)
snake_charmer_swaps(SC, Demon, DemonRole, N) :-
    player_chooses(snake_charmer, SC, point(Demon), night(N, RoleOrd, 2)),
    first_night_role_order(snake_charmer, RoleOrd),
    N = 1,
    character_assignment_state_at_time(night(N, 0, 0), Demon, DemonRole),
    demon(DemonRole),
    functioning(SC, night(N, RoleOrd, 3)).

% Other nights swap detection (separate from night 1: uses other_night_role_order)
snake_charmer_swaps(SC, Demon, DemonRole, N) :-
    player_chooses(snake_charmer, SC, point(Demon), night(N, RoleOrd, 2)),
    other_night_role_order(snake_charmer, RoleOrd),
    night_number(N), N > 1,
    character_assignment_state_at_time(night(N, 0, 0), Demon, DemonRole),
    demon(DemonRole),
    functioning(SC, night(N, RoleOrd, 3)).

% Night 1: SC becomes demon role (can't merge with other nights: different role order; can't merge with demon->SC: different player and role)
role_assignment_changes_to(night(N, RoleOrd, 3), SC, DemonRole) :-
    snake_charmer_swaps(SC, Demon, DemonRole, N),
    first_night_role_order(snake_charmer, RoleOrd),
    N = 1.

% Other nights: SC becomes demon role (separate from night 1: uses other_night_role_order)
role_assignment_changes_to(night(N, RoleOrd, 3), SC, DemonRole) :-
    snake_charmer_swaps(SC, Demon, DemonRole, N),
    other_night_role_order(snake_charmer, RoleOrd),
    N > 1.

% Night 1: demon becomes snake_charmer (can't merge with SC->demon: different target player and role value)
role_assignment_changes_to(night(N, RoleOrd, 3), Demon, snake_charmer) :-
    snake_charmer_swaps(SC, Demon, _, N),
    first_night_role_order(snake_charmer, RoleOrd),
    N = 1.

% Other nights: demon becomes snake_charmer (separate from night 1: uses other_night_role_order)
role_assignment_changes_to(night(N, RoleOrd, 3), Demon, snake_charmer) :-
    snake_charmer_swaps(SC, Demon, _, N),
    other_night_role_order(snake_charmer, RoleOrd),
    N > 1.

% Night 1: poison the former demon (can't merge with other nights: different role order)
reminder_on(sc_poisoned, Demon, night(N, RoleOrd, 3)) :-
    snake_charmer_swaps(_, Demon, _, N),
    first_night_role_order(snake_charmer, RoleOrd),
    N = 1.

% Other nights: poison the former demon (separate from night 1: uses other_night_role_order)
reminder_on(sc_poisoned, Demon, night(N, RoleOrd, 3)) :-
    snake_charmer_swaps(_, Demon, _, N),
    other_night_role_order(snake_charmer, RoleOrd),
    N > 1.

% Declare sc_poisoned as an impairment source (fact, not a rule; separate from reminder_on derivations)
causes_impairment(sc_poisoned).

% Former SC registers evil on night time points (can't merge with day/dawn: different time type in unification)
registers_as(SC, evil, T) :-
    snake_charmer_swaps(SC, _, _, N),
    time(T), T = night(N2, _, _), N2 >= N.

% Former SC registers evil on day time points (can't merge with night/dawn: day/2 vs night/3 vs dawn/1 arity)
registers_as(SC, evil, T) :-
    snake_charmer_swaps(SC, _, _, N),
    time(T), T = day(N2, _), N2 >= N.

% Former SC registers evil on dawn time points (can't merge with night/day: dawn/1 has different arity)
registers_as(SC, evil, T) :-
    snake_charmer_swaps(SC, _, _, N),
    time(T), T = dawn(N2), N2 >= N.
