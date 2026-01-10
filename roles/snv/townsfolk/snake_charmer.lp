% Snake Charmer
% Each night, choose an alive player: a chosen Demon swaps characters & alignments with you & is then poisoned.

first_night_substep(snake_charmer, 1, st_asks(choose_target)).
first_night_substep(snake_charmer, 2, player_chooses(target)).
first_night_substep(snake_charmer, 3, st_places(snake_charmer_swap)).

other_night_substep(snake_charmer, 1, st_asks(choose_target)).
other_night_substep(snake_charmer, 2, player_chooses(target)).
other_night_substep(snake_charmer, 3, st_places(snake_charmer_swap)).

% Snake Charmer chooses a player on first night
{ player_chooses(snake_charmer, SC, point(P), night(1, RoleOrd, 2)) : player(P), P != SC } = 1 :-
    first_night_role_order(snake_charmer, RoleOrd),
    assigned(0, SC, snake_charmer).

% Snake Charmer chooses a player on other nights (if alive)
{ player_chooses(snake_charmer, SC, point(P), night(N, RoleOrd, 2)) : player(P), P != SC } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(snake_charmer, RoleOrd),
    assigned(0, SC, snake_charmer),
    alive(SC, night(N, 0, 0)).

% If the chosen player is the demon and snake charmer is functioning, they swap
% Include DemonRole so we know what role the snake charmer becomes
snake_charmer_swaps(SC, Demon, DemonRole, N) :-
    player_chooses(snake_charmer, SC, point(Demon), night(N, RoleOrd, 2)),
    first_night_role_order(snake_charmer, RoleOrd),
    N = 1,
    character_assignment_state_at_time(night(N, 0, 0), Demon, DemonRole),
    demon(DemonRole),
    functioning(SC, night(N, RoleOrd, 3)).

snake_charmer_swaps(SC, Demon, DemonRole, N) :-
    player_chooses(snake_charmer, SC, point(Demon), night(N, RoleOrd, 2)),
    other_night_role_order(snake_charmer, RoleOrd),
    night_number(N), N > 1,
    character_assignment_state_at_time(night(N, 0, 0), Demon, DemonRole),
    demon(DemonRole),
    functioning(SC, night(N, RoleOrd, 3)).

% After swap: Snake Charmer becomes the demon, former demon becomes snake charmer
% Use role_assignment_changes_to so character_assignment_state_at_time is updated
role_assignment_changes_to(night(N, RoleOrd, 3), SC, DemonRole) :-
    snake_charmer_swaps(SC, Demon, DemonRole, N),
    first_night_role_order(snake_charmer, RoleOrd),
    N = 1.

role_assignment_changes_to(night(N, RoleOrd, 3), SC, DemonRole) :-
    snake_charmer_swaps(SC, Demon, DemonRole, N),
    other_night_role_order(snake_charmer, RoleOrd),
    N > 1.

role_assignment_changes_to(night(N, RoleOrd, 3), Demon, snake_charmer) :-
    snake_charmer_swaps(SC, Demon, _, N),
    first_night_role_order(snake_charmer, RoleOrd),
    N = 1.

role_assignment_changes_to(night(N, RoleOrd, 3), Demon, snake_charmer) :-
    snake_charmer_swaps(SC, Demon, _, N),
    other_night_role_order(snake_charmer, RoleOrd),
    N > 1.

% Former demon is poisoned after becoming snake charmer
reminder_on(sc_poisoned, Demon, T) :-
    snake_charmer_swaps(_, Demon, _, N),
    time(T), T = night(N2, _, _), N2 >= N.

reminder_on(sc_poisoned, Demon, T) :-
    snake_charmer_swaps(_, Demon, _, N),
    time(T), T = day(N2, _), N2 >= N.

reminder_on(sc_poisoned, Demon, T) :-
    snake_charmer_swaps(_, Demon, _, N),
    time(T), T = dawn(N2), N2 >= N.

causes_impairment(sc_poisoned).

% The former snake charmer is now evil
registers_as(SC, evil, T) :-
    snake_charmer_swaps(SC, _, _, N),
    time(T), T = night(N2, _, _), N2 >= N.

registers_as(SC, evil, T) :-
    snake_charmer_swaps(SC, _, _, N),
    time(T), T = day(N2, _), N2 >= N.

registers_as(SC, evil, T) :-
    snake_charmer_swaps(SC, _, _, N),
    time(T), T = dawn(N2), N2 >= N.
