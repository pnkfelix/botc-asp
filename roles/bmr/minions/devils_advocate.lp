% Devil's Advocate
% Each night, choose a living player (different to last night): if executed tomorrow, they don't die.

first_night_substep(devils_advocate, 1, st_asks(choose_target)).
first_night_substep(devils_advocate, 2, player_chooses(target)).
first_night_substep(devils_advocate, 3, st_places(da_survives)).

other_night_substep(devils_advocate, 1, st_asks(choose_target)).
other_night_substep(devils_advocate, 2, player_chooses(target)).
other_night_substep(devils_advocate, 3, st_places(da_survives)).

% Night 1 choice: DA picks a player to protect from execution (can't merge with other nights: different role order)
{ da_choice(DA, P, 1) : player(P), P != DA } = 1 :-
    first_night_role_order(devils_advocate, RoleOrd),
    assigned(0, DA, devils_advocate).

% Other nights choice: adds alive filter (separate from night 1: uses other_night_role_order + alive check)
{ da_choice(DA, P, N) : player(P), P != DA, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(devils_advocate, RoleOrd),
    assigned(0, DA, devils_advocate),
    alive(DA, night(N, 0, 0)).

% Constraint: cannot choose the same player as last night (enforces "different to last night")
:- da_choice(DA, P, N), da_choice(DA, P, N-1), night_number(N), N > 1.

% Night 1 bridge: emit player_chooses (can't merge with other nights: different role order lookup)
player_chooses(devils_advocate, DA, point(P), night(1, RoleOrd, 2)) :-
    first_night_role_order(devils_advocate, RoleOrd),
    da_choice(DA, P, 1).

% Other nights bridge: emit player_chooses (separate from night 1: uses other_night_role_order)
player_chooses(devils_advocate, DA, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(devils_advocate, RoleOrd),
    da_choice(DA, P, N).

% Night 1 token placement: protect chosen player if DA is functioning (can't merge: different role order)
reminder_on(da_survives, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(devils_advocate, RoleOrd),
    da_choice(DA, P, 1),
    assigned(0, DA, devils_advocate),
    functioning(DA, night(1, RoleOrd, 2)).

% Other nights token placement: separate from night 1 (uses other_night_role_order)
reminder_on(da_survives, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(devils_advocate, RoleOrd),
    da_choice(DA, P, N),
    assigned(0, DA, devils_advocate),
    functioning(DA, night(N, RoleOrd, 2)).

% Remove token at start of next night (protection only lasts one day)
token_removed(da_survives, P, night(N+1, 0, 0)) :-
    reminder_on(da_survives, P, day(N, exec)),
    night_number(N+1).

% Helper: true when a player with the da_survives token is executed (trigger for death prevention)
da_protects_from_execution(P, N) :-
    reminder_on(da_survives, P, day(N, exec)),
    executed(P, N).

% Constraint: protected player cannot die from execution (execution happens but they survive)
:- da_protects_from_execution(P, N), died(P, day(N, exec)).
