% Devil's Advocate
% Each night, choose a living player (different to last night): if executed tomorrow, they don't die.

first_night_substep(devils_advocate, 1, st_asks(choose_target)).
first_night_substep(devils_advocate, 2, player_chooses(target)).
first_night_substep(devils_advocate, 3, st_places(da_survives)).

other_night_substep(devils_advocate, 1, st_asks(choose_target)).
other_night_substep(devils_advocate, 2, player_chooses(target)).
other_night_substep(devils_advocate, 3, st_places(da_survives)).

% First night: Devil's Advocate chooses a player
{ da_choice(DA, P, 1) : player(P), P != DA } = 1 :-
    first_night_role_order(devils_advocate, RoleOrd),
    assigned(0, DA, devils_advocate).

% Other nights: Choose a player (different from last night)
{ da_choice(DA, P, N) : player(P), P != DA, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(devils_advocate, RoleOrd),
    assigned(0, DA, devils_advocate),
    alive(DA, night(N, 0, 0)).

% Cannot choose same player as last night
:- da_choice(DA, P, N), da_choice(DA, P, N-1), night_number(N), N > 1.

% Connect to player_chooses
player_chooses(devils_advocate, DA, point(P), night(1, RoleOrd, 2)) :-
    first_night_role_order(devils_advocate, RoleOrd),
    da_choice(DA, P, 1).

player_chooses(devils_advocate, DA, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(devils_advocate, RoleOrd),
    da_choice(DA, P, N).

% Place protection token (if functioning)
reminder_on(da_survives, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(devils_advocate, RoleOrd),
    da_choice(DA, P, 1),
    assigned(0, DA, devils_advocate),
    functioning(DA, night(1, RoleOrd, 2)).

reminder_on(da_survives, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(devils_advocate, RoleOrd),
    da_choice(DA, P, N),
    assigned(0, DA, devils_advocate),
    functioning(DA, night(N, RoleOrd, 2)).

% Token removed at end of day (after execution phase)
token_removed(da_survives, P, night(N+1, 0, 0)) :-
    reminder_on(da_survives, P, day(N, exec)),
    night_number(N+1).

% Protected from execution death
da_protects_from_execution(P, N) :-
    reminder_on(da_survives, P, day(N, exec)),
    executed(P, N).

% Override execution death for protected player
:- da_protects_from_execution(P, N), died(P, day(N, exec)).
