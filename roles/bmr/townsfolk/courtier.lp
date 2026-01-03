% Courtier
% Once per game, at night, choose a character: they are drunk for 3 nights & 3 days.

first_night_substep(courtier, 1, st_asks(choose_character)).
first_night_substep(courtier, 2, player_chooses(character)).
first_night_substep(courtier, 3, st_places(courtier_drunk)).

other_night_substep(courtier, 1, st_asks(choose_character)).
other_night_substep(courtier, 2, player_chooses(character)).
other_night_substep(courtier, 3, st_places(courtier_drunk)).

% Track if Courtier has used their ability
courtier_used_ability(Courtier, N) :-
    assigned(0, Courtier, courtier),
    night_number(N),
    courtier_chose(Courtier, _, N).

% Courtier may choose on first night (once per game)
{ courtier_chose(Courtier, R, 1) : role(R) } <= 1 :-
    first_night_role_order(courtier, RoleOrd),
    assigned(0, Courtier, courtier).

% Courtier may choose on other nights (if they haven't used ability yet and are alive)
{ courtier_chose(Courtier, R, N) : role(R) } <= 1 :-
    night_number(N), N > 1,
    other_night_role_order(courtier, RoleOrd),
    assigned(0, Courtier, courtier),
    alive(Courtier, night(N, 0, 0)),
    not courtier_used_ability_before(Courtier, N).

courtier_used_ability_before(Courtier, N) :-
    courtier_used_ability(Courtier, M),
    night_number(N),
    M < N.

% Connect to player_chooses
player_chooses(courtier, Courtier, point(R), night(1, RoleOrd, 2)) :-
    first_night_role_order(courtier, RoleOrd),
    courtier_chose(Courtier, R, 1).

player_chooses(courtier, Courtier, point(R), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(courtier, RoleOrd),
    courtier_chose(Courtier, R, N).

% The player with that character becomes drunk for 3 days/nights
% Track when the drunk effect started
courtier_drunk_start(P, N) :-
    courtier_chose(Courtier, R, N),
    assigned(0, Courtier, courtier),
    functioning(Courtier, night(N, RoleOrd, 2)),
    first_night_role_order(courtier, RoleOrd),
    assigned(0, P, R),
    N = 1.

courtier_drunk_start(P, N) :-
    courtier_chose(Courtier, R, N),
    assigned(0, Courtier, courtier),
    night_number(N), N > 1,
    other_night_role_order(courtier, RoleOrd),
    functioning(Courtier, night(N, RoleOrd, 2)),
    assigned(0, P, R).

% Place drunk token (lasts 3 days and 3 nights from when placed)
reminder_on(courtier_drunk, P, T) :-
    courtier_drunk_start(P, N),
    time(T),
    courtier_drunk_active(N, T).

% Drunk is active for 3 nights and 3 days after the choice
% Night N (when chosen), Day N, Night N+1, Day N+1, Night N+2, Day N+2
courtier_drunk_active(N, T) :- night_number(N), time(T), T = night(M, _, _), M >= N, M <= N+2.
courtier_drunk_active(N, T) :- night_number(N), time(T), T = day(M, _), M >= N, M <= N+2.
courtier_drunk_active(N, dawn(M)) :- night_number(N), day_number(M), M >= N, M <= N+2.

causes_impairment(courtier_drunk).
