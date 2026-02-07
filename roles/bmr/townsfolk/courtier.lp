% Courtier
% Once per game, at night, choose a character: they are drunk for 3 nights & 3 days.

first_night_substep(courtier, 1, st_asks(choose_character)).
first_night_substep(courtier, 2, player_chooses(character)).
first_night_substep(courtier, 3, st_places(courtier_drunk)).

other_night_substep(courtier, 1, st_asks(choose_character)).
other_night_substep(courtier, 2, player_chooses(character)).
other_night_substep(courtier, 3, st_places(courtier_drunk)).

% Helper: records the night the Courtier used their once-per-game ability (can't inline: used by _before check)
courtier_used_ability(Courtier, N) :-
    assigned(0, Courtier, courtier),
    night_number(N),
    courtier_chose(Courtier, _, N).

% Night 1 choice: Courtier may pick a character to drunk (can't merge with other nights: different role order)
{ courtier_chose(Courtier, R, 1) : role(R) } <= 1 :-
    first_night_role_order(courtier, RoleOrd),
    assigned(0, Courtier, courtier).

% Other nights choice: gated on alive + not-yet-used (separate from night 1: different role order + alive check)
{ courtier_chose(Courtier, R, N) : role(R) } <= 1 :-
    night_number(N), N > 1,
    other_night_role_order(courtier, RoleOrd),
    assigned(0, Courtier, courtier),
    alive(Courtier, night(N, 0, 0)),
    not courtier_used_ability_before(Courtier, N).

% Helper: true if ability was used on any earlier night M < N (enforces once-per-game)
courtier_used_ability_before(Courtier, N) :-
    courtier_used_ability(Courtier, M),
    night_number(N),
    M < N.

% Night 1 bridge: emit player_chooses from choice (can't merge with other nights: different role order lookup)
player_chooses(courtier, Courtier, point(R), night(1, RoleOrd, 2)) :-
    first_night_role_order(courtier, RoleOrd),
    courtier_chose(Courtier, R, 1).

% Other nights bridge: emit player_chooses (separate from night 1: uses other_night_role_order)
player_chooses(courtier, Courtier, point(R), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(courtier, RoleOrd),
    courtier_chose(Courtier, R, N).

% Night 1 drunk start: if functioning, the player with chosen character becomes drunk (can't merge: different role order)
courtier_drunk_start(P, N) :-
    courtier_chose(Courtier, R, N),
    assigned(0, Courtier, courtier),
    functioning(Courtier, night(N, RoleOrd, 2)),
    first_night_role_order(courtier, RoleOrd),
    assigned(0, P, R),
    N = 1.

% Other nights drunk start: separate from night 1 (uses other_night_role_order)
courtier_drunk_start(P, N) :-
    courtier_chose(Courtier, R, N),
    assigned(0, Courtier, courtier),
    night_number(N), N > 1,
    other_night_role_order(courtier, RoleOrd),
    functioning(Courtier, night(N, RoleOrd, 2)),
    assigned(0, P, R).

% Drunk token placement: active for 3 day/night cycles from when the choice was made
reminder_on(courtier_drunk, P, T) :-
    courtier_drunk_start(P, N),
    time(T),
    courtier_drunk_active(N, T).

% Active during nights N through N+2 (can't merge with day rule: different time structure)
courtier_drunk_active(N, T) :- night_number(N), time(T), T = night(M, _, _), M >= N, M <= N+2.
% Active during days N through N+2 (can't merge with night rule: different unification)
courtier_drunk_active(N, T) :- night_number(N), time(T), T = day(M, _), M >= N, M <= N+2.
% Active during dawns N through N+2 (separate from night/day: dawn is a distinct time type)
courtier_drunk_active(N, dawn(M)) :- night_number(N), day_number(M), M >= N, M <= N+2.

causes_impairment(courtier_drunk).
