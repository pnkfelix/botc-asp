% Goon
% Each night, the 1st player to choose you with their ability is drunk until dusk. You become their alignment.

first_night_substep(goon, 1, st_checks(goon_target)).

% Helper: records each player_chooses event that targets the Goon (used to find the first one)
goon_targeted_by(Goon, Chooser, R, T) :-
    assigned(0, Goon, goon),
    player_chooses(R, Chooser, point(Goon), T),
    Chooser != Goon.

% Helper: the first player to target the Goon on night N (earliest role order; triggers drunk + alignment)
goon_first_target(Goon, Chooser, N) :-
    goon_targeted_by(Goon, Chooser, R, night(N, RoleOrd, _)),
    not earlier_goon_target(Goon, N, RoleOrd).

% Helper: true if there exists an earlier targeting on the same night (used to identify "first")
earlier_goon_target(Goon, N, RoleOrd) :-
    goon_targeted_by(Goon, _, _, night(N, RoleOrd, _)),
    goon_targeted_by(Goon, _, _, night(N, EarlierOrd, _)),
    EarlierOrd < RoleOrd.

% Place drunk token on the first targeter if Goon is functioning (drunk until dusk)
reminder_on(goon_drunk, Chooser, T) :-
    goon_first_target(Goon, Chooser, N),
    assigned(0, Goon, goon),
    functioning(Goon, night(N, 0, 0)),
    goon_drunk_active(N, T).

% Active during the night when targeting occurs (can't merge with dawn: different time type)
goon_drunk_active(N, T) :-
    time(T), T = night(N, R, S).
% Active at dawn of the same night (separate from night: dawn is a distinct time type)
goon_drunk_active(N, dawn(N)) :- night_number(N).

% Remove drunk token at start of day (drunk expires at dusk)
token_removed(goon_drunk, P, day(N, 0)) :-
    goon_first_target(_, P, N),
    day_number(N).

causes_impairment(goon_drunk).

% Goon becomes evil if the first targeter is evil (separate from good: different alignment literal)
goon_becomes_alignment(Goon, evil, N) :-
    goon_first_target(Goon, Chooser, N),
    assigned(0, Goon, goon),
    functioning(Goon, night(N, 0, 0)),
    actual_category(Chooser, evil).

% Goon becomes good if the first targeter is good (separate from evil: different alignment literal)
goon_becomes_alignment(Goon, good, N) :-
    goon_first_target(Goon, Chooser, N),
    assigned(0, Goon, goon),
    functioning(Goon, night(N, 0, 0)),
    actual_category(Chooser, good).

% Goon registers as evil for all subsequent nights after turning evil (can't merge with day: different time type)
registers_as(Goon, evil, T) :-
    goon_becomes_alignment(Goon, evil, N),
    time(T), T = night(M, R, S), M >= N.

% Goon registers as evil for all subsequent days (separate from night: different unification)
registers_as(Goon, evil, T) :-
    goon_becomes_alignment(Goon, evil, N),
    time(T), T = day(M, Phase), M >= N.
