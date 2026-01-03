% Goon
% Each night, the 1st player to choose you with their ability is drunk until dusk. You become their alignment.

first_night_substep(goon, 1, st_checks(goon_target)).

% Track who targeted the Goon each night
% This requires knowing the order of night actions
% The first player to choose the Goon with their ability triggers the effect

% When a player chooses the Goon with their ability
goon_targeted_by(Goon, Chooser, R, T) :-
    assigned(0, Goon, goon),
    player_chooses(R, Chooser, point(Goon), T),
    Chooser != Goon.

% First targeting of Goon on a given night (by role order)
goon_first_target(Goon, Chooser, N) :-
    goon_targeted_by(Goon, Chooser, R, night(N, RoleOrd, _)),
    not earlier_goon_target(Goon, N, RoleOrd).

earlier_goon_target(Goon, N, RoleOrd) :-
    goon_targeted_by(Goon, _, _, night(N, RoleOrd, _)),
    goon_targeted_by(Goon, _, _, night(N, EarlierOrd, _)),
    EarlierOrd < RoleOrd.

% First targeter becomes drunk until dusk
reminder_on(goon_drunk, Chooser, T) :-
    goon_first_target(Goon, Chooser, N),
    assigned(0, Goon, goon),
    functioning(Goon, night(N, 0, 0)),
    goon_drunk_active(N, T).

goon_drunk_active(N, T) :-
    time(T), T = night(N, R, S).
goon_drunk_active(N, dawn(N)) :- night_number(N).

% Token removed at dusk (start of day)
token_removed(goon_drunk, P, day(N, 0)) :-
    goon_first_target(_, P, N),
    day_number(N).

causes_impairment(goon_drunk).

% Goon becomes the alignment of the first targeter
% Track Goon's alignment change
goon_becomes_alignment(Goon, evil, N) :-
    goon_first_target(Goon, Chooser, N),
    assigned(0, Goon, goon),
    functioning(Goon, night(N, 0, 0)),
    actual_category(Chooser, evil).

goon_becomes_alignment(Goon, good, N) :-
    goon_first_target(Goon, Chooser, N),
    assigned(0, Goon, goon),
    functioning(Goon, night(N, 0, 0)),
    actual_category(Chooser, good).

% Override registration for Goon after alignment change
registers_as(Goon, evil, T) :-
    goon_becomes_alignment(Goon, evil, N),
    time(T), T = night(M, R, S), M >= N.

registers_as(Goon, evil, T) :-
    goon_becomes_alignment(Goon, evil, N),
    time(T), T = day(M, Phase), M >= N.
