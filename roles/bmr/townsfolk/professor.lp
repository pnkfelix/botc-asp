% Professor
% Once per game, at night*, choose a dead player: if they are a Townsfolk, they are resurrected.

other_night_substep(professor, 1, st_asks(choose_dead_player)).
other_night_substep(professor, 2, player_chooses(target)).
other_night_substep(professor, 3, st_places(professor_alive)).

% Helper: records the night the Professor used their once-per-game ability (can't inline: used by _before check)
professor_used_ability(Professor, N) :-
    assigned(0, Professor, professor),
    night_number(N),
    professor_chose(Professor, _, N).

% Choice: Professor may pick one dead player (<=1 means can pass; once-per-game gated by _before check)
{ professor_chose(Professor, P, N) : player(P), dead_at_time(P, night(N, 0, 0)) } <= 1 :-
    night_number(N), N > 1,
    other_night_role_order(professor, RoleOrd),
    assigned(0, Professor, professor),
    alive(Professor, night(N, 0, 0)),
    not professor_used_ability_before(Professor, N).

% Helper: true if ability was used on any earlier night M < N (enforces once-per-game)
professor_used_ability_before(Professor, N) :-
    professor_used_ability(Professor, M),
    night_number(N),
    M < N.

% Bridge: emit player_chooses from professor_chose
player_chooses(professor, Professor, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(professor, RoleOrd),
    professor_chose(Professor, P, N).

% Resurrection trigger: chosen player must be a Townsfolk and Professor must be functioning
professor_resurrects(P, N) :-
    professor_chose(Professor, P, N),
    assigned(0, Professor, professor),
    night_number(N), N > 1,
    other_night_role_order(professor, RoleOrd),
    functioning(Professor, night(N, RoleOrd, 2)),
    assigned(0, P, R),
    townsfolk(R).

% Place alive token on resurrected player for grimoire display
reminder_on(professor_alive, P, night(N, RoleOrd, 3)) :-
    other_night_role_order(professor, RoleOrd),
    professor_resurrects(P, N).

% Bridge to generic resurrection system
resurrected(P, N) :- professor_resurrects(P, N).

% Resurrected player is alive for all subsequent nights (can't merge with day: different time structure)
alive(P, T) :-
    resurrected(P, N),
    time(T),
    T = night(M, R, S), M >= N.

% Resurrected player is alive for all subsequent days (can't merge with night: different unification)
alive(P, T) :-
    resurrected(P, N),
    time(T),
    T = day(M, Phase), M >= N.

% Resurrected player is alive for all subsequent dawns (separate from night/day: distinct time type)
alive(P, T) :-
    resurrected(P, N),
    time(T),
    T = dawn(M), M >= N.

% Constraint: dead_at_time must not persist after resurrection (prevents contradictory alive+dead state)
:- dead_at_time(P, T), resurrected(P, N), time(T), T = night(M, R, S), M > N.
