% Sage
% If the Demon kills you, you learn that it is 1 of 2 players.

other_night_substep(sage, 1, st_tells(sage_info)).

% Helper: detect S&V demon kill (can't merge with TB variant: different kill predicate, snv_demon_killed vs imp_dead)
sage_died_tonight(Sage, N) :-
    assigned(0, Sage, sage),
    night_number(N), N > 1,
    snv_demon_killed(Sage, N).

% Helper: detect TB demon kill for cross-script compatibility (separate from S&V: checks imp_dead token instead)
sage_died_tonight(Sage, N) :-
    assigned(0, Sage, sage),
    night_number(N), N > 1,
    other_night_role_order(sage, RoleOrd),
    reminder_on(imp_dead, Sage, night(N, RoleOrd, 1)).

% Functioning tell: choose two players to show (can't merge with not-functioning: demon constraint only applies here)
{ sage_shown(Sage, P1, P2, night(N, RoleOrd, 1)) :
    player(P1), player(P2), P1 != P2,
    chair(P1, C1), chair(P2, C2), C1 < C2 } = 1 :-
    assigned(0, Sage, sage),
    other_night_role_order(sage, RoleOrd),
    night_number(N), N > 1,
    sage_died_tonight(Sage, N),
    functioning(Sage, night(N, RoleOrd, 1)).

% Integrity constraint: one shown player must be demon (can't fold into choice rule: constraint on derived facts, not choice generation)
:- sage_shown(Sage, P1, P2, T),
   functioning(Sage, T),
   character_assignment_state_at_time(T, Demon, R),
   demon(R),
   P1 != Demon,
   P2 != Demon.

% Not-functioning tell: any two players (can't merge with functioning: no demon constraint, negated functioning condition)
{ sage_shown(Sage, P1, P2, night(N, RoleOrd, 1)) :
    player(P1), player(P2), P1 != P2,
    chair(P1, C1), chair(P2, C2), C1 < C2 } = 1 :-
    assigned(0, Sage, sage),
    other_night_role_order(sage, RoleOrd),
    night_number(N), N > 1,
    sage_died_tonight(Sage, N),
    not functioning(Sage, night(N, RoleOrd, 1)).

% Bridge to st_tells_core interface (deterministic projection; separate from choice rules: different predicate)
st_tells_core(sage, Sage, info(P1, P2), T) :-
    sage_shown(Sage, P1, P2, T).
