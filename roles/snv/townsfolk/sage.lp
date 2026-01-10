% Sage
% If the Demon kills you, you learn that it is 1 of 2 players.

other_night_substep(sage, 1, st_tells(sage_info)).

% Sage died tonight (killed by demon)
% Note: For S&V demons, we need to check for the appropriate death token
% This checks for any snv demon's kill token
sage_died_tonight(Sage, N) :-
    assigned(0, Sage, sage),
    night_number(N), N > 1,
    other_night_role_order(sage, RoleOrd),
    snv_demon_killed(Sage, night(N, RoleOrd, 1)).

% Generic predicate for S&V demon kills (will be defined by demon files)
% For now, also check imp_dead for compatibility with TB testing
sage_died_tonight(Sage, N) :-
    assigned(0, Sage, sage),
    night_number(N), N > 1,
    other_night_role_order(sage, RoleOrd),
    reminder_on(imp_dead, Sage, night(N, RoleOrd, 1)).

% Sage learns two players, one of which is the demon
{ sage_shown(Sage, P1, P2, night(N, RoleOrd, 1)) :
    player(P1), player(P2), P1 != P2,
    chair(P1, C1), chair(P2, C2), C1 < C2 } = 1 :-
    assigned(0, Sage, sage),
    other_night_role_order(sage, RoleOrd),
    night_number(N), N > 1,
    sage_died_tonight(Sage, N),
    functioning(Sage, night(N, RoleOrd, 1)).

% Constraint: one of the two shown must be the demon (when functioning)
% Check who is currently a demon at the time Sage receives info
:- sage_shown(Sage, P1, P2, T),
   functioning(Sage, T),
   character_assignment_state_at_time(T, Demon, R),
   demon(R),
   P1 != Demon,
   P2 != Demon.

% When not functioning, any two players
{ sage_shown(Sage, P1, P2, night(N, RoleOrd, 1)) :
    player(P1), player(P2), P1 != P2,
    chair(P1, C1), chair(P2, C2), C1 < C2 } = 1 :-
    assigned(0, Sage, sage),
    other_night_role_order(sage, RoleOrd),
    night_number(N), N > 1,
    sage_died_tonight(Sage, N),
    not functioning(Sage, night(N, RoleOrd, 1)).

% Convert to st_tells_core
st_tells_core(sage, Sage, info(P1, P2), T) :-
    sage_shown(Sage, P1, P2, T).
