% Barber
% If you died today or tonight, the Demon may choose 2 players (not another Demon) to swap characters.

other_night_substep(barber, 1, st_asks(demon_choose_swap)).
other_night_substep(barber, 2, player_chooses(swap_targets)).
other_night_substep(barber, 3, st_places(barber_swap)).

% Helper: Barber died during the night (separate from day death: different time structure)
barber_died_tonight(Barber, N) :-
    assigned(0, Barber, barber),
    died(Barber, T),
    T = night(N, _, _).

% Helper: Barber died during the day (separate from night death: different time structure)
barber_died_today(Barber, N) :-
    assigned(0, Barber, barber),
    died(Barber, day(N, _)).

% Helper: unifies night and day death sources (can't fold: different source predicates)
barber_died(Barber, N) :- barber_died_tonight(Barber, N).
barber_died(Barber, N) :- barber_died_today(Barber, N).

% Demon may optionally choose two non-demon players to swap characters (<=1 means can pass)
{ barber_swap(Demon, P1, P2, N) :
    player(P1), player(P2), P1 != P2,
    not demon(R1), not demon(R2),
    assigned(0, P1, R1), assigned(0, P2, R2),
    chair(P1, C1), chair(P2, C2), C1 < C2 } <= 1 :-
    barber_died(_, N),
    other_night_role_order(barber, RoleOrd),
    character_assignment_state_at_time(night(N, RoleOrd, 2), Demon, DemonRole),
    demon(DemonRole),
    alive(Demon, night(N, RoleOrd, 2)).

% P1 becomes P2's old role at dawn (can't merge with P2→R1 rule: different target player)
role_assignment_changes_to(dawn(N), P1, R2) :-
    barber_swap(_, P1, P2, N),
    assigned(0, P2, R2),
    day_number(N).

% P2 becomes P1's old role at dawn (separate from P1→R2: different player and role binding)
role_assignment_changes_to(dawn(N), P2, R1) :-
    barber_swap(_, P1, P2, N),
    assigned(0, P1, R1),
    day_number(N).

% Bridge swap target 1 to player_chooses (separate rules per target: different positional binding)
player_chooses(barber, Demon, point(P1), night(N, RoleOrd, 2)) :-
    barber_swap(Demon, P1, _, N),
    other_night_role_order(barber, RoleOrd).

% Bridge swap target 2 to player_chooses
player_chooses(barber, Demon, point(P2), night(N, RoleOrd, 2)) :-
    barber_swap(Demon, _, P2, N),
    other_night_role_order(barber, RoleOrd).
