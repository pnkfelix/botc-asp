% Seamstress
% Once per game, at night, choose 2 players (not yourself): you learn if they are the same alignment.

first_night_substep(seamstress, 1, st_asks(choose_two)).
first_night_substep(seamstress, 2, player_chooses(two_players)).
first_night_substep(seamstress, 3, st_tells(seamstress_info)).

other_night_substep(seamstress, 1, st_asks(choose_two)).
other_night_substep(seamstress, 2, player_chooses(two_players)).
other_night_substep(seamstress, 3, st_tells(seamstress_info)).

% Track when seamstress uses their ability
seamstress_used(Seamstress, N) :-
    seamstress_chooses(Seamstress, _, _, night(N, _, _)).

% Seamstress may choose two players on any night (once per game)
{ seamstress_chooses(Seamstress, P1, P2, night(N, RoleOrd, 2)) :
    player(P1), player(P2), P1 != P2, P1 != Seamstress, P2 != Seamstress,
    chair(P1, C1), chair(P2, C2), C1 < C2 } <= 1 :-
    first_night_role_order(seamstress, RoleOrd),
    N = 1,
    assigned(0, Seamstress, seamstress),
    alive(Seamstress, night(N, 0, 0)).

{ seamstress_chooses(Seamstress, P1, P2, night(N, RoleOrd, 2)) :
    player(P1), player(P2), P1 != P2, P1 != Seamstress, P2 != Seamstress,
    chair(P1, C1), chair(P2, C2), C1 < C2 } <= 1 :-
    other_night_role_order(seamstress, RoleOrd),
    night_number(N), N > 1,
    assigned(0, Seamstress, seamstress),
    alive(Seamstress, night(N, 0, 0)),
    not seamstress_used(Seamstress, PrevN) : night_number(PrevN), PrevN < N.

% Cannot use ability twice
:- seamstress_used(Seamstress, N1), seamstress_used(Seamstress, N2), N1 != N2.

% Convert to player_chooses for compatibility
player_chooses(seamstress, Seamstress, point(P1), T) :-
    seamstress_chooses(Seamstress, P1, _, T).
player_chooses(seamstress, Seamstress, point(P2), T) :-
    seamstress_chooses(Seamstress, _, P2, T).

% Check if two players are same alignment
same_alignment(P1, P2, T) :-
    player(P1), player(P2), time(T),
    registers_as(P1, good, T),
    registers_as(P2, good, T).
same_alignment(P1, P2, T) :-
    player(P1), player(P2), time(T),
    registers_as(P1, evil, T),
    registers_as(P2, evil, T).

% Seamstress learns result when functioning
st_tells_core(seamstress, Seamstress, yes, T) :-
    T = night(N, RoleOrd, 3),
    seamstress_chooses(Seamstress, P1, P2, night(N, RoleOrd, 2)),
    assigned(0, Seamstress, seamstress),
    functioning(Seamstress, T),
    same_alignment(P1, P2, T).

st_tells_core(seamstress, Seamstress, no, T) :-
    T = night(N, RoleOrd, 3),
    seamstress_chooses(Seamstress, P1, P2, night(N, RoleOrd, 2)),
    assigned(0, Seamstress, seamstress),
    functioning(Seamstress, T),
    not same_alignment(P1, P2, T).

% When not functioning, can get either result
{ st_tells_core(seamstress, Seamstress, yes, T);
  st_tells_core(seamstress, Seamstress, no, T) } = 1 :-
    T = night(N, RoleOrd, 3),
    seamstress_chooses(Seamstress, _, _, night(N, RoleOrd, 2)),
    assigned(0, Seamstress, seamstress),
    not functioning(Seamstress, T).
