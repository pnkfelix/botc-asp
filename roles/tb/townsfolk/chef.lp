% Chef

first_night_substep(chef, 1, st_tells(chef_info)).

evil_pair(P1, P2, T) :-
    neighbor(P1, P2),
    chair(P1, C1), chair(P2, C2), C1 < C2,
    registers_as(P1, evil, T),
    registers_as(P2, evil, T).

evil_pair_count(N, T) :-
    time(T),
    N = #count { P1, P2 : evil_pair(P1, P2, T) }.

st_tells_core(chef, Chef, count(N), T) :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(chef, RoleOrd),
    character_assignment_state_at_time(T, Chef, chef),
    functioning(Chef, T),
    evil_pair_count(N, T).

{ st_tells_core(chef, Chef, count(N), T) : N = 0..player_count/2 } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(chef, RoleOrd),
    character_assignment_state_at_time(T, Chef, chef),
    not functioning(Chef, T).
