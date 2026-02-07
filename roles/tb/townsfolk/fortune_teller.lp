% Fortune Teller

first_night_substep(fortune_teller, 1, st_asks(choose_two)).
first_night_substep(fortune_teller, 2, player_chooses(two_players)).
first_night_substep(fortune_teller, 3, st_tells(ft_info)).

other_night_substep(fortune_teller, 1, st_asks(choose_two)).
other_night_substep(fortune_teller, 2, player_chooses(two_players)).
other_night_substep(fortune_teller, 3, st_tells(ft_info)).

{ reminder_on(ft_red_herring, P, night(1, 0, 0)) : player(P) } = 1 :-
    assigned(0, _, fortune_teller).

:- reminder_on(ft_red_herring, P, _),
   assigned(0, P, R),
   demon(R).

:- reminder_on(ft_red_herring, P, T),
   not registers_as(P, good, T).

{ ft_choice(FT, P, T) : player(P) } = 2 :-
    T = night(N, RoleOrd, 2),
    N = 1,
    first_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller).

{ ft_choice(FT, P, T) : player(P) } = 2 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller).

ft_choice(FT, P, night(N, RoleOrd, 3)) :-
    ft_choice(FT, P, night(N, RoleOrd, 2)).

:- player_chooses(fortune_teller, FT, point(P), T),
   not ft_choice(FT, P, T).

player_chooses(fortune_teller, FT, point(P), T) :-
    ft_choice(FT, P, T).

ft_pings_as_demon(P, T) :-
    registers_as(P, demon, T).

ft_pings_as_demon(P, T) :-
    reminder_on(ft_red_herring, P, T).

ft_got_ping(FT, T) :-
    ft_choice(FT, P, T),
    ft_pings_as_demon(P, T).

st_tells_core(fortune_teller, FT, yes, T) :-
    T = night(N, RoleOrd, 3),
    N = 1,
    first_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    functioning(FT, T),
    ft_got_ping(FT, T).

st_tells_core(fortune_teller, FT, yes, T) :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    functioning(FT, T),
    ft_got_ping(FT, T).

st_tells_core(fortune_teller, FT, no, T) :-
    T = night(N, RoleOrd, 3),
    N = 1,
    first_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    functioning(FT, T),
    not ft_got_ping(FT, T).

st_tells_core(fortune_teller, FT, no, T) :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    functioning(FT, T),
    not ft_got_ping(FT, T).

{ st_tells_core(fortune_teller, FT, yes, T) ;
  st_tells_core(fortune_teller, FT, no, T) } = 1 :-
    T = night(N, RoleOrd, 3),
    N = 1,
    first_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    not functioning(FT, T).

{ st_tells_core(fortune_teller, FT, yes, T) ;
  st_tells_core(fortune_teller, FT, no, T) } = 1 :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    not functioning(FT, T).
