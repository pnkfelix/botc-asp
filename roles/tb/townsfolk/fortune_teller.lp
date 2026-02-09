% Fortune Teller

first_night_substep(fortune_teller, 1, st_asks(choose_two)).
first_night_substep(fortune_teller, 2, player_chooses(two_players)).
first_night_substep(fortune_teller, 3, st_tells(ft_info)).

other_night_substep(fortune_teller, 1, st_asks(choose_two)).
other_night_substep(fortune_teller, 2, player_chooses(two_players)).
other_night_substep(fortune_teller, 3, st_tells(ft_info)).

% At setup, ST places the red herring token on exactly one player
{ reminder_on(ft_red_herring, P, night(1, 0, 0)) : player(P) } = 1 :-
    assigned(0, _, fortune_teller).

% Red herring cannot be placed on the demon (it would make the FT trivially unreliable)
:- reminder_on(ft_red_herring, P, _),
   assigned(0, P, R),
   demon(R).

% Red herring must be on a good-registering player (separate from demon ban: covers edge cases like Spy)
:- reminder_on(ft_red_herring, P, T),
   not registers_as(P, good, T).

% Night 1: FT chooses exactly two players (can't merge with other-nights: uses first_night_role_order)
{ ft_choice(FT, P, T) : player(P) } = 2 :-
    T = night(N, RoleOrd, 2),
    N = 1,
    first_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller).

% Other nights: FT chooses exactly two players (can't merge with night 1: uses other_night_role_order)
{ ft_choice(FT, P, T) : player(P) } = 2 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller).

% Carry the FT's choice forward from substep 2 (choose) to substep 3 (tell) within the same night
ft_choice(FT, P, night(N, RoleOrd, 3)) :-
    ft_choice(FT, P, night(N, RoleOrd, 2)).

% Constraint: player_chooses for FT must correspond to an ft_choice (no spurious choices)
:- player_chooses(fortune_teller, FT, point(P), T),
   not ft_choice(FT, P, T).

% Bridge: ft_choice generates the public player_chooses atom for grimoire display
player_chooses(fortune_teller, FT, point(P), T) :-
    ft_choice(FT, P, T).

% A player pings if they actually register as a demon
ft_pings_as_demon(P, T) :-
    registers_as(P, demon, T).

% A player also pings if they carry the red herring token (separate: this is an independent trigger)
ft_pings_as_demon(P, T) :-
    reminder_on(ft_red_herring, P, T).

% Helper: the FT "got a ping" if at least one of their two chosen players pings as demon
ft_got_ping(FT, T) :-
    ft_choice(FT, P, T),
    ft_pings_as_demon(P, T).

% Night 1 + functioning + ping: FT learns "yes" (can't merge with other-nights: different role order)
st_tells_core(fortune_teller, FT, yes, T) :-
    T = night(N, RoleOrd, 3),
    N = 1,
    first_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    functioning(FT, T),
    ft_got_ping(FT, T).

% Other nights + functioning + ping: FT learns "yes"
st_tells_core(fortune_teller, FT, yes, T) :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    functioning(FT, T),
    ft_got_ping(FT, T).

% Night 1 + functioning + no ping: FT learns "no" (can't merge with "yes": opposite ping condition)
st_tells_core(fortune_teller, FT, no, T) :-
    T = night(N, RoleOrd, 3),
    N = 1,
    first_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    functioning(FT, T),
    not ft_got_ping(FT, T).

% Other nights + functioning + no ping: FT learns "no"
st_tells_core(fortune_teller, FT, no, T) :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    functioning(FT, T),
    not ft_got_ping(FT, T).

% Night 1 + not functioning: ST may answer yes or no arbitrarily (choice rule, not deterministic)
{ st_tells_core(fortune_teller, FT, yes, T) ;
  st_tells_core(fortune_teller, FT, no, T) } = 1 :-
    T = night(N, RoleOrd, 3),
    N = 1,
    first_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    not functioning(FT, T).

% Other nights + not functioning: ST may answer yes or no arbitrarily
{ st_tells_core(fortune_teller, FT, yes, T) ;
  st_tells_core(fortune_teller, FT, no, T) } = 1 :-
    T = night(N, RoleOrd, 3),
    night_number(N), N > 1,
    other_night_role_order(fortune_teller, RoleOrd),
    character_assignment_state_at_time(T, FT, fortune_teller),
    not functioning(FT, T).
