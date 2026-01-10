% Fortune Teller

first_night_substep(fortune_teller, 1, st_asks(choose_two)).
first_night_substep(fortune_teller, 2, player_chooses(two_players)).
first_night_substep(fortune_teller, 3, st_tells(ft_info)).

other_night_substep(fortune_teller, 1, st_asks(choose_two)).
other_night_substep(fortune_teller, 2, player_chooses(two_players)).
other_night_substep(fortune_teller, 3, st_tells(ft_info)).

% Place red herring token for anyone who received the fortune_teller token
% (includes both actual fortune tellers and drunks/marionettes who think they are)
{ reminder_on(ft_red_herring, P, night(1, 0, 0)) : player(P) } = 1 :-
    received(_, fortune_teller).

% Red herring constraints only apply to actual fortune tellers
:- reminder_on(ft_red_herring, P, _),
   assigned(0, _, fortune_teller),
   assigned(0, P, R),
   demon(R).

:- reminder_on(ft_red_herring, P, T),
   assigned(0, _, fortune_teller),
   not registers_as(P, good, T).

% Anyone who received fortune_teller token makes a choice (includes drunk/marionette)
{ ft_choice(FT, P, night(N, RoleOrd, 2)) : player(P) } = 2 :-
    received(FT, fortune_teller),
    first_night_role_order(fortune_teller, RoleOrd),
    N = 1.

{ ft_choice(FT, P, night(N, RoleOrd, 2)) : player(P) } = 2 :-
    received(FT, fortune_teller),
    other_night_role_order(fortune_teller, RoleOrd),
    night_number(N), N > 1,
    alive(FT, night(N, 0, 0)).

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

st_tells_core(fortune_teller, FT, yes, night(N, RoleOrd, 3)) :-
    first_night_role_order(fortune_teller, RoleOrd), N = 1,
    assigned(0, FT, fortune_teller),
    functioning(FT, night(N, RoleOrd, 3)),
    ft_got_ping(FT, night(N, RoleOrd, 3)).

st_tells_core(fortune_teller, FT, yes, night(N, RoleOrd, 3)) :-
    other_night_role_order(fortune_teller, RoleOrd), night_number(N), N > 1,
    assigned(0, FT, fortune_teller),
    functioning(FT, night(N, RoleOrd, 3)),
    ft_got_ping(FT, night(N, RoleOrd, 3)).

st_tells_core(fortune_teller, FT, no, night(N, RoleOrd, 3)) :-
    first_night_role_order(fortune_teller, RoleOrd), N = 1,
    assigned(0, FT, fortune_teller),
    functioning(FT, night(N, RoleOrd, 3)),
    not ft_got_ping(FT, night(N, RoleOrd, 3)).

st_tells_core(fortune_teller, FT, no, night(N, RoleOrd, 3)) :-
    other_night_role_order(fortune_teller, RoleOrd), night_number(N), N > 1,
    assigned(0, FT, fortune_teller),
    functioning(FT, night(N, RoleOrd, 3)),
    not ft_got_ping(FT, night(N, RoleOrd, 3)).

{ st_tells_core(fortune_teller, FT, yes, night(N, RoleOrd, 3)) ;
  st_tells_core(fortune_teller, FT, no, night(N, RoleOrd, 3)) } = 1 :-
    first_night_role_order(fortune_teller, RoleOrd), N = 1,
    assigned(0, FT, fortune_teller),
    not functioning(FT, night(N, RoleOrd, 3)).

{ st_tells_core(fortune_teller, FT, yes, night(N, RoleOrd, 3)) ;
  st_tells_core(fortune_teller, FT, no, night(N, RoleOrd, 3)) } = 1 :-
    other_night_role_order(fortune_teller, RoleOrd), night_number(N), N > 1,
    assigned(0, FT, fortune_teller),
    not functioning(FT, night(N, RoleOrd, 3)).

% Drunk/Marionette who think they're Fortune Teller get arbitrary info
{ st_tells_core(fortune_teller, FT, yes, night(N, RoleOrd, 3)) ;
  st_tells_core(fortune_teller, FT, no, night(N, RoleOrd, 3)) } = 1 :-
    first_night_role_order(fortune_teller, RoleOrd), N = 1,
    received(FT, fortune_teller),
    not assigned(0, FT, fortune_teller).

{ st_tells_core(fortune_teller, FT, yes, night(N, RoleOrd, 3)) ;
  st_tells_core(fortune_teller, FT, no, night(N, RoleOrd, 3)) } = 1 :-
    other_night_role_order(fortune_teller, RoleOrd), night_number(N), N > 1,
    received(FT, fortune_teller),
    not assigned(0, FT, fortune_teller),
    alive(FT, night(N, 0, 0)).
