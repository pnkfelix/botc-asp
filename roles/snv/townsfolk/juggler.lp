% Juggler
% On your 1st day, publicly guess up to 5 players' characters. That night, you learn how many you got correct.

other_night_substep(juggler, 1, st_tells(juggler_info)).

% juggler_guess(Juggler, Player, Role) - input predicate for each guess made on day 1
% A juggler can make up to 5 guesses

% Count correct guesses
juggler_correct_guess(Juggler, P, R) :-
    juggler_guess(Juggler, P, R),
    assigned(0, P, R).

juggler_correct_count(Juggler, N) :-
    assigned(0, Juggler, juggler),
    N = #count { P, R : juggler_correct_guess(Juggler, P, R) }.

% Constraint: at most 5 guesses
:- assigned(0, Juggler, juggler),
   #count { P, R : juggler_guess(Juggler, P, R) } > 5.

% Juggler learns the count on night 1 (the night after their first day)
st_tells_core(juggler, Juggler, count(N), T) :-
    T = night(2, RoleOrd, 1),
    other_night_role_order(juggler, RoleOrd),
    assigned(0, Juggler, juggler),
    alive(Juggler, night(2, 0, 0)),
    functioning(Juggler, T),
    juggler_correct_count(Juggler, N).

% When not functioning, can get any count 0-5
{ st_tells_core(juggler, Juggler, count(N), T) : N = 0..5 } = 1 :-
    T = night(2, RoleOrd, 1),
    other_night_role_order(juggler, RoleOrd),
    assigned(0, Juggler, juggler),
    alive(Juggler, night(2, 0, 0)),
    not functioning(Juggler, T).

% If no guesses were made, juggler still wakes but learns 0
st_tells_core(juggler, Juggler, count(0), T) :-
    T = night(2, RoleOrd, 1),
    other_night_role_order(juggler, RoleOrd),
    assigned(0, Juggler, juggler),
    alive(Juggler, night(2, 0, 0)),
    functioning(Juggler, T),
    not juggler_guess(Juggler, _, _).
