% Juggler
% On your 1st day, publicly guess up to 5 players' characters. That night, you learn how many you got correct.

other_night_substep(juggler, 1, st_tells(juggler_info)).

% juggler_guess(Juggler, Player, Role) - input predicate for each guess made on day 1
% A juggler can make up to 5 guesses

% Helper: marks each guess that matched the actual assignment (can't inline: used by aggregate count below)
juggler_correct_guess(Juggler, P, R) :-
    juggler_guess(Juggler, P, R),
    assigned(0, P, R).

% Aggregate helper: total number of correct guesses (can't fold into tell: aggregate must be pre-computed for use in st_tells_core)
juggler_correct_count(Juggler, N) :-
    assigned(0, Juggler, juggler),
    N = #count { P, R : juggler_correct_guess(Juggler, P, R) }.

% Max 5 guesses constraint: enforces the Juggler's guess limit (can't fold into correct_guess: this validates input, not correctness)
:- assigned(0, Juggler, juggler),
   #count { P, R : juggler_guess(Juggler, P, R) } > 5.

% Functioning tell: deterministic count from juggler_correct_count (can't merge with not-functioning: derived fact vs choice rule)
st_tells_core(juggler, Juggler, count(N), T) :-
    T = night(2, RoleOrd, 1),
    other_night_role_order(juggler, RoleOrd),
    assigned(0, Juggler, juggler),
    alive(Juggler, night(2, 0, 0)),
    functioning(Juggler, T),
    juggler_correct_count(Juggler, N).

% Not-functioning tell: ST freely chooses any count 0..5 (choice rule; can't merge with functioning: different semantics)
{ st_tells_core(juggler, Juggler, count(N), T) : N = 0..5 } = 1 :-
    T = night(2, RoleOrd, 1),
    other_night_role_order(juggler, RoleOrd),
    assigned(0, Juggler, juggler),
    alive(Juggler, night(2, 0, 0)),
    not functioning(Juggler, T).

% No-guesses fallback: Juggler still wakes and learns 0 (can't fold into functioning tell: #count over empty set needs explicit handling in ASP)
st_tells_core(juggler, Juggler, count(0), T) :-
    T = night(2, RoleOrd, 1),
    other_night_role_order(juggler, RoleOrd),
    assigned(0, Juggler, juggler),
    alive(Juggler, night(2, 0, 0)),
    functioning(Juggler, T),
    not juggler_guess(Juggler, _, _).
