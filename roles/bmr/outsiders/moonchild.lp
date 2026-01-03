% Moonchild
% When you learn that you died, publicly choose 1 alive player. Tonight, if it was a good player, they die.

other_night_substep(moonchild, 1, st_asks(choose_target)).
other_night_substep(moonchild, 2, player_chooses(target)).
other_night_substep(moonchild, 3, st_places(moonchild_dead)).

% Moonchild died during the night (learned at dawn)
moonchild_died_tonight(MC, N) :-
    assigned(0, MC, moonchild),
    night_number(N), N > 1,
    alive(MC, night(N, 0, 0)),
    died(MC, T),
    time(T), T = night(N, R, S).

% Moonchild died during the day (executed)
moonchild_died_today(MC, N) :-
    assigned(0, MC, moonchild),
    day_number(N),
    executed(MC, N).

% When Moonchild learns they died (at dawn for night death, at execution for day death)
% they choose a player
{ moonchild_choice(MC, P, N) : player(P), P != MC, alive(P, dawn(N)) } = 1 :-
    moonchild_died_tonight(MC, N).

{ moonchild_choice(MC, P, N) : player(P), P != MC, alive(P, day(N, exec)) } = 1 :-
    moonchild_died_today(MC, N).

% Connect to player_chooses (Moonchild wakes at night to have their choice processed)
player_chooses(moonchild, MC, point(P), night(N+1, RoleOrd, 2)) :-
    other_night_role_order(moonchild, RoleOrd),
    moonchild_choice(MC, P, N),
    moonchild_died_today(MC, N),
    night_number(N+1).

% If Moonchild was functioning when they died and chose a good player, that player dies
moonchild_kills(P, N) :-
    moonchild_choice(MC, P, N),
    assigned(0, MC, moonchild),
    (moonchild_died_tonight(MC, N) ; moonchild_died_today(MC, N)),
    functioning(MC, dawn(N)),
    actual_category(P, good),
    night_number(N+1).

% Place death token
reminder_on(moonchild_dead, P, night(N+1, RoleOrd, 3)) :-
    other_night_role_order(moonchild, RoleOrd),
    moonchild_kills(P, N).

% Moonchild's victim dies
dead(P, T) :- reminder_on(moonchild_dead, P, T).
died(P, T) :- moonchild_kills(P, N), final_night_time(N+1, T), alive(P, night(N+1, 0, 0)).
