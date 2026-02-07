% Moonchild
% When you learn that you died, publicly choose 1 alive player. Tonight, if it was a good player, they die.

other_night_substep(moonchild, 1, st_asks(choose_target)).
other_night_substep(moonchild, 2, player_chooses(target)).
other_night_substep(moonchild, 3, st_places(moonchild_dead)).

% Helper: Moonchild died during the night (separate from day death: different death source/timing)
moonchild_died_tonight(MC, N) :-
    assigned(0, MC, moonchild),
    night_number(N), N > 1,
    alive(MC, night(N, 0, 0)),
    died(MC, T),
    time(T), T = night(N, R, S).

% Helper: Moonchild died during the day by execution (separate from night death: different time/cause)
moonchild_died_today(MC, N) :-
    assigned(0, MC, moonchild),
    day_number(N),
    executed(MC, N).

% Night death choice: Moonchild picks a player at dawn (can't merge with day: different alive-check time)
{ moonchild_choice(MC, P, N) : player(P), P != MC, alive(P, dawn(N)) } = 1 :-
    moonchild_died_tonight(MC, N).

% Day death choice: Moonchild picks a player at execution (separate: different trigger and alive-check)
{ moonchild_choice(MC, P, N) : player(P), P != MC, alive(P, day(N, exec)) } = 1 :-
    moonchild_died_today(MC, N).

% Helper: unifies both death sources into one predicate (can't fold: different source predicates)
moonchild_died_day_n(MC, N) :- moonchild_died_tonight(MC, N).
moonchild_died_day_n(MC, N) :- moonchild_died_today(MC, N).

% Bridge: emit player_chooses on the following night (choice is processed at next night's wake time)
player_chooses(moonchild, MC, point(P), night(N+1, RoleOrd, 2)) :-
    other_night_role_order(moonchild, RoleOrd),
    moonchild_choice(MC, P, N),
    moonchild_died_today(MC, N),
    night_number(N+1).

% Kill resolves: chosen player dies if actually good and Moonchild was functioning at dawn
moonchild_kills(P, N) :-
    moonchild_choice(MC, P, N),
    assigned(0, MC, moonchild),
    moonchild_died_day_n(MC, N),
    functioning(MC, dawn(N)),
    actual_category(P, good),
    night_number(N+1).

% Place death token on victim for grimoire display
reminder_on(moonchild_dead, P, night(N+1, RoleOrd, 3)) :-
    other_night_role_order(moonchild, RoleOrd),
    moonchild_kills(P, N).

% Mark victim as dead at the instant of the kill (for mid-night state tracking)
dead_at_time(P, T) :- reminder_on(moonchild_dead, P, T).
% Record death at final night time for dawn processing (separate: different timing semantics)
died(P, T) :- moonchild_kills(P, N), final_night_time(N+1, T), alive(P, night(N+1, 0, 0)).
