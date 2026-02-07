% Tinker
% You might die at any time.

other_night_substep(tinker, 1, st_places(tinker_dead)).

% ST may optionally kill the Tinker each night (choice rule; models "might die at any time")
{ tinker_dies(Tinker, N) } :-
    night_number(N), N > 1,
    assigned(0, Tinker, tinker),
    alive(Tinker, night(N, 0, 0)).

% Place death token for grimoire display
reminder_on(tinker_dead, Tinker, night(N, RoleOrd, 1)) :-
    other_night_role_order(tinker, RoleOrd),
    tinker_dies(Tinker, N).

% Mark Tinker as dead at the instant of the kill (for mid-night state tracking)
dead_at_time(P, T) :- reminder_on(tinker_dead, P, T).
% Record night death at final night time for dawn processing (separate: different timing semantics)
died(P, T) :- tinker_dies(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)).

% ST may also kill the Tinker during the day (separate from night: different time structure entirely)
{ tinker_dies_day(Tinker, N) } :-
    day_number(N),
    assigned(0, Tinker, tinker),
    alive(Tinker, day(N, 0)).

% Record day death (separate from night death: different time type)
died(P, day(N, 0)) :- tinker_dies_day(P, N).
