% Tinker
% You might die at any time.

other_night_substep(tinker, 1, st_places(tinker_dead)).

% ST may choose to kill the Tinker at any time
% This is modeled as an optional death each night
{ tinker_dies(Tinker, N) } :-
    night_number(N), N > 1,
    assigned(0, Tinker, tinker),
    alive(Tinker, night(N, 0, 0)).

% Place death token
reminder_on(tinker_dead, Tinker, night(N, RoleOrd, 1)) :-
    other_night_role_order(tinker, RoleOrd),
    tinker_dies(Tinker, N).

% Tinker death
dead(P, T) :- reminder_on(tinker_dead, P, T).
died(P, T) :- tinker_dies(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)).

% Tinker can also die during the day (ST choice)
{ tinker_dies_day(Tinker, N) } :-
    day_number(N),
    assigned(0, Tinker, tinker),
    alive(Tinker, day(N, 0)).

died(P, day(N, 0)) :- tinker_dies_day(P, N).
