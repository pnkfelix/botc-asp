% Klutz
% When you learn that you died, publicly choose 1 alive player: if they are evil, your team loses.

% Helper: Klutz died during the night (separate rules per time type: different unification)
klutz_died(Klutz, N) :-
    assigned(0, Klutz, klutz),
    died(Klutz, T),
    T = night(N, _, _).

% Helper: Klutz died during the day
klutz_died(Klutz, N) :-
    assigned(0, Klutz, klutz),
    died(Klutz, day(N, _)).

% Helper: Klutz died at dawn
klutz_died(Klutz, N) :-
    assigned(0, Klutz, klutz),
    died(Klutz, dawn(N)).

% Klutz must publicly choose exactly one alive player upon learning of their death
{ klutz_chooses(Klutz, P, N) : player(P), P != Klutz, alive(P, dawn(N)) } = 1 :-
    klutz_died(Klutz, N).

% Helper: true when Klutz chose an actually evil player (triggers evil win condition)
klutz_chose_evil(Klutz, N) :-
    klutz_chooses(Klutz, P, N),
    actual_category(P, evil).

% Evil wins immediately at dawn if Klutz chose an evil player
evil_wins(dawn(N)) :-
    klutz_chose_evil(_, N).
