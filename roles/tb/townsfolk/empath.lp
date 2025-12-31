% Empath

first_night_substep(empath, 1, st_tells(empath_info)).
other_night_substep(empath, 1, st_tells(empath_info)).

empath_evil_neighbor_count(Empath, N, T) :-
    assigned(0, Empath, empath),
    time(T),
    N = #count { P : living_neighbor(Empath, P, T), registers_as(P, evil, T) }.

st_tells_core(empath, Empath, count(N), T) :-
    acting_role(T, empath),
    assigned(0, Empath, empath),
    alive(Empath, T),
    functioning(Empath, T),
    empath_evil_neighbor_count(Empath, N, T).

{ st_tells_core(empath, Empath, count(N), T) : N = 0..2 } = 1 :-
    acting_role(T, empath),
    assigned(0, Empath, empath),
    alive(Empath, T),
    not functioning(Empath, T).
