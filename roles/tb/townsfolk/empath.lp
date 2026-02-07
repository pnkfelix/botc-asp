% Empath

first_night_substep(empath, 1, st_tells(empath_info)).
other_night_substep(empath, 1, st_tells(empath_info)).

% Count evil neighbors at time T
empath_evil_neighbor_count(Empath, N, T) :-
    character_assignment_state_at_time(T, Empath, empath),
    time(T),
    N = #count { P : living_neighbor(Empath, P, T), registers_as(P, evil, T) }.

% Empath learns evil neighbor count (functioning)
st_tells_core(empath, Empath, count(N), T) :-
    acting_role(T, empath),
    character_assignment_state_at_time(T, Empath, empath),
    alive(Empath, T),
    functioning(Empath, T),
    empath_evil_neighbor_count(Empath, N, T).

% Empath learns arbitrary count (not functioning - poisoned/drunk)
{ st_tells_core(empath, Empath, count(N), T) : N = 0..2 } = 1 :-
    acting_role(T, empath),
    character_assignment_state_at_time(T, Empath, empath),
    alive(Empath, T),
    not functioning(Empath, T).
