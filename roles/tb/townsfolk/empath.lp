% Empath

first_night_substep(empath, 1, st_tells(empath_info)).
other_night_substep(empath, 1, st_tells(empath_info)).

% Helper: count evil-registering living neighbors; separate because #count can't inline into st_tells_core
empath_evil_neighbor_count(Empath, N, T) :-
    character_assignment_state_at_time(T, Empath, empath),
    time(T),
    N = #count { P : living_neighbor(Empath, P, T), registers_as(P, evil, T) }.

% Functioning: Empath learns the true evil neighbor count (deterministic, not a choice)
st_tells_core(empath, Empath, count(N), T) :-
    acting_role(T, empath),
    character_assignment_state_at_time(T, Empath, empath),
    alive(Empath, T),
    functioning(Empath, T),
    empath_evil_neighbor_count(Empath, N, T).

% Not functioning: ST may give any count 0-2 (choice rule, unlike the deterministic functioning case)
{ st_tells_core(empath, Empath, count(N), T) : N = 0..2 } = 1 :-
    acting_role(T, empath),
    character_assignment_state_at_time(T, Empath, empath),
    alive(Empath, T),
    not functioning(Empath, T).
