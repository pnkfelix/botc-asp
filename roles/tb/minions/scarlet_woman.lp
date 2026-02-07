% Scarlet Woman

alive_count(N, C) :-
    day_number(N),
    C = #count { P : alive(P, day(N, 0)) }.

sw_becomes_demon(SW, N) :-
    T = day(N, 0),
    character_assignment_state_at_time(T, SW, scarlet_woman),
    character_assignment_state_at_time(T, Demon, R), demon(R),
    executed(Demon, N),
    alive_count(N, C), C >= 5,
    alive(SW, T).

% Role change happens at execution time - use fine-grained predicate
role_assignment_changes_to(day(N, exec), SW, imp) :- sw_becomes_demon(SW, N).
