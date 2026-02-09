% Scarlet Woman

% Helper: count living players on each day; separate because #count can't inline into sw_becomes_demon
alive_count(N, C) :-
    day_number(N),
    C = #count { P : alive(P, day(N, 0)) }.

% Helper: SW triggers when the demon is executed with 5+ alive and SW is alive (all conditions must hold)
sw_becomes_demon(SW, N) :-
    T = day(N, 0),
    character_assignment_state_at_time(T, SW, scarlet_woman),
    character_assignment_state_at_time(T, Demon, R), demon(R),
    executed(Demon, N),
    alive_count(N, C), C >= 5,
    alive(SW, T).

% Emit the role change at execution time; separate from sw_becomes_demon to use the generic reassignment system
role_assignment_changes_to(day(N, exec), SW, imp) :- sw_becomes_demon(SW, N).
