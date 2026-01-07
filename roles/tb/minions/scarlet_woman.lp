% Scarlet Woman

alive_count(N, C) :-
    day_number(N),
    C = #count { P : alive(P, day(N, 0)) }.

sw_becomes_demon(SW, N) :-
    assigned(0, SW, scarlet_woman),
    assigned(0, Demon, R), demon(R),
    executed(Demon, N),
    alive_count(N, C), C >= 5,
    alive(SW, day(N, 0)).

% Role change happens at execution time - use fine-grained predicate
role_assignment_changes_to(day(N, exec), SW, imp) :- sw_becomes_demon(SW, N).
