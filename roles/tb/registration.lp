% Role Registration Rules

% Base case: a player may always register as their actual current role
may_register_as_role(P, R, T) :-
    character_assignment_state_at_time(T, P, R).

% Misregistration: each category has its own rule because Spy/Recluse opt into categories independently
may_register_as_role(P, R, T) :-
    townsfolk(R),
    may_misregister_as(P, townsfolk),
    time(T).

may_register_as_role(P, R, T) :-
    outsider(R),
    may_misregister_as(P, outsider),
    time(T).

may_register_as_role(P, R, T) :-
    minion(R),
    may_misregister_as(P, minion),
    time(T).

may_register_as_role(P, R, T) :-
    demon(R),
    may_misregister_as(P, demon),
    time(T).
