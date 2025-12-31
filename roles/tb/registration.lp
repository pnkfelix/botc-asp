% Role Registration Rules

may_register_as_role(P, R, T) :-
    assigned(0, P, R),
    time(T).

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
