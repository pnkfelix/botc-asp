% Spy

first_night_substep(spy, 1, st_shows(grimoire)).

may_mis_register(P) :- assigned(0, P, spy).
may_misregister_as(P, good) :- assigned(0, P, spy).
may_misregister_as(P, townsfolk) :- assigned(0, P, spy).
may_misregister_as(P, outsider) :- assigned(0, P, spy).
