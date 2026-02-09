% Spy

first_night_substep(spy, 1, st_shows(grimoire)).

% Flag: Spy bypasses the default registers_as logic (which would lock them to evil/minion)
may_mis_register(P) :- assigned(0, P, spy).
% Each category the Spy may falsely register as; separate atoms because ST chooses independently per query
may_misregister_as(P, good) :- assigned(0, P, spy).
may_misregister_as(P, townsfolk) :- assigned(0, P, spy).
may_misregister_as(P, outsider) :- assigned(0, P, spy).
