% Drunk

never_in_bag(drunk).
mistaken_identity(drunk, townsfolk).

reminder_on(drunk_is_drunk, P, T) :- assigned(0, P, drunk), time(T), alive(P, T).
causes_impairment(drunk_is_drunk).
