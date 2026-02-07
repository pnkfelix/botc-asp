% Drunk

% The "drunk" token never appears in the bag; the Drunk receives a townsfolk token instead
never_in_bag(drunk).
% The Drunk thinks they are a townsfolk (mistaken_identity drives the received-token logic in botc.lp)
mistaken_identity(drunk, townsfolk).

% The Drunk is permanently impaired while alive (their "townsfolk" ability never functions)
reminder_on(drunk_is_drunk, P, T) :- assigned(0, P, drunk), time(T), alive(P, T).
% Declare that this reminder token causes impairment (consumed by the generic functioning/impaired system)
causes_impairment(drunk_is_drunk).
