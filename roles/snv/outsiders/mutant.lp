% Mutant
% If you are "mad" about being an Outsider, you might be executed.

% Helper: Mutant may be executed by ST if they claim to be an Outsider (madness condition from input)
mutant_may_be_executed(Mutant, N) :-
    mutant_is_mad(Mutant, N),
    assigned(0, Mutant, mutant),
    alive(Mutant, day(N, 0)).

