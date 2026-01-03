% Mutant
% If you are "mad" about being an Outsider, you might be executed.

% The Mutant's ability is a social/behavioral constraint.
% "Mad" means the player is actively claiming or implying they are an Outsider.
% The ST may execute them for breaking madness.

% mutant_is_mad(Mutant, Day) - input predicate indicating the mutant broke madness on this day
% (i.e., they claimed or strongly implied they are an Outsider)

% If the Mutant is mad about being an Outsider, ST may execute them
% This is represented as the ST having the option to execute regardless of votes
mutant_may_be_executed(Mutant, N) :-
    mutant_is_mad(Mutant, N),
    assigned(0, Mutant, mutant),
    alive(Mutant, day(N, 0)).

% Note: The actual execution is still done via the executed(Player, Day) input predicate.
% This predicate just marks that the ST has the option to do so.

% If a non-Mutant claims to be an Outsider, the ST should NOT execute them under Mutant rules.
% This is a game rule constraint - the predicate is informational only.
