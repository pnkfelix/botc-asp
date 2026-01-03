% Savant
% Each day, you may visit the Storyteller to learn 2 things in private: 1 is true & 1 is false.

% The Savant ability is a day ability with abstract "statements" from the ST.
% We provide a framework for this interaction.

% savant_visits(Savant, Day) - input predicate indicating the savant visited ST on this day
% The ST provides two statements: one true, one false

% savant_statement(Savant, Day, 1, Statement) - first statement
% savant_statement(Savant, Day, 2, Statement) - second statement
% savant_true_statement(Savant, Day, 1 or 2) - which statement is true

% When the savant visits, exactly one statement is true
{ savant_true_statement(Savant, N, 1);
  savant_true_statement(Savant, N, 2) } = 1 :-
    savant_visits(Savant, N),
    assigned(0, Savant, savant),
    alive(Savant, day(N, 0)),
    functioning(Savant, day(N, 0)).

% When not functioning, either or neither could be "true" (from ST's perspective it's arbitrary)
{ savant_true_statement(Savant, N, 1);
  savant_true_statement(Savant, N, 2) } :-
    savant_visits(Savant, N),
    assigned(0, Savant, savant),
    alive(Savant, day(N, 0)),
    not functioning(Savant, day(N, 0)).

% The actual content of statements is abstract and must be defined per-game
% Example: savant_statement(savant_player, 1, "The demon is sitting next to a minion")
