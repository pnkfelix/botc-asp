% Artist
% Once per game, during the day, privately ask the Storyteller any yes/no question.

% The Artist ability is a day ability, which is not fully modeled in the night-based system.
% We provide a framework for tracking when the ability is used.

% artist_asks(Artist, Day) - input predicate indicating the artist asked a question on this day
% artist_answer(Artist, Day, yes/no) - the ST's answer

% Helper: records the day the Artist used their once-per-game ability (can't inline: used by uniqueness constraint below)
artist_used(Artist, N) :-
    artist_asks(Artist, N).

% Uniqueness constraint: Artist ability is once-per-game (can't fold into artist_used: constraints must be separate rules in ASP)
:- artist_used(Artist, N1), artist_used(Artist, N2), N1 != N2.

% Functioning tell: ST chooses yes/no answer for the Artist's question (can't merge with not-functioning: functioning requires truthfulness per game semantics)
{ st_tells_core(artist, Artist, yes, day(N, 0));
  st_tells_core(artist, Artist, no, day(N, 0)) } = 1 :-
    artist_asks(Artist, N),
    assigned(0, Artist, artist),
    alive(Artist, day(N, 0)),
    functioning(Artist, day(N, 0)).

% Not-functioning tell: ST freely chooses yes/no (can't merge with functioning: no truthfulness obligation when poisoned/drunk)
{ st_tells_core(artist, Artist, yes, day(N, 0));
  st_tells_core(artist, Artist, no, day(N, 0)) } = 1 :-
    artist_asks(Artist, N),
    assigned(0, Artist, artist),
    alive(Artist, day(N, 0)),
    not functioning(Artist, day(N, 0)).
