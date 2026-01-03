% Artist
% Once per game, during the day, privately ask the Storyteller any yes/no question.

% The Artist ability is a day ability, which is not fully modeled in the night-based system.
% We provide a framework for tracking when the ability is used.

% artist_asks(Artist, Day) - input predicate indicating the artist asked a question on this day
% artist_answer(Artist, Day, yes/no) - the ST's answer

% Track when artist uses their ability
artist_used(Artist, N) :-
    artist_asks(Artist, N).

% Cannot use ability twice
:- artist_used(Artist, N1), artist_used(Artist, N2), N1 != N2.

% When the artist asks and is functioning, ST gives a truthful answer
% The actual question is abstract - the answer must be modeled per-question
{ st_tells_core(artist, Artist, yes, day(N, 0));
  st_tells_core(artist, Artist, no, day(N, 0)) } = 1 :-
    artist_asks(Artist, N),
    assigned(0, Artist, artist),
    alive(Artist, day(N, 0)),
    functioning(Artist, day(N, 0)).

% When not functioning, can get either answer
{ st_tells_core(artist, Artist, yes, day(N, 0));
  st_tells_core(artist, Artist, no, day(N, 0)) } = 1 :-
    artist_asks(Artist, N),
    assigned(0, Artist, artist),
    alive(Artist, day(N, 0)),
    not functioning(Artist, day(N, 0)).
