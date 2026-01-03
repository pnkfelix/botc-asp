% Klutz
% When you learn that you died, publicly choose 1 alive player: if they are evil, your team loses.

% The Klutz must choose a player when they die
% klutz_chooses(Klutz, Player, Day/Night) - the player the Klutz points to

% When the Klutz dies, they must choose an alive player
klutz_died(Klutz, N) :-
    assigned(0, Klutz, klutz),
    died(Klutz, T),
    T = night(N, _, _).

klutz_died(Klutz, N) :-
    assigned(0, Klutz, klutz),
    died(Klutz, day(N, _)).

klutz_died(Klutz, N) :-
    assigned(0, Klutz, klutz),
    died(Klutz, dawn(N)).

% Klutz must choose exactly one alive player when they die
{ klutz_chooses(Klutz, P, N) : player(P), P != Klutz, alive(P, dawn(N)) } = 1 :-
    klutz_died(Klutz, N).

% If the Klutz chooses an evil player, good loses (evil wins)
klutz_chose_evil(Klutz, N) :-
    klutz_chooses(Klutz, P, N),
    actual_category(P, evil).

% Evil wins if Klutz chose evil
evil_wins(dawn(N)) :-
    klutz_chose_evil(_, N).

% Note: This creates an immediate evil win condition at dawn if the Klutz
% died that night and chose an evil player.
