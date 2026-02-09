% Lunatic
% You think you are a Demon, but you are not. The Demon knows who you are & who you choose at night.

% Lunatic is never in the bag (receives a demon token via mistaken_identity instead)
never_in_bag(lunatic).

% Lunatic believes they are the demon (ST gives them demon token at setup)
mistaken_identity(lunatic, demon).

first_night_substep(lunatic, 1, st_shows(fake_minions)).
first_night_substep(lunatic, 2, st_shows(fake_bluffs)).

other_night_substep(lunatic, 1, st_asks(choose_target)).
other_night_substep(lunatic, 2, player_chooses(fake_kill)).

% Lunatic picks a "kill" target each night (has no effect; choice is relayed to real demon)
{ lunatic_choice(Lunatic, P, N) : player(P) } = 1 :-
    night_number(N), N > 1,
    assigned(0, Lunatic, lunatic),
    alive(Lunatic, night(N, 0, 0)).

% Bridge: emit player_chooses from lunatic_choice (visible in grimoire even though no kill happens)
player_chooses(lunatic, Lunatic, point(P), night(N, RoleOrd, 2)) :-
    first_night_role_order(lunatic, RoleOrd),
    lunatic_choice(Lunatic, P, N).

% Helper: the real demon knows who the Lunatic is (always true when both are in play)
demon_knows_lunatic(Demon, Lunatic) :-
    assigned(0, Demon, D), demon(D),
    assigned(0, Lunatic, lunatic).

% Helper: the real demon learns each of the Lunatic's nightly choices
demon_knows_lunatic_choice(Demon, Lunatic, P, N) :-
    demon_knows_lunatic(Demon, Lunatic),
    lunatic_choice(Lunatic, P, N).
