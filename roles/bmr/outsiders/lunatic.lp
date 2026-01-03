% Lunatic
% You think you are a Demon, but you are not. The Demon knows who you are & who you choose at night.

% Lunatic cannot be in the bag (they receive a demon token instead)
never_in_bag(lunatic).

% Lunatic thinks they are a demon (receives demon token)
mistaken_identity(lunatic, demon).

% The Lunatic "wakes" as a demon and makes choices, but nothing happens
% The real demon is informed of the Lunatic's identity and choices

% First night: Lunatic is shown fake minions (just like a real demon)
first_night_substep(lunatic, 1, st_shows(fake_minions)).
first_night_substep(lunatic, 2, st_shows(fake_bluffs)).

% Other nights: Lunatic makes "kill" choices (which do nothing)
% The demon is shown who the Lunatic chose
other_night_substep(lunatic, 1, st_asks(choose_target)).
other_night_substep(lunatic, 2, player_chooses(fake_kill)).

% Lunatic chooses targets (thinking they're killing)
{ lunatic_choice(Lunatic, P, N) : player(P) } = 1 :-
    night_number(N), N > 1,
    assigned(0, Lunatic, lunatic),
    alive(Lunatic, night(N, 0, 0)).

player_chooses(lunatic, Lunatic, point(P), night(N, RoleOrd, 2)) :-
    first_night_role_order(lunatic, RoleOrd),
    lunatic_choice(Lunatic, P, N).

% The real demon learns who the Lunatic is (first night)
% And who the Lunatic chooses each night
% This is handled by the demon role waking briefly to see the Lunatic

demon_knows_lunatic(Demon, Lunatic) :-
    assigned(0, Demon, D), demon(D),
    assigned(0, Lunatic, lunatic).

demon_knows_lunatic_choice(Demon, Lunatic, P, N) :-
    demon_knows_lunatic(Demon, Lunatic),
    lunatic_choice(Lunatic, P, N).
