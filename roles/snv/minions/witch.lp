% Witch
% Each night, choose a player: if they nominate tomorrow, they die. If just 3 players live, you lose this ability.

first_night_substep(witch, 1, st_asks(choose_target)).
first_night_substep(witch, 2, player_chooses(target)).
first_night_substep(witch, 3, st_places(witch_cursed)).

other_night_substep(witch, 1, st_asks(choose_target)).
other_night_substep(witch, 2, player_chooses(target)).
other_night_substep(witch, 3, st_places(witch_cursed)).

% Count alive players
witch_alive_count(N, C) :-
    night_number(N),
    C = #count { P : alive(P, night(N, 0, 0)) }.

% Witch ability is active only if more than 3 players alive
witch_ability_active(N) :-
    night_number(N),
    witch_alive_count(N, C),
    C > 3.

% Witch chooses a player on first night (if ability active)
{ player_chooses(witch, Witch, point(P), night(1, RoleOrd, 2)) : player(P), P != Witch } = 1 :-
    first_night_role_order(witch, RoleOrd),
    assigned(0, Witch, witch),
    witch_ability_active(1).

% Witch chooses a player on other nights (if alive and ability active)
{ player_chooses(witch, Witch, point(P), night(N, RoleOrd, 2)) : player(P), P != Witch } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(witch, RoleOrd),
    assigned(0, Witch, witch),
    alive(Witch, night(N, 0, 0)),
    witch_ability_active(N).

% The cursed token is placed on the chosen player
reminder_on(witch_cursed, P, night(N, RoleOrd, 3)) :-
    player_chooses(witch, Witch, point(P), night(N, RoleOrd, 2)),
    functioning(Witch, night(N, RoleOrd, 3)).

% Remove curse at end of day (it lasts only for that day's nominations)
token_removed(witch_cursed, P, night(N+1, 0, 0)) :-
    reminder_on(witch_cursed, P, day(N, exec)),
    night_number(N+1).

:- reminder_on(witch_cursed, P, T), token_removed(witch_cursed, P, T).

% nominated(Player, Day) is an input predicate
% If a cursed player nominates, they die
witch_curse_triggers(P, N) :-
    reminder_on(witch_cursed, P, day(N, 0)),
    nominated(P, N).

died(P, day(N, 0)) :-
    witch_curse_triggers(P, N).
