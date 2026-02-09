% Witch
% Each night, choose a player: if they nominate tomorrow, they die. If just 3 players live, you lose this ability.

first_night_substep(witch, 1, st_asks(choose_target)).
first_night_substep(witch, 2, player_chooses(target)).
first_night_substep(witch, 3, st_places(witch_cursed)).

other_night_substep(witch, 1, st_asks(choose_target)).
other_night_substep(witch, 2, player_chooses(target)).
other_night_substep(witch, 3, st_places(witch_cursed)).

% Aggregate helper: count alive players per night (can't inline: #count must be pre-computed for ability_active check)
witch_alive_count(N, C) :-
    night_number(N),
    C = #count { P : alive(P, night(N, 0, 0)) }.

% Helper: Witch ability is active only if more than 3 players alive (can't inline: used by both night 1 and other nights choice)
witch_ability_active(N) :-
    night_number(N),
    witch_alive_count(N, C),
    C > 3.

% Night 1 choice: Witch picks a player to curse (can't merge with other nights: different role order, no alive check)
{ player_chooses(witch, Witch, point(P), night(1, RoleOrd, 2)) : player(P), P != Witch } = 1 :-
    first_night_role_order(witch, RoleOrd),
    assigned(0, Witch, witch),
    witch_ability_active(1).

% Other nights choice: adds alive check (separate from night 1: uses other_night_role_order)
{ player_chooses(witch, Witch, point(P), night(N, RoleOrd, 2)) : player(P), P != Witch } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(witch, RoleOrd),
    assigned(0, Witch, witch),
    alive(Witch, night(N, 0, 0)),
    witch_ability_active(N).

% Place curse token on chosen player when functioning (tracks who will die if they nominate)
reminder_on(witch_cursed, P, night(N, RoleOrd, 3)) :-
    player_chooses(witch, Witch, point(P), night(N, RoleOrd, 2)),
    functioning(Witch, night(N, RoleOrd, 3)).

% Remove curse token at start of next night (curse lasts only one day)
token_removed(witch_cursed, P, night(N+1, 0, 0)) :-
    reminder_on(witch_cursed, P, day(N, exec)),
    night_number(N+1).

% Integrity constraint: curse token cannot simultaneously exist and be removed at same time point
:- reminder_on(witch_cursed, P, T), token_removed(witch_cursed, P, T).

% Helper: curse triggers when cursed player nominates (input: nominated; can't inline: used by death rule below)
witch_curse_triggers(P, N) :-
    reminder_on(witch_cursed, P, day(N, 0)),
    nominated(P, N).

% Death: cursed player dies immediately when nomination triggers the curse
died(P, day(N, 0)) :-
    witch_curse_triggers(P, N).
