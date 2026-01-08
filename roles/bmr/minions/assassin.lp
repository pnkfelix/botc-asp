% Assassin
% Once per game, at night*, choose a player: they die, even if for some reason they could not.

other_night_substep(assassin, 1, st_asks(choose_target)).
other_night_substep(assassin, 2, player_chooses(target)).
other_night_substep(assassin, 3, st_places(assassin_dead)).

% Track if Assassin has used their ability
assassin_used_ability(Assassin, N) :-
    assigned(0, Assassin, assassin),
    night_number(N),
    assassin_target(Assassin, _, N).

assassin_used_ability_before(Assassin, N) :-
    assassin_used_ability(Assassin, M),
    night_number(N),
    M < N.

% Assassin may choose a player (once per game, on nights after the first)
{ assassin_target(Assassin, P, N) : player(P), P != Assassin } <= 1 :-
    night_number(N), N > 1,
    other_night_role_order(assassin, RoleOrd),
    assigned(0, Assassin, assassin),
    alive(Assassin, night(N, 0, 0)),
    not assassin_used_ability_before(Assassin, N).

% Connect to player_chooses
player_chooses(assassin, Assassin, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(assassin, RoleOrd),
    assassin_target(Assassin, P, N).

% Assassin kill bypasses ALL protection - the target dies no matter what
assassin_kills(P, N) :-
    assassin_target(Assassin, P, N),
    assigned(0, Assassin, assassin),
    other_night_role_order(assassin, RoleOrd),
    functioning(Assassin, night(N, RoleOrd, 2)).

reminder_on(assassin_dead, P, night(N, RoleOrd, 3)) :-
    other_night_role_order(assassin, RoleOrd),
    assassin_kills(P, N).

% Assassin kill is absolute - ignores all protection
% This is a special death that bypasses protected_from_demon, Sailor, etc.
dead_at_time(P, T) :- reminder_on(assassin_dead, P, T).
died(P, T) :- assassin_kills(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)).

% Override Sailor protection for Assassin
:- assassin_kills(P, N), protected_from_death(P, T), time(T), T = night(N, R, S), not died(P, T).
