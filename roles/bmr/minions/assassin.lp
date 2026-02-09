% Assassin
% Once per game, at night*, choose a player: they die, even if for some reason they could not.

other_night_substep(assassin, 1, st_asks(choose_target)).
other_night_substep(assassin, 2, player_chooses(target)).
other_night_substep(assassin, 3, st_places(assassin_dead)).

% Helper: records the night the Assassin used their once-per-game ability (can't inline: used by _before check)
assassin_used_ability(Assassin, N) :-
    assigned(0, Assassin, assassin),
    night_number(N),
    assassin_target(Assassin, _, N).

% Helper: true if ability was used on any earlier night M < N (enforces once-per-game)
assassin_used_ability_before(Assassin, N) :-
    assassin_used_ability(Assassin, M),
    night_number(N),
    M < N.

% Choice: Assassin may pick one player to kill (<=1 means can pass; once-per-game gated by _before check)
{ assassin_target(Assassin, P, N) : player(P), P != Assassin } <= 1 :-
    night_number(N), N > 1,
    other_night_role_order(assassin, RoleOrd),
    assigned(0, Assassin, assassin),
    alive(Assassin, night(N, 0, 0)),
    not assassin_used_ability_before(Assassin, N).

% Bridge: emit player_chooses from assassin_target
player_chooses(assassin, Assassin, point(P), night(N, RoleOrd, 2)) :-
    night_number(N), N > 1,
    other_night_role_order(assassin, RoleOrd),
    assassin_target(Assassin, P, N).

% Kill resolves: Assassin kill bypasses ALL protection if functioning (absolute kill)
assassin_kills(P, N) :-
    assassin_target(Assassin, P, N),
    assigned(0, Assassin, assassin),
    other_night_role_order(assassin, RoleOrd),
    functioning(Assassin, night(N, RoleOrd, 2)).

% Place death token on victim for grimoire display
reminder_on(assassin_dead, P, night(N, RoleOrd, 3)) :-
    other_night_role_order(assassin, RoleOrd),
    assassin_kills(P, N).

% Mark victim as dead at the instant of the kill (for mid-night state tracking)
dead_at_time(P, T) :- reminder_on(assassin_dead, P, T).
% Record death at final night time for dawn processing (separate: different timing semantics)
died(P, T) :- assassin_kills(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)).

% Constraint: Assassin kill overrides even Sailor's protected_from_death (must still produce died)
:- assassin_kills(P, N), protected_from_death(P, T), time(T), T = night(N, R, S), not died(P, T).
