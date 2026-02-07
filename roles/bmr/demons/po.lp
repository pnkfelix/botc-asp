% Po
% Each night*, you may choose a player: they die. If your last choice was no-one, choose 3 players tonight.

other_night_substep(po, 1, st_asks(choose_targets)).
other_night_substep(po, 2, player_chooses(targets)).
other_night_substep(po, 3, st_places(po_dead)).

% Helper: true when Po made no kill choice (charges up the triple-kill for next night)
po_chose_no_one(Po, N) :-
    assigned(0, Po, po),
    night_number(N), N > 1,
    not po_target(Po, _, _, N).

% Normal night: Po may kill 0 or 1 (<=1 is a choice); can't merge with charge night (different cardinality)
{ po_target(Po, P, 1, N) : player(P), P != Po, alive(P, night(N, 0, 0)) } <= 1 :-
    night_number(N), N > 1,
    other_night_role_order(po, RoleOrd),
    assigned(0, Po, po),
    alive(Po, night(N, 0, 0)),
    not demon_blocked(Po, N),
    game_active(night(N, 0, 0)),
    not po_chose_no_one(Po, N-1).

% Charge night target 1: must pick exactly 1 (separate rules per slot because they need distinct-player constraint)
{ po_target(Po, P, 1, N) : player(P), P != Po, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 2,
    other_night_role_order(po, RoleOrd),
    assigned(0, Po, po),
    alive(Po, night(N, 0, 0)),
    not demon_blocked(Po, N),
    game_active(night(N, 0, 0)),
    po_chose_no_one(Po, N-1).

% Charge night target 2: separate slot (can't merge with slot 1: each needs its own choice variable)
{ po_target(Po, P, 2, N) : player(P), P != Po, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 2,
    other_night_role_order(po, RoleOrd),
    assigned(0, Po, po),
    alive(Po, night(N, 0, 0)),
    not demon_blocked(Po, N),
    game_active(night(N, 0, 0)),
    po_chose_no_one(Po, N-1).

% Charge night target 3: separate slot
{ po_target(Po, P, 3, N) : player(P), P != Po, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 2,
    other_night_role_order(po, RoleOrd),
    assigned(0, Po, po),
    alive(Po, night(N, 0, 0)),
    not demon_blocked(Po, N),
    game_active(night(N, 0, 0)),
    po_chose_no_one(Po, N-1).

% All three charge-night targets must be distinct players
:- po_target(Po, P, I, N), po_target(Po, P, J, N), I != J, po_chose_no_one(Po, N-1).

% Bridge: all targets (from any slot) generate a player_chooses atom at the same substep
player_chooses(po, Po, point(P), night(N, RoleOrd, 2)) :-
    other_night_role_order(po, RoleOrd),
    po_target(Po, P, _, N).

% Kill resolves at substep 3: functioning + no protection = success (applies to each target independently)
po_kill_succeeds(P, T) :-
    po_target(Po, P, _, N),
    assigned(0, Po, po),
    night_number(N), N > 1,
    other_night_role_order(po, RoleOrd),
    T = night(N, RoleOrd, 3),
    functioning(Po, T),
    not protected_from_demon(P, T).

% Place kill token on victim for grimoire display
reminder_on(po_dead, P, T) :- po_kill_succeeds(P, T).

% Mark victim as dead at instant of kill (for mid-night state tracking)
dead_at_time(P, T) :- po_kill_succeeds(P, T).
% Record death at final night time for dawn processing
died(P, T) :- po_kill_succeeds(P, KillTime), KillTime = night(N, _, _), final_night_time(N, T), alive(P, night(N, 0, 0)).
