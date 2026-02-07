% Godfather
% You start knowing which Outsiders are in play. If 1 died today, choose a player tonight: they die. [-1 or +1 Outsider]

first_night_substep(godfather, 1, st_tells(gf_outsiders)).

other_night_substep(godfather, 1, st_asks(choose_target)).
other_night_substep(godfather, 2, player_chooses(target)).
other_night_substep(godfather, 3, st_places(gf_dead)).

% Setup: Godfather modifies outsider count by +1 or -1 (ST chooses which; "[−1 or +1 Outsider]")
{ causes_outsider_mod(godfather, -1); causes_outsider_mod(godfather, 1) } = 1 :-
    assigned(0, _, godfather).

% Helper: which outsider roles are actually in the game (used for info + count)
gf_sees_outsider(R) :- assigned(0, _, R), outsider(R).

% Helper: total number of distinct outsider roles in play (can't inline: #count must be in rule body)
gf_outsider_count(C) :- C = #count { R : gf_sees_outsider(R) }.

% Alphabetical ordering facts for outsiders (needed to present a deterministic sorted list)
outsider_alpha_order(barber, 1).
outsider_alpha_order(butler, 2).
outsider_alpha_order(drunk, 3).
outsider_alpha_order(goon, 4).
outsider_alpha_order(klutz, 5).
outsider_alpha_order(lunatic, 6).
outsider_alpha_order(moonchild, 7).
outsider_alpha_order(mutant, 8).
outsider_alpha_order(recluse, 9).
outsider_alpha_order(saint, 10).
outsider_alpha_order(sweetheart, 11).
outsider_alpha_order(tinker, 12).

% Helper: maps each in-play outsider to its sorted position N (for deterministic info display)
gf_sorted_outsider(N, R) :-
    gf_sees_outsider(R),
    outsider_alpha_order(R, Ord),
    N = #count { R2 : gf_sees_outsider(R2), outsider_alpha_order(R2, O2), O2 <= Ord }.

% Tell with 0 outsiders (separate rules per count: ASP can't parameterize tuple arity)
st_tells_core(godfather, GF, outsiders(none), night(1, RoleOrd, 1)) :-
    first_night_role_order(godfather, RoleOrd),
    assigned(0, GF, godfather),
    gf_outsider_count(0).

% Tell with 1 outsider (separate: different tuple arity)
st_tells_core(godfather, GF, outsiders(R1), night(1, RoleOrd, 1)) :-
    first_night_role_order(godfather, RoleOrd),
    assigned(0, GF, godfather),
    gf_outsider_count(1),
    gf_sorted_outsider(1, R1).

% Tell with 2 outsiders (separate: different tuple arity)
st_tells_core(godfather, GF, outsiders(R1, R2), night(1, RoleOrd, 1)) :-
    first_night_role_order(godfather, RoleOrd),
    assigned(0, GF, godfather),
    gf_outsider_count(2),
    gf_sorted_outsider(1, R1),
    gf_sorted_outsider(2, R2).

% Tell with 3 outsiders (separate: different tuple arity)
st_tells_core(godfather, GF, outsiders(R1, R2, R3), night(1, RoleOrd, 1)) :-
    first_night_role_order(godfather, RoleOrd),
    assigned(0, GF, godfather),
    gf_outsider_count(3),
    gf_sorted_outsider(1, R1),
    gf_sorted_outsider(2, R2),
    gf_sorted_outsider(3, R3).

% Tell with 4 outsiders (separate: different tuple arity)
st_tells_core(godfather, GF, outsiders(R1, R2, R3, R4), night(1, RoleOrd, 1)) :-
    first_night_role_order(godfather, RoleOrd),
    assigned(0, GF, godfather),
    gf_outsider_count(4),
    gf_sorted_outsider(1, R1),
    gf_sorted_outsider(2, R2),
    gf_sorted_outsider(3, R3),
    gf_sorted_outsider(4, R4).

% Helper: true if any outsider died on day N (trigger condition for Godfather's kill ability)
outsider_died_today(N) :-
    day_number(N),
    died(P, day(N, _)),
    assigned(0, P, R),
    outsider(R).

% If an outsider died today, Godfather picks a player to kill tonight (conditional kill trigger)
{ godfather_target(GF, P, N) : player(P), P != GF, alive(P, night(N+1, 0, 0)) } = 1 :-
    night_number(N+1),
    day_number(N),
    outsider_died_today(N),
    assigned(0, GF, godfather),
    alive(GF, day(N, exec)).

% Bridge: emit player_chooses from godfather_target
player_chooses(godfather, GF, point(P), night(N+1, RoleOrd, 2)) :-
    other_night_role_order(godfather, RoleOrd),
    godfather_target(GF, P, N).

% Kill resolves: Godfather kill succeeds if functioning (treated like a demon kill for protection purposes)
godfather_kills(P, N) :-
    godfather_target(GF, P, N-1),
    assigned(0, GF, godfather),
    other_night_role_order(godfather, RoleOrd),
    functioning(GF, night(N, RoleOrd, 2)),
    night_number(N).

% Place death token on victim for grimoire display
reminder_on(gf_dead, P, night(N, RoleOrd, 3)) :-
    other_night_role_order(godfather, RoleOrd),
    godfather_kills(P, N).

% Mark victim as dead at instant of kill if not protected from demon (treated as demon-type kill)
dead_at_time(P, T) :- reminder_on(gf_dead, P, T), not protected_from_demon(P, T).
% Record death at final night time for dawn processing (separate: different timing semantics)
died(P, T) :- godfather_kills(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)), not protected_from_demon(P, T).
