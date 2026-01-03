% Godfather
% You start knowing which Outsiders are in play. If 1 died today, choose a player tonight: they die. [-1 or +1 Outsider]

first_night_substep(godfather, 1, st_tells(gf_outsiders)).

other_night_substep(godfather, 1, st_asks(choose_target)).
other_night_substep(godfather, 2, player_chooses(target)).
other_night_substep(godfather, 3, st_places(gf_dead)).

% Godfather causes outsider count modification
{ causes_outsider_mod(godfather, -1); causes_outsider_mod(godfather, 1) } = 1 :-
    assigned(0, _, godfather).

% First night: Godfather learns which outsiders are in play
% Count of outsiders in play
gf_outsider_count(C) :-
    C = #count { P : assigned(0, P, R), outsider(R) }.

% ST tells Godfather the outsiders (as a count for simplicity)
st_tells_core(godfather, GF, count(C), night(1, RoleOrd, 1)) :-
    first_night_role_order(godfather, RoleOrd),
    assigned(0, GF, godfather),
    gf_outsider_count(C).

% Check if an outsider died today
outsider_died_today(N) :-
    day_number(N),
    died(P, day(N, _)),
    assigned(0, P, R),
    outsider(R).

% If an outsider died today, Godfather may kill tonight
{ godfather_target(GF, P, N) : player(P), P != GF, alive(P, night(N+1, 0, 0)) } = 1 :-
    night_number(N+1),
    day_number(N),
    outsider_died_today(N),
    assigned(0, GF, godfather),
    alive(GF, day(N, exec)).

% Connect to player_chooses
player_chooses(godfather, GF, point(P), night(N+1, RoleOrd, 2)) :-
    other_night_role_order(godfather, RoleOrd),
    godfather_target(GF, P, N).

% Godfather kill (if functioning)
godfather_kills(P, N) :-
    godfather_target(GF, P, N-1),
    assigned(0, GF, godfather),
    other_night_role_order(godfather, RoleOrd),
    functioning(GF, night(N, RoleOrd, 2)),
    night_number(N).

reminder_on(gf_dead, P, night(N, RoleOrd, 3)) :-
    other_night_role_order(godfather, RoleOrd),
    godfather_kills(P, N).

% Godfather kill is not protected by normal means (same as demon kill)
dead(P, T) :- reminder_on(gf_dead, P, T), not protected_from_demon(P, T).
died(P, T) :- godfather_kills(P, N), final_night_time(N, T), alive(P, night(N, 0, 0)), not protected_from_demon(P, T).
