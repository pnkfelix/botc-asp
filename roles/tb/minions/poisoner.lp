% Poisoner

first_night_substep(poisoner, 1, st_asks(choose_target)).
first_night_substep(poisoner, 2, player_chooses(target)).
first_night_substep(poisoner, 3, st_places(poisoned)).

other_night_substep(poisoner, 1, st_asks(choose_target)).
other_night_substep(poisoner, 2, player_chooses(target)).
other_night_substep(poisoner, 3, st_places(poisoned)).

% Night 1: Poisoner picks a target (can't merge with other-nights: uses first_night_role_order)
{ player_chooses(poisoner, Poisoner, point(P), T) : player(P), P != Poisoner } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(poisoner, RoleOrd),
    character_assignment_state_at_time(T, Poisoner, poisoner).

% Helper: detects if the original poisoner was reassigned (e.g., starpass, pit-hag) before night N
poi_reassigned(P, N) :-
    assigned(T, P, _),
    T > 0,
    night_number(N),
    T < N.

% Other nights: Poisoner picks a target (also requires alive check, unlike night 1 where everyone lives)
{ player_chooses(poisoner, Poisoner, point(P), T) : player(P), P != Poisoner } = 1 :-
    T = night(N, RoleOrd, 2),
    night_number(N), N > 1,
    other_night_role_order(poisoner, RoleOrd),
    character_assignment_state_at_time(T, Poisoner, poisoner),
    alive(Poisoner, night(N, 0, 0)).

% Night 1: place poison token on the chosen target (separate from other-nights: different role order)
reminder_on(poi_poisoned, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(poisoner, RoleOrd),
    player_chooses(poisoner, _, point(P), night(1, RoleOrd, 2)).

% Other nights: place poison token on the chosen target
reminder_on(poi_poisoned, P, night(N, RoleOrd, 3)) :-
    night_number(N), N > 1,
    other_night_role_order(poisoner, RoleOrd),
    player_chooses(poisoner, _, point(P), night(N, RoleOrd, 2)).

% Normal expiry: old poison is removed when the poisoner is still active and picks a new target
token_removed(poi_poisoned, P, night(N, 0, 0)) :-
    T = night(N, 0, 0),
    night_number(N), N > 1,
    reminder_on(poi_poisoned, P, day(N-1, exec)),
    character_assignment_state_at_time(T, _, poisoner).

% Reassignment expiry: poison also removed if the original poisoner changed roles (starpass, pit-hag, etc.)
% Note: uses assigned(0,...) because we need the original poisoner identity to check for reassignment
token_removed(poi_poisoned, Target, night(N, 0, 0)) :-
    night_number(N), N > 1,
    reminder_on(poi_poisoned, Target, day(N-1, exec)),
    assigned(0, Poisoner, poisoner),
    poi_reassigned(Poisoner, N).

% Token removal takes effect: a token can't simultaneously exist and be removed at the same time
:- reminder_on(poi_poisoned, P, T), token_removed(poi_poisoned, P, T).

% Declare that this reminder token causes impairment (consumed by the generic functioning/impaired system)
causes_impairment(poi_poisoned).
