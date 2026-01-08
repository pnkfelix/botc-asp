% ===========================================================================
% Domain constraints ("type checking")
% These catch errors like typos in player names or invalid roles.
% ===========================================================================

% --- Player name validation ---
:- assigned(0, P, _), not player(P).
:- reminder_on(_, P, _), not player(P).
:- player_chooses(_, P, _, _), not player(P).
:- st_tells_core(_, P, _, _), not player(P).
:- slayer_shot(P, _, _), not player(P).
:- slayer_shot(_, P, _), not player(P).
:- executed(P, _), not player(P).
:- died(P, _), not player(P).
:- alive(P, _), not player(P).
:- functioning(P, _), not player(P).
:- impaired(P, _), not player(P).
:- dead_at_time(P, _), not player(P).
:- publicly_dead_at_time(P, _), not player(P).

% --- Role name validation ---
:- assigned(0, _, R), not role(R).
:- acting_role(_, R), not role(R).
:- first_night_role_order(R, _), not role(R).
:- other_night_role_order(R, _), not role(R).
:- player_chooses(R, _, _, _), not role(R).
:- st_tells_core(R, _, _, _), not role(R).

% --- Time validation ---
:- reminder_on(_, _, T), not time(T).
:- player_chooses(_, _, _, T), not time(T).
:- st_tells_core(_, _, _, T), not time(T).
:- alive(_, T), not time(T).
:- died(_, T), not time(T).
:- functioning(_, T), not time(T).
:- dead_at_time(_, T), not time(T).
:- publicly_dead_at_time(_, T), not time(T).
