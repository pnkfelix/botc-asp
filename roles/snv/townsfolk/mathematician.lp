% Mathematician
% Each night, you learn how many players' abilities worked abnormally (possibly incorrectly) since dawn.

first_night_substep(mathematician, 1, st_tells(mathematician_info)).
other_night_substep(mathematician, 1, st_tells(mathematician_info)).

% Helper: true when a player received ST info while not functioning, i.e. their ability malfunctioned.
% Factored out because it is reused by the aggregate count below.
ability_malfunctioned(P, T) :-
    st_tells_core(_, P, _, T),
    not functioning(P, T).

% Helper: aggregate count of distinct players whose abilities malfunctioned this night.
% Factored out because #count cannot appear directly in a rule head argument.
abnormal_ability_count(N, NightNum) :-
    night_number(NightNum),
    N = #count { P : ability_malfunctioned(P, T), T = night(NightNum, _, _) }.

% Functioning tell, night 1: deterministic count of malfunctioned abilities.
% Separate from other-nights rule because night 1 uses first_night_role_order and has no alive check.
st_tells_core(mathematician, Math, count(N), T) :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(mathematician, RoleOrd),
    assigned(0, Math, mathematician),
    functioning(Math, T),
    abnormal_ability_count(N, 1).

% Not-functioning tell, night 1: ST freely chooses any count (choice rule).
% Can't merge with functioning rule: different semantics (choice vs deterministic).
{ st_tells_core(mathematician, Math, count(N), T) : N = 0..player_count } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(mathematician, RoleOrd),
    assigned(0, Math, mathematician),
    not functioning(Math, T).

% Functioning tell, other nights: deterministic count of malfunctioned abilities.
% Separate from night 1: uses other_night_role_order and requires alive check.
st_tells_core(mathematician, Math, count(N), T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(mathematician, RoleOrd),
    assigned(0, Math, mathematician),
    alive(Math, night(NightNum, 0, 0)),
    functioning(Math, T),
    abnormal_ability_count(N, NightNum).

% Not-functioning tell, other nights: ST freely chooses any count (choice rule).
% Can't merge with functioning rule: different semantics (choice vs deterministic).
{ st_tells_core(mathematician, Math, count(N), T) : N = 0..player_count } = 1 :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(mathematician, RoleOrd),
    assigned(0, Math, mathematician),
    alive(Math, night(NightNum, 0, 0)),
    not functioning(Math, T).
