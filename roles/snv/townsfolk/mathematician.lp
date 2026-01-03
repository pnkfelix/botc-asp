% Mathematician
% Each night, you learn how many players' abilities worked abnormally (possibly incorrectly) since dawn.

first_night_substep(mathematician, 1, st_tells(mathematician_info)).
other_night_substep(mathematician, 1, st_tells(mathematician_info)).

% Track abilities that malfunctioned
% ability_malfunctioned(Player, T) is true if a player's ability gave wrong info at time T
% This happens when a player is poisoned/drunk but still receives info

ability_malfunctioned(P, T) :-
    st_tells_core(_, P, _, T),
    not functioning(P, T).

% Count unique players whose abilities malfunctioned during the current night
% For night 1, count from night start; for other nights, count from previous dawn
abnormal_ability_count(N, NightNum) :-
    night_number(NightNum),
    N = #count { P : ability_malfunctioned(P, T), time(T), T = night(NightNum, RO, S) }.

% First night
st_tells_core(mathematician, Math, count(N), T) :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(mathematician, RoleOrd),
    assigned(0, Math, mathematician),
    functioning(Math, T),
    abnormal_ability_count(N, 1).

{ st_tells_core(mathematician, Math, count(N), T) : N = 0..player_count } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(mathematician, RoleOrd),
    assigned(0, Math, mathematician),
    not functioning(Math, T).

% Other nights
st_tells_core(mathematician, Math, count(N), T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(mathematician, RoleOrd),
    assigned(0, Math, mathematician),
    alive(Math, night(NightNum, 0, 0)),
    functioning(Math, T),
    abnormal_ability_count(N, NightNum).

{ st_tells_core(mathematician, Math, count(N), T) : N = 0..player_count } = 1 :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(mathematician, RoleOrd),
    assigned(0, Math, mathematician),
    alive(Math, night(NightNum, 0, 0)),
    not functioning(Math, T).
