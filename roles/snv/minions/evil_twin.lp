% Evil Twin
% You & an opposing player know each other. If the good player is executed, evil wins. Good can't win if you both live.

first_night_substep(evil_twin, 1, st_places(evil_twin_pair)).
first_night_substep(evil_twin, 2, st_tells(evil_twin_info)).

% The Evil Twin is paired with a good player (the "good twin")
% This is set up during the first night
{ evil_twin_pair(ET, GoodTwin) : player(GoodTwin), GoodTwin != ET, actual_category(GoodTwin, good) } = 1 :-
    assigned(0, ET, evil_twin).

% Both twins learn each other
st_tells_core(evil_twin, ET, point(GoodTwin), night(1, RoleOrd, 2)) :-
    first_night_role_order(evil_twin, RoleOrd),
    evil_twin_pair(ET, GoodTwin).

% The good twin also learns who the evil twin is (through ST pointing)
% This is modeled as the ST telling the good twin about the evil twin
st_tells_core(evil_twin, GoodTwin, point(ET), night(1, RoleOrd, 2)) :-
    first_night_role_order(evil_twin, RoleOrd),
    evil_twin_pair(ET, GoodTwin).

% If the good twin is executed, evil wins
evil_wins(day(N, exec)) :-
    evil_twin_pair(ET, GoodTwin),
    executed(GoodTwin, N),
    alive(ET, day(N, 0)).

% Good cannot win while both twins are alive
% This blocks the normal good win condition when demon is executed
good_twins_both_alive(N) :-
    evil_twin_pair(ET, GoodTwin),
    day_number(N),
    alive(ET, day(N, 0)),
    alive(GoodTwin, day(N, 0)).

% Override demon_dies_unreplaced to not trigger good win if twins both alive
% Note: This requires modifying the interaction with game_end.lp
% For now, we add a constraint that good cannot win while both twins live
:- good_wins(T),
   T = day(N, exec),
   good_twins_both_alive(N).
