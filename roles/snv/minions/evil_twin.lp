% Evil Twin
% You & an opposing player know each other. If the good player is executed, evil wins. Good can't win if you both live.

first_night_substep(evil_twin, 1, st_places(evil_twin_pair)).
first_night_substep(evil_twin, 2, st_tells(evil_twin_info)).

% Setup: pair Evil Twin with exactly one good player (choice rule: ST picks the good twin at game start)
{ evil_twin_pair(ET, GoodTwin) : player(GoodTwin), GoodTwin != ET, actual_category(GoodTwin, good) } = 1 :-
    assigned(0, ET, evil_twin).

% Tell Evil Twin who their good twin is (separate from good twin's tell: different recipient)
st_tells_core(evil_twin, ET, point(GoodTwin), night(1, RoleOrd, 2)) :-
    first_night_role_order(evil_twin, RoleOrd),
    evil_twin_pair(ET, GoodTwin).

% Tell good twin who the Evil Twin is (separate from ET's tell: different recipient and value)
st_tells_core(evil_twin, GoodTwin, point(ET), night(1, RoleOrd, 2)) :-
    first_night_role_order(evil_twin, RoleOrd),
    evil_twin_pair(ET, GoodTwin).

% Evil wins immediately if the good twin is executed while the Evil Twin lives
evil_wins(day(N, exec)) :-
    evil_twin_pair(ET, GoodTwin),
    executed(GoodTwin, N),
    alive(ET, day(N, 0)).

% Helper: true when both twins are alive on day N (can't inline: used by constraint below)
good_twins_both_alive(N) :-
    evil_twin_pair(ET, GoodTwin),
    day_number(N),
    alive(ET, day(N, 0)),
    alive(GoodTwin, day(N, 0)).

% Constraint: good cannot win by execution while both twins are alive (blocks normal demon-death win)
:- good_wins(T),
   T = day(N, exec),
   good_twins_both_alive(N).
