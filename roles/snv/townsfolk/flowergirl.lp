% Flowergirl
% Each night*, you learn if a Demon voted today.

other_night_substep(flowergirl, 1, st_tells(flowergirl_info)).

% Helper: true when the demon voted on a given day (can't inline: referenced by both functioning yes and no tells)
demon_voted(N) :-
    day_number(N),
    voted(P, N),
    character_assignment_state_at_time(day(N, 0), P, R),
    demon(R).

% Functioning "yes" tell: demon did vote (can't merge with "no": opposite condition on demon_voted; can't use choice rule: answer is deterministic)
st_tells_core(flowergirl, FG, yes, T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(flowergirl, RoleOrd),
    assigned(0, FG, flowergirl),
    alive(FG, night(NightNum, 0, 0)),
    functioning(FG, T),
    demon_voted(NightNum - 1).

% Functioning "no" tell: demon did not vote (can't merge with "yes": requires negation of demon_voted, separate derived fact)
st_tells_core(flowergirl, FG, no, T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(flowergirl, RoleOrd),
    assigned(0, FG, flowergirl),
    alive(FG, night(NightNum, 0, 0)),
    functioning(FG, T),
    not demon_voted(NightNum - 1).

% Not-functioning tell: ST freely chooses yes/no (choice rule; can't merge with functioning: different semantics, no truthfulness obligation)
{ st_tells_core(flowergirl, FG, yes, T);
  st_tells_core(flowergirl, FG, no, T) } = 1 :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(flowergirl, RoleOrd),
    assigned(0, FG, flowergirl),
    alive(FG, night(NightNum, 0, 0)),
    not functioning(FG, T).
