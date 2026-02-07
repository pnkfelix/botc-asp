% Town Crier
% Each night*, you learn if a Minion nominated today.

other_night_substep(town_crier, 1, st_tells(town_crier_info)).

% Helper: true if any minion nominated on day N (trigger condition for the tell).
% Factored out so the functioning yes/no rules can reference a single boolean predicate.
minion_nominated(N) :-
    day_number(N),
    nominated(P, N),
    assigned(0, P, R),
    minion(R).

% Functioning 'yes' tell: deterministic when a minion nominated today.
% Can't merge with 'no' rule: opposite condition (minion_nominated vs not minion_nominated).
st_tells_core(town_crier, TC, yes, T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(town_crier, RoleOrd),
    assigned(0, TC, town_crier),
    alive(TC, night(NightNum, 0, 0)),
    functioning(TC, T),
    minion_nominated(NightNum - 1).

% Functioning 'no' tell: deterministic when no minion nominated today.
% Can't merge with 'yes' rule: negated condition requires a separate rule in ASP.
st_tells_core(town_crier, TC, no, T) :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(town_crier, RoleOrd),
    assigned(0, TC, town_crier),
    alive(TC, night(NightNum, 0, 0)),
    functioning(TC, T),
    not minion_nominated(NightNum - 1).

% Not-functioning tell: ST freely chooses yes or no (choice rule).
% Can't merge with functioning rules: different semantics (choice vs deterministic).
{ st_tells_core(town_crier, TC, yes, T);
  st_tells_core(town_crier, TC, no, T) } = 1 :-
    T = night(NightNum, RoleOrd, 1),
    night_number(NightNum), NightNum > 1,
    other_night_role_order(town_crier, RoleOrd),
    assigned(0, TC, town_crier),
    alive(TC, night(NightNum, 0, 0)),
    not functioning(TC, T).
