% Fool
% The first time you die, you don't.

% Helper: Fool specifically would die (filters would_die to just the Fool player)
fool_would_die(Fool, T) :-
    assigned(0, Fool, fool),
    would_die(Fool, T).

% would_die from Imp kill (separate rules per demon: each has a different kill predicate)
would_die(P, T) :- imp_kill_succeeds(P, T), not protected_from_demon(P, T).
% would_die from Zombuul kill
would_die(P, T) :- zombuul_kill_succeeds(P, T), not protected_from_demon(P, T).
% would_die from Pukka kill
would_die(P, T) :- pukka_kill_succeeds(P, T), not protected_from_demon(P, T).
% would_die from Shabaloth kill
would_die(P, T) :- shabaloth_kill_succeeds(P, T), not protected_from_demon(P, T).
% would_die from Po kill
would_die(P, T) :- po_kill_succeeds(P, T), not protected_from_demon(P, T).
% would_die from execution (separate from demon kills: different time structure)
would_die(P, T) :- executed(P, N), T = day(N, exec).

% Helper: identifies the chronologically first death event for the Fool (one-shot ability trigger)
fool_first_death(Fool, T) :-
    fool_would_die(Fool, T),
    not fool_died_before(Fool, T).

% Helper: true if Fool would have died at any earlier time T1 (prevents re-triggering one-shot)
fool_died_before(Fool, T2) :-
    fool_would_die(Fool, T1),
    time(T1), time(T2),
    next_transitive(T1, T2).

% Transitive time ordering base case: immediate successor
next_transitive(T1, T2) :- next(T1, T2).
% Transitive time ordering recursive case: can't fold into base case (recursion requires two rules)
next_transitive(T1, T3) :- next(T1, T2), next_transitive(T2, T3).

% Fool survives their first death if functioning at that moment (core ability resolution)
fool_survives(Fool, T) :-
    fool_first_death(Fool, T),
    functioning(Fool, T).

% Place reminder token when Fool survives (tracks that the one-shot has been used)
reminder_on(fool_used, Fool, T) :-
    fool_survives(Fool, T).

% Token inertia: fool_used persists across all subsequent time points
reminder_on(fool_used, Fool, T2) :-
    reminder_on(fool_used, Fool, T1),
    next(T1, T2).

% Constraint: Fool cannot be recorded as died at a time they survived (prevents contradictory state)
:- fool_survives(Fool, T), died(Fool, T).
