% Slayer

% The shooter must actually be the Slayer (can't be combined: validates a different field than the next two)
:- slayer_shot(Slayer, _, N), not character_assignment_state_at_time(day(N, 0), Slayer, slayer).
% The Slayer must be alive to use their ability
:- slayer_shot(Slayer, _, N), not alive(Slayer, day(N, 0)).
% The target must be alive (can't shoot a dead player)
:- slayer_shot(_, Target, N), not alive(Target, day(N, 0)).

% Using the shot places the "no ability" token, marking it as spent
reminder_on(sla_no_ability, Slayer, day(N, 0)) :- slayer_shot(Slayer, _, N).

% One-shot: can't fire if the sla_no_ability token already exists from a previous day
:- slayer_shot(Slayer, _, N),
   reminder_on(sla_no_ability, Slayer, T),
   T = day(N2, _), N2 < N.

% The shot kills the target only if the Slayer is functioning AND the target registers as demon
died(Target, day(N, 0)) :-
    slayer_shot(Slayer, Target, N),
    functioning(Slayer, day(N, 0)),
    registers_as(Target, demon, day(N, 0)).
