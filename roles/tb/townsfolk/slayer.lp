% Slayer

:- slayer_shot(Slayer, _, _), not assigned(0, Slayer, slayer).
:- slayer_shot(Slayer, _, N), not alive(Slayer, day(N, 0)).
:- slayer_shot(_, Target, N), not alive(Target, day(N, 0)).

reminder_on(sla_no_ability, Slayer, day(N, 0)) :- slayer_shot(Slayer, _, N).

:- slayer_shot(Slayer, _, N),
   reminder_on(sla_no_ability, Slayer, T),
   T = day(N2, _), N2 < N.

died(Target, day(N, 0)) :-
    slayer_shot(Slayer, Target, N),
    functioning(Slayer, day(N, 0)),
    registers_as(Target, demon, day(N, 0)).
