% Gossip
% Each day, you may make a public statement. Tonight, if it was true, a player dies.

other_night_substep(gossip, 1, st_places(gossip_dead)).

% The Gossip's statement is modeled as a boolean that can be true or false
% This is abstract - in actual play, the Gossip makes a statement and ST determines truth
% For modeling, we allow ST to choose whether the statement was true

% Gossip may make a statement each day (abstract - statement content not modeled)
{ gossip_statement_true(Gossip, N) } :-
    day_number(N),
    assigned(0, Gossip, gossip),
    alive(Gossip, day(N, 0)).

% If Gossip is functioning and made a true statement, someone dies that night
% ST chooses who dies
{ gossip_kills(P, N) : player(P), P != Gossip, alive(P, night(N+1, 0, 0)) } = 1 :-
    day_number(N),
    night_number(N+1),
    assigned(0, Gossip, gossip),
    gossip_statement_true(Gossip, N),
    functioning(Gossip, day(N, exec)).

% No kill if Gossip was drunk/poisoned
:- gossip_kills(_, N),
   assigned(0, Gossip, gossip),
   gossip_statement_true(Gossip, N),
   not functioning(Gossip, day(N, exec)).

% Place death token
reminder_on(gossip_dead, P, night(N+1, RoleOrd, 1)) :-
    other_night_role_order(gossip, RoleOrd),
    gossip_kills(P, N).

% Gossip kill causes death
dead_at_time(P, T) :- reminder_on(gossip_dead, P, T).
died(P, T) :- gossip_kills(P, N), final_night_time(N+1, T), alive(P, night(N+1, 0, 0)).
