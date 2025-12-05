% ===========================================================================
% Domain constraints ("type checking")
% These catch errors like assigning to non-existent players or invalid roles
% Include this file to get early UNSAT on domain errors.
% Exclude it when debugging to rule out type constraint issues.
% ===========================================================================
:- assigned(0, P, _), not player(P).
:- assigned(0, _, R), not role(R).
:- reminder_on(_, P, _), not player(P).
