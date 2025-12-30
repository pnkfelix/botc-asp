% Test file to verify #include directory traversal works
% This should include a role from a subdirectory

#include "roles/test/test_role.lp".
#include "players.lp".

#const player_count = 7.

% Assign the test role to a player
assigned(0, felix, test_role).

#show assigned/3.
