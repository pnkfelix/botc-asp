% Test role to verify directory traversal in #include
% This file is in roles/test/ and should be able to include ../../botc.lp

#include "../../botc.lp".

% Define a simple test role
tb_townsfolk(test_role).

% Test role does nothing (just for testing includes)
