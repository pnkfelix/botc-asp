% @script-name: Sects & Violets
% @script-id: snv

#include "roles/snv/base.lp".

% Townsfolk
#include "roles/snv/townsfolk/clockmaker.lp".
#include "roles/snv/townsfolk/dreamer.lp".
#include "roles/snv/townsfolk/snake_charmer.lp".
#include "roles/snv/townsfolk/mathematician.lp".
#include "roles/snv/townsfolk/flowergirl.lp".
#include "roles/snv/townsfolk/town_crier.lp".
#include "roles/snv/townsfolk/oracle.lp".
#include "roles/snv/townsfolk/savant.lp".
#include "roles/snv/townsfolk/seamstress.lp".
#include "roles/snv/townsfolk/philosopher.lp".
#include "roles/snv/townsfolk/artist.lp".
#include "roles/snv/townsfolk/juggler.lp".
#include "roles/snv/townsfolk/sage.lp".

% Outsiders
#include "roles/snv/outsiders/mutant.lp".
#include "roles/snv/outsiders/sweetheart.lp".
#include "roles/snv/outsiders/barber.lp".
#include "roles/snv/outsiders/klutz.lp".

% Minions
#include "roles/snv/minions/evil_twin.lp".
#include "roles/snv/minions/witch.lp".
#include "roles/snv/minions/cerenovus.lp".
#include "roles/snv/minions/pit_hag.lp".

% Demons
#include "roles/snv/demons/fang_gu.lp".
#include "roles/snv/demons/vigormortis.lp".
#include "roles/snv/demons/vortox.lp".
#include "roles/snv/demons/no_dashii.lp".

% Game end conditions (depends on demon/minion logic above)
#include "game_end.lp".
