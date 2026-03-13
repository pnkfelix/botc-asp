// FFI bindings for botc-zdd- (ZDD shadow solver)
//
// Wraps the GameObserver API for use from PureScript.
// The ESM bundle is built from the botc-zdd- git submodule.

import {
  Game,
  GameObserver,
  TROUBLE_BREWING,
  RoleType,
} from "../../botc-zdd-/dist/botc-zdd.esm.js";

// ---------------------------------------------------------------------------
// Role name mapping: ASP <-> ZDD
// ---------------------------------------------------------------------------

// ASP uses snake_case (fortune_teller), ZDD uses Title Case (Fortune Teller).
// Build a bidirectional lookup from the TROUBLE_BREWING constant.

const aspToZddName = new Map();
const zddToAspName = new Map();

for (const role of TROUBLE_BREWING.roles) {
  // "Fortune Teller" -> "fortune_teller"
  const aspName = role.name.toLowerCase().replace(/ /g, "_");
  aspToZddName.set(aspName, role.name);
  zddToAspName.set(role.name, aspName);
}

// Lookup a TROUBLE_BREWING Role object by ASP name
const tbRoleByAspName = new Map();
for (const role of TROUBLE_BREWING.roles) {
  const aspName = role.name.toLowerCase().replace(/ /g, "_");
  tbRoleByAspName.set(aspName, role);
}

// ---------------------------------------------------------------------------
// Script construction
// ---------------------------------------------------------------------------

// Build a Script from ASP-style role name arrays.
// scriptRoles: { townsfolk: string[], outsiders: string[], minions: string[], demons: string[] }
function buildScript(scriptRoles) {
  const roles = [];

  const addRoles = (names, roleType) => {
    for (const aspName of names) {
      const tbRole = tbRoleByAspName.get(aspName);
      if (tbRole) {
        roles.push(tbRole);
      } else {
        // Fallback for unknown roles: convert asp name to Title Case
        const zddName = aspName.split("_").map(w => w.charAt(0).toUpperCase() + w.slice(1)).join(" ");
        roles.push({ name: zddName, type: roleType });
      }
    }
  };

  addRoles(scriptRoles.townsfolk, RoleType.Townsfolk);
  addRoles(scriptRoles.outsiders, RoleType.Outsider);
  addRoles(scriptRoles.minions, RoleType.Minion);
  addRoles(scriptRoles.demons, RoleType.Demon);

  return { name: "custom", roles };
}

// ---------------------------------------------------------------------------
// Game setup + world count (synchronous, runs in the Effect monad)
// ---------------------------------------------------------------------------

// Create a Game, advance through distribution -> seat assignment -> night info,
// and return the world count.
//
// scriptRoles: { townsfolk: string[], outsiders: string[], minions: string[], demons: string[] }
// playerCount: number
// seatAssignment: Array of { seat: number, role: string }
//   (seat is 0-based, role is ASP-style lowercase e.g. "fortune_teller")
//
// Returns: { worldCount: number, error: string | null }
export const runZddShadowImpl = (scriptRoles) => (playerCount) => (seatAssignment) => () => {
  try {
    const script = buildScript(scriptRoles);

    if (script.roles.length === 0) {
      return { worldCount: 0, error: "No roles found in script" };
    }

    const game = new Game(script, playerCount);

    // Phase 1: Distribution (with modifiers for Baron etc.)
    game.buildDistribution(true);

    // Phase 2: Seat assignment
    // Map the assigned ASP role names to variable IDs in the script.
    // Each role in script.roles has varId = its index.
    const selectedVarIds = [];
    const roleCounts = new Map();
    for (const sa of seatAssignment) {
      const zddName = aspToZddName.get(sa.role) || sa.role;
      const key = zddName;
      roleCounts.set(key, (roleCounts.get(key) || 0) + 1);
    }

    const usedCounts = new Map();
    for (let i = 0; i < script.roles.length; i++) {
      const key = script.roles[i].name;
      const needed = roleCounts.get(key) || 0;
      const used = usedCounts.get(key) || 0;
      if (used < needed) {
        selectedVarIds.push(i);
        usedCounts.set(key, used + 1);
      }
    }

    if (selectedVarIds.length !== playerCount) {
      return {
        worldCount: 0,
        error: `Role count mismatch: selected ${selectedVarIds.length} of ${script.roles.length} roles for ${playerCount} players. Roles: ${JSON.stringify(seatAssignment.map(sa => sa.role))}`,
      };
    }

    game.buildSeatAssignment(selectedVarIds);

    // Phase 3: Night 1 info
    const seatMap = new Map();
    const malfunctioningSeats = new Set();
    for (const sa of seatAssignment) {
      const zddName = aspToZddName.get(sa.role) || sa.role;
      seatMap.set(sa.seat, zddName);

      if (zddName === "Drunk") {
        malfunctioningSeats.add(sa.seat);
      }
    }

    game.buildNightInfo(
      seatMap,
      malfunctioningSeats.size > 0 ? malfunctioningSeats : undefined,
    );

    const observer = new GameObserver(game);
    const worldCount = observer.worldCount();

    return { worldCount, error: "" };
  } catch (e) {
    return { worldCount: 0, error: e.message || String(e) };
  }
};
