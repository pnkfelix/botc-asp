# Claude Code Instructions

This file contains instructions for Claude Code when working on this repository.

## Environment Differences

Claude Code runs in two environments with different capabilities:

### Terminal Claude Code (Full Access)
- Full shell access including `gh` CLI for GitHub operations
- Can run local builds and tests
- Can install npm packages directly
- Can push branches and create PRs via CLI

### Web Claude Code Remote (CCR) - Restricted
- Limited shell access, some network restrictions
- **No `gh` CLI** - use GitHub API via `curl` or provide manual instructions
- Binary downloads (e.g., from GitHub releases) may be blocked
- **Cannot run local builds** - must rely on GitHub Actions for build testing
- Can push branches, but user must create/merge PRs manually

#### CCR Development Workflow

CCR operates in a collaborative loop with the user. Since CCR cannot test builds locally, the workflow depends on GitHub Actions triggered by PRs.

**PR Lifecycle States:**
1. **No PR open** - CCR can push commits to the branch, but no CI runs until a PR is created
2. **PR open** - Each push triggers GitHub Actions; CCR can monitor build status
3. **PR merged** - The site goes live on GitHub Pages; further changes need a new PR

**User Actions Required (CCR cannot do these):**
1. **Create a PR** - After CCR pushes initial changes, user creates PR to trigger CI
2. **Merge the PR** - After CI passes and user approves, user merges to deploy
3. **Inspect the live site** - User evaluates rendered behavior on GitHub Pages
4. **Inspect DOM** - User may need to check browser DevTools (can be difficult on mobile)

**CCR Responsibilities:**
- Track which PR (if any) is currently open for the working branch
- Push commits and notify user when a PR is needed
- Monitor GitHub Actions status after user creates PR:
  ```bash
  curl -s "https://api.github.com/repos/pnkfelix/botc-asp/actions/runs?per_page=5"
  ```
- After PR merge, remind user that a fresh PR is needed for subsequent changes
- Suggest debugging output in rendering code if DOM inspection is impractical

**State Tracking:**
CCR should maintain awareness of:
- Current branch name
- Whether a PR exists for this branch (and its number/URL if known)
- Whether the most recent PR was merged (requiring a new PR for further work)
- Last known CI status (passing/failing/pending)

**Debugging Strategies:**
When the user cannot easily inspect the DOM (e.g., on mobile):
- Add temporary console.log statements for key state
- Render debug info directly in the UI (e.g., a collapsible debug panel)
- Add data attributes to elements for easier identification
- These can be removed once the issue is resolved

## Build Commands

All web-ui commands run from the `web-ui/` directory:

```bash
cd web-ui

# Install dependencies (first time or after package.json changes)
npm install

# Full build (recommended)
npm run build

# Individual steps:
npm run embed-lp    # Embed *.lp files into EmbeddedPrograms.js
npm run build:purs  # Compile PureScript via spago
npm run bundle      # Bundle JS with esbuild
npm run copy:wasm   # Copy clingo WASM to dist/

# Development mode
npm run dev         # Watch + rebuild on changes
```

## Project Conventions

### File Organization
- `*.lp` files at repo root: Core game logic
- `tb_tests/*.lp`: Test cases for Trouble Brewing script
- `web-ui/src/*.purs`: PureScript source files
- `web-ui/src/*.js`: FFI JavaScript implementations

### ASP File Naming
- `sat_*.lp` - Test that SHOULD be satisfiable (valid scenario)
- `unsat_*.lp` - Test that SHOULD be unsatisfiable (invalid scenario)

### PureScript/JavaScript FFI Pattern
Each FFI module has a pair of files:
- `ModuleName.purs` - PureScript interface with `foreign import`
- `ModuleName.js` - JavaScript implementation with `export const`

Example:
```purescript
-- Clingo.purs
foreign import runClingoImpl :: EffectFn3 ...
```
```javascript
// Clingo.js
export const runClingoImpl = (program, models, callback) => ...
```

## Testing Approach

### ASP Logic Tests
Run clingo directly on test files:
```bash
clingo botc.lp tb.lp tb_tests/sat_chef_counts_evil_pairs.lp 0
# Should output: SATISFIABLE

clingo botc.lp tb.lp tb_tests/unsat_red_herring_on_demon.lp 0
# Should output: UNSATISFIABLE
```

### Web UI Testing
The CI builds and bundles the PureScript code. Type errors will fail the build.
Currently no automated UI tests - test manually via GitHub Pages deployment.

## Git Workflow

1. Create feature branch from `main`
2. Make changes and commit
3. Push branch: `git push -u origin <branch-name>`
4. Create PR (via `gh pr create` in terminal, or manually in web)
5. CI runs on PRs that touch `web-ui/**` or `*.lp` files
6. Merge after CI passes

### Commit Messages
- Use imperative mood: "Add feature" not "Added feature"
- Keep first line under 72 characters
- Reference issues if applicable

## Common Tasks

### Adding a New Role
1. Add role to appropriate category in `tb.lp` (or script file)
2. Define role abilities as ASP rules
3. Create `sat_*.lp` and `unsat_*.lp` tests
4. Run `npm run embed-lp` to update web UI

### Adding FFI Function
1. Add JavaScript implementation in `*.js` file
2. Add foreign import in corresponding `*.purs` file
3. Use `Effect` or `Aff` types for side effects
4. Rebuild: `npm run build:purs && npm run bundle`

### Debugging CI Failures
Check workflow runs:
```bash
# Terminal Claude Code:
gh run list --limit 5

# Web Claude Code:
curl -s "https://api.github.com/repos/pnkfelix/botc-asp/actions/runs?per_page=5"
```

View specific run:
```bash
curl -s "https://api.github.com/repos/pnkfelix/botc-asp/actions/runs/<run_id>/jobs"
```

## Known Limitations

- Clingo WASM is large (~2MB); initial page load may be slow
- Parser (`AspParser.purs`) runs on every render - acceptable for demo but inefficient
- No undo/redo in text editors
- Mobile textarea selection can be finicky
