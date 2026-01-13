// FFI bindings for clingo-wasm
import * as clingo from "clingo-wasm";

// Helper to stringify a foreign value for debugging
export const showForeignImpl = (value) => JSON.stringify(value, null, 2);

export const initImpl = (wasmUrl) => () => {
  // The worker runs in a blob context where relative URLs don't work.
  // Construct an absolute URL using the current page location.
  // This works both locally (/) and on GitHub Pages (/botc-asp/).
  const absoluteUrl = new URL(wasmUrl, window.location.href).href;
  return clingo.init(absoluteUrl);
};

export const runImpl = (program) => (numModels) => () =>
  clingo.run(program, numModels, ["--opt-mode=optN"]);

// Ground only - try to get the ground program without solving
// This tests whether clingo-wasm supports --mode=gringo
export const groundImpl = (program) => () =>
  clingo.run(program, 0, ["--mode=gringo", "--output=text"]);

export const restartImpl = (wasmUrl) => () => {
  // Terminate the worker and re-initialize
  const absoluteUrl = new URL(wasmUrl, window.location.href).href;
  return clingo.restart(absoluteUrl);
};

// Get the directory part of a path (e.g., "foo/bar/baz.lp" -> "foo/bar")
function getDirectory(path) {
  const lastSlash = path.lastIndexOf('/');
  return lastSlash >= 0 ? path.substring(0, lastSlash) : '';
}

// Join path segments, handling ".." and "."
function joinPath(base, relative) {
  if (!base) return relative;
  if (relative.startsWith('/')) return relative.substring(1);

  const baseParts = base.split('/').filter(p => p);
  const relativeParts = relative.split('/').filter(p => p);

  for (const part of relativeParts) {
    if (part === '..') {
      baseParts.pop();
    } else if (part !== '.') {
      baseParts.push(part);
    }
  }

  return baseParts.join('/');
}

// Resolve #include directives in a program string
// fileResolver is a function: filename -> content (or null if not found)
// currentFilePath is the path of the file being processed (for relative includes)
// Returns the program with all includes inlined
//
// Path resolution follows Clingo's behavior:
// 1. First try relative to the working directory (root)
// 2. If not found, try relative to the directory of the including file
export const resolveIncludesWithPathImpl = (program) => (currentFilePath) => (fileResolver) => {
  const seen = new Set();

  function resolve(content, filePath, depth = 0) {
    if (depth > 10) {
      return "% [max include depth exceeded]\n";
    }

    const currentDir = getDirectory(filePath);

    // Match #include "filename" at the start of a line (with optional leading whitespace).
    // Use multiline mode (m) so ^ matches start of each line.
    // (\s*) matches only whitespace before #include, which naturally excludes
    // commented-out includes (% is not whitespace) and malformed lines.
    return content.replace(/^(\s*)#include\s+"([^"]+)"\s*\./gm, (match, prefix, includePath) => {
      // Try to resolve the path:
      // 1. First, try as-is (relative to working directory / root)
      // 2. If not found, try relative to the current file's directory
      let resolvedPath = includePath;
      let fileContent = fileResolver(resolvedPath);

      if (fileContent === null && currentDir) {
        // Try relative to the current file's directory
        resolvedPath = joinPath(currentDir, includePath);
        fileContent = fileResolver(resolvedPath);
      }

      // Prevent infinite loops using the resolved path
      if (seen.has(resolvedPath)) {
        return `% [circular include skipped: ${resolvedPath}]\n`;
      }
      seen.add(resolvedPath);

      if (fileContent === null) {
        return `% [file not found: ${includePath} (tried: ${includePath}${currentDir ? ', ' + joinPath(currentDir, includePath) : ''})]\n`;
      }

      // Recursively resolve includes in the included file
      const resolved = resolve(fileContent, resolvedPath, depth + 1);
      return `% === Begin included: ${resolvedPath} ===\n${resolved}\n% === End included: ${resolvedPath} ===\n`;
    });
  }

  return resolve(program, currentFilePath);
};

// Legacy version without path tracking (for backwards compatibility)
// Assumes all includes are relative to root
export const resolveIncludesImpl = (program) => (fileResolver) => {
  return resolveIncludesWithPathImpl(program)("")(fileResolver);
};

// Extract script roles from a resolved ASP program
// Looks for patterns like:
//   tb_townsfolk(washerwoman; librarian; ...).
//   snv_outsider(butler; drunk; ...).
// Returns: { townsfolk: [...], outsiders: [...], minions: [...], demons: [...] }
//
// IMPORTANT: Only extracts from FACTS (lines without ':-'), not from rule bodies.
// This prevents false positives like extracting "X" from "demon(X) :- bmr_demon(X)."
// or "P, T" from "not protected_from_demon(P, T)."
export const extractScriptRolesImpl = (program) => {
  const result = {
    townsfolk: [],
    outsiders: [],
    minions: [],
    demons: []
  };

  // Match patterns like: prefix_category(role1; role2; ...).
  // The prefix can be anything (tb, snv, bmr, etc.)
  // Categories: townsfolk, outsider, minion, demon
  const patterns = [
    { category: 'townsfolk', regex: /\b\w+_townsfolk\s*\(\s*([^)]+)\s*\)\s*\./ },
    { category: 'outsiders', regex: /\b\w+_outsider\s*\(\s*([^)]+)\s*\)\s*\./ },
    { category: 'minions', regex: /\b\w+_minion\s*\(\s*([^)]+)\s*\)\s*\./ },
    { category: 'demons', regex: /\b\w+_demon\s*\(\s*([^)]+)\s*\)\s*\./ }
  ];

  // Process line by line to only extract from facts (no ':-')
  const lines = program.split('\n');

  for (const line of lines) {
    // Skip lines that are rules (contain ':-') - we only want facts
    if (line.includes(':-')) {
      continue;
    }

    // Skip comment lines
    const trimmedLine = line.trim();
    if (trimmedLine.startsWith('%')) {
      continue;
    }

    for (const { category, regex } of patterns) {
      const match = regex.exec(line);
      if (match) {
        // Split on semicolon and extract role names
        const rolesStr = match[1];
        const roles = rolesStr.split(';').map(r => r.trim()).filter(r => r.length > 0);
        // Add to the category, avoiding duplicates
        // Also filter out anything that looks like a variable (contains uppercase or is just '_')
        for (const role of roles) {
          const isValidRoleName = /^[a-z][a-z0-9_]*$/.test(role);
          if (isValidRoleName && !result[category].includes(role)) {
            result[category].push(role);
          }
        }
      }
    }
  }

  return result;
};
