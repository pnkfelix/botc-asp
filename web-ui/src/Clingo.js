// FFI bindings for clingo-wasm
import * as clingo from "clingo-wasm";

export const initImpl = (wasmUrl) => () => {
  // The worker runs in a blob context where relative URLs don't work.
  // Construct an absolute URL using the current page location.
  // This works both locally (/) and on GitHub Pages (/botc-asp/).
  const absoluteUrl = new URL(wasmUrl, window.location.href).href;
  return clingo.init(absoluteUrl);
};

export const runImpl = (program) => (numModels) => () =>
  clingo.run(program, numModels);

export const restartImpl = (wasmUrl) => () => {
  // Terminate the worker and re-initialize
  const absoluteUrl = new URL(wasmUrl, window.location.href).href;
  return clingo.restart(absoluteUrl);
};

// Resolve #include directives in a program string
// fileResolver is a function: filename -> content (or null if not found)
// Returns the program with all includes inlined
export const resolveIncludesImpl = (program) => (fileResolver) => {
  const seen = new Set();

  function resolve(content, depth = 0) {
    if (depth > 10) {
      return "% [max include depth exceeded]\n";
    }

    // Match #include "filename".
    return content.replace(/#include\s+"([^"]+)"\s*\./g, (match, filename) => {
      // Prevent infinite loops
      if (seen.has(filename)) {
        return `% [circular include skipped: ${filename}]\n`;
      }
      seen.add(filename);

      // Get file content from resolver
      const fileContent = fileResolver(filename);
      if (fileContent === null) {
        return `% [file not found: ${filename}]\n`;
      }

      // Recursively resolve includes in the included file
      const resolved = resolve(fileContent, depth + 1);
      return `% === Begin included: ${filename} ===\n${resolved}\n% === End included: ${filename} ===\n`;
    });
  }

  return resolve(program);
};
