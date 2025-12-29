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

    // Match #include "filename".
    return content.replace(/#include\s+"([^"]+)"\s*\./g, (match, includePath) => {
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
