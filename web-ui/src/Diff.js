// FFI bindings for diff library (jsdiff)
import * as Diff from "diff";

// Compute a line-by-line diff between two strings
// Returns an array of change objects: { value: string, added: boolean, removed: boolean }
export const diffLinesImpl = (oldStr) => (newStr) => {
  const changes = Diff.diffLines(oldStr, newStr, {
    ignoreWhitespace: false,
    newlineIsToken: false
  });

  // Convert to a format PureScript can easily work with
  // Each change has: value (the text), added (boolean), removed (boolean)
  // If neither added nor removed, it's unchanged context
  return changes.map(change => ({
    value: change.value,
    added: change.added || false,
    removed: change.removed || false
  }));
};

// Create a unified diff patch string (like git diff output)
export const createPatchImpl = (fileName) => (oldStr) => (newStr) => {
  return Diff.createPatch(fileName, oldStr, newStr, "original", "current");
};

// Compute a structured diff suitable for display
// Returns an array of lines with their status: "added", "removed", or "unchanged"
export const computeLineDiffImpl = (oldStr) => (newStr) => {
  const changes = Diff.diffLines(oldStr, newStr);
  const result = [];

  for (const change of changes) {
    // Split the change value into individual lines
    const lines = change.value.split('\n');
    // Remove the last empty string if the value ends with newline
    if (lines[lines.length - 1] === '') {
      lines.pop();
    }

    for (const line of lines) {
      if (change.added) {
        result.push({ line: line, status: "added" });
      } else if (change.removed) {
        result.push({ line: line, status: "removed" });
      } else {
        result.push({ line: line, status: "unchanged" });
      }
    }
  }

  return result;
};
