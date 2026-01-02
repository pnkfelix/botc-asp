// Simple line-based diff implementation (no external library)
// Uses content-addressed matching to find resynchronization points

// Compute a simple line-by-line diff between two strings
// Returns an array of lines with their status: "added", "removed", or "unchanged"
export const computeLineDiffImpl = (oldStr) => (newStr) => {
  const oldLines = oldStr.split('\n');
  const newLines = newStr.split('\n');

  // Remove trailing empty line if present (from trailing newline)
  if (oldLines[oldLines.length - 1] === '') oldLines.pop();
  if (newLines[newLines.length - 1] === '') newLines.pop();

  const result = [];

  // Build a map of line content -> indices in new file (for resync)
  const newLineMap = new Map();
  for (let i = 0; i < newLines.length; i++) {
    const line = newLines[i];
    if (!newLineMap.has(line)) {
      newLineMap.set(line, []);
    }
    newLineMap.get(line).push(i);
  }

  // Track which new lines have been matched
  const matchedNew = new Set();

  // Simple two-pointer approach with lookahead for resync
  let oldIdx = 0;
  let newIdx = 0;

  while (oldIdx < oldLines.length || newIdx < newLines.length) {
    if (oldIdx >= oldLines.length) {
      // Remaining new lines are additions
      result.push({ line: newLines[newIdx], status: "added" });
      matchedNew.add(newIdx);
      newIdx++;
    } else if (newIdx >= newLines.length) {
      // Remaining old lines are removals
      result.push({ line: oldLines[oldIdx], status: "removed" });
      oldIdx++;
    } else if (oldLines[oldIdx] === newLines[newIdx]) {
      // Lines match - unchanged
      result.push({ line: oldLines[oldIdx], status: "unchanged" });
      matchedNew.add(newIdx);
      oldIdx++;
      newIdx++;
    } else {
      // Lines differ - try to find a resync point
      // Look ahead in new lines for current old line
      const oldLine = oldLines[oldIdx];
      const candidates = newLineMap.get(oldLine) || [];
      const resyncNew = candidates.find(idx => idx > newIdx && !matchedNew.has(idx));

      if (resyncNew !== undefined && resyncNew - newIdx <= 5) {
        // Found old line ahead in new - emit intervening new lines as added
        while (newIdx < resyncNew) {
          result.push({ line: newLines[newIdx], status: "added" });
          matchedNew.add(newIdx);
          newIdx++;
        }
        // Now they match
        result.push({ line: oldLines[oldIdx], status: "unchanged" });
        matchedNew.add(newIdx);
        oldIdx++;
        newIdx++;
      } else {
        // No good resync - current old line was removed
        result.push({ line: oldLines[oldIdx], status: "removed" });
        oldIdx++;
      }
    }
  }

  return result;
};

// Create a simple unified diff patch string
export const createPatchImpl = (fileName) => (oldStr) => (newStr) => {
  const diff = computeLineDiffImpl(oldStr)(newStr);
  const lines = ["--- " + fileName + " (original)", "+++ " + fileName + " (current)"];

  for (const d of diff) {
    if (d.status === "added") {
      lines.push("+ " + d.line);
    } else if (d.status === "removed") {
      lines.push("- " + d.line);
    } else {
      lines.push("  " + d.line);
    }
  }

  return lines.join('\n');
};
