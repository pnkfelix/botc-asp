#!/usr/bin/env node
/**
 * ASP Syntax Checker
 *
 * Detects anonymous variables (_) in term unification expressions,
 * which cause "syntax error, unexpected =" in Clingo.
 *
 * BAD:  T = night(N, _, _)
 * BAD:  T = day(M, _)
 * GOOD: time(T), T = night(N, R, S)
 * GOOD: time(T), T = day(M, Phase)
 *
 * The fix pattern is: ground T via time(T) first, then unify with
 * named variables instead of underscores.
 *
 * Usage: node scripts/check-asp-syntax.js [directory]
 *        Default directory is repo root (parent of web-ui)
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { join, relative } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Default to repo root (parent of web-ui)
const repoRoot = join(__dirname, '..', '..');

/**
 * Recursively find all .lp files in a directory
 */
function findLpFiles(dir, files = []) {
  const entries = readdirSync(dir);
  for (const entry of entries) {
    const fullPath = join(dir, entry);
    try {
      const stat = statSync(fullPath);
      if (stat.isDirectory()) {
        // Skip node_modules, .git, dist, output directories
        if (!['node_modules', '.git', 'dist', 'output', '.spago'].includes(entry)) {
          findLpFiles(fullPath, files);
        }
      } else if (entry.endsWith('.lp')) {
        files.push(fullPath);
      }
    } catch (e) {
      // Skip files we can't access
    }
  }
  return files;
}

/**
 * Check if a character position is inside a comment
 * ASP comments start with % and go to end of line
 */
function isInsideComment(line, charIndex) {
  const commentStart = line.indexOf('%');
  return commentStart !== -1 && charIndex >= commentStart;
}

/**
 * Find anonymous variables in term unification expressions
 * Pattern: = identifier(args) where args contains _
 *
 * Returns array of { line, column, match, context }
 */
function checkFile(filePath, content) {
  const warnings = [];
  const lines = content.split('\n');

  // Regex to match: = identifier( ... )
  // We capture the full term and check for _ inside
  // This handles cases like: T = night(N, _, _) or X = foo(a, b, _)
  const termUnificationRegex = /=\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)/g;

  for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
    const line = lines[lineIndex];
    let match;

    termUnificationRegex.lastIndex = 0;
    while ((match = termUnificationRegex.exec(line)) !== null) {
      const fullMatch = match[0];
      const termName = match[1];
      const args = match[2];
      const matchStart = match.index;

      // Skip if inside a comment
      if (isInsideComment(line, matchStart)) {
        continue;
      }

      // Check if args contain anonymous variable (_)
      // We need to find standalone _ (not part of a longer identifier)
      // Pattern: _ that is not preceded/followed by alphanumeric or _
      const anonVarRegex = /(?<![a-zA-Z0-9_])_(?![a-zA-Z0-9])/g;
      let anonMatch;

      while ((anonMatch = anonVarRegex.exec(args)) !== null) {
        // Calculate column of the underscore within the full line
        // matchStart is where "= term(" starts
        // We need to find where the args start (after the opening paren)
        const argsStartInMatch = fullMatch.indexOf('(') + 1;
        const underscoreColumn = matchStart + argsStartInMatch + anonMatch.index + 1; // +1 for 1-based column

        warnings.push({
          line: lineIndex + 1, // 1-based line number
          column: underscoreColumn,
          match: fullMatch,
          termName: termName,
          context: line.trim()
        });

        // Only report once per term unification (avoid duplicate warnings)
        break;
      }
    }
  }

  return warnings;
}

/**
 * Main function
 */
function main() {
  const targetDir = process.argv[2] || repoRoot;

  console.log(`Scanning for ASP syntax issues in: ${targetDir}\n`);

  const lpFiles = findLpFiles(targetDir);
  let totalWarnings = 0;
  const fileWarnings = [];

  for (const filePath of lpFiles) {
    try {
      const content = readFileSync(filePath, 'utf-8');
      const warnings = checkFile(filePath, content);

      if (warnings.length > 0) {
        const relativePath = relative(repoRoot, filePath);
        fileWarnings.push({ path: relativePath, warnings });
        totalWarnings += warnings.length;
      }
    } catch (e) {
      console.error(`Error reading ${filePath}: ${e.message}`);
    }
  }

  // Output warnings in filename:line:column format
  for (const { path, warnings } of fileWarnings) {
    for (const warning of warnings) {
      console.log(`${path}:${warning.line}:${warning.column}: warning: anonymous variable in term unification`);
      console.log(`  ${warning.context}`);
      console.log(`  Fix: Replace '_' with named variables (e.g., R, S, Phase)`);
      console.log();
    }
  }

  // Summary
  console.log('---');
  console.log(`Scanned ${lpFiles.length} .lp files`);
  if (totalWarnings > 0) {
    console.log(`Found ${totalWarnings} warning(s) in ${fileWarnings.length} file(s)`);
    console.log('\nTo fix: Use named variables instead of _ in term unification.');
    console.log('Example: time(T), T = night(N, R, S) instead of T = night(N, _, _)');
    process.exit(1);
  } else {
    console.log('No issues found!');
    process.exit(0);
  }
}

main();
