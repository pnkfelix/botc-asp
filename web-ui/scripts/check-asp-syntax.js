#!/usr/bin/env node
/**
 * ASP Syntax Checker
 *
 * Detects problematic anonymous variables (_) that cause Clingo errors:
 *
 * 1. Anonymous variables in term unification WITHOUT prior grounding:
 *    BAD:  T = night(N, _, _)           -- T not grounded
 *    GOOD: time(T), T = night(N, _, _)  -- T grounded by time(T) first
 *
 * 2. Anonymous variables in term constructions in rule heads:
 *    BAD:  game_over(night(N, _, _)) :- ...
 *    GOOD: game_over(T) :- ..., time(T), T = night(N, R, S).
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
 * Extract the rule head (everything before :-) from a rule
 */
function extractRuleHead(ruleText) {
  const headMatch = ruleText.match(/^([^:]*?)(?::-|$)/);
  return headMatch ? headMatch[1] : '';
}

/**
 * Extract the rule body (everything after :-) from a rule
 */
function extractRuleBody(ruleText) {
  const bodyMatch = ruleText.match(/:-(.*)$/s);
  return bodyMatch ? bodyMatch[1] : '';
}

/**
 * Check if a variable is grounded by time(Var) before its use in the body
 * @param {string} body - The rule body text
 * @param {string} varName - The variable name to check
 * @param {number} usePosition - Position where the variable is used in unification
 */
function isGroundedByTime(body, varName, usePosition) {
  // Look for time(VarName) or time(VarName, ...) before the use position
  // The pattern should match: time(T) where T is our variable
  const timePattern = new RegExp(`time\\s*\\(\\s*${varName}\\s*[,)]`, 'g');
  let match;
  while ((match = timePattern.exec(body)) !== null) {
    if (match.index < usePosition) {
      return true;
    }
  }
  return false;
}

/**
 * Check for anonymous variables in nested terms (for rule heads)
 * Pattern: identifier(... identifier2(..._...) ...)
 */
function findAnonInNestedTerms(text, startColumn) {
  const warnings = [];

  // Match nested term patterns like: pred(term(a, _, b))
  // We look for: identifier(...) where the contents have identifier(..._...)
  const nestedTermRegex = /([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)/g;
  let match;

  while ((match = nestedTermRegex.exec(text)) !== null) {
    const termName = match[1];
    const args = match[2];
    const matchStart = match.index;

    // Check if args contain a nested term with anonymous variable
    // Pattern: identifier(args_with_underscore)
    const innerTermRegex = /([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*_[^)]*)\)/g;
    let innerMatch;

    while ((innerMatch = innerTermRegex.exec(args)) !== null) {
      const innerTermName = innerMatch[1];
      const innerArgs = innerMatch[2];

      // Check for standalone underscore
      const anonVarRegex = /(?<![a-zA-Z0-9_])_(?![a-zA-Z0-9])/g;
      if (anonVarRegex.test(innerArgs)) {
        // Find the position of the underscore
        anonVarRegex.lastIndex = 0;
        const anonMatch = anonVarRegex.exec(innerArgs);
        if (anonMatch) {
          const column = startColumn + matchStart + match[0].indexOf(innerMatch[0]) +
                        innerMatch[0].indexOf('(') + 1 + anonMatch.index + 1;
          warnings.push({
            column,
            termName: innerTermName,
            fullMatch: innerMatch[0]
          });
        }
      }
    }
  }

  return warnings;
}

/**
 * Check a file for ASP syntax issues
 */
function checkFile(filePath, content) {
  const warnings = [];
  const lines = content.split('\n');

  // First, reconstruct rules (handling multi-line rules)
  // A rule ends with a period not inside parentheses
  let currentRule = '';
  let ruleStartLine = 0;
  const rules = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // Remove comments for rule parsing
    const commentIdx = line.indexOf('%');
    const codePart = commentIdx >= 0 ? line.substring(0, commentIdx) : line;

    if (currentRule === '') {
      ruleStartLine = i;
    }
    currentRule += codePart + '\n';

    // Check if rule ends (period at end, not inside parens)
    // Simple heuristic: line ends with . and parens are balanced
    const trimmed = codePart.trim();
    if (trimmed.endsWith('.')) {
      let parenDepth = 0;
      for (const ch of currentRule) {
        if (ch === '(') parenDepth++;
        else if (ch === ')') parenDepth--;
      }
      if (parenDepth === 0) {
        rules.push({ text: currentRule, startLine: ruleStartLine });
        currentRule = '';
      }
    }
  }

  // Now check each rule
  for (const rule of rules) {
    const ruleText = rule.text;
    const head = extractRuleHead(ruleText);
    const body = extractRuleBody(ruleText);

    // Check 1: Anonymous variables in nested terms in rule head
    const headWarnings = findAnonInNestedTerms(head, 0);
    for (const hw of headWarnings) {
      // Find the actual line number within the rule
      const beforeMatch = ruleText.substring(0, ruleText.indexOf(hw.fullMatch));
      const lineOffset = (beforeMatch.match(/\n/g) || []).length;
      const lineNum = rule.startLine + lineOffset + 1;
      const lineText = lines[rule.startLine + lineOffset];

      warnings.push({
        line: lineNum,
        column: hw.column,
        type: 'head',
        match: hw.fullMatch,
        termName: hw.termName,
        context: lineText.trim()
      });
    }

    // Check 2: Ungrounded term unification in body
    // Pattern: Var = term(..._...) where Var is not grounded by time(Var) first
    const termUnificationRegex = /([A-Z][a-zA-Z0-9_]*)\s*=\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)/g;
    let match;

    // Calculate where body starts in the rule text
    const bodyStartIdx = ruleText.indexOf(':-');
    if (bodyStartIdx === -1) continue; // No body, skip

    while ((match = termUnificationRegex.exec(body)) !== null) {
      const varName = match[1];
      const termName = match[2];
      const args = match[3];
      const matchPosInBody = match.index;

      // Check if args contain anonymous variable
      const anonVarRegex = /(?<![a-zA-Z0-9_])_(?![a-zA-Z0-9])/g;
      if (!anonVarRegex.test(args)) continue;

      // Check if variable is grounded by time() before this use
      if (isGroundedByTime(body, varName, matchPosInBody)) {
        continue; // Grounded, no problem
      }

      // Also check for grounding patterns in conditionals like: time(T2), T2 = ...
      // This is a bit tricky with conditionals (:), but let's do a simple check
      // Check if this match is inside a conditional that has its own time() grounding
      const precedingBody = body.substring(0, matchPosInBody);

      // Look for pattern: time(Var) ... Var = term (with Var being our variable)
      // within the same "clause" (between commas/semicolons at same level)

      // Find the actual line and column
      const fullPosInRule = bodyStartIdx + 2 + matchPosInBody; // +2 for ":-"
      const beforeMatch = ruleText.substring(0, fullPosInRule);
      const lineOffset = (beforeMatch.match(/\n/g) || []).length;
      const lineNum = rule.startLine + lineOffset + 1;
      const lineText = lines[rule.startLine + lineOffset];

      // Calculate column within the line
      const lastNewline = beforeMatch.lastIndexOf('\n');
      const colInLine = lastNewline >= 0 ? fullPosInRule - lastNewline : fullPosInRule + 1;

      // Find position of underscore within the match
      anonVarRegex.lastIndex = 0;
      const anonMatch = anonVarRegex.exec(args);
      const underscoreOffset = match[0].indexOf('(') + 1 + anonMatch.index;

      warnings.push({
        line: lineNum,
        column: colInLine + underscoreOffset,
        type: 'ungrounded',
        match: match[0],
        varName: varName,
        termName: termName,
        context: lineText.trim()
      });
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
      const typeDesc = warning.type === 'head'
        ? 'anonymous variable in term construction in rule head'
        : 'anonymous variable in ungrounded term unification';
      console.log(`${path}:${warning.line}:${warning.column}: warning: ${typeDesc}`);
      console.log(`  ${warning.context}`);
      if (warning.type === 'head') {
        console.log(`  Fix: Move term to body with time() grounding: time(T), T = ${warning.termName}(...)`);
      } else {
        console.log(`  Fix: Add time(${warning.varName}) before the unification, or use named variables`);
      }
      console.log();
    }
  }

  // Summary
  console.log('---');
  console.log(`Scanned ${lpFiles.length} .lp files`);
  if (totalWarnings > 0) {
    console.log(`Found ${totalWarnings} warning(s) in ${fileWarnings.length} file(s)`);
    process.exit(1);
  } else {
    console.log('No issues found!');
    process.exit(0);
  }
}

main();
