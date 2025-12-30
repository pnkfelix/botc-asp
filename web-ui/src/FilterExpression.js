// FilterExpression.js
// A simple boolean expression parser and evaluator for filtering atoms
//
// Syntax:
//   term       = literal substring to match (alphanumeric, underscore)
//   !term      = negation (also -term)
//   a | b      = OR
//   a b        = AND (space-separated)
//   (expr)     = grouping
//
// Examples:
//   assigned           - atoms containing "assigned"
//   !time              - atoms NOT containing "time"
//   assigned | tells   - atoms containing "assigned" OR "tells"
//   alice chef         - atoms containing both "alice" AND "chef"
//   !(time | chair)    - atoms NOT containing "time" or "chair"

// Token types
const TOKEN_LITERAL = 'LITERAL';
const TOKEN_NOT = 'NOT';
const TOKEN_OR = 'OR';
const TOKEN_LPAREN = 'LPAREN';
const TOKEN_RPAREN = 'RPAREN';
const TOKEN_EOF = 'EOF';

// Tokenize input string
function tokenize(input) {
  const tokens = [];
  let i = 0;

  while (i < input.length) {
    const ch = input[i];

    // Skip whitespace (but whitespace between terms means AND)
    if (ch === ' ' || ch === '\t' || ch === '\n') {
      i++;
      continue;
    }

    // Single-char tokens
    if (ch === '(' ) {
      tokens.push({ type: TOKEN_LPAREN, value: '(' });
      i++;
      continue;
    }
    if (ch === ')') {
      tokens.push({ type: TOKEN_RPAREN, value: ')' });
      i++;
      continue;
    }
    if (ch === '|') {
      tokens.push({ type: TOKEN_OR, value: '|' });
      i++;
      continue;
    }
    if (ch === '!' || ch === '-') {
      tokens.push({ type: TOKEN_NOT, value: ch });
      i++;
      continue;
    }

    // Literal: alphanumeric, underscore, or quoted string
    if (ch === '"' || ch === "'") {
      // Quoted string
      const quote = ch;
      i++;
      let literal = '';
      while (i < input.length && input[i] !== quote) {
        literal += input[i];
        i++;
      }
      if (i < input.length) i++; // skip closing quote
      tokens.push({ type: TOKEN_LITERAL, value: literal });
      continue;
    }

    // Unquoted literal: alphanumeric and underscore
    if (/[a-zA-Z0-9_]/.test(ch)) {
      let literal = '';
      while (i < input.length && /[a-zA-Z0-9_]/.test(input[i])) {
        literal += input[i];
        i++;
      }
      tokens.push({ type: TOKEN_LITERAL, value: literal });
      continue;
    }

    // Unknown character, skip
    i++;
  }

  tokens.push({ type: TOKEN_EOF });
  return tokens;
}

// Parser
// Grammar:
//   expr     = orExpr
//   orExpr   = andExpr (('|') andExpr)*
//   andExpr  = unary+
//   unary    = ('!' | '-')* primary
//   primary  = LITERAL | '(' expr ')'

function parse(input) {
  const tokens = tokenize(input);
  let pos = 0;

  function peek() {
    return tokens[pos];
  }

  function consume() {
    return tokens[pos++];
  }

  function parseExpr() {
    return parseOrExpr();
  }

  function parseOrExpr() {
    let left = parseAndExpr();
    if (!left) return null;

    while (peek().type === TOKEN_OR) {
      consume(); // skip '|'
      const right = parseAndExpr();
      if (!right) return left;
      left = { type: 'OR', left, right };
    }

    return left;
  }

  function parseAndExpr() {
    const terms = [];

    while (true) {
      const term = parseUnary();
      if (!term) break;
      terms.push(term);

      // Check if next token could start another term (for implicit AND)
      const next = peek();
      if (next.type !== TOKEN_NOT &&
          next.type !== TOKEN_LPAREN &&
          next.type !== TOKEN_LITERAL) {
        break;
      }
    }

    if (terms.length === 0) return null;
    if (terms.length === 1) return terms[0];

    // Build left-associative AND tree
    let result = terms[0];
    for (let i = 1; i < terms.length; i++) {
      result = { type: 'AND', left: result, right: terms[i] };
    }
    return result;
  }

  function parseUnary() {
    if (peek().type === TOKEN_NOT) {
      consume();
      const operand = parseUnary();
      if (!operand) return null;
      return { type: 'NOT', operand };
    }
    return parsePrimary();
  }

  function parsePrimary() {
    const token = peek();

    if (token.type === TOKEN_LITERAL) {
      consume();
      return { type: 'LITERAL', value: token.value.toLowerCase() };
    }

    if (token.type === TOKEN_LPAREN) {
      consume();
      const expr = parseExpr();
      if (peek().type === TOKEN_RPAREN) {
        consume();
      }
      return expr;
    }

    return null;
  }

  return parseExpr();
}

// Evaluate AST against an atom string
function evaluate(ast, atom) {
  if (!ast) return true; // empty filter matches everything

  const atomLower = atom.toLowerCase();

  switch (ast.type) {
    case 'LITERAL':
      return atomLower.includes(ast.value);
    case 'NOT':
      return !evaluate(ast.operand, atom);
    case 'AND':
      return evaluate(ast.left, atom) && evaluate(ast.right, atom);
    case 'OR':
      return evaluate(ast.left, atom) || evaluate(ast.right, atom);
    default:
      return true;
  }
}

// Parse a filter expression and return a function that tests atoms
export const compileFilterImpl = (expr) => {
  const trimmed = expr.trim();
  if (trimmed === '') {
    // Empty filter matches everything
    return (_atom) => true;
  }

  try {
    const ast = parse(trimmed);
    if (!ast) {
      return (_atom) => true;
    }
    return (atom) => evaluate(ast, atom);
  } catch (e) {
    // On parse error, match nothing
    return (_atom) => false;
  }
};

// Filter an array of atoms using a filter expression
export const filterAtomsImpl = (expr, atoms) => {
  const matcher = compileFilterImpl(expr);
  return atoms.filter(matcher);
};

// Check if a filter expression is valid (returns error message or empty string)
export const validateFilterImpl = (expr) => {
  const trimmed = expr.trim();
  if (trimmed === '') return '';

  try {
    const ast = parse(trimmed);
    if (!ast) return 'Empty or invalid expression';
    return '';
  } catch (e) {
    return e.message || 'Parse error';
  }
};

// Test if an atom matches a compiled filter function
export const testFilterImpl = (filterFn, atom) => {
  return filterFn(atom);
};
