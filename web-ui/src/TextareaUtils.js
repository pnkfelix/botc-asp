// FFI for textarea manipulation

// Scroll a textarea to a specific line and select it
export const scrollToLineImpl = (elementId) => (lineNumber) => () => {
  const textarea = document.getElementById(elementId);
  if (!textarea) return;

  const text = textarea.value;
  const lines = text.split('\n');

  // Calculate character position of the start of the target line
  let charPos = 0;
  for (let i = 0; i < lineNumber - 1 && i < lines.length; i++) {
    charPos += lines[i].length + 1; // +1 for newline
  }

  // Calculate end of the line
  const lineLength = lines[lineNumber - 1] ? lines[lineNumber - 1].length : 0;
  const endPos = charPos + lineLength;

  // Focus the textarea
  textarea.focus();

  // Select the line
  textarea.setSelectionRange(charPos, endPos);

  // Calculate approximate scroll position
  // Get line height by creating a temporary element
  const style = window.getComputedStyle(textarea);
  const lineHeight = parseFloat(style.lineHeight) || parseFloat(style.fontSize) * 1.2;
  const paddingTop = parseFloat(style.paddingTop) || 0;

  // Scroll to put the line roughly in the middle of the visible area
  const textareaHeight = textarea.clientHeight;
  const targetScroll = (lineNumber - 1) * lineHeight - (textareaHeight / 2) + paddingTop;

  textarea.scrollTop = Math.max(0, targetScroll);
};

// Focus a textarea by ID
export const focusTextareaImpl = (elementId) => () => {
  const textarea = document.getElementById(elementId);
  if (textarea) {
    textarea.focus();
  }
};

// Scroll to and highlight text within a specific child element of a container
// childIndex specifies which child div to search within (0-indexed)
// Returns true if the text was found and highlighted, false otherwise
export const scrollToTextInChildImpl = (elementId) => (childIndex) => (searchText) => () => {
  const container = document.getElementById(elementId);
  if (!container || !searchText) return false;

  // Find all answer set divs (direct children of the inner wrapper div)
  const wrapper = container.firstElementChild;
  if (!wrapper) return false;

  const answerSetDivs = wrapper.children;
  if (childIndex < 0 || childIndex >= answerSetDivs.length) return false;

  // Get the specific answer set div and find its code element
  const targetDiv = answerSetDivs[childIndex];
  const codeElement = targetDiv.querySelector('code') || targetDiv;
  const fullText = codeElement.textContent || '';

  // Find the position of the search text
  const position = fullText.indexOf(searchText);
  if (position === -1) return false;

  // Clear any existing highlights
  const existingHighlight = document.getElementById('timeline-highlight');
  if (existingHighlight) {
    existingHighlight.remove();
  }

  // Create a highlight by wrapping the matched text
  const textContent = codeElement.textContent;
  const beforeText = textContent.substring(0, position);
  const matchedText = textContent.substring(position, position + searchText.length);
  const afterText = textContent.substring(position + searchText.length);

  // Use CSS highlight approach - update the code element with a marked span
  codeElement.innerHTML =
    escapeHtml(beforeText) +
    '<span id="timeline-highlight" style="background-color: #ffeb3b; padding: 2px 0; border-radius: 2px; animation: highlight-pulse 1s ease-in-out;">' +
    escapeHtml(matchedText) +
    '</span>' +
    escapeHtml(afterText);

  // Scroll the highlight into view
  const highlight = document.getElementById('timeline-highlight');
  if (highlight) {
    highlight.scrollIntoView({ behavior: 'smooth', block: 'center' });

    // Remove highlight after a delay
    setTimeout(() => {
      codeElement.textContent = textContent;
    }, 3000);
  }

  return true;
};

// Scroll to and highlight text within a scrollable element
// Returns true if the text was found and highlighted, false otherwise
export const scrollToTextImpl = (elementId) => (searchText) => () => {
  const container = document.getElementById(elementId);
  if (!container || !searchText) return false;

  // Find the code element inside
  const codeElement = container.querySelector('code') || container;
  const fullText = codeElement.textContent || '';

  // Find the position of the search text
  const position = fullText.indexOf(searchText);
  if (position === -1) return false;

  // Clear any existing highlights
  const existingHighlight = document.getElementById('timeline-highlight');
  if (existingHighlight) {
    existingHighlight.remove();
  }

  // Create a highlight by wrapping the matched text
  // We need to be careful with the DOM structure
  const textContent = codeElement.textContent;
  const beforeText = textContent.substring(0, position);
  const matchedText = textContent.substring(position, position + searchText.length);
  const afterText = textContent.substring(position + searchText.length);

  // Calculate approximate scroll position based on character position
  // Estimate characters per line based on container width and font
  const style = window.getComputedStyle(codeElement);
  const fontSize = parseFloat(style.fontSize) || 12;
  const charWidth = fontSize * 0.6; // Approximate monospace char width
  const containerWidth = container.clientWidth - 20; // Account for padding
  const charsPerLine = Math.floor(containerWidth / charWidth) || 80;
  const lineHeight = parseFloat(style.lineHeight) || fontSize * 1.2;

  // Count newlines and wrapped lines before the match
  let linesBefore = 0;
  let currentLineLength = 0;
  for (let i = 0; i < position; i++) {
    if (beforeText[i] === '\n') {
      linesBefore++;
      currentLineLength = 0;
    } else if (beforeText[i] === ' ') {
      currentLineLength++;
      // Check for word wrapping
      if (currentLineLength >= charsPerLine) {
        linesBefore++;
        currentLineLength = 0;
      }
    } else {
      currentLineLength++;
    }
  }

  // Scroll to put the line roughly in the middle
  const containerHeight = container.clientHeight;
  const targetScroll = (linesBefore * lineHeight) - (containerHeight / 2);
  container.scrollTop = Math.max(0, targetScroll);

  // Use CSS highlight approach - update the code element with a marked span
  codeElement.innerHTML =
    escapeHtml(beforeText) +
    '<span id="timeline-highlight" style="background-color: #ffeb3b; padding: 2px 0; border-radius: 2px; animation: highlight-pulse 1s ease-in-out;">' +
    escapeHtml(matchedText) +
    '</span>' +
    escapeHtml(afterText);

  // Scroll the highlight into view (more precise)
  const highlight = document.getElementById('timeline-highlight');
  if (highlight) {
    highlight.scrollIntoView({ behavior: 'smooth', block: 'center' });

    // Remove highlight after a delay
    setTimeout(() => {
      codeElement.textContent = textContent;
    }, 3000);
  }

  return true;
};

// Helper to escape HTML
function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

// Syntax highlighting for ASP/LP code
// Returns HTML with span elements for syntax highlighting
export const highlightLPImpl = (code) => () => {
  const lines = code.split('\n');
  const highlightedLines = lines.map(line => highlightLine(line));
  return highlightedLines.join('\n');
};

// Highlight a single line of LP code
function highlightLine(line) {
  // First, find if there's a comment (% not inside quotes)
  let commentStart = -1;
  let inString = false;

  for (let i = 0; i < line.length; i++) {
    const char = line[i];
    if (char === '"' && (i === 0 || line[i-1] !== '\\')) {
      inString = !inString;
    } else if (char === '%' && !inString) {
      commentStart = i;
      break;
    }
  }

  let codePart = commentStart >= 0 ? line.substring(0, commentStart) : line;
  let commentPart = commentStart >= 0 ? line.substring(commentStart) : '';

  // Highlight the code part
  let highlighted = highlightCodePart(codePart);

  // Add comment with highlighting
  if (commentPart) {
    highlighted += '<span class="lp-comment">' + escapeHtml(commentPart) + '</span>';
  }

  return highlighted;
}

// Highlight non-comment code
function highlightCodePart(code) {
  if (!code) return '';

  // Tokenize and highlight
  // Order matters: we process in a way that avoids double-highlighting
  let result = '';
  let i = 0;

  while (i < code.length) {
    // Check for string literals
    if (code[i] === '"') {
      let end = i + 1;
      while (end < code.length && (code[end] !== '"' || code[end-1] === '\\')) {
        end++;
      }
      if (end < code.length) end++; // include closing quote
      result += '<span class="lp-string">' + escapeHtml(code.substring(i, end)) + '</span>';
      i = end;
      continue;
    }

    // Check for directives (#include, #const, etc.)
    if (code[i] === '#') {
      const match = code.substring(i).match(/^#[a-z_]+/);
      if (match) {
        result += '<span class="lp-directive">' + escapeHtml(match[0]) + '</span>';
        i += match[0].length;
        continue;
      }
    }

    // Check for rule operators
    if (code.substring(i, i + 2) === ':-') {
      result += '<span class="lp-keyword">' + ':-' + '</span>';
      i += 2;
      continue;
    }
    if (code.substring(i, i + 2) === ':~') {
      result += '<span class="lp-keyword">' + ':~' + '</span>';
      i += 2;
      continue;
    }

    // Check for word-based tokens (keywords and variables)
    const wordMatch = code.substring(i).match(/^[A-Za-z_][A-Za-z0-9_]*/);
    if (wordMatch) {
      const word = wordMatch[0];
      if (word === 'not') {
        result += '<span class="lp-keyword">' + escapeHtml(word) + '</span>';
      } else if (/^[A-Z_]/.test(word)) {
        // Variable: starts with uppercase or underscore
        result += '<span class="lp-variable">' + escapeHtml(word) + '</span>';
      } else {
        // Regular predicate/atom
        result += escapeHtml(word);
      }
      i += word.length;
      continue;
    }

    // Default: escape and add the character
    result += escapeHtml(code[i]);
    i++;
  }

  return result;
}

// Synchronize scroll position from textarea to highlight overlay
export const syncScrollImpl = (textareaId) => (overlayId) => () => {
  const textarea = document.getElementById(textareaId);
  const overlay = document.getElementById(overlayId);
  if (textarea && overlay) {
    overlay.scrollTop = textarea.scrollTop;
    overlay.scrollLeft = textarea.scrollLeft;
  }
};

// Update the syntax highlight overlay with highlighted HTML
// Also sets up scroll synchronization if not already done
export const updateHighlightOverlayImpl = (textareaId) => (overlayId) => (code) => () => {
  const overlay = document.getElementById(overlayId);
  const textarea = document.getElementById(textareaId);
  if (!overlay) return;

  // Generate highlighted HTML
  const lines = code.split('\n');
  const highlightedLines = lines.map(line => highlightLine(line));
  // Add an extra newline at the end to match textarea scrolling behavior
  overlay.innerHTML = highlightedLines.join('\n') + '\n';

  // Sync scroll position immediately after updating content
  // This is critical because innerHTML update can reset scroll position
  if (textarea) {
    overlay.scrollTop = textarea.scrollTop;
    overlay.scrollLeft = textarea.scrollLeft;
  }

  // Set up scroll sync if not already done
  if (textarea && !textarea.dataset.scrollSyncSetup) {
    textarea.dataset.scrollSyncSetup = 'true';
    textarea.addEventListener('scroll', () => {
      overlay.scrollTop = textarea.scrollTop;
      overlay.scrollLeft = textarea.scrollLeft;
    });
  }
};

// Copy text to clipboard
// Returns true if successful, false otherwise
export const copyToClipboardImpl = (text) => () => {
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(text).catch(err => {
      console.error('Failed to copy to clipboard:', err);
    });
    return true;
  } else {
    // Fallback for older browsers
    const textarea = document.createElement('textarea');
    textarea.value = text;
    textarea.style.position = 'fixed';
    textarea.style.left = '-9999px';
    document.body.appendChild(textarea);
    textarea.select();
    try {
      document.execCommand('copy');
      document.body.removeChild(textarea);
      return true;
    } catch (err) {
      console.error('Fallback copy failed:', err);
      document.body.removeChild(textarea);
      return false;
    }
  }
};

// Copy text to clipboard and stop event propagation
// Takes event and text, returns true if copy successful
export const copyToClipboardWithEventImpl = (event) => (text) => () => {
  // Stop the event from bubbling up (e.g., to prevent selecting the answer set)
  if (event && event.stopPropagation) {
    event.stopPropagation();
  }
  // Use the regular copy function
  return copyToClipboardImpl(text)();
};

// Detect if the cursor is on an #include directive and return the file path
// Returns null if not on an include, or the file path string if found
export const getIncludeAtCursorImpl = (elementId) => () => {
  const textarea = document.getElementById(elementId);
  if (!textarea) return null;

  const text = textarea.value;
  const cursorPos = textarea.selectionStart;

  // Find the line containing the cursor
  const lines = text.split('\n');
  let charCount = 0;
  let currentLine = '';

  for (let i = 0; i < lines.length; i++) {
    const lineEnd = charCount + lines[i].length;
    if (cursorPos >= charCount && cursorPos <= lineEnd) {
      currentLine = lines[i];
      break;
    }
    charCount += lines[i].length + 1; // +1 for newline
  }

  // Check if the line is an #include directive
  // Pattern: #include "filename".
  const includeMatch = currentLine.match(/#include\s+"([^"]+)"\s*\./);
  if (includeMatch) {
    return includeMatch[1]; // Return the filename
  }

  return null;
};
