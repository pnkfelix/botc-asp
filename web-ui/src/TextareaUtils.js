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
