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
