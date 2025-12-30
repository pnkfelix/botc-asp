// ElementHitTest.js - FFI for detecting player elements under touch/mouse position

export const findPlayerAtPointImpl = (x) => (y) => {
  const element = document.elementFromPoint(x, y);
  if (!element) return null;

  // Walk up the DOM tree looking for data-player attribute
  let current = element;
  while (current && current !== document.body) {
    const playerName = current.getAttribute('data-player');
    if (playerName) {
      return playerName;
    }
    current = current.parentElement;
  }

  return null;
};
