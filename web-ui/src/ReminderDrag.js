// ReminderDrag.js - Pure JavaScript drag handler for reminder tokens using Pointer Events
// This bypasses Halogen's event system which has issues with pointer/touch events

let dragState = null;
let floatingElement = null;
let initialized = false;

// Initialize drag handling for the grimoire
// Sets up event listeners; drops are communicated via CustomEvent 'reminderDropped'
export const initDragHandlerImpl = () => {
  if (initialized) return;
  initialized = true;

  // Use event delegation on the document for maximum reliability
  document.addEventListener('pointerdown', handlePointerDown, { passive: false });
  document.addEventListener('pointermove', handlePointerMove, { passive: false });
  document.addEventListener('pointerup', handlePointerUp, { passive: false });
  document.addEventListener('pointercancel', handlePointerCancel, { passive: false });
};

// Clean up event listeners
export const cleanupDragHandlerImpl = () => {
  if (!initialized) return;
  initialized = false;

  document.removeEventListener('pointerdown', handlePointerDown);
  document.removeEventListener('pointermove', handlePointerMove);
  document.removeEventListener('pointerup', handlePointerUp);
  document.removeEventListener('pointercancel', handlePointerCancel);
};

// Subscribe to reminderDropped events
// Returns an unsubscribe function
export const subscribeToDropsImpl = (callback) => () => {
  const handler = (e) => {
    const { token, fromPlayer, toPlayer, time } = e.detail;
    callback({ token, fromPlayer, toPlayer, time })();
  };
  document.addEventListener('reminderDropped', handler);
  // Return unsubscribe function
  return () => {
    document.removeEventListener('reminderDropped', handler);
  };
};

function handlePointerDown(e) {
  // Check if we clicked on a draggable reminder token
  const reminderEl = e.target.closest('[data-reminder-token]');
  if (!reminderEl) return;

  // Prevent default to avoid text selection, scrolling, etc.
  e.preventDefault();

  // Capture pointer to this element for reliable tracking
  reminderEl.setPointerCapture(e.pointerId);

  // Extract reminder data from attributes
  const token = reminderEl.getAttribute('data-reminder-token');
  const player = reminderEl.getAttribute('data-reminder-player');
  const time = reminderEl.getAttribute('data-reminder-time');
  const color = reminderEl.getAttribute('data-reminder-color') || '#666';
  const abbrev = reminderEl.getAttribute('data-reminder-abbrev') || token.slice(0, 3).toUpperCase();

  // Start drag state
  dragState = {
    token,
    fromPlayer: player,
    time,
    color,
    abbrev,
    pointerId: e.pointerId,
    startX: e.clientX,
    startY: e.clientY,
    currentX: e.clientX,
    currentY: e.clientY,
    targetPlayer: player,
    sourceElement: reminderEl
  };

  // Create floating element
  createFloatingElement(dragState);

  // Hide the original element
  reminderEl.style.opacity = '0.3';

  // Highlight current player as initial target
  highlightPlayer(player);
}

function handlePointerMove(e) {
  if (!dragState || e.pointerId !== dragState.pointerId) return;

  e.preventDefault();

  dragState.currentX = e.clientX;
  dragState.currentY = e.clientY;

  // Update floating element position
  if (floatingElement) {
    floatingElement.style.left = (e.clientX - 14) + 'px';
    floatingElement.style.top = (e.clientY - 14) + 'px';
  }

  // Find player under pointer
  const playerEl = findPlayerAtPoint(e.clientX, e.clientY);
  const newTarget = playerEl ? playerEl.getAttribute('data-player') : null;

  if (newTarget !== dragState.targetPlayer) {
    // Clear old highlight
    if (dragState.targetPlayer) {
      unhighlightPlayer(dragState.targetPlayer);
    }
    // Set new highlight
    if (newTarget) {
      highlightPlayer(newTarget);
    }
    dragState.targetPlayer = newTarget;
  }
}

function handlePointerUp(e) {
  if (!dragState || e.pointerId !== dragState.pointerId) return;

  e.preventDefault();

  // Find final target
  const playerEl = findPlayerAtPoint(e.clientX, e.clientY);
  const toPlayer = playerEl ? playerEl.getAttribute('data-player') : null;

  // Complete the drag
  completeDrag(toPlayer);
}

function handlePointerCancel(e) {
  if (!dragState || e.pointerId !== dragState.pointerId) return;

  // Cancel without moving
  completeDrag(null);
}

function completeDrag(toPlayer) {
  if (!dragState) return;

  // Clean up UI
  if (floatingElement) {
    floatingElement.remove();
    floatingElement = null;
  }

  // Restore original element opacity
  if (dragState.sourceElement) {
    dragState.sourceElement.style.opacity = '1';
  }

  // Clear all highlights
  document.querySelectorAll('[data-player]').forEach(el => {
    el.style.outline = '';
    el.style.outlineOffset = '';
  });

  // If we have a valid target different from source, dispatch CustomEvent
  if (toPlayer && toPlayer !== dragState.fromPlayer) {
    const event = new CustomEvent('reminderDropped', {
      bubbles: true,
      detail: {
        token: dragState.token,
        fromPlayer: dragState.fromPlayer,
        toPlayer: toPlayer,
        time: dragState.time
      }
    });
    document.dispatchEvent(event);
  }

  dragState = null;
}

// Check if a token is a pseudo reminder (not owned by any role)
function isPseudoReminder(token) {
  return token && token.startsWith('ex_');
}

function createFloatingElement(state) {
  floatingElement = document.createElement('div');
  // Use square (2px radius) for pseudo reminders, circle (50%) for regular
  const borderRadius = isPseudoReminder(state.token) ? '2px' : '50%';
  floatingElement.style.cssText = `
    position: fixed;
    left: ${state.currentX - 14}px;
    top: ${state.currentY - 14}px;
    width: 28px;
    height: 28px;
    border-radius: ${borderRadius};
    background: ${state.color};
    border: 2px solid #FFD700;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 8px;
    font-weight: bold;
    color: white;
    pointer-events: none;
    z-index: 10000;
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
    touch-action: none;
  `;
  floatingElement.textContent = state.abbrev;
  document.body.appendChild(floatingElement);
}

function findPlayerAtPoint(x, y) {
  const element = document.elementFromPoint(x, y);
  if (!element) return null;

  // Walk up the DOM tree looking for data-player attribute
  let current = element;
  while (current && current !== document.body) {
    if (current.hasAttribute('data-player')) {
      return current;
    }
    current = current.parentElement;
  }
  return null;
}

function highlightPlayer(playerName) {
  const playerEl = document.querySelector(`[data-player="${playerName}"]`);
  if (playerEl) {
    playerEl.style.outline = '3px solid #FFD700';
    playerEl.style.outlineOffset = '2px';
  }
}

function unhighlightPlayer(playerName) {
  const playerEl = document.querySelector(`[data-player="${playerName}"]`);
  if (playerEl) {
    playerEl.style.outline = '';
    playerEl.style.outlineOffset = '';
  }
}
