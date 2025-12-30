// RoleDrag.js - Pure JavaScript drag handler for role tokens using Pointer Events
// This mirrors ReminderDrag.js but for dragging roles between players

let dragState = null;
let floatingElement = null;
let initialized = false;

// Initialize drag handling for role tokens
// Sets up event listeners; drops are communicated via CustomEvent 'roleDropped'
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

// Subscribe to roleDropped events
// Returns an unsubscribe function
export const subscribeToDropsImpl = (callback) => () => {
  const handler = (e) => {
    const { role, fromPlayer, toPlayer, time } = e.detail;
    callback({ role, fromPlayer, toPlayer, time })();
  };
  document.addEventListener('roleDropped', handler);
  // Return unsubscribe function
  return () => {
    document.removeEventListener('roleDropped', handler);
  };
};

function handlePointerDown(e) {
  // Check if we clicked on a draggable role token
  const roleEl = e.target.closest('[data-role-token]');
  if (!roleEl) return;

  // Prevent default to avoid text selection, scrolling, etc.
  e.preventDefault();

  // Capture pointer to this element for reliable tracking
  roleEl.setPointerCapture(e.pointerId);

  // Extract role data from attributes
  const role = roleEl.getAttribute('data-role-token');
  const player = roleEl.getAttribute('data-role-player');
  const time = roleEl.getAttribute('data-role-time') || '';
  const color = roleEl.getAttribute('data-role-color') || '#666';
  const displayName = roleEl.getAttribute('data-role-display') || role;

  // Start drag state
  dragState = {
    role,
    fromPlayer: player,
    time,
    color,
    displayName,
    pointerId: e.pointerId,
    startX: e.clientX,
    startY: e.clientY,
    currentX: e.clientX,
    currentY: e.clientY,
    targetPlayer: player,
    sourceElement: roleEl
  };

  // Create floating element
  createFloatingElement(dragState);

  // Dim the original element
  roleEl.style.opacity = '0.4';

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
    floatingElement.style.left = (e.clientX - 35) + 'px';
    floatingElement.style.top = (e.clientY - 35) + 'px';
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
    const event = new CustomEvent('roleDropped', {
      bubbles: true,
      detail: {
        role: dragState.role,
        fromPlayer: dragState.fromPlayer,
        toPlayer: toPlayer,
        time: dragState.time
      }
    });
    document.dispatchEvent(event);
  }

  dragState = null;
}

function createFloatingElement(state) {
  floatingElement = document.createElement('div');
  floatingElement.style.cssText = `
    position: fixed;
    left: ${state.currentX - 35}px;
    top: ${state.currentY - 35}px;
    width: 70px;
    height: 70px;
    border-radius: 50%;
    background: ${state.color};
    border: 3px solid #FFD700;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 10px;
    font-weight: bold;
    color: white;
    pointer-events: none;
    z-index: 10000;
    box-shadow: 0 4px 12px rgba(0,0,0,0.4);
    touch-action: none;
    text-align: center;
    padding: 5px;
    box-sizing: border-box;
  `;
  floatingElement.textContent = state.displayName;
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
