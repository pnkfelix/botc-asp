// FFI for touch event handling

// Get the first touch point's clientX from a TouchEvent
export const touchClientX = (touchEvent) => {
  const touches = touchEvent.touches.length > 0
    ? touchEvent.touches
    : touchEvent.changedTouches;
  return touches.length > 0 ? touches[0].clientX : 0;
};

// Get the first touch point's clientY from a TouchEvent
export const touchClientY = (touchEvent) => {
  const touches = touchEvent.touches.length > 0
    ? touchEvent.touches
    : touchEvent.changedTouches;
  return touches.length > 0 ? touches[0].clientY : 0;
};

// Convert TouchEvent to Event for preventDefault
export const touchToEvent = (touchEvent) => touchEvent;
