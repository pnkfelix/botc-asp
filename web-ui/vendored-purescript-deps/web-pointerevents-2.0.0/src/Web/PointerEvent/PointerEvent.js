export function isPrimary(ev) {
  return ev.isPrimary;
}

export function pointerId(ev) {
  return ev.pointerId;
}

export function pointerTypeImpl(ev) {
  return ev.pointerType;
}

export function width(ev) {
  return ev.width;
}

export function height(ev) {
  return ev.height;
}

export function pressure(ev) {
  return ev.pressure;
}

export function tangentialPressure(ev) {
  return ev.tangentialPressure;
}

export function tiltX(ev) {
  return ev.tiltX;
}

export function tiltY(ev) {
  return ev.tiltY;
}

export function twist(ev) {
  return ev.twist;
}

export function altitudeAngle(ev) {
  return ev.altitudeAngle;
}

export function azimuthAngle(ev) {
  return ev.azimuthAngle;
}

export function getCoalescedEvents(ev) {
  return ev.getCoalescedEvents();
}

export function getPredictedEvents(ev) {
  return ev.getPredictedEvents();
}