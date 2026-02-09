export function setPointerCapture(id) {
  return function(el) {
    return function () {
      el.setPointerCapture(id);
    };
  };
}
  
export function releasePointerCapture(id) {
  return function(el) {
    return function () {
      el.releasePointerCapture(id);
    };
  };
}
  
export function hasPointerCapture(id) {
  return function(el) {
    return function () {
      return el.hasPointerCapture(id);
    };
  };
}