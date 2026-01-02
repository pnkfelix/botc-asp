// FFI bindings for reading URL parameters

// Get a URL parameter by name, returns null if not present
export const getUrlParamImpl = (name) => () => {
  const params = new URLSearchParams(window.location.search);
  return params.get(name);
};
