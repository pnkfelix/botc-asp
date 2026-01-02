// FFI bindings for reading and updating URL parameters

// Get a URL parameter by name, returns null if not present
export const getUrlParamImpl = (name) => () => {
  const params = new URLSearchParams(window.location.search);
  return params.get(name);
};

// Set a URL parameter (updates URL in-place without page reload)
// If value is null or undefined, removes the parameter
export const setUrlParamImpl = (name) => (value) => () => {
  const url = new URL(window.location);
  if (value === null || value === undefined) {
    url.searchParams.delete(name);
  } else {
    url.searchParams.set(name, value);
  }
  // Use replaceState to update URL without adding to history
  window.history.replaceState({}, '', url);
};
