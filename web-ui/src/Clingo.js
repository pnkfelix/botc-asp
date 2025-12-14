// FFI bindings for clingo-wasm
import * as clingo from "clingo-wasm";

export const initImpl = (wasmUrl) => () => {
  // The worker runs in a blob context where relative URLs don't work.
  // Construct an absolute URL using the current origin.
  const absoluteUrl = new URL(wasmUrl, window.location.origin).href;
  return clingo.init(absoluteUrl);
};

export const runImpl = (program) => (numModels) => () =>
  clingo.run(program, numModels);
