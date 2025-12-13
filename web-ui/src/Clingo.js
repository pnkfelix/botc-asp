// FFI bindings for clingo-wasm
import * as clingo from "clingo-wasm";

export const initImpl = (wasmUrl) => () => clingo.init(wasmUrl);

export const runImpl = (program) => (numModels) => () =>
  clingo.run(program, numModels);
