# Vendored PureScript Dependencies

This directory contains the PureScript source code for all transitive
dependencies of the `botc-web-ui` project. These are vendored here so that
`purs compile` can run in environments that lack network access (such as
Claude Code Remote / CCR), enabling type checking without a full `spago build`.

## Why vendor these?

The PureScript compiler (`purs`) is a native binary that ships with the npm
`purescript` package and works fine in restricted environments. The problem is
that `spago` (the build tool) needs to *download* dependency sources from the
PureScript package registry, which requires network access. By vendoring the
sources here, we bypass that requirement entirely.

## Structure

Each subdirectory is named `<package>-<version>` and contains the package's
`src/` tree of `.purs` files (and sometimes `.js` FFI files). These mirror
what `spago install` places in `.spago/p/`.

## How to populate / update

On a machine with network access, run the automation script:

```bash
cd web-ui
npm install                            # ensures spago is available
./scripts/vendor-purescript-deps.sh    # downloads, copies, and hashes
```

The script runs `spago install`, copies all packages from `.spago/p/` into
this directory, and writes a `.spago-lock-hash` file containing the SHA-256
hash of `spago.lock`. This hash is used by `scripts/check-purs.sh` to warn
when the vendored dependencies may be stale (i.e., `spago.lock` has changed
since the last vendor run).

Then commit the result (including `.spago-lock-hash`).

## Versions

The dependency versions are pinned by `spago.lock`. The packages in the
current build plan (72 packages) are:

```
aff-7.1.0              aff-promise-4.0.0       arrays-7.3.0
avar-5.0.0              bifunctors-6.0.0        catenable-lists-7.0.0
console-6.1.0           const-6.0.0             contravariant-6.0.0
control-6.0.0           datetime-6.1.0          distributive-6.0.0
dom-indexed-12.0.0      effect-4.0.0            either-6.1.0
enums-6.0.1             exceptions-6.1.0        exists-6.0.0
foldable-traversable-6.0.0  foreign-7.0.0       foreign-object-4.1.0
fork-6.0.0              free-7.1.0              freeap-7.0.0
functions-6.0.0         functors-5.0.0          gen-4.0.0
halogen-7.0.0           halogen-subscriptions-2.0.0  halogen-svg-elems-8.0.0
halogen-vdom-8.0.0      identity-6.0.0          integers-6.0.0
invariant-6.0.0         js-date-8.0.0           js-promise-1.0.0
lazy-6.0.0              lists-7.0.0             maybe-6.0.0
media-types-6.0.0       newtype-5.0.0           nonempty-7.0.0
now-6.0.0               nullable-6.0.0          numbers-9.0.1
ordered-collections-3.2.0  orders-6.0.0         parallel-7.0.0
partial-4.0.0           prelude-6.0.1           profunctor-6.0.1
refs-6.0.0              safe-coerce-2.0.0       st-6.2.0
strings-6.0.1           tailrec-6.1.0           transformers-6.1.0
tuples-7.0.0            type-equality-4.0.1     typelevel-prelude-7.0.0
unfoldable-6.0.0        unsafe-coerce-6.0.0     unsafe-reference-5.0.0
web-clipboard-6.0.0     web-dom-6.0.0           web-events-4.0.0
web-file-4.0.0          web-html-4.1.0          web-pointerevents-2.0.0
web-storage-5.0.0       web-touchevents-4.0.0   web-uievents-5.0.0
```

## How type checking works

The `scripts/check-purs.sh` script runs `purs compile` over both the project
source and these vendored dependencies:

```bash
purs compile 'src/**/*.purs' 'vendored-purescript-deps/*/src/**/*.purs'
```

This performs full parsing, name resolution, and type checking -- the same
checks that `spago build` would do.
