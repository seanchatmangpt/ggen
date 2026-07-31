# Dependency boundaries

The v26.8.1 dependency model must distinguish public package dependencies, vendored private implementation crates, dev-only verification tools, optional protocol modules, and forbidden direct dependencies.

## Required boundaries

- The published root `ggen` identity must remain distinct from the non-publishable engine crate.
- `wasm4pm` must not become a direct native dependency because process analysis and its WASM binding constraints remain external; native use routes through `wasm4pm-compat` only where admitted.
- Verification-only crates must not leak into shipped runtime closure.
- Optional BUSL or otherwise non-OSI dependency paths require explicit feature and license treatment.
- Vendored crates must record provenance, divergence from registry releases, and criteria for eventual de-vendoring.
- Graph engines and schemas may interoperate through narrow contracts but cannot silently duplicate semantic authority.

## Validation

Cargo metadata, feature matrices, license inventories, publish dry runs, and negative dependency guards must generate one machine-readable dependency-boundary report bound to the exact source head.
