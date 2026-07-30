# ggen-first Lean 4 to Rust

`ggen-lean4-rust-pipeline-pack` manufactures a Lean 4 program from an admitted RDF graph. Lean checks the proof bundle before that program may emit a Rust crate. The resulting Rust binary produces a BLAKE3-bound execution receipt referencing the Lean proof receipt.

## Source boundary

The consumer authors exactly:

```text
ggen.toml
ontology.ttl
```

There is no handwritten Lean file, Rust file, Cargo manifest, test, receipt schema, or pipeline report in the consumer surface.

## Law-state route

```text
parse RDF
→ run fail-closed graph gates
→ ggen emits Lean source and a signed sync receipt
→ Lean kernel checks the theorem bundle
→ the admitted Lean executable emits Cargo.toml and Rust source
→ rustfmt canonicalizes the emitted Rust
→ Clippy and tests admit the crate
→ the public binary executes
→ BLAKE3 binds the execution receipt to proof-receipt.json
→ replay and second-emission identity are verified
```

## Why this is Lean 4 to Rust rather than parallel projection

The pack contains no Rust template. ggen owns these outputs:

```text
generated/lean/lean-toolchain
generated/lean/lakefile.lean
generated/lean/Main.lean
generated/PIPELINE.md
```

`generated/rust/` does not exist after `ggen sync run`. It appears only after:

```bash
cd generated/lean
lake build
lake exe emitRust
```

The Lean emitter accepts a `ProofReceipt` value whose fields contain the universal bound proof, concrete witness proof, and fixed-point proof. If any theorem is absent or invalid, the executable does not compile and Rust cannot be emitted.

## Current semantic cell

The first admitted cell is a bounded successor:

```text
step(x) = x + 1  when x < bound
step(x) = bound  otherwise
```

Lean proves:

- `step_le`: every result is at most the declared bound;
- `step_witness`: the declared witness input reaches the declared output;
- `step_fixed_point`: the bound is a fixed point.

The specimen uses bound `10` and witness `9 → 10`.

## Verification

Run from the repository root after Lean is installed:

```bash
bash scripts/verify-lean4-rust-pipeline.sh
```

The repository workflow bootstraps the generated Lean project first, installs the exact toolchain declared by the ontology, then executes the complete verifier.

## Exclusions

This v1 pipeline does not claim arbitrary Lean extraction, total semantic equivalence between Lean and Rust, automatic translation of unrestricted recursive functions, foreign-function correctness, or side-effect equivalence. It proves and compiles one bounded, pure, finite successor cell. Extension requires adding a new admitted semantic cell rather than weakening this boundary.
