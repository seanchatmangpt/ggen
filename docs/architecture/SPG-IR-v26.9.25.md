# Semantic Procedural Graph compiler — v26.9.25

`ggen-spg` is a deterministic compiler boundary for Semantic Procedural Graphs.

It does **not** introduce a new planner. It validates one semantic procedure identity and emits bounded projection envelopes for specialized machinery.

```text
SPG -> { HDDL | FOND | TLA+ | OCEL 2.0 | SA2A | BRCE }
```

The source graph remains authoritative for semantic identity. A projection binding is emitted with:

```text
semantic_equivalence = UNCLAIMED
standing = NONE
```

because adjacency is not proof of equivalence and construction is not execution.

## Commands

```bash
cargo run --manifest-path crates/ggen-architecture/Cargo.toml \
  -p ggen-architecture-cli --bin ggen-spg -- validate graph.json

cargo run --manifest-path crates/ggen-architecture/Cargo.toml \
  -p ggen-architecture-cli --bin ggen-spg -- compile graph.json --family tla_plus

cargo run --manifest-path crates/ggen-architecture/Cargo.toml \
  -p ggen-architecture-cli --bin ggen-spg -- diff old.json new.json
```

## Structural law

A consequential edge is refused unless it carries:

- a non-`NONE` authority requirement;
- evidence requirements;
- `receipt_required = true`;
- a falsifier.

An SPG with `standing = ALIVE` is refused by this structural compiler. Runtime standing belongs to observed execution and receipts, not the source graph.

## Prior-art boundary

The compiler requires prior-art records to be present but does not decide their truth. The prior-art admission court owns the `reuse -> compose -> extend -> invent` decision. This prevents ggen from turning a generated novelty claim into standing.

## Falsifier

If required execution semantics cannot be represented by SPG or explicitly bounded in a projection contract, extend the source semantics rather than hiding the gap in generated code.
