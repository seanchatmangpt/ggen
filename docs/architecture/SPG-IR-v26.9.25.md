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

## Rewrite manufacturing / exact-subject replay

Beyond validating, diffing, and compiling projections, `ggen-spg` manufactures deterministic graph rewrites against an exact subject. `rewrite-plan` produces a plan from a source SPG to a candidate target, bound to an exact subject (repository `owner/name`, immutable 40-hex source commit, canonical source-graph digest — a mismatched digest is refused); `apply` replays that plan onto the exact source graph. `replay` applies the plan twice and refuses with `SPG_REWRITE_NONDETERMINISTIC_REPLAY` unless both runs are byte-identical, emitting a `chatman.spg-rewrite-replay.v1` receipt with the plan digest, target-graph digest, and both replay digests. As everywhere in this compiler, the receipt carries `authority = NONE` and `standing = NONE` — a reproducible rewrite is still not an executed, admitted one.

```bash
cargo run --manifest-path tools/ggen-architecture/Cargo.toml \
  -p ggen-architecture-cli --bin ggen-spg -- rewrite-plan old.json new.json \
  --repository owner/name --commit <40-hex-commit>

cargo run --manifest-path tools/ggen-architecture/Cargo.toml \
  -p ggen-architecture-cli --bin ggen-spg -- replay graph.json plan.json
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
