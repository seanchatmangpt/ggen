# ggen Architecture Foundry Runtime

This Rust package is the executable control plane for the program in
`docs/architecture-foundry/work-program.yaml`.

It does not declare architecture standing from prose. It validates the A-K workstream graph,
binds the exact heads of `ggen` and `ggen-legacy`, initializes a machine-readable corpus,
executes receipt-bound cross-repository extraction, admits workstreams only when their
predicates and evidence digests pass, verifies lineage, replays receipts, and admits the final
solution only after all A-K workstreams and final predicates close.

## Build

```bash
cargo build --manifest-path tools/architecture-foundry/Cargo.toml
cargo test --manifest-path tools/architecture-foundry/Cargo.toml
```

## Validate the program

```bash
cargo run --manifest-path tools/architecture-foundry/Cargo.toml -- \
  validate-program \
  --program docs/architecture-foundry/work-program.yaml
```

## Establish the two-repository baseline

Run from a parent directory containing clean clones named `ggen` and `ggen-legacy`:

```bash
cargo run --manifest-path ggen/tools/architecture-foundry/Cargo.toml -- \
  baseline \
  --program ggen/docs/architecture-foundry/work-program.yaml \
  --source ggen \
  --corpus ggen-legacy \
  --out evidence/baseline
```

## Initialize `ggen-legacy`

```bash
cargo run --manifest-path ggen/tools/architecture-foundry/Cargo.toml -- \
  initialize-corpus \
  --program ggen/docs/architecture-foundry/work-program.yaml \
  --source ggen \
  --corpus ggen-legacy
```

Initialization creates the foundry manifest, typed catalogs, A-K state projection, standing
projection, and an immutable BLAKE3 receipt. Commit the generated corpus as one bounded
checkpoint before the next operation.

## Extract an admitted component

```bash
cargo run --manifest-path ggen/tools/architecture-foundry/Cargo.toml -- \
  extract \
  --program ggen/docs/architecture-foundry/work-program.yaml \
  --source ggen \
  --corpus ggen-legacy \
  --migration migration-batch.yaml
```

The extraction operation copies without deleting the source. Every component receives a
lineage record binding source head, corpus parent head, source path, destination path,
capabilities, disposition, replacement owner, rationale, and content digest.

## Admit a workstream

An independent agent writes a JSON workstream report using schema
`ggen.enterprise-architecture-foundry.workstream-report/1`. The runtime refuses promotion
unless dependencies are admitted, exact heads match, every required predicate equals the
work program, and every evidence file digest verifies.

```bash
cargo run --manifest-path ggen/tools/architecture-foundry/Cargo.toml -- \
  admit-workstream \
  --program ggen/docs/architecture-foundry/work-program.yaml \
  --source ggen \
  --corpus ggen-legacy \
  --report evidence/workstream-A.json
```

## Verify and replay

```bash
cargo run --manifest-path ggen/tools/architecture-foundry/Cargo.toml -- \
  verify \
  --program ggen/docs/architecture-foundry/work-program.yaml \
  --source ggen \
  --corpus ggen-legacy

cargo run --manifest-path ggen/tools/architecture-foundry/Cargo.toml -- \
  replay \
  --source ggen \
  --corpus ggen-legacy
```

`ALIVE` remains unreachable until all A-K workstreams are admitted and an independent final
evidence report satisfies every final predicate at the current exact heads.
