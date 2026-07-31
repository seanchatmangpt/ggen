# RWR Level-5 Foundation Pack

This pack manufactures a complete Ross-Weill-Robertson foundation-for-execution consumer for ggen. It combines the four canonical enterprise-architecture stages—Business Silos, Standardized Technology, Optimized Core, and Business Modularity—with the later MIT CISR fifth stage, Digital Ecosystem.

The pack does not promote from labels. Its ontology declares **21 dimensions** spanning:

- operating model;
- enterprise architecture core diagram;
- digitized platform and operational backbone;
- engagement model;
- value realization;
- machinery, automation, autonomics, receipts, and replay.

Every dimension requires exactly three independent evidence surfaces. The generated consumer therefore carries **63 proof obligations**. `ALIVE` is issued only after the executable `ggen_graph::rwr::ReferenceFoundation` crosses real filesystem boundaries, emits atomic artifact-plus-receipt transactions, replays the evidence ledger, and closes every dimension at Digital Ecosystem maturity.

## Run

```bash
cargo test -p ggen-graph --test rwr_level5_e2e
cargo build -p ggen-cli-lib --bin ggen
cd packs/rwr-level5-foundation-pack
../../target/debug/ggen sync run
cargo test --manifest-path consumer/rwr-level5-foundation/Cargo.toml
../../target/debug/ggen receipt verify
bash consumer/rwr-level5-foundation/scripts/verify.sh
```

## Source of truth

- `ontology.ttl` defines the complete RWR matrix, required proof surfaces, implementation artifact, verifier, and named falsifier for every dimension.
- `gates/` refuse missing dimensions, missing properties, missing proof surfaces, duplicate order values, or a foundation target below Digital Ecosystem.
- `crates/ggen-graph/src/rwr/` implements the maturity assessor, execution grants, atomic actuator, MAPE-K controller, evidence ledger, receipts, replay, and executable reference foundation.
- `crates/ggen-graph/tests/rwr_level5_e2e.rs` is the direct real-boundary verifier.
- `consumer/rwr-level5-foundation/` is generated from the ontology and is never hand-maintained.

## Standing discipline

The source pack is a complete Level-5 implementation contract. Repository standing remains `UNKNOWN` until CI executes the crown verifier on the committed tree. A green crown run yields `ALIVE`; any missing surface is `UNKNOWN`, a falsifier hit is `BLOCKED`, and compilation failure is `BUILD_BROKEN`.
