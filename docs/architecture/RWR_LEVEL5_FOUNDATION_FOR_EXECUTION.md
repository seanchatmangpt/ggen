# RWR Level-5 Foundation for Execution in ggen

## Governing law

Ross, Weill, and Robertson define enterprise architecture as the organizing logic for business processes and IT infrastructure, reflecting the integration and standardization requirements of the operating model. Their canonical maturity path is:

```text
Business Silos → Standardized Technology → Optimized Core → Business Modularity
```

MIT CISR later extended the path with a fifth stage:

```text
Business Modularity → Digital Ecosystem
```

ggen implements the fifth stage as a proof-bearing execution system rather than an architecture label:

```text
O → O* → machinery → grant → automation → consequence → receipt → replay
                         ↑                              ↓
                         └──── MAPE-K autonomics ──────┘
```

## Full maturity matrix

The executable contract contains 21 dimensions. Every dimension requires three independent observed surfaces and one named falsifier.

| RWR domain | Dimensions |
|---|---|
| Operating model | Process integration; process standardization; decision rights |
| Core diagram | Core processes; shared data; linking/automation; customer and partner channels |
| Digitized platform | Technology standardization; operational backbone; reusable business components; ecosystem interfaces |
| Engagement model | Enterprise governance; project management; architecture linking mechanisms |
| Value realization | Reliability/transparency; strategic agility; economic value |
| Execution control | Machinery closure; automation closure; autonomic closure; receipt/replay |

The matrix therefore has:

```text
21 dimensions × 3 proof surfaces = 63 crown obligations
```

## Admission calculus

For dimension `d`, Level 5 is admitted only when every required surface has a latest passing observation at Digital Ecosystem maturity:

```text
L5(d) ⇔ ∀s ∈ required(d), latest(d,s).outcome = Pass
         ∧ latest(d,s).level ≥ DigitalEcosystem
```

The ecosystem crown is conjunctive:

```text
ALIVE ⇔ ∀d ∈ FullRwrMatrix, L5(d)
```

No average score or strong dimension can compensate for an open dimension.

## Machinery

`FoundationMachine` admits a bounded `Action` under executable `ExecutionPolicy` and derives an `ExecutionGrant` bound to:

- action identity;
- maturity dimension;
- payload BLAKE3 digest;
- matrix version.

A changed payload cannot reuse an earlier grant.

## Automation

`FilesystemActuator` stages the artifact and `receipt.json` inside one transaction directory. A same-filesystem directory rename exposes them together under `committed/<action-id>/`. Duplicate action identities are refused.

This is the first bounded implementation of:

```text
zero unreceipted actuation
```

## Autonomics

`AutonomicController` implements a bounded MAPE-K loop over receipted state:

1. Monitor the current committed artifact.
2. Analyze its digest against desired state.
3. Plan a bounded repair action.
4. Execute through the same policy/grant/actuator path.
5. Update knowledge by re-reading committed state.
6. Converge or return a typed refusal after the hard cycle bound.

The autonomic path has no alternate actuator and no unreceipted repair lane.

## Receipt and replay

The evidence ledger is append-only. Later epochs supersede earlier observations for current standing while the ledger root preserves the complete history. Assessment receipts bind:

- matrix version;
- full evidence root;
- every dimension assessment;
- crown Gall state.

`ReplayVerifier` accepts each actuation receipt once and refuses duplicate replay.

## Falsifier

The Level-5 claim is false if any one of the following occurs:

- a dimension has fewer than three required proof surfaces;
- a project can bypass architecture or governance linking;
- a payload can change after grant derivation;
- a consequence becomes visible without its receipt;
- a repair uses a second ungoverned actuator;
- duplicate replay is accepted;
- the evidence ledger cannot reproduce the same crown assessment;
- a generated consumer needs hand-written target logic.

## Operational verifier

```bash
cargo test -p ggen-graph --test rwr_level5_e2e
```

The ontology-first consumer adds the independent generation path:

```bash
cd packs/rwr-level5-foundation-pack
ggen sync run
cargo test --manifest-path consumer/rwr-level5-foundation/Cargo.toml
ggen receipt verify
```
