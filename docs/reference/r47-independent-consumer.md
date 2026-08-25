# R48 independent replication consumer

This repository is an independently executing consumer of the `epistemic-sensor-factory-pack` replication contract.

## Identity

The admitted producer fixture is owned by `seanchatmangpt/ggen-marketplace@b942ff54f7d00e376dd2f28beb930390f4feb97b`. It admits `seanchatmangpt/ggen@f4e1bce1efdcdc4f6c2531be9f66070950f7ec93` as an eligible replication target. A candidate is admissible only when it descends from that exact target base.

The reusable consumer court is sourced from exact marketplace R48 subject `4715c562c3dd2c073c0ca119edbc013647978cae`; ggen owns only its repository-local identity contract and invocation boundary.

## Authority

The court is `VERIFY_ONLY`. It has no consequential DO authority, uses read-only GitHub permissions, does not mutate source, and emits a machine-readable receipt artifact.

## Replay

With the exact R48 marketplace subject available at `_r48_producer`:

```sh
python3 _r48_producer/packs/epistemic-sensor-factory-pack/tools/consumer_court.py \
  --consumer-root . \
  --contract verification/r47-consumer/consumer.json \
  --repo seanchatmangpt/ggen \
  --candidate-sha "$(git rev-parse HEAD)" \
  --producer-fixture _r48_producer/packs/epistemic-sensor-factory-pack/fixtures/r46-live-replication.ttl
```

Passing this target-local invocation proves only the exact consumer subject. Producer standing remains independently governed; neither standing implies the other.

## Refusals

The shared court fails closed on repository identity mismatch, exact-subject mismatch, non-descendant lineage, producer correspondence failure, producer ineligibility, or authority broadening.

## Manufacturing note

`HANDWRITTEN_IRREDUCIBLE_REASON`: the repository-local JSON identity and workflow invocation are the independent authority boundary. Verification algorithms, receipt construction, and falsifier semantics are canonical producer-owned substrate and are deliberately not duplicated in this consumer.
