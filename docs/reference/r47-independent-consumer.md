# R47 independent replication consumer

This repository is an independently executing consumer of the `epistemic-sensor-factory-pack` replication contract.

## Identity

The admitted producer is `seanchatmangpt/ggen-marketplace@b942ff54f7d00e376dd2f28beb930390f4feb97b`. The producer fixture admits `seanchatmangpt/ggen@f4e1bce1efdcdc4f6c2531be9f66070950f7ec93` as an eligible replication target. A candidate is admissible only when it descends from that exact target base.

## Authority

The court is `VERIFY_ONLY`. It has no consequential DO authority, uses read-only GitHub permissions, does not mutate source, and emits a machine-readable receipt artifact.

## Replay

```sh
python3 verification/r47-consumer/test_verify.py
python3 verification/r47-consumer/verify.py \
  --repo seanchatmangpt/ggen \
  --candidate-sha "$(git rev-parse HEAD)"
```

Passing the target-local court proves only the consumer subject. Producer standing remains independently governed by the marketplace exact subject; neither standing implies the other.

## Refusals

The court fails closed on repository identity mismatch, exact-subject mismatch, non-descendant lineage, producer correspondence failure, producer ineligibility, or authority broadening.

## Manufacturing note

`HANDWRITTEN_IRREDUCIBLE_REASON`: this small repository-local adapter is the independent execution/authority boundary. Generating or executing it solely in the producer repository would collapse producer evidence and consumer evidence into one authority domain. The reusable producer semantic contract remains canonical in `ggen-marketplace`.
