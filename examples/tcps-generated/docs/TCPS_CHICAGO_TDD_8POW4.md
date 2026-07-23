# TCPS Auto Select 8⁴ behavioral fabric

The 8⁴ layer executes the actual TCPS selection and authorization APIs. It no
longer assigns unrelated files to synthetic combinations.

## Four real eight-state factors

```text
8 authority masks
× 8 readiness masks
× 8 time budgets
× 8 request and policy modes
= 4,096 real Auto Select judgments
```

The request and policy mode combines three independent binary conditions:

- deterministic capability required;
- receipt-capable capability required;
- forward-score policy or reverse-score policy with an intentional top tie.

The other factors directly populate `選択要求` fields.

## Auto Select v2 kernel law

The RDF-admitted kernel validates policy structure before constructing any u64
candidate mask. It rejects:

- an active-prefix hole;
- a hidden candidate after `件数`;
- a count larger than the backing array;
- a count larger than the 64-bit mask capacity.

The public constructor also refuses a sixty-fifth candidate. Caller-provided mass
scratch is cleared at the beginning of every judgment, including refusal paths.

Hard-gate failure is preserved as an ordered algebra rather than collapsed into
“no eligible candidate”:

1. `権限不足`;
2. `決定性不足`;
3. `受領証不足`;
4. `時間超過`;
5. `準備候補なし`.

An empty, structurally valid policy remains `適格候補なし`. A malformed policy is
`方策矛盾`.

## Real execution

For every cell, the fabric:

1. constructs a real eight-candidate `方策`;
2. executes `自動選択::選択する`;
3. compares the result with an independently authored staged oracle;
4. checks eligibility and readiness masks;
5. checks min-squared mass and deterministic first-lane tie resolution;
6. checks every workspace lane;
7. checks the complete selection or refusal receipt;
8. passes every selected proposal through `青い川のダム::仲介者::許可する`;
9. checks the complete authorization receipt.

The measure fixtures are heterogeneous. Each candidate has exactly one controlling
minimum among the seven dimensions, and non-minimum fields differ. This makes the
mass oracle capable of falsifying an implementation that accidentally uses a
maximum, average, product, or another dimension.

The complete matrix has these pinned outcomes:

| Outcome | Count |
|---|---:|
| Selected and authorized | 1,670 |
| Authority refusal | 512 |
| Determinism refusal | 256 |
| Receipt-capability refusal | 256 |
| Time refusal | 240 |
| Eligible candidates not ready | 1,162 |
| **Total** | **4,096** |

Structural-policy and empty-policy refusals are tested independently because they
are outside the four-axis valid-policy matrix.

The authorization suite separately falsifies wrong-tool, expired-capability,
insufficient-authority, and wrong-policy paths. Selection never implies
authorization.

## Coverage rails

The mathematical coverage ladder indexes real input states:

- `8¹ = 8` marginal coverage;
- `8² = 64` pairwise orthogonal coverage over `GF(8)`;
- `8³ = 512` triple-wise orthogonal coverage;
- `8⁴ = 4,096` exhaustive coverage.

The `GF(8)` multiplication table is pinned independently for the irreducible
polynomial `x³ + x + 1`. The exhaustive rail uses a cyclic 12-bit Gray path, and
the first sixteen values are pinned as independent known vectors.

## Receipt law

Every decision receipt is checked field by field against an independently computed
SHA-256 request digest and the immutable policy digest. Every scenario digest
binds:

- coordinates;
- actual outcome class;
- eligibility and readiness masks;
- selected tool and mass when present;
- actual selection or refusal receipt;
- actual authorization receipt when present.

The 4,096 digests are combined with a domain-separated BLAKE3 Merkle tree. The
root is replay-stable and changes when any leaf changes.

## Evidence artifacts

The test emits and validates:

- `matrix-receipt.json`;
- `scenario-evidence.jsonl`;
- `scenario-plan.tsv`;
- `coverage-rails.tsv`;
- `merkle-root.txt`.

The previous pseudo-OCEL and existence-only RDF projections were removed rather
than represented as standards-compliant evidence.

## Run

From `examples/tcps-generated`:

```bash
bash scripts/validate-8pow4.sh
```

The runner executes both the product-evidence fabric and the 8⁴ behavioral
fabric.
