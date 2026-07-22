# TCPS Auto Select 8⁴ behavioral fabric

The 8⁴ layer now executes the actual TCPS selection and authorization APIs. It no
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

## Real execution

For every cell, the fabric:

1. constructs a real eight-candidate `方策`;
2. executes `自動選択::選択する`;
3. compares the result with an independently authored public-contract oracle;
4. checks eligibility and readiness masks;
5. checks maximum ready mass and deterministic first-lane tie resolution;
6. checks every workspace lane;
7. checks typed refusal and receipt semantics;
8. passes every selected proposal through `青い川のダム::仲介者::許可する`;
9. checks the authorization receipt.

The complete matrix has these pinned outcomes:

| Outcome | Count |
|---|---:|
| Selected and authorized | 1,670 |
| Refused because no candidate is eligible | 1,264 |
| Refused because eligible candidates are not ready | 1,162 |
| Total | 4,096 |

The authorization suite separately falsifies wrong-tool, expired-capability,
insufficient-authority, and wrong-policy paths.

## Coverage rails

The mathematical coverage ladder remains, but it now indexes real input states:

- `8¹ = 8` marginal coverage;
- `8² = 64` pairwise orthogonal coverage over `GF(8)`;
- `8³ = 512` triple-wise orthogonal coverage;
- `8⁴ = 4,096` exhaustive coverage.

The `GF(8)` multiplication table is pinned independently for the irreducible
polynomial `x³ + x + 1`. The exhaustive rail uses a cyclic 12-bit Gray path, and
the first sixteen values are pinned as independent known vectors.

## Receipt law

Every scenario digest binds:

- coordinates;
- actual outcome class;
- eligibility and readiness masks;
- selected tool and mass when present;
- actual selection receipt;
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
