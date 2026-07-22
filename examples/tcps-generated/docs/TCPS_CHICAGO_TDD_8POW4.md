# TCPS Chicago TDD 8⁴ Combinatorial Innovation Fabric

This document specifies the executable 8⁴ layer in
`tests/tcps_chicago_tdd_8pow4.rs`.

## Purpose

The merged 1000× fabric established one complete Chicago-TDD constitution for
43 checked-in TCPS capabilities and compiled 1,204 obligations. The 8⁴ layer
adds a second, orthogonal geometry for exploring the interaction space itself:

```text
8 production layers
× 8 TCPS doctrines
× 8 execution surfaces
× 8 standing states
= 4,096 unique scenario cells
```

The implementation does not replace the 1,204-obligation graph. It composes
with it:

```text
real TCPS product
→ 43 real filesystem witnesses
→ 1,204 constitutional obligations
→ 4,096 combinatorial scenario cells
→ replay-stable receipt and evidence
```

## Four axes

### Production layer

1. meaning
2. flow
3. quality
4. safety
5. authority
6. runtime
7. release
8. evidence

### TCPS doctrine

1. standard work
2. jidoka
3. just in time
4. heijunka
5. kanban
6. poka-yoke
7. genchi genbutsu
8. kaizen

### Execution surface

1. source
2. native Rust
3. `no_std`
4. CLI
5. FFI
6. WASM
7. packaging
8. evidence

### Standing state

1. admitted
2. selected
3. authorized
4. actuated
5. receipted
6. replayed
7. refused
8. unsupported

The standing axis deliberately preserves refusal and unsupported as distinct
states. Selection, authorization, and actuation also remain separate.

## 8-power coverage ladder

The compiler produces four lawful execution rails.

| Rail | Cells | Guarantee |
|---|---:|---|
| `8¹` | 8 | every state of every axis appears once |
| `8²` | 64 | every pair of axis states appears exactly once |
| `8³` | 512 | every triple of axis states appears exactly once |
| `8⁴` | 4,096 | every complete coordinate appears exactly once |

The pairwise rail is an orthogonal array over `GF(8)`. Its rows are:

```text
x, y, x + y, x + 2y
```

where addition is XOR and multiplication uses the irreducible polynomial
`x³ + x + 1`.

The triple-wise rail uses:

```text
x, y, z, x + y + z
```

Any three columns uniquely determine the fourth. Therefore all 512 possible
three-axis projections occur once.

The exhaustive rail remains the completeness rail. Smaller rails reduce
scenario execution when the required interaction strength is known.

## Gray-path traversal

The exhaustive rail is ordered by a cyclic 12-bit binary-reflected Gray code.
Each axis occupies three bits. Adjacent cells therefore differ by exactly one
bit in exactly one axis, including the final-to-first transition.

This minimizes scenario setup churn while retaining all 4,096 coordinates.

## Real witness compression

Every cell is bound deterministically to one of the 43 real TCPS capability
artifacts already used by the 1000× fabric.

The 43 filesystem observations are executed once. Their admitted fingerprints
discharge all 4,096 cells. Witness load is balanced to within one cell:

```text
minimum load = 95 cells
maximum load = 96 cells
```

A missing artifact is a loud refusal. A cell cannot be discharged without its
accepted real witness.

## Orthogonality proofs

The Chicago tests establish:

- 512 occurrences of every one-axis state;
- 64 occurrences of every two-axis state pair;
- 8 occurrences of every three-axis state triple;
- one occurrence of every full four-axis coordinate;
- uniqueness of all cell identities;
- bijection between ordinal, Gray value, and coordinates.

## Receipt and evidence

The artifact scenario emits:

- `hypercube-receipt.json`
- `hypercube.ttl`
- `hypercube.ocel.json`
- `hypercube-plan.tsv`
- `hypercube-merkle.txt`
- `coverage-ladder.tsv`

The receipt binds the four axes, all four rail cardinalities, 43 real probes,
all 4,096 discharges, witness-load bounds, and a deterministic Merkle root.
Occurrence time remains outside semantic identity.

## Running

From `examples/tcps-generated`:

```bash
bash scripts/validate-8pow4.sh
```

The script runs both layers:

```text
1,204-obligation constitutional fabric
+
4,096-cell combinatorial hypercube
```

No mock collaborator is introduced. All product probes read the actual
checked-in TCPS consumer.
