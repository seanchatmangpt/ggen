# v26.11.3 Lane Partition

Canonical checkout: /Users/sac/ggen. Agents never run git commands.
Coordinator owns git transitions and commits.

| Lane | Owns | Scope |
|---|---|---|
| L01 | crates/ggen-abb-sbb/src/ (refusal surface) | Typed Refusal across the boundary (R2) |
| L02 | engine pack.rs (compose), sync.rs 2c path | compose → Stage 2c, never alone (R1) |
| L03 | sync.rs Stage 2c ea.graph.json branch | Fail loud or explicit opt-out flag (R3) |
| L04 | pack authoring (Turtle+SHACL + projection) | Turtle+SHACL authoring; structs = target (R4) |
| L05 | qualification courts (digest binding) | Digest-bound only; ALIVE = evidence (R5) |
| L06 | arcus consumer qualification (read-only) | E2E receipt: arcus consumes kernel via R1 path |

## Off-limits (all lanes)

- justfile — coordinator only
- git state commands — coordinator only
- .ggen-v2/, .specify/artifacts/ shared receipts — write via coordinator
- crates outside your ownership row — minimal-unblock SLA only, disclosed in
  BOTH receipts (10-minute compile-freeze rule)

## Integration

Conflicts resolve by reading both sides (never destructive heuristics); sole
writers are listed above; RESOLUTIONS.md records any mid-flight seam ruling.
