<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Dormant Code Register](#dormant-code-register)
  - [Preserved but not promoted. Bytes remain. No deletion.](#preserved-but-not-promoted-bytes-remain-no-deletion)
  - [Preservation Law](#preservation-law)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Dormant Code Register
## Preserved but not promoted. Bytes remain. No deletion.

Every entry here is marked with its preservation rationale — the architectural
pattern, boundary shape, interface name, or proof surface it contains.
Nothing here is "dead code." Everything is dormant matter awaiting admission.

---

| Artifact | Path | Preserved because | Admission path |
|---|---|---|---|
| `genesis-wasm-shell` | `crates/genesis-wasm-shell/` | Contains WASM↔Genesis boundary shape. Only code that attempts AtomVM custody. | Register in workspace. Fix imports. Classify BROKEN_BUT_REAL → LIVE. |
| `genesis-lockchain` | `crates/genesis-lockchain/` | Contains Merkle receipt chain, distributed lock logic — richest Truex substrate. | Register in workspace as Truex foundation. |
| `genesis-construct8` | `crates/genesis-construct8/` | Contains Shard, Corpus, Forge, Replay, OCEL/SHACL projectors — richest capability cluster. | Register in workspace. Wire to genesis-core-v2. |
| `genesis-schema-v2` | `crates/genesis-schema-v2/` | YAWL pattern metadata types. In workspace but underused. | Wire to genesis-core-v2 pattern registry. |
| `genesis-types-v2` | `crates/genesis-types-v2/` | ExecutionContext, WorkflowStep, PatternId, StepId. Foundation for YAWL. | Wire to pattern registry + OCEL projection. |
| `playground/` | `playground/src/` | Contains Shard/Corpus/Segment models, checker, profiler, scheduler. Architectural sketch. | Classify each file. Extract CAPABILITY_SEED contracts. |
| `examples/7-agent-validation/` | `examples/7-agent-validation/src/` | Consensus + PROV integration. Multi-agent validation pattern. | Wire PROV output to genesis-core-v2::Receipt. |
| `examples/gcp-erlang-autonomics/` | `examples/gcp-erlang-autonomics/` | Contains AtomVM integration test (`edge_atomvm_integration.rs`). Only AtomVM boundary attempt. | Connect to genesis-wasm-shell once workspace-registered. |
| `tests/chaos/` | `tests/chaos/` | Event store chaos model. | Classify sub-files. Connect to PlagueRecord. |
| `tools/truth-gate/` | `tools/truth-gate/` | Evidence policy enforcement. Hooks for PreToolUse/PostToolUse. | Wire to ArtifactStatus taxonomy. |
| `ggen-projection` | `crates/ggen-projection/src/lib.rs` | RelationPage projection layer. Contains PROV references. | Connect to genesis-core-v2 RelationPage. Wire to OCEL projection. |

---

## Preservation Law

Dormant code is marked in-source with:

```rust
// STATUS: DORMANT — preserved per Non-Deletion Completion Protocol
// PATTERN: <pattern name>
// ADMISSION_PATH: <what would make this LIVE>
```

No dormant code is compiled by default (feature-gated or workspace-excluded).
No dormant code is deleted. No dormant code is rewritten.
The bytes remain. The pattern waits.
