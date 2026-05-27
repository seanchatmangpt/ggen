# Vision 2030 GALL requirements (PRD / ARD)

This document formalizes the PRD and ARD requirements for the `ggen-graph` substrate.

## Requirement Mapping

### PRDRequirements
- **req_r1_one_crate**: Single-crate package constraint. All `ggen-graph` core logic must live inside `crates/ggen-graph/`.
- **req_r2_ontology**: Public ontology constants governance.
- **req_r5_ocel_prov**: OCEL/PROV evidence projection and replayable receipts.

### ARDRequirements
- **req_r3_deterministic**: Deterministic graph conservation and delta transitions.
- **req_r4_knowledge_hook**: Knowledge hook query execution scheduler.

## Verification Checkpoints

Every requirement is mapped to a corresponding validation script or Rust witness target to ensure zero narrative promotion.
