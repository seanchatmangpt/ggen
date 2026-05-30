<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Vision 2030 GALL requirements (PRD / ARD)](#vision-2030-gall-requirements-prd--ard)
  - [Requirement Mapping](#requirement-mapping)
    - [PRDRequirements](#prdrequirements)
    - [ARDRequirements](#ardrequirements)
  - [Verification Checkpoints](#verification-checkpoints)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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
