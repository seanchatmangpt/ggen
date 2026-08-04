# `tools/ggen-architecture/tests/building_block.rs`

Source SHA-256: `f485014294d5e209c0ea220746122f7e6a15378cb84eea1de3c1bc5be4d1d1ac`

```mermaid
classDiagram
    class fn_admitted_block {
      <<fn>>
    }
    class fn_facade_exposes_one_canonical_building_block_kernel {
      <<fn>>
    }
    class fn_composition_is_deterministic_and_receipted {
      <<fn>>
    }
    class fn_alive_requires_witness_falsifier_verifier_receipt_and_replay {
      <<fn>>
    }
```

## Dependencies

- `ggen_architecture::profiles::fortune5::REQUIRED_BROKER`
- `ggen_architecture::{ ArchitectureFacet, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId, BuildingBlockRegistry, BuildingBlockStanding, EvidenceKind, EvidenceObligation, EvidenceReceipt, ObligationId, Port, PortDirection, PortId, PortKind, ProfileId, RealizationBinding, RealizationId, ResourceCeiling, ResourceClaim, }`
- `std::collections::{BTreeMap, BTreeSet}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
